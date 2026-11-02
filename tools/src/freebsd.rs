// SPDX-License-Identifier: MPL-2.0
//! Owned VM console, cold/warm build-cache identity and native guest acceptance.
#[cfg(unix)]
use crate::process::{Output, Spec};
use crate::{cli::Args, config, error, package, storage, Result};
use std::ffi::OsString;
#[cfg(unix)]
use std::fs;
use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
#[cfg(unix)]
use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};
use std::time::{Duration, Instant};
use utils::wson::Value;

/// Bounded matching over a stream, with complete newline-terminated identities.
pub struct Console<S> {
    stream: S,
    log: File,
    buffer: String,
    boot_deadline: Instant,
}

impl<S: Read + Write> Console<S> {
    pub fn new(stream: S, log: File, timeout: Duration) -> Self {
        Self { stream, log, buffer: String::new(), boot_deadline: Instant::now() + timeout }
    }

    pub fn expect(&mut self, pattern: &str, timeout: Option<Duration>) -> Result<Vec<String>> {
        let expression = regex_lite::Regex::new(pattern)?;
        let deadline =
            timeout.map(|timeout| Instant::now() + timeout).unwrap_or(self.boot_deadline);
        loop {
            if let Some(captures) = expression.captures(&self.buffer) {
                let end = captures.get(0).unwrap().end();
                let result = captures
                    .iter()
                    .map(|m| m.map(|m| m.as_str()).unwrap_or("").to_string())
                    .collect();
                self.buffer.drain(..end);
                return Ok(result);
            }
            if Instant::now() >= deadline {
                return Err(error(format!("guest did not reach {pattern:?} before deadline")));
            }
            let mut bytes = [0; 16384];
            match self.stream.read(&mut bytes) {
                Ok(0) => return Err(error(format!("guest console closed before {pattern:?}"))),
                Ok(count) => {
                    self.log.write_all(&bytes[..count])?;
                    self.log.flush()?;
                    self.buffer.push_str(&String::from_utf8_lossy(&bytes[..count]));
                    if self.buffer.len() > 1024 * 1024 {
                        return Err(error("guest console unmatched output exceeded 1 MiB"));
                    }
                },
                Err(e)
                    if matches!(
                        e.kind(),
                        std::io::ErrorKind::WouldBlock
                            | std::io::ErrorKind::TimedOut
                            | std::io::ErrorKind::Interrupted
                    ) => {},
                Err(e) => return Err(e.into()),
            }
        }
    }

    pub fn send(&mut self, text: &str) -> Result<()> {
        self.stream.write_all(text.as_bytes())?;
        self.stream.flush()?;
        Ok(())
    }

    pub fn loader(&mut self, text: &str) -> Result<()> {
        for character in text.chars() {
            let character = character.to_string();
            self.send(&character)?;
            self.expect(&regex_lite::escape(&character), None)?;
        }
        self.send("\r")
    }

    pub fn boot(&mut self, architecture: &str) -> Result<()> {
        if architecture == "riscv64" {
            let result = self.expect(r"mountroot> |Enter full ", None)?;
            if result[0].starts_with("mountroot>") {
                self.send("ufs:/dev/vtbd0p3\n")?;
                self.expect("Enter full ", None)?;
            }
        } else {
            self.expect("Autoboot in", None)?;
            self.send("3")?;
            self.expect("OK ", None)?;
            if architecture == "amd64" {
                self.loader("set console=comconsole")?;
                self.expect("OK ", None)?;
            }
            self.loader("boot -s")?;
            self.expect("Enter full ", None)?;
        }
        self.send("\r")?;
        self.expect(r"root@[^\r\n]*# ", None)?;
        Ok(())
    }
}

#[cfg(unix)]
struct Vm {
    console: Console<std::os::unix::net::UnixStream>,
    owner: GuestOwner,
}

#[cfg(unix)]
struct GuestOwner {
    stop: Arc<AtomicBool>,
    task: Option<std::thread::JoinHandle<Result<Output>>>,
}

#[cfg(unix)]
impl Vm {
    fn start(
        mut command: Vec<OsString>,
        work: &Path,
        boot: Duration,
        timeout: Duration,
    ) -> Result<Self> {
        use std::os::unix::net::UnixStream;
        let socket = work.join("serial.sock");
        command.extend([
            // -nographic also enables firmware/loader output on the serial
            // console. -display none alone leaves BIOS output on hidden VGA.
            "-nographic".into(),
            "-monitor".into(),
            "none".into(),
            "-chardev".into(),
            format!("socket,id=console,path={},server=on,wait=on", socket.display()).into(),
            "-serial".into(),
            "chardev:console".into(),
        ]);
        let stop = Arc::new(AtomicBool::new(false));
        let mut spec = Spec::new(command);
        spec.stop = Some(stop.clone());
        spec.log = Some(work.join("qemu.log"));
        spec.timeout = timeout;
        let mut owner = GuestOwner { stop, task: Some(std::thread::spawn(move || spec.run())) };
        let started = Instant::now();
        let stream = loop {
            match UnixStream::connect(&socket) {
                Ok(stream) => break stream,
                Err(e)
                    if started.elapsed() < boot
                        && !owner.task.as_ref().unwrap().is_finished()
                        && matches!(
                            e.kind(),
                            std::io::ErrorKind::NotFound | std::io::ErrorKind::ConnectionRefused
                        ) =>
                {
                    std::thread::sleep(Duration::from_millis(20))
                },
                Err(e) => {
                    owner.stop.store(true, Ordering::SeqCst);
                    let result = owner
                        .task
                        .take()
                        .unwrap()
                        .join()
                        .map_err(|_| error("VM owner panicked"))?;
                    return Err(error(format!(
                        "VM console connection failed: {e}; process: {result:?}"
                    )));
                },
            }
        };
        stream.set_read_timeout(Some(Duration::from_millis(100)))?;
        stream.set_write_timeout(Some(Duration::from_secs(10)))?;
        let log = File::create(work.join("guest.log"))?;
        Ok(Self {
            console: Console::new(stream, log, boot.saturating_sub(started.elapsed())),
            owner,
        })
    }

    fn shutdown(&mut self, timeout: Duration) -> Result<()> {
        let started = Instant::now();
        while !self.owner.task.as_ref().unwrap().is_finished() {
            if started.elapsed() >= timeout {
                return Err(error("VM did not power off cleanly; cache not saved"));
            }
            std::thread::sleep(Duration::from_millis(50));
        }
        let result =
            self.owner.task.take().unwrap().join().map_err(|_| error("VM owner panicked"))??;
        if result.status != 0 || result.failure.is_some() {
            return Err(error(format!(
                "QEMU shutdown failed: {:?}, exit {}",
                result.failure, result.status
            )));
        }
        Ok(())
    }
}

#[cfg(unix)]
impl Drop for GuestOwner {
    fn drop(&mut self) {
        if let Some(task) = self.task.take() {
            self.stop.store(true, Ordering::SeqCst);
            let _ = task.join();
        }
    }
}

pub fn cache_identity(root: &Path) -> Result<String> {
    let mut files = vec![
        "Cargo.lock".into(),
        "Cargo.toml".into(),
        ".cargo/config.toml".into(),
        "meson.build".into(),
        "meson.options".into(),
        "tools/Cargo.toml".into(),
    ];
    for folder in
        ["llvm", "whale", "utils", "front/lexer", "front/parser", "front/hir", "front/error"]
    {
        files.push(format!("{folder}/Cargo.toml"));
    }
    files.extend(
        storage::files(&root.join("tools/src"))?
            .into_iter()
            .filter(|path| path.extension().is_some_and(|v| v == "rs"))
            .map(|p| p.strip_prefix(root).unwrap().to_string_lossy().replace('\\', "/")),
    );
    files.sort();
    files.dedup();
    let inputs = files
        .iter()
        .map(|name| Ok((name.clone(), Value::string(storage::digest(&root.join(name))?))))
        .collect::<Result<Vec<_>>>()?;
    let pins = [
        "FREEBSD_IMAGE",
        "FREEBSD_IMAGE_SHA256",
        "RUST_VERSION",
        "LLVM_SOURCE_VERSION",
        "MESON_VERSION",
    ]
    .into_iter()
    .map(|name| Ok((name, Value::string(config::pin(name)?))))
    .collect::<Result<Vec<_>>>()?;
    let value = Value::object([
        ("schema", Value::integer(2)),
        ("pins", Value::object(pins)),
        ("inputs", Value::object(inputs)),
    ]);
    Ok(storage::bytes_digest(config::json(&value)?.as_bytes()))
}

pub fn cache_image(directory: &Path, expected: &str) -> Result<Option<PathBuf>> {
    let image = directory.join("vm.qcow2");
    let manifest = directory.join("manifest.json");
    if !image.is_file() || !manifest.is_file() {
        return Ok(None);
    }
    let validate = (|| -> Result<()> {
        let metadata = config::read_json(&manifest)?;
        if metadata.get_str("identity") != Some(expected)
            || metadata.get_str("sha256") != Some(storage::digest(&image)?.as_str())
        {
            return Err(error("cache identity or checksum mismatch"));
        }
        let info = config::parse_json(&package::run([
            OsString::from("qemu-img"),
            "info".into(),
            "--output=json".into(),
            image.clone().into(),
        ])?)?;
        if info.get_str("format") != Some("qcow2")
            || info.get("backing-filename").is_some()
            || info.get("data-file").is_some()
            || info.get("dirty-flag") == Some(&Value::Bool(true))
        {
            return Err(error("cache is not a clean standalone qcow2 image"));
        }
        if let Some(data) = info.get("format-specific").and_then(|v| v.get("data")) {
            if data.get("data-file").is_some() || data.get("corrupt") == Some(&Value::Bool(true)) {
                return Err(error("cache image is corrupt or externally backed"));
            }
        }
        Ok(())
    })();
    match validate {
        Ok(()) => Ok(Some(image)),
        Err(e) => {
            println!("Discarding unusable FreeBSD VM cache: {e}");
            Ok(None)
        },
    }
}

#[cfg(unix)]
fn save_image(overlay: &Path, directory: &Path, identity: &str, source: &str) -> Result<()> {
    fs::create_dir_all(directory)?;
    let stage = tempfile::Builder::new().prefix(".snapshot-").tempdir_in(directory)?;
    let image = stage.path().join("vm.qcow2");
    let mut convert = Spec::new([
        OsString::from("qemu-img"),
        "convert".into(),
        "-O".into(),
        "qcow2".into(),
        overlay.into(),
        image.clone().into(),
    ]);
    convert.timeout = Duration::from_secs(900);
    convert.checked()?;
    let mut check = Spec::new([OsString::from("qemu-img"), "check".into(), image.clone().into()]);
    check.timeout = Duration::from_secs(120);
    check.checked()?;
    let manifest = stage.path().join("manifest.json");
    fs::write(
        &manifest,
        config::json(&Value::object([
            ("identity", Value::string(identity)),
            ("source", Value::string(source)),
            ("sha256", Value::string(storage::digest(&image)?)),
        ]))?,
    )?;
    storage::replace_set(&[
        (image, directory.join("vm.qcow2")),
        (manifest, directory.join("manifest.json")),
    ])?;
    Ok(())
}

#[cfg(unix)]
fn image(work: &Path, provision: bool) -> Result<PathBuf> {
    let name = config::pin("FREEBSD_IMAGE")?;
    let digest = config::pin("FREEBSD_IMAGE_SHA256")?;
    let temporary =
        std::env::var_os("RUNNER_TEMP").map(PathBuf::from).unwrap_or_else(|| work.to_path_buf());
    let archive = temporary.join(&name);
    if provision {
        crate::sdk::download(
            &format!(
                "https://download.freebsd.org/releases/VM-IMAGES/14.4-RELEASE/amd64/Latest/{name}"
            ),
            &archive,
            &digest,
        )?;
    }
    if storage::digest(&archive)? != digest {
        return Err(error("FreeBSD image checksum mismatch"));
    }
    let target =
        work.join(name.strip_suffix(".xz").ok_or_else(|| error("pinned image must use xz"))?);
    let pending = tempfile::Builder::new().prefix(".image-").tempdir_in(work)?;
    let compressed = pending.path().join(&name);
    fs::copy(&archive, &compressed)?;
    Spec::new([
        OsString::from("xz"),
        "--decompress".into(),
        "--memlimit-decompress=1GiB".into(),
        compressed.into(),
    ])
    .checked()?;
    let candidate = pending.path().join(name.strip_suffix(".xz").unwrap());
    storage::replace_set(&[(candidate, target.clone())])?;
    Ok(target)
}

#[cfg(unix)]
fn provider_cases(args: &mut Args) -> Result<i32> {
    let arch = args.required("--arch")?;
    let architecture = match arch.as_str() {
        "amd64" => "x86_64",
        "arm64" => "aarch64",
        "riscv64" => "riscv64",
        _ => return Err(error("unsupported FreeBSD runtime architecture")),
    };
    let image = args.path("--image")?.ok_or_else(|| error("missing --image"))?;
    let compiler = crate::runner::compiler(args.path("--compiler")?)?;
    let firmware = args.path("--firmware")?;
    let kernel = args.path("--kernel")?;
    let clang = args.path("--clang")?.unwrap_or_else(|| "clang-21".into());
    let linker = args.path("--linker")?.unwrap_or_else(|| "ld.lld".into());
    let output = args.path("--out-dir")?.unwrap_or_else(|| crate::root().join(".tmp/freebsd-sys"));
    let report = args.path("--report-json")?.unwrap_or_else(|| output.join("report.json"));
    let seconds = |value: Option<String>, fallback: u64| -> Result<Duration> {
        let seconds = value.map(|v| v.parse()).transpose()?.unwrap_or(fallback);
        if seconds == 0 {
            return Err(error("timeouts must be positive"));
        }
        Ok(Duration::from_secs(seconds))
    };
    let command_timeout = seconds(args.text("--command-timeout")?, 120)?;
    let boot_timeout = seconds(args.text("--boot-timeout")?, 240)?;
    let case_timeout = seconds(args.text("--case-timeout")?, 60)?;
    let inputs = crate::runner::protected_inputs(&[image.clone(), compiler.clone()])?;
    storage::validate_output(&report, &inputs)?;
    let mut data = Value::object([
        ("schema_version", Value::integer(1)),
        ("arch", Value::string(&arch)),
        ("status", Value::string("running")),
        ("phase", Value::string("validation")),
        ("commands", Value::Array(vec![])),
        ("cases", Value::Array(vec![])),
    ]);
    let action = (|| -> Result<()> {
        if !image.is_file() {
            return Err(error("FreeBSD image is missing"));
        }
        if arch == "arm64" && firmware.as_ref().is_none_or(|p| !p.is_file()) {
            return Err(error("matching AArch64 UEFI firmware is required"));
        }
        if arch == "riscv64" && kernel.as_ref().is_none_or(|p| !p.is_file()) {
            return Err(error("matching RISC-V kernel is required"));
        }
        fs::create_dir_all(&output)?;
        let work = tempfile::Builder::new().prefix(&format!("{arch}-")).tempdir_in(&output)?.keep();
        config::field(&mut data, "work_dir", Value::string(work.to_string_lossy()))?;
        config::field(&mut data, "phase", Value::string("build"))?;
        let run = |command: Vec<OsString>, data: &mut Value| -> Result<()> {
            let mut spec = Spec::new(command);
            spec.timeout = command_timeout;
            spec.log = Some(work.join("last-command.log"));
            let result = spec.run();
            let mut row = Value::object([(
                "command",
                Value::strings(spec.args.iter().map(|a| a.to_string_lossy())),
            )]);
            match &result {
                Ok(output) => {
                    config::field(&mut row, "exit_code", config::number(output.status as i64))?;
                    config::field(
                        &mut row,
                        "status",
                        Value::string(if output.failure.as_deref() == Some("timeout") {
                            "timeout"
                        } else if output.status == 0 && output.failure.is_none() {
                            "pass"
                        } else {
                            "fail"
                        }),
                    )?;
                },
                Err(e) => {
                    config::field(&mut row, "status", Value::string("launch_error"))?;
                    config::field(&mut row, "error", Value::string(e.to_string()))?;
                },
            }
            let mut commands = config::array(data, "commands")?.to_vec();
            commands.push(row);
            config::field(data, "commands", Value::Array(commands))?;
            storage::atomic_write(&report, config::json(data)?.as_bytes())?;
            let output = result?;
            if output.status != 0 || output.failure.is_some() {
                return Err(error(format!(
                    "FreeBSD build command failed: {}",
                    output.text_stderr()
                )));
            }
            Ok(())
        };
        let manifest = crate::cases::Manifest::load(&crate::root(), true)?;
        let mut target = manifest.target(&format!("freebsd-{arch}"))?.clone();
        target.exclude.clear();
        let sources = manifest
            .sources(Some(&target))?
            .into_iter()
            .filter(|p| p.starts_with(manifest.root.join(format!("freebsd/{arch}"))))
            .collect::<Vec<_>>();
        if sources.is_empty() {
            return Err(error("no FreeBSD provider cases selected"));
        }
        let iso_root = work.join("cases");
        fs::create_dir(&iso_root)?;
        let triple = format!("{architecture}-unknown-freebsd");
        let runtime = work.join("start.o");
        run(
            vec![
                clang.into(),
                format!("--target={triple}").into(),
                "-O2".into(),
                "-fno-builtin".into(),
                "-ffreestanding".into(),
                "-fno-stack-protector".into(),
                "-c".into(),
                crate::root().join("tests/fixtures/freebsd_case_runtime/start.c").into(),
                "-o".into(),
                runtime.clone().into(),
            ],
            &mut data,
        )?;
        let mut cases = Vec::new();
        let mut names = Vec::new();
        for source in sources {
            let unit = if source.file_name().unwrap() == "main.wave" {
                source.parent().unwrap()
            } else {
                &source
            };
            let name = unit
                .file_stem()
                .unwrap()
                .to_str()
                .ok_or_else(|| error("invalid FreeBSD case name"))?
                .to_string();
            names.push(name.clone());
            for opt in ["O0", "O2"] {
                let directory = work.join(format!("{name}-{opt}"));
                run(
                    vec![
                        compiler.clone().into(),
                        "build".into(),
                        source.clone().into(),
                        "--std-root".into(),
                        crate::root().join("std").into(),
                        "--target".into(),
                        triple.clone().into(),
                        "--emit=obj".into(),
                        format!("-{opt}").into(),
                        "--out-dir".into(),
                        directory.clone().into(),
                    ],
                    &mut data,
                )?;
                run(
                    vec![
                        linker.clone().into(),
                        "-static".into(),
                        "-e".into(),
                        "_start".into(),
                        runtime.clone().into(),
                        directory
                            .join(format!("{}.o", source.file_stem().unwrap().to_string_lossy()))
                            .into(),
                        "-o".into(),
                        iso_root.join(format!("{name}-{opt}")).into(),
                    ],
                    &mut data,
                )?;
                cases.push(Value::object([
                    ("name", Value::string(format!("{name}-{opt}"))),
                    ("status", Value::string("not_run")),
                ]));
            }
        }
        fs::write(iso_root.join("run.sh"),format!("for name in {}; do\n for opt in O0 O2; do\n /mnt/$name-$opt\n echo \\\"WAVE-RESULT $name-$opt $?\\\"\n done\ndone\n",names.join(" ")).replace("\\\"","\""))?;
        let iso = work.join("cases.iso");
        run(
            vec![
                "genisoimage".into(),
                "-quiet".into(),
                "-R".into(),
                "-o".into(),
                iso.clone().into(),
                iso_root.into(),
            ],
            &mut data,
        )?;
        let overlay = work.join("guest.qcow2");
        run(
            vec![
                "qemu-img".into(),
                "create".into(),
                "-f".into(),
                "qcow2".into(),
                "-F".into(),
                "qcow2".into(),
                "-b".into(),
                fs::canonicalize(image)?.into(),
                overlay.clone().into(),
            ],
            &mut data,
        )?;
        let block = if arch == "riscv64" { "virtio-blk-device" } else { "virtio-blk-pci" };
        let mut command = vec![
            format!("qemu-system-{architecture}").into(),
            "-m".into(),
            "768".into(),
            "-smp".into(),
            "2".into(),
            "-accel".into(),
            "tcg".into(),
            "-drive".into(),
            format!("file={},format=qcow2,if=none,id=system", overlay.display()).into(),
            "-device".into(),
            format!("{block},drive=system").into(),
            "-drive".into(),
            format!("file={},format=raw,if=none,id=cases,readonly=on", iso.display()).into(),
            "-device".into(),
            format!("{block},drive=cases").into(),
            "-nic".into(),
            "none".into(),
        ];
        match arch.as_str() {
            "amd64" => command.extend([
                "-object".into(),
                "rng-random,filename=/dev/urandom,id=rng0".into(),
                "-device".into(),
                "virtio-rng-pci,rng=rng0".into(),
            ]),
            "arm64" => command.extend([
                "-machine".into(),
                "virt".into(),
                "-cpu".into(),
                "cortex-a72".into(),
                "-bios".into(),
                fs::canonicalize(firmware.as_ref().unwrap())?.into(),
            ]),
            _ => command.extend([
                "-machine".into(),
                "virt,acpi=off".into(),
                "-cpu".into(),
                "rv64,sstc=false".into(),
                "-bios".into(),
                "default".into(),
                "-kernel".into(),
                fs::canonicalize(kernel.as_ref().unwrap())?.into(),
                "-append".into(),
                "-s".into(),
            ]),
        };
        config::field(&mut data, "phase", Value::string("guest_boot"))?;
        config::field(
            &mut data,
            "guest_command",
            Value::strings(command.iter().map(|v: &OsString| v.to_string_lossy())),
        )?;
        config::field(&mut data, "cases", Value::Array(cases.clone()))?;
        let mut vm = Vm::start(
            command,
            &work,
            boot_timeout,
            boot_timeout + case_timeout * (cases.len() as u32) + Duration::from_secs(90),
        )?;
        vm.console.boot(&arch)?;
        vm.console.send("mount -uw /\nmount -t cd9660 /dev/vtbd1 /mnt\nsh /mnt/run.sh\n")?;
        config::field(&mut data, "phase", Value::string("guest_cases"))?;
        for index in 0..cases.len() {
            let case = &mut cases[index];
            let name = config::text(case, "name")?.to_string();
            config::field(case, "status", Value::string("running"))?;
            match vm.console.expect(
                &format!(r"WAVE-RESULT {} ([0-9]+)\r", regex_lite::escape(&name)),
                Some(case_timeout),
            ) {
                Ok(result) => {
                    let code = result[1].parse::<u64>()?;
                    config::field(case, "exit_code", Value::integer(code))?;
                    config::field(
                        case,
                        "status",
                        Value::string(if code == 0 { "pass" } else { "fail" }),
                    )?;
                },
                Err(e) => {
                    config::field(case, "status", Value::string("missing_result"))?;
                    config::field(&mut data, "cases", Value::Array(cases.clone()))?;
                    return Err(e);
                },
            }
            config::field(&mut data, "cases", Value::Array(cases.clone()))?;
            storage::atomic_write(&report, config::json(&data)?.as_bytes())?;
        }
        config::field(&mut data, "phase", Value::string("guest_shutdown"))?;
        vm.console.send("poweroff\n")?;
        vm.shutdown(Duration::from_secs(60))?;
        if cases.iter().any(|case| case.get_str("status") != Some("pass")) {
            return Err(error("FreeBSD provider cases failed"));
        }
        Ok(())
    })();
    config::field(
        &mut data,
        "status",
        Value::string(if action.is_ok() { "pass" } else { "fail" }),
    )?;
    if let Err(e) = &action {
        eprintln!("FreeBSD provider validation failed: {e}");
        config::field(&mut data, "error", Value::string(e.to_string()))?;
    }
    storage::validate_output(&report, &inputs)?;
    storage::atomic_write(&report, config::json(&data)?.as_bytes())?;
    Ok(i32::from(action.is_err()))
}

pub fn main(arguments: &[OsString]) -> Result<i32> {
    let (command, rest) =
        arguments.split_first().ok_or_else(|| error("expected FreeBSD command"))?;
    let mut args = Args::new(rest);
    if command == "cache-key" {
        let output =
            args.path("--github-output")?.ok_or_else(|| error("missing --github-output"))?;
        args.finish()?;
        storage::validate_output(&output, &crate::runner::protected_inputs(&[])?)?;
        let directory =
            std::env::var_os("WAVE_FREEBSD_CACHE_DIR").map(PathBuf::from).unwrap_or_else(|| {
                std::env::var_os("RUNNER_TEMP")
                    .map(PathBuf::from)
                    .unwrap_or_else(std::env::temp_dir)
                    .join("wave-freebsd-cache")
            });
        let prefix = format!("wave-freebsd-v2-{}-", cache_identity(&crate::root())?);
        let source = package::run(["git", "rev-parse", "HEAD"])?;
        for (name, value) in [
            ("path", directory.to_string_lossy().into_owned()),
            ("key", format!("{prefix}{}", source.trim())),
            ("restore-prefix", prefix),
        ] {
            config::append_output(&output, name, &value)?;
        }
        return Ok(0);
    }
    #[cfg(unix)]
    if command == "fetch-image" {
        let work = args.path("--work-dir")?.ok_or_else(|| error("missing --work-dir"))?;
        let provision = args.flag("--provision");
        let output = args.path("--github-env")?;
        args.finish()?;
        fs::create_dir_all(&work)?;
        let path = image(&work, provision)?;
        let name = path.file_name().unwrap().to_string_lossy();
        if let Some(output) = output {
            storage::validate_output(&output, &crate::runner::protected_inputs(&[])?)?;
            config::append_env(&output, "FREEBSD_UNPACKED_IMAGE", &name)?;
        } else {
            println!("{}", path.display());
        }
        return Ok(0);
    }
    #[cfg(unix)]
    if command == "package" {
        let output = args.path("--output")?.unwrap_or_else(crate::root);
        let provision = args.flag("--provision");
        let cache = args.path("--cache")?.unwrap_or_else(|| {
            std::env::var_os("WAVE_FREEBSD_CACHE_DIR").map(PathBuf::from).unwrap_or_else(|| {
                std::env::var_os("RUNNER_TEMP")
                    .map(PathBuf::from)
                    .unwrap_or_else(std::env::temp_dir)
                    .join("wave-freebsd-cache")
            })
        });
        args.finish()?;
        build_package(&output, &cache, provision)?;
        return Ok(0);
    }
    #[cfg(unix)]
    if command == "providers" {
        return provider_cases(&mut args);
    }
    Err(error("unknown or unavailable FreeBSD command"))
}

#[cfg(unix)]
const BOOTSTRAP: &str = r###"#!/bin/sh
set -eu
if [ ! -f /root/.wave-ci-prepared ]; then
    gpart recover vtbd0
    root_part=$(gpart show -p vtbd0 | awk '$4 == "freebsd-ufs" {print $3}')
    test -n "$root_part"
    gpart resize -i "${root_part##*p}" vtbd0
# Use the mount point: the mounted /dev/gpt/rootfs alias can differ from vtbd0pN.
# growfs must recognize the live mount to suspend UFS writes while expanding it.
    growfs -y /
fi
mkdir -p /root/.ssh
cp /mnt/authorized_keys /root/.ssh/authorized_keys
chmod 700 /root/.ssh
chmod 600 /root/.ssh/authorized_keys
grep -qxF 'PermitRootLogin prohibit-password' /etc/ssh/sshd_config ||
    echo 'PermitRootLogin prohibit-password' >> /etc/ssh/sshd_config
dhclient vtnet0
service sshd onestart
"###;

#[cfg(unix)]
const GUEST_BUILD: &str = r###"#!/bin/sh
set -eu
export ASSUME_ALWAYS_YES=yes
export PYTHONUNBUFFERED=1
phase() { printf '\n[FreeBSD %s] %s\n' "$(date -u +%H:%M:%S)" "$*"; }
phase 'Guest CPU'
sysctl hw.model hw.ncpu
if [ -f /root/.wave-ci-prepared ]; then
    [ "$(cat /root/.wave-ci-prepared)" = CACHE_PIN ]
    phase 'Reuse installed LLVM and Rust'
else
    phase 'Install build dependencies'
    pkg bootstrap -f
    pkg install -y bash git python312 py312-pip ninja llvm21 patchelf curl ca_root_nss
    ln -sf /usr/local/bin/python3.12 /usr/local/bin/python3
    curl --proto '=https' --tlsv1.2 -fsSL https://sh.rustup.rs -o /tmp/rustup.sh
    sh /tmp/rustup.sh -y --profile minimal --default-toolchain RUST_PIN
fi
export PATH=/root/.cargo/bin:/usr/local/llvm21/bin:/usr/local/bin:/usr/bin:/bin
export LLVM_SYS_211_PREFIX=/usr/local/llvm21
export LLVM_CONFIG_PATH=/usr/local/llvm21/bin/llvm-config
export CARGO_BUILD_JOBS=2
export CARGO_TARGET_X86_64_UNKNOWN_FREEBSD_LINKER=cc
[ "$(llvm-config --version)" = LLVM_PIN ]
case "$(rustc --version)" in 'rustc RUST_PIN '*) ;; *) exit 1;; esac
printf '%s\n' CACHE_PIN > /root/.wave-ci-prepared
if [ -d /root/Wave/.git ]; then
    phase 'Refresh source while retaining Cargo outputs'
    git -C /root/Wave fetch --no-tags /mnt/source.bundle HEAD
    git -C /root/Wave checkout --detach --force SOURCE_PIN
    git -C /root/Wave clean -ffdx -e target/
else
    git clone /mnt/source.bundle /root/Wave
    git -C /root/Wave checkout --detach SOURCE_PIN
fi
cd /root/Wave
phase 'Prepare pinned Meson'
python3 -m venv /root/.wave-meson
/root/.wave-meson/bin/python -m pip install --disable-pip-version-check meson==MESON_PIN
export PATH=/root/.wave-meson/bin:$PATH
phase 'Run workspace unit tests'
cargo test --locked --workspace --lib --jobs 2
phase 'Run frontend regression tests'
cargo test --locked --test frontend_regressions --jobs 2
phase 'Run native environment boundary cases with injected fixture variables'
cargo test --locked --test env_boundaries --jobs 2
phase 'Build release archive'
meson setup .tmp/meson-freebsd -Dcompiler_target=x86_64-unknown-freebsd
meson compile -C .tmp/meson-freebsd release
phase 'Validate extracted package and C linking'
./.tmp/meson-freebsd/wave-tools check extracted-package --target x86_64-unknown-freebsd --archive "$(ls wave-v*-x86_64-unknown-freebsd.tar.gz)"
phase 'Package validation complete'
"###;

#[cfg(unix)]
fn quoted(value: &str) -> String {
    format!("'{}'", value.replace('\'', "'\\''"))
}

#[cfg(unix)]
fn build_package(output: &Path, cache: &Path, provision: bool) -> Result<()> {
    if !provision {
        return Err(error("FreeBSD VM package provisioning requires --provision"));
    }
    let work = tempfile::Builder::new().prefix("wave-freebsd-package-").tempdir()?;
    let inputs = work.path().join("inputs");
    fs::create_dir(&inputs)?;
    let identity = cache_identity(&crate::root())?;
    let cached = cache_image(cache, &identity)?;
    let warm = cached.is_some();
    let image = match cached {
        Some(path) => path,
        None => image(work.path(), true)?,
    };
    let key = work.path().join("guest-key");
    Spec::new([
        OsString::from("ssh-keygen"),
        "-q".into(),
        "-t".into(),
        "ed25519".into(),
        "-N".into(),
        "".into(),
        "-f".into(),
        key.clone().into(),
    ])
    .checked()?;
    fs::copy(key.with_extension("pub"), inputs.join("authorized_keys"))?;
    fs::write(inputs.join("bootstrap.sh"), BOOTSTRAP)?;
    let source = package::run(["git", "rev-parse", "HEAD"])?;
    let source = source.trim();
    crate::publication::sha(source)?;
    Spec::new([
        OsString::from("git"),
        "bundle".into(),
        "create".into(),
        inputs.join("source.bundle").into(),
        "HEAD".into(),
    ])
    .checked()?;
    let guest = GUEST_BUILD
        .replace("RUST_PIN", &quoted(&config::pin("RUST_VERSION")?))
        .replace("LLVM_PIN", &quoted(&config::pin("LLVM_SOURCE_VERSION")?))
        .replace("MESON_PIN", &quoted(&config::pin("MESON_VERSION")?))
        .replace("CACHE_PIN", &quoted(&identity))
        .replace("SOURCE_PIN", &quoted(source));
    fs::write(inputs.join("build.sh"), guest)?;
    let iso = work.path().join("inputs.iso");
    Spec::new([
        OsString::from("genisoimage"),
        "-quiet".into(),
        "-R".into(),
        "-o".into(),
        iso.clone().into(),
        inputs.into(),
    ])
    .checked()?;
    let overlay = work.path().join("guest.qcow2");
    Spec::new([
        OsString::from("qemu-img"),
        "create".into(),
        "-f".into(),
        "qcow2".into(),
        "-F".into(),
        "qcow2".into(),
        "-b".into(),
        fs::canonicalize(image)?.into(),
        overlay.clone().into(),
    ])
    .checked()?;
    if !warm {
        Spec::new([
            OsString::from("qemu-img"),
            "resize".into(),
            overlay.clone().into(),
            "+24G".into(),
        ])
        .checked()?;
    }
    let port = {
        let listener = std::net::TcpListener::bind("127.0.0.1:0")?;
        listener.local_addr()?.port()
    };
    let kvm = fs::OpenOptions::new().read(true).write(true).open("/dev/kvm").is_ok();
    let command = vec![
        "qemu-system-x86_64".into(),
        "-cpu".into(),
        if kvm { "host" } else { "Nehalem" }.into(),
        "-m".into(),
        "6144".into(),
        "-smp".into(),
        "2".into(),
        "-accel".into(),
        if kvm { "kvm" } else { "tcg" }.into(),
        "-drive".into(),
        format!("file={},format=qcow2,if=virtio", overlay.display()).into(),
        "-drive".into(),
        format!("file={},format=raw,if=virtio,readonly=on", iso.display()).into(),
        "-nic".into(),
        format!("user,model=virtio-net-pci,hostfwd=tcp:127.0.0.1:{port}-:22").into(),
    ];
    let mut vm =
        Vm::start(command, work.path(), Duration::from_secs(300), Duration::from_secs(18000))?;
    vm.console.boot("amd64")?;
    vm.console.send("mount -uw / && mount -t cd9660 /dev/vtbd1 /mnt && sh /mnt/bootstrap.sh; printf '\\nWAVE-SSH-STATUS %s\\n' \"$?\"\n")?;
    let status = vm
        .console
        .expect(r"(?:^|\r?\n)WAVE-SSH-STATUS ([0-9]+)\r?\n", Some(Duration::from_secs(180)))?;
    if status[1] != "0" {
        return Err(error(format!("FreeBSD SSH bootstrap exited {}", status[1])));
    }
    vm.console.send("cat /etc/ssh/ssh_host_ed25519_key.pub\n")?;
    let key_line = vm.console.expect(
        r"(?:^|\r?\n)(ssh-ed25519 [A-Za-z0-9+/=]+)(?:[ \t]+[^\r\n]*)?\r?\n",
        Some(Duration::from_secs(30)),
    )?;
    let known = work.path().join("known_hosts");
    fs::write(&known, format!("[127.0.0.1]:{port} {}\n", key_line[1]))?;
    let options = vec![
        "-i".into(),
        key.into(),
        "-o".into(),
        "BatchMode=yes".into(),
        "-o".into(),
        "StrictHostKeyChecking=yes".into(),
        "-o".into(),
        format!("UserKnownHostsFile={}", known.display()).into(),
        "-o".into(),
        "ConnectTimeout=30".into(),
    ];
    let ssh = |script: &str, timeout: u64| -> Result<()> {
        let mut args = vec![OsString::from("ssh")];
        args.extend(options.iter().cloned());
        args.extend(["-p".into(), port.to_string().into(), "root@127.0.0.1".into(), script.into()]);
        let mut spec = Spec::new(args);
        spec.timeout = Duration::from_secs(timeout);
        spec.stream = true;
        spec.checked()?;
        Ok(())
    };
    ssh("sh /mnt/build.sh", 14400)?;
    let version = config::compiler_version()?;
    let name = format!("wave-v{version}-x86_64-unknown-freebsd.tar.gz");
    fs::create_dir_all(output)?;
    let pending = tempfile::Builder::new().prefix(".freebsd-artifacts-").tempdir_in(output)?;
    for suffix in ["", ".sha256", ".metadata.json"] {
        let mut args = vec![OsString::from("scp"), "-O".into()];
        args.extend(options.iter().cloned());
        args.extend([
            "-P".into(),
            port.to_string().into(),
            format!("root@127.0.0.1:/root/Wave/{name}{suffix}").into(),
            pending.path().join(format!("{name}{suffix}")).into(),
        ]);
        Spec::new(args).checked()?;
    }
    let archive = pending.path().join(&name);
    let digest = storage::digest(&archive)?;
    let metadata = config::read_json(&pending.path().join(format!("{name}.metadata.json")))?;
    if metadata.get_str("sha256") != Some(&digest)
        || metadata.get_str("source_sha") != Some(source)
        || metadata.get_str("target") != Some("x86_64-unknown-freebsd")
        || fs::read_to_string(pending.path().join(format!("{name}.sha256")))?.trim()
            != format!("{digest}  {name}")
    {
        return Err(error("FreeBSD artifact identity or checksum mismatch"));
    }
    ssh("set -e; rm -f /root/.ssh/authorized_keys /etc/ssh/ssh_host_* /root/.cargo/credentials /root/.cargo/credentials.toml /root/Wave/wave-v*.tar.gz*; pkg clean -ay; sync",120)?;
    vm.console.send("shutdown -p now\n")?;
    vm.console.expect("Powering system off", Some(Duration::from_secs(120)))?;
    vm.shutdown(Duration::from_secs(30))?;
    let entries = ["", ".sha256", ".metadata.json"].map(|suffix| {
        let name = format!("{name}{suffix}");
        (pending.path().join(&name), output.join(name))
    });
    storage::replace_set(&entries)?;
    save_image(&overlay, cache, &identity, source)?;
    Ok(())
}
