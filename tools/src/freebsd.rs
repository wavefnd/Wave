// SPDX-License-Identifier: MPL-2.0
//! Pinned cross-build inputs, owned VM console and native guest acceptance.
use crate::{cli::Args, config, error, storage, Result};
#[cfg(unix)]
use crate::{
    package,
    process::{Output, Spec},
};
use std::ffi::OsString;
#[cfg(unix)]
use std::fs;
use std::fs::File;
use std::io::{Read, Write};
#[cfg(unix)]
use std::path::Path;
use std::path::PathBuf;
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
    echo_output: bool,
}

fn checked_deadline(timeout: Duration) -> Result<Instant> {
    Instant::now()
        .checked_add(timeout)
        .ok_or_else(|| error("timeout is too large to represent a deadline"))
}

impl<S: Read + Write> Console<S> {
    pub fn new(stream: S, log: File, timeout: Duration) -> Result<Self> {
        let boot_deadline = checked_deadline(timeout)?;
        Ok(Self { stream, log, buffer: String::new(), boot_deadline, echo_output: false })
    }

    pub fn expect(&mut self, pattern: &str, timeout: Option<Duration>) -> Result<Vec<String>> {
        let expression = regex_lite::Regex::new(pattern)?;
        let deadline = timeout.map(checked_deadline).transpose()?.unwrap_or(self.boot_deadline);
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
            if !self.read_output()? {
                return Err(error(format!("guest console closed before {pattern:?}")));
            }
        }
    }

    fn read_output(&mut self) -> Result<bool> {
        let mut bytes = [0; 16384];
        match self.stream.read(&mut bytes) {
            Ok(0) => Ok(false),
            Ok(count) => {
                self.log.write_all(&bytes[..count])?;
                self.log.flush()?;
                if self.echo_output {
                    std::io::stdout().write_all(&bytes[..count])?;
                    std::io::stdout().flush()?;
                }
                self.buffer.push_str(&String::from_utf8_lossy(&bytes[..count]));
                if self.buffer.len() > 1024 * 1024 {
                    return Err(error("guest console unmatched output exceeded 1 MiB"));
                }
                Ok(true)
            },
            Err(e)
                if matches!(
                    e.kind(),
                    std::io::ErrorKind::WouldBlock
                        | std::io::ErrorKind::TimedOut
                        | std::io::ErrorKind::Interrupted
                ) =>
            {
                Ok(true)
            },
            Err(e) => Err(e.into()),
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
            console: Console::new(stream, log, boot.saturating_sub(started.elapsed()))?,
            owner,
        })
    }

    fn shutdown(&mut self, timeout: Duration) -> Result<()> {
        let started = Instant::now();
        while !self.owner.task.as_ref().unwrap().is_finished() {
            if started.elapsed() >= timeout {
                return Err(error("VM did not power off cleanly; cache not saved"));
            }
            // QEMU writes the serial socket synchronously. Waiting without
            // draining it can block the guest before ACPI powers the VM off.
            if !self.console.read_output()? {
                std::thread::sleep(Duration::from_millis(20));
            }
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

pub fn cache_identity() -> Result<String> {
    let names = [
        "FREEBSD_IMAGE",
        "FREEBSD_IMAGE_SHA256",
        "FREEBSD_SYSROOT_URL",
        "FREEBSD_SYSROOT_SHA256",
        "FREEBSD_LLVM_RUNTIME_URL",
        "FREEBSD_LLVM_RUNTIME_SHA256",
    ];
    let values = names
        .iter()
        .map(|name| Ok((*name, Value::string(config::pin(name)?))))
        .collect::<Result<Vec<_>>>()?;
    Ok(storage::bytes_digest(config::json(&Value::object(values))?.as_bytes()))
}

#[cfg(unix)]
fn image(work: &Path, provision: bool) -> Result<PathBuf> {
    image_to(work, work, provision)
}

#[cfg(unix)]
fn image_to(cache: &Path, work: &Path, provision: bool) -> Result<PathBuf> {
    let name = config::pin("FREEBSD_IMAGE")?;
    let digest = config::pin("FREEBSD_IMAGE_SHA256")?;
    let archive = cache.join(&name);
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
        let timeout = Duration::from_secs(seconds);
        checked_deadline(timeout)?;
        Ok(timeout)
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
        let prefix = format!("wave-freebsd-inputs-v1-{}", cache_identity()?);
        for (name, value) in [
            ("path", directory.to_string_lossy().into_owned()),
            ("key", prefix.clone()),
            ("restore-prefix", prefix),
        ] {
            config::append_output(&output, name, &value)?;
        }
        return Ok(0);
    }
    #[cfg(unix)]
    if command == "validate-package" {
        let work = args.path("--work-dir")?.ok_or_else(|| error("missing --work-dir"))?;
        let cache = args.path("--cache")?.ok_or_else(|| error("missing --cache"))?;
        args.finish()?;
        validate_package_vm(&work, &cache)?;
        return Ok(0);
    }
    #[cfg(unix)]
    if command == "build-sysroot" {
        let archive = args.path("--archive")?.ok_or_else(|| error("missing --archive"))?;
        let output = args.path("--output")?.ok_or_else(|| error("missing --output"))?;
        args.finish()?;
        sysroot(&archive, &output)?;
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
fn quoted(value: &str) -> String {
    format!("'{}'", value.replace('\'', "'\"'\"'"))
}

#[cfg(unix)]
fn executable_script(path: &Path, contents: &str) -> Result<()> {
    use std::os::unix::fs::PermissionsExt;
    fs::write(path, contents)?;
    fs::set_permissions(path, fs::Permissions::from_mode(0o755))?;
    Ok(())
}

/// Reuse the reviewed release's target LLVM binaries; only headers/config run on the host.
#[cfg(unix)]
pub fn cross_environment(
    runtime: &Path,
    sysroot: &Path,
    host: &Path,
    work: &Path,
) -> Result<std::collections::BTreeMap<OsString, OsString>> {
    let runtime = fs::canonicalize(runtime)?;
    let sysroot = fs::canonicalize(sysroot)?;
    let host = fs::canonicalize(host)?;
    let version =
        package::run([host.join("bin/llvm-config").into_os_string(), "--version".into()])?;
    if version.trim() != config::pin("LLVM_SOURCE_VERSION")? {
        return Err(error("host LLVM must match the reviewed FreeBSD LLVM version exactly"));
    }
    for tool in ["ld.lld", "llc", "llvm-as", "llvm-mc"] {
        crate::binary::inspect(&runtime.join("bin").join(tool), Some("x86_64-unknown-freebsd"))?;
    }
    crate::binary::inspect(&runtime.join("lib/libLLVM.so"), Some("x86_64-unknown-freebsd"))?;
    if !sysroot.join("usr/lib/crt1.o").is_file() {
        return Err(error("FreeBSD base sysroot is incomplete"));
    }
    fs::create_dir_all(work.join("bin"))?;
    let work = fs::canonicalize(work)?;
    let config = work.join("bin/llvm-config");
    executable_script(&config, &format!(
        "#!/bin/sh\ncase \"$1\" in\n --libdir) printf '%s\\n' {};;\n --prefix) printf '%s\\n' {};;\n --libnames) printf '%s\\n' libLLVM.so;;\n --system-libs) printf '\\n';;\n --host-target) printf '%s\\n' x86_64-unknown-freebsd;;\n *) exec {} \"$@\";;\nesac\n",
        quoted(&runtime.join("lib").to_string_lossy()),
        quoted(&runtime.to_string_lossy()),
        quoted(&host.join("bin/llvm-config").to_string_lossy()),
    ))?;
    let cc = work.join("cc");
    executable_script(&cc, &format!(
        "#!/bin/sh\nexec {} --target=x86_64-unknown-freebsd14.4 --sysroot={} -fuse-ld=lld -Wl,-rpath-link,{} \"$@\"\n",
        quoted(&host.join("bin/clang").to_string_lossy()), quoted(&sysroot.to_string_lossy()),
        quoted(&runtime.join("lib").to_string_lossy()),
    ))?;
    let mut env = std::collections::BTreeMap::new();
    for (name, value) in [
        ("WAVE_CROSS_LLVM_TARGET", OsString::from("x86_64-unknown-freebsd")),
        ("WAVE_CROSS_SYSROOT", sysroot.into_os_string()),
        ("WAVE_LLVM_HOME", runtime.clone().into_os_string()),
        ("WAVE_LLVM_BIN", runtime.join("bin").into_os_string()),
        ("WAVE_LLVM_LIB", runtime.join("lib").into_os_string()),
        ("LLVM_SYS_211_PREFIX", work.into_os_string()),
        ("LLVM_CONFIG_PATH", config.into_os_string()),
        ("WAVE_LLVM_MC", host.join("bin/llvm-mc").into_os_string()),
        ("WAVE_HOST_LLVM_MC", host.join("bin/llvm-mc").into_os_string()),
        ("CARGO_TARGET_X86_64_UNKNOWN_FREEBSD_LINKER", cc.clone().into_os_string()),
        ("CC_x86_64_unknown_freebsd", cc.into_os_string()),
        ("AR_x86_64_unknown_freebsd", host.join("bin/llvm-ar").into_os_string()),
    ] {
        env.insert(name.into(), value);
    }
    Ok(env)
}

/// Select the base-system build inputs before applying the existing archive/link verifier.
#[cfg(unix)]
fn sysroot(archive: &Path, output: &Path) -> Result<()> {
    let work =
        tempfile::Builder::new().prefix(".freebsd-base-").tempdir_in(output.parent().unwrap())?;
    let compressed = work.path().join("base.tar.xz");
    fs::copy(archive, &compressed).map_err(|e| error(format!("copy base archive: {e}")))?;
    Spec::new([
        OsString::from("xz"),
        "--decompress".into(),
        "--memlimit-decompress=1GiB".into(),
        compressed.into(),
    ])
    .checked()?;
    let selected = work.path().join("sysroot.tar");
    let mut builder = tar::Builder::new(File::create(&selected)?);
    for entry in tar::Archive::new(
        File::open(work.path().join("base.tar"))
            .map_err(|e| error(format!("open expanded FreeBSD base: {e}")))?,
    )
    .entries()?
    {
        let mut entry = entry?;
        let mut sparse = None;
        if let Some(fields) = entry.pax_extensions()? {
            for field in fields {
                let field = field?;
                if field.key()? == "GNU.sparse.name" {
                    sparse = Some(PathBuf::from(field.value()?));
                }
            }
        }
        let path = sparse.clone().unwrap_or(entry.path()?.into_owned());
        let name = path.to_string_lossy();
        let name = name.trim_start_matches("./");
        // The libxo test plug-in points outside the build sysroot and is not a compiler input.
        if ["lib/", "libexec/", "usr/lib/", "usr/include/", "usr/libdata/"]
            .iter()
            .any(|prefix| name.starts_with(prefix))
            && !name.starts_with("usr/lib/libxo/")
            // Base compiler internals are unnecessary for the clang 21 host
            // driver and contain very large sparse LLVM 19 libraries.
            && !name.starts_with("usr/lib/libprivate")
        {
            let mut header = entry.header().clone();
            if sparse.is_some() {
                // GNU pax sparse members expose compact storage under a synthetic
                // name through tar::Entry. Let the existing host tar expand only
                // this member to stdout; filesystem extraction still uses sdk's
                // path/link policy below.
                let expanded = Spec::new([
                    OsString::from("tar"),
                    "-xOf".into(),
                    work.path().join("base.tar").into(),
                    "--".into(),
                    path.clone().into(),
                ])
                .checked()?;
                header.set_entry_type(tar::EntryType::Regular);
                header.set_size(expanded.stdout.len() as u64);
                builder.append_data(&mut header, &path, expanded.stdout.as_slice())?;
            } else {
                header.set_size(entry.size());
                builder.append_data(&mut header, &path, &mut entry)?;
            }
        }
    }
    builder.finish()?;
    crate::sdk::extract_toolchain(&selected, output)
        .map_err(|e| error(format!("extract filtered sysroot: {e}")))
}

#[cfg(unix)]
fn host_inputs(cache: &Path, work: &Path) -> Result<(PathBuf, PathBuf)> {
    fs::create_dir_all(cache)?;
    let runtime = cache.join("runtime.tar.gz");
    crate::sdk::download(
        &config::pin("FREEBSD_LLVM_RUNTIME_URL")?,
        &runtime,
        &config::pin("FREEBSD_LLVM_RUNTIME_SHA256")?,
    )?;
    let base = cache.join("base.txz");
    crate::sdk::download(
        &config::pin("FREEBSD_SYSROOT_URL")?,
        &base,
        &config::pin("FREEBSD_SYSROOT_SHA256")?,
    )?;
    crate::sdk::extract(&runtime, &work.join("runtime"), false)?;
    let entries = fs::read_dir(work.join("runtime"))?.collect::<std::io::Result<Vec<_>>>()?;
    if entries.len() != 1 || !entries[0].file_type()?.is_dir() {
        return Err(error("reviewed FreeBSD runtime must have exactly one package root"));
    }
    let sdk = entries[0].path().join("llvm");
    let base_root = work.join("sysroot");
    sysroot(&base, &base_root).map_err(|e| error(format!("FreeBSD build sysroot: {e}")))?;
    Ok((sdk, base_root))
}

#[cfg(unix)]
fn validate_package_vm(work: &Path, cache: &Path) -> Result<()> {
    let inputs = work.join("inputs");
    let name = format!("wave-v{}-x86_64-unknown-freebsd.tar.gz", config::compiler_version()?);
    let metadata =
        config::read_json(&work.join("artifacts").join(format!("{name}.metadata.json")))?;
    if metadata.get_str("source_sha") != Some(package::run(["git", "rev-parse", "HEAD"])?.trim())
        || metadata.get_str("target") != Some("x86_64-unknown-freebsd")
        || metadata.get_str("sha256") != Some(storage::digest(&inputs.join(&name))?.as_str())
    {
        return Err(error("FreeBSD validation input source/target/checksum mismatch"));
    }
    crate::binary::inspect(&inputs.join("wave-tools"), Some("x86_64-unknown-freebsd"))?;
    let iso = work.join("inputs.iso");
    Spec::new([
        OsString::from("genisoimage"),
        "-quiet".into(),
        "-R".into(),
        "-o".into(),
        iso.clone().into(),
        inputs.into(),
    ])
    .checked()?;
    println!("Validating the completed package and host-built tests inside FreeBSD; no Rust/LLVM installation or Cargo build in the VM");
    let image = image_to(cache, work, true)?;
    let overlay = work.join("guest.qcow2");
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
    let kvm = fs::OpenOptions::new().read(true).write(true).open("/dev/kvm").is_ok();
    let mut vm = Vm::start(
        vec![
            "qemu-system-x86_64".into(),
            "-cpu".into(),
            if kvm { "host" } else { "Nehalem" }.into(),
            "-m".into(),
            "3072".into(),
            "-smp".into(),
            "2".into(),
            "-accel".into(),
            if kvm { "kvm" } else { "tcg" }.into(),
            "-drive".into(),
            format!("file={},format=qcow2,if=virtio", overlay.display()).into(),
            "-drive".into(),
            format!("file={},format=raw,if=virtio,readonly=on", iso.display()).into(),
            "-nic".into(),
            "user,model=virtio-net-pci".into(),
        ],
        work,
        Duration::from_secs(240),
        Duration::from_secs(1800),
    )?;
    vm.console.boot("amd64")?;
    vm.console.echo_output = true;
    vm.console.send("mount -uw / && mkdir -p /wave-inputs && mount -t cd9660 /dev/vtbd1 /wave-inputs && sh /wave-inputs/validate.sh; printf '\\nWAVE-PACKAGE-STATUS %s\\n' \"$?\"\n")?;
    let result = vm
        .console
        .expect(r"(?:^|\r?\n)WAVE-PACKAGE-STATUS ([0-9]+)\r?\n", Some(Duration::from_secs(1200)))?;
    if result[1] != "0" {
        return Err(error(format!(
            "FreeBSD package validation failed (exit {}); see {}",
            result[1],
            work.join("guest.log").display()
        )));
    }
    vm.console.send("poweroff\n")?;
    vm.shutdown(Duration::from_secs(60))?;
    Ok(())
}

#[cfg(unix)]
fn build_package(output: &Path, cache: &Path, provision: bool) -> Result<()> {
    if !provision {
        return Err(error("FreeBSD cross package preparation requires --provision"));
    }
    if !cfg!(target_os = "linux") || std::env::consts::ARCH != "x86_64" {
        return Err(error("FreeBSD package builds require a Linux amd64 host"));
    }
    let temporary =
        std::env::var_os("RUNNER_TEMP").map(PathBuf::from).unwrap_or_else(std::env::temp_dir);
    let work =
        tempfile::Builder::new().prefix("wave-ci-freebsd-package-").tempdir_in(temporary)?.keep();
    let root = fs::canonicalize(crate::root())?;
    let target = "x86_64-unknown-freebsd";
    let run = |args: Vec<OsString>,
               env: &std::collections::BTreeMap<OsString, OsString>|
     -> Result<Output> {
        let mut spec = Spec::new(args);
        spec.env = env.clone();
        spec.stream = true;
        spec.timeout = Duration::from_secs(1800);
        spec.checked()
    };
    println!("Preparing pinned FreeBSD sysroot and existing LLVM runtime on Linux");
    let (sdk, base) = host_inputs(cache, &work)?;
    let host = package::prefix()?;
    let env = cross_environment(&sdk, &base, &host, &work.join("host-tools"))?;
    let rust = config::pin("RUST_VERSION")?;
    run(
        vec![
            "rustup".into(),
            "target".into(),
            "add".into(),
            "--toolchain".into(),
            rust.clone().into(),
            target.into(),
        ],
        &env,
    )?;
    let meson = work.join("meson");
    println!("Cross-building the FreeBSD compiler on Linux through Cargo and Meson");
    run(
        vec![
            "python3".into(),
            "-m".into(),
            "mesonbuild.mesonmain".into(),
            "setup".into(),
            meson.clone().into(),
            root.clone().into(),
            format!("-Dcompiler_target={target}").into(),
            "-Dbuildtype=release".into(),
        ],
        &env,
    )?;
    run(
        vec![
            "python3".into(),
            "-m".into(),
            "mesonbuild.mesonmain".into(),
            "compile".into(),
            "-C".into(),
            meson.clone().into(),
            "--jobs".into(),
            "2".into(),
            "wavec".into(),
        ],
        &env,
    )?;
    let artifacts = work.join("artifacts");
    let helper = std::env::current_exe()?;
    run(
        vec![
            helper.clone().into(),
            "package".into(),
            "--binary".into(),
            meson.join("wavec").into(),
            "--target".into(),
            target.into(),
            "--dist-dir".into(),
            work.join("dist").into(),
            "--archive-dir".into(),
            artifacts.clone().into(),
        ],
        &env,
    )?;

    println!("Cross-compiling existing FreeBSD validation tests on Linux");
    let tests = run(
        vec![
            "cargo".into(),
            format!("+{rust}").into(),
            "test".into(),
            "--locked".into(),
            "--jobs".into(),
            "2".into(),
            "--release".into(),
            "--target".into(),
            target.into(),
            "--workspace".into(),
            "--exclude".into(),
            "wave-tools".into(),
            "--lib".into(),
            "--test".into(),
            "frontend_regressions".into(),
            "--test".into(),
            "env_boundaries".into(),
            "--no-run".into(),
            "--message-format=json-render-diagnostics".into(),
        ],
        &env,
    )?
    .strict_stdout()?;
    run(
        vec![
            "cargo".into(),
            format!("+{rust}").into(),
            "build".into(),
            "--locked".into(),
            "--jobs".into(),
            "2".into(),
            "--release".into(),
            "--target".into(),
            target.into(),
            "-p".into(),
            "wave-tools".into(),
        ],
        &env,
    )?;
    let target_dir = std::env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| root.join("target"));
    let target_dir = fs::canonicalize(target_dir)?;
    let inputs = work.join("inputs");
    fs::create_dir(&inputs)?;
    fs::copy(target_dir.join(target).join("release/wave-tools"), inputs.join("wave-tools"))?;
    let mut payload = tar::Builder::new(File::create(inputs.join("validation.tar"))?);
    let mut executables = std::collections::BTreeSet::new();
    let mut copied = std::collections::BTreeSet::new();
    for line in tests.lines().filter(|line| !line.trim().is_empty()) {
        let event = config::parse_json(line)?;
        if event.get_str("reason") == Some("compiler-artifact") {
            if let Some(path) = event.get_str("executable") {
                let path = PathBuf::from(path);
                if !path.starts_with(&target_dir) {
                    return Err(error("test executable is outside Cargo output"));
                }
                if copied.insert(path.clone()) {
                    payload.append_path_with_name(&path, path.strip_prefix("/")?)?;
                }
                if event.get("profile").and_then(|profile| profile.get("test"))
                    == Some(&Value::Bool(true))
                {
                    executables.insert(path);
                }
            }
        } else if event.get_str("reason") == Some("build-script-executed")
            && event
                .get_str("package_id")
                .is_some_and(|id| id.starts_with("path+") && id.contains("/llvm#"))
        {
            let crt = Path::new(config::text(&event, "out_dir")?).join("crt");
            payload.append_dir_all(crt.strip_prefix("/")?, &crt)?;
        }
    }
    if executables.len() < 8
        || !executables
            .iter()
            .any(|p| p.file_name().unwrap().to_string_lossy().starts_with("env_boundaries-"))
        || !executables
            .iter()
            .any(|p| p.file_name().unwrap().to_string_lossy().starts_with("frontend_regressions-"))
    {
        return Err(error("FreeBSD validation executables are incomplete"));
    }
    payload.finish()?;
    let source = Spec::new([
        OsString::from("git"),
        "archive".into(),
        "--format=tar".into(),
        "HEAD".into(),
        "-o".into(),
        inputs.join("source.tar").into(),
    ]);
    source.checked()?;
    let version = config::compiler_version()?;
    let name = format!("wave-v{version}-{target}.tar.gz");
    let archive = artifacts.join(&name);
    fs::copy(&archive, inputs.join(&name))?;
    let package_name = name.strip_suffix(".tar.gz").unwrap();
    let commands = executables
        .iter()
        .map(|path| format!("{}\n", quoted(&path.to_string_lossy())))
        .collect::<String>();
    let source_root = quoted(&root.to_string_lossy());
    let script = format!(
        r#"#!/bin/sh
set -eu
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin
export ASSUME_ALWAYS_YES=yes
# Git is a runtime prerequisite for the existing installer regression tests.
dhclient vtnet0
pkg bootstrap -f
pkg install -y git
mkdir -p {source_root} /tmp/wave-runtime
tar -xf /wave-inputs/source.tar -C {source_root}
tar -xf /wave-inputs/validation.tar -C /
tar -xzf /wave-inputs/{name} -C /tmp/wave-runtime
export LD_LIBRARY_PATH=/tmp/wave-runtime/{package_name}/llvm/lib
export WAVE_REPOSITORY={source_root}
export WAVE_TOOLS=/wave-inputs/wave-tools
cd {source_root}
{commands}
/wave-inputs/wave-tools check extracted-package --target {target} --archive /wave-inputs/{name} --output /tmp/wave-package-evidence
printf 'FreeBSD package and native validation passed\n'
"#
    );
    fs::write(inputs.join("validate.sh"), script)?;
    validate_package_vm(&work, cache)?;
    let digest = storage::digest(&archive)?;
    let report = Value::object([
        ("status", Value::string("pass")),
        ("source_sha", Value::string(package::run(["git", "rev-parse", "HEAD"])?.trim())),
        ("archive_sha256", Value::string(digest)),
        ("host_built_tests", Value::integer(executables.len() as u64)),
        ("guest_cargo_builds", Value::integer(0)),
        ("clean_poweroff", Value::Bool(true)),
    ]);
    storage::atomic_write(
        &work.join("package-validation.json"),
        config::json(&report)?.as_bytes(),
    )?;
    fs::create_dir_all(output)?;
    let entries = ["", ".sha256", ".metadata.json"].map(|suffix| {
        let name = format!("{name}{suffix}");
        (artifacts.join(&name), output.join(name))
    });
    storage::replace_set(&entries)?;
    println!("FreeBSD package validated; evidence: {}", work.display());
    Ok(())
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;

    #[test]
    fn shutdown_drains_serial_backpressure_before_waiting_for_the_vm() {
        use std::os::unix::net::UnixStream;
        let directory = tempfile::tempdir().unwrap();
        let log = directory.path().join("guest.log");
        let (stream, mut peer) = UnixStream::pair().unwrap();
        stream.set_read_timeout(Some(Duration::from_millis(10))).unwrap();
        peer.set_write_timeout(Some(Duration::from_secs(2))).unwrap();
        let bytes = vec![b'x'; 512 * 1024];
        let expected = bytes.len();
        let task = std::thread::spawn(move || -> Result<Output> {
            peer.write_all(&bytes)?;
            Ok(Output {
                status: 0,
                stdout: vec![],
                stderr: vec![],
                failure: None,
                elapsed: Duration::ZERO,
            })
        });
        let mut vm = Vm {
            console: Console::new(stream, File::create(&log).unwrap(), Duration::from_secs(1))
                .unwrap(),
            owner: GuestOwner { stop: Arc::new(AtomicBool::new(false)), task: Some(task) },
        };
        vm.shutdown(Duration::from_secs(1)).unwrap();
        // The final write can complete before the last buffered chunk is read.
        while vm.console.read_output().unwrap() {}
        assert_eq!(fs::read(&log).unwrap().len(), expected);
    }
}
