// SPDX-License-Identifier: MPL-2.0
//! Native MSVC acceptance and PE dependency evidence; never an emulation pass.
use crate::{binary, cli::Args, config, error, process::Spec, storage, Result};
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;
use utils::wson::Value;

pub fn require_native(target: &str) -> Result<()> {
    if !["x86_64-pc-windows-msvc", "aarch64-pc-windows-msvc"].contains(&target) {
        return Err(error("expected an MSVC target"));
    }
    #[cfg(windows)]
    {
        use windows_sys::Win32::System::Threading::{GetCurrentProcess, IsWow64Process2};
        let mut process = 0;
        let mut native = 0;
        if unsafe { IsWow64Process2(GetCurrentProcess(), &mut process, &mut native) } == 0 {
            return Err(std::io::Error::last_os_error().into());
        }
        let expected = if target.starts_with("aarch64") { 0xaa64 } else { 0x8664 };
        if process != 0 || native != expected {
            return Err(error(format!(
                "native {target} required; process={process:#x}, host={native:#x}"
            )));
        }
        Ok(())
    }
    #[cfg(not(windows))]
    {
        Err(error("MSVC runtime verification requires native Windows"))
    }
}

pub fn dependency_closure(
    roots: &[PathBuf],
    target: &str,
    search: &[PathBuf],
    runtime: &[PathBuf],
) -> Result<(BTreeMap<String, PathBuf>, BTreeMap<String, PathBuf>)> {
    let mut queue: VecDeque<_> = roots.iter().cloned().collect();
    let mut seen = BTreeSet::new();
    let mut providers: BTreeMap<String, PathBuf> = BTreeMap::new();
    let mut payloads = BTreeMap::new();
    let mut prerequisites = BTreeMap::new();
    let vc = regex_lite::Regex::new(
        r"(?i)^(?:vcruntime140(?:_1)?|msvcp140(?:_[0-9]+|_atomic_wait|_codecvt_ids)?|concrt140)\.dll$",
    )?;
    while let Some(path) = queue.pop_front() {
        let path = fs::canonicalize(path)?;
        if !seen.insert(path.clone()) {
            continue;
        }
        let info = binary::inspect(&path, Some(target))?;
        if info.format != "Pe" {
            return Err(error("dependency closure requires PE executables and DLLs"));
        }
        for name in info.needed {
            let lower = name.to_lowercase();
            if ["libgcc", "libstdc++", "winpthread", "mingw"].iter().any(|p| lower.contains(p)) {
                return Err(error(format!("retired MinGW dependency {name}")));
            }
            if binary::system_library(&name, target) {
                continue;
            }
            let required = vc.is_match(&name);
            let directories = if required {
                runtime.to_vec()
            } else {
                std::iter::once(path.parent().unwrap().to_owned())
                    .chain(search.iter().cloned())
                    .collect()
            };
            let mut provider = None;
            for directory in directories {
                if !directory.is_dir() {
                    continue;
                }
                for entry in fs::read_dir(directory)? {
                    let entry = entry?;
                    if entry.file_name().to_string_lossy().eq_ignore_ascii_case(&name)
                        && entry.path().is_file()
                    {
                        provider = Some(fs::canonicalize(entry.path())?);
                        break;
                    }
                }
                if provider.is_some() {
                    break;
                }
            }
            let provider = provider.ok_or_else(|| {
                error(format!(
                    "{}: missing {} {name}",
                    path.display(),
                    if required { "VC runtime prerequisite" } else { "dependency" }
                ))
            })?;
            if let Some(previous) = providers.get(&lower) {
                if storage::digest(previous)? != storage::digest(&provider)? {
                    return Err(error(format!("conflicting DLL providers for {name}")));
                }
            }
            providers.insert(lower.clone(), provider.clone());
            if required {
                prerequisites.insert(lower, provider.clone());
            } else {
                payloads.insert(lower, provider.clone());
            }
            queue.push_back(provider);
        }
    }
    Ok((payloads, prerequisites))
}

struct Audit {
    compiler: PathBuf,
    llvm: PathBuf,
    output: PathBuf,
    sources: PathBuf,
    target: String,
    profile: tempfile::TempDir,
    commands: Vec<Value>,
    failures: Vec<Value>,
}

impl Audit {
    fn command(
        &mut self,
        args: Vec<OsString>,
        directory: &Path,
        expected: Option<i32>,
        stdout: Option<&str>,
        diagnostic: Option<&str>,
    ) -> Result<crate::process::Output> {
        let mut spec = Spec::new(args);
        spec.cwd = directory.into();
        spec.timeout = Duration::from_secs(90);
        for name in ["HOME", "USERPROFILE"] {
            spec.env.insert(name.into(), self.profile.path().as_os_str().into());
        }
        spec.env.insert("NO_COLOR".into(), "1".into());
        spec.env.insert("WAVE_LLVM_HOME".into(), self.llvm.parent().unwrap().as_os_str().into());
        let output = spec.run();
        let mut record = Value::object([
            ("command", Value::strings(spec.args.iter().map(|a| a.to_string_lossy()))),
            ("cwd", Value::string(directory.to_string_lossy())),
            ("expected_exit", expected.map(|v| config::number(v as i64)).unwrap_or(Value::Null)),
        ]);
        match &output {
            Ok(result) => {
                for (key, value) in [
                    ("exit", config::number(result.status as i64)),
                    ("stdout", Value::string(result.text_stdout())),
                    ("stderr", Value::string(result.text_stderr())),
                    ("timeout", Value::Bool(result.failure.as_deref() == Some("timeout"))),
                ] {
                    config::field(&mut record, key, value)?;
                }
            },
            Err(e) => config::field(&mut record, "error", Value::string(e.to_string()))?,
        }
        self.commands.push(record);
        storage::atomic_write(
            &self.output.join("commands.json"),
            config::json(&Value::Array(self.commands.clone()))?.as_bytes(),
        )?;
        let result = output?;
        if result.failure.is_some()
            || expected.map(|code| result.status != code).unwrap_or(result.status == 0)
        {
            return Err(error(format!(
                "MSVC command failed: {:?}, exit {}, {:?}\n{}\n{}",
                spec.args,
                result.status,
                result.failure,
                result.text_stdout(),
                result.text_stderr()
            )));
        }
        if stdout.is_some_and(|expected| result.text_stdout() != expected) {
            return Err(error("unexpected MSVC fixture stdout"));
        }
        if diagnostic.is_some_and(|text| {
            !format!("{}{}", result.text_stdout(), result.text_stderr())
                .to_lowercase()
                .contains(&text.to_lowercase())
        }) {
            return Err(error(format!("missing diagnostic {}", diagnostic.unwrap())));
        }
        Ok(result)
    }

    fn wave(
        &self,
        name: &str,
        directory: &Path,
        opt: u8,
        crt: &str,
        extra: Vec<OsString>,
    ) -> Vec<OsString> {
        let mut args = vec![
            self.compiler.clone().into(),
            "--std-root".into(),
            crate::root().join("std").into(),
            "build".into(),
            self.sources.join(format!("{name}.wave")).into(),
            "--target".into(),
            self.target.clone().into(),
            format!("-O{opt}").into(),
            format!("-Clinker={}", self.llvm.join("lld-link.exe").display()).into(),
            "--out-dir".into(),
            directory.into(),
        ];
        if crt == "static" {
            args.push("--static".into());
        }
        args.extend(extra);
        args
    }

    fn c_object(&mut self, name: &str, directory: &Path, opt: u8, crt: &str) -> Result<PathBuf> {
        let obj = directory.join(format!("{name}.obj"));
        let base = vec![
            self.llvm.join("clang-cl.exe").into(),
            format!("--target={}", self.target).into(),
            "/nologo".into(),
            "/TC".into(),
            "/c".into(),
            if opt == 0 { "/Od" } else { "/O2" }.into(),
            if crt == "static" { "/MT" } else { "/MD" }.into(),
        ];
        let mut args = base.clone();
        args.extend([
            format!("/Fo{}", obj.display()).into(),
            self.sources.join(format!("{name}.c")).into(),
        ]);
        self.command(args, directory, Some(0), None, None)?;
        let mut args = base;
        args.extend([
            "/clang:-S".into(),
            "/clang:-emit-llvm".into(),
            "/clang:-o".into(),
            format!("/clang:{}", directory.join(format!("{name}-c.ll")).display()).into(),
            self.sources.join(format!("{name}.c")).into(),
        ]);
        self.command(args, directory, Some(0), None, None)?;
        binary::inspect(&obj, Some(&self.target))?;
        Ok(obj)
    }

    fn execute(&mut self, exe: &Path, directory: &Path, expected: i32, stdout: &str) -> Result<()> {
        let info = binary::inspect(exe, Some(&self.target))?;
        if info.needed.iter().any(|name| {
            ["libgcc", "libstdc++", "libwinpthread"].iter().any(|p| name.to_lowercase().contains(p))
        }) {
            return Err(error("MSVC fixture has a MinGW dependency"));
        }
        self.command(vec![exe.into()], directory, Some(expected), Some(stdout), None)?;
        Ok(())
    }

    fn case(
        &mut self,
        name: &str,
        action: impl FnOnce(&mut Self, &Path) -> Result<()>,
    ) -> Result<()> {
        let directory = self.output.join(name);
        fs::create_dir(&directory)?;
        match action(self, &directory) {
            Ok(()) => println!("PASS {name}"),
            Err(e) => {
                eprintln!("FAIL {name}: {e}");
                self.failures.push(Value::object([
                    ("case", Value::string(name)),
                    ("error", Value::string(e.to_string())),
                ]));
            },
        }
        Ok(())
    }

    fn hosted(
        &mut self,
        directory: &Path,
        name: &str,
        opt: u8,
        crt: &str,
        code: i32,
        stdout: &str,
    ) -> Result<()> {
        let exe = directory.join(format!("{name}.exe"));
        self.command(
            self.wave(
                name,
                directory,
                opt,
                crt,
                vec!["--emit=ir,obj,bin".into(), "-o".into(), exe.clone().into()],
            ),
            directory,
            Some(0),
            None,
            None,
        )?;
        self.command(
            vec![
                self.llvm.join("llvm-readobj.exe").into(),
                "--symbols".into(),
                directory.join(format!("{name}.o")).into(),
            ],
            directory,
            Some(0),
            None,
            None,
        )?;
        self.execute(&exe, directory, code, stdout)
    }

    fn abi(
        &mut self,
        directory: &Path,
        name: &str,
        opt: u8,
        crt: &str,
        stdout: &str,
    ) -> Result<()> {
        let peer = self.c_object(name, directory, opt, crt)?;
        let exe = directory.join(format!("{name}.exe"));
        self.command(
            self.wave(
                name,
                directory,
                opt,
                crt,
                vec![peer.into(), "--emit=ir,obj,bin".into(), "-o".into(), exe.clone().into()],
            ),
            directory,
            Some(0),
            None,
            None,
        )?;
        if name == "stack" {
            self.command(
                vec![
                    self.llvm.join("llvm-readobj.exe").into(),
                    "--unwind".into(),
                    "--symbols".into(),
                    directory.join("stack.o").into(),
                ],
                directory,
                Some(0),
                None,
                Some("RuntimeFunction"),
            )?;
        }
        self.execute(&exe, directory, 0, stdout)
    }

    fn negative(&mut self, directory: &Path, crt: &str, mismatch: bool) -> Result<()> {
        let exe = directory.join("preserved.exe");
        let sentinel = b"previous output must survive a failed link\n";
        fs::write(&exe, sentinel)?;
        let mut objects = vec![];
        if mismatch {
            objects.push(self.c_object("crt_mismatch", directory, 0, "dynamic")?.into());
            objects.push(self.c_object("crt_static", directory, 0, "static")?.into());
        }
        objects.extend(["-o".into(), exe.clone().into()]);
        let args = self.wave(
            if mismatch { "implicit" } else { "missing_helper" },
            directory,
            0,
            crt,
            objects,
        );
        self.command(
            args,
            directory,
            None,
            None,
            Some(if mismatch { "RuntimeLibrary" } else { "wave_missing_runtime_helper" }),
        )?;
        if fs::read(exe)? != sentinel {
            return Err(error("failed link replaced previous output"));
        }
        Ok(())
    }

    fn dll(&mut self, directory: &Path, opt: u8, crt: &str) -> Result<()> {
        let dll = directory.join("wave-library.dll");
        let mut extra = vec!["--shared".into()];
        extra.extend(
            ["wave_packet", "wave_allocate", "wave_release"]
                .map(|name| format!("-Clink-arg=/EXPORT:{name}").into()),
        );
        extra.extend(["-o".into(), dll.clone().into()]);
        self.command(
            self.wave("dll", directory, opt, "dynamic", extra),
            directory,
            Some(0),
            None,
            None,
        )?;
        self.command(
            vec![self.llvm.join("llvm-readobj.exe").into(), "--coff-exports".into(), dll.into()],
            directory,
            Some(0),
            None,
            Some("wave_packet"),
        )?;
        for imported in [false, true] {
            let exe = directory.join(if imported { "import-consumer.exe" } else { "loader.exe" });
            let mut args = vec![
                self.llvm.join("clang-cl.exe").into(),
                format!("--target={}", self.target).into(),
                "/nologo".into(),
                "/TC".into(),
            ];
            if imported {
                args.push("/DIMPORT_CONSUMER".into());
            }
            args.extend([
                if opt == 0 { "/Od" } else { "/O2" }.into(),
                if crt == "static" { "/MT" } else { "/MD" }.into(),
                self.sources.join("dll_host.c").into(),
                format!("/Fe{}", exe.display()).into(),
                "/link".into(),
                "/INCREMENTAL:NO".into(),
            ]);
            if imported {
                args.push(directory.join("wave-library.lib").into());
            }
            self.command(args, directory, Some(0), None, None)?;
            self.execute(&exe, directory, 0, "native DLL host checked\n")?;
        }
        self.command(
            vec![
                self.llvm.join("clang-cl.exe").into(),
                format!("--target={}", self.target).into(),
                "/nologo".into(),
                "/TC".into(),
                "/LD".into(),
                if opt == 0 { "/Od" } else { "/O2" }.into(),
                if crt == "static" { "/MT" } else { "/MD" }.into(),
                self.sources.join("dll_peer.c").into(),
                format!("/Fe{}", directory.join("peer.dll").display()).into(),
                "/link".into(),
                format!("/IMPLIB:{}", directory.join("peer.lib").display()).into(),
            ],
            directory,
            Some(0),
            None,
            None,
        )?;
        let exe = directory.join("consumer.exe");
        self.command(
            self.wave(
                "dll_consumer",
                directory,
                opt,
                crt,
                vec![directory.join("peer.lib").into(), "-o".into(), exe.clone().into()],
            ),
            directory,
            Some(0),
            None,
            None,
        )?;
        self.execute(&exe, directory, 0, "native DLL consumer checked\n")
    }

    fn response(&mut self, directory: &Path, linker: &Path) -> Result<()> {
        let objects = directory.join(format!("한글 object directory {}", ["nested"; 8].join(" ")));
        fs::create_dir(&objects)?;
        let source = objects.join("empty.c");
        let object = objects.join("empty.obj");
        fs::write(&source, "typedef int no_external_symbols;\n")?;
        self.command(
            vec![
                self.llvm.join("clang-cl.exe").into(),
                "/nologo".into(),
                "/c".into(),
                "/MD".into(),
                source.into(),
                format!("/Fo{}", object.display()).into(),
            ],
            directory,
            Some(0),
            None,
            None,
        )?;
        let mut extra = Vec::new();
        for i in 0..64 {
            let copy = objects.join(format!("translation-unit-{i:03}.obj"));
            fs::copy(&object, &copy)?;
            extra.push(copy.into());
        }
        let exe = directory.join("long-paths.exe");
        extra.extend(["-o".into(), exe.clone().into()]);
        let mut args = self.wave("implicit", directory, 0, "dynamic", extra);
        for arg in &mut args {
            if arg.to_string_lossy().starts_with("-Clinker=") {
                *arg = format!("-Clinker={}", linker.display()).into();
            }
        }
        self.command(args.clone(), directory, Some(0), None, None)?;
        self.execute(&exe, directory, 0, "native implicit main\n")?;
        let previous = fs::read(&exe)?;
        args.push("--link=wave_deliberately_missing".into());
        self.command(args, directory, None, None, Some("wave_deliberately_missing"))?;
        if fs::read(exe)? != previous
            || fs::read_dir(directory)?.any(|p| {
                p.is_ok_and(|p| {
                    p.file_name().to_string_lossy().starts_with(".wave-output-")
                        && p.file_name().to_string_lossy().ends_with(".tmp")
                })
            })
        {
            return Err(error("response-file failure changed output or leaked temporary files"));
        }
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        require_native(&self.target)?;
        for tool in [&self.compiler, self.llvm.join("clang-cl.exe").as_path()] {
            binary::inspect(tool, Some(&self.target))?;
        }
        for name in ["LIB", "INCLUDE", "VCToolsInstallDir"] {
            if std::env::var_os(name).is_none_or(|v| v.is_empty()) {
                return Err(error(format!("matching MSVC Developer Prompt {name} required")));
            }
        }
        let output = self.output.clone();
        self.command(
            vec![self.llvm.join("clang-cl.exe").into(), "--version".into()],
            &output,
            Some(0),
            None,
            None,
        )?;
        self.command(
            vec![self.compiler.clone().into(), "-V".into()],
            &output,
            Some(0),
            None,
            None,
        )?;
        for opt in [0, 2] {
            for crt in ["dynamic", "static"] {
                for (name, code, stdout) in [
                    ("implicit", 0, "native implicit main\n"),
                    ("exit", 37, "native exit 37\n"),
                    ("helpers", 0, "native helpers checked\n"),
                    ("wide_numeric", 0, "native wide numeric checked\n"),
                ] {
                    self.case(&format!("{name}-O{opt}-{crt}"), |a, d| {
                        a.hosted(d, name, opt, crt, code, stdout)
                    })?;
                }
                for (name, stdout) in [
                    ("abi", "native ABI checked\n"),
                    ("abi_edges", ""),
                    ("stack", "native stack checked\n"),
                ] {
                    self.case(&format!("{name}-O{opt}-{crt}"), |a, d| {
                        a.abi(d, name, opt, crt, stdout)
                    })?;
                }
                self.case(&format!("dll-O{opt}-{crt}"), |a, d| a.dll(d, opt, crt))?;
            }
        }
        let linker = self.llvm.join("lld-link.exe");
        self.case("response-lld", |a, d| a.response(d, &linker))?;
        let vc = PathBuf::from(std::env::var_os("VCToolsInstallDir").unwrap());
        let arch = if self.target.starts_with("aarch64") { "arm64" } else { "x64" };
        let host = if arch == "arm64" { "Hostarm64" } else { "Hostx64" };
        let linker = vc.join("bin").join(host).join(arch).join("link.exe");
        if !linker.is_file() {
            return Err(error("native MSVC link.exe is required"));
        }
        self.case("response-msvc", |a, d| a.response(d, &linker))?;
        self.case("custom-entry", |a, d| {
            a.command(
                a.wave("custom_entry", d, 0, "dynamic", vec!["--emit=obj".into()]),
                d,
                Some(0),
                None,
                None,
            )?;
            let exe = d.join("custom-entry.exe");
            a.command(
                vec![
                    a.compiler.clone().into(),
                    "build".into(),
                    d.join("custom_entry.o").into(),
                    "--target".into(),
                    a.target.clone().into(),
                    format!("-Clinker={}", a.llvm.join("lld-link.exe").display()).into(),
                    "--entry=native_entry".into(),
                    "-Cno-default-libs".into(),
                    "--link=kernel32".into(),
                    "-o".into(),
                    exe.clone().into(),
                ],
                d,
                Some(0),
                None,
                None,
            )?;
            a.execute(&exe, d, 23, "")
        })?;
        for crt in ["dynamic", "static"] {
            self.case(&format!("missing-helper-{crt}"), |a, d| a.negative(d, crt, false))?;
        }
        self.case("mismatched-crt", |a, d| a.negative(d, "static", true))?;
        self.case("unsupported-static-dll", |a, d| {
            a.command(
                a.wave(
                    "implicit",
                    d,
                    0,
                    "static",
                    vec!["--shared".into(), "-o".into(), d.join("invalid.dll").into()],
                ),
                d,
                None,
                None,
                Some("cannot combine --shared and --static"),
            )?;
            Ok(())
        })?;
        Ok(())
    }
}

pub fn acceptance(
    compiler: PathBuf,
    llvm: PathBuf,
    target: String,
    output: PathBuf,
) -> Result<i32> {
    if storage::exists(&output) {
        return Err(error("MSVC evidence directory already exists"));
    }
    fs::create_dir_all(&output)?;
    let sources = output.join("sources");
    storage::copy_tree(&crate::root().join("tests/fixtures/msvc_native"), &sources)?;
    for extension in ["c", "wave"] {
        fs::copy(
            crate::root().join(format!("tests/fixtures/c_abi_edges/interop.{extension}")),
            sources.join(format!("abi_edges.{extension}")),
        )?;
    }
    let profile = tempfile::Builder::new()
        .prefix("wave-msvc-profile-")
        .tempdir_in(output.parent().unwrap())?;
    storage::copy_tree(&crate::root().join("std"), &profile.path().join(".wave/lib/wave/std"))?;
    let mut audit = Audit {
        compiler,
        llvm,
        output,
        sources,
        target,
        profile,
        commands: vec![],
        failures: vec![],
    };
    if let Err(e) = audit.run() {
        audit.failures.push(Value::object([
            ("case", Value::string("prerequisites")),
            ("error", Value::string(e.to_string())),
        ]));
    }
    let passed = audit.failures.is_empty();
    storage::atomic_write(
        &audit.output.join("result.json"),
        config::json(&Value::object([
            ("target", Value::string(audit.target)),
            ("failures", Value::Array(audit.failures)),
            ("passed", Value::Bool(passed)),
            ("commands", Value::integer(audit.commands.len() as u64)),
        ]))?
        .as_bytes(),
    )?;
    Ok(i32::from(!passed))
}

pub fn main(arguments: &[OsString]) -> Result<i32> {
    let mut args = Args::new(arguments);
    let compiler = crate::runner::compiler(args.path("--wavec")?)?;
    let llvm = args.path("--llvm-bin")?.ok_or_else(|| error("missing --llvm-bin"))?;
    let target = args.required("--target")?;
    let output = args.path("--output")?.ok_or_else(|| error("missing --output"))?;
    args.finish()?;
    acceptance(compiler, llvm, target, output)
}

pub fn diagnose(arguments: &[OsString]) -> Result<i32> {
    let mut args = Args::new(arguments);
    let compiler = crate::runner::compiler(args.path("--wavec")?)?;
    let llvm = args.path("--llvm-bin")?.ok_or_else(|| error("missing --llvm-bin"))?;
    let output = args.path("--output")?.ok_or_else(|| error("missing --output"))?;
    args.finish()?;
    require_native("aarch64-pc-windows-msvc")?;
    binary::inspect(&compiler, Some("aarch64-pc-windows-msvc"))?;
    fs::create_dir_all(&output)?;
    let output = fs::canonicalize(output)?;
    let profile = tempfile::Builder::new().prefix("wave-arm64-probe-").tempdir()?;
    storage::copy_tree(&crate::root().join("std"), &profile.path().join(".wave/lib/wave/std"))?;
    let probe = |label: &str, command: Vec<OsString>, timeout: u64| -> Result<bool> {
        let mut spec = Spec::new(command);
        spec.env.insert("WAVE_CODEGEN_TRACE".into(), "1".into());
        for name in ["HOME", "USERPROFILE"] {
            spec.env.insert(name.into(), profile.path().as_os_str().into());
        }
        spec.timeout = Duration::from_secs(timeout);
        let result = spec.run();
        let (passed, text) = match result {
            Ok(result) => (
                result.status == 0 && result.failure.is_none(),
                format!(
                    "[{label}] {:?}\n{}\n{}\nexit: {} (0x{:08X})\n{}",
                    spec.args,
                    result.text_stdout(),
                    result.text_stderr(),
                    result.status,
                    result.status as u32,
                    result.failure.unwrap_or_default()
                ),
            ),
            Err(e) => (false, format!("[{label}] {:?}\n{e}\n", spec.args)),
        };
        storage::atomic_write(&output.join(format!("{label}.log")), text.as_bytes())?;
        print!("{text}");
        Ok(passed)
    };
    let mut passed = true;
    for (label, command) in [
        ("compiler-version", vec![compiler.clone().into(), "-V".into()]),
        ("default-target", vec![compiler.clone().into(), "print".into(), "default-target".into()]),
        ("llvm-version", vec![llvm.join("llvm-config.exe").into(), "--version".into()]),
    ] {
        passed &= probe(label, command, 30)?;
    }
    let debugger = llvm.join("lldb.exe");
    let available =
        probe("debugger-version", vec![debugger.clone().into(), "--version".into()], 30)?;
    if !available {
        println!("Debugger unavailable; compiler failures will be reported without a backtrace.");
    }
    let minimal = output.join("minimal.wave");
    fs::write(&minimal, "fun main() -> i32 { return 0; }\n")?;
    for source in [minimal, crate::root().join("tests/cases/windows/arm64/test1.wave")] {
        let name = source.file_stem().unwrap().to_string_lossy();
        if !probe(
            &format!("{name}-check"),
            vec![compiler.clone().into(), "check".into(), source.clone().into()],
            30,
        )? {
            passed = false;
            continue;
        }
        for emit in ["ir", "obj"] {
            let label = format!("{name}-msvc-{emit}");
            let command = vec![
                compiler.clone().into(),
                "build".into(),
                source.clone().into(),
                "--target=aarch64-pc-windows-msvc".into(),
                format!("--emit={emit}").into(),
                "--out-dir".into(),
                output.join(&label).into(),
            ];
            if !probe(&label, command.clone(), 30)? {
                passed = false;
                if available {
                    let mut args = vec![
                        debugger.clone().into(),
                        "--batch".into(),
                        "--no-lldbinit".into(),
                        "-o".into(),
                        "run".into(),
                        "-k".into(),
                        "thread backtrace all".into(),
                        "-k".into(),
                        "register read".into(),
                        "-k".into(),
                        "image list".into(),
                        "--".into(),
                    ];
                    args.extend(command);
                    probe(&format!("{label}-debugger"), args, 90)?;
                }
                break;
            }
        }
    }
    Ok(i32::from(!passed))
}
