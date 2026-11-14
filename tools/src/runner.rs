// SPDX-License-Identifier: MPL-2.0
//! Case contracts shared by native, QEMU, WebAssembly and source-corpus execution.
use crate::{binary, cases, config, error, process, storage, Result};
use std::collections::{BTreeMap, BTreeSet};
use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};
use std::time::{Duration, Instant};
use utils::wson::Value;

#[derive(Debug, Clone)]
pub struct Metadata {
    pub mode: String,
    pub runner: String,
    pub runtime: String,
    pub requires: Vec<String>,
    pub target: Option<String>,
    pub emit: String,
    pub freestanding: bool,
    pub expected_exit: i32,
    pub udp_input: bool,
    pub stdin: Option<String>,
    pub stdin_file: Option<String>,
    pub stdout_file: Option<String>,
    pub stderr_file: Option<String>,
    pub env_file: Option<String>,
    pub object_arch: Option<String>,
    pub object_bits: Option<u32>,
    pub riscv_float_abi: Option<String>,
    pub loongarch_float_abi: Option<String>,
    pub asm_contains: Vec<String>,
    pub asm_not_contains: Vec<String>,
}

impl Default for Metadata {
    fn default() -> Self {
        Self {
            mode: "run".into(),
            runner: "native".into(),
            runtime: "portable".into(),
            requires: vec![],
            target: None,
            emit: "obj".into(),
            freestanding: false,
            expected_exit: 0,
            udp_input: false,
            stdin: None,
            stdin_file: None,
            stdout_file: None,
            stderr_file: None,
            env_file: None,
            object_arch: None,
            object_bits: None,
            riscv_float_abi: None,
            loongarch_float_abi: None,
            asm_contains: vec![],
            asm_not_contains: vec![],
        }
    }
}

fn boolean(text: &str) -> Result<bool> {
    match text.to_ascii_lowercase().as_str() {
        "true" | "1" | "yes" => Ok(true),
        "false" | "0" | "no" => Ok(false),
        _ => Err(error("metadata requires true/false")),
    }
}

fn patterns(text: &str) -> Result<Vec<String>> {
    let items: Vec<_> =
        text.split('|').map(str::trim).filter(|s| !s.is_empty()).map(String::from).collect();
    if items.is_empty() {
        Err(error("empty metadata patterns"))
    } else {
        Ok(items)
    }
}

impl Metadata {
    pub fn parse(source: &str) -> Result<Self> {
        let mut values = BTreeMap::new();
        for line in source.lines() {
            let line = line.trim();
            if !line.is_empty() && !line.starts_with("//") {
                break;
            }
            if let Some(body) = line.strip_prefix("// wave-test:") {
                for item in body.split(',') {
                    let (key, value) = item
                        .trim()
                        .split_once('=')
                        .ok_or_else(|| error("malformed wave-test metadata"))?;
                    let key = key.trim();
                    let value = value.trim();
                    if key.is_empty() || value.is_empty() || values.insert(key, value).is_some() {
                        return Err(error("empty or duplicate wave-test metadata"));
                    }
                }
            }
        }
        let mut metadata = Self::default();
        for (key, value) in &values {
            match *key {
                "mode" => metadata.mode = value.to_lowercase(),
                "runner" => metadata.runner = value.to_lowercase(),
                "runtime" => metadata.runtime = value.to_lowercase(),
                "requires" => metadata.requires = patterns(value)?,
                "target" => metadata.target = Some(value.to_string()),
                "emit" => metadata.emit = value.to_lowercase(),
                "freestanding" => metadata.freestanding = boolean(value)?,
                "expected-exit" => metadata.expected_exit = value.parse()?,
                "udp-input" => metadata.udp_input = boolean(value)?,
                "stdin" => metadata.stdin = Some(value.to_string()),
                "stdin-file" => metadata.stdin_file = Some(value.to_string()),
                "stdout-file" => metadata.stdout_file = Some(value.to_string()),
                "stderr-file" => metadata.stderr_file = Some(value.to_string()),
                "env-file" => metadata.env_file = Some(value.to_string()),
                "object-arch" => metadata.object_arch = Some(binary::target_arch(value).into()),
                "object-bits" => metadata.object_bits = Some(value.parse()?),
                "riscv-float-abi" => metadata.riscv_float_abi = Some(value.to_lowercase()),
                "loongarch-float-abi" => metadata.loongarch_float_abi = Some(value.to_lowercase()),
                "asm-contains" => metadata.asm_contains = patterns(value)?,
                "asm-not-contains" => metadata.asm_not_contains = patterns(value)?,
                _ => return Err(error(format!("unsupported wave-test key: {key}"))),
            }
        }
        if !["run", "check", "build"].contains(&metadata.mode.as_str())
            || !["native", "server", "compile"].contains(&metadata.runner.as_str())
            || !["portable", "native", "compile"].contains(&metadata.runtime.as_str())
        {
            return Err(error("invalid execution mode/runner/runtime"));
        }
        if (metadata.mode == "build") != (metadata.runner == "compile")
            || metadata.runner == "server" && metadata.mode != "run"
        {
            return Err(error("incompatible execution mode/runner"));
        }
        if metadata.runner != "compile"
            && values.keys().any(|k| {
                matches!(
                    *k,
                    "target"
                        | "emit"
                        | "freestanding"
                        | "object-arch"
                        | "object-bits"
                        | "riscv-float-abi"
                        | "loongarch-float-abi"
                        | "asm-contains"
                        | "asm-not-contains"
                )
            })
        {
            return Err(error("artifact metadata requires compile runner"));
        }
        if !["obj", "asm", "ir", "bc"].contains(&metadata.emit.as_str()) {
            return Err(error("invalid artifact emit mode"));
        }
        let fixtures = [
            &metadata.stdin_file,
            &metadata.stdout_file,
            &metadata.stderr_file,
            &metadata.env_file,
        ];
        for path in fixtures.into_iter().flatten() {
            cases::relative(path)?;
        }
        if (metadata.stdin.is_some() || fixtures.iter().any(|f| f.is_some()) || metadata.udp_input)
            && (metadata.mode != "run" || metadata.runner != "native")
        {
            return Err(error("fixtures/input require native run mode"));
        }
        if metadata.stdin.is_some() && metadata.stdin_file.is_some() {
            return Err(error("stdin and stdin-file are mutually exclusive"));
        }
        if metadata.requires.iter().collect::<BTreeSet<_>>().len() != metadata.requires.len()
            || metadata.requires.iter().any(|v| {
                !["stdio", "filesystem", "environment", "clock", "sockets", "process"]
                    .contains(&v.as_str())
            })
        {
            return Err(error("invalid or duplicate capability"));
        }
        if metadata.object_bits.is_some_and(|bits| ![32, 64].contains(&bits))
            || metadata.object_arch.as_deref().is_some_and(|a| {
                !["x86_64", "aarch64", "riscv64", "loongarch64", "wasm32", "wasm64"].contains(&a)
            })
        {
            return Err(error("invalid object architecture/width"));
        }
        if metadata
            .riscv_float_abi
            .as_deref()
            .is_some_and(|v| !["lp64", "lp64f", "lp64d"].contains(&v))
            || metadata.riscv_float_abi.is_some()
                && metadata.object_arch.as_deref() != Some("riscv64")
            || metadata
                .loongarch_float_abi
                .as_deref()
                .is_some_and(|v| !["lp64s", "lp64f", "lp64d"].contains(&v))
            || metadata.loongarch_float_abi.is_some()
                && metadata.object_arch.as_deref() != Some("loongarch64")
        {
            return Err(error("invalid floating-point ABI metadata"));
        }
        let object = metadata.object_bits.is_some()
            || metadata.object_arch.is_some()
            || metadata.riscv_float_abi.is_some()
            || metadata.loongarch_float_abi.is_some();
        let assembly = !metadata.asm_contains.is_empty() || !metadata.asm_not_contains.is_empty();
        if object && metadata.emit != "obj"
            || assembly && metadata.emit != "asm"
            || (object || assembly) && metadata.expected_exit != 0
        {
            return Err(error("artifact contract conflicts with emit/exit contract"));
        }
        Ok(metadata)
    }

    pub fn fixture(&self, name: &str) -> Result<String> {
        let root = std::fs::canonicalize(crate::root().join("tests/fixtures"))?;
        let path = std::fs::canonicalize(root.join(cases::relative(name)?))?;
        if !path.starts_with(root) {
            return Err(error("fixture escapes root"));
        }
        Ok(process::normalize(&std::fs::read_to_string(path)?))
    }

    fn input(&self) -> Result<Option<Vec<u8>>> {
        Ok(if let Some(name) = &self.stdin_file {
            Some(self.fixture(name)?.into_bytes())
        } else {
            self.stdin.as_ref().map(|value| format!("{value}\n").into_bytes())
        })
    }

    fn environment(&self) -> Result<BTreeMap<OsString, OsString>> {
        let mut env = BTreeMap::new();
        if let Some(name) = &self.env_file {
            let value = config::parse_json(&self.fixture(name)?)?;
            let Value::Object(fields) = value else {
                return Err(error("environment fixture must be an object"));
            };
            let mut names = BTreeSet::new();
            for (name, value) in fields {
                let Value::String(value) = value else {
                    return Err(error("environment fixture values must be strings"));
                };
                let normalized = if cfg!(windows) { name.to_uppercase() } else { name.clone() };
                if name.is_empty()
                    || name.contains(['=', '\0'])
                    || value.contains('\0')
                    || !names.insert(normalized)
                {
                    return Err(error("invalid or repeated environment fixture name"));
                }
                env.insert(name.into(), value.into());
            }
        }
        Ok(env)
    }

    fn output(&self, result: &process::Output) -> Result<()> {
        for (name, actual) in
            [(&self.stdout_file, &result.stdout), (&self.stderr_file, &result.stderr)]
        {
            if let Some(name) = name {
                let actual = process::normalize(std::str::from_utf8(actual)?);
                let expected = self.fixture(name)?;
                if actual != expected {
                    return Err(error(format!("output fixture mismatch ({name}): expected {expected:?}, actual {actual:?}")));
                }
            }
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct Options {
    pub compiler: PathBuf,
    pub target: Option<cases::Target>,
    pub timeout: Duration,
    pub build_timeout: Duration,
    pub qemu: Option<PathBuf>,
    pub sysroot: Option<PathBuf>,
    pub optimization: String,
    pub expected_stdout: Option<String>,
    pub hosted_abi: bool,
}

pub fn compiler(explicit: Option<PathBuf>) -> Result<PathBuf> {
    if let Some(path) = explicit.or_else(|| std::env::var_os("WAVEC").map(PathBuf::from)) {
        let path = if path.is_file() {
            path
        } else {
            crate::package::which(&path.to_string_lossy())
                .ok_or_else(|| error(format!("compiler is missing: {}", path.display())))?
        };
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            if std::fs::metadata(&path)?.permissions().mode() & 0o111 == 0 {
                return Err(error("compiler is not executable"));
            }
        }
        #[cfg(windows)]
        {
            let triple = if cfg!(target_arch = "aarch64") {
                "aarch64-pc-windows-msvc"
            } else {
                "x86_64-pc-windows-msvc"
            };
            binary::inspect(&path, Some(triple))?;
        }
        return Ok(std::fs::canonicalize(path)?);
    }
    let root = crate::root();
    let mut paths = Vec::new();
    if cfg!(windows) {
        let arch = if cfg!(target_arch = "aarch64") { "aarch64" } else { "x86_64" };
        for mode in ["release", "debug"] {
            paths.push(root.join(format!("target/{arch}-pc-windows-msvc/{mode}/wavec.exe")));
        }
    }
    for mode in ["release", "debug"] {
        paths.push(root.join("target").join(mode).join(if cfg!(windows) {
            "wavec.exe"
        } else {
            "wavec"
        }));
    }
    paths
        .into_iter()
        .find(|p| p.is_file())
        .ok_or_else(|| error("compiler not found; build wavec or provide --wavec"))
}

pub fn protected_inputs(extra: &[PathBuf]) -> Result<Vec<PathBuf>> {
    let root = crate::root();
    let mut inputs = extra.to_vec();
    for folder in ["src", "front", "llvm", "whale", "utils", "std", "tests", "tools", ".github"] {
        inputs.extend(storage::files(&root.join(folder))?);
    }
    for name in [
        "Cargo.toml",
        "Cargo.lock",
        "meson.build",
        "meson.options",
        ".gitignore",
        ".editorconfig",
        "MAINTAINERS",
        "LICENSE",
        "CONTRIBUTING.md",
    ] {
        inputs.push(root.join(name));
    }
    for entry in std::fs::read_dir(&root)? {
        let path = entry?.path();
        let name = path.file_name().unwrap().to_string_lossy();
        if path.is_file()
            && (name.ends_with(".wave")
                || name.starts_with("wave-v")
                    && [".tar.gz", ".zip", ".sha256", ".metadata.json"]
                        .iter()
                        .any(|suffix| name.ends_with(suffix)))
        {
            inputs.push(path);
        }
    }
    Ok(inputs)
}

fn command(
    row: &mut Value,
    phase: &str,
    spec: &process::Spec,
    expected: i32,
) -> Result<process::Output> {
    config::field(row, "phase", Value::string(phase))?;
    let output = match spec.run() {
        Ok(output) => output,
        Err(failure) => {
            config::field(row, "reason", Value::string(failure.to_string()))?;
            return Err(failure);
        },
    };
    let status = if output.failure.as_deref() == Some("timeout") {
        "timeout"
    } else if output.failure.as_deref() == Some("cancelled") {
        "interrupted"
    } else if output.failure.is_some() || output.status != expected {
        "fail"
    } else {
        "pass"
    };
    let record = Value::object([
        ("command", Value::strings(spec.args.iter().map(|a| a.to_string_lossy()))),
        ("phase", Value::string(phase)),
        ("expected_exit", config::number(expected as i64)),
        ("actual_exit", config::number(output.status as i64)),
        ("status", Value::string(status)),
        ("stdout", Value::string(output.text_stdout().chars().take(4096).collect::<String>())),
        ("stderr", Value::string(output.text_stderr().chars().take(4096).collect::<String>())),
        ("stdout_truncated", Value::Bool(output.text_stdout().chars().count() > 4096)),
        ("stderr_truncated", Value::Bool(output.text_stderr().chars().count() > 4096)),
        ("timeout_seconds", Value::integer(spec.timeout.as_secs())),
    ]);
    for name in [
        "phase",
        "expected_exit",
        "actual_exit",
        "stdout",
        "stderr",
        "stdout_truncated",
        "stderr_truncated",
        "timeout_seconds",
    ] {
        config::field(row, name, record.get(name).unwrap().clone())?;
    }
    if let Value::Object(fields) = row {
        let commands = fields.iter_mut().find(|(k, _)| k == "commands").unwrap();
        let Value::Array(commands) = &mut commands.1 else { unreachable!() };
        commands.push(record);
    }
    if let Some(reason) = &output.failure {
        return Err(error(format!("{phase}: {reason}")));
    }
    if output.status != expected {
        return Err(error(format!(
            "{phase}: exit {}, expected {expected}: {}",
            output.status,
            output.text_stderr()
        )));
    }
    Ok(output)
}

fn verify_artifact(path: &Path, metadata: &Metadata, target: Option<&str>) -> Result<()> {
    if !path.is_file() {
        return Err(error(format!("compiler did not produce artifact {}", path.display())));
    }
    if metadata.emit == "obj" {
        if target.is_some_and(|v| v.starts_with("wasm"))
            || metadata.object_arch.as_deref().is_some_and(|a| a.starts_with("wasm"))
        {
            return crate::wasm::verify(
                path,
                metadata
                    .object_bits
                    .or_else(|| target.map(|t| if t.starts_with("wasm64") { 64 } else { 32 })),
            );
        }
        let info = binary::inspect(path, target)?;
        if metadata.object_bits.is_some_and(|bits| bits != info.bits)
            || metadata.object_arch.as_ref().is_some_and(|a| !info.architectures.contains(a))
        {
            return Err(error("artifact architecture/width mismatch"));
        }
        if let Some(abi) = &metadata.riscv_float_abi {
            let expected = match abi.as_str() {
                "lp64" => 0,
                "lp64f" => 2,
                _ => 4,
            };
            if info.flags & 6 != expected {
                return Err(error("RISC-V floating ABI mismatch"));
            }
        }
        if let Some(abi) = &metadata.loongarch_float_abi {
            let expected = match abi.as_str() {
                "lp64s" => 1,
                "lp64f" => 2,
                _ => 3,
            };
            if info.flags & 7 != expected {
                return Err(error("LoongArch floating ABI mismatch"));
            }
        }
    }
    if metadata.emit == "asm" {
        let assembly = assembly_code(
            &std::fs::read_to_string(path)?,
            target.unwrap_or("x86_64-unknown-linux-gnu"),
        )?;
        for (patterns, present) in
            [(&metadata.asm_contains, true), (&metadata.asm_not_contains, false)]
        {
            for pattern in patterns {
                let token = regex_lite::Regex::new(&format!(
                    r"(?m)(?:^|[^A-Za-z0-9_.$]){}(?:$|[^A-Za-z0-9_.$])",
                    regex_lite::escape(pattern)
                ))?;
                if token.is_match(&assembly) != present {
                    return Err(error(format!("assembly token contract failed: {pattern}")));
                }
            }
        }
    }
    Ok(())
}

fn run_one(source: &Path, metadata: &Metadata, options: &Options, row: &mut Value) -> Result<()> {
    // Hosted ABI smoke is an explicit runtime probe, independent of the same
    // source's normal compile-only assembly contract in the complete case suite.
    let hosted = Metadata::default();
    let metadata = if options.hosted_abi { &hosted } else { metadata };
    let output = tempfile::Builder::new().prefix("wave-case-").tempdir()?;
    let target = metadata
        .target
        .clone()
        .map(Ok)
        .or_else(|| {
            options
                .target
                .as_ref()
                .map(|t| t.triple.clone().map(Ok).unwrap_or_else(|| t.rust_target()))
        })
        .transpose()?;
    let mode = options.target.as_ref().map(|t| t.executor.as_str()).unwrap_or("native");
    let compile = metadata.runner == "compile"
        || mode == "compile"
        || ["qemu", "wasm"].contains(&mode)
            && options.target.as_ref().is_some_and(|target| !runtime_eligible(metadata, target));
    if compile && options.expected_stdout.is_some() {
        return Err(error("an stdout contract requires actual runtime execution"));
    }
    let emit = if compile { metadata.emit.as_str() } else { "exe" };
    let suffix = match emit {
        "obj" => "o",
        "asm" => "s",
        "ir" => "ll",
        "bc" => "bc",
        _ => {
            if target.as_deref().is_some_and(|t| t.starts_with("wasm")) {
                "wasm"
            } else if cfg!(windows) {
                "exe"
            } else {
                "bin"
            }
        },
    };
    let named_output = !compile || emit == "obj";
    let stem = if named_output {
        std::ffi::OsStr::new("case")
    } else {
        source.file_stem().ok_or_else(|| error("case source has no stem"))?
    };
    let mut artifact_name = stem.to_os_string();
    artifact_name.push(".");
    artifact_name.push(suffix);
    let artifact = output.path().join(artifact_name);
    let mut args: Vec<OsString> = vec![
        options.compiler.clone().into_os_string(),
        if metadata.mode == "check" { "check" } else { "build" }.into(),
        source.as_os_str().into(),
        "--std-root".into(),
        crate::root().join("std").into_os_string(),
        "--out-dir".into(),
        output.path().as_os_str().into(),
    ];
    if metadata.mode != "check" {
        args.push(options.optimization.clone().into());
        // Wave accepts -o for a binary or one object, but names assembly,
        // textual IR and bitcode from the input stem inside --out-dir.
        if named_output {
            args.extend(["-o".into(), artifact.clone().into_os_string()]);
        }
    }
    if let Some(target) = &target {
        args.extend(["--target".into(), target.into()]);
    }
    if let Some(sysroot) = &options.sysroot {
        args.extend(["--sysroot".into(), sysroot.clone().into_os_string()]);
    }
    if compile {
        args.push(format!("--emit={emit}").into());
    } else if ["qemu", "wasm"].contains(&mode) {
        args.push("--emit=obj,bin".into());
    }
    if metadata.freestanding {
        args.push("--freestanding".into());
    }
    let mut spec = process::Spec::new(args);
    spec.timeout = options.build_timeout;
    command(row, if metadata.mode == "check" { "check" } else { "build" }, &spec, 0)?;
    if metadata.mode == "check" {
        return Ok(());
    }
    if !artifact.is_file() {
        return Err(error("build succeeded without expected output"));
    }
    if compile {
        config::field(row, "phase", Value::string("artifact"))?;
        let mut metadata = metadata.clone();
        metadata.emit = emit.into();
        return verify_artifact(&artifact, &metadata, target.as_deref());
    }
    if ["qemu", "wasm"].contains(&mode) {
        let mut contract = metadata.clone();
        contract.emit = "obj".into();
        let object = output.path().join(format!(
            "{}.o",
            source.file_stem().ok_or_else(|| error("case source has no stem"))?.to_string_lossy()
        ));
        verify_artifact(&object, &contract, target.as_deref())?;
    }
    if options.hosted_abi {
        let target =
            target.as_deref().ok_or_else(|| error("hosted ABI validation requires a target"))?;
        let info = binary::inspect(&artifact, Some(target))?;
        let (flags, interpreter) = match binary::target_arch(target) {
            "riscv64" => (0x5, "/lib/ld-linux-riscv64-lp64d.so.1"),
            "loongarch64" => (0x43, "/lib64/ld-linux-loongarch-lp64d.so.1"),
            _ => return Err(error("hosted smoke supports RISC-V64 and LoongArch64")),
        };
        if info.flags != flags || info.interpreter.as_deref() != Some(interpreter) {
            return Err(error("hosted executable ABI flags or interpreter mismatch"));
        }
    }
    let runtime: Vec<OsString> = match mode {
        "native" => vec![artifact.clone().into_os_string()],
        "qemu" => vec![
            options
                .qemu
                .clone()
                .ok_or_else(|| error("QEMU executable is required"))?
                .into_os_string(),
            "-L".into(),
            options
                .sysroot
                .clone()
                .ok_or_else(|| error("QEMU sysroot is required"))?
                .into_os_string(),
            artifact.clone().into_os_string(),
        ],
        "wasm" => {
            let mut planned = spec.clone();
            planned.args.extend(["--run".into(), "--dry-run".into(), "--error-format=json".into()]);
            let planned = command(row, "plan", &planned, 0)?;
            let value = config::parse_json(&planned.strict_stdout()?)?;
            crate::wasm::execution_command(&value, &artifact)?
        },
        _ => return Err(error("unsupported executor")),
    };
    let mut spec = process::Spec::new(runtime);
    spec.timeout = options.timeout;
    spec.input = metadata.input()?;
    spec.env = metadata.environment()?;
    if metadata.runner == "server" {
        return server(spec, row);
    }
    let stopped = Arc::new(AtomicBool::new(false));
    let udp = if metadata.udp_input {
        let stopped = stopped.clone();
        Some(std::thread::spawn(move || {
            if let Ok(socket) = std::net::UdpSocket::bind("127.0.0.1:0") {
                while !stopped.load(Ordering::SeqCst) {
                    let _ = socket.send_to(b"hello from python\n", "127.0.0.1:8080");
                    std::thread::sleep(Duration::from_millis(100));
                }
            }
        }))
    } else {
        None
    };
    let result = command(row, "run", &spec, metadata.expected_exit);
    stopped.store(true, Ordering::SeqCst);
    if let Some(udp) = udp {
        let _ = udp.join();
    }
    let result = result?;
    metadata.output(&result)?;
    if let Some(expected) = &options.expected_stdout {
        if result.strict_stdout()?.trim_end_matches(['\r', '\n']) != expected {
            return Err(error(format!("runtime stdout did not match {expected:?}")));
        }
    }

    Ok(())
}

fn server(mut spec: process::Spec, row: &mut Value) -> Result<()> {
    use std::io::{Read, Write};
    let directory = tempfile::tempdir()?;
    let log = directory.path().join("server.log");
    spec.stdout_log = Some(log.clone());
    let stop = Arc::new(AtomicBool::new(false));
    spec.stop = Some(stop.clone());
    let started = Instant::now();
    let timeout = spec.timeout;
    let task = std::thread::spawn(move || spec.run());
    let outcome = (|| -> Result<()> {
        let port = loop {
            if task.is_finished() {
                return Err(error("server exited before readiness"));
            }
            if started.elapsed() >= timeout {
                return Err(error("server readiness timed out"));
            }
            let text = std::fs::read_to_string(&log).unwrap_or_default();
            if text.lines().any(|l| {
                matches!(l.trim(), "WAVE-SERVER-UNAVAILABLE -1" | "WAVE-SERVER-UNAVAILABLE -13")
            }) {
                return Err(error("server local sockets unavailable"));
            }
            if let Some(line) = text.lines().find_map(|l| l.strip_prefix("WAVE-SERVER-READY ")) {
                let port = line.trim().parse::<u16>()?;
                if port == 0 {
                    return Err(error("invalid server port"));
                }
                break port;
            }
            std::thread::sleep(Duration::from_millis(20));
        };
        let mut random = [0u8; 16];
        getrandom::fill(&mut random)
            .map_err(|e| error(format!("server challenge entropy: {e}")))?;
        let token = storage::bytes_digest(&random);
        let request = format!("GET /{} HTTP/1.1\r\nHost: localhost\r\n\r\n", &token[..32]);
        let mut socket = std::net::TcpStream::connect_timeout(
            &std::net::SocketAddr::from(([127, 0, 0, 1], port)),
            timeout.saturating_sub(started.elapsed()).max(Duration::from_millis(1)),
        )?;
        socket.set_read_timeout(Some(
            timeout.saturating_sub(started.elapsed()).max(Duration::from_millis(1)),
        ))?;
        socket.write_all(request.as_bytes())?;
        let mut response = Vec::new();
        socket.take(8192).read_to_end(&mut response)?;
        let response = String::from_utf8(response)?;
        let body = response.split_once("\r\n\r\n").map(|(_, b)| b);
        if !response.starts_with("HTTP/1.1 200 OK\r\n")
            || body != Some(format!("Welcome to the Wave HTTP Server!\n{request}").as_str())
            || task.is_finished()
        {
            return Err(error("owned server failed HTTP challenge"));
        }
        Ok(())
    })();
    stop.store(true, Ordering::SeqCst);
    let result = task.join().map_err(|_| error("server owner panicked"))??;
    config::field(row, "stdout", Value::string(result.text_stdout()))?;
    config::field(row, "stderr", Value::string(result.text_stderr()))?;
    outcome
}

pub fn execute(
    sources: &[PathBuf],
    options: &Options,
    report: Option<&Path>,
    selection: Value,
) -> Result<i32> {
    if let Some(report) = report {
        let mut inputs = protected_inputs(std::slice::from_ref(&options.compiler))?;
        inputs.extend_from_slice(sources);
        storage::validate_output(report, &inputs)?;
    }
    let mut rows: Vec<_> = sources
        .iter()
        .map(|p| {
            Value::object([
                (
                    "name",
                    Value::string(p.strip_prefix(crate::root()).unwrap_or(p).to_string_lossy()),
                ),
                ("status", Value::string("not_run")),
                ("commands", Value::Array(vec![])),
            ])
        })
        .collect();
    let save = |rows: &[Value]| -> Result<()> {
        if let Some(path) = report {
            let summary = Value::object(
                ["pass", "skip", "fail", "timeout", "interrupted", "not_run"].map(|status| {
                    (
                        status,
                        Value::integer(
                            rows.iter().filter(|r| r.get_str("status") == Some(status)).count()
                                as u64,
                        ),
                    )
                }),
            );
            let value = Value::object([
                ("schema_version", Value::integer(1)),
                (
                    "phase",
                    Value::string(
                        options.target.as_ref().map(|t| t.executor.as_str()).unwrap_or("native"),
                    ),
                ),
                ("compiler", Value::string(options.compiler.to_string_lossy())),
                (
                    "host",
                    Value::object([
                        ("os", Value::string(std::env::consts::OS)),
                        ("arch", Value::string(cases::normalize_arch(std::env::consts::ARCH))),
                    ]),
                ),
                ("selection", selection.clone()),
                ("summary", summary),
                ("tests", Value::Array(rows.to_vec())),
            ]);
            storage::atomic_write(path, config::json(&value)?.as_bytes())?;
        }
        Ok(())
    };
    save(&rows)?;
    for (index, source) in sources.iter().enumerate() {
        if process::cancelled() != 0 {
            break;
        }
        let row = &mut rows[index];
        println!("RUN {}", source.display());
        let result = (|| {
            let metadata = Metadata::parse(&std::fs::read_to_string(source)?)?;
            run_one(source, &metadata, options, row)
        })();
        match result {
            Ok(()) => {
                config::field(row, "status", Value::string("pass"))?;
            },
            Err(e) => {
                eprintln!("FAIL {}: {e}", source.display());
                config::field(row, "reason", Value::string(e.to_string()))?;
                config::field(
                    row,
                    "status",
                    Value::string(if process::cancelled() != 0 {
                        "interrupted"
                    } else if e.to_string().contains("timeout") {
                        "timeout"
                    } else {
                        "fail"
                    }),
                )?;
            },
        }
        save(&rows)?;
    }
    save(&rows)?;
    Ok(if process::cancelled() != 0 {
        process::cancelled() as i32
    } else {
        i32::from(rows.iter().any(|r| r.get_str("status") != Some("pass")))
    })
}

pub fn main(arguments: &[OsString], corpus: bool) -> Result<i32> {
    let mut args = crate::cli::Args::new(arguments);
    let compiler = compiler(args.path("--wavec")?)?;
    let report = args.path("--report-json")?;
    let target = args.text("--target-id")?;
    let explicit_target = target.is_some();
    let shard = args.text("--shard-index")?.unwrap_or_else(|| "0".into()).parse::<usize>()?;
    let shards = args.text("--shard-count")?.unwrap_or_else(|| "1".into()).parse::<usize>()?;
    if !(1..=256).contains(&shards) || shard >= shards {
        return Err(error("invalid case shard"));
    }
    let timeout = crate::cli::timeout(args.text("--timeout")?, 30)?;
    let compile_only = args.flag("--compile-only");
    let runtime_only = args.flag("--runtime-only");
    if compile_only && runtime_only {
        return Err(error("compile-only and runtime-only are mutually exclusive"));
    }
    let optimization = args.text("--optimization")?.unwrap_or_else(|| "-O0".into());
    let examples = args.flag("--run-std-examples");
    let expected_stdout = args.text("--stdout-exact")?;
    let hosted_abi = args.flag("--hosted-abi");
    let mut qemu = args.path("--qemu")?;
    let mut sysroot = args.path("--sysroot")?;
    let mut suites = Vec::new();
    while let Some(suite) = args.text("--suite")? {
        suites.push(suite);
    }
    let mut only = Vec::new();
    while let Some(name) = args.text("--only")? {
        only.push(name);
    }
    let mut skip = Vec::new();
    while let Some(name) = args.text("--skip")? {
        skip.push(name);
    }
    let mut explicit = Vec::new();
    while let Some(source) = args.path("--source")? {
        explicit.push(source);
    }
    args.finish()?;
    let manifest = cases::Manifest::load(&crate::root(), true)?;
    if target.is_some() && !suites.is_empty() {
        return Err(error("--suite and --target-id cannot be combined"));
    }
    for suite in &suites {
        let path = manifest.root.join(cases::relative(suite)?);
        if !path.is_dir() {
            return Err(error(format!("unknown case suite: {suite}")));
        }
    }
    let mut target = if corpus || !suites.is_empty() {
        None
    } else if let Some(id) = target {
        Some(manifest.target(&id)?.clone())
    } else {
        Some(manifest.native()?.clone())
    };
    let sources = if !explicit.is_empty() {
        explicit
    } else if corpus {
        let mut sources = Vec::new();
        for folder in ["std", "examples"] {
            sources.extend(
                storage::files(&crate::root().join(folder))?
                    .into_iter()
                    .filter(|p| p.extension().is_some_and(|e| e == "wave")),
            );
        }
        sources
    } else {
        let mut sources = manifest.sources(target.as_ref())?;
        if !suites.is_empty() {
            sources = manifest.sources(None)?;
            sources.retain(|p| {
                suites.iter().any(|s| {
                    p.parent().is_some_and(|parent| {
                        parent == manifest.root.join(s)
                            || parent.parent().is_some_and(|parent| parent == manifest.root.join(s))
                    })
                })
            });
        }
        sources
    };
    let case_name = |path: &Path| -> String {
        let path = path.strip_prefix(&manifest.root).unwrap_or(path);
        let path = if path.file_name().is_some_and(|v| v == "main.wave") {
            path.parent().unwrap()
        } else {
            path
        };
        path.to_string_lossy().replace('\\', "/")
    };
    for name in &only {
        if !sources.iter().any(|path| case_name(path) == *name) {
            return Err(error(format!("unknown selected case: {name}")));
        }
    }
    let mut filtered = Vec::new();
    for source in sources {
        let name = case_name(&source);
        if (!only.is_empty() && !only.contains(&name)) || skip.contains(&name) {
            continue;
        }
        if runtime_only {
            let metadata = Metadata::parse(&std::fs::read_to_string(&source)?)?;
            if !target.as_ref().is_some_and(|target| runtime_eligible(&metadata, target)) {
                continue;
            }
        }
        filtered.push(source);
    }
    let sources: Vec<_> = filtered
        .into_iter()
        .enumerate()
        .filter_map(|(index, path)| (index % shards == shard).then_some(path))
        .collect();
    if compile_only {
        if let Some(target) = &mut target {
            target.executor = "compile".into();
        } else {
            return Err(error("compile-only requires a manifest target"));
        }
    }
    if sources.is_empty() {
        return Err(error("empty test selection"));
    }
    let selection = Value::object([
        (
            "mode",
            Value::string(if corpus {
                "corpus"
            } else if !suites.is_empty() {
                "suites"
            } else if explicit_target {
                "target"
            } else {
                "auto"
            }),
        ),
        (
            "executor",
            Value::string(target.as_ref().map(|t| t.executor.as_str()).unwrap_or("native")),
        ),
        (
            "target",
            Value::optional_string(
                target.as_ref().map(|t| t.rust_target()).transpose()?.as_deref(),
            ),
        ),
        (
            "suites",
            Value::strings(if suites.is_empty() {
                target.as_ref().map(|t| t.suites()).unwrap_or_default()
            } else {
                suites.sort();
                suites.dedup();
                suites.clone()
            }),
        ),
        ("id", Value::optional_string(target.as_ref().map(|t| t.id.as_str()))),
        ("shard_index", Value::integer(shard as u64)),
        ("shard_count", Value::integer(shards as u64)),
    ]);
    if let Some(target) = &target {
        if target.executor == "qemu" {
            if qemu.is_none() {
                qemu = Some(
                    if target.arch == "loong64" { "qemu-loongarch64" } else { "qemu-riscv64" }
                        .into(),
                );
            }
            if sysroot.is_none() {
                let triple =
                    target.triple.as_ref().ok_or_else(|| error("QEMU target has no triple"))?;
                let output = crate::package::run([
                    compiler.as_os_str(),
                    "print".as_ref(),
                    "sysroot".as_ref(),
                    "--target".as_ref(),
                    triple.as_ref(),
                ])?;
                let root = PathBuf::from(output.trim());
                if !root.is_dir() {
                    return Err(error("compiler sysroot does not exist"));
                }
                if target.arch == "loong64"
                    && std::env::var_os("WAVE_LOONGARCH64_SYSROOT")
                        .is_none_or(|value| Path::new(&value) != root)
                {
                    return Err(error("LoongArch sysroot differs from the pinned toolchain"));
                }
                sysroot = Some(root);
            }
        }
    }
    let options = Options {
        compiler,
        target,
        timeout,
        build_timeout: timeout.max(Duration::from_secs(30)),
        qemu,
        sysroot,
        optimization,
        expected_stdout,
        hosted_abi,
    };
    if corpus {
        return corpus_run(&sources, &options, report.as_deref(), selection, examples);
    }
    execute(&sources, &options, report.as_deref(), selection)
}

fn corpus_run(
    sources: &[PathBuf],
    options: &Options,
    report: Option<&Path>,
    selection: Value,
    examples: bool,
) -> Result<i32> {
    if let Some(report) = report {
        let mut inputs = protected_inputs(std::slice::from_ref(&options.compiler))?;
        inputs.extend_from_slice(sources);
        storage::validate_output(report, &inputs)?;
    }
    let mut rows = Vec::new();
    for source in sources {
        if process::cancelled() != 0 {
            break;
        }
        let mut row = Value::object([
            ("name", Value::string(source.strip_prefix(crate::root())?.to_string_lossy())),
            ("phase", Value::string("check")),
            ("status", Value::string("not_run")),
            ("commands", Value::Array(vec![])),
        ]);
        let mut spec = process::Spec::new([
            options.compiler.as_os_str(),
            std::ffi::OsStr::new("--std-root"),
            crate::root().join("std").as_os_str(),
            std::ffi::OsStr::new("check"),
            source.as_os_str(),
        ]);
        spec.timeout = options.build_timeout;
        let result = command(&mut row, "check", &spec, 0);
        config::field(
            &mut row,
            "status",
            Value::string(if result.is_ok() { "pass" } else { "fail" }),
        )?;
        if let Err(e) = result {
            config::field(&mut row, "reason", Value::string(e.to_string()))?;
        }
        rows.push(row);
    }
    if examples {
        let mut network = None;
        for source in sources
            .iter()
            .filter(|p| p.parent() == Some(crate::root().join("examples/std").as_path()))
        {
            let mut row = Value::object([
                ("name", Value::string(source.strip_prefix(crate::root())?.to_string_lossy())),
                ("phase", Value::string("run")),
                ("status", Value::string("not_run")),
                ("commands", Value::Array(vec![])),
            ]);
            if process::cancelled() != 0 {
                break;
            }
            let result = (|| {
                let metadata = Metadata::parse(&std::fs::read_to_string(source)?)?;
                if metadata.mode != "run"
                    || metadata.runner != "native"
                    || metadata.udp_input
                    || metadata.runtime == "compile"
                {
                    return Err(error(
                        "standard-library examples require native run mode without udp-input",
                    ));
                }
                if metadata.requires.iter().any(|cap| cap == "sockets") {
                    if network.is_none() {
                        network = Some(local_network_unavailable()?);
                    }
                    if let Some(reason) = network.as_ref().unwrap() {
                        config::field(&mut row, "status", Value::string("skip"))?;
                        config::field(&mut row, "reason", Value::string(reason))?;
                        return Ok(());
                    }
                }
                run_one(source, &metadata, options, &mut row)
            })();
            let skipped = row.get_str("status") == Some("skip");
            config::field(
                &mut row,
                "status",
                Value::string(if skipped {
                    "skip"
                } else if result.is_ok() {
                    "pass"
                } else if process::cancelled() != 0 {
                    "interrupted"
                } else {
                    "fail"
                }),
            )?;
            if let Err(e) = result {
                config::field(&mut row, "reason", Value::string(e.to_string()))?;
            }
            rows.push(row);
        }
    }
    if let Some(path) = report {
        storage::atomic_write(
            path,
            config::json(&Value::object([
                ("schema_version", Value::integer(1)),
                ("selection", selection),
                ("compiler", Value::string(options.compiler.to_string_lossy())),
                (
                    "summary",
                    Value::object(["pass", "fail", "timeout", "skip", "interrupted"].map(
                        |status| {
                            (
                                status,
                                Value::integer(
                                    rows.iter()
                                        .filter(|row| row.get_str("status") == Some(status))
                                        .count() as u64,
                                ),
                            )
                        },
                    )),
                ),
                ("tests", Value::Array(rows.clone())),
            ]))?
            .as_bytes(),
        )?;
    }
    let failed =
        rows.iter().filter(|r| !matches!(r.get_str("status"), Some("pass" | "skip"))).count();
    println!("Corpus: {} records, {failed} failed", rows.len());
    Ok(if process::cancelled() != 0 { process::cancelled() as i32 } else { i32::from(failed > 0) })
}

/// Probe only local socket permission/family support. Other errors remain failures.
pub fn local_network_unavailable() -> Result<Option<String>> {
    let result = (|| -> std::io::Result<()> {
        let _tcp = std::net::TcpListener::bind("127.0.0.1:0")?;
        let _udp = std::net::UdpSocket::bind("127.0.0.1:0")?;
        Ok(())
    })();
    match result {
        Ok(()) => Ok(None),
        Err(e) => {
            #[cfg(unix)]
            let unsupported = {
                use nix::errno::Errno;
                [
                    Errno::EPERM,
                    Errno::EACCES,
                    Errno::EAFNOSUPPORT,
                    Errno::EPROTONOSUPPORT,
                    Errno::ENOSYS,
                ]
                .iter()
                .any(|code| Some(*code as i32) == e.raw_os_error())
            };
            #[cfg(windows)]
            let unsupported = matches!(e.raw_os_error(), Some(10013 | 10043 | 10047));
            #[cfg(not(any(unix, windows)))]
            let unsupported = false;
            if unsupported || e.kind() == std::io::ErrorKind::PermissionDenied {
                Ok(Some(format!("local sockets unavailable: {e}")))
            } else {
                Err(e.into())
            }
        },
    }
}

pub fn runtime_eligible(metadata: &Metadata, target: &cases::Target) -> bool {
    metadata.mode == "run"
        && metadata.runner == "native"
        && metadata.runtime != "compile"
        && (metadata.runtime != "native" || ["native", "qemu"].contains(&target.executor.as_str()))
        && metadata.requires.iter().all(|cap| target.capabilities.contains(cap))
        && metadata.stdin.is_none()
        && metadata.stdin_file.is_none()
        && !metadata.udp_input
        && metadata.expected_exit == 0
}

pub fn assembly_code(source: &str, target: &str) -> Result<String> {
    let arch = binary::target_arch(target);
    let comment = if arch == "aarch64" {
        if target.contains("darwin") || target.contains("apple") {
            ";"
        } else {
            "//"
        }
    } else {
        "#"
    };
    let blocks = regex_lite::Regex::new(r"(?s)/\*.*?\*/")?;
    let source = blocks.replace_all(source, " ");
    Ok(source
        .lines()
        .filter_map(|line| {
            let line = line.split(comment).next().unwrap().trim();
            let ignored = line.is_empty()
                || line.starts_with('.')
                || line.ends_with(':')
                || arch == "aarch64" && line.starts_with('#');
            (!ignored).then_some(line)
        })
        .collect::<Vec<_>>()
        .join("\n"))
}
