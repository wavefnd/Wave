// SPDX-License-Identifier: MPL-2.0
use crate::{binary, cli::Args, config, error, package, process::Spec, storage, Result};
use std::ffi::OsString;
use std::path::{Path, PathBuf};
use utils::wson::Value;

pub fn std_policy() -> Result<i32> {
    let data =
        Spec::new(["git", "ls-files", "-z", "-c", "-o", "--exclude-standard", "--", "*.wave"])
            .checked()?
            .stdout;
    let approved = regex_lite::Regex::new(
        r"^(?:std/sys/(linux|macos|freebsd)/((amd64|arm64|riscv64)/memory|resolver|interfaces|vector_io|event)\.wave|std/sys/wasm/(env|fs|io|memory|process|time)\.wave|std/sys/wasm64/memory\.wave|std/sys/macos/common/env\.wave)$",
    )?;
    let mut failed = false;
    for path in data.split(|b| *b == 0).filter(|p| !p.is_empty()) {
        let relative = std::str::from_utf8(path)?;
        let source = std::fs::read_to_string(crate::root().join(relative))?;
        let tokens = crate::source::tokens(&source)
            .map_err(|e| error(format!("policy tokenization failed: {relative}: {e}")))?;
        for (index, token) in tokens.iter().enumerate() {
            if token.kind != crate::source::Kind::Word {
                continue;
            }
            let value = token.lexeme.as_str();
            let next = |offset: usize| tokens.get(index + offset).map(|t| t.lexeme.as_str());
            let reason = if relative.starts_with("std/")
                && value == "extern"
                && next(1) == Some("(")
                && next(2) == Some("c")
                && !approved.is_match(relative)
            {
                Some("extern(c) outside approved OS providers")
            } else if relative.starts_with("std/")
                && value == "import"
                && next(1) == Some("(")
                && next(2).is_some_and(|v| {
                    v.trim_matches('"') == "std::libc"
                        || v.trim_matches('"').starts_with("std::libc::")
                })
            {
                Some("removed std::libc import")
            } else if value == "let"
                && (index == 0
                    || tokens[index - 1].line < token.line
                    || ["(", "{", ";", "}"].contains(&tokens[index - 1].lexeme.as_str()))
            {
                let name = index + 1 + usize::from(next(1) == Some("mut"));
                if tokens.get(name + 1).is_some_and(|t| t.lexeme == ":") {
                    Some("retired let declaration")
                } else {
                    None
                }
            } else {
                None
            };
            if let Some(reason) = reason {
                failed = true;
                eprintln!("{relative}:{}: {reason}", token.line);
            }
        }
    }
    println!("std policy: {}", if failed { "FAILED" } else { "OK" });
    Ok(i32::from(failed))
}

pub fn binary(arguments: &[OsString]) -> Result<i32> {
    let mut args = Args::new(arguments);
    let target = args.text("--target")?;
    let rpath = args.text("--rpath")?;
    let json = args.flag("--json");
    if args.values.len() != 1 {
        return Err(error("binary check requires one path"));
    }
    let path = PathBuf::from(&args.values[0]);
    let info = binary::inspect(&path, target.as_deref())?;
    if let Some(expected) = rpath {
        package::require_rpath(&path, &expected)?;
    }
    if json {
        print!(
            "{}",
            config::json(&Value::object([
                ("format", Value::string(info.format)),
                ("architectures", Value::strings(info.architectures)),
                ("bits", Value::integer(info.bits as u64)),
                ("flags", Value::integer(info.flags as u64)),
                ("needed", Value::strings(info.needed)),
                ("rpaths", Value::strings(info.rpaths))
            ]))?
        );
    } else {
        println!("Verified {}", path.display());
    }
    Ok(0)
}

pub fn release_assets(
    directory: &Path,
    version: &str,
    source: Option<&str>,
    expected_revision: Option<u64>,
) -> Result<usize> {
    crate::publication::validate_version(version, false)?;
    if let Some(source) = source {
        crate::publication::sha(source)?;
    }
    let manifest = crate::cases::Manifest::load(&crate::root(), false)?;
    let mut expected = std::collections::BTreeSet::new();
    let mut identities = Vec::new();
    for target in manifest.targets.iter().filter(|t| {
        t.enabled
            && (t.os == "linux"
                || t.os == "macos"
                || t.os == "windows"
                || t.os == "freebsd" && t.arch == "amd64")
    }) {
        let target = target.archive_target()?;
        let name = format!(
            "wave-v{version}-{target}{}",
            if target.contains("windows") { ".zip" } else { ".tar.gz" }
        );
        for suffix in ["", ".sha256"] {
            expected.insert(format!("{name}{suffix}"));
        }
        let archive = directory.join(&name);
        let checksum = directory.join(format!("{name}.sha256"));
        if std::fs::symlink_metadata(&archive)?.is_symlink()
            || std::fs::symlink_metadata(&checksum)?.is_symlink()
            || !archive.is_file()
            || !checksum.is_file()
        {
            return Err(error("release assets must be regular files"));
        }
        let text = std::fs::read_to_string(&checksum)?;
        let records: Vec<_> = text.lines().collect();
        if records.len() != 1 || records[0].len() < 66 {
            return Err(error("malformed checksum record"));
        }
        let (digest, filename) = records[0].split_at(64);
        if !digest.bytes().all(|b| b.is_ascii_hexdigit())
            || ![format!("  {name}"), format!(" *{name}")].contains(&filename.to_string())
            || storage::digest(&archive)? != digest.to_ascii_lowercase()
        {
            return Err(error(format!("archive/checksum mismatch: {name}")));
        }
        if let Some(source) = source {
            let metadata_path = directory.join(format!("{name}.metadata.json"));
            if std::fs::symlink_metadata(&metadata_path)?.is_symlink() {
                return Err(error("metadata must be a regular file"));
            }
            let metadata = config::read_json(&metadata_path)?;
            if metadata.get_u64("schema_version") != Some(1)
                || metadata.get_str("compiler_version") != Some(version)
                || metadata.get_str("source_sha") != Some(source)
                || metadata.get_str("archive") != Some(&name)
                || metadata.get_str("sha256") != Some(digest)
            {
                return Err(error(format!("package identity mismatch: {name}")));
            }
            let original = config::text(&metadata, "target")?;
            if original.replace("riscv64gc-", "riscv64-").replace("-unknown-linux", "-linux")
                != target
            {
                return Err(error("package target identity mismatch"));
            }
            let revision = metadata
                .get_u64("std_compatibility_revision")
                .ok_or_else(|| error("invalid std revision"))?;
            if expected_revision.is_some_and(|expected| expected != revision)
                || identities.first().is_some_and(|previous| *previous != revision)
            {
                return Err(error("std compatibility identity mismatch"));
            }
            identities.push(revision);
            let Value::Object(fields) = package::contract(original) else { unreachable!() };
            for (key, value) in fields {
                if metadata.get(&key).is_none_or(|actual| !config::equivalent(actual, &value)) {
                    return Err(error(format!("package {key} contract mismatch")));
                }
            }
        }
    }
    let actual: std::collections::BTreeSet<_> = std::fs::read_dir(directory)?
        .map(|e| e.map(|e| e.file_name().to_string_lossy().to_string()))
        .collect::<std::io::Result<Vec<_>>>()?
        .into_iter()
        .filter(|n| n.ends_with(".tar.gz") || n.ends_with(".zip") || n.ends_with(".sha256"))
        .collect();
    if actual != expected {
        return Err(error("unexpected/missing release archives/checksums"));
    }
    if source.is_some() {
        let actual: std::collections::BTreeSet<_> = std::fs::read_dir(directory)?
            .map(|e| e.map(|e| e.file_name().to_string_lossy().to_string()))
            .collect::<std::io::Result<Vec<_>>>()?
            .into_iter()
            .filter(|n| n.ends_with(".metadata.json"))
            .collect();
        let required = expected
            .iter()
            .filter(|n| !n.ends_with(".sha256"))
            .map(|n| format!("{n}.metadata.json"))
            .collect();
        if actual != required {
            return Err(error("unexpected/missing metadata assets"));
        }
    }
    let mut records = String::new();
    for name in expected.iter().filter(|n| !n.ends_with(".sha256")) {
        records += &format!("{}  {name}\n", storage::digest(&directory.join(name))?);
    }
    storage::atomic_write(&directory.join("SHA256SUMS"), records.as_bytes())?;
    Ok(expected.len() / 2)
}

pub fn main(arguments: &[OsString]) -> Result<i32> {
    let (command, rest) =
        arguments.split_first().ok_or_else(|| error("check requires a contract name"))?;
    match command.to_str() {
        Some("case-sources") => source_checks(rest),
        Some("wasm-hosts") => wasm_hosts(rest),
        Some("windows-host") => windows_host(rest),
        Some("extracted-package") => {
            let mut args = Args::new(rest);
            let archive = args.path("--archive")?.ok_or_else(|| error("missing --archive"))?;
            let target = args.required("--target")?;
            let temporary = tempfile::Builder::new().prefix("wave-extracted-package-").tempdir()?;
            let output =
                args.path("--output")?.unwrap_or_else(|| temporary.path().join("evidence"));
            args.finish()?;
            package::extracted_smoke(&archive, &target, &output)?;
            Ok(0)
        },
        Some("crt") => {
            let mut args = Args::new(rest);
            let receipt = args.path("--receipt")?;
            let root = args.path("--root")?;
            let mut architectures = Vec::new();
            while let Some(arch) = args.text("--arch")? {
                if !["x86_64", "aarch64", "riscv64", "loongarch64"].contains(&arch.as_str()) {
                    return Err(error("unknown CRT architecture"));
                }
                architectures.push(arch);
            }
            args.finish()?;
            let root = if let Some(receipt) = receipt {
                if root.is_some() {
                    return Err(error("choose either CRT root or build receipt"));
                }
                let receipt = config::read_json(&receipt)?;
                let binary = Path::new(config::text(&receipt, "binary")?);
                if receipt.get_u64("schema_version") != Some(1)
                    || storage::digest(binary)? != config::text(&receipt, "binary_sha256")?
                {
                    return Err(error("Cargo build receipt is stale"));
                }
                PathBuf::from(config::text(&receipt, "llvm_out_dir")?).join("crt")
            } else {
                root.ok_or_else(|| error("missing --root or --receipt"))?
            };
            if architectures.is_empty() {
                package::verify_crt(&root)?;
            } else {
                package::verify_crt_selection(
                    &root,
                    &architectures.iter().map(String::as_str).collect::<Vec<_>>(),
                )?;
            }
            Ok(0)
        },
        Some("ci-jobs") => {
            let mut args = Args::new(rest);
            let workflow = args
                .path("--workflow")?
                .unwrap_or_else(|| crate::root().join(".github/workflows/ci.yml"));
            let manifest = args
                .path("--manifest")?
                .unwrap_or_else(|| crate::root().join("tests/cases/cases.toml"));
            args.finish()?;
            let names = crate::publication::required_names(
                &std::fs::read_to_string(workflow)?,
                &std::fs::read_to_string(manifest)?,
            )?;
            print!("{}", config::json(&Value::strings(names))?);
            Ok(0)
        },
        Some("std-inputs") => {
            Args::new(rest).finish()?;
            let temporary = tempfile::tempdir()?;
            let stage = temporary.path().join("std");
            package::committed_std(&crate::root(), &stage)?;
            let mut rows = Vec::new();
            for file in storage::files(&stage)? {
                rows.push(Value::object([
                    (
                        "path",
                        Value::string(
                            file.strip_prefix(&stage)?.to_string_lossy().replace('\\', "/"),
                        ),
                    ),
                    ("sha256", Value::string(storage::digest(&file)?)),
                ]));
            }
            print!("{}", config::json(&Value::Array(rows))?);
            Ok(0)
        },
        Some("std-policy") => {
            Args::new(rest).finish()?;
            std_policy()
        },
        Some("case-config") => crate::cases::main(rest),
        Some("binary") => binary(rest),
        Some("release-assets") => {
            let mut args = Args::new(rest);
            let directory =
                args.path("--directory")?.ok_or_else(|| error("missing asset directory"))?;
            let version = args.required("--version")?;
            let source = args.text("--source")?;
            let revision = args.text("--std-revision")?.map(|v| v.parse::<u64>()).transpose()?;
            args.finish()?;
            let count = release_assets(&directory, &version, source.as_deref(), revision)?;
            println!("Verified {count} archives");
            Ok(0)
        },
        _ => Err(error("unknown check contract")),
    }
}

fn source_checks(arguments: &[OsString]) -> Result<i32> {
    let mut args = Args::new(arguments);
    let compiler = args.path("--wavec")?.ok_or_else(|| error("missing --wavec"))?;
    let report = args.path("--report-json")?.ok_or_else(|| error("missing --report-json"))?;
    args.finish()?;
    storage::validate_output(
        &report,
        &crate::runner::protected_inputs(std::slice::from_ref(&compiler))?,
    )?;
    let mut records = Vec::new();
    let mut data = Value::object([
        ("phase", Value::string("source-check")),
        ("results", Value::Array(vec![])),
    ]);
    let action = (|| -> Result<()> {
        let manifest = crate::cases::Manifest::load(&crate::root(), true)?;
        let compiler = crate::runner::compiler(Some(compiler.clone()))?;
        for path in manifest.sources(None)? {
            let name = path.strip_prefix(&manifest.root)?.to_string_lossy().replace('\\', "/");
            let mut row = Value::object([
                ("source", Value::string(&name)),
                ("status", Value::string("failed")),
                ("exit_code", Value::Null),
            ]);
            let mut spec = Spec::new([
                compiler.as_os_str(),
                "check".as_ref(),
                path.as_os_str(),
                "--std-root".as_ref(),
                crate::root().join("std").as_os_str(),
            ]);
            spec.timeout = std::time::Duration::from_secs(15);
            match spec.run() {
                Ok(output) => {
                    config::field(&mut row, "exit_code", config::number(output.status as i64))?;
                    config::field(&mut row, "stdout", Value::string(output.text_stdout()))?;
                    config::field(&mut row, "stderr", Value::string(output.text_stderr()))?;
                    config::field(
                        &mut row,
                        "status",
                        Value::string(if output.failure.as_deref() == Some("timeout") {
                            "timeout"
                        } else if output.status == 0 && output.failure.is_none() {
                            "passed"
                        } else {
                            "failed"
                        }),
                    )?;
                    if let Some(reason) = output.failure {
                        config::field(&mut row, "error", Value::string(reason))?;
                    }
                },
                Err(e) => config::field(&mut row, "error", Value::string(e.to_string()))?,
            }
            println!("[{}] {name}", config::text(&row, "status")?);
            records.push(row);
            config::field(&mut data, "results", Value::Array(records.clone()))?;
            storage::atomic_write(&report, config::json(&data)?.as_bytes())?;
        }
        if records.is_empty() {
            return Err(error("no case sources selected"));
        }
        Ok(())
    })();
    if let Err(e) = &action {
        config::field(&mut data, "error", Value::string(e.to_string()))?;
    }
    storage::atomic_write(&report, config::json(&data)?.as_bytes())?;
    Ok(i32::from(action.is_err() || records.iter().any(|r| r.get_str("status") != Some("passed"))))
}

fn wasm_hosts(arguments: &[OsString]) -> Result<i32> {
    let mut args = Args::new(arguments);
    let compiler = crate::runner::compiler(args.path("--wavec")?)?;
    args.finish()?;
    let work = tempfile::Builder::new().prefix("wave-wasm-hosts-").tempdir()?;
    let preopen = work.path().join("preopen");
    std::fs::create_dir(&preopen)?;
    std::fs::write(preopen.join("README.md"), b"# WASI filesystem smoke fixture\n")?;
    let config = std::env::var_os("LLVM_CONFIG_PATH")
        .map(PathBuf::from)
        .unwrap_or_else(|| "llvm-config".into());
    let targets = package::run([config.as_os_str(), "--targets-built".as_ref()])?;
    if !targets.split_whitespace().any(|target| target == "WebAssembly") {
        return Err(error("LLVM WebAssembly backend missing"));
    }
    for (name, target, host) in [
        ("wasm_module", "wasm32-unknown-unknown", "run_wasm_smoke.mjs"),
        ("wasm_wasi", "wasm32-wasip1", "run_wasi_smoke.mjs"),
    ] {
        let mut build = Spec::new([
            compiler.as_os_str(),
            "build".as_ref(),
            crate::root().join(format!("examples/platform/{name}.wave")).as_os_str(),
            "--std-root".as_ref(),
            crate::root().join("std").as_os_str(),
            "--target".as_ref(),
            target.as_ref(),
            "--out-dir".as_ref(),
            work.path().as_os_str(),
        ]);
        build.timeout = std::time::Duration::from_secs(60);
        build.checked()?;
        let module = work.path().join(format!("{name}.wasm"));
        crate::wasm::verify(&module, Some(32))?;
        let mut command = vec![
            OsString::from("node"),
            "--no-warnings".into(),
            crate::root().join("tools").join(host).into(),
            module.into(),
        ];
        if name == "wasm_wasi" {
            command.push(preopen.clone().into());
        }
        let mut spec = Spec::new(command);
        spec.timeout = std::time::Duration::from_secs(30);
        spec.checked()?;
        let name = if name == "wasm_module" { "wasm_run" } else { name };
        let mut spec = Spec::new([
            compiler.as_os_str(),
            "run".as_ref(),
            crate::root().join(format!("examples/platform/{name}.wave")).as_os_str(),
            "--std-root".as_ref(),
            crate::root().join("std").as_os_str(),
            "--target".as_ref(),
            target.as_ref(),
        ]);
        spec.cwd = if name == "wasm_wasi" { preopen.clone() } else { crate::root() };
        spec.timeout = std::time::Duration::from_secs(30);
        spec.checked()?;
    }
    Ok(0)
}

fn windows_host(arguments: &[OsString]) -> Result<i32> {
    let mut args = Args::new(arguments);
    let target = args.required("--target")?;
    let compiler = args.path("--wavec")?;
    let object = args.flag("--arm64-object");
    let explicit = args.flag("--explicit");
    args.finish()?;
    crate::windows::require_native(&target)?;
    let rust = config::pin("RUST_VERSION")?;
    if !package::run(["rustc", &format!("+{rust}"), "-vV"])?
        .lines()
        .any(|line| line == format!("host: {target}"))
    {
        return Err(error("Rust host must match native MSVC architecture"));
    }
    let config = std::env::var_os("LLVM_CONFIG_PATH")
        .map(PathBuf::from)
        .ok_or_else(|| error("LLVM_CONFIG_PATH required"))?;
    if !package::run([config.as_os_str(), "--version".as_ref()])?
        .starts_with(&format!("{}.", crate::config::pin("LLVM_VERSION")?))
    {
        return Err(error("LLVM version mismatch"));
    }
    let backends = package::run([config.as_os_str(), "--targets-built".as_ref()])?;
    let required = if target.starts_with("aarch64") {
        vec!["AArch64"]
    } else {
        vec!["X86", "AArch64", "RISCV"]
    };
    if required.iter().any(|name| !backends.split_whitespace().any(|item| item == *name)) {
        return Err(error("native LLVM backend set is incomplete"));
    }
    if let Some(compiler) = compiler {
        let compiler = crate::runner::compiler(Some(compiler))?;
        binary::inspect(&compiler, Some(&target))?;
        Spec::new([compiler.as_os_str(), "-V".as_ref()]).checked()?;
        if package::run([compiler.as_os_str(), "print".as_ref(), "default-target".as_ref()])?.trim()
            != target
        {
            return Err(error("compiler default target is not native MSVC"));
        }
        if object {
            if !target.starts_with("aarch64") {
                return Err(error("ARM64 object probe requires a native ARM64 target"));
            }
            let output = tempfile::tempdir()?;
            let mut command: Vec<OsString> = vec![
                compiler.into(),
                "build".into(),
                crate::root().join("tests/cases/windows/arm64/test1.wave").into(),
                "--std-root".into(),
                crate::root().join("std").into(),
                "--emit=obj".into(),
                "--out-dir".into(),
                output.path().into(),
            ];
            if explicit {
                command.extend(["--target".into(), target.clone().into()]);
            }
            Spec::new(command).checked()?;
            binary::inspect(&output.path().join("test1.o"), Some(&target))?;
        }
    }
    Ok(0)
}
