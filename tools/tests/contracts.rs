// SPDX-License-Identifier: MPL-2.0
use std::fs;
use std::path::PathBuf;
use std::time::Duration;
use wave_tools::{config, process::Spec, storage};

fn spec(arguments: &[&str]) -> Spec {
    let mut spec = Spec::new(arguments.iter().copied());
    spec.supervisor = Some(PathBuf::from(env!("CARGO_BIN_EXE_wave-tools")));
    spec.timeout = Duration::from_secs(3);
    spec
}

#[test]
fn environment_codec_preserves_records_and_rejects_partial_inputs() {
    for value in ["", "single", "a\nb", "a\r\nb\n", "\n", "spaces  \n\n", "한글\r"] {
        let record = config::encode_env("VALUE", value).unwrap();
        assert_eq!(config::decode_env(&record).unwrap()["VALUE"], value);
    }
    for text in ["BAD-NAME=x\n", "VALUE<<\nx\n", "VALUE<<end\nnever ends", "VALUE=a\0b\n"] {
        assert!(config::decode_env(text.as_bytes()).is_err());
    }
}

#[test]
fn environment_append_preserves_existing_and_separates_missing_lf() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("env");
    fs::write(&path, "EXISTING=ok").unwrap();
    config::append_env(&path, "MULTI", "one\ntwo\n").unwrap();
    let decoded = config::decode_env(&fs::read(&path).unwrap()).unwrap();
    assert_eq!(decoded["EXISTING"], "ok");
    assert_eq!(decoded["MULTI"], "one\ntwo\n");
}

#[test]
fn activation_failure_restores_or_keeps_recoverable_old_bytes() {
    for restoration_fails in [false, true] {
        let directory = tempfile::tempdir().unwrap();
        let root = directory.path();
        let candidate = root.join("new");
        let destination = root.join("current");
        fs::write(&candidate, "NEW").unwrap();
        fs::write(&destination, "OLD").unwrap();
        let result = storage::replace_set_with(
            &[(candidate.clone(), destination.clone())],
            |source, target| {
                if source == candidate || restoration_fails && source.file_name().unwrap() == "0" {
                    return Err(std::io::Error::other("injected rename failure"));
                }
                fs::rename(source, target)
            },
        );
        assert!(result.is_err());
        if restoration_fails {
            assert!(result.unwrap_err().to_string().contains("retained backups"));
            assert!(storage::files(root).unwrap().iter().any(|p| fs::read(p).unwrap() == b"OLD"));
        } else {
            assert_eq!(fs::read(destination).unwrap(), b"OLD");
        }
    }
}

#[test]
fn report_protection_rejects_archive_and_hardlink_inputs() {
    let directory = tempfile::tempdir().unwrap();
    let source = directory.path().join("archive.tar.gz");
    fs::write(&source, "archive").unwrap();
    let alias = directory.path().join("report.json");
    fs::hard_link(&source, &alias).unwrap();
    assert!(storage::validate_output(&alias, std::slice::from_ref(&source)).is_err());
    assert!(storage::validate_output(&source, std::slice::from_ref(&source)).is_err());
    assert_eq!(fs::read(source).unwrap(), b"archive");
}

#[test]
#[cfg(unix)]
fn process_preserves_bytes_status_and_kills_descendants_on_timeout() {
    let output = spec(&["sh", "-c", "printf '\\377'; printf error >&2; exit 7"]).run().unwrap();
    assert_eq!(output.status, 7);
    assert_eq!(output.stdout, vec![255]);
    assert_eq!(output.stderr, b"error");
    assert!(output.strict_stdout().is_err());
    let directory = tempfile::tempdir().unwrap();
    let marker = directory.path().join("escaped");
    let command = format!("(sleep 2; touch '{}') & wait", marker.display());
    let mut short = spec(&["sh", "-c", &command]);
    short.timeout = Duration::from_millis(100);
    assert_eq!(short.run().unwrap().failure.as_deref(), Some("timeout"));
    std::thread::sleep(Duration::from_millis(2200));
    assert!(!marker.exists());
}

#[test]
#[cfg(unix)]
fn output_budget_is_bounded_and_reported() {
    let mut command = spec(&["sh", "-c", "yes output"]);
    command.output_limit = 1024;
    let result = command.run().unwrap();
    assert_eq!(result.failure.as_deref(), Some("output limit exceeded"));
    assert_eq!(result.stdout.len(), 1024);
}

#[test]
fn metadata_rejects_ambiguous_fixtures_and_invalid_compile_contracts() {
    use wave_tools::runner::Metadata;
    for source in [
        "// wave-test: stdin=x,stdin-file=input.txt\nfun main() {}",
        "// wave-test: runner=compile,mode=build,object-bits=12",
        "// wave-test: requires=clock|clock",
        "// wave-test: stdout-file=../outside",
        "// wave-test: target=aarch64-unknown-linux-gnu",
    ] {
        assert!(Metadata::parse(source).is_err(), "{source}");
    }
    let valid=Metadata::parse("// wave-test: mode=build, runner=compile, target=riscv64-unknown-linux-gnu, object-arch=riscv64, riscv-float-abi=lp64d\nfun main() {}").unwrap();
    assert_eq!(valid.object_arch.as_deref(), Some("riscv64"));
}

#[test]
fn rejects_wrong_macho_architecture_and_unknown_binary_data() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("library.dylib");
    let mut header = Vec::new();
    for value in [0xfeedfacfu32, 0x0100000c, 0, 6, 0, 0, 0x85, 0] {
        header.extend(value.to_le_bytes());
    }
    fs::write(&path, header).unwrap();
    assert!(wave_tools::binary::inspect(&path, Some("aarch64-apple-darwin")).is_ok());
    assert!(wave_tools::binary::inspect(&path, Some("x86_64-apple-darwin")).is_err());
    fs::write(&path, b"uninspectable placeholder").unwrap();
    assert!(wave_tools::binary::inspect(&path, None).is_err());
}

#[test]
fn actual_repository_matrix_has_the_existing_thirty_required_cells() {
    let manifest = wave_tools::cases::Manifest::load(&wave_tools::root(), true).unwrap();
    let value = manifest.matrices().unwrap();
    let utils::wson::Value::Object(groups) = value else { panic!() };
    assert_eq!(
        groups.iter().map(|(_, value)| value.get_arr("include").unwrap().len()).sum::<usize>(),
        30
    );
}

#[test]
fn tool_options_stop_at_the_child_separator() {
    use wave_tools::cli::Args;
    let values = ["--timeout", "2", "--", "child", "--timeout", "99", "--capture"].map(Into::into);
    let mut args = Args::new(&values);
    assert_eq!(args.text("--timeout").unwrap().as_deref(), Some("2"));
    assert!(!args.flag("--capture"));
    assert_eq!(
        args.values,
        ["--", "child", "--timeout", "99", "--capture"].map(std::ffi::OsString::from)
    );
    assert!(Args::new(&["--timeout".into(), "--".into(), "child".into()])
        .text("--timeout")
        .is_err());
}

#[test]
fn assembly_contracts_ignore_comments_directives_labels_and_partial_symbols() {
    use wave_tools::runner::assembly_code;
    let x86 = assembly_code("# syscall\n.ascii \"syscall\"\nsyscall_label:\ncall syscall_wrapper\n/* syscall */ nop\nsyscall\n", "x86_64-unknown-linux-gnu").unwrap();
    assert_eq!(x86, "call syscall_wrapper\nnop\nsyscall");
    let arm =
        assembly_code("#APP\nmov x0, #1 // svc\n; svc\nsvc #0\n", "aarch64-apple-darwin").unwrap();
    assert!(!arm.contains("; svc"));
    assert!(arm.contains("svc #0"));
}

#[test]
fn activation_preflights_every_destination_before_mutating_anything() {
    let directory = tempfile::tempdir().unwrap();
    let source = directory.path().join("candidate");
    let destination = directory.path().join("current");
    fs::write(&source, "NEW").unwrap();
    fs::write(&destination, "OLD").unwrap();
    let mut renamed = false;
    let result = storage::replace_set_with(
        &[
            (source.clone(), destination.clone()),
            (directory.path().join("missing"), directory.path().join("other")),
        ],
        |_, _| {
            renamed = true;
            Ok(())
        },
    );
    assert!(result.is_err());
    assert!(!renamed);
    assert_eq!(fs::read(destination).unwrap(), b"OLD");
}

#[test]
fn native_process_bytes_limits_and_descendant_cleanup_work_without_a_shell() {
    let tool = env!("CARGO_BIN_EXE_wave-tools");
    let result = spec(&[tool, "__fixture-process", "bytes"]).run().unwrap();
    assert_eq!(result.status, 7);
    assert_eq!(result.stdout, vec![255]);
    assert_eq!(result.stderr, b"error");
    let mut flood = spec(&[tool, "__fixture-process", "flood"]);
    flood.output_limit = 1024;
    let output = flood.run().unwrap();
    assert_eq!(output.failure.as_deref(), Some("output limit exceeded"));
    assert!(output.stdout.len() <= 1024 && output.stderr.len() <= 1024);
    let directory = tempfile::tempdir().unwrap();
    for mode in ["spawn-wait", "spawn-exit"] {
        let marker = directory.path().join(mode);
        let mut child = spec(&[tool, "__fixture-process", mode, marker.to_str().unwrap()]);
        child.timeout = Duration::from_secs(1);
        let result = child.run().unwrap();
        assert!(result.text_stdout().contains("READY"));
        if mode == "spawn-wait" {
            assert_eq!(result.failure.as_deref(), Some("timeout"));
        } else {
            assert_eq!(result.status, 0);
            assert!(result.failure.is_none());
        }
    }
    std::thread::sleep(Duration::from_millis(2200));
    assert_eq!(fs::read_dir(directory.path()).unwrap().count(), 0);
}

#[test]
fn cargo_receipt_rejects_missing_or_ambiguous_outputs_and_retains_the_exact_llvm_build() {
    use utils::wson::Value;
    let directory = tempfile::tempdir().unwrap();
    let binary = directory.path().join("wavec");
    fs::write(&binary, b"fresh compiler").unwrap();
    let artifact = Value::object([
        ("reason", Value::string("compiler-artifact")),
        ("target", Value::object([("name", Value::string("wavec"))])),
        ("executable", Value::string(binary.to_string_lossy())),
    ]);
    let build = Value::object([
        ("reason", Value::string("build-script-executed")),
        ("package_id", Value::string("path+file:///checkout/llvm#0.1.0")),
        ("out_dir", Value::string("/exact/build/out")),
    ]);
    let messages = format!("{}{}", config::json(&artifact).unwrap().replace('\n', ""), "\n");
    assert!(wave_tools::build::cargo_receipt(&messages, &binary).is_err());
    let messages = messages + &config::json(&build).unwrap().replace('\n', "") + "\n";
    let receipt = wave_tools::build::cargo_receipt(&messages, &binary).unwrap();
    assert_eq!(receipt.get_str("llvm_out_dir"), Some("/exact/build/out"));
    assert_eq!(receipt.get_str("binary_sha256"), Some(storage::digest(&binary).unwrap().as_str()));
    let mut other = build;
    config::field(&mut other, "out_dir", Value::string("/stale/build/out")).unwrap();
    assert!(wave_tools::build::cargo_receipt(
        &(messages + &config::json(&other).unwrap().replace('\n', "")),
        &binary
    )
    .is_err());
}

#[test]
fn cleanup_preflights_ownership_preserves_tmp_and_recognizes_native_archives() {
    let work = tempfile::tempdir().unwrap();
    let root = work.path().join("repository");
    fs::create_dir_all(root.join("tests/cases")).unwrap();
    fs::copy(
        wave_tools::root().join("tests/cases/cases.toml"),
        root.join("tests/cases/cases.toml"),
    )
    .unwrap();
    for name in ["target", "dist", ".tmp", "custom"] {
        fs::create_dir(root.join(name)).unwrap();
        fs::write(root.join(name).join("sentinel"), "KEEP").unwrap();
    }
    assert!(
        wave_tools::build::clean_outputs(&root, &root.join("custom"), &root.join("dist")).is_err()
    );
    assert!(wave_tools::build::clean_outputs(&root, &root.join("target"), &root.join("custom"))
        .is_err());
    assert!(root.join("target/sentinel").is_file());
    assert!(wave_tools::build::clean_outputs(&root, &root, &root.join("dist")).is_err());
    for name in [
        "wave-v0.0.1-alpha-dev-x86_64-linux-gnu.tar.gz",
        "wave-v0.0.1-alpha-dev-aarch64-pc-windows-msvc.zip.sha256",
        "wave-v0.0.1-alpha-dev-unknown.tar.gz",
        "unrelated.zip",
    ] {
        fs::write(root.join(name), "archive").unwrap();
    }
    wave_tools::build::clean_outputs(&root, &root.join("target"), &root.join("dist")).unwrap();
    assert!(root.join(".tmp/sentinel").is_file());
    assert!(root.join("custom/sentinel").is_file());
    assert!(!root.join("wave-v0.0.1-alpha-dev-x86_64-linux-gnu.tar.gz").exists());
    assert!(!root.join("wave-v0.0.1-alpha-dev-aarch64-pc-windows-msvc.zip.sha256").exists());
    assert!(root.join("wave-v0.0.1-alpha-dev-unknown.tar.gz").is_file());
    assert!(root.join("unrelated.zip").is_file());
}
