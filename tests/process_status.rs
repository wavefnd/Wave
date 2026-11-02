//! Bounded native I/O regressions using a temporary copy of the current std.
#![cfg(any(
    target_os = "linux",
    target_os = "macos",
    target_os = "freebsd",
    target_os = "windows"
))]

use std::process::Command;

#[cfg(not(windows))]
#[test]
fn native_wait_status_and_descriptor_boundaries() {
    let compiler = env!("CARGO_BIN_EXE_wavec");
    let target = Command::new(compiler).args(["print", "default-target"]).output().unwrap();
    assert!(target.status.success());
    let host = String::from_utf8(target.stdout).unwrap().trim().to_owned();
    if llvm::codegen::target::target_spec_for_triple(&host).is_none() {
        eprintln!("native I/O fixtures skipped: LLVM target {host} is disabled");
        return;
    }
    let output = wave_tools::build::test_command()
        .unwrap()
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(["native-std", "--wavec", compiler, "--target", &host, "--process-only"])
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "native standard I/O: {}\n{}\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn target_wait_status_tables_compile_for_each_supported_provider() {
    use std::{fs, path::PathBuf};
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let directory =
        std::env::temp_dir().join(format!("wave-process-status-tables-{}", std::process::id()));
    fs::create_dir_all(&directory).unwrap();
    for target in [
        "x86_64-unknown-linux-gnu",
        "aarch64-apple-darwin",
        "x86_64-apple-darwin",
        "x86_64-unknown-freebsd",
        "x86_64-pc-windows-msvc",
        "aarch64-pc-windows-msvc",
        "wasm32-wasip1",
    ] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        for opt in ["-O0", "-O2"] {
            let result = Command::new(env!("CARGO_BIN_EXE_wavec"))
                .arg("--std-root")
                .arg(root.join("std"))
                .arg("build")
                .arg(root.join("tests/fixtures/process_status.wave"))
                .args(["--target", target, opt, "--emit=ir,obj", "--out-dir"])
                .arg(directory.join(format!("{target}-{opt}")))
                .output()
                .unwrap();
            assert!(
                result.status.success(),
                "{target} {opt}: {}",
                String::from_utf8_lossy(&result.stderr)
            );
        }
    }
    fs::remove_dir_all(directory).unwrap();
}
