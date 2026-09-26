//! Bounded native I/O regressions using a temporary copy of the current std.
#![cfg(any(target_os = "linux", target_os = "macos", target_os = "windows"))]

use std::process::Command;

#[test]
fn native_standard_io_preserves_data_descriptors_and_deadlines() {
    let compiler = env!("CARGO_BIN_EXE_wavec");
    let target = Command::new(compiler)
        .args(["print", "default-target"])
        .output()
        .unwrap();
    assert!(target.status.success());
    let host = String::from_utf8(target.stdout).unwrap().trim().to_owned();
    if llvm::codegen::target::target_spec_for_triple(&host).is_none() {
        eprintln!("native I/O fixtures skipped: LLVM target {host} is disabled");
        return;
    }
    let output = Command::new(if cfg!(windows) { "python" } else { "python3" })
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .env("WAVE_TEST_COMPILER", compiler)
        .env("WAVE_TEST_TARGET", host)
        .args([
            "-m",
            "unittest",
            "-v",
            "tools.test_std_io_runtime",
            "tools.test_std_boundary_runtime",
        ])
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
