// SPDX-License-Identifier: MPL-2.0
//! Timeout options reject out-of-range seconds before any deadline arithmetic.
use std::process::Command;

fn exec(timeout: &str, capture: bool) -> std::process::Output {
    let mut command = Command::new(env!("CARGO_BIN_EXE_wave-tools"));
    command.args(["exec", "--timeout", timeout]);
    if capture {
        command.arg("--capture");
    }
    command.arg("--");
    if cfg!(windows) {
        command.args(["cmd", "/c", "echo ok"]);
    } else {
        command.args(["sh", "-c", "echo ok"]);
    }
    command.output().unwrap()
}

#[test]
fn exec_rejects_excessive_timeouts_without_panicking() {
    let result = exec(&u64::MAX.to_string(), false);
    assert!(!result.status.success());
    let stderr = String::from_utf8_lossy(&result.stderr);
    assert!(stderr.contains("timeout must be at most"), "{stderr}");
    assert!(!stderr.contains("panicked"), "{stderr}");
}

#[test]
fn exec_accepts_the_supported_boundary_and_normal_timeouts() {
    let result = exec(&wave_tools::cli::MAX_TIMEOUT_SECONDS.to_string(), true);
    assert!(result.status.success(), "{}", String::from_utf8_lossy(&result.stderr));
    assert_eq!(String::from_utf8_lossy(&result.stdout).trim(), "ok");
    let result = exec("30", true);
    assert!(result.status.success(), "{}", String::from_utf8_lossy(&result.stderr));
    assert_eq!(String::from_utf8_lossy(&result.stdout).trim(), "ok");
}
