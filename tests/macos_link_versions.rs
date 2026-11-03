// SPDX-License-Identifier: MPL-2.0
//! Both Darwin architectures use the same validated minimum and SDK linker contract.
use std::fs;
use std::path::PathBuf;
use std::process::{Command, Output};
use std::sync::atomic::{AtomicU64, Ordering};

static NEXT: AtomicU64 = AtomicU64::new(0);

fn wavec_bin() -> PathBuf {
    option_env!("CARGO_BIN_EXE_wavec")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("target/debug/wavec"))
}

fn case_dir() -> PathBuf {
    let root = std::env::temp_dir().join(format!(
        "wave-macos-platform-{}-{}",
        std::process::id(),
        NEXT.fetch_add(1, Ordering::Relaxed)
    ));
    fs::create_dir_all(&root).unwrap();
    root
}

fn run(args: &[&str], environment: &[(&str, &str)]) -> Output {
    let mut cmd = Command::new(wavec_bin());
    cmd.args(args)
        .env("NO_COLOR", "1")
        .env_remove("MACOSX_DEPLOYMENT_TARGET")
        .env_remove("MACOSX_SDK_VERSION")
        .env_remove("SDKROOT");
    for (key, value) in environment {
        cmd.env(key, value);
    }
    cmd.output().unwrap()
}

fn output_text(output: Output) -> (bool, String) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    (output.status.success(), format!("{stdout}\n{stderr}"))
}

#[test]
fn darwin_dry_run_uses_distinct_sdk_versions_for_both_architectures() {
    let root = case_dir();
    let source = root.join("main.wave");
    fs::write(&source, "fun main() -> i32 { return 0; }\n").unwrap();
    let sdk = root.join("MacOSX15.4.sdk");
    fs::create_dir_all(&sdk).unwrap();
    for (triple, arch) in [
        ("x86_64-apple-darwin", "x86_64"),
        ("aarch64-apple-darwin", "arm64"),
    ] {
        if llvm::codegen::target::target_spec_for_triple(triple).is_none() {
            continue;
        }
        let (ok, out) = output_text(run(
            &[
                "build",
                source.to_str().unwrap(),
                &format!("--target={triple}"),
                "--emit=bin",
                "--dry-run",
                "--error-format=json",
                &format!("--sysroot={}", sdk.display()),
                "--macos-deployment-target=12.1",
            ],
            &[],
        ));
        assert!(ok, "{triple}: {out}");
        assert!(out.contains(r#""macos_deployment_target":"12.1""#), "{out}");
        assert!(out.contains(r#""macos_sdk_version":"15.4""#), "{out}");
        assert!(out.contains(&format!(
            r#""-arch","{arch}","-platform_version","macos","12.1","15.4""#
        )), "{out}");
        assert!(out.contains("MacOSX15.4.sdk"), "{out}");
    }
}

#[test]
fn explicit_flags_override_environment_and_sdk_directory() {
    let triple = "x86_64-apple-darwin";
    if llvm::codegen::target::target_spec_for_triple(triple).is_none() {
        return;
    }
    let (ok, out) = output_text(run(
        &[
            "print", "target-spec", "--format=json",
            "--target=x86_64-apple-darwin",
            "--macos-deployment-target", "12.1",
            "--macos-sdk-version", "16.1",
            "--sysroot=/SDKs/MacOSX15.4.sdk",
        ],
        &[("MACOSX_DEPLOYMENT_TARGET", "13.0"), ("MACOSX_SDK_VERSION", "15.0")],
    ));
    assert!(ok, "{out}");
    assert!(out.contains(r#""macos_deployment_target":"12.1""#), "{out}");
    assert!(out.contains(r#""macos_sdk_version":"16.1""#), "{out}");

    let (ok, out) = output_text(run(
        &[
            "print", "target-spec", "--format=json",
            "--target=x86_64-apple-darwin",
            "--sysroot=/SDKs/MacOSX15.4.sdk",
        ],
        &[("MACOSX_DEPLOYMENT_TARGET", "11.2"), ("MACOSX_SDK_VERSION", "15.0")],
    ));
    assert!(ok, "{out}");
    assert!(out.contains(r#""macos_deployment_target":"11.2""#), "{out}");
    assert!(out.contains(r#""macos_sdk_version":"15.0""#), "{out}");
}

#[test]
fn invalid_macos_settings_fail_as_usage_errors() {
    let triple = "x86_64-apple-darwin";
    if llvm::codegen::target::target_spec_for_triple(triple).is_none() {
        return;
    }
    for args in [
        vec!["print", "target-spec", "--target=x86_64-apple-darwin", "--macos-deployment-target=bad"],
        vec!["print", "target-spec", "--target=x86_64-apple-darwin", "--macos-sdk-version=0.0"],
        vec![
            "print", "target-spec", "--target=x86_64-apple-darwin",
            "--macos-deployment-target=15.1", "--macos-sdk-version=15.0",
        ],
        vec!["print", "target-spec", "--target=x86_64-unknown-linux-gnu", "--macos-sdk-version=15.4"],
    ] {
        let (ok, out) = output_text(run(&args, &[]));
        assert!(!ok, "unexpectedly accepted {args:?}: {out}");
        assert!(out.contains("macOS") || out.contains("--macos"), "{out}");
    }
}
