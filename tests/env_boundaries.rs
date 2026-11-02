//! Portable environment boundaries and the Darwin CRT adapter contract.
#![cfg(any(
    target_os = "linux",
    target_os = "macos",
    target_os = "windows",
    target_os = "freebsd"
))]

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};

static NEXT: AtomicU64 = AtomicU64::new(0);
struct Case(PathBuf);
impl Drop for Case {
    fn drop(&mut self) {
        if !std::thread::panicking() {
            let _ = fs::remove_dir_all(&self.0);
        }
    }
}

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn copy_tree(source: &Path, destination: &Path) {
    fs::create_dir_all(destination).unwrap();
    for entry in fs::read_dir(source).unwrap() {
        let entry = entry.unwrap();
        let target = destination.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_tree(&entry.path(), &target);
        } else {
            fs::copy(entry.path(), target).unwrap();
        }
    }
}

fn case() -> Case {
    let case = Case(std::env::temp_dir().join(format!(
        "wave-env-boundaries-{}-{}",
        std::process::id(),
        NEXT.fetch_add(1, Ordering::Relaxed)
    )));
    copy_tree(&root().join("std"), &case.0.join("std"));
    case
}

fn native_target() -> Option<String> {
    let target = Command::new(env!("CARGO_BIN_EXE_wavec"))
        .args(["print", "default-target"])
        .output()
        .unwrap();
    assert!(target.status.success());
    let target = String::from_utf8(target.stdout).unwrap().trim().to_owned();
    if llvm::codegen::target::target_spec_for_triple(&target).is_none() {
        eprintln!("environment native regression skipped: {target} disabled");
        None
    } else {
        Some(target)
    }
}

fn build(case: &Case, fixture: &Path, opt: &str, target: &str, object: bool) -> PathBuf {
    let output = case.0.join(if object {
        "fixture.o"
    } else if cfg!(windows) {
        "fixture.exe"
    } else {
        "fixture"
    });
    let result = Command::new(env!("CARGO_BIN_EXE_wavec"))
        .arg("--std-root")
        .arg(case.0.join("std"))
        .arg("build")
        .arg(fixture)
        .args(["--target", target, opt, "--emit", if object { "obj" } else { "bin" }, "-o"])
        .arg(&output)
        .output()
        .unwrap();
    assert!(result.status.success(), "{target} {opt}: {}", String::from_utf8_lossy(&result.stderr));
    output
}

fn run(executable: &Path, variables: bool) {
    let mut command = wave_tools::build::test_command().unwrap();
    command.current_dir(root()).env_remove("WAVE_TEST_ENV_ABSENT_427_428");
    if variables {
        let values = utils::wson::parse_json(
            &fs::read_to_string(root().join("tests/fixtures/env_boundaries.json")).unwrap(),
        )
        .unwrap();
        let utils::wson::Value::Object(fields) = values else {
            panic!("environment fixture must be an object");
        };
        for (name, value) in fields {
            let utils::wson::Value::String(value) = value else {
                panic!("fixture must contain strings");
            };
            command.env(name, value);
        }
    }
    let result = command.args(["exec", "--timeout", "30", "--"]).arg(executable).output().unwrap();
    assert!(result.status.success(), "{result:?}");
}

#[test]
fn portable_environment_boundaries_use_injected_variables() {
    let Some(target) = native_target() else { return };
    let case = case();
    for opt in ["-O0", "-O2"] {
        let binary =
            build(&case, &root().join("tests/cases/shared/test142.wave"), opt, &target, false);
        run(&binary, true);
    }
}

#[test]
fn darwin_environment_adapter_handles_pointer_and_capacity_boundaries() {
    let Some(target) = native_target() else { return };
    let case = case();
    let path = case.0.join("std/sys/macos/common/env.wave");
    let mut source = fs::read_to_string(&path).unwrap();
    source =
        source.lines().filter(|line| !line.starts_with("extern(c,")).collect::<Vec<_>>().join("\n");
    source.push('\n');
    source.push_str(include_str!("fixtures/env_boundaries_mock_helpers.wave"));
    fs::write(path, source).unwrap();
    for opt in ["-O0", "-O2"] {
        let binary = build(
            &case,
            &root().join("tests/fixtures/env_boundaries_mock.wave"),
            opt,
            &target,
            false,
        );
        run(&binary, false);
    }
}

#[cfg(not(target_os = "freebsd"))]
#[test]
fn environment_lookup_retries_only_the_source_capacity_status() {
    let Some(target) = native_target() else { return };
    let case = case();
    fs::write(
        case.0.join("std/sys/env.wave"),
        include_str!("fixtures/env_boundaries_retry_provider.wave"),
    )
    .unwrap();
    for opt in ["-O0", "-O2"] {
        let binary = build(
            &case,
            &root().join("tests/fixtures/env_boundaries_retry.wave"),
            opt,
            &target,
            false,
        );
        run(&binary, false);
    }
}

#[test]
fn environment_boundaries_emit_for_both_darwin_architectures() {
    let case = case();
    for target in ["x86_64-apple-darwin", "aarch64-apple-darwin"] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        for opt in ["-O0", "-O2"] {
            let object =
                build(&case, &root().join("tests/cases/shared/test142.wave"), opt, target, true);
            assert!(object.metadata().unwrap().len() > 0);
        }
    }
}
