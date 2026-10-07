//! Native standard-library regressions with bounded process-tree execution.
#![cfg(any(target_os = "linux", target_os = "windows"))]

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};

static NEXT_FIXTURE: AtomicU64 = AtomicU64::new(0);

struct FixtureDirectory(PathBuf);

impl Drop for FixtureDirectory {
    fn drop(&mut self) {
        if std::thread::panicking() {
            eprintln!("retained failed runtime fixture: {}", self.0.display());
            return;
        }
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn copy_tree(source: &Path, destination: &Path) {
    fs::create_dir_all(destination).unwrap();
    for entry in fs::read_dir(source).unwrap() {
        let entry = entry.unwrap();
        let destination = destination.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_tree(&entry.path(), &destination);
        } else {
            fs::copy(entry.path(), destination).unwrap();
        }
    }
}

fn run_native_fixture(name: &str) {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let compiler = PathBuf::from(env!("CARGO_BIN_EXE_wavec"));
    let target = Command::new(&compiler).args(["print", "default-target"]).output().unwrap();
    assert!(target.status.success());
    let host = String::from_utf8(target.stdout).unwrap().trim().to_owned();
    if llvm::codegen::target::target_spec_for_triple(&host).is_none() {
        eprintln!("native runtime fixture skipped: LLVM target {host} is disabled");
        return;
    }
    let sequence = NEXT_FIXTURE.fetch_add(1, Ordering::Relaxed);
    let directory = FixtureDirectory(
        std::env::var_os("WAVE_RUNTIME_ARTIFACT_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(std::env::temp_dir)
            .join(format!("wave-runtime-{}-{sequence}", std::process::id())),
    );
    fs::create_dir_all(&directory.0).unwrap();
    let home = directory.0.join("home");
    copy_tree(&root.join("std"), &home.join(".wave/lib/wave/std"));
    fs::copy(root.join("tests/fixtures").join(name), directory.0.join("source.wave")).unwrap();
    let executable = directory.0.join(if cfg!(windows) { "fixture.exe" } else { "fixture" });
    let mut command = Command::new(&compiler);
    command
        .env("HOME", &home)
        .env("USERPROFILE", &home)
        .args(["build", "--target", &host])
        .arg(root.join("tests/fixtures").join(name))
        .arg("-o")
        .arg(&executable);
    fs::write(directory.0.join("command.txt"), format!("{command:?}\n")).unwrap();
    let output = command.output().unwrap();
    fs::write(directory.0.join("compiler.stdout"), &output.stdout).unwrap();
    fs::write(directory.0.join("compiler.stderr"), &output.stderr).unwrap();
    assert!(
        output.status.success(),
        "{name} compile: {}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    // The fixture forks children: bound and reap the whole tree on failure.
    let output = Command::new(if cfg!(windows) { "python" } else { "python3" })
        .current_dir(&root)
        .args([
            "-c",
            "import sys; from tools.process_tree import run_process; r = run_process([sys.argv[1]], timeout=20); raise SystemExit(r.returncode)",
        ])
        .arg(executable)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{name} runtime: {}\n{}\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[cfg(all(target_os = "linux", any(feature = "llvm-target-aarch64", feature = "llvm-target-all")))]
#[test]
fn executor_imports_compile_with_a_one_mib_process_stack() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let sequence = NEXT_FIXTURE.fetch_add(1, Ordering::Relaxed);
    let directory = FixtureDirectory(
        std::env::var_os("WAVE_RUNTIME_ARTIFACT_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(std::env::temp_dir)
            .join(format!("wave-runtime-{}-{sequence}", std::process::id())),
    );
    let home = directory.0.join("home");
    copy_tree(&root.join("std"), &home.join(".wave/lib/wave/std"));
    for phase in ["check", "ir", "obj"] {
        let mut command = Command::new("python3");
        command
            .current_dir(&root)
            .env("HOME", &home)
            .args([
                "-c",
                r#"
import resource
import sys
from tools.process_tree import run_process

resource.setrlimit(
    resource.RLIMIT_STACK,
    (1048576, resource.getrlimit(resource.RLIMIT_STACK)[1]),
)
result = run_process(sys.argv[1:], capture_output=True, text=True, timeout=30)
print(result.stdout)
print(result.stderr, file=sys.stderr)
raise SystemExit(result.returncode)
"#,
                env!("CARGO_BIN_EXE_wavec"),
                if phase == "check" { "check" } else { "build" },
            ])
            .arg(root.join("tests/fixtures/async/executor_restart.wave"))
            .arg("--target=aarch64-pc-windows-msvc");
        if phase != "check" {
            command.arg(format!("--emit={phase}"));
            command.arg("--out-dir").arg(&directory.0);
        }
        let output = command.output().unwrap();
        assert!(
            output.status.success(),
            "{phase} with 1 MiB stack: {}\n{}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

#[cfg(target_os = "linux")]
#[test]
fn child_standard_streams_preserve_shared_and_cyclic_descriptors() {
    run_native_fixture("process/descriptor_remapping.wave");
}

#[test]
fn sockets_survive_executor_restart_and_release_their_completion_port() {
    run_native_fixture("async/executor_restart.wave");
}
