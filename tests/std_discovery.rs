// This file is part of the Wave language project.
// SPDX-License-Identifier: MPL-2.0

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, MutexGuard, OnceLock};

static NEXT: AtomicU64 = AtomicU64::new(0);
static CASE_LOCK: Mutex<()> = Mutex::new(());

struct Case {
    root: PathBuf,
    compiler: PathBuf,
    home: PathBuf,
    work: PathBuf,
    _guard: MutexGuard<'static, ()>,
}

impl Case {
    fn new() -> Self {
        // A child forked while another case copies its compiler can inherit the
        // writable file descriptor until exec, making that compiler ETXTBSY.
        // Keep executable copies and child execution from overlapping cases.
        let guard = CASE_LOCK.lock().unwrap();
        let root = std::env::temp_dir().join(format!(
            "wave-std-discovery-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        let package = root.join("compiler 한글 with spaces");
        let home = root.join("home");
        let work = root.join("unrelated workspace");
        for directory in [&package, &home, &work] {
            fs::create_dir_all(directory).unwrap();
        }
        let compiler = package.join(format!("wavec{}", std::env::consts::EXE_SUFFIX));
        fs::copy(env!("CARGO_BIN_EXE_wavec"), &compiler).unwrap();
        Self { root, compiler, home, work, _guard: guard }
    }

    fn command(&self) -> Command {
        let mut command = Command::new(&self.compiler);
        command
            .current_dir(&self.work)
            .env("HOME", &self.home)
            .env("USERPROFILE", &self.home)
            .env("NO_COLOR", "1");
        command
    }

    fn check(&self, filename: &str) -> Command {
        let mut command = self.command();
        command.args(["check", filename, "--target", frontend_target()]);
        command
    }

    fn bundled(&self) -> PathBuf {
        self.compiler.parent().unwrap().join("std")
    }

    fn legacy(&self) -> PathBuf {
        self.home.join(".wave/lib/wave/std")
    }

    fn source(&self, name: &str) {
        fs::write(
            self.work.join("main.wave"),
            format!("import(\"std::probe\")::{{{name}}}; fun main() -> i32 {{ return {name}; }}"),
        )
        .unwrap();
    }
}

impl Drop for Case {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.root);
    }
}

fn frontend_target() -> &'static str {
    static TARGET: OnceLock<String> = OnceLock::new();
    TARGET.get_or_init(|| {
        let output = Command::new(env!("CARGO_BIN_EXE_wavec"))
            .args(["print", "target-list"])
            .output()
            .unwrap();
        successful(&output);
        String::from_utf8(output.stdout)
            .unwrap()
            .lines()
            .next()
            .expect("at least one target must be enabled")
            .to_owned()
    })
}

fn stub_std(root: &Path, name: &str) {
    fs::create_dir_all(root).unwrap();
    fs::copy(
        Path::new(env!("CARGO_MANIFEST_DIR")).join("std/manifest.json"),
        root.join("manifest.json"),
    )
    .unwrap();
    fs::write(root.join("probe.wave"), format!("pub const {name}: i32 = 17;\n")).unwrap();
}

fn successful(output: &Output) {
    assert!(
        output.status.success(),
        "status {}\n{}\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn rejected(output: &Output, message: &str) {
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains(message), "{stderr}");
}

fn printed_path(command: &mut Command, expected: &Path) {
    let output = command.output().unwrap();
    successful(&output);
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert_eq!(Path::new(stdout.trim()).canonicalize().unwrap(), expected.canonicalize().unwrap());
}

#[test]
fn relocated_compiler_discovers_its_bundled_std_before_home_or_working_directory() {
    let mut case = Case::new();
    stub_std(&case.bundled(), "BUNDLED");
    stub_std(&case.legacy(), "LEGACY");
    stub_std(&case.work.join("std"), "WORKING_DIRECTORY");
    case.source("BUNDLED");
    successful(&case.check("main.wave").output().unwrap());
    printed_path(case.command().args(["print", "std-path"]), &case.bundled());
    printed_path(case.command().args(["print", "dep-search-paths"]), &case.bundled());

    let relocated = case.root.join("relocated compiler 한글");
    let filename = case.compiler.file_name().unwrap().to_owned();
    fs::rename(case.compiler.parent().unwrap(), &relocated).unwrap();
    case.compiler = relocated.join(filename);
    successful(
        &case.check("main.wave").env_remove("HOME").env_remove("USERPROFILE").output().unwrap(),
    );
    printed_path(case.command().args(["print", "std-path"]), &case.bundled());
}

#[test]
fn explicit_std_root_overrides_bundled_std_without_fallback() {
    let case = Case::new();
    stub_std(&case.bundled(), "BUNDLED");
    stub_std(&case.legacy(), "LEGACY");
    let explicit = case.root.join("explicit std 한글");
    stub_std(&explicit, "EXPLICIT");
    case.source("EXPLICIT");
    successful(&case.check("main.wave").arg("--std-root").arg(&explicit).output().unwrap());
    printed_path(
        case.command().arg("--std-root").arg(&explicit).args(["print", "std-path"]),
        &explicit,
    );

    case.source("BUNDLED");
    let missing = case.root.join("missing");
    rejected(
        &case.check("main.wave").arg("--std-root").arg(&missing).output().unwrap(),
        "cannot resolve std root",
    );
    fs::write(explicit.join("manifest.json"), "malformed").unwrap();
    rejected(
        &case.check("main.wave").arg("--std-root").arg(&explicit).output().unwrap(),
        "invalid",
    );
}

#[test]
fn invalid_bundled_std_is_reported_and_does_not_block_std_free_programs() {
    let case = Case::new();
    stub_std(&case.bundled(), "BUNDLED");
    stub_std(&case.legacy(), "LEGACY");
    case.source("LEGACY");
    fs::write(case.work.join("plain.wave"), "fun main() -> i32 { return 0; }\n").unwrap();
    for manifest in
        [Some("{\"name\":\"std\",\"compatibility_revision\":999999}"), Some("malformed"), None]
    {
        let path = case.bundled().join("manifest.json");
        match manifest {
            Some(text) => fs::write(&path, text).unwrap(),
            None => fs::remove_file(&path).unwrap(),
        }
        let output = case.check("main.wave").output().unwrap();
        rejected(&output, "E1002");
        rejected(&output, "compatibility revision");
        let error = String::from_utf8_lossy(&output.stderr);
        assert!(error.contains("compiler 한글 with spaces"), "{error}");
        successful(&case.check("plain.wave").output().unwrap());
    }
}

#[test]
fn legacy_std_remains_available_when_no_bundle_is_present() {
    let case = Case::new();
    stub_std(&case.legacy(), "LEGACY");
    case.source("LEGACY");
    successful(&case.check("main.wave").output().unwrap());
    printed_path(case.command().args(["print", "std-path"]), &case.legacy());

    let output =
        case.check("main.wave").env_remove("HOME").env_remove("USERPROFILE").output().unwrap();
    rejected(&output, "cannot locate std");
    fs::write(case.work.join("plain.wave"), "fun main() -> i32 { return 0; }\n").unwrap();
    successful(
        &case.check("plain.wave").env_remove("HOME").env_remove("USERPROFILE").output().unwrap(),
    );
}

#[cfg(unix)]
#[test]
fn executable_symlink_uses_the_actual_compiler_bundle() {
    let case = Case::new();
    stub_std(&case.bundled(), "BUNDLED");
    stub_std(&case.work.join("std"), "SYMLINK_DIRECTORY");
    case.source("BUNDLED");
    let alias = case.work.join("wavec-alias");
    std::os::unix::fs::symlink(&case.compiler, &alias).unwrap();
    let output = Command::new(alias)
        .current_dir(&case.work)
        .env("HOME", &case.home)
        .env("NO_COLOR", "1")
        .args(["check", "main.wave", "--target", frontend_target()])
        .output()
        .unwrap();
    successful(&output);
}

#[cfg(unix)]
#[test]
fn dangling_bundled_std_symlink_does_not_fall_back_to_home() {
    let case = Case::new();
    stub_std(&case.legacy(), "LEGACY");
    case.source("LEGACY");
    std::os::unix::fs::symlink(case.root.join("missing-std"), case.bundled()).unwrap();
    rejected(&case.check("main.wave").output().unwrap(), "cannot resolve std root");
}
