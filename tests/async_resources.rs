//! Deterministic scheduler work and owned IOCP staging regressions.
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
        "wave-async-resources-{}-{}",
        std::process::id(),
        NEXT.fetch_add(1, Ordering::Relaxed)
    )));
    copy_tree(&root().join("std"), &case.0.join("std"));
    case
}

fn run(case: &Case, fixture: &str, opt: &str) {
    let compiler = env!("CARGO_BIN_EXE_wavec");
    let default = Command::new(compiler).args(["print", "default-target"]).output().unwrap();
    assert!(default.status.success());
    let target = String::from_utf8(default.stdout).unwrap();
    if llvm::codegen::target::target_spec_for_triple(target.trim()).is_none() {
        eprintln!("async resource execution skipped: native target disabled");
        return;
    }
    let executable = case.0.join(if cfg!(windows) { "fixture.exe" } else { "fixture" });
    let output = Command::new(compiler)
        .arg("--std-root")
        .arg(case.0.join("std"))
        .arg("build")
        .arg(root().join("tests/fixtures").join(fixture))
        .arg(opt)
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(output.status.success(), "{fixture}: {}", String::from_utf8_lossy(&output.stderr));
    // Bound execution through the same process-tree helper used by runtime tests.
    let output = wave_tools::build::test_command()
        .unwrap()
        .current_dir(root())
        .args(["exec", "--timeout", "30", "--"])
        .arg(executable)
        .output()
        .unwrap();
    assert!(output.status.success(), "{fixture} {opt}: {output:?}");
}

#[test]
fn task_work_and_watch_storage_follow_live_work() {
    let case = case();
    let path = case.0.join("std/task.wave");
    let mut source = fs::read_to_string(&path).unwrap();
    // Count actual traversal/poll operations. Helpers are injected only into this
    // private copy; no test-only counters or API are shipped in std::task.
    source = source
        .replace("while (slot != null) {", "while (slot != null) { _test_work += 1;")
        .replace(
            "var slot: ptr<TaskSlot> = child.dependents;",
            "_test_work += 1; var slot: ptr<TaskSlot> = child.dependents;",
        )
        .replace(
            "var slot: ptr<TaskSlot> = _ready_head;",
            "_test_work += 1; var slot: ptr<TaskSlot> = _ready_head;",
        );
    let start = source.find("fun _watch_start_portable(").unwrap();
    let end = source[start..].find("fun _arm_portable").unwrap() + start;
    source.replace_range(start..end, "fun _watch_start_portable(token: i64, fd: i64, flags: i32) -> i64 { return 0; }\nfun _watch_stop_portable(fd: i64) -> bool { return _test_pending; }\n");
    // On Windows the same registry test uses an explicit acknowledgement mock.
    source = source
        .replace("return iocp_watch(token, fd, flags);", "return 0;")
        .replace("return iocp_cancel(token) != 0;", "return _test_pending;");
    source.push_str(include_str!("fixtures/async_resources_task_helpers.wave"));
    fs::write(path, source).unwrap();
    for opt in ["-O0", "-O2"] {
        run(&case, "async_resources_tasks.wave", opt);
    }
}

#[test]
fn iocp_staging_is_bounded_until_completion_acknowledgement() {
    let case = case();
    let path = case.0.join("std/sys/windows/iocp.wave");
    let mut source = fs::read_to_string(&path).unwrap();
    source = source.replace(
        "import(\"std::mem::alloc\")::{mem_alloc_zeroed, mem_free};",
        "import(\"std::mem::alloc\" as native_alloc);",
    );
    let start = source.find("import(\"std::sys::windows::completion_port\")").unwrap();
    let end = source[start..].find("};").unwrap() + start + 2;
    source.replace_range(start..end, "");
    source =
        source.lines().filter(|line| !line.starts_with("extern(")).collect::<Vec<_>>().join("\n");
    source = source
        .replace("mem_alloc_zeroed(", "_test_alloc(")
        .replace("mem_free(", "_test_free(")
        .replace("export(system,", "export(c,")
        .replace("__wave_async_windows_notify_address()", "null");
    source.push('\n');
    source.push_str(include_str!("fixtures/async_resources_iocp_mocks.wave"));
    fs::write(path, source).unwrap();
    for opt in ["-O0", "-O2"] {
        run(&case, "async_resources_iocp.wave", opt);
    }
}

#[test]
fn async_transfers_advance_across_multiple_staging_chunks() {
    let case = case();
    for opt in ["-O0", "-O2"] {
        run(&case, "async_resources_transfer.wave", opt);
    }
}
