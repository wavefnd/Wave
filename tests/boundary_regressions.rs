// This file is part of the Wave language project.
// Copyright (c) 2024–2026 Wave Foundation and contributors
// SPDX-License-Identifier: MPL-2.0

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicU64, Ordering};

static NEXT_CASE: AtomicU64 = AtomicU64::new(0);

fn copy_tree(source: &Path, destination: &Path) {
    fs::create_dir_all(destination).unwrap();
    for entry in fs::read_dir(source).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        let out = destination.join(entry.file_name());
        if path.is_dir() {
            copy_tree(&path, &out);
        } else {
            fs::copy(path, out).unwrap();
        }
    }
}

struct Case {
    root: PathBuf,
    home: PathBuf,
}
impl Case {
    fn new(name: &str) -> Self {
        let root = std::env::temp_dir().join(format!(
            "wave-boundary-{name}-{}-{}",
            std::process::id(),
            NEXT_CASE.fetch_add(1, Ordering::Relaxed)
        ));
        let home = root.join("home");
        copy_tree(
            &Path::new(env!("CARGO_MANIFEST_DIR")).join("std"),
            &home.join(".wave/lib/wave/std"),
        );
        Self { root, home }
    }
    fn command(&self) -> Command {
        let mut cmd = Command::new(env!("CARGO_BIN_EXE_wavec"));
        cmd.env("HOME", &self.home)
            .env("NO_COLOR", "1")
            .current_dir(&self.root);
        cmd
    }
    fn run(&self, source: &Path) -> String {
        for opt in ["-O0", "-O2"] {
            success(
                self.command()
                    .arg("build")
                    .arg(source)
                    .arg(opt)
                    .arg("--run")
                    .arg("--emit=ir,obj,bin")
                    .arg("--out-dir")
                    .arg(self.root.join(opt))
                    .output()
                    .unwrap(),
            );
        }
        fs::read_to_string(
            self.root
                .join("-O0")
                .join(source.file_stem().unwrap())
                .with_extension("ll"),
        )
        .unwrap()
    }
}
impl Drop for Case {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.root);
    }
}
fn success(output: Output) {
    assert!(
        output.status.success(),
        "status {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}
fn source(relative: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join(relative)
}
fn shared(number: u32) -> String {
    Case::new(&number.to_string()).run(&source(&format!("tests/cases/shared/test{number}.wave")))
}

#[test]
fn contextual_integer_arithmetic_preserves_width_and_signedness() {
    let ir = shared(128);
    assert!(ir.contains("icmp ule i64 %load_value, 4294967264"), "{ir}");
    assert!(!ir.contains("icmp ule i64 %load_value, -32"), "{ir}");
}

#[test]
fn oversized_integer_operands_report_source_errors() {
    let case = Case::new("integer-errors");
    for (index, body) in [
        "4294967296 - 32;",
        "var value: i32 = 4294967296 - 32;",
        "var value: u8 = 256 - 1;",
        "var value: u64 = 0; if (value < (18446744073709551616 - 1)) { return 1; }",
        "var value: i64 = 0; if ((9223372036854775808 - 1) > value) { return 1; }",
        "var value: f64 = 4294967296 / 2;",
    ]
    .iter()
    .enumerate()
    {
        let path = case.root.join(format!("invalid{index}.wave"));
        fs::write(&path, format!("fun main() -> i32 {{ {body} return 0; }}")).unwrap();
        let output = case.command().arg("check").arg(&path).output().unwrap();
        let error = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success(), "{body}");
        assert!(
            error.contains("does not fit")
                && error.contains("E3001")
                && error.contains(&format!("invalid{index}.wave")),
            "{error}"
        );
    }
}

#[test]
fn float_inequality_is_unordered_in_all_numeric_paths() {
    let ir = shared(129);
    assert!(ir.matches("fcmp une float").count() >= 3, "{ir}");
    assert!(ir.matches("fcmp une double").count() >= 3, "{ir}");
    assert!(!ir.contains("fcmp one"), "{ir}");
}
#[test]
fn floating_negation_preserves_sign_and_nan_payloads() {
    let ir = shared(130);
    assert!(
        ir.contains("fneg float") && ir.contains("fneg double"),
        "{ir}"
    );
}
#[test]
fn dirname_fallback_preserves_guard_bytes() {
    shared(131);
}
#[test]
fn paths_follow_native_target_policy() {
    shared(132);
}
#[test]
fn path_boundaries_compile_for_windows_architectures() {
    let case = Case::new("windows-paths");
    for target in ["x86_64-pc-windows-msvc", "aarch64-pc-windows-msvc"] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        for number in [131, 132, 133] {
            success(
                case.command()
                    .arg("build")
                    .arg(source(&format!("tests/cases/shared/test{number}.wave")))
                    .args(["--target", target, "--emit=ir,obj"])
                    .arg("--out-dir")
                    .arg(case.root.join(target))
                    .output()
                    .unwrap(),
            );
        }
    }
}
#[test]
fn basename_ignores_trailing_separators() {
    shared(133);
}
#[test]
fn durations_normalize_before_overflow_checks() {
    shared(134);
}
#[test]
fn fnv1a_matches_known_64_bit_vectors() {
    shared(135);
}
#[test]
fn trim_ranges_are_ordered_and_bounded() {
    shared(136);
}
#[test]
fn buffer_append_handles_adjacent_allocations_and_moving_self_sources() {
    let case = Case::new("buffer");
    fs::copy(
        source("tests/fixtures/boundaries/memory.wave"),
        case.home.join(".wave/lib/wave/std/sys/memory.wave"),
    )
    .unwrap();
    case.run(&source("tests/fixtures/boundaries/buffer_append.wave"));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn linux_buffer_case_preserves_invalid_range_state_and_success_exit() {
    let case = Case::new("linux-buffer-invalid-range");
    for opt in ["-O0", "-O2"] {
        let output_dir = case.root.join(opt);
        success(
            case.command()
                .arg("build")
                .arg(source("tests/cases/linux/amd64/test23.wave"))
                .arg(opt)
                .arg("--emit=bin")
                .arg("--out-dir")
                .arg(&output_dir)
                .output()
                .unwrap(),
        );
        let output = Command::new(output_dir.join("test23")).output().unwrap();
        assert_eq!(
            output.status.code(),
            Some(42),
            "{opt}: {}\n{}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

#[test]
fn read_to_end_rejects_capacity_overflow_and_preserves_normal_growth() {
    let case = Case::new("read-to-end");
    fs::write(case.root.join("input.bin"), vec![b'A'; 5000]).unwrap();
    fs::write(case.root.join("empty.bin"), []).unwrap();
    case.run(&source("tests/fixtures/boundaries/read_to_end.wave"));
}
#[test]
#[cfg(windows)]
fn empty_windows_event_set_returns_zero() {
    let case = Case::new("empty-windows-event");

    let path = case.root.join("empty_event.wave");

    fs::write(
        &path,
        r#"
import("std::sys::event")::{
    NativeEvent, event_create, event_wait, event_close
};

fun main() -> i32 {
    var handle: i64 = event_create(4);
    if (handle < 0) { return 1; }

    var events: array<NativeEvent, 4>;
    var count: i64 = event_wait(handle, &events[0], 4, 0);

    event_close(handle);

    if (count != 0) { return 2; }
    return 0;
}
"#,
    )
    .unwrap();

    success(
        case.command()
            .arg("build")
            .arg(&path)
            .arg("--run")
            .output()
            .unwrap(),
    );
}