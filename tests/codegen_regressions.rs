// This file is part of the Wave language project.
// Copyright (c) 2024–2026 Wave Foundation
// Copyright (c) 2024–2026 LunaStev and contributors
//
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file,
// You can obtain one at https://mozilla.org/MPL/2.0/.
//
// SPDX-License-Identifier: MPL-2.0
// AI TRAINING NOTICE: Prohibited without prior written permission. No use for machine learning or generative AI training, fine-tuning, distillation, embedding, or dataset creation.

use std::ffi::{OsStr, OsString};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
use wavec::link_validation::{validate_loongarch64_link_inputs, LoongArchFloatAbi};
use wavec::link_validation::{validate_riscv_link_inputs, RiscvFloatAbi};

static NEXT_TEMP_CASE: AtomicU64 = AtomicU64::new(0);

fn wavec_bin() -> PathBuf {
    if let Some(path) = option_env!("CARGO_BIN_EXE_wavec") {
        return PathBuf::from(path);
    }

    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("target/debug/wavec")
}

fn temp_case_dir(name: &str) -> PathBuf {
    let sequence = NEXT_TEMP_CASE.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!(
        "wavec-{}-{}-{}",
        name,
        std::process::id(),
        sequence
    ));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    dir
}

fn write_wave(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(name);
    fs::write(&path, source).unwrap();
    path
}

fn copy_tree(source: &Path, destination: &Path) {
    fs::create_dir_all(destination).unwrap();
    for entry in fs::read_dir(source).unwrap() {
        let entry = entry.unwrap();
        let source_path = entry.path();
        let destination_path = destination.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_tree(&source_path, &destination_path);
        } else {
            fs::copy(source_path, destination_path).unwrap();
        }
    }
}

fn wavec_command() -> Command {
    let mut command = Command::new(wavec_bin());
    command.env("NO_COLOR", "1");
    command
}

fn run_wavec<I, S>(args: I)
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let output = wavec_command().args(args).output().unwrap();
    assert!(
        output.status.success(),
        "wavec failed with status {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn run_wavec_capture<I, S>(args: I) -> (String, String)
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let output = wavec_command().args(args).output().unwrap();
    assert!(
        output.status.success(),
        "wavec failed with status {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    (
        String::from_utf8_lossy(&output.stdout).to_string(),
        String::from_utf8_lossy(&output.stderr).to_string(),
    )
}

fn run_wavec_raw<I, S>(args: I) -> std::process::Output
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    wavec_command().args(args).output().unwrap()
}

fn run_wavec_expect_failure<I, S>(args: I) -> String
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let output = wavec_command().args(args).output().unwrap();
    assert!(
        !output.status.success(),
        "wavec unexpectedly succeeded\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

#[test]
fn windows_system_abi_is_target_scoped() {
    let dir = temp_case_dir("windows-system-abi");
    let source = write_wave(
        &dir,
        "system.wave",
        r#"
extern(system, "GetCurrentProcessId") fun get_current_process_id() -> u32;
fun main() -> i32 { return get_current_process_id() as i32; }
"#,
    );
    for target in [
        "x86_64-w64-windows-gnu",
        "aarch64-w64-windows-gnu",
        "aarch64-pc-windows-gnu",
    ] {
        let windows_out = dir.join(target);
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--emit=ir"),
            OsStr::new("--out-dir"),
            windows_out.as_os_str(),
        ]);
        let ir = fs::read_to_string(windows_out.join("system.ll")).unwrap();
        assert!(ir.contains("@GetCurrentProcessId"), "{target}: {ir}");
    }

    let arm64_bin = dir.join("system-arm64.exe");
    let (link_plan, stderr) = run_wavec_capture([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target=aarch64-w64-windows-gnu"),
        OsStr::new("--emit=bin"),
        OsStr::new("--dry-run"),
        OsStr::new("-o"),
        arm64_bin.as_os_str(),
    ]);
    assert!(stderr.trim().is_empty(), "{stderr}");
    let expected_linker = std::env::var("WAVE_WINDOWS_ARM64_LINKER")
        .unwrap_or_else(|_| "aarch64-w64-mingw32-gcc".to_string());
    assert!(link_plan.contains(&expected_linker), "{link_plan}");

    let linux_out = dir.join("linux");
    let error = run_wavec_expect_failure([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target=x86_64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        linux_out.as_os_str(),
    ]);
    assert!(error.contains("Windows 'system'"), "{error}");
}

#[test]
fn incompatible_std_is_rejected_from_an_isolated_home() {
    let dir = temp_case_dir("std-compatibility-home");
    let home = dir.join("home");
    let std_root = home.join(".wave/lib/wave/std");
    fs::create_dir_all(std_root.join("string")).unwrap();
    fs::write(
        std_root.join("manifest.json"),
        r#"{"name":"std","compatibility_revision":0}"#,
    )
    .unwrap();
    fs::write(
        std_root.join("string/len.wave"),
        "pub fun len(value: str) -> i64 { return 0; }\n",
    )
    .unwrap();
    let source = write_wave(
        &dir,
        "main.wave",
        "import(\"std::string::len\")::{len};\nfun main() { len(\"x\"); }\n",
    );

    let output = wavec_command()
        .env("HOME", &home)
        .arg("check")
        .arg(&source)
        .output()
        .unwrap();
    assert!(!output.status.success());
    let error = String::from_utf8_lossy(&output.stderr);
    assert!(
        error.contains("installed std compatibility revision 0"),
        "{error}"
    );
    assert!(error.contains("requires 3"), "{error}");
    assert!(error.contains("wavec update std"), "{error}");
}

#[test]
fn variant_frontend_resolves_imported_types_constructors_and_patterns() {
    let dir = temp_case_dir("variant-imports");
    write_wave(
        &dir,
        "option.wave",
        r#"
pub variant Option<T> {
    Some(T),
    None
}
"#,
    );
    let selected = write_wave(
        &dir,
        "selected.wave",
        r#"
import("./option")::{Option};

fun unwrap(value: Option<i32>) -> i32 {
    match value {
        Option::Some(item) => { return item; }
        Option::None => { return 0; }
    }
}
"#,
    );
    let namespaced = write_wave(
        &dir,
        "namespaced.wave",
        r#"
import("./option" as option);

fun make() -> option::Option<i32> {
    return option::Option::Some(7);
}

fun infer() {
    var value: option::Option<i32> = option::Option::Some(9);
    match value {
        option::Option::Some(item) => {}
        option::Option::None => {}
    }
}
"#,
    );
    write_wave(&dir, "facade.wave", "pub import(\"./option\")::{Option};\n");
    let reexported = write_wave(
        &dir,
        "reexported.wave",
        r#"
import("./facade")::{Option};

fun empty() -> Option<i32> {
    return Option::None;
}
"#,
    );
    let nested = write_wave(
        &dir,
        "nested.wave",
        r#"
variant Inner<T> {
    Value(T)
}

variant Outer<T> {
    Wrap(Inner<T>)
}

fun infer_nested() {
    var value: Outer<i32> = Outer::Wrap(Inner::Value(11));
    match value {
        Outer::Wrap(Inner::Value(item)) => {}
    }
}
"#,
    );

    run_wavec(["check", selected.to_str().unwrap()]);
    run_wavec(["check", namespaced.to_str().unwrap()]);
    run_wavec(["check", reexported.to_str().unwrap()]);
    run_wavec(["check", nested.to_str().unwrap()]);
}

fn bytes_contains(haystack: &[u8], needle: &[u8]) -> bool {
    haystack
        .windows(needle.len())
        .any(|window| window == needle)
}

fn riscv64_elf_flags(path: &Path) -> u32 {
    let object = fs::read(path).unwrap();
    assert!(
        object.len() >= 52,
        "truncated ELF object: {}",
        path.display()
    );
    assert_eq!(&object[..4], b"\x7fELF", "{}", path.display());
    assert_eq!(object[4], 2, "expected ELF64 object: {}", path.display());
    assert_eq!(u16::from_le_bytes([object[18], object[19]]), 243);
    u32::from_le_bytes([object[48], object[49], object[50], object[51]])
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
fn loongarch64_elf_flags(path: &Path) -> u32 {
    let object = fs::read(path).unwrap();
    assert!(
        object.len() >= 52,
        "truncated ELF object: {}",
        path.display()
    );
    assert_eq!(&object[..4], b"\x7fELF", "{}", path.display());
    assert_eq!(object[4], 2, "expected ELF64 object: {}", path.display());
    assert_eq!(u16::from_le_bytes([object[18], object[19]]), 258);
    u32::from_le_bytes([object[48], object[49], object[50], object[51]])
}

fn json_string_for_test(value: &str) -> String {
    let mut out = String::from("\"");
    for ch in value.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if c.is_control() => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

fn json_contains_path_components(json: &str, components: &[&str]) -> bool {
    if components.is_empty() {
        return true;
    }
    // Check direct matches first (all forward slash or all backslash)
    if json_contains_path_value(json, &components.join("/"))
        || json_contains_path_value(json, &components.join("\\"))
    {
        return true;
    }

    // Handle mixed separators or escaped separators by checking normalized token runs
    let normalized_json = json.replace("\\\\", "/").replace('\\', "/");
    let normalized_target = components.join("/");
    normalized_json.contains(&normalized_target)
}

fn json_contains_path_value(json: &str, value: &str) -> bool {
    json.contains(value) || json.contains(&value.replace('\\', "\\\\"))
}

fn write_minimal_elf64_object(path: &Path, machine: u16) {
    let mut header = [0_u8; 64];
    header[..4].copy_from_slice(b"\x7fELF");
    header[4] = 2;
    header[5] = 1;
    header[6] = 1;
    header[16..18].copy_from_slice(&1_u16.to_le_bytes());
    header[18..20].copy_from_slice(&machine.to_le_bytes());
    fs::write(path, header).unwrap();
}

#[test]
fn json_path_matching_accepts_unix_and_escaped_windows_separators() {
    let components = ["crt", "riscv64-unknown-linux-gnu", "crt1.o"];
    assert!(json_contains_path_components(
        r#"{"args":["/opt/wave/crt/riscv64-unknown-linux-gnu/crt1.o"]}"#,
        &components
    ));
    assert!(json_contains_path_components(
        r#"{"args":["C:\\wave\\crt\\riscv64-unknown-linux-gnu\\crt1.o"]}"#,
        &components
    ));
    assert!(json_contains_path_components(
        r#"{"args":["D:\\sysroot\\usr/lib\\crt1.o"]}"#,
        &["sysroot", "usr", "lib", "crt1.o"]
    ));
    assert!(json_contains_path_value(
        r#"{"args":["-LD:\\wave\\sysroot\\lib"]}"#,
        r#"-LD:\wave\sysroot\lib"#
    ));
}

#[test]
fn retired_let_declarations_are_rejected() {
    let dir = temp_case_dir("retired-let-syntax");
    let cases = [
        ("let.wave", "fun main() { let value: i32 = 1; }\n"),
        (
            "let_mut.wave",
            "fun main() { let mut value: i32 = 1; value += 1; }\n",
        ),
        (
            "for_let.wave",
            "fun main() { for (let index: i32 = 0; index < 1; index += 1) {} }\n",
        ),
    ];

    for (file_name, source) in cases {
        let source = write_wave(&dir, file_name, source);
        let error = run_wavec_expect_failure([OsStr::new("check"), source.as_os_str()]);
        assert!(error.contains("error[E2001]"), "{}: {}", file_name, error);
        assert!(error.contains("Let (`let`)"), "{}: {}", file_name, error);
    }
}

#[test]
fn import_graph_supports_packages_aliases_selections_and_visibility() {
    let dir = temp_case_dir("module-import-contract");
    let package = dir.join("add");
    fs::create_dir_all(package.join("src")).unwrap();
    fs::write(
        package.join("src/lib.wave"),
        r#"
fun internal_sum(a: i32, b: i32) -> i32 { return a + b; }
pub fun sum(a: i32, b: i32) -> i32 { return internal_sum(a, b); }
pub struct Point {}
pub import("./extra")::{increment};
"#,
    )
    .unwrap();
    fs::write(
        package.join("src/extra.wave"),
        "pub fun increment(value: i32) -> i32 { return value + 1; }\n",
    )
    .unwrap();
    fs::write(
        package.join("src/math.wave"),
        "pub fun double(value: i32) -> i32 { return value * 2; }\n",
    )
    .unwrap();
    fs::write(
        dir.join("helpers.wave"),
        "fun scale(value: i32) -> i32 { return value * 3; }\npub fun triple(value: i32) -> i32 { return scale(value); }\n",
    )
    .unwrap();
    fs::write(
        dir.join("other.wave"),
        "fun scale(value: i32) -> i32 { return value * 4; }\npub fun quadruple(value: i32) -> i32 { return scale(value); }\n",
    )
    .unwrap();

    let entry = write_wave(
        &dir,
        "main.wave",
        r#"
import("add");
import("add::math");
import("./helpers" as helpers);
import("./other" as other);
import("add")::{sum, Point, increment};

fun main() {
    var a: i32 = add::sum(1, 2);
    var b: i32 = sum(1, 2);
    var c: i32 = add::math::double(2);
    var d: i32 = helpers::triple(3);
    var f: i32 = other::quadruple(3);
    var p: Point = Point();
    var e: i32 = increment(4);
}
"#,
    );
    let mapping = OsString::from(format!("add={}", package.display()));
    run_wavec([
        OsStr::new("check"),
        entry.as_os_str(),
        OsStr::new("--dep"),
        mapping.as_os_str(),
    ]);

    let selected_private = write_wave(
        &dir,
        "selected_private.wave",
        "import(\"add\")::{internal_sum};\nfun main() {}\n",
    );
    let error = run_wavec_expect_failure([
        OsStr::new("check"),
        selected_private.as_os_str(),
        OsStr::new("--dep"),
        mapping.as_os_str(),
    ]);
    assert!(
        error.contains("symbol 'internal_sum' is private in module 'add'"),
        "{}",
        error
    );

    let qualified_private = write_wave(
        &dir,
        "qualified_private.wave",
        "import(\"add\");\nfun main() { var n: i32 = add::internal_sum(1, 2); }\n",
    );
    let error = run_wavec_expect_failure([
        OsStr::new("check"),
        qualified_private.as_os_str(),
        OsStr::new("--dep"),
        mapping.as_os_str(),
    ]);
    assert!(
        error.contains("symbol 'internal_sum' is private in module 'add'"),
        "{}",
        error
    );
}

#[test]
fn imported_generic_struct_literals_specialize_across_modules() {
    let dir = temp_case_dir("module-generic-struct-literal");
    fs::write(
        dir.join("result.wave"),
        r#"
pub struct Result<T> {
    ok: bool;
    value: T;
}

pub fun success<T>(value: T) -> Result<T> {
    return Result<T> { ok: true, value: value };
}
"#,
    )
    .unwrap();
    let entry = write_wave(
        &dir,
        "main.wave",
        r#"
import("./result")::{Result, success};
import("./result");

struct Payload {
    value: i64;
}

fun main() -> i32 {
    var direct: Result<i32> = Result<i32> { ok: true, value: 7 };
    var nested: Result<Payload> = success<Payload>(Payload { value: 42 });
    var qualified: result::Result<i64> = result::Result<i64> { ok: true, value: 9 };
    if (!direct.ok || direct.value != 7) { return 1; }
    if (!nested.ok || nested.value.value != 42) { return 2; }
    if (!qualified.ok || qualified.value != 9) { return 3; }
    return 0;
}
"#,
    );

    run_wavec([OsStr::new("run"), entry.as_os_str()]);
}

#[test]
fn integer_right_shift_preserves_wave_signedness() {
    let dir = temp_case_dir("integer-right-shift-signedness");
    let source = write_wave(
        &dir,
        "shift.wave",
        r#"
fun logical_shift(value: u32) -> u32 { return value >> 24; }
fun arithmetic_shift(value: i32) -> i32 { return value >> 24; }
fun main() -> i32 { return logical_shift(0x80000000 as u32) as i32; }
"#,
    );
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]);
    let ir = fs::read_to_string(dir.join("shift.ll")).unwrap();
    assert!(
        ir.contains("lshr i32"),
        "unsigned shift must use lshr:\n{ir}"
    );
    assert!(ir.contains("ashr i32"), "signed shift must use ashr:\n{ir}");
}

#[test]
fn integer_division_remainder_and_comparison_preserve_wave_signedness() {
    let dir = temp_case_dir("integer-operation-signedness");
    let source = write_wave(
        &dir,
        "integer_ops.wave",
        r#"
fun unsigned_div(value: u32) -> u32 { return value / 2; }
fun unsigned_rem(value: u32) -> u32 { return value % 3; }
fun unsigned_gt(value: u32) -> bool { return value > 1; }
fun nested_unsigned_shift(value: u16) -> u16 {
    return (value & 0xFF00) >> 8;
}
fun unsigned_compound(value: u32) -> u32 {
    var result: u32 = value;
    result /= 2;
    result %= 5;
    return result;
}

fun signed_div(value: i32) -> i32 { return value / 2; }
fun signed_rem(value: i32) -> i32 { return value % 3; }
fun signed_gt(value: i32) -> bool { return value > 1; }

fun main() -> i32 {
    var high: u32 = 0x80000000 as u32;
    if (unsigned_div(high) != 1073741824) { return 1; }
    if (unsigned_rem(high) != 2) { return 2; }
    if (unsigned_gt(high) as i32 != 1) { return 3; }
    if (unsigned_compound(high) != 4) { return 4; }
    if (signed_div(-9) != -4) { return 5; }
    if (signed_rem(-8) != -2) { return 6; }
    if (signed_gt(-1)) { return 7; }
    if (nested_unsigned_shift(0x8001) != 0x80) { return 8; }
    return 0;
}
"#,
    );
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]);
    let ir = fs::read_to_string(dir.join("integer_ops.ll")).unwrap();
    for instruction in ["udiv i32", "urem i32", "icmp ugt i32"] {
        assert!(
            ir.contains(instruction),
            "unsigned integers must use {instruction}:\n{ir}"
        );
    }
    assert!(
        ir.contains("zext i1"),
        "boolean comparison results must normalize true to one:\n{ir}"
    );
    assert!(
        ir.contains("lshr i16"),
        "nested u16 bitwise expressions must retain unsigned shift semantics:\n{ir}"
    );
    for instruction in ["sdiv i32", "srem i32", "icmp sgt i32"] {
        assert!(
            ir.contains(instruction),
            "signed integers must use {instruction}:\n{ir}"
        );
    }
    run_wavec([OsStr::new("run"), source.as_os_str()]);

    let invalid_literal = write_wave(
        &dir,
        "invalid_literal.wave",
        "fun mask(value: u16) -> u16 { return value & 0x10000; }\nfun main() {}\n",
    );
    let error = run_wavec_expect_failure([OsStr::new("check"), invalid_literal.as_os_str()]);
    assert!(
        error.contains("integer literal `0x10000` does not fit `u16`"),
        "{error}"
    );
}

#[test]
fn explicit_integer_widening_preserves_wave_signedness() {
    let dir = temp_case_dir("integer-widening-signedness");
    let source = write_wave(
        &dir,
        "widen.wave",
        r#"
fun widen_unsigned(value: u8) -> u32 { return value as u32; }
fun widen_signed(value: i8) -> i32 { return value as i32; }
fun wide_literal() -> u64 { return 0x7FF8000000000000 as u64; }
fun main() -> i32 {
    if (widen_unsigned(192 as u8) != 192) { return 1; }
    if (widen_signed(-64 as i8) != -64) { return 2; }
    if (wide_literal() != 0x7FF8000000000000) { return 3; }
    return 0;
}
"#,
    );
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]);
    let ir = fs::read_to_string(dir.join("widen.ll")).unwrap();
    assert!(
        ir.contains("zext i8"),
        "unsigned widening must use zext:\n{ir}"
    );
    assert!(
        ir.contains("sext i8"),
        "signed widening must use sext:\n{ir}"
    );
    assert!(
        ir.contains("ret i64 9221120237041090560"),
        "an explicitly cast wide literal must retain all target bits:\n{ir}"
    );
    run_wavec([OsStr::new("run"), source.as_os_str()]);
}

#[test]
fn unsigned_float_conversions_and_member_compounds_preserve_signedness() {
    let dir = temp_case_dir("unsigned-float-and-member-signedness");
    let source = write_wave(
        &dir,
        "unsigned_float.wave",
        r#"
struct Counter { value: u32; }

fun member_compound(value: u32) -> u32 {
    var counter: Counter = Counter { value: value };
    counter.value /= 2;
    counter.value %= 5;
    return counter.value;
}

fun explicit_to_float(value: u32) -> f64 { return value as f64; }
fun explicit_to_uint(value: f64) -> u32 { return value as u32; }
fun wide_decimal_literal_to_float() -> f64 { return 1099511627776 as f64; }
fun wide_hex_literal_to_float() -> f64 { return 0x10000000000 as f64; }
fun precise_literal_to_f64() -> f64 { return 1.0000000000000002 as f64; }
fun mixed_left(value: u32) -> f64 { return value + 0.0; }
fun mixed_right(value: u32) -> f64 { return 0.0 + value; }

fun float_compound(value: u32) -> f64 {
    var result: f64 = 0.0;
    result += value;
    return result;
}

fun main() -> i32 {
    var high: u32 = 0x80000000 as u32;
    if (member_compound(high) != 4) { return 1; }
    if (explicit_to_float(high) != 2147483648.0) { return 2; }
    if (explicit_to_uint(2147483648.0) != high) { return 3; }
    if (wide_decimal_literal_to_float() != 1099511627776.0) { return 4; }
    if (wide_hex_literal_to_float() != 1099511627776.0) { return 5; }
    if (precise_literal_to_f64() == 1.0) { return 6; }
    if (mixed_left(high) != 2147483648.0) { return 7; }
    if (mixed_right(high) != 2147483648.0) { return 8; }
    if (float_compound(high) != 2147483648.0) { return 9; }
    return 0;
}
"#,
    );
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]);
    let ir = fs::read_to_string(dir.join("unsigned_float.ll")).unwrap();
    for instruction in ["uitofp i32", "fptoui double", "udiv i32", "urem i32"] {
        assert!(
            ir.contains(instruction),
            "unsigned numeric lowering must use {instruction}:\n{ir}"
        );
    }
    run_wavec([OsStr::new("run"), source.as_os_str()]);
}

#[test]
fn implicit_unsigned_widening_pointer_offsets_and_grouped_casts_preserve_values() {
    let dir = temp_case_dir("implicit-unsigned-widening-and-pointer-offsets");
    let source = write_wave(
        &dir,
        "unsigned_contexts.wave",
        r#"
struct WideField { value: u32; }

fun identity_u32(value: u32) -> u32 { return value; }
fun widen_local(value: u8) -> u32 {
    var widened: u32 = value;
    return widened;
}
fun widen_assign(value: u8) -> u32 {
    var widened: u32 = 0;
    widened = value;
    return widened;
}
fun widen_return(value: u8) -> u32 { return value; }
fun widen_call(value: u8) -> u32 { return identity_u32(value); }
fun widen_field(value: u8) -> u32 {
    var holder: WideField = WideField { value: value };
    return holder.value;
}
fun widen_array(value: u8) -> u32 {
    var values: array<u32, 1> = [value];
    return values[0];
}
fun add_offset(base: ptr<u8>, offset: u8) -> ptr<u8> { return base + offset; }
fun add_offset_left(offset: u8, base: ptr<u8>) -> ptr<u8> { return offset + base; }
fun add_offset_u32(base: ptr<u8>, offset: u32) -> ptr<u8> { return base + offset; }
fun widen_u32(value: u32) -> u64 {
    var widened: u64 = value;
    return widened;
}

fun main() -> i32 {
    var high: u8 = 255 as u8;
    if (widen_local(high) != 255) { return 1; }
    if (widen_assign(high) != 255) { return 2; }
    if (widen_return(high) != 255) { return 3; }
    if (widen_call(high) != 255) { return 4; }
    if (widen_field(high) != 255) { return 5; }
    if (widen_array(high) != 255) { return 6; }
    if (widen_u32(0x80000000 as u32) != 2147483648) { return 7; }

    var storage: array<u8, 256>;
    storage[high] = 77;
    if (storage[255] != 77) { return 8; }

    var wide_index: u32 = 254;
    storage[wide_index] = 76;
    if (storage[254] != 76) { return 9; }

    var base: ptr<u8> = &storage[0];
    if (add_offset(base, high) != &storage[255]) { return 10; }
    if (add_offset_left(high, base) != &storage[255]) { return 11; }
    if (add_offset_u32(base, wide_index) != &storage[254]) { return 12; }

    if ((-9223372036854775808) as f64 != -9223372036854775808.0) { return 13; }
    if ((0x10000000000) as f64 != 1099511627776.0) { return 14; }
    if ((1 / 2) as f64 != 0.0) { return 15; }
    return 0;
}
"#,
    );
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]);
    let ir = fs::read_to_string(dir.join("unsigned_contexts.ll")).unwrap();
    assert!(
        ir.matches("zext i8").count() >= 4,
        "implicit unsigned widening must use zext in every value context:\n{ir}"
    );
    assert!(
        ir.contains("idx_zext") && ir.contains("ptr_idx_zext") && ir.contains("zext i32"),
        "unsigned index and pointer offsets must zero-extend to pointer width:\n{ir}"
    );
    run_wavec([OsStr::new("run"), source.as_os_str()]);
}

#[test]
fn conundrum_example_finishes_its_gallery() {
    let source = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/conundrum.wave");
    let (stdout, stderr) = run_wavec_capture([OsStr::new("run"), source.as_os_str()]);
    assert_eq!(stdout.trim(), "gallery checked");
    assert!(stderr.is_empty(), "{stderr}");
}

#[test]
fn integer_index_expressions_preserve_values_and_evaluate_once() {
    let dir = temp_case_dir("integer-index-expressions");
    let source = write_wave(
        &dir,
        "indices.wave",
        r#"
const ORDER: array<i32, 3> = [2, 0, 1];
struct Cursor {
    position: i32;
    calls: i32;
}
fun advance(cursor: ptr<Cursor>) -> i32 {
    cursor.calls += 1;
    var result: i32 = cursor.position;
    cursor.position += 1;
    return result;
}
fun choose(value: u8) -> u8 {
    return value;
}
fun main() -> i32 {
    var values: array<i32, 4> = [10, 20, 30, 40];
    var cursor: Cursor = Cursor { position: 0, calls: 0 };
    values[advance(&cursor)] += 7;
    values[advance(&cursor)]++;
    var slot: ptr<i32> = &values[advance(&cursor)];
    deref slot = 32;
    if (cursor.calls != 3 || cursor.position != 3) {
        return 1;
    }
    if (values[0] != 17 || values[1] != 21 || values[2] != 32) {
        return 2;
    }
    var step: i32 = 1;
    if (values[(step * 2) - 1] != 21) {
        return 3;
    }
    if (values[0x2] != 32 || values[(0b11 as u128)] != 40) {
        return 4;
    }
    var grid: array<array<i32, 2>, 2> = [[3, 5], [7, 11]];
    grid[step - 1][step + 0] = 13;
    if (grid[ORDER[step]][ORDER[step + 1]] != 13) {
        return 5;
    }
    var tail: ptr<i32> = &values[2];
    var backward: i8 = -1;
    if (tail[backward] != 21 || tail[-2] != 17) {
        return 6;
    }
    tail[(backward as i32) + 1] = 33;
    if (values[cursor.position - 1] != 33) {
        return 7;
    }
    var bytes: array<i32, 256>;
    bytes[255] = 91;
    var high: u8 = 255;
    if (bytes[high] != 91 || bytes[choose(high)] != 91) {
        return 8;
    }
    var indexes: array<u8, 1> = [high];
    if (bytes[indexes[step - 1]] != 91) {
        return 9;
    }
    var projected: ptr<array<i32, 4>> = &values;
    if (projected[cursor.position - 2] != 21) {
        return 10;
    }
    return 0;
}
"#,
    );
    run_wavec([OsStr::new("check"), source.as_os_str()]);
    run_wavec([OsStr::new("run"), source.as_os_str()]);
}

#[test]
fn integer_index_input_destinations_use_the_same_typed_offsets() {
    use std::io::Write;
    use std::process::Stdio;

    let dir = temp_case_dir("integer-index-input");
    let source = write_wave(
        &dir,
        "input_indices.wave",
        r#"
fun choose(value: u8) -> u8 {
    return value;
}
fun main() -> i32 {
    var values: array<i32, 256>;
    var high: u8 = 255;
    input("{}", values[choose(high)]);
    if (values[255] != 73) {
        return 1;
    }
    return 0;
}
"#,
    );
    let mut child = wavec_command()
        .arg("run")
        .arg(&source)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    child.stdin.take().unwrap().write_all(b"73\n").unwrap();
    let output = child.wait_with_output().unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn integer_index_offsets_follow_target_width_and_signedness() {
    let dir = temp_case_dir("integer-index-widths");
    let source = write_wave(
        &dir,
        "widths.wave",
        r#"
fun narrow_signed(base: ptr<u8>, offset: i8) -> ptr<u8> {
    return &base[offset];
}
fun narrow_unsigned(base: ptr<u8>, offset: u8) -> ptr<u8> {
    return &base[offset];
}
fun word_unsigned(base: ptr<u8>, offset: u32) -> ptr<u8> {
    return &base[offset];
}
fun wide_unsigned(base: ptr<u8>, offset: u64) -> ptr<u8> {
    return &base[offset];
}
fun widest_unsigned(base: ptr<u8>, offset: u128) -> ptr<u8> {
    return &base[offset];
}
fun main() -> i32 {
    return 0;
}
"#,
    );
    let mut targets = vec![("x86_64-unknown-linux-gnu", 64)];
    if cfg!(any(
        feature = "llvm-target-all",
        feature = "llvm-target-wasm"
    )) {
        targets.push(("wasm32-unknown-unknown", 32));
        targets.push(("wasm64-unknown-unknown", 64));
    }
    for (target, width) in targets {
        let out = dir.join(target);
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--emit=ir"),
            OsStr::new("--out-dir"),
            out.as_os_str(),
        ]);
        let ir = fs::read_to_string(out.join("widths.ll")).unwrap();
        for (function, instruction, operand) in [
            ("narrow_signed", "sext", "i8"),
            ("narrow_unsigned", "zext", "i8"),
            ("widest_unsigned", "trunc", "i128"),
        ] {
            let body = ir
                .split(&format!("@{function}("))
                .nth(1)
                .unwrap()
                .split('}')
                .next()
                .unwrap();
            assert!(
                body.contains(&format!("{instruction} {operand}"))
                    && body.contains(&format!("to i{width}")),
                "{target}: {body}"
            );
        }
        let body = ir
            .split("@word_unsigned(")
            .nth(1)
            .unwrap()
            .split('}')
            .next()
            .unwrap();
        assert_eq!(body.contains("zext i32"), width == 64, "{target}: {body}");
        let body = ir
            .split("@wide_unsigned(")
            .nth(1)
            .unwrap()
            .split('}')
            .next()
            .unwrap();
        assert_eq!(body.contains("trunc i64"), width == 32, "{target}: {body}");
    }
}

#[test]
fn noninteger_index_results_report_source_diagnostics() {
    let dir = temp_case_dir("noninteger-index-results");
    for (name, ty, value) in [("float", "f64", "1.0"), ("text", "str", "\"bad\"")] {
        let source = write_wave(&dir, &format!("{name}.wave"), &format!(
            "fun select() -> {ty} {{ return {value}; }}\nfun main() {{\n    var values: array<i32, 2> = [1, 2];\n    println(\"{{}}\", values[select()]);\n}}\n"
        ));
        for format in ["--error-format=human", "--error-format=json"] {
            let output =
                run_wavec_raw([OsStr::new("check"), source.as_os_str(), OsStr::new(format)]);
            assert!(!output.status.success());
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("E3001") && stderr.contains("index expression must be an integer"),
                "{stderr}"
            );
            assert!(!stderr.contains("E9001"), "{stderr}");
            if format.ends_with("json") {
                assert!(stderr.contains("\"line\":4"), "{stderr}");
            } else {
                assert!(stderr.contains(&format!("{name}.wave:4:")), "{stderr}");
            }
        }
    }
}

#[test]
fn numeric_comparisons_do_not_inherit_the_boolean_result_width() {
    let dir = temp_case_dir("numeric-comparison-width");
    let source = write_wave(
        &dir,
        "compare.wave",
        r#"
fun is_connect_pending(value: i64) -> bool {
    return value == -36 || value == -115 || value == -10035;
}
fun main() -> i32 {
    if (!is_connect_pending(-36)) { return 1; }
    if (!is_connect_pending(-115)) { return 2; }
    if (!is_connect_pending(-10035)) { return 3; }
    if (is_connect_pending(220)) { return 4; }
    return 0;
}
"#,
    );
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]);
    let ir = fs::read_to_string(dir.join("compare.ll")).unwrap();
    assert!(
        ir.contains("icmp eq i64"),
        "numeric comparisons must retain the operand width:\n{ir}"
    );
    run_wavec([OsStr::new("run"), source.as_os_str()]);
}

#[test]
fn freebsd_lp64_providers_emit_direct_syscalls_and_kernel_layouts() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let dir = temp_case_dir("freebsd-raw-providers");
    let home = dir.join("home");
    copy_tree(&root.join("std"), &home.join(".wave/lib/wave/std"));
    for (arch, target, instruction, registers) in [
        (
            "amd64",
            "x86_64-unknown-freebsd",
            "syscall",
            ["={rax}", "={rdx}"],
        ),
        (
            "arm64",
            "aarch64-unknown-freebsd",
            "svc $3",
            ["={x0}", "={x1}"],
        ),
        (
            "riscv64",
            "riscv64-unknown-freebsd",
            "ecall",
            ["={x10}", "={x11}"],
        ),
    ] {
        for number in [11, 12] {
            let source = root.join(format!("tests/cases/freebsd/{arch}/test{number}.wave"));
            for optimization in ["-O0", "-O2"] {
                let output_dir = dir.join(arch).join(number.to_string()).join(optimization);
                for emit in ["--emit=ir", "--emit=obj"] {
                    let output = wavec_command()
                        .env("HOME", &home)
                        .arg("build")
                        .arg(&source)
                        .args(["--target", target, emit, optimization])
                        .arg("--out-dir")
                        .arg(&output_dir)
                        .output()
                        .unwrap();
                    assert!(
                        output.status.success(),
                        "{target} test{number} {optimization} {emit}:\n{}\n{}",
                        String::from_utf8_lossy(&output.stdout),
                        String::from_utf8_lossy(&output.stderr)
                    );
                    assert!(
                        output.stderr.is_empty(),
                        "{target} must compile without backend diagnostics: {}",
                        String::from_utf8_lossy(&output.stderr)
                    );
                }
                let ir = fs::read_to_string(output_dir.join(format!("test{number}.ll"))).unwrap();
                assert!(
                    ir.contains(instruction),
                    "{target} must issue a kernel trap"
                );
                for register in registers {
                    assert!(
                        ir.contains(register),
                        "{target} must declare kernel return register {register}"
                    );
                }
                for line in ir.lines().filter(|line| line.starts_with("declare ")) {
                    assert!(
                        line.contains("@llvm."),
                        "{target} raw sys provider acquired an external dependency: {line}"
                    );
                }
                if optimization == "-O0" && number == 12 {
                    assert!(
                        ir.contains(
                            "NativeKevent = type { i64, i16, i16, i32, i64, ptr, [4 x i64] }"
                        ),
                        "FreeBSD kevent requires ext[4] and a 64-byte stride"
                    );
                    assert!(
                        ir.contains("NativeMessage = type { ptr, i32, ptr, i32, ptr, i32, i32 }"),
                        "FreeBSD msghdr uses 32-bit socklen_t and iovlen"
                    );
                }
            }
        }
    }
}

#[test]
fn std_net_compiles_for_every_supported_socket_abi() {
    let dir = temp_case_dir("std-net-target-matrix");
    let home = dir.join("home");
    let std_destination = home.join(".wave/lib/wave/std");
    copy_tree(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("std"),
        &std_destination,
    );

    let sources = [
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/std/net_tcp.wave"),
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/std/net_udp.wave"),
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/std/net_ipv6.wave"),
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/std/net_dns.wave"),
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/std/net_unix.wave"),
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/std/net_interfaces.wave"),
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/std/net_vectored.wave"),
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/std/net_event.wave"),
    ];
    let targets = [
        "x86_64-unknown-linux-gnu",
        "aarch64-unknown-linux-gnu",
        "riscv64-unknown-linux-gnu",
        #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
        "loongarch64-unknown-linux-gnu",
        "x86_64-apple-darwin",
        "aarch64-apple-darwin",
        "x86_64-pc-windows-gnu",
        "aarch64-w64-windows-gnu",
        "x86_64-unknown-freebsd",
        "aarch64-unknown-freebsd",
        "riscv64-unknown-freebsd",
    ];

    for target in targets {
        for source in &sources {
            let output_dir = dir.join(target).join(source.file_stem().unwrap());
            let output = wavec_command()
                .env("HOME", &home)
                .arg("build")
                .arg(source)
                .arg("--target")
                .arg(target)
                .arg("--emit=ir")
                .arg("--out-dir")
                .arg(&output_dir)
                .output()
                .unwrap();
            assert!(
                output.status.success(),
                "{target} {} failed\nstdout:\n{}\nstderr:\n{}",
                source.display(),
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            );

            if source.file_stem().unwrap() == "net_dns" {
                let ir_path = output_dir.join("net_dns.ll");
                let ir = fs::read_to_string(&ir_path).unwrap();
                let bsd_sockaddr = target.contains("apple") || target.contains("freebsd");
                if bsd_sockaddr {
                    assert!(
                        ir.contains("NativeSockAddrV4 = type { i8, i8, i16, i32, [8 x i8] }"),
                        "{target} must use the BSD sockaddr_in field order"
                    );
                    assert!(
                        ir.contains("NativeSockAddrV6 = type { i8, i8, i16, i32, [16 x i8], i32 }"),
                        "{target} must use the BSD sockaddr_in6 field order"
                    );
                } else {
                    assert!(
                        ir.contains("NativeSockAddrV4 = type { i16, i16, i32, [8 x i8] }"),
                        "{target} must use the Linux/Windows sockaddr_in field order"
                    );
                    assert!(
                        ir.contains("NativeSockAddrV6 = type { i16, i16, i32, [16 x i8], i32 }"),
                        "{target} must use the Linux/Windows sockaddr_in6 field order"
                    );
                }

                let addrinfo_length = if target.contains("windows") {
                    "NativeAddrInfo = type { i32, i32, i32, i32, i64, ptr, ptr, ptr }"
                } else {
                    "NativeAddrInfo = type { i32, i32, i32, i32, i32, ptr, ptr, ptr }"
                };
                assert!(
                    ir.contains(addrinfo_length),
                    "{target} must use the target addrinfo address-length width"
                );
            }

            if source.file_stem().unwrap() == "net_event" {
                let ir = fs::read_to_string(output_dir.join("net_event.ll")).unwrap();
                let backend_symbol = if target.contains("linux") {
                    "_event_create("
                } else if target.contains("freebsd") {
                    "_native_kevent("
                } else if target.contains("apple") {
                    "@kqueue"
                } else {
                    "@WSAPoll"
                };
                assert!(
                    ir.contains(backend_symbol),
                    "{target} must select readiness backend {backend_symbol}"
                );

                if target.contains("linux") {
                    for obsolete in ["@epoll_create1(", "@epoll_ctl(", "@epoll_pwait("] {
                        assert!(
                            !ir.contains(obsolete),
                            "{target} must use kernel syscalls, not {obsolete}"
                        );
                    }
                    let create_number = if target.starts_with("x86_64-") {
                        291
                    } else {
                        20
                    };
                    assert!(
                        ir.contains(&format!("_syscall1(i64 {create_number}, i64 0)")),
                        "{target} must call epoll_create1 through its native syscall number"
                    );
                    let (stride, data_offset) = if target.starts_with("x86_64-") {
                        (12, 4)
                    } else {
                        (16, 8)
                    };
                    assert!(
                        ir.contains(&format!(
                            "__epoll_event_stride() #0 {{\nentry:\n  ret i32 {stride}"
                        )),
                        "{target} must use the Linux epoll_event stride {stride}"
                    );
                    assert!(
                        ir.contains(&format!(
                            "__epoll_data_offset() #0 {{\nentry:\n  ret i32 {data_offset}"
                        )),
                        "{target} must use the Linux epoll data offset {data_offset}"
                    );
                }
            }
        }
    }
}

#[test]
fn import_graph_rejects_cycles_path_escape_and_public_main() {
    let dir = temp_case_dir("module-import-errors");
    fs::write(
        dir.join("a.wave"),
        "import(\"./b\");\npub fun from_a() -> i32 { return 1; }\n",
    )
    .unwrap();
    fs::write(
        dir.join("b.wave"),
        "import(\"./a\");\npub fun from_b() -> i32 { return 2; }\n",
    )
    .unwrap();
    let cycle = write_wave(&dir, "cycle.wave", "import(\"./a\");\nfun main() {}\n");
    let error = run_wavec_expect_failure([OsStr::new("check"), cycle.as_os_str()]);
    assert!(error.contains("import cycle detected"), "{}", error);

    let escape = write_wave(
        &dir,
        "escape.wave",
        "import(\"./../outside\");\nfun main() {}\n",
    );
    let error = run_wavec_expect_failure([OsStr::new("check"), escape.as_os_str()]);
    assert!(error.contains("escapes its module directory"), "{}", error);

    let package_escape = write_wave(
        &dir,
        "package_escape.wave",
        "import(\"add::..\");\nfun main() {}\n",
    );
    let error = run_wavec_expect_failure([OsStr::new("check"), package_escape.as_os_str()]);
    assert!(
        error.contains("package and module names must be identifiers"),
        "{}",
        error
    );

    let public_main = write_wave(&dir, "public_main.wave", "pub fun main() {}\n");
    let error = run_wavec_expect_failure([OsStr::new("check"), public_main.as_os_str()]);
    assert!(
        error.contains("entry function `main` cannot be public"),
        "{}",
        error
    );

    fs::write(dir.join("library_main.wave"), "fun main() {}\n").unwrap();
    let imported_main = write_wave(
        &dir,
        "imported_main.wave",
        "import(\"./library_main\");\nfun main() {}\n",
    );
    let error = run_wavec_expect_failure([OsStr::new("check"), imported_main.as_os_str()]);
    assert!(
        error.contains("function 'main' may only be declared in the entry module"),
        "{}",
        error
    );
}

fn run_link_tests_enabled() -> bool {
    std::env::var_os("WAVE_RUN_LINK_TESTS").is_some()
}

#[test]
fn semantic_validation_rejects_invalid_returns_calls_and_loop_control() {
    let dir = temp_case_dir("semantic-validation");
    let cases = [
        (
            "wrong_return_type.wave",
            r#"
fun value() -> i32 {
    return "not an integer";
}

"#,
            "type mismatch in return value of function `value`",
        ),
        (
            "value_from_void.wave",
            r#"
fun noop() {
    return 1;
}
"#,
            "void function `noop` cannot return a value",
        ),
        (
            "empty_non_void_return.wave",
            r#"
fun value() -> i32 {
    return;
}
"#,
            "non-void function `value` must return `i32`",
        ),
        (
            "missing_return_path.wave",
            r#"
fun value(flag: bool) -> i32 {
    if (flag) {
        return 1;
    }
}
"#,
            "non-void function `value` may exit without returning `i32`",
        ),
        (
            "break_outside_loop.wave",
            r#"
fun main() {
    break;
}
"#,
            "`break` can only be used inside a loop",
        ),
        (
            "continue_outside_loop.wave",
            r#"
fun main() {
    continue;
}
"#,
            "`continue` can only be used inside a loop",
        ),
        (
            "wrong_call_type.wave",
            r#"
fun identity(value: i32) -> i32 {
    return value;
}

fun main() {
    identity("text");
}
"#,
            "type mismatch in argument 1 of function `identity`",
        ),
        (
            "wrong_main_return.wave",
            r#"
fun main() -> f64 {
    return 0.0;
}
"#,
            "entry function `main` must return `i32` or omit its return type",
        ),
        (
            "generic_main.wave",
            r#"
fun main<T>() {}
"#,
            "entry function `main` cannot declare generic parameters",
        ),
    ];

    for (file_name, source, expected) in cases {
        let source = write_wave(&dir, file_name, source);
        for mode in ["check", "build"] {
            let output = if mode == "check" {
                run_wavec_raw([OsStr::new("check"), source.as_os_str()])
            } else {
                run_wavec_raw([
                    OsStr::new("build"),
                    source.as_os_str(),
                    OsStr::new("--emit=obj"),
                    OsStr::new("--out-dir"),
                    dir.as_os_str(),
                ])
            };
            assert!(
                !output.status.success(),
                "{} unexpectedly passed semantic validation in {} mode",
                file_name,
                mode
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("error[E3001]"),
                "{} ({}): {}",
                file_name,
                mode,
                stderr
            );
            assert!(
                stderr.contains(expected),
                "{} ({}): {}",
                file_name,
                mode,
                stderr
            );
            assert!(
                !stderr.contains("E9001") && !stderr.contains("compiler internal error"),
                "{} ({}) leaked a backend failure: {}",
                file_name,
                mode,
                stderr
            );
        }
    }
}

#[test]
fn semantic_validation_rejects_backend_only_type_failures_early() {
    let dir = temp_case_dir("semantic-backend-parity");
    let cases = [
        (
            "narrow_return.wave",
            "fun narrow(x: i64) -> i32 { return x; }\nfun main() {}\n",
            "expected `i32`, found `i64`",
        ),
        (
            "narrow_call.wave",
            "fun take(x: i32) {}\nfun main() { var wide: i64 = 1; take(wide); }\n",
            "argument 1 of function `take`",
        ),
        (
            "narrow_initializer.wave",
            "fun main() { var wide: i64 = 1; var narrow: i32 = wide; }\n",
            "initializer for `narrow`",
        ),
        (
            "narrow_assignment.wave",
            "fun main() { var wide: i64 = 1; var narrow: i32 = 0; narrow = wide; }\n",
            "assignment to `narrow`",
        ),
        (
            "unknown_function.wave",
            "fun main() { missing_function(); }\n",
            "call to unknown function `missing_function`",
        ),
        (
            "unknown_field.wave",
            "struct Point { x: i32; }\nfun read(p: Point) -> i32 { return p.missing; }\nfun main() {}\n",
            "struct `Point` has no field `missing`",
        ),
        (
            "unknown_method.wave",
            "struct Point { x: i32; }\nfun main() { var p: Point = Point { x: 1 }; p.missing(); }\n",
            "struct `Point` has no method `missing`",
        ),
        (
            "unknown_struct_literal_field.wave",
            "struct Point { x: i32; }\nfun make() -> Point { return Point { missing: 1 }; }\nfun main() {}\n",
            "struct `Point` has no field `missing`",
        ),
        (
            "missing_struct_literal_field.wave",
            "struct Point { x: i32; y: i32; }\nfun main() { var p: Point = Point { x: 1 }; }\n",
            "struct literal `Point` is missing field(s): y",
        ),
        (
            "array_return.wave",
            "fun values() -> i32 { return [1, 2]; }\nfun main() {}\n",
            "found `array literal`",
        ),
        (
            "array_element.wave",
            "fun main() { var values: array<i32, 1> = [\"text\"]; }\n",
            "element 0 of initializer for `values`",
        ),
        (
            "invalid_condition.wave",
            "struct Flag { value: i32; }\nfun main() { var flag: Flag = Flag { value: 1 }; if (flag) {} }\n",
            "if condition must be bool, numeric, pointer, or string",
        ),
        (
            "invalid_match.wave",
            "struct Value { x: i32; }\nfun main() { var v: Value = Value { x: 1 }; match (v) { _ => {} } }\n",
            "match value must be an integer, enum, or variant",
        ),
        (
            "invalid_deref.wave",
            "fun main() { var value: i32 = 1; println(\"{}\", deref value); }\n",
            "deref expects a pointer",
        ),
        (
            "invalid_index_target.wave",
            "fun main() { var value: i32 = 1; println(\"{}\", value[0]); }\n",
            "index access requires an array or pointer",
        ),
        (
            "invalid_index_type.wave",
            "fun main() { var values: array<i32, 1> = [1]; println(\"{}\", values[\"text\"]); }\n",
            "index expression must be an integer",
        ),
        (
            "invalid_unary.wave",
            "fun main() { println(\"{}\", -\"text\"); }\n",
            "unary operator `Neg` is not supported for `str`",
        ),
        (
            "invalid_increment.wave",
            "fun main() { var flag: bool = true; flag++; }\n",
            "++/-- requires a numeric or pointer lvalue",
        ),
        (
            "invalid_export.wave",
            "export(rust) fun exposed() -> i32 { return 1; }\nfun main() {}\n",
            "unsupported export ABI 'rust'",
        ),
    ];

    for (file_name, source, expected) in cases {
        let source = write_wave(&dir, file_name, source);
        for mode in ["check", "build"] {
            let output = if mode == "check" {
                run_wavec_raw([OsStr::new("check"), source.as_os_str()])
            } else {
                run_wavec_raw([
                    OsStr::new("build"),
                    source.as_os_str(),
                    OsStr::new("--emit=obj"),
                    OsStr::new("--out-dir"),
                    dir.as_os_str(),
                ])
            };
            assert!(
                !output.status.success(),
                "{} unexpectedly succeeded in {} mode",
                file_name,
                mode
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains("error[E3001]"), "{}: {}", file_name, stderr);
            assert!(stderr.contains(expected), "{}: {}", file_name, stderr);
            assert!(
                !stderr.contains("E9001")
                    && !stderr.contains("compiler internal error")
                    && !stderr.contains("panic location"),
                "{} leaked a backend failure in {} mode: {}",
                file_name,
                mode,
                stderr
            );
        }
    }
}

#[test]
fn semantic_validation_rejects_mutation_in_conditions() {
    let dir = temp_case_dir("condition-mutation");
    let cases = [
        (
            "indexed_assignment_in_if.wave",
            "fun main() { var command: array<char, 2> = ['x', 'y']; if (command[0] = 'h') {} }\n",
            "assignment `=` is not allowed in if condition",
            "use `==` for comparison, or move the assignment before the condition",
        ),
        (
            "assignment_in_else_if.wave",
            "fun main() { var value: i32 = 0; if (false) {} else if (value = 1) {} }\n",
            "assignment `=` is not allowed in else-if condition",
            "use `==` for comparison, or move the assignment before the condition",
        ),
        (
            "compound_assignment_in_while.wave",
            "fun main() { var value: i32 = 0; while (value += 1) {} }\n",
            "compound assignment `+=` is not allowed in while condition",
            "move the mutation before the condition",
        ),
        (
            "assignment_in_for_condition.wave",
            "fun main() { for (var value: i32 = 0; value = 1; value += 1) {} }\n",
            "assignment `=` is not allowed in for condition",
            "use `==` for comparison, or move the assignment before the condition",
        ),
        (
            "nested_assignment_in_if.wave",
            "fun main() { var value: i32 = 0; if ((value = 1) == 1) {} }\n",
            "assignment `=` is not allowed in if condition",
            "use `==` for comparison, or move the assignment before the condition",
        ),
        (
            "increment_in_if.wave",
            "fun main() { var value: i32 = 0; if (value++) {} }\n",
            "increment or decrement `++` is not allowed in if condition",
            "move the mutation before the condition",
        ),
    ];

    for (file_name, source, expected, help) in cases {
        let source = write_wave(&dir, file_name, source);
        for mode in ["check", "build"] {
            let output = if mode == "check" {
                run_wavec_raw([OsStr::new("check"), source.as_os_str()])
            } else {
                run_wavec_raw([
                    OsStr::new("build"),
                    source.as_os_str(),
                    OsStr::new("--emit=obj"),
                    OsStr::new("--out-dir"),
                    dir.as_os_str(),
                ])
            };
            assert!(
                !output.status.success(),
                "{} unexpectedly accepted mutation in a condition in {} mode",
                file_name,
                mode
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains("error[E3001]"), "{}: {}", file_name, stderr);
            assert!(stderr.contains(expected), "{}: {}", file_name, stderr);
            assert!(stderr.contains(help), "{}: {}", file_name, stderr);
            assert!(
                !stderr.contains("E9001") && !stderr.contains("compiler internal error"),
                "{} ({}) leaked a backend failure: {}",
                file_name,
                mode,
                stderr
            );
        }
    }
}

#[test]
fn semantic_validation_allows_mutation_outside_conditions() {
    let dir = temp_case_dir("condition-mutation-valid");
    let source = write_wave(
        &dir,
        "valid.wave",
        r#"
fun main() {
    var value: i32 = 0;
    value = 1;
    if (value == 1) {}

    while (value < 2) {
        value += 1;
    }

    for (var index: i32 = 0; index < 2; index += 1) {}
}
"#,
    );

    run_wavec([OsStr::new("check"), source.as_os_str()]);
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]);
}

#[test]
fn semantic_validation_preserves_pointer_depth_for_address_of() {
    let dir = temp_case_dir("address-of-pointer-depth");
    let valid = write_wave(
        &dir,
        "valid.wave",
        r#"
fun main() {
    var message: str = "Hello";
    var correct: ptr<str> = &message;
    var copied: ptr<str> = correct;
    var converted: ptr<i8> = &message as ptr<i8>;
}
"#,
    );

    run_wavec([OsStr::new("check"), valid.as_os_str()]);
    run_wavec([
        OsStr::new("build"),
        valid.as_os_str(),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]);

    let invalid_cases = [
        (
            "string_depth.wave",
            r#"
fun main() {
    var message: str = "Hello";
    var wrong: ptr<i8> = &message;
}
"#,
            "type mismatch in initializer for `wrong`: expected `ptr<i8>`, found `ptr<str>`",
        ),
        (
            "pointer_pointee.wave",
            r#"
fun consume(value: ptr<u8>) {}

fun main() {
    var number: i32 = 1;
    consume(&number);
}
"#,
            "type mismatch in argument 1 of function `consume`: expected `ptr<u8>`, found `ptr<i32>`",
        ),
    ];

    for (file_name, source, expected) in invalid_cases {
        let source = write_wave(&dir, file_name, source);
        for mode in ["check", "build"] {
            let output = if mode == "check" {
                run_wavec_raw([OsStr::new("check"), source.as_os_str()])
            } else {
                run_wavec_raw([
                    OsStr::new("build"),
                    source.as_os_str(),
                    OsStr::new("--emit=obj"),
                    OsStr::new("--out-dir"),
                    dir.as_os_str(),
                ])
            };
            assert!(
                !output.status.success(),
                "{} unexpectedly accepted an implicit pointer conversion in {} mode",
                file_name,
                mode
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains("error[E3001]"), "{}: {}", file_name, stderr);
            assert!(stderr.contains(expected), "{}: {}", file_name, stderr);
            assert!(
                !stderr.contains("E9001")
                    && !stderr.contains("compiler internal error")
                    && !stderr.contains("panic location"),
                "{} leaked a backend failure in {} mode: {}",
                file_name,
                mode,
                stderr
            );
        }
    }
}

#[test]
fn second_semantic_audit_rejects_unsafe_programs_before_codegen() {
    let dir = temp_case_dir("semantic-audit-two");
    let cases = [
        (
            "array_format.wave",
            "fun main() { println(\"{}\", [1, 2]); }\n",
            "format argument must be a scalar",
        ),
        (
            "void_value.wave",
            "fun noop() {}\nfun main() { println(\"{}\", noop()); }\n",
            "found `void`",
        ),
        (
            "struct_format.wave",
            "struct Pair { x: i32; }\nfun main() { var p: Pair = Pair { x: 1 }; println(\"{}\", p); }\n",
            "found `Pair`",
        ),
        (
            "compound_string.wave",
            "fun main() { var text: str = \"a\"; text += \"b\"; }\n",
            "compound assignment `AddAssign` requires numeric operands",
        ),
        (
            "compound_bool.wave",
            "fun main() { var flag: bool = true; flag += false; }\n",
            "compound assignment `AddAssign` requires numeric operands",
        ),
        (
            "input_literal.wave",
            "fun main() { input(\"{}\", 1); }\n",
            "input argument must be a mutable lvalue",
        ),
        (
            "invalid_struct_cast.wave",
            "struct Pair { x: i32; }\nfun main() { var p: Pair = Pair { x: 1 }; var bits: i32 = p as i32; }\n",
            "invalid cast from `Pair` to `i32`",
        ),
        (
            "invalid_string_float_cast.wave",
            "fun main() { var value: f32 = \"text\" as f32; }\n",
            "invalid cast from `str` to `f32`",
        ),
        (
            "invalid_void_cast.wave",
            "fun noop() {}\nfun main() { var value: i32 = noop() as i32; }\n",
            "invalid cast from `void` to `i32`",
        ),
        (
            "unknown_cast_type.wave",
            "fun main() { var value: i32 = 1 as Missing; }\n",
            "unknown type `Missing` in cast target",
        ),
        (
            "unknown_variable_type.wave",
            "fun main() { var value: Missing; }\n",
            "unknown type `Missing` in variable `value`",
        ),
        (
            "unknown_return_type.wave",
            "fun make() -> Missing { var value: Missing; return value; }\nfun main() {}\n",
            "unknown type `Missing` in return type of function `make`",
        ),
        (
            "unknown_struct_field_type.wave",
            "struct Holder { value: Missing; }\nfun main() {}\n",
            "unknown type `Missing` in field `Holder.value`",
        ),
        (
            "unknown_alias_target.wave",
            "type Alias = Missing;\nfun main() {}\n",
            "unknown type `Missing` in type alias `Alias`",
        ),
        (
            "unknown_proto_target.wave",
            "proto Missing { fun read(self: Missing) -> i32 { return 1; } }\nfun main() {}\n",
            "proto implementation targets unknown type `Missing`",
        ),
        (
            "void_variable.wave",
            "fun main() { var value: void; }\n",
            "variable `value` cannot use the `void` type",
        ),
        (
            "void_struct_field.wave",
            "struct Invalid { value: void; }\nfun main() {}\n",
            "field `Invalid.value` cannot use the `void` type",
        ),
        (
            "cyclic_pointer_alias.wave",
            "type Loop = ptr<Loop>;\nfun main() {}\n",
            "cyclic type alias involving `Loop`",
        ),
        (
            "float_enum_repr.wave",
            "enum Invalid -> f32 { Value = 1 }\nfun main() {}\n",
            "enum `Invalid` representation must be an integer type",
        ),
        (
            "out_of_range_literal.wave",
            "fun main() { var value: i8 = 300; }\n",
            "initializer for `value`",
        ),
        (
            "negative_out_of_range_literal.wave",
            "fun main() { var value: i8 = -129; }\n",
            "initializer for `value`",
        ),
        (
            "negative_unsigned_literal.wave",
            "fun main() { var value: u8 = -1; }\n",
            "initializer for `value`",
        ),
        (
            "wrong_addressed_array_element.wave",
            "fun values() -> ptr<array<i32, 1>> { return &[\"text\"]; }\nfun main() {}\n",
            "element 0 of return value of function `values`",
        ),
        (
            "duplicate_local.wave",
            "fun main() { var value: i32 = 1; var value: i32 = 2; }\n",
            "duplicate variable declaration `value` in the same scope",
        ),
        (
            "duplicate_struct_field.wave",
            "struct Pair { x: i32; x: i64; }\nfun main() {}\n",
            "duplicate field `x` in struct `Pair`",
        ),
        (
            "duplicate_enum_variant.wave",
            "enum Mode -> i32 { Same = 1, Same = 2 }\nfun main() {}\n",
            "duplicate variant `Same` in enum `Mode`",
        ),
        (
            "duplicate_method.wave",
            "struct Pair { x: i32; }\nproto Pair { fun read(self: Pair) -> i32 { return self.x; } fun read(self: Pair) -> i32 { return self.x; } }\nfun main() {}\n",
            "duplicate method `Pair.read`",
        ),
        (
            "duplicate_match_value.wave",
            "enum Mode -> i32 { First = 1, Second = 1 }\nfun main() { var mode: Mode = First; match (mode) { First => {} Second => {} } }\n",
            "duplicate match case pattern `value:1`",
        ),
        (
            "mixed_float_widths.wave",
            "fun add(left: f32, right: f64) -> f32 { return left + right; }\nfun main() {}\n",
            "mixed float widths require an explicit cast",
        ),
    ];

    for (file_name, source, expected) in cases {
        let source = write_wave(&dir, file_name, source);
        for mode in ["check", "build"] {
            let output = if mode == "check" {
                run_wavec_raw([OsStr::new("check"), source.as_os_str()])
            } else {
                run_wavec_raw([
                    OsStr::new("build"),
                    source.as_os_str(),
                    OsStr::new("--emit=obj"),
                    OsStr::new("--out-dir"),
                    dir.as_os_str(),
                ])
            };
            assert!(
                !output.status.success(),
                "{} unexpectedly succeeded in {} mode",
                file_name,
                mode
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains("error[E3001]"), "{}: {}", file_name, stderr);
            assert!(stderr.contains(expected), "{}: {}", file_name, stderr);
            assert!(
                !stderr.contains("E9001")
                    && !stderr.contains("compiler internal error")
                    && !stderr.contains("panic location"),
                "{} leaked a backend failure in {} mode: {}",
                file_name,
                mode,
                stderr
            );
        }
    }
}

#[test]
fn second_semantic_audit_preserves_explicit_and_contextual_valid_cases() {
    let dir = temp_case_dir("semantic-audit-two-valid");
    let source = write_wave(
        &dir,
        "valid.wave",
        r#"
enum Mode -> i32 {
    First = 1,
    Alias = 1
}

fun noop() {}

fun add(left: f32, right: f64) -> f32 {
    return left + (right as f32);
}

fun main() {
    noop();
    var minimum: i8 = -128;
    var bit_pattern: i8 = 0xFF;
    var unsigned_max: u128 = 340282366920938463463374607431768211455;
    var explicit: i8 = 300 as i8;
    var values: ptr<array<i32, 2>> = &[1, 2];
    var input_value: i32 = 0;
    if (true) {
        var minimum: i32 = 1;
        println("{}", minimum);
    }
    println("{} {} {} {} {}", minimum, bit_pattern, unsigned_max, explicit, values);
}
"#,
    );

    run_wavec([OsStr::new("check"), source.as_os_str()]);
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]);
}

#[test]
fn semantic_diagnostics_point_at_the_relevant_source() {
    let dir = temp_case_dir("semantic-source-position");
    let source = write_wave(
        &dir,
        "wrong_return.wave",
        r#"
fun value() -> i32 {
    return "text";
}
"#,
    );
    let output = run_wavec_raw([OsStr::new("check"), source.as_os_str()]);
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("wrong_return.wave:3:12"), "{}", stderr);
    assert!(stderr.contains("return \"text\";"), "{}", stderr);

    let repeated_return = write_wave(
        &dir,
        "repeated_return.wave",
        r#"
fun value(flag: bool) -> i32 {
    if (flag) {
        return 1;
    }
    return "text";
}
"#,
    );
    let output = run_wavec_raw([OsStr::new("check"), repeated_return.as_os_str()]);
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("repeated_return.wave:6:12"), "{}", stderr);
    assert!(stderr.contains("return \"text\";"), "{}", stderr);

    let duplicate = write_wave(
        &dir,
        "duplicate.wave",
        r#"
fun other() {
    var value: i32 = 0;
}

fun main() {
    var value: i32 = 1;
    var value: i32 = 2;
}
"#,
    );
    let output = run_wavec_raw([OsStr::new("check"), duplicate.as_os_str()]);
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("duplicate.wave:8:9"), "{}", stderr);
    assert!(stderr.contains("var value: i32 = 2;"), "{}", stderr);

    let duplicate_type = write_wave(
        &dir,
        "duplicate_type.wave",
        "struct Item { first: i32; }\nstruct Item { second: i32; }\nfun main() {}\n",
    );
    let output = run_wavec_raw([OsStr::new("check"), duplicate_type.as_os_str()]);
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("duplicate_type.wave:2:8"), "{}", stderr);
    assert!(
        stderr.contains("struct Item { second: i32; }"),
        "{}",
        stderr
    );

    let imported = dir.join("broken.wave");
    fs::write(
        &imported,
        "fun broken() {\n    var value: Missing = 1;\n}\n",
    )
    .unwrap();
    let entry = write_wave(
        &dir,
        "import_main.wave",
        "import(\"./broken\");\n\nfun main() {}\n",
    );
    let output = run_wavec_raw([OsStr::new("check"), entry.as_os_str()]);
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("broken.wave:2:9"), "{}", stderr);
    assert!(stderr.contains("var value: Missing = 1;"), "{}", stderr);
    assert!(!stderr.contains("import_main.wave:1:1"), "{}", stderr);

    let generic_import = dir.join("generic_broken.wave");
    fs::write(
        &generic_import,
        "struct Box<T> { value: T; }\nfun broken() {\n    var value: Box<i32, i64>;\n}\n",
    )
    .unwrap();
    let generic_entry = write_wave(
        &dir,
        "generic_import_main.wave",
        "import(\"./generic_broken\");\n\nfun main() {}\n",
    );
    let output = run_wavec_raw([OsStr::new("check"), generic_entry.as_os_str()]);
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("generic_broken.wave:3:9"), "{}", stderr);
    assert!(
        stderr.contains("type `Box` expects 1 generic argument(s), found 2"),
        "{}",
        stderr
    );
    assert!(
        !stderr.contains("generic monomorphization failed"),
        "{}",
        stderr
    );
}

#[test]
fn semantic_validation_accepts_complete_returns_and_explicit_casts() {
    let dir = temp_case_dir("semantic-validation-valid");
    let source = write_wave(
        &dir,
        "valid.wave",
        r#"
struct Pair {
    x: i32;
    y: i32;
}

fun choose(flag: bool) -> i32 {
    if (flag) {
        return 1;
    } else {
        return 2;
    }
    println("unreachable");
}

fun dead_after_return() {
    return;
    println("unreachable");
}

fun widen(value: i32) -> i64 {
    return value;
}

fun narrow_explicitly(value: i64) -> i32 {
    return value as i32;
}

fun values() -> array<i32, 2> {
    return [1, 2];
}

fun pointer_bits() -> i64 {
    return "text" as i64;
}

fun infinite() -> i32 {
    while (true) {
        continue;
    }
}

fun main() -> i32 {
    var value: i32 = choose(true);
    var bits: i64 = pointer_bits();
    var pair: Pair = Pair { x: 1, y: 2 };
    var items: array<i32, 2> = values();
    var pointer: ptr<Pair> = &pair;
    if (pointer) {
        if (value == 1 && bits != 0 && items[0] == 1 && widen(pair.y) == 2) {
            return narrow_explicitly(0);
        }
    }
    return infinite();
}
"#,
    );
    let out_dir = dir.join("out");
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        out_dir.as_os_str(),
    ]);
}

#[test]
fn vex_cli_print_json_contracts_are_machine_readable() {
    let (stdout, stderr) = run_wavec_capture([
        OsStr::new("print"),
        OsStr::new("target-spec"),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-linux-gnu"),
        OsStr::new("--format=json"),
    ]);
    assert!(stderr.trim().is_empty(), "unexpected stderr:\n{}", stderr);
    let json = stdout.trim();
    assert!(json.starts_with('{') && json.ends_with('}'), "{}", json);
    assert!(
        json.contains("\"triple\":\"x86_64-unknown-linux-gnu\""),
        "{}",
        json
    );
    assert!(json.contains("\"arch\":\"x86_64\""), "{}", json);
    assert!(json.contains("\"os\":\"linux\""), "{}", json);
    assert!(json.contains("\"env\":\"gnu\""), "{}", json);
    assert!(json.contains("\"object_format\":\"elf\""), "{}", json);
    assert!(json.contains("\"default_linker\":"), "{}", json);

    let (stdout, _) = run_wavec_capture([
        OsStr::new("print"),
        OsStr::new("supported-emit-kinds"),
        OsStr::new("--format=json"),
    ]);
    let emit_json = stdout.trim();
    assert!(
        emit_json.contains("\"check\"")
            && emit_json.contains("\"ir\"")
            && emit_json.contains("\"obj\"")
            && emit_json.contains("\"bin\""),
        "{}",
        emit_json
    );

    let (stdout, _) = run_wavec_capture([
        OsStr::new("print"),
        OsStr::new("supported-input-types"),
        OsStr::new("--format=json"),
    ]);
    let input_json = stdout.trim();
    assert!(
        input_json.contains("\"wave\"")
            && input_json.contains("\"ir\"")
            && input_json.contains("\"bc\"")
            && input_json.contains("\"asm\"")
            && input_json.contains("\"obj\"")
            && input_json.contains("\"archive\""),
        "{}",
        input_json
    );

    let (stdout, _) = run_wavec_capture([
        OsStr::new("print"),
        OsStr::new("supported-print-items"),
        OsStr::new("--format=json"),
    ]);
    let print_items = stdout.trim();
    assert!(
        print_items.contains("\"cpu-list\"")
            && print_items.contains("\"target-features\"")
            && print_items.contains("\"default-linker\"")
            && print_items.contains("\"supported-input-types\"")
            && print_items.contains("\"supported-emit-kinds\""),
        "{}",
        print_items
    );

    let (stdout, _) = run_wavec_capture([
        OsStr::new("print"),
        OsStr::new("cpu-list"),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-linux-gnu"),
        OsStr::new("--format=json"),
    ]);
    assert!(stdout.trim().starts_with('['), "{}", stdout);

    let (stdout, _) = run_wavec_capture([
        OsStr::new("print"),
        OsStr::new("target-features"),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-linux-gnu"),
        OsStr::new("--format=json"),
    ]);
    assert!(stdout.contains("\"sse2\""), "{}", stdout);
}

#[test]
#[cfg(feature = "llvm-target-wasm")]
fn webassembly_targets_plan_modules_with_their_pointer_width() {
    let dir = temp_case_dir("wasm32-module-plan");
    let source = write_wave(
        &dir,
        "module.wave",
        r#"
fun add(a: i32, b: i32) -> i32 {
    return a + b;
}

fun main() -> i32 {
    return add(20, 22);
}
"#,
    );
    let out_dir = dir.join("out");
    let (plan, stderr) = run_wavec_capture([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm32-unknown-unknown"),
        OsStr::new("--dry-run"),
        OsStr::new("--error-format=json"),
        OsStr::new("--out-dir"),
        out_dir.as_os_str(),
    ]);
    assert!(stderr.trim().is_empty(), "unexpected stderr:\n{stderr}");
    assert!(
        plan.contains("\"target\":\"wasm32-unknown-unknown\""),
        "{plan}"
    );
    assert!(
        plan.contains("\"program\":") && plan.contains("wasm-ld"),
        "{plan}"
    );
    assert!(plan.contains("--no-entry"), "{plan}");
    assert!(plan.contains("--allow-undefined"), "{plan}");
    assert!(plan.contains("--export-if-defined=main"), "{plan}");
    assert!(plan.contains("--export-memory"), "{plan}");
    assert!(
        plan.contains(&json_string_for_test(
            &out_dir.join("module.wasm").to_string_lossy()
        )),
        "{plan}"
    );

    let (run_plan, stderr) = run_wavec_capture([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm32-unknown-unknown"),
        OsStr::new("--run"),
        OsStr::new("--dry-run"),
        OsStr::new("--error-format=json"),
    ]);
    assert!(stderr.trim().is_empty(), "{stderr}");
    assert!(run_plan.contains("\"program\":\"node\""), "{run_plan}");
    assert!(run_plan.contains("--input-type=module"), "{run_plan}");

    let wasm64_out = dir.join("wasm64-out");
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm64-unknown-unknown"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        wasm64_out.as_os_str(),
    ]);
    let wasm64_ir = fs::read_to_string(wasm64_out.join("module.ll")).unwrap();
    assert!(
        wasm64_ir.contains("target triple = \"wasm64-unknown-unknown\"")
            && wasm64_ir.contains("p:64:64"),
        "{wasm64_ir}"
    );

    let (wasm64_plan, stderr) = run_wavec_capture([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm64-unknown-unknown"),
        OsStr::new("--run"),
        OsStr::new("--dry-run"),
        OsStr::new("--error-format=json"),
    ]);
    assert!(stderr.trim().is_empty(), "{stderr}");
    assert!(wasm64_plan.contains("-mwasm64"), "{wasm64_plan}");
    assert!(
        wasm64_plan.contains("--experimental-wasm-memory64"),
        "{wasm64_plan}"
    );

    let asm_source = write_wave(
        &dir,
        "inline_asm.wave",
        "fun main() {\n    asm {\n        \"nop\"\n    }\n}\n",
    );
    let asm_error = run_wavec_expect_failure([
        OsStr::new("build"),
        asm_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm32-unknown-unknown"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        out_dir.as_os_str(),
    ]);
    assert!(
        asm_error.contains("inline assembly is not supported for webassembly wasm32 unknown"),
        "{asm_error}"
    );
}

#[test]
#[cfg(feature = "llvm-target-wasm")]
fn webassembly_c_abi_and_wasi_import_contracts_are_explicit() {
    let dir = temp_case_dir("wasm32-abi-contracts");
    let host_source = write_wave(
        &dir,
        "host.wave",
        r#"
struct Pair {
    x: i32;
    y: i32;
}

extern(c, "host_transform") fun transform(value: Pair) -> Pair;

export(c, "wave_transform") fun apply(value: Pair) -> Pair {
    return transform(value);
}

fun main() -> i32 { return 0; }
"#,
    );
    let host_out = dir.join("host-out");
    run_wavec([
        OsStr::new("build"),
        host_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm32-unknown-unknown"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        host_out.as_os_str(),
    ]);
    let host_ir = fs::read_to_string(host_out.join("host.ll")).unwrap();
    assert!(
        host_ir.contains("ptr sret(%Pair) align 4") && host_ir.contains("ptr byval(%Pair) align 4"),
        "{host_ir}"
    );
    assert!(
        host_ir.contains("\"wasm-import-module\"=\"env\"")
            && host_ir.contains("\"wasm-import-name\"=\"host_transform\""),
        "{host_ir}"
    );
    assert!(
        host_ir.contains("\"wasm-export-name\"=\"wave_transform\""),
        "{host_ir}"
    );

    let host64_out = dir.join("host64-out");
    run_wavec([
        OsStr::new("build"),
        host_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm64-unknown-unknown"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        host64_out.as_os_str(),
    ]);
    let host64_ir = fs::read_to_string(host64_out.join("host.ll")).unwrap();
    assert!(
        host64_ir.contains("target triple = \"wasm64-unknown-unknown\"")
            && host64_ir.contains("p:64:64")
            && host64_ir.contains("ptr sret(%Pair) align 4")
            && host64_ir.contains("ptr byval(%Pair) align 4")
            && host64_ir.contains("\"wasm-import-module\"=\"env\""),
        "{host64_ir}"
    );

    let wasi_source = write_wave(
        &dir,
        "wasi.wave",
        r#"
extern(c, "fd_write") fun fd_write(fd: u32, vectors: ptr<u8>, count: u32, written: ptr<u32>) -> u16;
fun main() -> i32 { return 0; }
"#,
    );
    let wasi_out = dir.join("wasi-out");
    run_wavec([
        OsStr::new("build"),
        wasi_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm32-wasip1"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        wasi_out.as_os_str(),
    ]);
    let wasi_ir = fs::read_to_string(wasi_out.join("wasi.ll")).unwrap();
    assert!(
        wasi_ir.contains("target triple = \"wasm32-wasip1\""),
        "{wasi_ir}"
    );
    assert!(
        wasi_ir.contains("\"wasm-import-module\"=\"wasi_snapshot_preview1\"")
            && wasi_ir.contains("\"wasm-import-name\"=\"fd_write\""),
        "{wasi_ir}"
    );
    assert!(wasi_ir.contains("define void @_start()"), "{wasi_ir}");

    let (spec, stderr) = run_wavec_capture([
        OsStr::new("print"),
        OsStr::new("target-spec"),
        OsStr::new("--target"),
        OsStr::new("wasm32-wasip1"),
        OsStr::new("--format=json"),
    ]);
    assert!(stderr.trim().is_empty(), "{stderr}");
    assert!(spec.contains("\"os\":\"wasi\""), "{spec}");
    assert!(spec.contains("\"env\":\"p1\""), "{spec}");
    assert!(spec.contains("\"hosted\":true"), "{spec}");

    let (run_plan, stderr) = run_wavec_capture([
        OsStr::new("build"),
        wasi_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm32-wasip1"),
        OsStr::new("--run"),
        OsStr::new("--dry-run"),
        OsStr::new("--error-format=json"),
    ]);
    assert!(stderr.trim().is_empty(), "{stderr}");
    assert!(run_plan.contains("\"program\":\"node\""), "{run_plan}");
    assert!(run_plan.contains("node:wasi"), "{run_plan}");
}

#[test]
fn vex_cli_json_errors_and_dry_run_are_stable() {
    let bad = run_wavec_raw([
        OsStr::new("--error-format=json"),
        OsStr::new("build"),
        OsStr::new("--bad-option"),
    ]);
    assert!(
        !bad.status.success(),
        "invalid CLI should fail\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&bad.stdout),
        String::from_utf8_lossy(&bad.stderr)
    );
    assert_eq!(bad.status.code(), Some(2));
    let stderr = String::from_utf8_lossy(&bad.stderr);
    assert!(stderr.contains("\"error\""), "{}", stderr);
    assert!(stderr.contains("\"kind\":\"usage\""), "{}", stderr);
    assert!(stderr.contains("\"exit_code\":2"), "{}", stderr);

    let dir = temp_case_dir("vex-dry-run-json");
    let src = write_wave(
        &dir,
        "main.wave",
        r#"
fun main() -> i32 {
    return 0;
}
"#,
    );
    let out_dir = dir.join("out");
    let (stdout, stderr) = run_wavec_capture([
        OsStr::new("--error-format=json"),
        OsStr::new("build"),
        src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-none-elf"),
        OsStr::new("--freestanding"),
        OsStr::new("--emit=obj"),
        OsStr::new("--dry-run"),
        OsStr::new("--out-dir"),
        out_dir.as_os_str(),
    ]);
    assert!(stderr.trim().is_empty(), "unexpected stderr:\n{}", stderr);
    let plan = stdout.trim();
    assert!(plan.starts_with('{') && plan.ends_with('}'), "{}", plan);
    assert!(plan.contains("\"schema_version\":1"), "{}", plan);
    assert!(
        plan.contains("\"target\":\"x86_64-unknown-none-elf\""),
        "{}",
        plan
    );
    assert!(plan.contains("\"cpu\":\"generic\""), "{}", plan);
    assert!(plan.contains("\"features\":\"\""), "{}", plan);
    assert!(plan.contains("\"abi\":null"), "{}", plan);
    assert!(plan.contains("\"isa\":null"), "{}", plan);
    assert!(plan.contains("\"mode\":\"compile-only\""), "{}", plan);
    assert!(plan.contains("\"emit_kinds\":[\"obj\"]"), "{}", plan);
    assert!(plan.contains("\"control_mode\":null"), "{}", plan);
    assert!(plan.contains("\"freestanding\":true"), "{}", plan);
    assert!(plan.contains("\"compile\""), "{}", plan);
    assert!(plan.contains("\"link\":null"), "{}", plan);
    assert!(plan.contains("\"execute\":null"), "{}", plan);

    let syntax_src = write_wave(
        &dir,
        "syntax_error.wave",
        r#"
fun main( {
}
"#,
    );
    let syntax = run_wavec_raw([
        OsStr::new("build"),
        syntax_src.as_os_str(),
        OsStr::new("--emit=check"),
        OsStr::new("--error-format=json"),
    ]);
    assert!(
        !syntax.status.success(),
        "syntax error should fail\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&syntax.stdout),
        String::from_utf8_lossy(&syntax.stderr)
    );
    let stderr = String::from_utf8_lossy(&syntax.stderr);
    assert!(stderr.contains("\"error\""), "{}", stderr);
    assert!(stderr.contains("\"kind\":\"syntax-error\""), "{}", stderr);
    assert!(stderr.contains("\"line\":"), "{}", stderr);
    assert!(stderr.contains("\"column\":"), "{}", stderr);

    let ordered_format = run_wavec_raw([
        OsStr::new("--error-format=human"),
        OsStr::new("build"),
        OsStr::new("--bad-option"),
        OsStr::new("--error-format=json"),
    ]);
    assert_eq!(ordered_format.status.code(), Some(2));
    let stderr = String::from_utf8_lossy(&ordered_format.stderr);
    assert!(
        stderr.contains("\"kind\":\"usage\""),
        "later build-level json format must control parser errors:\n{}",
        stderr
    );
}

#[test]
fn vex_cli_mixed_inputs_and_mode_conflicts_are_stable() {
    let dir = temp_case_dir("vex-cli-mixed-inputs");
    let src = write_wave(
        &dir,
        "main.wave",
        r#"
fun main() -> i32 {
    return 0;
}
"#,
    );
    let obj = dir.join("native.o");
    let archive = dir.join("libnative.a");
    fs::write(&obj, b"not-a-real-object").unwrap();
    fs::write(&archive, b"not-a-real-archive").unwrap();

    let out_dir = dir.join("artifacts");
    let target_dir = dir.join("intermediate");
    let binary = dir.join("app");
    let (stdout, stderr) = run_wavec_capture([
        OsStr::new("--error-format=json"),
        OsStr::new("build"),
        src.as_os_str(),
        obj.as_os_str(),
        archive.as_os_str(),
        OsStr::new("--emit=obj,bin"),
        OsStr::new("--dry-run"),
        OsStr::new("--out-dir"),
        out_dir.as_os_str(),
        OsStr::new("--target-dir"),
        target_dir.as_os_str(),
        OsStr::new("-o"),
        binary.as_os_str(),
    ]);
    assert!(stderr.trim().is_empty(), "unexpected stderr:\n{}", stderr);
    let plan = stdout.trim();
    assert!(plan.contains("\"kind\":\"wave\""), "{}", plan);
    assert!(plan.contains("\"kind\":\"obj\""), "{}", plan);
    assert!(plan.contains("\"kind\":\"archive\""), "{}", plan);
    assert!(
        plan.contains("\"emit_kinds\":[\"obj\",\"bin\"]"),
        "{}",
        plan
    );
    assert!(
        plan.contains(&format!(
            "\"output\":{}",
            json_string_for_test(&binary.to_string_lossy())
        )),
        "-o must apply to final linked binary only:\n{}",
        plan
    );
    assert!(
        plan.contains(&format!(
            "\"output\":{}",
            json_string_for_test(&out_dir.join("main.o").to_string_lossy())
        )),
        "obj artifact must follow --out-dir, not -o:\n{}",
        plan
    );
    assert!(
        plan.contains(&json_string_for_test(&archive.to_string_lossy())),
        "archive input must be forwarded to the link plan:\n{}",
        plan
    );

    let run_plan = run_wavec_raw([
        OsStr::new("--error-format=json"),
        OsStr::new("build"),
        src.as_os_str(),
        OsStr::new("--run"),
        OsStr::new("--dry-run"),
        OsStr::new("--"),
        OsStr::new("arg one"),
        OsStr::new("--flag"),
    ]);
    assert!(
        run_plan.status.success(),
        "run dry-run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run_plan.stdout),
        String::from_utf8_lossy(&run_plan.stderr)
    );
    let stdout = String::from_utf8_lossy(&run_plan.stdout);
    assert!(stdout.contains("\"mode\":\"build+run\""), "{}", stdout);
    assert!(stdout.contains("\"execute\":{"), "{}", stdout);
    assert!(
        stdout.contains("\"args\":[\"arg one\",\"--flag\"]"),
        "{}",
        stdout
    );

    let cases: &[(&str, &[&str], &[&str])] = &[
        (
            "check cannot be combined",
            &["build", "main.wave", "--emit=check,ast"],
            &["--emit=check", "alone"],
        ),
        (
            "link-only obj emit conflict",
            &["build", "native.o", "--link-only", "--emit=obj"],
            &["--link-only", "--emit=bin"],
        ),
        (
            "shared static conflict",
            &["build", "main.wave", "--shared", "--static", "--dry-run"],
            &["--shared", "--static"],
        ),
        (
            "run shared conflict",
            &["build", "main.wave", "--shared", "--run", "--dry-run"],
            &["--run", "--shared"],
        ),
        (
            "no-pie relocation conflict",
            &[
                "-C",
                "relocation-model=pie",
                "build",
                "main.wave",
                "--no-pie",
                "--dry-run",
            ],
            &["--no-pie", "relocation-model=pie"],
        ),
        (
            "forced input conflict",
            &["build", "native.o", "--input-type=wave", "--dry-run"],
            &["--input-type=wave", "conflicts"],
        ),
        (
            "unknown input inference",
            &["build", "unknown.blob", "--dry-run"],
            &["cannot infer input type", "--input-type"],
        ),
    ];

    for (name, args, needles) in cases {
        let full_args = std::iter::once("--error-format=json")
            .chain(args.iter().copied())
            .map(OsStr::new);
        let out = run_wavec_raw(full_args);
        assert!(
            !out.status.success(),
            "{} unexpectedly succeeded\nstdout:\n{}\nstderr:\n{}",
            name,
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );
        assert_eq!(out.status.code(), Some(2), "{}", name);
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(
            stderr.contains("\"kind\":\"usage\""),
            "{}: {}",
            name,
            stderr
        );
        for needle in *needles {
            assert!(
                stderr.contains(needle),
                "{} missing '{}': {}",
                name,
                needle,
                stderr
            );
        }
    }
}

#[test]
fn target_attribute_supports_arch_os_env_and_abi_conditions() {
    let dir = temp_case_dir("target-attr-conditions");
    let src = write_wave(
        &dir,
        "select.wave",
        r#"
#[target(arch="x86_64", os="none", env="none")]
fun selected() -> i32 {
    return 64;
}

#[target(arch="aarch64", os="none", env="none")]
fun selected() -> i32 {
    return 128;
}

#[target(arch="riscv64", os="none", env="none", abi="lp64d")]
fun selected() -> i32 {
    return 255;
}

fun main() -> i32 {
    return selected();
}
"#,
    );

    let x86_dir = dir.join("x86");
    run_wavec([
        OsStr::new("build"),
        src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-none-elf"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        x86_dir.as_os_str(),
    ]);
    let x86_ir = fs::read_to_string(x86_dir.join("select.ll")).unwrap();
    assert!(x86_ir.contains("ret i32 64"), "{}", x86_ir);
    assert!(!x86_ir.contains("ret i32 128"), "{}", x86_ir);

    let arm_dir = dir.join("arm");
    run_wavec([
        OsStr::new("build"),
        src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("aarch64-unknown-none-elf"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        arm_dir.as_os_str(),
    ]);
    let arm_ir = fs::read_to_string(arm_dir.join("select.ll")).unwrap();
    assert!(arm_ir.contains("ret i32 128"), "{}", arm_ir);
    assert!(!arm_ir.contains("ret i32 64"), "{}", arm_ir);

    let riscv_dir = dir.join("riscv");
    run_wavec([
        OsStr::new("build"),
        src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-none-elf"),
        OsStr::new("--abi"),
        OsStr::new("lp64d"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        riscv_dir.as_os_str(),
    ]);
    let riscv_ir = fs::read_to_string(riscv_dir.join("select.ll")).unwrap();
    assert!(riscv_ir.contains("ret i32 255"), "{}", riscv_ir);
}

#[test]
fn target_configuration_is_rejected_before_frontend_or_backend_work() {
    let dir = temp_case_dir("target-validation");
    let source = write_wave(&dir, "main.wave", "fun main() -> i32 { return 0; }\n");

    let run_failure = |options: &[&str], expected: &str| {
        let mut args = vec![
            OsString::from("--error-format=json"),
            OsString::from("build"),
            source.as_os_str().to_os_string(),
        ];
        args.extend(options.iter().map(OsString::from));
        let output = run_wavec_raw(args);
        assert_eq!(
            output.status.code(),
            Some(2),
            "target validation should fail with usage exit code 2\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.trim().is_empty(),
            "JSON usage errors must not write to stdout: {stdout}"
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("\"kind\":\"usage\""), "{}", stderr);
        assert!(stderr.contains(expected), "{}", stderr);
        assert!(
            !stderr.contains("E9001")
                && !stderr.contains("compiler internal error")
                && !stderr.contains("panic location"),
            "target validation leaked a backend failure: {}",
            stderr
        );
    };

    for mode in [
        &["--target", "mips64-unknown-linux-gnu", "--emit=check"][..],
        &["--target", "mips64-unknown-linux-gnu", "--emit=obj"][..],
        &[
            "--target",
            "mips64-unknown-linux-gnu",
            "--emit=obj",
            "--dry-run",
        ][..],
    ] {
        run_failure(mode, "unsupported target 'mips64-unknown-linux-gnu'");
    }

    for (options, expected) in [
        (
            &["--target", "x86_64-garbage-linux-gnu", "--emit=check"][..],
            "unsupported target 'x86_64-garbage-linux-gnu'",
        ),
        (
            &["--target", "riscv64-unknown-linux-musl", "--emit=check"][..],
            "unsupported target 'riscv64-unknown-linux-musl'",
        ),
        (
            &[
                "--target",
                "x86_64-unknown-linux-gnu",
                "--cpu",
                "sifive-u74",
                "--emit=check",
            ][..],
            "unsupported CPU 'sifive-u74'",
        ),
        (
            &[
                "--target",
                "riscv64-unknown-none-elf",
                "--cpu",
                "rocket",
                "--emit=obj",
            ][..],
            "unsupported CPU 'rocket'",
        ),
        (
            &[
                "--target",
                "x86_64-unknown-linux-gnu",
                "--features",
                "+m",
                "--emit=check",
            ][..],
            "unsupported feature 'm'",
        ),
        (
            &[
                "--target",
                "aarch64-unknown-linux-gnu",
                "--features",
                "+fp",
                "--emit=obj",
            ][..],
            "unsupported feature 'fp'",
        ),
        (
            &[
                "--target",
                "x86_64-unknown-linux-gnu",
                "--abi",
                "lp64d",
                "--emit=check",
            ][..],
            "unsupported ABI 'lp64d'",
        ),
        (
            &[
                "--target",
                "x86_64-unknown-linux-gnu",
                "--features",
                "sse2",
                "--emit=check",
            ][..],
            "invalid target feature 'sse2'",
        ),
        (
            &[
                "--target",
                "x86_64-unknown-linux-gnu",
                "--features",
                "+sse2,-sse2",
                "--emit=check",
            ][..],
            "target feature 'sse2' is specified more than once",
        ),
        (
            &[
                "--target",
                "x86_64-unknown-linux-gnu",
                "--features",
                "+sse2,,+avx",
                "--emit=check",
            ][..],
            "invalid empty target feature",
        ),
        (
            &[
                "--target",
                "riscv64-unknown-none-elf",
                "--features",
                "+m,+a,-f,+d,+c",
                "--abi",
                "lp64d",
                "--emit=check",
            ][..],
            "feature 'd' requires feature 'f'",
        ),
        (
            &[
                "--target",
                "riscv64-unknown-none-elf",
                "--features",
                "+f,-d",
                "--abi",
                "lp64d",
                "--emit=check",
            ][..],
            "ABI 'lp64d' for target 'riscv64-unknown-none-elf' requires features 'f' and 'd'",
        ),
        (
            &[
                "--target",
                "riscv64-unknown-none-elf",
                "--features",
                "-f",
                "--abi",
                "lp64f",
                "--emit=check",
            ][..],
            "ABI 'lp64f' for target 'riscv64-unknown-none-elf' requires feature 'f'",
        ),
        (
            &[
                "--target",
                "riscv64-unknown-linux-gnu",
                "--features",
                "-zicsr",
                "--emit=check",
            ][..],
            "feature 'f' requires feature 'zicsr'",
        ),
    ] {
        run_failure(options, expected);
    }

    let x86_out = dir.join("valid-x86");
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-linux-gnu"),
        OsStr::new("--cpu"),
        OsStr::new("x86-64-v2"),
        OsStr::new("--features"),
        OsStr::new("+sse2,-avx"),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        x86_out.as_os_str(),
    ]);
    assert!(x86_out.join("main.o").is_file());

    let riscv_out = dir.join("valid-riscv");
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-none-elf"),
        OsStr::new("--cpu"),
        OsStr::new("sifive-u74"),
        OsStr::new("--features"),
        OsStr::new("+m,+a,+f,+d,+c"),
        OsStr::new("--abi"),
        OsStr::new("lp64d"),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        riscv_out.as_os_str(),
    ]);
    assert!(riscv_out.join("main.o").is_file());

    let linux_riscv_out = dir.join("valid-linux-riscv");
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        linux_riscv_out.as_os_str(),
    ]);
    let linux_riscv_object = linux_riscv_out.join("main.o");
    assert!(linux_riscv_object.is_file());
    assert_eq!(riscv64_elf_flags(&linux_riscv_object) & 0x7, 0x5);
    let linux_riscv_bytes = fs::read(&linux_riscv_object).unwrap();
    assert!(bytes_contains(&linux_riscv_bytes, b"zicsr"));
    assert!(bytes_contains(&linux_riscv_bytes, b"zifencei"));

    let (stdout, stderr) = run_wavec_capture([
        OsStr::new("print"),
        OsStr::new("target-spec"),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-none-elf"),
        OsStr::new("--format=json"),
    ]);
    assert!(stderr.trim().is_empty(), "{}", stderr);
    assert!(stdout.contains("\"hosted\":false"), "{}", stdout);
    assert!(stdout.contains("\"freestanding\":true"), "{}", stdout);
    assert!(stdout.contains("\"cpu\":\"generic-rv64\""), "{}", stdout);
    assert!(
        stdout.contains("\"features\":\"+m,+a,-f,-d,+c,-zicsr,-zifencei\""),
        "{}",
        stdout
    );
    assert!(stdout.contains("\"abi\":\"lp64\""), "{}", stdout);
    assert!(stdout.contains("\"isa\":\"rv64imac\""), "{}", stdout);

    let (stdout, stderr) = run_wavec_capture([
        OsStr::new("print"),
        OsStr::new("target-spec"),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--format=json"),
    ]);
    assert!(stderr.trim().is_empty(), "{}", stderr);
    assert!(stdout.contains("\"hosted\":true"), "{}", stdout);
    assert!(stdout.contains("\"freestanding\":false"), "{}", stdout);
    assert!(
        stdout.contains("\"features\":\"+m,+a,+f,+d,+c,+zicsr,+zifencei\""),
        "{}",
        stdout
    );
    assert!(stdout.contains("\"abi\":\"lp64d\""), "{}", stdout);
    assert!(stdout.contains("\"isa\":\"rv64gc\""), "{}", stdout);

    let (stdout, stderr) = run_wavec_capture([
        OsStr::new("--error-format=json"),
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--emit=obj"),
        OsStr::new("--dry-run"),
    ]);
    assert!(stderr.trim().is_empty(), "{}", stderr);
    assert!(stdout.contains("\"cpu\":\"generic-rv64\""), "{}", stdout);
    assert!(
        stdout.contains("\"features\":\"+m,+a,+f,+d,+c,+zicsr,+zifencei\""),
        "{}",
        stdout
    );
    assert!(stdout.contains("\"abi\":\"lp64d\""), "{}", stdout);
    assert!(stdout.contains("\"isa\":\"rv64gc\""), "{}", stdout);

    let (stdout, stderr) = run_wavec_capture([
        OsStr::new("--error-format=json"),
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--abi=lp64"),
        OsStr::new("--emit=bin"),
        OsStr::new("--dry-run"),
    ]);
    assert!(stderr.trim().is_empty(), "{}", stderr);
    assert!(
        stdout.contains("--dynamic-linker=/lib/ld-linux-riscv64-lp64.so.1"),
        "{}",
        stdout
    );
    assert!(
        !stdout.contains("ld-linux-riscv64-lp64d.so.1"),
        "{}",
        stdout
    );
    assert!(
        json_contains_path_components(
            &stdout,
            &["crt", "riscv64-unknown-linux-gnu", "lp64", "crt1.o",],
        ),
        "LP64 link plan must select Wave's ABI-matched CRT:\n{}",
        stdout
    );
    for host_path in [
        "/usr/lib64/crt1.o",
        "/usr/lib64/crti.o",
        "/usr/lib64/crtn.o",
        "-L/usr/lib64",
        "-L/lib64",
        "-L/usr/lib ",
        "-L/lib ",
    ] {
        assert!(
            !stdout.contains(host_path),
            "cross-target link plan consumed host runtime path '{}':\n{}",
            host_path,
            stdout
        );
    }

    let (stdout, stderr) = run_wavec_capture([
        OsStr::new("--error-format=json"),
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--static"),
        OsStr::new("--emit=bin"),
        OsStr::new("--dry-run"),
    ]);
    assert!(stderr.trim().is_empty(), "{}", stderr);
    assert!(stdout.contains("-static"), "{}", stdout);
    assert!(
        !stdout.contains("--dynamic-linker="),
        "static link plan must not select a dynamic loader:\n{}",
        stdout
    );
    assert!(
        json_contains_path_components(
            &stdout,
            &["crt", "riscv64-unknown-linux-gnu", "lp64d", "crt1.o",],
        ),
        "static link plan must retain Wave's LP64D CRT entry point:\n{}",
        stdout
    );

    let (stdout, stderr) = run_wavec_capture([
        OsStr::new("--error-format=json"),
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--static"),
        OsStr::new("--pie"),
        OsStr::new("--emit=bin"),
        OsStr::new("--dry-run"),
    ]);
    assert!(stderr.trim().is_empty(), "{}", stderr);
    assert!(
        stdout.contains("rcrt1.o"),
        "static PIE link plan must use the relocatable CRT entry point:\n{}",
        stdout
    );
}

#[test]
fn freebsd_link_plans_use_target_sysroot_and_elf_loader() {
    let dir = temp_case_dir("freebsd-link-plans");
    let source = write_wave(&dir, "main.wave", "fun main() -> i32 { return 0; }\n");
    let sysroot = dir.join("sysroot");
    fs::create_dir_all(sysroot.join("usr/lib")).unwrap();
    for file in ["crt1.o", "Scrt1.o", "crti.o", "crtn.o"] {
        fs::write(sysroot.join("usr/lib").join(file), []).unwrap();
    }
    for (target, emulation) in [
        ("x86_64-unknown-freebsd", "elf_x86_64"),
        ("aarch64-unknown-freebsd", "aarch64elf"),
        ("riscv64-unknown-freebsd", "elf64lriscv"),
    ] {
        for (mode, start) in [("--no-pie", "crt1.o"), ("--pie", "Scrt1.o")] {
            let (plan, stderr) = run_wavec_capture([
                OsStr::new("--error-format=json"),
                OsStr::new("build"),
                source.as_os_str(),
                OsStr::new("--target"),
                OsStr::new(target),
                OsStr::new("--sysroot"),
                sysroot.as_os_str(),
                OsStr::new(mode),
                OsStr::new("--emit=bin"),
                OsStr::new("--dry-run"),
            ]);
            assert!(stderr.is_empty(), "{stderr}");
            assert!(plan.contains(&format!("\"-m\",\"{emulation}\"")), "{plan}");
            assert!(
                plan.contains("--dynamic-linker=/libexec/ld-elf.so.1"),
                "{plan}"
            );
            for file in [start, "crti.o", "crtn.o"] {
                assert!(
                    json_contains_path_components(&plan, &["sysroot", "usr", "lib", file]),
                    "{plan}"
                );
            }
            assert!(
                !plan.contains("ld-linux") && !plan.contains("unknown-linux-gnu"),
                "{plan}"
            );
        }
    }
}

#[test]
fn hosted_linux_link_plans_use_wave_crt_for_every_architecture_and_mode() {
    let dir = temp_case_dir("bundled-linux-crt-matrix");
    let source = write_wave(&dir, "main.wave", "fun main() -> i32 { return 0; }\n");

    for (target, abi) in [
        ("x86_64-unknown-linux-gnu", None),
        ("aarch64-unknown-linux-gnu", None),
        ("riscv64-unknown-linux-gnu", Some("lp64")),
        ("riscv64-unknown-linux-gnu", Some("lp64f")),
        ("riscv64-unknown-linux-gnu", Some("lp64d")),
        #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
        ("loongarch64-unknown-linux-gnu", Some("lp64s")),
        #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
        ("loongarch64-unknown-linux-gnu", Some("lp64d")),
    ] {
        for (options, object_name) in [
            (Vec::<&str>::new(), "crt1.o"),
            (vec!["--pie"], "Scrt1.o"),
            (vec!["--static", "--pie"], "rcrt1.o"),
        ] {
            let mut args = vec![
                OsString::from("--error-format=json"),
                OsString::from("build"),
                source.as_os_str().to_os_string(),
                OsString::from("--target"),
                OsString::from(target),
            ];
            if let Some(abi) = abi {
                args.push(OsString::from(format!("--abi={abi}")));
            }
            args.extend(options.into_iter().map(OsString::from));
            args.push(OsString::from("--emit=bin"));
            args.push(OsString::from("--dry-run"));

            let output = run_wavec_raw(args);
            assert!(
                output.status.success(),
                "{target} {object_name} dry-run failed:\nstdout:\n{}\nstderr:\n{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            );
            let stdout = String::from_utf8_lossy(&output.stdout);
            let mut components = vec!["crt", target];
            if let Some(abi) = abi {
                components.push(abi);
            }
            components.push(object_name);
            assert!(
                json_contains_path_components(&stdout, &components),
                "{target} must select Wave's {object_name}:\n{stdout}"
            );
            let mut crti_components = vec!["crt", target];
            let mut crtn_components = vec!["crt", target];
            if let Some(abi) = abi {
                crti_components.push(abi);
                crtn_components.push(abi);
            }
            crti_components.push("crti.o");
            crtn_components.push("crtn.o");
            assert!(
                json_contains_path_components(&stdout, &crti_components)
                    && json_contains_path_components(&stdout, &crtn_components),
                "{target} must select Wave's crti.o and crtn.o:\n{stdout}"
            );
        }
    }
}

#[test]
fn riscv64_debian_cross_prefix_does_not_double_apply_linker_sysroot() {
    let dir = temp_case_dir("riscv64-debian-cross-prefix");
    let source = write_wave(&dir, "main.wave", "fun main() -> i32 { return 0; }\n");
    let sysroot = dir.join("usr").join("riscv64-linux-gnu");
    let runtime = sysroot.join("lib");
    fs::create_dir_all(&runtime).unwrap();

    for crt in ["crt1.o", "crti.o", "crtn.o"] {
        write_minimal_elf64_object(&runtime.join(crt), 243);
    }
    let runtime_prefix = runtime.to_string_lossy();
    fs::write(
        runtime.join("libc.so"),
        format!(
            "GROUP ( {runtime_prefix}/libc.so.6 {runtime_prefix}/libc_nonshared.a \
             AS_NEEDED ( {runtime_prefix}/ld-linux-riscv64-lp64d.so.1 ) )\n"
        ),
    )
    .unwrap();
    fs::write(
        runtime.join("libm.so"),
        format!("GROUP ( {runtime_prefix}/libm.so.6 )\n"),
    )
    .unwrap();
    for runtime_file in [
        "libc.so.6",
        "libc_nonshared.a",
        "libm.so.6",
        "ld-linux-riscv64-lp64d.so.1",
    ] {
        fs::write(runtime.join(runtime_file), []).unwrap();
    }

    let (target_spec, stderr) = run_wavec_capture([
        OsStr::new("--sysroot"),
        sysroot.as_os_str(),
        OsStr::new("print"),
        OsStr::new("target-spec"),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--format=json"),
    ]);
    assert!(stderr.trim().is_empty(), "{}", stderr);
    assert!(
        target_spec.contains("\"sysroot_source\":\"explicit\""),
        "{}",
        target_spec
    );
    assert!(
        json_contains_path_value(&target_spec, &sysroot.to_string_lossy()),
        "{}",
        target_spec
    );

    let (stdout, stderr) = run_wavec_capture([
        OsStr::new("--error-format=json"),
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--sysroot"),
        sysroot.as_os_str(),
        OsStr::new("--emit=bin"),
        OsStr::new("--dry-run"),
    ]);
    assert!(stderr.trim().is_empty(), "{}", stderr);
    assert!(
        stdout.contains("\"sysroot_source\":\"explicit\""),
        "{}",
        stdout
    );
    assert!(stdout.contains("--sysroot=/"), "{}", stdout);
    assert!(
        !stdout.contains(&format!("--sysroot={}", sysroot.display())),
        "cross prefix must not be applied twice by ld.lld:\n{}",
        stdout
    );
    assert!(
        json_contains_path_value(&stdout, &format!("-L{}", runtime.display())),
        "target runtime search path must remain isolated to the cross prefix:\n{}",
        stdout
    );
}

#[test]
fn advertised_target_options_reach_object_codegen_without_backend_diagnostics() {
    let dir = temp_case_dir("target-option-matrix");
    let source = write_wave(&dir, "matrix.wave", "fun main() -> i32 { return 0; }\n");

    let build_object = |target: &str, label: &str, options: &[OsString]| -> PathBuf {
        let out_dir = dir.join(label);
        let mut args = vec![
            OsString::from("build"),
            source.as_os_str().to_os_string(),
            OsString::from("--target"),
            OsString::from(target),
        ];
        args.extend_from_slice(options);
        args.extend([
            OsString::from("--emit=obj"),
            OsString::from("--out-dir"),
            out_dir.as_os_str().to_os_string(),
        ]);

        let (stdout, stderr) = run_wavec_capture(args);
        assert!(stdout.trim().is_empty(), "{}", stdout);
        let only_nonstandard_lp64f_warnings = target.starts_with("loongarch64-")
            && !stderr.trim().is_empty()
            && stderr
                .lines()
                .all(|line| line == "warning: 'lp64f' has not been standardized");
        assert!(
            stderr.trim().is_empty() || only_nonstandard_lp64f_warnings,
            "advertised target option emitted a backend diagnostic for {target}: {stderr}"
        );
        let object = out_dir.join("matrix.o");
        assert!(object.is_file(), "{target} {label}");
        object
    };

    let (targets, stderr) = run_wavec_capture([OsStr::new("print"), OsStr::new("target-list")]);
    assert!(stderr.trim().is_empty(), "{}", stderr);

    for target in targets.lines().filter(|line| !line.is_empty()) {
        let target_label = target.replace('-', "_");
        let object = build_object(target, &format!("{target_label}_default"), &[]);

        let (target_spec, stderr) = run_wavec_capture([
            OsStr::new("print"),
            OsStr::new("target-spec"),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--format=json"),
        ]);
        assert!(stderr.trim().is_empty(), "{}", stderr);
        let object = fs::read(object).unwrap();
        if target_spec.contains("\"object_format\":\"elf\"") {
            assert!(object.starts_with(b"\x7fELF"), "{target}: {target_spec}");
        } else if target_spec.contains("\"object_format\":\"macho\"") {
            assert!(
                object.starts_with(&[0xcf, 0xfa, 0xed, 0xfe]),
                "{target}: {target_spec}"
            );
        } else if target_spec.contains("\"object_format\":\"coff\"") {
            let machine = if target_spec.contains("\"arch\":\"aarch64\"") {
                [0x64, 0xaa]
            } else {
                [0x64, 0x86]
            };
            assert!(object.starts_with(&machine), "{target}: {target_spec}");
        } else if target_spec.contains("\"object_format\":\"wasm\"") {
            assert!(object.starts_with(b"\0asm"), "{target}: {target_spec}");
        } else {
            panic!("target spec has an unknown object format: {target_spec}");
        }

        let (cpus, stderr) = run_wavec_capture([
            OsStr::new("print"),
            OsStr::new("cpu-list"),
            OsStr::new("--target"),
            OsStr::new(target),
        ]);
        assert!(stderr.trim().is_empty(), "{}", stderr);
        for cpu in cpus.lines().filter(|line| !line.is_empty()) {
            let cpu_label = cpu.replace('-', "_");
            build_object(
                target,
                &format!("{target_label}_cpu_{cpu_label}"),
                &[OsString::from("--cpu"), OsString::from(cpu)],
            );
        }

        let (features, stderr) = run_wavec_capture([
            OsStr::new("print"),
            OsStr::new("target-features"),
            OsStr::new("--target"),
            OsStr::new(target),
        ]);
        assert!(stderr.trim().is_empty(), "{}", stderr);
        for feature in features.lines().filter(|line| !line.is_empty()) {
            let feature_label = feature.replace(['-', '.'], "_");
            for (sign, action) in [("+", "enable"), ("-", "disable")] {
                let float_abi_target =
                    target.starts_with("riscv64-") || target.starts_with("loongarch64-");
                let setting = match (float_abi_target, feature, sign) {
                    (true, "f", "-") => "-f,-d".to_string(),
                    (true, "d", "+") => "+f,+d".to_string(),
                    _ if target.starts_with("riscv64-") && feature == "zicsr" && sign == "-" => {
                        "-f,-d,-zicsr".to_string()
                    }
                    _ => format!("{sign}{feature}"),
                };
                if target == "riscv64-unknown-freebsd"
                    && sign == "-"
                    && matches!(feature, "f" | "d" | "zicsr")
                {
                    let output = wavec_command()
                        .arg("build")
                        .arg(&source)
                        .args(["--target", target, "--features", &setting, "--emit=obj"])
                        .arg("--out-dir")
                        .arg(dir.join("rejected-freebsd-abi"))
                        .output()
                        .unwrap();
                    assert!(!output.status.success());
                    assert!(
                        String::from_utf8_lossy(&output.stderr).contains("supported ABIs: lp64d")
                    );
                    continue;
                }
                build_object(
                    target,
                    &format!("{target_label}_feature_{action}_{feature_label}"),
                    &[OsString::from("--features"), OsString::from(setting)],
                );
            }
        }
    }

    #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
    {
        for (abi, features, expected_flags) in [
            ("lp64", "+m,+a,-f,-d,+c", 0x1),
            ("lp64f", "+m,+a,+f,-d,+c", 0x3),
            ("lp64d", "+m,+a,+f,+d,+c", 0x5),
        ] {
            for target in ["riscv64-unknown-linux-gnu", "riscv64-unknown-none-elf"] {
                let object = build_object(
                    target,
                    &format!("{}_abi_{abi}", target.replace('-', "_")),
                    &[
                        OsString::from("--features"),
                        OsString::from(features),
                        OsString::from("--abi"),
                        OsString::from(abi),
                    ],
                );
                assert_eq!(riscv64_elf_flags(&object) & 0x7, expected_flags);
            }
        }
    }

    #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
    {
        for (abi, expected_flags) in [("lp64s", 0x1), ("lp64f", 0x2), ("lp64d", 0x3)] {
            let object = build_object(
                "loongarch64-unknown-linux-gnu",
                &format!("loongarch64_unknown_linux_gnu_abi_{abi}"),
                &[OsString::from("--abi"), OsString::from(abi)],
            );
            assert_eq!(loongarch64_elf_flags(&object) & 0x7, expected_flags);
        }
    }
}

#[test]
fn lvalue_store_updates_deref_index_and_struct_fields() {
    let dir = temp_case_dir("lvalue-store");
    let src = write_wave(
        &dir,
        "lvalue_store.wave",
        r#"
struct Pair {
    a: i32;
    b: i32;
}

struct PointerBox {
    data: ptr<i32>;
}

fun write_deref(p: ptr<i32>, v: i32) {
    deref p = v;
}

fun write_index(p: ptr<i32>, v: i32) {
    p[1] = v;
}

fun write_field(p: ptr<Pair>, v: i32) {
    p.b = v;
}

fun write_array_pointer(p: ptr<array<i32, 3>>, v: i32) {
    deref p[1] = v;
}

fun write_pointer_field(p: ptr<PointerBox>, value: ptr<i32>) {
    deref p.data = value;
}

fun id_ptr(p: ptr<i32>) -> ptr<i32> {
    return p;
}

fun main() -> i32 {
    var x: i32 = 1;
    write_deref(&x, 41);
    if (x != 41) {
        return 1;
    }

    var arr: array<i32, 3> = [1, 2, 3];
    write_index(&arr[0], 9);
    if (arr[1] != 9) {
        return 2;
    }

    var pair: Pair = Pair { a: 7, b: 8 };
    write_field(&pair, 99);
    if (pair.b != 99) {
        return 3;
    }

    pair.a = 12;
    if (pair.a != 12) {
        return 4;
    }

    deref id_ptr(&x) = 77;
    if (x != 77) {
        return 5;
    }

    var array_ptr_target: array<i32, 3> = [4, 5, 6];
    write_array_pointer(&array_ptr_target, 23);
    if (array_ptr_target[1] != 23) {
        return 6;
    }

    var y: i32 = 88;
    var pointer_box: PointerBox = PointerBox { data: &x };
    write_pointer_field(&pointer_box, &y);
    if (pointer_box.data != &y) {
        return 7;
    }

    return 0;
}
"#,
    );

    let ir_dir = dir.join("ir");
    run_wavec([
        OsStr::new("build"),
        src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-none-elf"),
        OsStr::new("--freestanding"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        ir_dir.as_os_str(),
    ]);

    let ir = fs::read_to_string(ir_dir.join("lvalue_store.ll")).unwrap();
    assert!(
        ir.contains("store i32") && ir.contains("getelementptr"),
        "lvalue store regression should generate store/GEP operations without requiring a host linker:\n{}",
        ir
    );

    if run_link_tests_enabled() {
        let target_dir = dir.join("target");
        run_wavec([
            OsStr::new("build"),
            src.as_os_str(),
            OsStr::new("--run"),
            OsStr::new("--target-dir"),
            target_dir.as_os_str(),
        ]);
    }
}

#[test]
fn projected_deref_preserves_pointer_storage_types() {
    let source = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/cases/shared/test103.wave");
    let dir = temp_case_dir("projected-pointer-loads");
    // Use the advertised targets so this regression also runs in isolated
    // backend builds and covers WebAssembly's 32-bit pointer representation.
    let (targets, _) = run_wavec_capture(["print", "target-list"]);
    for target in targets
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
    {
        let out = dir.join(target);
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--emit=ir"),
            OsStr::new("--out-dir"),
            out.as_os_str(),
        ]);
        let ir = fs::read_to_string(out.join("test103.ll")).unwrap();
        for name in ["check_writer", "check_index", "check_depth"] {
            let body = ir
                .split(&format!("@{name}("))
                .nth(1)
                .unwrap()
                .split("\n}")
                .next()
                .unwrap();
            assert!(body.contains("load ptr"), "{target} {name}: {body}");
            assert!(!body.contains("load i8"), "{target} {name}: {body}");
        }
        for name in ["widen_field", "widen_scalar"] {
            let body = ir
                .split(&format!("@{name}("))
                .nth(1)
                .unwrap()
                .split("\n}")
                .next()
                .unwrap();
            assert!(body.contains("load i8"), "{target} {name}: {body}");
            assert!(!body.contains("load i64"), "{target} {name}: {body}");
        }
    }
}

#[test]
fn freestanding_codegen_marks_functions_no_red_zone() {
    let dir = temp_case_dir("freestanding-noredzone");
    let src = write_wave(
        &dir,
        "leaf.wave",
        r#"
fun leaf(a: i64, b: i64, c: i64, d: i64, e: i64) -> i64 {
    var x: i64 = a + b;
    var y: i64 = c + d;
    return x + y + e;
}
"#,
    );

    let explicit_dir = dir.join("explicit");
    run_wavec([
        OsStr::new("build"),
        src.as_os_str(),
        OsStr::new("--freestanding"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        explicit_dir.as_os_str(),
    ]);
    let explicit_out = explicit_dir.join("leaf.ll");
    let explicit_ir = fs::read_to_string(&explicit_out).unwrap();
    assert!(
        explicit_ir.contains("noredzone"),
        "--freestanding IR must carry the LLVM noredzone function attribute:\n{}",
        explicit_ir
    );
    assert!(
        explicit_ir.contains("nounwind"),
        "--freestanding IR must mark Wave functions nounwind:\n{}",
        explicit_ir
    );

    let bare_dir = dir.join("bare");
    run_wavec([
        OsStr::new("build"),
        src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-none-elf"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        bare_dir.as_os_str(),
    ]);
    let bare_out = bare_dir.join("leaf.ll");
    let bare_ir = fs::read_to_string(&bare_out).unwrap();
    assert!(
        bare_ir.contains("noredzone"),
        "bare-metal target IR must carry the LLVM noredzone function attribute:\n{}",
        bare_ir
    );
    assert!(
        bare_ir.contains("nounwind"),
        "bare-metal target IR must mark Wave functions nounwind:\n{}",
        bare_ir
    );
}

#[test]
fn inline_asm_requires_explicit_stack_contract() {
    let dir = temp_case_dir("asm-stack-contract");
    let bad_src = write_wave(
        &dir,
        "bad_stack.wave",
        r#"
fun main() {
    asm {
        "sub rsp, 8"
        "add rsp, 8"
    }
}
"#,
    );

    let bad_dir = dir.join("bad");
    let err = run_wavec_expect_failure([
        OsStr::new("build"),
        bad_src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        bad_dir.as_os_str(),
    ]);
    assert!(
        err.contains("clobber(\\\"stack\\\")") || err.contains("clobber(\"stack\")"),
        "stack contract diagnostic should mention clobber(\"stack\"):\n{}",
        err
    );

    let good_src = write_wave(
        &dir,
        "good_stack.wave",
        r#"
fun main() {
    asm {
        "sub rsp, 8"
        "add rsp, 8"
        clobber("stack")
    }
}
"#,
    );
    let good_dir = dir.join("good");
    run_wavec([
        OsStr::new("build"),
        good_src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        good_dir.as_os_str(),
    ]);
    let ir = fs::read_to_string(good_dir.join("good_stack.ll")).unwrap();
    assert!(
        ir.contains("asm sideeffect alignstack"),
        "stack-declared inline asm should be volatile and alignstack:\n{}",
        ir
    );

    let unbalanced_src = write_wave(
        &dir,
        "unbalanced_stack.wave",
        r#"
fun main() {
    asm {
        "sub rsp, 8"
        clobber("stack")
    }
}
"#,
    );
    let unbalanced_dir = dir.join("unbalanced");
    let err = run_wavec_expect_failure([
        OsStr::new("build"),
        unbalanced_src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        unbalanced_dir.as_os_str(),
    ]);
    assert!(
        err.contains("stack delta is not balanced"),
        "unbalanced stack asm should be rejected:\n{}",
        err
    );

    let missing_noreturn_src = write_wave(
        &dir,
        "missing_noreturn.wave",
        r#"
fun main() {
    asm {
        "jmp rax"
        in("rax") 0
    }
}
"#,
    );
    let missing_noreturn_dir = dir.join("missing-noreturn");
    let err = run_wavec_expect_failure([
        OsStr::new("build"),
        missing_noreturn_src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        missing_noreturn_dir.as_os_str(),
    ]);
    assert!(
        err.contains("clobber(\\\"noreturn\\\")") || err.contains("clobber(\"noreturn\")"),
        "non-returning asm should require clobber(\"noreturn\"):\n{}",
        err
    );

    let noreturn_src = write_wave(
        &dir,
        "noreturn.wave",
        r#"
fun jump_out(addr: u64) {
    asm {
        "jmp rax"
        in("rax") addr
        clobber("noreturn")
    }
}
"#,
    );
    let noreturn_dir = dir.join("noreturn");
    run_wavec([
        OsStr::new("build"),
        noreturn_src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        noreturn_dir.as_os_str(),
    ]);
    let ir = fs::read_to_string(noreturn_dir.join("noreturn.ll")).unwrap();
    assert!(
        ir.contains("unreachable"),
        "noreturn inline asm should terminate the current IR block:\n{}",
        ir
    );
}

#[test]
fn inline_asm_rejects_invalid_contracts_and_allows_local_jumps() {
    let dir = temp_case_dir("asm-contract-extra");

    let local_jump_src = write_wave(
        &dir,
        "local_jump.wave",
        r#"
fun main() {
    asm {
        "jmp 1f"
        "1:"
    }
}

"#,
    );
    let local_jump_dir = dir.join("local-jump");
    run_wavec([
        OsStr::new("build"),
        local_jump_src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        local_jump_dir.as_os_str(),
    ]);

    let conflict_src = write_wave(
        &dir,
        "conflicting_stack.wave",
        r#"
fun main() {
    asm {
        "nop"
        clobber("stack")
        clobber("nostack")
    }
}
"#,
    );
    let conflict_dir = dir.join("conflict");
    let err = run_wavec_expect_failure([
        OsStr::new("build"),
        conflict_src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        conflict_dir.as_os_str(),
    ]);
    assert!(
        err.contains("cannot declare both"),
        "stack/nostack conflict should be rejected:\n{}",
        err
    );

    let expr_noreturn_src = write_wave(
        &dir,
        "expr_noreturn.wave",
        r#"
fun invalid_noreturn_expression() -> i64 {
    var x: i64 = asm {
        "jmp rax"
        in("rax") 0
        clobber("noreturn")
    };
    return x;
}

fun main() {
    invalid_noreturn_expression();
}
"#,
    );
    let expr_noreturn_dir = dir.join("expr-noreturn");
    let err = run_wavec_expect_failure([
        OsStr::new("build"),
        expr_noreturn_src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        expr_noreturn_dir.as_os_str(),
    ]);
    assert!(
        err.contains("asm expression cannot declare"),
        "asm expressions must reject noreturn:\n{}",
        err
    );

    let clobber_operand_conflict_src = write_wave(
        &dir,
        "clobber_operand_conflict.wave",
        r#"
fun main() {
    var x: i64 = 1;
    asm {
        "mov rax, rax"
        in("rax") x
        clobber("rax")
    }
}
"#,
    );
    let clobber_operand_conflict_dir = dir.join("clobber-operand-conflict");
    let err = run_wavec_expect_failure([
        OsStr::new("build"),
        clobber_operand_conflict_src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        clobber_operand_conflict_dir.as_os_str(),
    ]);
    assert!(
        err.contains("conflicts with an input/output operand register"),
        "clobber/operand register conflict should be rejected:\n{}",
        err
    );
}

#[test]
fn riscv64_inline_asm_enforces_reserved_registers_fprs_and_indirect_jumps() {
    let dir = temp_case_dir("riscv64-asm-contract");

    for reserved in ["sp", "zero"] {
        let source = write_wave(
            &dir,
            &format!("reserved_{}.wave", reserved),
            &format!(
                r#"
fun bind(value: u64) {{
    asm {{
        "nop"
        in("{reserved}") value
    }}
}}

fun main() {{}}
"#
            ),
        );
        let reserved_out = dir.join(format!("reserved-{}", reserved));
        let error = run_wavec_expect_failure([
            OsStr::new("--error-format=json"),
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-linux-gnu"),
            OsStr::new("--emit=obj"),
            OsStr::new("--out-dir"),
            reserved_out.as_os_str(),
        ]);
        assert!(error.contains("\"code\":\"E3401\""), "{}", error);
        assert!(!error.contains("compiler internal error"), "{}", error);
    }

    let float_source = write_wave(
        &dir,
        "float_register.wave",
        r#"
fun consume(value: f64) {
    asm {
        "fmv.d fa0, fa0"
        in("fa0") value
    }
}

fun main() {}
"#,
    );
    let float_out = dir.join("float-register");
    run_wavec([
        OsStr::new("build"),
        float_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--abi=lp64d"),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        float_out.as_os_str(),
    ]);
    assert!(float_out.join("float_register.o").is_file());

    let jump_source = write_wave(
        &dir,
        "jalr.wave",
        r#"
fun jump(addr: u64) {
    asm {
        "jalr x0, 0(a0)"
        in("a0") addr
        clobber("stack")
    }
}

fun main() {}
"#,
    );
    let jump_out = dir.join("jalr-missing-noreturn");
    let error = run_wavec_expect_failure([
        OsStr::new("build"),
        jump_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-none-elf"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        jump_out.as_os_str(),
    ]);
    assert!(error.contains("clobber(\"noreturn\")"), "{}", error);

    let noreturn_source = write_wave(
        &dir,
        "jalr_noreturn.wave",
        r#"
fun jump(addr: u64) {
    asm {
        "jalr x0, 0(a0)"
        in("a0") addr
        clobber("stack")
        clobber("noreturn")
    }
}

fun main() {}
"#,
    );
    let noreturn_out = dir.join("jalr-noreturn");
    run_wavec([
        OsStr::new("build"),
        noreturn_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-none-elf"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        noreturn_out.as_os_str(),
    ]);
    let ir = fs::read_to_string(noreturn_out.join("jalr_noreturn.ll")).unwrap();
    assert!(ir.contains("unreachable"), "{}", ir);
}

#[test]
fn riscv64_ir_and_bitcode_preserve_target_contract_when_recompiled() {
    let dir = temp_case_dir("riscv64-artifact-contract");
    let source = write_wave(&dir, "main.wave", "fun main() -> i32 { return 0; }\n");
    let original = dir.join("original");
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--abi=lp64f"),
        OsStr::new("--emit=ir,bc,obj"),
        OsStr::new("--out-dir"),
        original.as_os_str(),
    ]);

    let ir = fs::read_to_string(original.join("main.ll")).unwrap();
    assert!(ir.contains("!\"target-abi\", !\"lp64f\""), "{}", ir);
    assert!(ir.contains("!\"riscv-isa\""), "{}", ir);
    assert!(ir.contains("\"target-cpu\"=\"generic-rv64\""), "{}", ir);
    assert!(
        ir.contains("\"target-features\"=\"+m,+a,+f,-d,+c,+zicsr,+zifencei\""),
        "{}",
        ir
    );

    let from_ir = dir.join("from-ir");
    let ir_input = original.join("main.ll");
    run_wavec([
        OsStr::new("build"),
        ir_input.as_os_str(),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        from_ir.as_os_str(),
    ]);
    let from_bc = dir.join("from-bc");
    let bitcode_input = original.join("main.bc");
    run_wavec([
        OsStr::new("build"),
        bitcode_input.as_os_str(),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        from_bc.as_os_str(),
    ]);

    for object in [
        original.join("main.o"),
        from_ir.join("main.o"),
        from_bc.join("main.o"),
    ] {
        assert_eq!(
            riscv64_elf_flags(&object) & 0x7,
            0x3,
            "{}",
            object.display()
        );
        let bytes = fs::read(&object).unwrap();
        assert!(bytes_contains(&bytes, b"zicsr"), "{}", object.display());
        assert!(bytes_contains(&bytes, b"zifencei"), "{}", object.display());
    }
}

#[test]
fn riscv64_export_c_uses_indirect_aggregate_parameters_and_sret() {
    let dir = temp_case_dir("riscv64-export-c-aggregate");
    let source = write_wave(
        &dir,
        "aggregate.wave",
        r#"
struct Triple {
    a: u64;
    b: u64;
    c: u64;
}

export(c) fun wave_take(value: Triple) -> u64 {
    return value.c;
}

export(c) fun wave_make(a: u64, b: u64, c: u64) -> Triple {
    return Triple { a: a, b: b, c: c };
}

fun main() -> i32 {
    var value: Triple = wave_make(1, 2, 3);
    if (wave_take(value) != 3) {
        return 1;
    }
    return 0;
}
"#,
    );
    let out = dir.join("out");
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        out.as_os_str(),
    ]);
    let ir = fs::read_to_string(out.join("aggregate.ll")).unwrap();
    assert!(
        ir.contains("define i64 @wave_take(ptr byval(%Triple) align 8"),
        "{}",
        ir
    );
    assert!(
        ir.contains("define void @wave_make(ptr sret(%Triple) align 8"),
        "{}",
        ir
    );
    assert!(ir.contains("@__wave_export_impl_wave_take"), "{}", ir);
    assert!(ir.contains("@__wave_export_impl_wave_make"), "{}", ir);
}

#[test]
fn riscv64_c_abi_marks_required_integer_extensions() {
    let dir = temp_case_dir("riscv64-c-abi-integer-extensions");
    let source = write_wave(
        &dir,
        "integer_extensions.wave",
        r#"
extern(c) fun c_i8(value: i8) -> i8;
extern(c) fun c_u8(value: u8) -> u8;
extern(c) fun c_u32(value: u32) -> u32;
extern(c) fun c_variadic(count: i32, ...) -> i64;

export(c) fun wave_i8(value: i8) -> i8 { return value; }
export(c) fun wave_u8(value: u8) -> u8 { return value; }
export(c) fun wave_u32(value: u32) -> u32 { return value; }

fun main() -> i32 {
    if (c_i8(-1) != -1) { return 1; }
    if (c_u8(255) != 255) { return 2; }
    if (c_u32(4294967295) != 4294967295) { return 3; }
    var narrow: i8 = -1;
    var single: f32 = 1.5;
    if (c_variadic(2, narrow, single) != 0) { return 4; }
    return 0;
}
"#,
    );
    let out = dir.join("out");
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        out.as_os_str(),
    ]);
    let ir = fs::read_to_string(out.join("integer_extensions.ll")).unwrap();

    for expected in [
        "define signext i8 @wave_i8(i8 signext",
        "define zeroext i8 @wave_u8(i8 zeroext",
        "define signext i32 @wave_u32(i32 signext",
        "call signext i8 @c_i8(i8 signext",
        "call zeroext i8 @c_u8(i8 zeroext",
        "call signext i32 @c_u32(i32 signext",
        "declare signext i8 @c_i8(i8 signext)",
        "declare zeroext i8 @c_u8(i8 zeroext)",
        "declare signext i32 @c_u32(i32 signext)",
        "call i64 (i32, ...) @c_variadic(i32 signext 2, i32 signext %vararg1_sext, double %vararg2_f64)",
        "declare i64 @c_variadic(i32 signext, ...)",
    ] {
        assert!(ir.contains(expected), "missing `{expected}` in:\n{ir}");
    }
}

fn clang_for_contract_tests() -> Option<PathBuf> {
    if let Some(value) = std::env::var_os("CLANG") {
        let path = PathBuf::from(value);
        if Command::new(&path).arg("--version").output().is_ok() {
            return Some(path);
        }
    }
    ["clang-21", "clang"]
        .into_iter()
        .map(PathBuf::from)
        .find(|path| Command::new(path).arg("--version").output().is_ok())
}

fn llvm_ar_for_contract_tests() -> Option<PathBuf> {
    if let Some(value) = std::env::var_os("LLVM_AR") {
        let path = PathBuf::from(value);
        if Command::new(&path).arg("--version").output().is_ok() {
            return Some(path);
        }
    }
    ["llvm-ar-21", "llvm-ar"]
        .into_iter()
        .map(PathBuf::from)
        .find(|path| Command::new(path).arg("--version").output().is_ok())
}

#[test]
fn odd_sized_aggregate_transport_matches_clang_ir_contracts() {
    let Some(clang) = clang_for_contract_tests() else {
        eprintln!("skipped: clang is unavailable for ABI IR comparison");
        return;
    };
    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/c_abi_edges");
    let dir = temp_case_dir("odd-aggregate-clang-contract");

    for target in [
        "x86_64-unknown-linux-gnu",
        "x86_64-unknown-freebsd",
        "aarch64-unknown-freebsd",
        "riscv64-unknown-freebsd",
        "x86_64-apple-darwin",
        "aarch64-unknown-linux-gnu",
        "aarch64-apple-darwin",
        "riscv64-unknown-linux-gnu",
        "aarch64-w64-windows-gnu",
        "x86_64-pc-windows-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-pc-windows-msvc",
        #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
        "loongarch64-unknown-linux-gnu",
    ] {
        let tag = target.split('-').next().unwrap();
        let target_label = target.replace('-', "_");
        let clang_ir_path = dir.join(format!("{target_label}-clang.ll"));
        let clang_output = Command::new(&clang)
            .args(["-target", target, "-S", "-emit-llvm", "-ffreestanding"])
            .arg(fixture.join("interop.c"))
            .arg("-o")
            .arg(&clang_ir_path)
            .output()
            .unwrap();
        assert!(
            clang_output.status.success(),
            "clang ABI probe failed for {target}:\n{}",
            String::from_utf8_lossy(&clang_output.stderr)
        );
        let wave_dir = dir.join(format!("{target_label}-wave"));
        run_wavec([
            OsStr::new("build"),
            fixture.join("interop.wave").as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--emit=ir"),
            OsStr::new("--out-dir"),
            wave_dir.as_os_str(),
        ]);
        let clang_ir = fs::read_to_string(clang_ir_path).unwrap();
        let wave_ir = fs::read_to_string(wave_dir.join("interop.ll")).unwrap();
        if matches!(target, "x86_64-pc-windows-msvc" | "x86_64-pc-windows-gnu") {
            // Win64 passes only 1/2/4/8-byte aggregates as integers. All other
            // sizes use an sret pointer and a caller-owned indirect argument.
            for (name, size) in [
                ("bytes1", 1),
                ("bytes2", 2),
                ("bytes3", 3),
                ("bytes4", 4),
                ("bytes5", 5),
                ("bytes6", 6),
                ("bytes7", 7),
                ("bytes8", 8),
                ("bytes9", 9),
                ("bytes12", 12),
                ("bytes16", 16),
                ("nested", 8),
                ("array_member", 6),
                ("pointer_member", 8),
            ] {
                for (ir, prefix) in [(&clang_ir, "c"), (&wave_ir, "wave")] {
                    let line = ir
                        .lines()
                        .find(|line| {
                            line.starts_with("define ")
                                && line.contains(&format!("@{prefix}_{name}("))
                        })
                        .unwrap();
                    if matches!(size, 1 | 2 | 4 | 8) {
                        assert!(
                            line.contains(&format!("i{} @{prefix}_{name}(i{}", size * 8, size * 8)),
                            "{target}: {line}"
                        );
                    } else {
                        assert!(
                            line.contains(&format!("void @{prefix}_{name}(ptr"))
                                && line.contains("sret(")
                                && line.contains(", ptr")
                                && !line.contains("byval"),
                            "{target}: {line}"
                        );
                    }
                }
            }
            assert!(wave_ir.contains("alloca %Bytes3, align 16"), "{wave_ir}");
            continue;
        }
        for size in [1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 16] {
            let object_integer = format!("i{}", size * 8);
            let (result, argument) = match (tag, size <= 8) {
                ("x86_64", true) => (object_integer.clone(), object_integer),
                ("x86_64", false) => {
                    let remainder = (size - 8) * 8;
                    (
                        format!("{{ i64, i{remainder} }}"),
                        format!("i64, i{remainder}"),
                    )
                }
                ("aarch64", true) => (object_integer, "i64".to_string()),
                ("aarch64", false) => ("[2 x i64]".to_string(), "[2 x i64]".to_string()),
                ("riscv64" | "loongarch64", true) => ("i64".to_string(), "i64".to_string()),
                ("riscv64" | "loongarch64", false) => {
                    ("[2 x i64]".to_string(), "[2 x i64]".to_string())
                }
                _ => unreachable!(),
            };
            let c_contract = format!("{result} @c_bytes{size}({argument}");
            let wave_contract = format!("{result} @wave_bytes{size}({argument}");
            let clang_c_definition = clang_ir
                .lines()
                .find(|line| line.contains(&format!("@c_bytes{size}(")))
                .unwrap_or_else(|| panic!("missing c_bytes{size} definition:\n{clang_ir}"))
                .replace(" %0", "")
                .replace(" %1", "");
            assert!(
                clang_c_definition.contains(&c_contract),
                "missing `{c_contract}` in `{clang_c_definition}`"
            );
            assert!(
                wave_ir.contains(&format!("declare {c_contract}")),
                "{wave_ir}"
            );
            assert!(
                clang_ir.contains(&wave_contract),
                "missing `{wave_contract}`:\n{clang_ir}"
            );
            let wave_definition = wave_ir
                .lines()
                .find(|line| line.contains(&format!("@wave_bytes{size}(")))
                .unwrap_or_else(|| panic!("missing wave_bytes{size} definition:\n{wave_ir}"))
                .replace(" %0", "")
                .replace(" %1", "");
            assert!(
                wave_definition.contains(&format!("define {wave_contract}")),
                "missing `{wave_contract}` in `{wave_definition}`"
            );
        }
        for contract in ["void @c_empty()", "void @wave_empty()"] {
            assert!(
                clang_ir.contains(contract),
                "missing `{contract}`:\n{clang_ir}"
            );
        }
        assert!(wave_ir.contains("declare void @c_empty()"), "{wave_ir}");
        assert!(wave_ir.contains("define void @wave_empty()"), "{wave_ir}");

        let aggregate_contracts: &[(&str, &str, &str)] = match tag {
            "x86_64" => &[
                ("nested", "i64", "i64"),
                ("array_member", "i48", "i48"),
                ("pointer_member", "ptr", "ptr"),
            ],
            "aarch64" => &[
                ("nested", "i64", "i64"),
                ("array_member", "i48", "i64"),
                ("pointer_member", "i64", "ptr"),
            ],
            "riscv64" | "loongarch64" => &[
                ("nested", "i64", "i64"),
                ("array_member", "i64", "i64"),
                ("pointer_member", "i64", "i64"),
            ],
            _ => unreachable!(),
        };
        for (name, result, argument) in aggregate_contracts {
            let c_contract = format!("{result} @c_{name}({argument}");
            let wave_contract = format!("{result} @wave_{name}({argument}");
            // Some Clang builds spell a one-pointer aggregate transported in
            // one x86_64 or AArch64 GPR as `i64`, while others preserve the
            // opaque `ptr` in IR. Both spellings describe the same ABI slot.
            let mut clang_contracts = vec![c_contract.clone()];
            if matches!(tag, "x86_64" | "aarch64") && *name == "pointer_member" {
                clang_contracts.push("i64 @c_pointer_member(i64".to_string());
            }
            let clang_definition = clang_ir
                .lines()
                .find(|line| line.contains(&format!("@c_{name}(")))
                .unwrap_or_else(|| panic!("missing c_{name} definition:\n{clang_ir}"))
                .replace(" %0", "");
            assert!(
                clang_contracts
                    .iter()
                    .any(|contract| clang_definition.contains(contract)),
                "missing one of {clang_contracts:?} for {target} in `{clang_definition}`"
            );
            assert!(
                wave_ir.contains(&format!("declare {c_contract}")),
                "{wave_ir}"
            );
            let mut clang_wave_contracts = vec![wave_contract.clone()];
            if matches!(tag, "x86_64" | "aarch64") && *name == "pointer_member" {
                clang_wave_contracts.push("i64 @wave_pointer_member(i64".to_string());
            }
            assert!(
                clang_wave_contracts
                    .iter()
                    .any(|contract| clang_ir.contains(contract)),
                "missing one of {clang_wave_contracts:?} for {target}:\n{clang_ir}"
            );
            let wave_definition = wave_ir
                .lines()
                .find(|line| line.contains(&format!("@wave_{name}(")))
                .unwrap_or_else(|| panic!("missing wave_{name} definition:\n{wave_ir}"))
                .replace(" %0", "");
            assert!(
                wave_definition.contains(&format!("define {wave_contract}")),
                "{wave_definition}"
            );
        }
    }
}

#[test]
fn narrow_integer_c_abi_attributes_match_clang_targets() {
    let Some(clang) = clang_for_contract_tests() else {
        eprintln!("skipped: clang is unavailable for ABI IR comparison");
        return;
    };
    let dir = temp_case_dir("narrow-integer-c-abi-contract");
    let fixture =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/c_abi_edges/narrow.c");
    let source = write_wave(
        &dir,
        "narrow.wave",
        r#"
extern(c) fun c_i8(value: i8) -> i8;
extern(c) fun c_u8(value: u8) -> u8;
extern(c) fun c_i16(value: i16) -> i16;
extern(c) fun c_u16(value: u16) -> u16;
extern(c) fun c_i32(value: i32) -> i32;
extern(c) fun c_u32(value: u32) -> u32;
export(c) fun wave_i8(value: i8) -> i8 { return value; }
export(c) fun wave_u8(value: u8) -> u8 { return value; }
export(c) fun wave_i16(value: i16) -> i16 { return value; }
export(c) fun wave_u16(value: u16) -> u16 { return value; }
export(c) fun wave_i32(value: i32) -> i32 { return value; }
export(c) fun wave_u32(value: u32) -> u32 { return value; }
fun main() -> i32 { return c_i8(-1) as i32 + c_u8(1) as i32 + c_i16(-1) as i32 + c_u16(1) as i32 + c_i32(-1) + c_u32(1) as i32; }
"#,
    );
    for target in [
        "x86_64-unknown-linux-gnu",
        "x86_64-unknown-freebsd",
        "aarch64-unknown-freebsd",
        "riscv64-unknown-freebsd",
        "x86_64-apple-darwin",
        "aarch64-apple-darwin",
        "aarch64-unknown-linux-gnu",
        "x86_64-pc-windows-gnu",
        "riscv64-unknown-linux-gnu",
        "aarch64-w64-windows-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-pc-windows-msvc",
        #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
        "loongarch64-unknown-linux-gnu",
    ] {
        let out = dir.join(target);
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--emit=ir"),
            OsStr::new("--out-dir"),
            out.as_os_str(),
        ]);
        let ir = fs::read_to_string(out.join("narrow.ll")).unwrap();
        let clang_ir_path = dir.join(format!("{}-clang.ll", target));
        let clang_output = Command::new(&clang)
            .args(["-target", target, "-S", "-emit-llvm", "-ffreestanding"])
            .arg(&fixture)
            .arg("-o")
            .arg(&clang_ir_path)
            .output()
            .unwrap();
        assert!(
            clang_output.status.success(),
            "clang ABI probe failed for {target}:\n{}",
            String::from_utf8_lossy(&clang_output.stderr)
        );
        let clang_ir = fs::read_to_string(clang_ir_path).unwrap();

        for name in ["i8", "u8", "i16", "u16", "i32", "u32"] {
            let clang_line = clang_ir
                .lines()
                .find(|line| line.starts_with("define") && line.contains(&format!("@c_{name}(")))
                .unwrap_or_else(|| panic!("missing Clang definition for {name}:\n{clang_ir}"));
            let wave_definition = ir
                .lines()
                .find(|line| line.starts_with("define") && line.contains(&format!("@wave_{name}(")))
                .unwrap_or_else(|| panic!("missing Wave definition for {name}:\n{ir}"));
            let wave_declaration = ir
                .lines()
                .find(|line| line.starts_with("declare") && line.contains(&format!("@c_{name}(")))
                .unwrap_or_else(|| panic!("missing Wave declaration for {name}:\n{ir}"));
            let extension = if clang_line.contains("signext") {
                Some("signext")
            } else if clang_line.contains("zeroext") {
                Some("zeroext")
            } else {
                None
            };
            let clang_count = extension.map_or(0, |value| clang_line.matches(value).count());
            let wave_definition_count =
                extension.map_or(0, |value| wave_definition.matches(value).count());
            let wave_declaration_count =
                extension.map_or(0, |value| wave_declaration.matches(value).count());
            assert_eq!(
                wave_definition_count, clang_count,
                "target {target}, type {name}: Clang `{clang_line}`, Wave `{wave_definition}`"
            );
            assert_eq!(
                wave_declaration_count, clang_count,
                "target {target}, type {name}: Clang `{clang_line}`, Wave `{wave_declaration}`"
            );
            if extension.is_none() {
                assert!(!wave_definition.contains("signext"));
                assert!(!wave_definition.contains("zeroext"));
                assert!(!wave_declaration.contains("signext"));
                assert!(!wave_declaration.contains("zeroext"));
            }
        }
    }
}

#[test]
fn c_variadic_promotions_use_semantic_expression_types() {
    let dir = temp_case_dir("c-variadic-semantic-promotions");
    let source = write_wave(
        &dir,
        "promotions.wave",
        r#"
extern(c) fun consume(count: i32, ...) -> i64;
fun signed_result() -> i8 { return -1; }
fun main() -> i32 {
    var signed: i8 = -128;
    var unsigned: u8 = 255;
    var zero: i8 = 0;
    var one: i8 = 1;
    consume(10, signed, unsigned, signed + 127, zero - one, signed * zero - one, signed < zero, !zero, (signed + 127) * one, signed_result(), 255 as u8);
    consume(2, null as ptr<byte>, null as ptr<i32>);
    return 0;
}
"#,
    );
    let out = dir.join("out");
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        out.as_os_str(),
    ]);
    let ir = fs::read_to_string(out.join("promotions.ll")).unwrap();
    for expected in [
        "%vararg1_sext = sext i8",
        "%vararg2_zext = zext i8",
        "%vararg3_sext = sext i8",
        "%vararg4_sext = sext i8",
        "%vararg5_sext = sext i8",
        "%vararg6_zext = zext i1",
        "%vararg7_zext = zext i1",
        "%vararg8_sext = sext i8",
        "%vararg9_sext = sext i8",
    ] {
        assert!(ir.contains(expected), "missing `{expected}`:\n{ir}");
    }
    assert!(ir.contains("i32 signext 255"), "{ir}");
    assert!(ir.contains("ptr null, ptr null"), "{ir}");

    let invalid = write_wave(
        &dir,
        "untyped_null.wave",
        "extern(c) fun consume(count: i32, ...) -> i64; fun main() { consume(1, null); }\n",
    );
    let error = run_wavec_expect_failure([OsStr::new("check"), invalid.as_os_str()]);
    assert!(
        error.contains("variadic argument 2") && error.contains("no scalar type"),
        "{error}"
    );
}

#[test]
fn discarded_c_return_values_do_not_require_an_expected_type() {
    let dir = temp_case_dir("discarded-c-return-values");
    let source = write_wave(
        &dir,
        "discard.wave",
        r#"
struct Empty {}
struct Pair { first: i64; second: i64; }
extern(c) fun c_empty(value: Empty) -> Empty;
extern(c) fun c_pair(value: Pair) -> Pair;
extern(c) fun c_integer() -> i64;
extern(c) fun c_float() -> f64;
extern(c) fun c_pointer() -> ptr<byte>;
fun main() {
    c_empty(Empty {});
    c_pair(Pair { first: 1, second: 2 });
    c_integer();
    c_float();
    c_pointer();
}
"#,
    );
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        dir.join("out").as_os_str(),
    ]);
}

#[test]
fn riscv_link_input_abi_is_validated_before_linking() {
    let dir = temp_case_dir("riscv-pre-link-abi");
    let source = write_wave(&dir, "main.wave", "fun main() -> i32 { return 0; }\n");
    let mut objects = Vec::new();
    for abi in ["lp64", "lp64f", "lp64d"] {
        let out = dir.join(abi);
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-linux-gnu"),
            OsStr::new("--abi"),
            OsStr::new(abi),
            OsStr::new("--emit=obj"),
            OsStr::new("--out-dir"),
            out.as_os_str(),
        ]);
        objects.push((abi, out.join("main.o")));
    }
    for (abi, object) in &objects {
        let expected = RiscvFloatAbi::from_target_abi(abi).unwrap();
        validate_riscv_link_inputs(expected, &[object.display().to_string()]).unwrap();
    }
    let error =
        validate_riscv_link_inputs(RiscvFloatAbi::Lp64d, &[objects[0].1.display().to_string()])
            .unwrap_err()
            .to_string();
    assert!(error.contains("target ABI: LP64D"), "{error}");
    assert!(error.contains("input ABI: LP64"), "{error}");

    let llvm_ar = llvm_ar_for_contract_tests()
        .expect("llvm-ar is required to build cross-target archive fixtures");
    let archive = dir.join("libmixed.a");
    let archive_output = Command::new(&llvm_ar)
        .arg("--format=darwin")
        .arg("rcs")
        .arg(&archive)
        .arg(&objects[1].1)
        .output()
        .expect("failed to start llvm-ar");
    assert!(
        archive_output.status.success(),
        "{} failed to build the archive fixture:\n{}",
        llvm_ar.display(),
        String::from_utf8_lossy(&archive_output.stderr)
    );
    let archive_error =
        validate_riscv_link_inputs(RiscvFloatAbi::Lp64d, &[archive.display().to_string()])
            .unwrap_err()
            .to_string();
    assert!(
        archive_error.contains("libmixed.a(main.o)"),
        "{archive_error}"
    );
    assert!(
        archive_error.contains("input ABI: LP64F"),
        "{archive_error}"
    );

    for (input, expected_input_abi) in [(&objects[0].1, "LP64"), (&archive, "LP64F")] {
        let output = run_wavec_raw([
            OsStr::new("build"),
            source.as_os_str(),
            input.as_os_str(),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-linux-gnu"),
            OsStr::new("--abi=lp64d"),
            OsStr::new("--no-start-files"),
            OsStr::new("-Cno-default-libs"),
            OsStr::new("-Clinker=/bin/false"),
            OsStr::new("--out-dir"),
            dir.join("link-attempt").as_os_str(),
        ]);
        assert!(!output.status.success());
        let error = String::from_utf8_lossy(&output.stderr);
        assert!(
            error.contains("RISC-V floating-point ABI mismatch before linking"),
            "{error}"
        );
        assert!(error.contains("target ABI: LP64D"), "{error}");
        assert!(
            error.contains(&format!("input ABI: {expected_input_abi}")),
            "{error}"
        );
        assert!(
            !error.contains("link failed"),
            "external linker ran before ABI validation: {error}"
        );
    }
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
#[test]
fn loongarch64_link_inputs_require_matching_abi_before_linking() {
    let dir = temp_case_dir("loongarch64-pre-link-abi");
    let source = write_wave(&dir, "main.wave", "fun main() -> i32 { return 0; }\n");
    let out = dir.join("out");
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target=loongarch64-unknown-linux-gnu"),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        out.as_os_str(),
    ]);
    let compatible = out.join("main.o");
    validate_loongarch64_link_inputs(
        LoongArchFloatAbi::Lp64d,
        &[compatible.display().to_string()],
    )
    .unwrap();

    let incompatible = dir.join("soft-float.o");
    let mut bytes = fs::read(&compatible).unwrap();
    bytes[48..52].copy_from_slice(&0x41_u32.to_le_bytes());
    fs::write(&incompatible, bytes).unwrap();
    let error = validate_loongarch64_link_inputs(
        LoongArchFloatAbi::Lp64d,
        &[incompatible.display().to_string()],
    )
    .unwrap_err()
    .to_string();
    assert!(error.contains("target ABI: LP64D"), "{error}");
    assert!(error.contains("input ABI: LP64S"), "{error}");

    let output = run_wavec_raw([
        OsStr::new("build"),
        source.as_os_str(),
        incompatible.as_os_str(),
        OsStr::new("--target=loongarch64-unknown-linux-gnu"),
        OsStr::new("--no-start-files"),
        OsStr::new("-Cno-default-libs"),
        OsStr::new("-Clinker=/bin/false"),
        OsStr::new("--out-dir"),
        dir.join("link-attempt").as_os_str(),
    ]);
    assert!(!output.status.success());
    let error = String::from_utf8_lossy(&output.stderr);
    assert!(
        error.contains("LoongArch floating-point ABI mismatch before linking"),
        "{error}"
    );
    assert!(
        !error.contains("link failed"),
        "external linker ran before ABI validation: {error}"
    );
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
#[test]
fn loongarch64_lp64f_hosted_linking_is_rejected_before_the_linker() {
    let dir = temp_case_dir("loongarch64-lp64f-hosted-link");
    let source = write_wave(&dir, "main.wave", "fun main() -> i32 { return 0; }\n");
    let output = run_wavec_raw([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target=loongarch64-unknown-linux-gnu"),
        OsStr::new("--abi=lp64f"),
        OsStr::new("-Clinker=/bin/false"),
        OsStr::new("--out-dir"),
        dir.join("link-attempt").as_os_str(),
    ]);

    assert!(!output.status.success());
    let error = String::from_utf8_lossy(&output.stderr);
    assert!(
        error.contains("glibc does not provide an LP64F runtime"),
        "{error}"
    );
    assert!(
        !error.contains("link failed"),
        "external linker ran before LP64F runtime validation: {error}"
    );
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
#[test]
fn loongarch64_float_abi_modes_match_clang_contracts() {
    let dir = temp_case_dir("loongarch64-float-abi-modes");
    let source = write_wave(
        &dir,
        "modes.wave",
        r#"
struct F1 { x: f32; }
struct D1 { x: f64; }
struct FF { x: f32; y: f32; }
struct FD { x: f32; y: f64; }
extern(c) fun c_f1(x: F1) -> F1;
extern(c) fun c_d1(x: D1) -> D1;
extern(c) fun c_ff(x: FF) -> FF;
extern(c) fun c_fd(x: FD) -> FD;
fun main() -> i32 { return 0; }
"#,
    );

    let contracts = [
        (
            "lp64s",
            0x1,
            [
                "declare i64 @c_f1(i64)",
                "declare i64 @c_d1(i64)",
                "declare i64 @c_ff(i64)",
                "declare [2 x i64] @c_fd([2 x i64])",
            ],
        ),
        (
            "lp64f",
            0x2,
            [
                "declare float @c_f1(float)",
                "declare i64 @c_d1(i64)",
                "declare { float, float } @c_ff(float, float)",
                "declare [2 x i64] @c_fd([2 x i64])",
            ],
        ),
        (
            "lp64d",
            0x3,
            [
                "declare float @c_f1(float)",
                "declare double @c_d1(double)",
                "declare { float, float } @c_ff(float, float)",
                "declare { float, double } @c_fd(float, double)",
            ],
        ),
    ];

    for (abi, expected_flags, declarations) in contracts {
        let output = dir.join(abi);
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target=loongarch64-unknown-linux-gnu"),
            OsStr::new("--abi"),
            OsStr::new(abi),
            OsStr::new("--emit=ir,obj"),
            OsStr::new("--out-dir"),
            output.as_os_str(),
        ]);
        let ir = fs::read_to_string(output.join("modes.ll")).unwrap();
        for declaration in declarations {
            assert!(
                ir.contains(declaration),
                "{abi}: missing `{declaration}`\n{ir}"
            );
        }
        let object = output.join("modes.o");
        assert_eq!(loongarch64_elf_flags(&object) & 0x7, expected_flags);
        let target_abi = LoongArchFloatAbi::from_target_abi(abi).unwrap();
        validate_loongarch64_link_inputs(target_abi, &[object.display().to_string()]).unwrap();

        let assembly = dir.join(format!("raw-{abi}.s"));
        fs::write(&assembly, "ret\n").unwrap();
        let assembly_output = dir.join(format!("asm-{abi}"));
        run_wavec([
            OsStr::new("build"),
            assembly.as_os_str(),
            OsStr::new("--target=loongarch64-unknown-linux-gnu"),
            OsStr::new("--abi"),
            OsStr::new(abi),
            OsStr::new("--emit=obj"),
            OsStr::new("--out-dir"),
            assembly_output.as_os_str(),
        ]);
        assert_eq!(
            loongarch64_elf_flags(&assembly_output.join(format!("raw-{abi}.o"))) & 0x7,
            expected_flags
        );
    }
}

fn run_linux_c_abi_fixture(
    fixture_name: &str,
    target: &str,
    c_compiler: &str,
    runner: Option<&str>,
) {
    let dir = temp_case_dir(&format!("{fixture_name}-c-abi-interop"));
    let fixture_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join(fixture_name);
    let c_object = dir.join("interop-c.o");
    let wave_out = dir.join("wave");
    let binary = dir.join("interop");

    let c_compile = Command::new(c_compiler)
        .args([
            "-O2",
            "-ffreestanding",
            "-fno-builtin",
            "-fno-stack-protector",
            "-c",
        ])
        .arg(fixture_dir.join("interop.c"))
        .arg("-o")
        .arg(&c_object)
        .output()
        .unwrap_or_else(|error| panic!("failed to start {c_compiler}: {error}"));
    assert!(
        c_compile.status.success(),
        "{fixture_name} C fixture compile failed:\n{}",
        String::from_utf8_lossy(&c_compile.stderr)
    );

    run_wavec([
        OsStr::new("build"),
        fixture_dir.join("interop.wave").as_os_str(),
        OsStr::new("--target"),
        OsStr::new(target),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        wave_out.as_os_str(),
    ]);

    let link = Command::new(c_compiler)
        .args(["-nostdlib", "-static", "-Wl,-e,_start"])
        .arg(wave_out.join("interop.o"))
        .arg(&c_object)
        .arg("-o")
        .arg(&binary)
        .output()
        .unwrap_or_else(|error| panic!("failed to start {c_compiler} linker: {error}"));
    assert!(
        link.status.success(),
        "{fixture_name} fixture link failed:\n{}",
        String::from_utf8_lossy(&link.stderr)
    );

    let mut command = if let Some(runner) = runner {
        let mut command = Command::new(runner);
        command.arg(&binary);
        command
    } else {
        Command::new(&binary)
    };
    let run = command
        .output()
        .unwrap_or_else(|error| panic!("failed to run {fixture_name} fixture: {error}"));
    assert!(
        run.status.success(),
        "{fixture_name} C/Wave ABI fixture failed with status {}\nstdout:\n{}\nstderr:\n{}",
        run.status,
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
}

fn pe_machine(path: &Path) -> u16 {
    let bytes = fs::read(path).unwrap();
    assert!(
        bytes.len() >= 0x40 && &bytes[..2] == b"MZ",
        "invalid PE file"
    );
    let pe_offset = u32::from_le_bytes(bytes[0x3c..0x40].try_into().unwrap()) as usize;
    assert!(
        bytes.len() >= pe_offset + 6 && &bytes[pe_offset..pe_offset + 4] == b"PE\0\0",
        "invalid PE signature"
    );
    u16::from_le_bytes(bytes[pe_offset + 4..pe_offset + 6].try_into().unwrap())
}

#[test]
fn windows_arm64_c_abi_links_with_mingw() {
    if std::env::var_os("WAVE_RUN_WINDOWS_ARM64_INTEROP_TESTS").is_none() {
        eprintln!("skipped: set WAVE_RUN_WINDOWS_ARM64_INTEROP_TESTS=1 to run the link contract");
        return;
    }

    let linker = std::env::var("WAVE_WINDOWS_ARM64_LINKER")
        .expect("WAVE_WINDOWS_ARM64_LINKER must name the llvm-mingw ARM64 driver");
    let dir = temp_case_dir("windows-arm64-c-abi-interop");
    let fixture_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/c_abi_edges");
    let c_object = dir.join("interop-c.obj");
    let binary = dir.join("interop.exe");

    let c_compile = Command::new(&linker)
        .args(["-O2", "-fno-builtin", "-fno-stack-protector", "-c"])
        .arg(fixture_dir.join("interop.c"))
        .arg("-o")
        .arg(&c_object)
        .output()
        .unwrap_or_else(|error| panic!("failed to start {linker}: {error}"));
    assert!(
        c_compile.status.success(),
        "Windows ARM64 C fixture compile failed:\n{}",
        String::from_utf8_lossy(&c_compile.stderr)
    );

    run_wavec([
        OsStr::new("build"),
        fixture_dir.join("interop.wave").as_os_str(),
        c_object.as_os_str(),
        OsStr::new("--target=aarch64-w64-windows-gnu"),
        OsStr::new("--emit=bin"),
        OsStr::new("-o"),
        binary.as_os_str(),
    ]);

    assert_eq!(pe_machine(&binary), 0xaa64, "expected PE/COFF ARM64");

    let std_source =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/cases/windows/arm64/test1.wave");
    let std_binary = dir.join("std-smoke.exe");
    run_wavec([
        OsStr::new("build"),
        std_source.as_os_str(),
        OsStr::new("--target=aarch64-w64-windows-gnu"),
        OsStr::new("--emit=bin"),
        OsStr::new("-o"),
        std_binary.as_os_str(),
    ]);
    assert_eq!(
        pe_machine(&std_binary),
        0xaa64,
        "expected std-linked PE/COFF ARM64"
    );
}

#[test]
fn x86_64_c_abi_interoperates_with_c() {
    if std::env::var_os("WAVE_RUN_X86_64_INTEROP_TESTS").is_none() {
        eprintln!("skipped: set WAVE_RUN_X86_64_INTEROP_TESTS=1 to run native ABI test");
        return;
    }

    assert_eq!(std::env::consts::ARCH, "x86_64");
    assert_eq!(std::env::consts::OS, "linux");
    run_linux_c_abi_fixture("x86_64_sysv", "x86_64-unknown-linux-gnu", "gcc", None);
    run_linux_c_abi_fixture("c_abi_edges", "x86_64-unknown-linux-gnu", "gcc", None);
}

#[test]
fn aarch64_c_abi_interoperates_with_c() {
    if std::env::var_os("WAVE_RUN_AARCH64_INTEROP_TESTS").is_none() {
        eprintln!("skipped: set WAVE_RUN_AARCH64_INTEROP_TESTS=1 to run ABI test");
        return;
    }

    assert_eq!(std::env::consts::OS, "linux");
    let native = std::env::consts::ARCH == "aarch64";
    run_linux_c_abi_fixture(
        "aarch64_aapcs64",
        "aarch64-unknown-linux-gnu",
        if native {
            "gcc"
        } else {
            "aarch64-linux-gnu-gcc"
        },
        if native { None } else { Some("qemu-aarch64") },
    );
    run_linux_c_abi_fixture(
        "c_abi_edges",
        "aarch64-unknown-linux-gnu",
        if native {
            "gcc"
        } else {
            "aarch64-linux-gnu-gcc"
        },
        if native { None } else { Some("qemu-aarch64") },
    );
}

#[test]
fn riscv64_c_abi_interoperates_with_c_under_qemu() {
    if std::env::var_os("WAVE_RUN_RISCV64_INTEROP_TESTS").is_none() {
        eprintln!("skipped: set WAVE_RUN_RISCV64_INTEROP_TESTS=1 to run cross-toolchain test");
        return;
    }

    let dir = temp_case_dir("riscv64-c-abi-interop");
    let fixture_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("riscv64_psabi");
    let fixture_wave = fixture_dir.join("interop.wave");
    let c_object = dir.join("interop-c.o");
    let wave_out = dir.join("wave");
    let binary = dir.join("interop");

    let c_compile = Command::new("riscv64-linux-gnu-gcc")
        .args([
            "-march=rv64gc",
            "-mabi=lp64d",
            "-msmall-data-limit=0",
            "-O2",
            "-ffreestanding",
            "-fno-builtin",
            "-fno-stack-protector",
            "-c",
        ])
        .arg(fixture_dir.join("interop.c"))
        .arg("-o")
        .arg(&c_object)
        .output()
        .expect("failed to start riscv64-linux-gnu-gcc");
    assert!(
        c_compile.status.success(),
        "C fixture compile failed:\n{}",
        String::from_utf8_lossy(&c_compile.stderr)
    );

    run_wavec([
        OsStr::new("build"),
        fixture_wave.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        wave_out.as_os_str(),
    ]);

    let link = Command::new("riscv64-linux-gnu-gcc")
        .args([
            "-march=rv64gc",
            "-mabi=lp64d",
            "-nostdlib",
            "-static",
            "-Wl,-e,_start",
        ])
        .arg(wave_out.join("interop.o"))
        .arg(&c_object)
        .arg("-o")
        .arg(&binary)
        .output()
        .expect("failed to start riscv64-linux-gnu-gcc linker");
    assert!(
        link.status.success(),
        "RISC-V fixture link failed:\n{}",
        String::from_utf8_lossy(&link.stderr)
    );

    let run = Command::new("qemu-riscv64")
        .arg(&binary)
        .output()
        .expect("failed to start qemu-riscv64");
    assert!(
        run.status.success(),
        "C/Wave psABI fixture failed with status {}\nstdout:\n{}\nstderr:\n{}",
        run.status,
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );

    run_linux_c_abi_fixture(
        "c_abi_edges",
        "riscv64-unknown-linux-gnu",
        "riscv64-linux-gnu-gcc",
        Some("qemu-riscv64"),
    );
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
#[test]
fn loongarch64_lp64d_c_abi_interoperates_with_clang_under_qemu() {
    run_loongarch64_c_abi_fixture("lp64d", "loongarch64_psabi");
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
#[test]
fn loongarch64_lp64s_c_abi_interoperates_with_clang_under_qemu() {
    run_loongarch64_c_abi_fixture("lp64s", "loongarch64_lp64s");
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
fn run_loongarch64_c_abi_fixture(abi: &str, fixture_name: &str) {
    if std::env::var_os("WAVE_RUN_LOONGARCH64_INTEROP_TESTS").is_none() {
        eprintln!("skipped: set WAVE_RUN_LOONGARCH64_INTEROP_TESTS=1 to run cross-toolchain test");
        return;
    }

    let clang = clang_for_contract_tests().expect("clang 21 is required for LoongArch ABI tests");
    let lld = ["ld.lld-21", "ld.lld"]
        .into_iter()
        .find(|tool| Command::new(tool).arg("--version").output().is_ok())
        .expect("ld.lld 21 is required for LoongArch ABI tests");
    assert!(
        Command::new("qemu-loongarch64")
            .arg("--version")
            .output()
            .is_ok(),
        "qemu-loongarch64 is required for LoongArch ABI tests"
    );

    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(fixture_name);
    let dir = temp_case_dir(&format!("loongarch64-{abi}-c-abi-interop"));
    let c_object = dir.join("interop-c.o");
    let wave_out = dir.join("wave");
    let binary = dir.join("interop");

    let c_compile = Command::new(&clang)
        .arg(format!("-mabi={abi}"))
        .args([
            "--target=loongarch64-unknown-linux-gnu",
            "-O2",
            "-ffreestanding",
            "-fno-builtin",
            "-fno-stack-protector",
            "-c",
        ])
        .arg(fixture.join("interop.c"))
        .arg("-o")
        .arg(&c_object)
        .output()
        .expect("failed to start clang for LoongArch");
    assert!(
        c_compile.status.success(),
        "LoongArch C fixture compile failed:\n{}",
        String::from_utf8_lossy(&c_compile.stderr)
    );

    run_wavec([
        OsStr::new("build"),
        fixture.join("interop.wave").as_os_str(),
        OsStr::new("--target=loongarch64-unknown-linux-gnu"),
        OsStr::new("--abi"),
        OsStr::new(abi),
        OsStr::new("--emit=ir,obj"),
        OsStr::new("--out-dir"),
        wave_out.as_os_str(),
    ]);
    let wave_ir = fs::read_to_string(wave_out.join("interop.ll")).unwrap();
    let contracts: &[&str] = if abi == "lp64d" {
        &[
            "declare double @c_f1(double)",
            "declare { double, double } @c_f2(double, double)",
            "declare { double, i64 } @c_fi(double, i64)",
            "declare { i64, double } @c_if_pair(i64, double)",
            "declare { float, double } @c_fd_padded(float, double)",
            "declare { double, i32 } @c_nested(double, i32)",
            "declare void @c_large(ptr sret(%Large) align 8, ptr)",
        ]
    } else {
        &[
            "declare i64 @c_double(double)",
            "declare [2 x i64] @c_pair([2 x i64])",
            "declare void @c_large(ptr sret(%Large) align 8, ptr)",
        ]
    };
    for contract in contracts {
        assert!(
            wave_ir.contains(*contract),
            "missing `{contract}`:\n{wave_ir}"
        );
    }

    let expected_flags = if abi == "lp64s" { 1 } else { 3 };
    for object in [&c_object, &wave_out.join("interop.o")] {
        assert_eq!(loongarch64_elf_flags(object) & 7, expected_flags);
        validate_loongarch64_link_inputs(
            LoongArchFloatAbi::from_target_abi(abi).unwrap(),
            &[object.display().to_string()],
        )
        .unwrap();
    }
    let link = Command::new(lld)
        .args(["-m", "elf64loongarch", "-static", "-e", "_start"])
        .arg(&c_object)
        .arg(wave_out.join("interop.o"))
        .arg("-o")
        .arg(&binary)
        .output()
        .expect("failed to start ld.lld for LoongArch");
    assert!(
        link.status.success(),
        "LoongArch fixture link failed:\n{}",
        String::from_utf8_lossy(&link.stderr)
    );

    let run = Command::new("qemu-loongarch64")
        .arg(&binary)
        .output()
        .expect("failed to start qemu-loongarch64");
    assert!(
        run.status.success(),
        "LoongArch {abi} fixture failed with status {}\nstdout:\n{}\nstderr:\n{}",
        run.status,
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
}

#[test]
fn riscv64_lp64_abi_modes_interoperate_with_c_under_qemu() {
    if std::env::var_os("WAVE_RUN_RISCV64_INTEROP_TESTS").is_none() {
        eprintln!("skipped: set WAVE_RUN_RISCV64_INTEROP_TESTS=1 to run cross-toolchain test");
        return;
    }

    let fixture_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("riscv64_psabi");

    for (abi, march, features) in [
        ("lp64", "rv64imac", "+m,+a,-f,-d,+c"),
        ("lp64f", "rv64imafc", "+m,+a,+f,-d,+c"),
        ("lp64d", "rv64gc", "+m,+a,+f,+d,+c"),
    ] {
        let dir = temp_case_dir(&format!("riscv64-{abi}-c-abi-interop"));
        let c_object = dir.join("abi-modes-c.o");
        let wave_out = dir.join("wave");
        let binary = dir.join("abi-modes");

        let c_compile = Command::new("riscv64-linux-gnu-gcc")
            .arg(format!("-march={march}"))
            .arg(format!("-mabi={abi}"))
            .args([
                "-msmall-data-limit=0",
                "-O2",
                "-ffreestanding",
                "-fno-builtin",
                "-fno-stack-protector",
                "-c",
            ])
            .arg(fixture_dir.join("abi_modes.c"))
            .arg("-o")
            .arg(&c_object)
            .output()
            .expect("failed to start riscv64-linux-gnu-gcc");
        assert!(
            c_compile.status.success(),
            "{abi} C fixture compile failed:\n{}",
            String::from_utf8_lossy(&c_compile.stderr)
        );

        run_wavec([
            OsStr::new("build"),
            fixture_dir.join("abi_modes.wave").as_os_str(),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-linux-gnu"),
            OsStr::new("--features"),
            OsStr::new(features),
            OsStr::new("--abi"),
            OsStr::new(abi),
            OsStr::new("--emit=obj"),
            OsStr::new("--out-dir"),
            wave_out.as_os_str(),
        ]);

        let link = Command::new("riscv64-linux-gnu-gcc")
            .arg(format!("-march={march}"))
            .arg(format!("-mabi={abi}"))
            .args(["-nostdlib", "-static", "-Wl,-e,_start"])
            .arg(wave_out.join("abi_modes.o"))
            .arg(&c_object)
            .arg("-o")
            .arg(&binary)
            .output()
            .expect("failed to start riscv64-linux-gnu-gcc linker");
        assert!(
            link.status.success(),
            "{abi} RISC-V fixture link failed:\n{}",
            String::from_utf8_lossy(&link.stderr)
        );

        let run = Command::new("qemu-riscv64")
            .arg(&binary)
            .output()
            .expect("failed to start qemu-riscv64");
        assert!(
            run.status.success(),
            "{abi} C/Wave psABI fixture failed with status {}",
            run.status
        );
    }
}

#[test]
fn riscv64_relocation_atomic_and_compressed_feature_contracts() {
    if std::env::var_os("WAVE_RUN_RISCV64_INTEROP_TESTS").is_none() {
        eprintln!("skipped: set WAVE_RUN_RISCV64_INTEROP_TESTS=1 to run toolchain test");
        return;
    }

    let dir = temp_case_dir("riscv64-object-contracts");
    let relocation_source = write_wave(
        &dir,
        "relocation.ll",
        r#"
@local_value = global i64 7, align 8
@external_value = external global i64

define ptr @local_address() {
entry:
  ret ptr @local_value
}

define ptr @external_address() {
entry:
  ret ptr @external_value
}
"#,
    );
    let pic_out = dir.join("pic");
    let static_out = dir.join("static");
    run_wavec([
        OsStr::new("build"),
        relocation_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("-Crelocation-model=pic"),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        pic_out.as_os_str(),
    ]);
    run_wavec([
        OsStr::new("build"),
        relocation_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("-Crelocation-model=static"),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        static_out.as_os_str(),
    ]);

    let read_relocations = |path: &Path| {
        let output = Command::new("llvm-readobj")
            .arg("--relocations")
            .arg(path)
            .output()
            .expect("failed to start llvm-readobj");
        assert!(
            output.status.success(),
            "llvm-readobj failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
        String::from_utf8_lossy(&output.stdout).into_owned()
    };
    let pic_relocations = read_relocations(&pic_out.join("relocation.o"));
    assert!(pic_relocations.contains("R_RISCV_GOT_HI20 local_value"));
    assert!(pic_relocations.contains("R_RISCV_GOT_HI20 external_value"));
    assert!(pic_relocations.contains("R_RISCV_PCREL_LO12_I"));
    assert!(!pic_relocations.contains("R_RISCV_HI20 local_value"));

    let static_relocations = read_relocations(&static_out.join("relocation.o"));
    assert!(static_relocations.contains("R_RISCV_HI20 local_value"));
    assert!(static_relocations.contains("R_RISCV_LO12_I local_value"));
    assert!(static_relocations.contains("R_RISCV_HI20 external_value"));
    assert!(!static_relocations.contains("R_RISCV_GOT_HI20"));

    let atomic_source = write_wave(
        &dir,
        "atomic.ll",
        r#"
define i64 @atomic_add(ptr %address) {
entry:
  %previous = atomicrmw add ptr %address, i64 1 seq_cst
  ret i64 %previous
}
"#,
    );
    let atomic_a_out = dir.join("atomic-a");
    let atomic_no_a_out = dir.join("atomic-no-a");
    let compressed_off_out = dir.join("compressed-off");
    for (features, out) in [
        ("+m,+a,-f,-d,+c", &atomic_a_out),
        ("+m,-a,-f,-d,+c", &atomic_no_a_out),
        ("+m,+a,-f,-d,-c", &compressed_off_out),
    ] {
        run_wavec([
            OsStr::new("build"),
            atomic_source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-linux-gnu"),
            OsStr::new("--features"),
            OsStr::new(features),
            OsStr::new("--abi=lp64"),
            OsStr::new("--emit=obj,asm"),
            OsStr::new("--out-dir"),
            out.as_os_str(),
        ]);
    }

    let atomic_a_assembly = fs::read_to_string(atomic_a_out.join("atomic.s")).unwrap();
    assert!(atomic_a_assembly.contains("amoadd.d.aqrl"));
    assert!(!atomic_a_assembly.contains("__atomic_fetch_add_8"));
    let atomic_no_a_assembly = fs::read_to_string(atomic_no_a_out.join("atomic.s")).unwrap();
    assert!(!atomic_no_a_assembly.contains("amoadd"));
    assert!(atomic_no_a_assembly.contains("__atomic_fetch_add_8"));

    let compressed_object = atomic_a_out.join("atomic.o");
    let uncompressed_object = compressed_off_out.join("atomic.o");
    assert_eq!(riscv64_elf_flags(&compressed_object) & 1, 1);
    assert_eq!(riscv64_elf_flags(&uncompressed_object) & 1, 0);
    let compressed_bytes = fs::read(&compressed_object).unwrap();
    let uncompressed_bytes = fs::read(&uncompressed_object).unwrap();
    assert!(bytes_contains(&compressed_bytes, b"c2p0"));
    assert!(!bytes_contains(&uncompressed_bytes, b"c2p0"));

    let disassemble = |path: &Path| {
        let output = Command::new("llvm-objdump")
            .arg("--disassemble")
            .arg(path)
            .output()
            .expect("failed to start llvm-objdump");
        assert!(
            output.status.success(),
            "llvm-objdump failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
        String::from_utf8_lossy(&output.stdout).into_owned()
    };
    let compressed_disassembly = disassemble(&compressed_object);
    let uncompressed_disassembly = disassemble(&uncompressed_object);
    assert!(compressed_disassembly.contains("8082"));
    assert!(uncompressed_disassembly.contains("00008067"));
}

#[test]
fn waveos_boot_smoke_builds_windows_freestanding_coff_object() {
    let dir = temp_case_dir("waveos-boot-smoke-coff");
    let source =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/cases/windows/amd64/test1.wave");
    let object = dir.join("waveos_boot_smoke.obj");

    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-pc-windows-gnu"),
        OsStr::new("--freestanding"),
        OsStr::new("--emit=obj"),
        OsStr::new("-o"),
        object.as_os_str(),
    ]);

    let bytes = fs::read(&object).unwrap();
    assert!(
        bytes_contains(&bytes, &[0xB0, 0x45, 0xE6, 0xE9]),
        "COFF object must keep the embedded kernel byte array in .data"
    );
    assert!(
        bytes_contains(&bytes, b"embedded_kernel"),
        "COFF object must keep a relocatable embedded_kernel symbol"
    );
    assert!(
        bytes_contains(&bytes, &[0x41, 0xFF, 0xE3]),
        "jump_to_kernel must lower to an indirect jmp through r11"
    );
    assert!(
        !bytes_contains(&bytes, &[0x49, 0xC7, 0xC3, 0x00, 0x00, 0x20, 0x00]),
        "jump_to_kernel must not hard-code mov r11, 0x200000"
    );
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
#[test]
fn loongarch64_shared_language_workloads_run_under_qemu() {
    run_shared_language_workloads(
        "WAVE_RUN_LOONGARCH64_INTEROP_TESTS",
        "loongarch64-unknown-linux-gnu",
        "qemu-loongarch64",
    );
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-aarch64"))]
#[test]
fn aarch64_shared_language_workloads_run_under_qemu() {
    run_shared_language_workloads(
        "WAVE_RUN_AARCH64_INTEROP_TESTS",
        "aarch64-unknown-linux-gnu",
        "qemu-aarch64",
    );
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
#[test]
fn riscv64_shared_language_workloads_run_under_qemu() {
    run_shared_language_workloads(
        "WAVE_RUN_RISCV64_INTEROP_TESTS",
        "riscv64-unknown-linux-gnu",
        "qemu-riscv64",
    );
}

#[cfg(any(
    feature = "llvm-target-all",
    feature = "llvm-target-loongarch",
    feature = "llvm-target-aarch64",
    feature = "llvm-target-riscv"
))]
fn run_shared_language_workloads(flag: &str, target: &str, runner: &str) {
    if std::env::var_os(flag).is_none() {
        eprintln!("skipped: set {flag}=1 to run cross-architecture workloads");
        return;
    }
    let clang = clang_for_contract_tests().expect("clang 21 is required");
    let dir = temp_case_dir(&format!("shared-workloads-{target}"));
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let runtime = dir.join("runtime.o");
    let compile = Command::new(&clang)
        .args([
            "-target",
            target,
            "-O2",
            "-ffreestanding",
            "-fno-builtin",
            "-fno-stack-protector",
            "-c",
        ])
        .arg(root.join("tests/fixtures/linux_case_runtime/start.c"))
        .arg("-o")
        .arg(&runtime)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let arch = match target {
        "aarch64-unknown-linux-gnu" => "arm64",
        "riscv64-unknown-linux-gnu" => "riscv64",
        "loongarch64-unknown-linux-gnu" => "loong64",
        _ => panic!("unsupported workload target {target}"),
    };
    let cases = (104..=113)
        .map(|number| format!("shared/test{number}"))
        .chain((1..=10).map(|number| format!("shared/{arch}/test{number}")))
        .chain(
            (1..=if arch == "riscv64" {
                4
            } else if arch == "arm64" {
                2
            } else {
                1
            })
                .map(|number| format!("linux/{arch}/test{number}")),
        );
    for (source, optimization) in
        cases.flat_map(|source| ["-O0", "-O2"].map(|optimization| (source.clone(), optimization)))
    {
        let case = source.rsplit('/').next().unwrap();
        let output_dir = dir.join(format!("{}-{optimization}", source.replace('/', "-")));
        run_wavec([
            OsStr::new("build"),
            OsStr::new(optimization),
            root.join(format!("tests/cases/{source}.wave")).as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--emit=obj"),
            OsStr::new("--out-dir"),
            output_dir.as_os_str(),
        ]);
        let binary = output_dir.join(case);
        let link = Command::new(&clang)
            .args([
                "-target",
                target,
                "-fuse-ld=lld",
                "-nostdlib",
                "-static",
                "-Wl,-e,_start",
            ])
            .arg(&runtime)
            .arg(output_dir.join(format!("{case}.o")))
            .arg("-o")
            .arg(&binary)
            .output()
            .unwrap();
        assert!(
            link.status.success(),
            "{target}/{case}/{optimization}: {}",
            String::from_utf8_lossy(&link.stderr)
        );
        let mut child = Command::new(runner)
            .arg(&binary)
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .unwrap();
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(20);
        while child.try_wait().unwrap().is_none() {
            if std::time::Instant::now() >= deadline {
                child.kill().unwrap();
                child.wait().unwrap();
                panic!("{target}/{case}/{optimization}: timed out after 20 seconds");
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
        let output = child.wait_with_output().unwrap();
        assert!(
            output.status.success(),
            "{target}/{case}/{optimization}: {}\n{}\n{}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

#[test]
fn explicit_msvc_targets_emit_coff_and_link_without_mingw() {
    let dir = temp_case_dir("msvc-foundation");
    let source = write_wave(
        &dir,
        "entry.wave",
        r#"
export(c) fun entry() -> i32 { return 7; }
extern(system) fun native_api(value: i32) -> i32;
fun main() -> i32 { return 0; }
"#,
    );
    for (target, machine) in [
        ("x86_64-pc-windows-msvc", 0x8664u16),
        ("aarch64-pc-windows-msvc", 0xaa64u16),
    ] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        let output_dir = dir.join(target);
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--emit=obj"),
            OsStr::new("--out-dir"),
            output_dir.as_os_str(),
        ]);
        let object = output_dir.join("entry.o");
        let data = fs::read(&object).unwrap();
        assert_eq!(u16::from_le_bytes([data[0], data[1]]), machine);
        let executable = output_dir.join("entry.exe");
        let (plan, _) = run_wavec_capture([
            OsStr::new("build"),
            object.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--dry-run"),
            OsStr::new("--entry=entry"),
            OsStr::new("-Cno-default-libs"),
            OsStr::new("-o"),
            executable.as_os_str(),
        ]);
        assert!(
            plan.contains("lld-link")
                && plan.contains("/NODEFAULTLIB")
                && plan.contains("/ENTRY:entry"),
            "{plan}"
        );
        assert!(
            !plan.contains("mingw") && !plan.contains("-lmsvcrt"),
            "{plan}"
        );
        // This verifies a PE image, not execution on a Windows host.
        if Command::new("lld-link").arg("--version").output().is_ok() {
            run_wavec([
                OsStr::new("build"),
                object.as_os_str(),
                OsStr::new("--target"),
                OsStr::new(target),
                OsStr::new("--entry=entry"),
                OsStr::new("-Cno-default-libs"),
                OsStr::new("-o"),
                executable.as_os_str(),
            ]);
            let image = fs::read(&executable).unwrap();
            assert_eq!(&image[..2], b"MZ");
            let pe = u32::from_le_bytes(image[0x3c..0x40].try_into().unwrap()) as usize;
            assert_eq!(&image[pe..pe + 4], b"PE\0\0");
            assert_eq!(
                u16::from_le_bytes(image[pe + 4..pe + 6].try_into().unwrap()),
                machine
            );
        }
    }
}

#[cfg(target_os = "linux")]
#[test]
fn async_tasks_suspend_resume_cancel_and_exchange_tcp_data() {
    let available = run_wavec_capture(["print", "target-list"]).0;
    let arch = std::env::consts::ARCH;
    if !available.lines().any(|t| t.starts_with(arch)) {
        return;
    }
    let dir = temp_case_dir("async-runtime");
    let home = dir.join("home");
    copy_tree(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("std"),
        &home.join(".wave/lib/wave/std"),
    );
    for name in [
        "lazy_nested",
        "control_flow",
        "timer_fairness",
        "tcp_roundtrip",
    ] {
        let source = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join(format!("tests/fixtures/async/{name}.wave"));
        let output = dir.join(name);
        let compiled = wavec_command()
            .env("HOME", &home)
            .arg("build")
            .arg(source)
            .arg("-o")
            .arg(&output)
            .output()
            .unwrap();
        assert!(
            compiled.status.success(),
            "{name}: {}",
            String::from_utf8_lossy(&compiled.stderr)
        );
        let mut child = Command::new(&output).spawn().unwrap();
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(15);
        loop {
            if let Some(status) = child.try_wait().unwrap() {
                assert!(status.success(), "{name}: {status}");
                break;
            }
            if std::time::Instant::now() >= deadline {
                child.kill().unwrap();
                child.wait().unwrap();
                panic!("{name} did not complete");
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
    }
    fs::remove_dir_all(dir).unwrap();
}

#[test]
fn async_frames_emit_for_enabled_native_targets() {
    let dir = temp_case_dir("async-targets");
    let home = dir.join("home");
    copy_tree(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("std"),
        &home.join(".wave/lib/wave/std"),
    );
    let source =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/async/control_flow.wave");
    let available = run_wavec_capture(["print", "target-list"]).0;
    for target in [
        "x86_64-unknown-linux-gnu",
        "aarch64-unknown-linux-gnu",
        "riscv64-unknown-linux-gnu",
        "loongarch64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-pc-windows-msvc",
        "x86_64-apple-darwin",
        "aarch64-apple-darwin",
        "x86_64-unknown-freebsd",
    ] {
        if !available.lines().any(|t| t == target) {
            continue;
        }
        let output = dir.join(format!("{target}.o"));
        let compiled = wavec_command()
            .env("HOME", &home)
            .arg("build")
            .arg(&source)
            .args(["--target", target, "--emit=obj", "-o"])
            .arg(&output)
            .output()
            .unwrap();
        assert!(
            compiled.status.success(),
            "{target}: {}",
            String::from_utf8_lossy(&compiled.stderr)
        );
        assert!(output.metadata().unwrap().len() > 0);
    }
    fs::remove_dir_all(dir).unwrap();
}
