// SPDX-License-Identifier: MPL-2.0
// Shared test helpers retained at the parent module so all suites use the
// same fixture isolation, target gates, process timeouts and CLI helpers.
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
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
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
    let dir =
        std::env::temp_dir().join(format!("wavec-{}-{}-{}", name, std::process::id(), sequence));
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
    if !native_target_enabled() {
        command.args(["--target", frontend_target()]);
    }
    command
}

fn native_target_enabled() -> bool {
    cfg!(any(
        feature = "llvm-target-all",
        all(target_arch = "x86_64", feature = "llvm-target-x86"),
        all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
        all(target_arch = "riscv64", feature = "llvm-target-riscv"),
        all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
    ))
}

fn frontend_target() -> &'static str {
    [
        "x86_64-unknown-linux-gnu",
        "aarch64-unknown-linux-gnu",
        "riscv64-unknown-linux-gnu",
        "loongarch64-unknown-linux-gnu",
        "wasm64-unknown-unknown",
    ]
    .into_iter()
    .find(|target| llvm::codegen::target::target_spec_for_triple(target).is_some())
    .expect("at least one LLVM backend must be enabled for codegen regressions")
}

// Execute with the public host default, including MSVC on Windows.
#[cfg(any(
    feature = "llvm-target-all",
    all(target_arch = "x86_64", feature = "llvm-target-x86"),
    all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
    all(target_arch = "riscv64", feature = "llvm-target-riscv"),
    all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
))]
fn native_wave_command(source: &Path) -> Command {
    let mut command = wavec_command();
    command.arg("run").arg(source);
    command
}

#[cfg(any(
    feature = "llvm-target-all",
    all(target_arch = "x86_64", feature = "llvm-target-x86"),
    all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
    all(target_arch = "riscv64", feature = "llvm-target-riscv"),
    all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
))]
fn run_native_wave(source: &Path) -> (String, String) {
    let output = native_wave_command(source).output().unwrap();
    assert!(
        output.status.success(),
        "native execution failed: {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    (
        String::from_utf8_lossy(&output.stdout).into_owned(),
        String::from_utf8_lossy(&output.stderr).into_owned(),
    )
}

fn run_shared_case_at_both_optimization_levels(case: &str) -> String {
    let source = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/cases/shared")
        .join(format!("{case}.wave"));
    let dir = temp_case_dir(case);
    for optimization in ["-O0", "-O2"] {
        let mut command = wavec_command();
        command
            .arg("build")
            .arg(&source)
            .arg(optimization)
            .arg(if native_target_enabled() { "--emit=ir,obj,bin" } else { "--emit=ir,obj" })
            .arg("--out-dir")
            .arg(dir.join(optimization));
        if native_target_enabled() {
            command.arg("--run");
        }
        let output = command.output().unwrap();
        assert!(output.status.success(), "{case}: {}", String::from_utf8_lossy(&output.stderr));
    }
    fs::read_to_string(dir.join("-O0").join(format!("{case}.ll"))).unwrap()
}










"#,
    )
    .unwrap();
    let source = write_wave(
        &dir,
        "partial.wave",
        r#"
import("std::random::fill")::{RandomFillResult, random_fill};
fun main() -> i32 {
    var data: array<u8, 5> = [85, 85, 85, 85, 85];
    var result: RandomFillResult = random_fill(&data[0], 5);
    if (result.ok || result.written != 3 || result.error != -5) { return 1; }
    if (data[0] != 31 || data[1] != 32 || data[2] != 33 || data[3] != 85 || data[4] != 85) { return 2; }
    return 0;
}
"#,
    );
    let output = wavec_command()
        .env("HOME", &home)
        .arg("build")
        .arg(source)
        .arg("--run")
        .arg("--out-dir")
        .arg(dir.join("partial"))
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
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

#[cfg(any(
    feature = "llvm-target-all",
    feature = "llvm-target-riscv",
    feature = "llvm-target-x86"
))]
fn bytes_contains(haystack: &[u8], needle: &[u8]) -> bool {
    haystack.windows(needle.len()).any(|window| window == needle)
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
fn riscv64_elf_flags(path: &Path) -> u32 {
    let object = fs::read(path).unwrap();
    assert!(object.len() >= 52, "truncated ELF object: {}", path.display());
    assert_eq!(&object[..4], b"\x7fELF", "{}", path.display());
    assert_eq!(object[4], 2, "expected ELF64 object: {}", path.display());
    assert_eq!(u16::from_le_bytes([object[18], object[19]]), 243);
    u32::from_le_bytes([object[48], object[49], object[50], object[51]])
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
fn loongarch64_elf_flags(path: &Path) -> u32 {
    let object = fs::read(path).unwrap();
    assert!(object.len() >= 52, "truncated ELF object: {}", path.display());
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

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
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



"#,
    );
    let mapping = OsString::from(format!("add={}", package.display()));
    run_wavec([OsStr::new("check"), entry.as_os_str(), OsStr::new("--dep"), mapping.as_os_str()]);

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
    assert!(error.contains("symbol 'internal_sum' is private in module 'add'"), "{}", error);

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
    assert!(error.contains("symbol 'internal_sum' is private in module 'add'"), "{}", error);
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

    run_wavec([
        OsStr::new("build"),
        entry.as_os_str(),
        OsStr::new("--emit=ir,obj"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]);
    #[cfg(any(
        feature = "llvm-target-all",
        all(target_arch = "x86_64", feature = "llvm-target-x86"),
        all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
        all(target_arch = "riscv64", feature = "llvm-target-riscv"),
        all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
    ))]
    run_native_wave(&entry);
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
        assert!(ir.contains(instruction), "unsigned integers must use {instruction}:\n{ir}");
    }
    assert!(ir.contains("zext i1"), "boolean comparison results must normalize true to one:\n{ir}");
    assert!(
        ir.contains("lshr i16"),
        "nested u16 bitwise expressions must retain unsigned shift semantics:\n{ir}"
    );
    for instruction in ["sdiv i32", "srem i32", "icmp sgt i32"] {
        assert!(ir.contains(instruction), "signed integers must use {instruction}:\n{ir}");
    }
    #[cfg(any(
        feature = "llvm-target-all",
        all(target_arch = "x86_64", feature = "llvm-target-x86"),
        all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
        all(target_arch = "riscv64", feature = "llvm-target-riscv"),
        all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
    ))]
    run_native_wave(&source);

    let invalid_literal = write_wave(
        &dir,
        "invalid_literal.wave",
        "fun mask(value: u16) -> u16 { return value & 0x10000; }\nfun main() {}\n",
    );
    let error = run_wavec_expect_failure([OsStr::new("check"), invalid_literal.as_os_str()]);
    assert!(error.contains("integer literal `0x10000` does not fit `u16`"), "{error}");
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
    assert!(ir.contains("zext i8"), "unsigned widening must use zext:\n{ir}");
    assert!(ir.contains("sext i8"), "signed widening must use sext:\n{ir}");
    assert!(
        ir.contains("ret i64 9221120237041090560"),
        "an explicitly cast wide literal must retain all target bits:\n{ir}"
    );
    #[cfg(any(
        feature = "llvm-target-all",
        all(target_arch = "x86_64", feature = "llvm-target-x86"),
        all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
        all(target_arch = "riscv64", feature = "llvm-target-riscv"),
        all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
    ))]
    run_native_wave(&source);
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
    #[cfg(any(
        feature = "llvm-target-all",
        all(target_arch = "x86_64", feature = "llvm-target-x86"),
        all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
        all(target_arch = "riscv64", feature = "llvm-target-riscv"),
        all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
    ))]
    run_native_wave(&source);
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
    assert!(ir.contains("idx_zext"), "unsigned array index must zero-extend:\n{ir}");
    for (function, width) in [("add_offset", 8), ("add_offset_left", 8), ("add_offset_u32", 32)] {
        let body = ir
            .split(&format!("define ptr @{function}("))
            .nth(1)
            .unwrap()
            .split("\n}")
            .next()
            .unwrap();
        assert!(
            body.lines()
                .any(|line| line.contains(&format!("zext i{width} ")) && line.contains(" to i64"))
                && body.contains("getelementptr inbounds i8"),
            "{function} must zero-extend its unsigned offset before GEP:\n{body}"
        );
    }
    #[cfg(any(
        feature = "llvm-target-all",
        all(target_arch = "x86_64", feature = "llvm-target-x86"),
        all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
        all(target_arch = "riscv64", feature = "llvm-target-riscv"),
        all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
    ))]
    run_native_wave(&source);
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
    #[cfg(any(
        feature = "llvm-target-all",
        all(target_arch = "x86_64", feature = "llvm-target-x86"),
        all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
        all(target_arch = "riscv64", feature = "llvm-target-riscv"),
        all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
    ))]
    run_native_wave(&source);
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
    let mut child = native_wave_command(&source)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    child.stdin.take().unwrap().write_all(b"73\n").unwrap();
    let output = child.wait_with_output().unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
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
    let mut targets = vec![(frontend_target(), 64)];
    if cfg!(any(feature = "llvm-target-all", feature = "llvm-target-wasm")) {
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
            let body =
                ir.split(&format!("@{function}(")).nth(1).unwrap().split('}').next().unwrap();
            assert!(
                body.contains(&format!("{instruction} {operand}"))
                    && body.contains(&format!("to i{width}")),
                "{target}: {body}"
            );
        }
        let body = ir.split("@word_unsigned(").nth(1).unwrap().split('}').next().unwrap();
        assert_eq!(body.contains("zext i32"), width == 64, "{target}: {body}");
        let body = ir.split("@wide_unsigned(").nth(1).unwrap().split('}').next().unwrap();
        assert_eq!(body.contains("trunc i64"), width == 32, "{target}: {body}");
    }
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
    assert!(ir.contains("icmp eq i64"), "numeric comparisons must retain the operand width:\n{ir}");
    #[cfg(any(
        feature = "llvm-target-all",
        all(target_arch = "x86_64", feature = "llvm-target-x86"),
        all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
        all(target_arch = "riscv64", feature = "llvm-target-riscv"),
        all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
    ))]
    run_native_wave(&source);
}




fn run_link_tests_enabled() -> bool {
    std::env::var_os("WAVE_RUN_LINK_TESTS").is_some()
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
            assert!(stderr.contains("error[E3001]"), "{} ({}): {}", file_name, mode, stderr);
            assert!(stderr.contains(expected), "{} ({}): {}", file_name, mode, stderr);
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
    assert!(stderr.contains("struct Item { second: i32; }"), "{}", stderr);

    let imported = dir.join("broken.wave");
    fs::write(&imported, "fun broken() {\n    var value: Missing = 1;\n}\n").unwrap();
    let entry = write_wave(&dir, "import_main.wave", "import(\"./broken\");\n\nfun main() {}\n");
    let output = run_wavec_raw([OsStr::new("check"), entry.as_os_str()]);
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("broken.wave:2:16"), "{}", stderr);
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
    assert!(stderr.contains("generic_broken.wave:3:16"), "{}", stderr);
    assert!(stderr.contains("type `Box` expects 1 generic argument(s), found 2"), "{}", stderr);
    assert!(!stderr.contains("generic monomorphization failed"), "{}", stderr);
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
    assert!(plan.contains("\"target\":\"wasm32-unknown-unknown\""), "{plan}");
    assert!(plan.contains("\"program\":") && plan.contains("wasm-ld"), "{plan}");
    assert!(plan.contains("--no-entry"), "{plan}");
    assert!(plan.contains("--allow-undefined"), "{plan}");
    assert!(plan.contains("--export-if-defined=main"), "{plan}");
    assert!(plan.contains("--export-memory"), "{plan}");
    assert!(
        plan.contains(&json_string_for_test(&out_dir.join("module.wasm").to_string_lossy())),
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
    let parsed = utils::wson::parse_json(&wasm64_plan).unwrap();
    let Some(utils::wson::Value::Array(args)) = parsed.get("execute").unwrap().get("args") else {
        panic!("missing WebAssembly host arguments: {wasm64_plan}");
    };
    assert!(
        !args.contains(&utils::wson::Value::string("--experimental-wasm-memory64")),
        "memory64 host flags must be selected at execution time: {wasm64_plan}"
    );

    let asm_source =
        write_wave(&dir, "inline_asm.wave", "fun main() {\n    asm {\n        \"nop\"\n    }\n}\n");
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

"#,
    );
    let expected = "한글 text A -42 340282366920938463463374607431768211455 -0.000000 0.007812 100% host puts\n";
    for target in ["wasm32-unknown-unknown", "wasm32-wasip1", "wasm64-unknown-unknown"] {
        for optimization in ["-O0", "-O2"] {
            let output = wavec_command()
                .arg("build")
                .arg(&source)
                .args(["--target", target, optimization, "--run", "--std-root"])
                .arg(Path::new(env!("CARGO_MANIFEST_DIR")).join("std"))
                .arg("--out-dir")
                .arg(dir.join(target))
                .output()
                .unwrap();
            assert_eq!(
                output.status.code(),
                Some(7),
                "{target} {optimization}: {}",
                String::from_utf8_lossy(&output.stderr)
            );
            assert_eq!(output.stdout, expected.as_bytes(), "{target} {optimization}");
        }
    }
    fs::remove_dir_all(dir).unwrap();
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
    assert!(host_ir.contains("\"wasm-export-name\"=\"wave_transform\""), "{host_ir}");

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
    assert!(wasi_ir.contains("target triple = \"wasm32-wasip1\""), "{wasi_ir}");
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

"#,
    );
    let out_dir = dir.join("out");
    let (stdout, stderr) = run_wavec_capture([
        OsStr::new("--error-format=json"),
        OsStr::new("build"),
        src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new(frontend_target()),
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
    assert!(plan.contains(&format!("\"target\":\"{}\"", frontend_target())), "{}", plan);
    let parsed = utils::wson::parse_json(plan).unwrap();
    let spec = llvm::codegen::target::target_spec_for_triple(frontend_target()).unwrap();
    let options = llvm::codegen::target::resolve_target_options(spec, None, None, None).unwrap();
    assert_eq!(parsed.get_str("cpu"), Some(options.cpu.as_str()));
    assert_eq!(parsed.get_str("features"), Some(options.features.as_str()));
    assert_eq!(parsed.get_str("abi"), options.abi.as_deref());
    assert_eq!(parsed.get_str("isa"), options.isa.as_deref());
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
    assert!(plan.contains("\"emit_kinds\":[\"obj\",\"bin\"]"), "{}", plan);
    assert!(
        plan.contains(&format!("\"output\":{}", json_string_for_test(&binary.to_string_lossy()))),
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

    let mut run_command = wavec_command();
    if !native_target_enabled() && frontend_target() == "wasm64-unknown-unknown" {
        run_command.args(["--target", "wasm32-wasip1"]);
    }
    let run_plan = run_command
        .args([
            OsStr::new("--error-format=json"),
            OsStr::new("build"),
            src.as_os_str(),
            OsStr::new("--run"),
            OsStr::new("--dry-run"),
            OsStr::new("--"),
            OsStr::new("arg one"),
            OsStr::new("--flag"),
        ])
        .output()
        .unwrap();
    assert!(
        run_plan.status.success(),
        "run dry-run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run_plan.stdout),
        String::from_utf8_lossy(&run_plan.stderr)
    );
    let stdout = String::from_utf8_lossy(&run_plan.stdout);
    assert!(stdout.contains("\"mode\":\"build+run\""), "{}", stdout);
    assert!(stdout.contains("\"execute\":{"), "{}", stdout);
    let plan = utils::wson::parse_json(&stdout).unwrap();
    let expected = [utils::wson::Value::string("arg one"), utils::wson::Value::string("--flag")];
    assert_eq!(plan.get_arr("run_args"), Some(expected.as_slice()));
    let execute_args = plan.get("execute").unwrap().get_arr("args").unwrap();
    assert!(execute_args.ends_with(&expected), "{stdout}");

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
            &["-C", "relocation-model=pie", "build", "main.wave", "--no-pie", "--dry-run"],
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
        let full_args =
            std::iter::once("--error-format=json").chain(args.iter().copied()).map(OsStr::new);
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
        assert!(stderr.contains("\"kind\":\"usage\""), "{}: {}", name, stderr);
        // WebAssembly rejects native link modes before checking their mutual
        // conflicts. Keep the native conflict assertions on native backends,
        // and check the documented unsupported-mode diagnostic on wasm-only.
        let needles = if !native_target_enabled()
            && frontend_target() == "wasm64-unknown-unknown"
            && (args.contains(&"--shared") || args.contains(&"--no-pie"))
        {
            &["--shared", "--static", "--pie/--no-pie", "not supported for WebAssembly"][..]
        } else {
            *needles
        };
        for needle in needles {
            assert!(stderr.contains(needle), "{} missing '{}': {}", name, needle, stderr);
        }
    }
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
        OsStr::new(frontend_target()),
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

    if run_link_tests_enabled() && native_target_enabled() {
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
        ir.lines()
            .any(|line| line.starts_with("define i64 @wave_take(ptr ") && !line.contains("byval")),
        "{}",
        ir
    );
    assert!(ir.contains("define void @wave_make(ptr sret(%Triple) align 8"), "{}", ir);
    assert!(ir.contains("@__wave_export_impl_wave_take"), "{}", ir);
    assert!(ir.contains("@__wave_export_impl_wave_make"), "{}", ir);
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

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
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
    assert!(error.contains("variadic argument 2") && error.contains("no scalar type"), "{error}");
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





#[cfg(any(
    feature = "llvm-target-all",
    feature = "llvm-target-x86",
    feature = "llvm-target-aarch64",
    feature = "llvm-target-riscv"
))]
fn run_linux_c_abi_fixture(
    fixture_name: &str,
    target: &str,
    c_compiler: &str,
    runner: Option<&str>,
) {
    run_linux_c_abi_fixture_at_optimization(fixture_name, target, c_compiler, runner, "-O0");
}

#[cfg(any(
    feature = "llvm-target-all",
    feature = "llvm-target-x86",
    feature = "llvm-target-aarch64",
    feature = "llvm-target-riscv"
))]
fn run_linux_c_abi_fixture_at_optimization(
    fixture_name: &str,
    target: &str,
    c_compiler: &str,
    runner: Option<&str>,
    optimization: &str,
) {
    let dir = temp_case_dir(&format!("{fixture_name}-c-abi-interop"));
    let fixture_dir =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests").join("fixtures").join(fixture_name);
    let c_object = dir.join("interop-c.o");
    let wave_out = dir.join("wave");
    let binary = dir.join("interop");

    let c_compile = Command::new(c_compiler)
        .args(["-O2", "-ffreestanding", "-fno-builtin", "-fno-stack-protector", "-c"])
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
        OsStr::new(optimization),
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
    assert!(bytes.len() >= 0x40 && &bytes[..2] == b"MZ", "invalid PE file");
    let pe_offset = u32::from_le_bytes(bytes[0x3c..0x40].try_into().unwrap()) as usize;
    assert!(
        bytes.len() >= pe_offset + 6 && &bytes[pe_offset..pe_offset + 4] == b"PE\0\0",
        "invalid PE signature"
    );
    u16::from_le_bytes(bytes[pe_offset + 4..pe_offset + 6].try_into().unwrap())
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
        Command::new("qemu-loongarch64").arg("--version").output().is_ok(),
        "qemu-loongarch64 is required for LoongArch ABI tests"
    );

    let fixture =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures").join(fixture_name);
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
        assert!(wave_ir.contains(*contract), "missing `{contract}`:\n{wave_ir}");
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
    assert!(compile.status.success(), "{}", String::from_utf8_lossy(&compile.stderr));
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
            .args(["-target", target, "-fuse-ld=lld", "-nostdlib", "-static", "-Wl,-e,_start"])
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
