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

use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn wavec_bin() -> PathBuf {
    if let Some(path) = option_env!("CARGO_BIN_EXE_wavec") {
        return PathBuf::from(path);
    }

    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("target/debug/wavec")
}

fn temp_case_dir(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("wavec-{}-{}", name, std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    dir
}

fn write_wave(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(name);
    fs::write(&path, source).unwrap();
    path
}

fn run_wavec<I, S>(args: I)
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let output = Command::new(wavec_bin()).args(args).output().unwrap();
    assert!(
        output.status.success(),
        "wavec failed with status {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn run_wavec_expect_failure<I, S>(args: I) -> String
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let output = Command::new(wavec_bin()).args(args).output().unwrap();
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

fn bytes_contains(haystack: &[u8], needle: &[u8]) -> bool {
    haystack
        .windows(needle.len())
        .any(|window| window == needle)
}

fn run_link_tests_enabled() -> bool {
    std::env::var_os("WAVE_RUN_LINK_TESTS").is_some()
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
    let mut x: i32 = 1;
    write_deref(&x, 41);
    if (x != 41) {
        return 1;
    }

    let mut arr: array<i32, 3> = [1, 2, 3];
    write_index(&arr[0], 9);
    if (arr[1] != 9) {
        return 2;
    }

    let mut pair: Pair = Pair { a: 7, b: 8 };
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

    let mut array_ptr_target: array<i32, 3> = [4, 5, 6];
    write_array_pointer(&array_ptr_target, 23);
    if (array_ptr_target[1] != 23) {
        return 6;
    }

    let mut y: i32 = 88;
    let mut pointer_box: PointerBox = PointerBox { data: &x };
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
fn freestanding_codegen_marks_functions_no_red_zone() {
    let dir = temp_case_dir("freestanding-noredzone");
    let src = write_wave(
        &dir,
        "leaf.wave",
        r#"
fun leaf(a: i64, b: i64, c: i64, d: i64, e: i64) -> i64 {
    let x: i64 = a + b;
    let y: i64 = c + d;
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
fun main() -> i64 {
    let x: i64 = asm {
        "jmp rax"
        in("rax") 0
        clobber("noreturn")
    };
    return x;
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
    let x: i64 = 1;
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
fn waveos_boot_smoke_builds_windows_freestanding_coff_object() {
    let dir = temp_case_dir("waveos-boot-smoke-coff");
    let source = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test/test108.wave");
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
