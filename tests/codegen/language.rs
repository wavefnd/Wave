// SPDX-License-Identifier: MPL-2.0
//! Language semantics regression cases.
//! These tests share the parent integration suite's fixtures and target gates.
use super::*;

#[test]
fn typed_pointer_expression_strides_execute_at_o0_and_o2() {
    let ir = run_shared_case_at_both_optimization_levels("test120");
    for instruction in [
        "getelementptr inbounds i32, ptr %call_counted",
        "getelementptr inbounds i32, ptr %method_call",
        "getelementptr inbounds %Pair, ptr %ptr_gep",
    ] {
        assert!(ir.contains(instruction), "missing {instruction}:\n{ir}");
    }
}

#[test]
fn shadowing_initializers_execute_at_o0_and_o2() {
    run_shared_case_at_both_optimization_levels("test121");
}

#[test]
fn locals_shadow_global_constants_at_o0_and_o2() {
    run_shared_case_at_both_optimization_levels("test122");
}

#[test]
fn contextual_float_signedness_executes_at_o0_and_o2() {
    let ir = run_shared_case_at_both_optimization_levels("test123");
    assert!(ir.contains("fptoui double"), "{ir}");
    assert!(ir.contains("fptosi double"), "{ir}");
}

#[test]
fn untyped_integer_truthiness_preserves_nonzero_values_at_o0_and_o2() {
    run_shared_case_at_both_optimization_levels("test140");
}

#[test]
fn fixed_array_returns_preserve_caller_observed_values_at_o0_and_o2() {
    let ir = run_shared_case_at_both_optimization_levels("test141");
    assert!(ir.contains("ret_array = load [4 x i8]"), "{ir}");
    assert!(ir.contains("load [2 x [2 x i32]]"), "{ir}");
    assert!(ir.contains("load [0 x i8]"), "{ir}");
}

#[test]
fn compact_variants_preserve_constants_statics_and_nested_payloads() {
    run_shared_case_at_both_optimization_levels("test126");
    run_shared_case_at_both_optimization_levels("test87");
    let dir = temp_case_dir("compact-variant-targets");
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/cases/shared/test126.wave");
    for target in [
        "x86_64-unknown-linux-gnu",
        "aarch64-unknown-linux-gnu",
        "riscv64-unknown-linux-gnu",
        "wasm32-unknown-unknown",
        "wasm64-unknown-unknown",
    ] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--emit=ir,obj"),
            OsStr::new("--out-dir"),
            dir.join(target).as_os_str(),
        ]);
        let ir = fs::read_to_string(dir.join(target).join("test126.ll")).unwrap();
        let layout = ir.lines().find(|line| line.starts_with("%variant.Payload =")).unwrap();
        assert!(layout.contains("[32 x i8]") && layout.contains("[0 x"), "{target}: {layout}");
    }
}

#[test]
#[cfg(any(
    feature = "llvm-target-all",
    all(target_arch = "x86_64", feature = "llvm-target-x86"),
    all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
    all(target_arch = "riscv64", feature = "llvm-target-riscv"),
    all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
))]
fn uleb128_cursors_preserve_state_on_failure() {
    let dir = temp_case_dir("uleb128-cursors");
    let home = dir.join("home");
    copy_tree(&Path::new(env!("CARGO_MANIFEST_DIR")).join("std"), &home.join(".wave/lib/wave/std"));
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/cases/shared/test125.wave");
    for optimization in ["-O0", "-O2"] {
        let output = wavec_command()
            .env("HOME", &home)
            .arg("build")
            .arg(&source)
            .arg(optimization)
            .arg("--run")
            .arg("--out-dir")
            .arg(dir.join(optimization))
            .output()
            .unwrap();
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    }
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
    assert!(ir.contains("lshr i32"), "unsigned shift must use lshr:\n{ir}");
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

#[test]
#[cfg(any(
    feature = "llvm-target-all",
    all(target_arch = "x86_64", feature = "llvm-target-x86"),
    all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
    all(target_arch = "riscv64", feature = "llvm-target-riscv"),
    all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
))]
fn conundrum_example_finishes_its_gallery() {
    let source =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/applications/conundrum.wave");
    let (stdout, stderr) = run_native_wave(&source);
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

#[test]
#[cfg(any(
    feature = "llvm-target-all",
    all(target_arch = "x86_64", feature = "llvm-target-x86"),
    all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
    all(target_arch = "riscv64", feature = "llvm-target-riscv"),
    all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
))]
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

#[test]
fn projected_deref_preserves_pointer_storage_types() {
    let source = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/cases/shared/test103.wave");
    let dir = temp_case_dir("projected-pointer-loads");
    // Use the advertised targets so this regression also runs in isolated
    // backend builds and covers WebAssembly's 32-bit pointer representation.
    let (targets, _) = run_wavec_capture(["print", "target-list"]);
    for target in targets.lines().map(str::trim).filter(|line| !line.is_empty()) {
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
            let body = ir.split(&format!("@{name}(")).nth(1).unwrap().split("\n}").next().unwrap();
            assert!(body.contains("load ptr"), "{target} {name}: {body}");
            assert!(!body.contains("load i8"), "{target} {name}: {body}");
        }
        for name in ["widen_field", "widen_scalar"] {
            let body = ir.split(&format!("@{name}(")).nth(1).unwrap().split("\n}").next().unwrap();
            assert!(body.contains("load i8"), "{target} {name}: {body}");
            assert!(!body.contains("load i64"), "{target} {name}: {body}");
        }
    }
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-x86"))]
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

#[test]
fn phase1_alias_and_literal_byte_cases_execute_at_o0_and_o2() {
    run_shared_case_at_both_optimization_levels("test137");
    let ir = run_shared_case_at_both_optimization_levels("test138");
    assert!(ir.contains(r"\7F\80\FF\C3\A9\00"), "{ir}");
}

#[test]
#[cfg(any(
    feature = "llvm-target-all",
    all(target_arch = "x86_64", feature = "llvm-target-x86"),
    all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
    all(target_arch = "riscv64", feature = "llvm-target-riscv"),
    all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
))]
fn phase1_imported_aliases_preserve_access_at_o0_and_o2() {
    let dir = temp_case_dir("phase1-imported-aliases");
    write_wave(
        &dir,
        "types.wave",
        r#"
        pub struct Pair { value: i32; }
        pub type Values = array<i32, 2>;
        pub type Pointer = ptr<i32>;
        pub type Record = Pair;
    "#,
    );
    let source = write_wave(
        &dir,
        "imported_aliases.wave",
        r#"
        import("./types")::{Pair, Values, Pointer, Record};
        type MoreValues = Values;
        type MorePointer = Pointer;
        type MoreRecord = Record;
        fun main() -> i32 {
            var values: MoreValues = [10, 20];
            values[1] = 21;
            var p: MorePointer = &values[0]; deref p = 11; p[1] = 22;
            var record: MoreRecord = Pair { value: 1 }; record.value = 3;
            if (values[0] != 11 || values[1] != 22 || record.value != 3) { return 1; }
            return 0;
        }
    "#,
    );
    for opt in ["-O0", "-O2"] {
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new(opt),
            OsStr::new("--run"),
            OsStr::new("--out-dir"),
            dir.join(opt).as_os_str(),
        ]);
    }
}

#[test]
#[cfg(any(
    feature = "llvm-target-all",
    all(target_arch = "x86_64", feature = "llvm-target-x86"),
    all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
    all(target_arch = "riscv64", feature = "llvm-target-riscv"),
    all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
))]
fn phase1_literal_printing_preserves_non_utf8_bytes() {
    let dir = temp_case_dir("phase1-byte-output");
    let source = write_wave(
        &dir,
        "literal_bytes.wave",
        r#"fun main() { print("\xFF"); println("é\x80 {}", 42); }"#,
    );
    for opt in ["-O0", "-O2"] {
        let output = run_wavec_raw([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new(opt),
            OsStr::new("--run"),
            OsStr::new("--out-dir"),
            dir.join(opt).as_os_str(),
        ]);
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        let expected: &[u8] =
            if cfg!(windows) { b"\xff\xc3\xa9\x80 42\r\n" } else { b"\xff\xc3\xa9\x80 42\n" };
        assert_eq!(output.stdout, expected);
    }
}

#[test]
fn addressed_array_literals_preserve_contextual_storage_at_o0_and_o2() {
    run_shared_case_at_both_optimization_levels("test73");
    let source =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/addressed_arrays/main.wave");
    let directory = temp_case_dir("addressed-array-conversions");
    for optimization in ["-O0", "-O2"] {
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new(optimization),
            OsStr::new(if native_target_enabled() { "--run" } else { "--emit=ir,obj" }),
            OsStr::new("--out-dir"),
            directory.join(optimization).as_os_str(),
        ]);
    }
}
