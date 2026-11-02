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
    #[cfg(any(
        feature = "llvm-target-all",
        all(target_arch = "x86_64", feature = "llvm-target-x86"),
        all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
        all(target_arch = "riscv64", feature = "llvm-target-riscv"),
        all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
    ))]
    run_native_wave(&source);
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
