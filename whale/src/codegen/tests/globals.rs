// SPDX-License-Identifier: MPL-2.0
use super::module;
use whale_ir::*;

fn run(source: &str) -> Option<ConstValue> {
    let ir = module(source);
    super::interpret_checked(
        &ir,
        ir.functions[0].id,
        &[],
        InterpreterOptions { max_steps: 2000, ..InterpreterOptions::default() },
    )
    .unwrap()
    .value
}

#[test]
fn global_scalar_constants_use_hir_dependency_values_and_ordered_casts() {
    let source = r#"
const LIMIT: i32 = NARROWED + 4;
const NARROWED: i32 = (SOURCE as u8) as i32;
const SOURCE: i32 = 257;
const FLAG: bool = LIMIT != 0;
fun probe() -> i32 {
    if (FLAG) { return LIMIT; }
    return 0;
}
"#;
    assert_eq!(run(source), Some(ConstValue::I(5)));
    let ir = module(source);
    assert!(!ir.functions[0]
        .blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .any(|i| { matches!(i, Instruction::Bin { .. } | Instruction::Cast { .. }) }));
}

#[test]
fn global_integer_constants_preserve_every_supported_width_and_signedness() {
    for ty in ["i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "byte"] {
        let source = format!("const VALUE: {ty} = 7; fun probe() -> {ty} {{ return VALUE; }}");
        let expected = if ty.starts_with('i') { ConstValue::I(7) } else { ConstValue::U(7) };
        assert_eq!(run(&source), Some(expected), "{ty}");
    }
    assert_eq!(
        run("const VALUE: i8 = -128; fun probe() -> i32 { return VALUE as i32; }"),
        Some(ConstValue::I(-128))
    );
    assert_eq!(run("const VALUE: u128 = 340282366920938463463374607431768211455; fun probe() -> u128 { return VALUE; }"), Some(ConstValue::U(u128::MAX)));
}

#[test]
fn local_bindings_shadow_globals_after_their_initializer() {
    let source = r#"
type Number = i32;
const VALUE: Number = 7;
fun probe() -> i32 {
    var VALUE: Number = VALUE + 1;
    if (true) {
        var VALUE: Number = VALUE + 2;
        if (VALUE != 10) { return -1; }
    }
    VALUE += 3;
    return VALUE;
}
"#;
    assert_eq!(run(source), Some(ConstValue::I(11)));
}

#[test]
fn floating_global_constants_retain_typed_rounding_and_signed_zero_in_ir() {
    for (ty, value, expected) in [
        ("f32", "16777217.0", FloatBits::F32(16777216f32.to_bits())),
        ("f64", "16777217.0", FloatBits::F64(16777217f64.to_bits())),
        ("f32", "-0.0", FloatBits::F32((-0.0f32).to_bits())),
        ("f64", "-0.0", FloatBits::F64((-0.0f64).to_bits())),
    ] {
        let ir = module(&format!(
            "const VALUE: {ty} = {value}; fun probe() -> {ty} {{ return VALUE; }}"
        ));
        assert!(ir.functions[0].blocks[0].instructions.iter().any(|i| {
            matches!(i, Instruction::Const { value: ConstValue::F(bits), .. } if *bits == expected)
        }), "{ty} {value}");
    }
}
