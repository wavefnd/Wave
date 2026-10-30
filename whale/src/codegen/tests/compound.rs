// SPDX-License-Identifier: MPL-2.0
use super::{mask, module};
use whale_ir::*;

fn run(module: &Module, arguments: &[ConstValue]) -> Option<ConstValue> {
    super::interpret_checked(
        module,
        module.functions[0].id,
        arguments,
        InterpreterOptions { max_steps: 2000, ..InterpreterOptions::default() },
    )
    .unwrap()
    .value
}

#[test]
fn compound_integer_arithmetic_wraps_and_returns_the_stored_value() {
    for bits in [8, 16, 32, 64, 128] {
        for signed in [false, true] {
            let ty = format!("{}{bits}", if signed { "i" } else { "u" });
            for (operator, initial, rhs, expected) in [
                ("+=", mask(bits), 2, 1),
                ("-=", 0, 1, mask(bits)),
                ("*=", mask(bits), 3, mask(bits) - 2),
            ] {
                let ir = module(&format!(
                    "fun probe(n: {ty}, rhs: {ty}) -> {ty} {{ var x: {ty} = n; \
                     var result: {ty} = x {operator} rhs; \
                     if (result != x) {{ return 0; }} return result; }}"
                ));
                let value = |raw: u128| {
                    if signed {
                        let shift = 128 - bits;
                        ConstValue::I(((raw << shift) as i128) >> shift)
                    } else {
                        ConstValue::U(raw)
                    }
                };
                assert_eq!(
                    run(&ir, &[value(initial), value(rhs)]),
                    Some(value(expected)),
                    "{ty} {operator}"
                );
            }
        }
    }
}

#[test]
fn compound_rhs_conversions_and_old_value_are_evaluated_once_in_order() {
    let ir = module(
        r#"
fun probe(n: i32) -> i32 {
    var x: u8 = 250;
    var narrowed: u8 = x += ((n as u8) as u8);
    var y: i32 = 2;
    var captured: i32 = y *= (y = y + 1);
    return narrowed as i32 + captured + y;
}
"#,
    );
    assert_eq!(run(&ir, &[ConstValue::I(263)]), Some(ConstValue::I(13)));
    let ir = module(
        "fun probe() -> i32 { var x: i32 = 2; var y: i32 = 3; \
         return (x += (y *= 4)) + x + y; }",
    );
    assert_eq!(run(&ir, &[]), Some(ConstValue::I(40)));
}

#[test]
fn compound_float_operations_verify_and_round_trip_without_claiming_execution() {
    for ty in ["f32", "f64"] {
        module(&format!(
            "fun probe(n: {ty}) -> {ty} {{ var x: {ty} = n; \
             x += 1.0; x -= 2.0; x *= 3.0; return x; }}"
        ));
    }
}
