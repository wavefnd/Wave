// SPDX-License-Identifier: MPL-2.0
//! Preserve typed scalar aliases and eager boolean bitwise evaluation.
use super::module;
use whale_ir::*;

fn options() -> InterpreterOptions {
    InterpreterOptions {
        max_steps: 2000,
        max_call_depth: 16,
        max_frames: 64,
        max_live_values: 4096,
        max_trace_frames: 32,
        ir_limits: IrLimits { max_nodes: 10_000, ..IrLimits::default() },
        memory_limits: MemoryLimits {
            max_bytes: 4096,
            max_allocations: 128,
            max_pointer_fragments: 128,
            max_work: 100_000,
        },
    }
}

fn run(module: &Module, args: &[ConstValue]) -> Option<ConstValue> {
    super::interpret_checked(module, module.functions[0].id, args, options()).unwrap().value
}

#[test]
fn bool_bitwise_truth_tables_execute_without_invalid_bool_bin_instructions() {
    for operator in ["&", "|", "^"] {
        let ir =
            module(&format!("fun probe(a: bool, b: bool) -> bool {{ return a {operator} b; }}"));
        for left in [false, true] {
            for right in [false, true] {
                let expected = match operator {
                    "&" => left & right,
                    "|" => left | right,
                    "^" => left ^ right,
                    _ => unreachable!(),
                };
                assert_eq!(
                    run(&ir, &[ConstValue::Bool(left), ConstValue::Bool(right)]),
                    Some(ConstValue::Bool(expected)),
                    "{left} {operator} {right}"
                );
            }
        }
        assert!(!ir.functions[0]
            .blocks
            .iter()
            .flat_map(|b| &b.instructions)
            .any(|instruction| { matches!(instruction, Instruction::Bin { ty: Type::Bool, .. }) }));
    }
}

#[test]
fn bitwise_operands_execute_once_in_order_and_remain_distinct_from_logical_operators() {
    for (operator, left, expected) in
        [("&", false, 2), ("|", true, 3), ("^", false, 3), ("&&", false, 1), ("||", true, 2)]
    {
        let ir = module(&format!(
            r#"
fun probe() -> i32 {{
    var count: i32 = 0;
    var result: bool = ((count = count + 1) > 0 && {left})
        {operator} ((count = count * 2) > 0);
    return count + result as i32;
}}
"#
        ));
        assert_eq!(run(&ir, &[]), Some(ConstValue::I(expected)), "{operator}");
    }
}

#[test]
fn eager_bitwise_evaluates_a_trapping_rhs_that_logical_operators_skip() {
    for operator in ["&", "|", "^", "&&", "||"] {
        let left = operator == "|" || operator == "||";
        let ir = module(&format!(
            "fun probe(n: i32) -> bool {{ return {left} {operator} (((1 as i32) << n) > 0); }}"
        ));
        let result =
            super::interpret_checked(&ir, ir.functions[0].id, &[ConstValue::I(32)], options());
        if matches!(operator, "&&" | "||") {
            assert_eq!(result.unwrap().value, Some(ConstValue::Bool(left)));
        } else {
            assert!(matches!(result, Err(InterpreterError::Trap(_))), "{operator}: {result:?}");
        }
    }
}

#[test]
fn scalar_aliases_and_chains_leave_canonical_ir_and_execution_unchanged() {
    let aliases = module(
        r#"
type Number = i32;
type NumberAgain = Number;
type Flag = bool;
type Unused = f64;
fun probe(n: NumberAgain) -> Number {
    var flag: Flag = n > 0;
    var narrowed: NumberAgain = (n as u8) as Number;
    if (flag) { return narrowed + 1; }
    return narrowed;
}
"#,
    );
    let canonical = module(
        r#"
fun probe(n: i32) -> i32 {
    var flag: bool = n > 0;
    var narrowed: i32 = (n as u8) as i32;
    if (flag) { return narrowed + 1; }
    return narrowed;
}
"#,
    );
    assert_eq!(print_module(&aliases), print_module(&canonical));
    for (input, expected) in [(-1, 255), (0, 0), (256, 1), (257, 2)] {
        assert_eq!(run(&aliases, &[ConstValue::I(input)]), Some(ConstValue::I(expected)));
    }
}

#[test]
fn every_supported_value_scalar_alias_retains_its_canonical_signature() {
    for ty in [
        "i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "byte", "char",
        "bool", "f32", "f64",
    ] {
        let aliases = module(&format!(
            "type Scalar = {ty}; type Chained = Scalar;\n\
             fun probe(n: Chained) -> Scalar {{ return n; }}"
        ));
        let canonical = module(&format!("fun probe(n: {ty}) -> {ty} {{ return n; }}"));
        assert_eq!(print_module(&aliases), print_module(&canonical), "{ty}");
    }
}

#[test]
fn char_values_preserve_the_existing_unsigned_byte_contract() {
    let ir = module("fun probe(n: i32) -> i32 { return (n as char) as i32; }");
    for n in [-257, -1, 0, 255, 256, 257] {
        assert_eq!(run(&ir, &[ConstValue::I(n)]), Some(ConstValue::I((n as u8) as i128)));
    }
    let ir = module("fun probe() -> i32 { return 'ÿ' as i32; }");
    assert_eq!(run(&ir, &[]), Some(ConstValue::I(255)));
    let ir = module("fun probe(n: char) -> bool { return n < 'z'; }");
    for (n, expected) in [(0, true), (121, true), (122, false), (255, false)] {
        assert_eq!(run(&ir, &[ConstValue::U(n)]), Some(ConstValue::Bool(expected)));
    }
}

#[test]
fn integer_match_uses_verified_patterns_and_evaluates_the_scrutinee_once() {
    let ir = module(
        r#"
const CASE: i32 = -1;
fun probe(n: i32) -> i32 {
    var count: i32 = 0;
    var result: i32 = 0;
    match ((count = count + 1) * n) {
        CASE => { result = 7; }
        1 => { result = 9; }
        _ => { result = 11; }
    }
    return result + count;
}
"#,
    );
    for (n, expected) in [(-1, 8), (1, 10), (2, 12)] {
        assert_eq!(run(&ir, &[ConstValue::I(n)]), Some(ConstValue::I(expected)));
    }
    let ir = module(
        r#"
fun probe() -> i32 {
    var n: i32 = 0;
    var sum: i32 = 0;
    while (n < 5) {
        n += 1;
        match (n) {
            1 => { continue; }
            4 => { break; }
            _ => { sum += n; }
        }
    }
    match (sum) { 5 => { return 7; } _ => { return 9; } }
}
"#,
    );
    assert_eq!(run(&ir, &[]), Some(ConstValue::I(7)));
    let ir = module("fun probe(n: u128) -> i32 { match (n) { 340282366920938463463374607431768211455 => { return 1; } } return 0; }");
    assert_eq!(run(&ir, &[ConstValue::U(u128::MAX)]), Some(ConstValue::I(1)));
    assert_eq!(run(&ir, &[ConstValue::U(0)]), Some(ConstValue::I(0)));
}

#[test]
fn char_calls_and_match_preserve_parameters_returns_and_branch_values() {
    let ir = module("fun probe(n: char) -> char { return identity(n); } fun identity(n: char) -> char { return n; }");
    assert_eq!(run(&ir, &[ConstValue::U(255)]), Some(ConstValue::U(255)));
    let ir = module(
        "fun probe(n: char) -> i32 { match (n) { 255 => { return 1; } _ => { return 0; } } }",
    );
    assert_eq!(run(&ir, &[ConstValue::U(255)]), Some(ConstValue::I(1)));
}

#[test]
fn scalar_match_preserves_source_order_when_a_wildcard_precedes_later_arms() {
    let ir =
        module("fun probe(n: i32) -> i32 { match (n) { _ => { return 7; } 1 => { return 9; } } }");
    assert_eq!(run(&ir, &[ConstValue::I(1)]), Some(ConstValue::I(7)));
}
