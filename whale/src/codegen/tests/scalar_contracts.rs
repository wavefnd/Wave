// SPDX-License-Identifier: MPL-2.0
use super::{mask, module};
use crate::codegen::{lower_with_limits, verify_output};
use hir::TypedProgram;
use whale_ir::*;

fn run(module: &Module, args: &[ConstValue]) -> Result<Option<ConstValue>, InterpreterError> {
    super::interpret_checked(
        module,
        module.functions[0].id,
        args,
        InterpreterOptions {
            max_steps: 2000,
            max_call_depth: 16,
            max_frames: 64,
            max_live_values: 4096,
            max_trace_frames: 32,
            ir_limits: IrLimits { max_nodes: 20_000, ..IrLimits::default() },
            memory_limits: MemoryLimits {
                max_bytes: 4096,
                max_allocations: 128,
                max_pointer_fragments: 128,
                max_work: 100_000,
            },
        },
    )
    .map(|result| result.value)
}

fn signed(raw: u128, bits: u32) -> i128 {
    let shift = 128 - bits;
    (((raw & mask(bits)) << shift) as i128) >> shift
}

fn explicit_trap(result: Result<Option<ConstValue>, InterpreterError>, expected: &str) {
    assert!(
        matches!(result, Err(InterpreterError::Trap(InterpreterTrap {
        reason: TrapReason::Explicit(ref message), ..
    })) if message == expected),
        "{result:?}"
    );
}

#[test]
fn checked_division_and_remainder_execute_all_integer_widths() {
    for bits in [8, 16, 32, 64, 128] {
        for is_signed in [false, true] {
            let ty = format!("{}{bits}", if is_signed { "i" } else { "u" });
            let minimum = if bits == 128 { i128::MIN } else { -(1i128 << (bits - 1)) };
            for operator in ["/", "%"] {
                let ir = module(&format!(
                    "fun probe(a: {ty}, b: {ty}) -> {ty} {{ return a {operator} b; }}"
                ));
                for a in [0, 1, 17, 1 << (bits - 1), mask(bits) - 1, mask(bits)] {
                    for b in [0, 1, 3, 1 << (bits - 1), mask(bits)] {
                        let argument = |raw| {
                            if is_signed {
                                ConstValue::I(signed(raw, bits))
                            } else {
                                ConstValue::U(raw & mask(bits))
                            }
                        };
                        let result = run(&ir, &[argument(a), argument(b)]);
                        if b == 0 {
                            explicit_trap(
                                result,
                                if operator == "/" {
                                    "integer division by zero"
                                } else {
                                    "integer remainder by zero"
                                },
                            );
                        } else if is_signed {
                            let (a, b) = (signed(a, bits), signed(b, bits));
                            if operator == "/" && a == minimum && b == -1 {
                                explicit_trap(result, "signed integer division overflow");
                            } else {
                                let expected = if operator == "/" {
                                    a / b
                                } else if b == -1 {
                                    0
                                } else {
                                    a % b
                                };
                                assert_eq!(
                                    result.unwrap(),
                                    Some(ConstValue::I(expected)),
                                    "{ty}: {a}{operator}{b}"
                                );
                            }
                        } else {
                            let expected = if operator == "/" { a / b } else { a % b };
                            assert_eq!(
                                result.unwrap(),
                                Some(ConstValue::U(expected)),
                                "{ty}: {a}{operator}{b}"
                            );
                        }
                    }
                }
            }
        }
    }
}

#[test]
fn division_preserves_mixed_conversions_single_evaluation_and_short_circuiting() {
    let ir = module("fun probe(a: i8, b: i32) -> i32 { return a / b; }");
    assert_eq!(run(&ir, &[ConstValue::I(-17), ConstValue::I(3)]).unwrap(), Some(ConstValue::I(-5)));
    let ir = module(
        "fun probe() -> i32 { var n: i32 = 0; var result: i32 = (n = n + 12) / (n = n - 9); return result * 10 + n; }",
    );
    assert_eq!(run(&ir, &[]).unwrap(), Some(ConstValue::I(43)));
    let ir = module(
        "fun probe() -> i32 { var n: i32 = 12; var a: i32 = n /= (n = 3); var b: i32 = n %= 3; return a * 100 + b * 10 + n; }",
    );
    assert_eq!(run(&ir, &[]).unwrap(), Some(ConstValue::I(411)));
    for source in [
        "fun probe() -> bool { return false && ((1 / 0) > 0); }",
        "fun probe() -> bool { return true || ((1 % 0) > 0); }",
    ] {
        let ir = module(source);
        assert_eq!(run(&ir, &[]).unwrap(), Some(ConstValue::Bool(source.contains("true"))));
    }
}

#[test]
fn scalar_uninitialized_storage_resets_at_each_declaration_execution() {
    let ir = module("fun probe() -> i32 { var n: i32; n = 7; return n; }");
    assert_eq!(run(&ir, &[]).unwrap(), Some(ConstValue::I(7)));
    let ir = module("fun probe(flag: bool) -> i32 { var n: i32; if (flag) { n = 7; } return n; }");
    assert_eq!(run(&ir, &[ConstValue::Bool(true)]).unwrap(), Some(ConstValue::I(7)));
    let is_uninitialized = |result| {
        matches!(
            result,
            Err(InterpreterError::Trap(InterpreterTrap {
                reason: TrapReason::Memory(MemoryTrap::Uninitialized { .. }),
                ..
            }))
        )
    };
    assert!(is_uninitialized(run(&ir, &[ConstValue::Bool(false)])));
    let ir = module(
        "fun probe() -> i32 { var i: i32 = 0; var result: i32 = 0; while (i < 2) { var n: i32; if (i == 0) { n = 7; } result = n; i += 1; } return result; }",
    );
    assert!(is_uninitialized(run(&ir, &[])), "loop re-entry must reset initialization");
    let ir =
        module("fun probe() -> i32 { var n: i32 = 3; if (true) { var n: i32; n = 7; } return n; }");
    assert_eq!(run(&ir, &[]).unwrap(), Some(ConstValue::I(3)));
    let ir = module("fun probe() -> i32 { var n: i32; if (true) { var n: i32 = 7; } return n; }");
    assert!(is_uninitialized(run(&ir, &[])), "inner declaration must not initialize outer storage");
    for ty in
        ["i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "bool", "f32", "f64"]
    {
        let ir = module(&format!("fun probe(n: {ty}) -> {ty} {{ var x: {ty}; x = n; return x; }}"));
        assert!(print_module(&ir).contains("uninit"));
        assert!(!print_module(&ir).contains("undef"));
    }
}

#[test]
fn direct_defaults_are_prepared_typed_and_evaluated_once_in_argument_order() {
    let ir = module(
        "fun probe() -> i32 { var n: i32 = 0; var a: i32 = pick((n = n + 1)); var b: i32 = pick(); return a * 100 + b + n; } fun pick(a: i32 = 1, b: i32 = 7) -> i32 { return a * 10 + b; }",
    );
    assert_eq!(run(&ir, &[]).unwrap(), Some(ConstValue::I(1718)));
    let ir = module(
        "fun probe() -> i32 { var n: i32 = 3; return pick((n = 4), (n = 5)) + n; } fun pick(a: i32 = 1, b: i32 = 7) -> i32 { return a * 10 + b; }",
    );
    assert_eq!(run(&ir, &[]).unwrap(), Some(ConstValue::I(50)));
}

#[test]
fn float_division_and_remainder_execute_without_integer_traps() {
    for ty in ["f32", "f64"] {
        for operator in ["/", "%"] {
            let ir = module(&format!(
                "fun probe(a: {ty}, b: {ty}) -> {ty} {{ return a {operator} b; }}"
            ));
            let text = print_module(&ir);
            assert!(text.contains(if operator == "/" { "fdiv" } else { "frem" }), "{text}");
            assert!(!text.contains("trap_if"), "{text}");
            let bits = if ty == "f32" { 32 } else { 64 };
            let arguments =
                [6.0, 2.0].map(|n| ConstValue::F(FloatBits::from_f64(bits, n).unwrap()));
            let expected = if operator == "/" { 3.0 } else { 0.0 };
            assert_eq!(
                run(&ir, &arguments).unwrap(),
                Some(ConstValue::F(FloatBits::from_f64(bits, expected).unwrap()))
            );
        }
        module(&format!(
            "fun probe(n: {ty}) -> {ty} {{ var x: {ty} = n; x /= 0.0; x %= -0.0; return x; }}"
        ));
    }
    let ir = module("fun probe(a: i16, b: f64) -> f64 { return a / b; }");
    assert!(print_module(&ir).contains("itof_s"));
    module("fun probe() -> f64 { return (1.0 as f64) / -0.0; }");
}

#[test]
fn verification_limits_are_normal_diagnostics_and_invalid_output_remains_internal() {
    let tokens = lexer::Lexer::new_with_file("fun probe() -> i32 { return 7; }", "limits.wave")
        .tokenize()
        .unwrap();
    let program = TypedProgram::lower(parser::parse_syntax_with_spans(&tokens).unwrap()).unwrap();
    let error =
        lower_with_limits(&program, IrLimits { max_nodes: 1, ..IrLimits::default() }).unwrap_err();
    assert!(error.message.contains("resource limit exceeded"), "{error:?}");
    assert!(!error.message.contains("internal compiler error"), "{error:?}");
    let error =
        lower_with_limits(&program, IrLimits { max_analysis_work: 0, ..IrLimits::default() })
            .unwrap_err();
    assert!(error.message.contains("resource limit exceeded"), "{error:?}");
    let mut invalid = module("fun probe() -> i32 { return 7; }");
    invalid.functions[0].blocks[0].terminator = None;
    let error = verify_output(&invalid, IrLimits::default()).unwrap_err();
    assert!(error.message.contains("internal compiler error"), "{error:?}");
    assert!(error.message.contains("UnterminatedBlock"), "{error:?}");
}

#[test]
fn shared_defined_result_fixture_matches_the_llvm_execution_contract() {
    let ir = module(include_str!("../../../../tests/fixtures/whale_scalar_defined.wave"));
    assert_eq!(run(&ir, &[]).unwrap(), Some(ConstValue::I(0)));
}

#[test]
fn generated_default_call_is_evaluated_once_with_ordered_effects() {
    use parser::ast::{ASTNode, Expression};
    let source = "fun probe() -> i32 { return pick(); } fun helper() -> i32 { return 9; } fun pick(n: i32 = 1) -> i32 { return n; }";
    let tokens = lexer::Lexer::new_with_file(source, "generated-default.wave").tokenize().unwrap();
    let mut nodes = parser::parse_syntax_with_spans(&tokens).unwrap();
    for node in &mut nodes {
        let mut node = node;
        while let ASTNode::Located { value, .. } = node {
            node = value;
        }
        if let ASTNode::Function(function) = node {
            if function.name == "pick" {
                let parameter = &mut function.parameters[0];
                let span = parameter.initial_value.as_ref().unwrap().span().cloned();
                parameter.initial_value = Some(
                    Expression::FunctionCall {
                        name: "helper".into(),
                        type_args: vec![],
                        args: vec![],
                    }
                    .with_span(span),
                );
            }
        }
    }
    let program = TypedProgram::lower(nodes).unwrap();
    let ir = crate::codegen::lower(&program).unwrap();
    let result = super::interpret_checked(
        &ir,
        ir.functions[0].id,
        &[],
        InterpreterOptions { max_frames: 3, max_steps: 100, ..InterpreterOptions::default() },
    )
    .unwrap();
    assert_eq!(result.value, Some(ConstValue::I(9)));
    // Exactly probe + helper + pick frames; there is no duplicate default call.
    assert!(matches!(
        super::interpret_checked(
            &ir,
            ir.functions[0].id,
            &[],
            InterpreterOptions { max_frames: 2, max_steps: 100, ..InterpreterOptions::default() }
        ),
        Err(InterpreterError::CallLimit { resource: CallResource::Frames, .. })
    ));
}

#[test]
fn floating_ieee_fixture_executes_with_the_bounded_scalar_oracle() {
    let ir = module(include_str!("../../../../tests/fixtures/whale_float_defined.wave"));
    let text = print_module(&ir);
    for operation in ["fdiv f32", "frem f32", "fdiv f64", "frem f64"] {
        assert!(text.contains(operation), "{operation}: {text}");
    }
    assert!(!text.contains("integer division"));
    assert!(!text.contains("integer remainder"));
    assert_eq!(run(&ir, &[]).unwrap(), Some(ConstValue::I(0)));
}
