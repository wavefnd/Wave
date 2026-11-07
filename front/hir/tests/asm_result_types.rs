// SPDX-License-Identifier: MPL-2.0
//! Expression-form assembly produces typed HIR facts at its scalar boundaries.
use hir::conversions::{ConversionKind, ConversionMode};
use hir::{HirExpressionType, TypedProgram};
use lexer::Lexer;
use parser::ast::{ASTNode, Expression, StatementNode, WaveType};
use parser::parse_syntax_with_spans;

fn lower(source: &str) -> Result<TypedProgram, hir::HirLoweringError> {
    let tokens = Lexer::new_with_file(source, "asm-results.wave").tokenize().unwrap();
    TypedProgram::lower(parse_syntax_with_spans(&tokens).unwrap())
}

fn initializer(program: &TypedProgram, index: usize) -> &Expression {
    let ASTNode::Function(function) = &program.syntax()[0] else { panic!("function") };
    let ASTNode::Variable(variable) = &function.body[index] else { panic!("variable") };
    variable.initial_value.as_ref().unwrap()
}

#[test]
fn typed_asm_result_retains_signed_extension_and_float_conversion() {
    let program = lower(
        r#"
fun main() {
    var small: i32 = 0;
    var extended: i64 = asm { "mov eax, 7" out("eax") small clobber("nostack") };
    var wide: i64 = 0;
    var floating: f64 = asm { "mov rax, 7" out("rax") wide clobber("nostack") };
}
"#,
    )
    .unwrap();
    program.verify_conversions().unwrap();

    let extended = initializer(&program, 1);
    assert_eq!(program.type_of(extended), Some(&HirExpressionType::Resolved(WaveType::Int(32))));
    let fact = program.numeric_expression_of(extended).unwrap();
    assert_eq!(fact.evaluation_type, WaveType::Int(32));
    assert_eq!(fact.result_type, WaveType::Int(64));
    assert_eq!(fact.conversions.len(), 1);
    assert_eq!(fact.conversions[0].kind, ConversionKind::SignExtend);
    assert_eq!(fact.conversions[0].mode, ConversionMode::Implicit);
    assert_eq!(fact.conversions[0].span.file, "asm-results.wave");

    let floating = initializer(&program, 3);
    assert_eq!(program.type_of(floating), Some(&HirExpressionType::Resolved(WaveType::Int(64))));
    let fact = program.numeric_expression_of(floating).unwrap();
    assert_eq!(fact.evaluation_type, WaveType::Int(64));
    assert_eq!(fact.result_type, WaveType::Float(64));
    assert_eq!(fact.conversions.len(), 1);
    assert_eq!(fact.conversions[0].kind, ConversionKind::SignedToFloat);
    assert_eq!(fact.conversions[0].mode, ConversionMode::Implicit);
}

#[test]
fn asm_results_participate_in_explicit_casts_arithmetic_and_returns() {
    let source = r#"
fun main() -> i64 {
    var sink: i32 = 0;
    var flag: bool = (asm { "mov eax, 7" out("eax") sink }) as bool;
    var sum: i32 = asm { "mov eax, 7" out("eax") sink } + 2;
    return asm { "mov eax, 7" out("eax") sink };
}
"#;
    let program = lower(source).unwrap();
    program.verify_conversions().unwrap();
    let flag = initializer(&program, 1);
    let cast = program.numeric_expression_of(flag).unwrap();
    assert_eq!(cast.conversions[0].kind, ConversionKind::IntegerToBool);
    assert_eq!(cast.conversions[0].mode, ConversionMode::Explicit);

    let sum = initializer(&program, 2);
    let fact = program.numeric_expression_of(sum).unwrap();
    assert_eq!(fact.computation_type, Some(WaveType::Int(32)));
    let Expression::BinaryExpression { left, .. } = sum else { panic!("arithmetic") };
    assert_eq!(program.type_of(left), Some(&HirExpressionType::Resolved(WaveType::Int(32))));

    let ASTNode::Function(function) = &program.syntax()[0] else { panic!("function") };
    let ASTNode::Statement(StatementNode::Return(Some(value))) = &function.body[3] else {
        panic!("return");
    };
    let fact = program.numeric_expression_of(value).unwrap();
    assert_eq!(fact.evaluation_type, WaveType::Int(32));
    assert_eq!(fact.result_type, WaveType::Int(64));
    assert_eq!(fact.conversions[0].kind, ConversionKind::SignExtend);
}

#[test]
fn invalid_asm_value_contexts_fail_during_semantic_analysis() {
    let cases = [
        (
            r#"fun main() { var sink: i64 = 0; var bad: bool = asm { "mov rax, 7" out("rax") sink }; }"#,
            "type mismatch",
        ),
        (
            r#"fun main() { var bad: i64 = asm { "nop" }; }"#,
            "without an output cannot supply a value",
        ),
        (
            r#"fun main() { var a: i64 = 0; var b: i64 = 0; var bad: i64 = asm { "nop" out("rax") a out("rdx") b }; }"#,
            "exactly one output",
        ),
        (
            r#"fun main() { var a: array<i64, 1> = [0]; var bad: i64 = asm { "nop" out("rax") a }; }"#,
            "output must be scalar",
        ),
    ];
    for (source, expected) in cases {
        let failure = lower(source).unwrap_err();
        assert!(
            failure.to_string().contains(expected),
            "expected {expected:?}, got {failure} for {source}"
        );
    }
}
