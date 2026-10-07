//! Contracts for the backend-neutral typed frontend boundary.

use hir::{HirExpressionType, TypedProgram};
use lexer::Lexer;
use parser::ast::{ASTNode, Expression, WaveType};
use parser::parse_syntax_only;

fn lower(source: &str) -> TypedProgram {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().expect("lex should succeed");
    let syntax = parse_syntax_only(&tokens).expect("parse should succeed");
    TypedProgram::lower(syntax).expect("semantic lowering should succeed")
}

#[test]
fn literal_arithmetic_records_its_integer_context_before_codegen() {
    let program = lower(
        r#"
fun check(value: u64) -> bool {
    var comparison: bool = value <= (4294967296 - 32);
    var unsigned: u64 = 18446744073709551615 / 3;
    var float: f64 = 5 / 2;
    return comparison;
}
"#,
    );
    let ASTNode::Function(function) = &program.syntax()[0] else {
        panic!("function");
    };
    let ASTNode::Variable(comparison) = &function.body[0] else {
        panic!("variable");
    };
    let Expression::BinaryExpression { right, .. } = comparison.initial_value.as_ref().unwrap()
    else {
        panic!("comparison");
    };
    let Expression::Grouped(arithmetic) = right.as_ref() else {
        panic!("group");
    };
    assert_eq!(program.type_of(arithmetic), Some(&HirExpressionType::Resolved(WaveType::Uint(64))));
    let Expression::BinaryExpression { left, right, .. } = arithmetic.as_ref() else {
        panic!("arithmetic");
    };
    assert_eq!(program.expected_type_of(left), Some(&WaveType::Uint(64)));
    assert_eq!(program.expected_type_of(right), Some(&WaveType::Uint(64)));
    for (index, expected) in [(1, WaveType::Uint(64)), (2, WaveType::Int(32))] {
        let ASTNode::Variable(variable) = &function.body[index] else {
            panic!("variable");
        };
        assert_eq!(
            program.type_of(variable.initial_value.as_ref().unwrap()),
            Some(&HirExpressionType::Resolved(expected))
        );
    }
}

#[test]
fn assigns_stable_ids_and_preserves_semantic_expression_types() {
    let program = lower(
        r#"
fun calculate(left: i64, right: i64) -> i64 {
    var total: i64 = left + right;
    var literal: i64 = 1;
    var pointer: ptr<i8> = null;
    return total;
}
"#,
    );

    let ASTNode::Function(function) = &program.syntax()[0] else {
        panic!("expected function");
    };
    let ASTNode::Variable(total) = &function.body[0] else {
        panic!("expected total variable");
    };
    let binary = total.initial_value.as_ref().expect("expected initializer");
    assert_eq!(program.type_of(binary), Some(&HirExpressionType::Resolved(WaveType::Int(64))));

    let Expression::BinaryExpression { left, right, .. } = binary else {
        panic!("expected binary expression");
    };
    let left_id = program.expression_id(left).expect("left expression id");
    let right_id = program.expression_id(right).expect("right expression id");
    assert_ne!(left_id, right_id);
    assert_eq!(left_id.index() + 1, right_id.index());
    assert_eq!(
        program.expression_type(left_id),
        Some(&HirExpressionType::Resolved(WaveType::Int(64)))
    );

    let ASTNode::Variable(literal) = &function.body[1] else {
        panic!("expected literal variable");
    };
    assert_eq!(
        program.type_of(literal.initial_value.as_ref().unwrap()),
        Some(&HirExpressionType::IntegerLiteral)
    );

    let ASTNode::Variable(pointer) = &function.body[2] else {
        panic!("expected pointer variable");
    };
    assert_eq!(
        program.type_of(pointer.initial_value.as_ref().unwrap()),
        Some(&HirExpressionType::Null)
    );
    assert_eq!(program.expression_count(), 6);
}

#[test]
fn rejects_invalid_programs_before_constructing_typed_hir() {
    let mut lexer = Lexer::new(
        r#"
fun invalid() -> i32 {
    return missing;
}
"#,
    );
    let tokens = lexer.tokenize().expect("lex should succeed");
    let syntax = parse_syntax_only(&tokens).expect("parse should succeed");
    let error = TypedProgram::lower(syntax).expect_err("lowering must reject invalid input");
    assert!(error.diagnostic().message.contains("undeclared identifier `missing`"));
}

#[test]
fn canonicalizes_named_types_at_the_backend_boundary() {
    let program = lower(
        r#"
type Count = i64;
enum Status -> i16 { Ready = 1 }

fun convert(value: Count) -> Status {
    var pointer: ptr<Count> = &value;
    return value as Status;
}
"#,
    );

    let ASTNode::Function(function) = &program.syntax()[2] else {
        panic!("expected function");
    };
    assert_eq!(function.parameters[0].param_type, WaveType::Int(64));
    assert_eq!(function.return_type, Some(WaveType::Int(16)));

    let ASTNode::Variable(pointer) = &function.body[0] else {
        panic!("expected pointer variable");
    };
    assert_eq!(pointer.type_name, WaveType::Pointer(Box::new(WaveType::Int(64))));

    let ASTNode::Statement(parser::ast::StatementNode::Return(Some(Expression::Cast {
        target_type,
        ..
    }))) = &function.body[1]
    else {
        panic!("expected cast return");
    };
    assert_eq!(*target_type, WaveType::Int(16));
}

#[test]
fn addressed_array_literals_retain_pointer_and_element_conversion_contexts() {
    let program = lower(
        r#"
fun check(value: u8) {
    var pointer: ptr<array<u64, 3>> = null;
    pointer = &[value, 4294967296, 5 / 2];
}
"#,
    );
    program.verify_conversions().unwrap();
    let ASTNode::Function(function) = &program.syntax()[0] else { panic!() };
    let ASTNode::Statement(parser::ast::StatementNode::Expression(Expression::AssignOperation {
        value,
        ..
    })) = &function.body[1]
    else {
        panic!("unexpected assignment: {:?}", function.body[1]);
    };
    let pointer_type =
        WaveType::Pointer(Box::new(WaveType::Array(Box::new(WaveType::Uint(64)), 3)));
    assert_eq!(program.numeric_expression_of(value).unwrap().result_type, pointer_type);
    let Expression::AddressOf(array) = value.as_ref() else { panic!() };
    assert_eq!(
        program.expected_type_of(array),
        Some(&WaveType::Array(Box::new(WaveType::Uint(64)), 3))
    );
    let Expression::ArrayLiteral(elements) = array.as_ref() else { panic!() };
    for element in elements {
        assert_eq!(program.expected_type_of(element), Some(&WaveType::Uint(64)));
        assert_eq!(program.numeric_expression_of(element).unwrap().result_type, WaveType::Uint(64));
    }
    assert_eq!(
        program.numeric_expression_of(&elements[0]).unwrap().conversions[0].kind,
        hir::conversions::ConversionKind::ZeroExtend
    );
}
