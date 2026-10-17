// SPDX-License-Identifier: MPL-2.0
use hir::{conversions::ConversionKind, ConstantValue, HirLoweringError, TypedProgram};
use lexer::Lexer;
use parser::{
    ast::{ASTNode, Expression, WaveType},
    generics::monomorphize_generics,
    parse_syntax_with_spans,
};

fn lower(source: &str) -> Result<TypedProgram, Box<HirLoweringError>> {
    let tokens = Lexer::new_with_file(source, "float-range.wave").tokenize().unwrap();
    let syntax = parse_syntax_with_spans(&tokens).unwrap();
    TypedProgram::lower(monomorphize_generics(syntax).unwrap()).map_err(Box::new)
}

#[test]
fn default_float_literals_must_fit_their_materialization_type_in_every_context() {
    for expression in [
        "1e39",
        "-1e39",
        "((1e39))",
        "1e39 + 0.0",
        "1e39 < 0.0",
        "!(1e39 as bool)",
        "1e39 as bool",
        "false && (1e39 as bool)",
        "true || (1e39 < 0.0)",
    ] {
        let source = format!("fun main() {{\n    {expression};\n}}\n");
        let error = lower(&source).expect_err(&source);
        let diagnostic = error.diagnostic();
        assert!(diagnostic.message.contains("floating-point literal is out of range"), "{error:?}");
        let span = diagnostic.span.as_ref().unwrap();
        assert_eq!(span.file, "float-range.wave");
        let token = if expression == "-1e39" { "-1e39" } else { "1e39" };
        assert_eq!(&source[span.start..span.end], token);
        assert_eq!(span.line, 2);
    }
    for body in ["println(\"{}\", 1e39);", "if (1e39) {}", "while (1e39) {}"] {
        let source = format!("fun main() {{ {body} }}");
        let error = lower(&source).expect_err(&source);
        assert!(error.diagnostic().message.contains("floating-point literal is out of range"));
    }
}

#[test]
fn contextual_f64_literals_keep_their_selected_width() {
    let source = r#"
type Wide = f64;
fun argument(value: Wide = 1e39) -> Wide { return value; }
fun returned() -> Wide { return -1e39; }
fun arithmetic() -> Wide { return 1e39 + 0.0; }
fun comparison(value: Wide) -> bool { return 1e39 < value; }
fun main() {
    var value: Wide = 1e39;
    var values: array<Wide, 2> = [1e39, -1e39];
    var cast: Wide = 1e39 as Wide;
    argument(1e39);
    argument();
}
"#;
    let program = lower(source).unwrap();
    program.verify_conversions().unwrap();
}

#[test]
fn finite_rounding_underflow_signed_zero_and_arithmetic_overflow_are_preserved() {
    let program = lower(
        r#"
const maximum: f32 = 3.4028234663852886e38;
const rounded: f32 = 0.1;
const underflow: f32 = 1e-50;
const zero: f32 = -0.0;
const arithmetic_overflow: f32 = 3.4028234663852886e38 * 2.0;
fun main() { 16777217.0; }
"#,
    )
    .unwrap();
    let values: Vec<_> = program
        .syntax()
        .iter()
        .filter_map(|node| {
            let ASTNode::Variable(variable) = node else { return None };
            let value =
                program.constant_value_of(variable.initial_value.as_ref().unwrap()).unwrap();
            let ConstantValue::Float(value) = value else { panic!("float constant") };
            Some(*value)
        })
        .collect();
    assert_eq!(values[0], f32::MAX as f64);
    assert_eq!(values[1], 0.1_f32 as f64);
    assert_eq!(values[2], 0.0);
    assert_eq!(values[3].to_bits(), (-0.0_f64).to_bits());
    assert!(values[4].is_infinite());
}

#[test]
fn explicit_narrowing_keeps_its_original_f64_source_and_ordered_conversion() {
    lower("const SKIPPED: bool = true || (((1e300 as f64) as i32) == 0); fun main() {}").unwrap();
    let program = lower(
        r#"
const narrowed: f32 = (1e39 as f64) as f32;
fun runtime(value: f64) -> f32 { return value as f32; }
fun main() {}
"#,
    )
    .unwrap();
    program.verify_conversions().unwrap();
    let ASTNode::Variable(variable) = &program.syntax()[0] else { panic!("constant") };
    let expr = variable.initial_value.as_ref().unwrap();
    let fact = program.numeric_expression_of(expr).unwrap();
    assert_eq!(fact.evaluation_type, WaveType::Float(64));
    assert_eq!(fact.conversions[0].kind, ConversionKind::FloatTruncate);
    assert_eq!(fact.conversions[0].source_type, WaveType::Float(64));
    assert_eq!(fact.conversions[0].target_type, WaveType::Float(32));
    assert!(matches!(expr, Expression::Cast { .. }));
    let Some(ConstantValue::Float(value)) = program.constant_value_of(expr) else {
        panic!("float constant");
    };
    assert!(value.is_infinite());
}
