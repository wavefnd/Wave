//! Frontend contracts for payload variants and variant matching.

use hir::{HirExpressionType, TypedProgram};
use lexer::Lexer;
use parser::ast::{ASTNode, MatchPattern, StatementNode, WaveType};
use parser::generics::monomorphize_generics;
use parser::parse_syntax_only;

fn syntax(source: &str) -> Vec<ASTNode> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().expect("lex should succeed");
    parse_syntax_only(&tokens).expect("parse should succeed")
}

fn lower(source: &str) -> TypedProgram {
    let ast = syntax(source);
    let ast = monomorphize_generics(ast).expect("generic rewriting should succeed");
    TypedProgram::lower(ast).expect("typed HIR lowering should succeed")
}

fn semantic_error(source: &str) -> String {
    let ast = syntax(source);
    TypedProgram::lower(ast)
        .expect_err("semantic validation must reject the program")
        .diagnostic()
        .message
        .clone()
}

#[test]
fn parses_variant_declarations_and_recursive_patterns() {
    let program = lower(
        r#"
variant Cell<T> {
    Value(T)
}

variant Envelope<T> {
    Cell(Cell<T>)
}

fun unwrap(item: Envelope<i32>) -> i32 {
    match item {
        Envelope::Cell(Cell::Value(value)) => {
            return value;
        }
    }
}

variant Bit {
    Zero,
    One
}

variant WrappedBit {
    Bit(Bit)
}

fun unwrap_bit(item: WrappedBit) -> i32 {
    match item {
        WrappedBit::Bit(Bit::Zero) => { return 0; }
        WrappedBit::Bit(Bit::One) => { return 1; }
    }
}
"#,
    );

    let ASTNode::Variant(cell) = &program.syntax()[0] else {
        panic!("expected variant declaration");
    };
    assert_eq!(cell.name, "Cell");
    assert_eq!(cell.generic_params, ["T"]);
    assert_eq!(cell.cases[0].payload_types, [WaveType::Struct("T".into())]);

    let ASTNode::Function(unwrap) = &program.syntax()[2] else {
        panic!("expected unwrap function");
    };
    let ASTNode::Statement(StatementNode::Match { arms, .. }) = &unwrap.body[0] else {
        panic!("expected match statement");
    };
    assert!(matches!(
        &arms[0].pattern,
        MatchPattern::Variant { payloads, .. }
            if matches!(payloads.as_slice(), [MatchPattern::Variant { .. }])
    ));
    let outer = program
        .variant_pattern(program.pattern_id(&arms[0].pattern).unwrap())
        .expect("outer variant pattern metadata");
    assert_eq!(outer.discriminant, 0);
    assert_eq!(outer.payload_types, [WaveType::Variant("Cell<i32>".into())]);
    let MatchPattern::Variant { payloads, .. } = &arms[0].pattern else { unreachable!() };
    let nested = program
        .variant_pattern(program.pattern_id(&payloads[0]).unwrap())
        .expect("nested variant pattern metadata");
    assert_eq!(nested.variant_type, WaveType::Variant("Cell<i32>".into()));
}

#[test]
fn resolves_contextual_generic_constructors_into_typed_hir() {
    let program = lower(
        r#"
variant Option<T> {
    Some(T),
    None
}

fun some() -> Option<i32> {
    return Option::Some(7);
}

fun none() -> Option<i32> {
    return Option::None;
}
"#,
    );

    for index in [1, 2] {
        let ASTNode::Function(function) = &program.syntax()[index] else {
            panic!("expected function");
        };
        let ASTNode::Statement(StatementNode::Return(Some(expression))) = &function.body[0] else {
            panic!("expected constructor return");
        };
        assert_eq!(
            program.type_of(expression),
            Some(&HirExpressionType::Resolved(WaveType::Variant("Option<i32>".into())))
        );
        let construction = program
            .variant_construction(program.expression_id(expression).unwrap())
            .expect("variant construction metadata");
        assert_eq!(construction.discriminant, (index - 1) as u32);
        assert_eq!(
            construction.payload_types,
            if index == 1 { vec![WaveType::Int(32)] } else { Vec::new() }
        );
    }
}

#[test]
fn infers_generic_arguments_through_nested_variant_payloads() {
    let program = lower(
        r#"
variant Inner<T> {
    Value(T)
}

variant Outer<T> {
    Wrap(Inner<T>)
}

fun make() {
    Outer::Wrap(Inner::Value(7));
}
"#,
    );

    let ASTNode::Function(make) = &program.syntax()[2] else {
        panic!("expected make function");
    };
    let ASTNode::Statement(StatementNode::Expression(initializer)) = &make.body[0] else {
        panic!("expected constructor expression");
    };
    assert_eq!(
        program.type_of(initializer),
        Some(&HirExpressionType::Resolved(WaveType::Variant("Outer<i32>".into())))
    );
}

#[test]
fn rejects_invalid_variant_declarations_and_constructors() {
    let cases = [
        (
            "variant Bad { Same, Same }",
            "duplicate case `Same` in variant `Bad`",
        ),
        (
            "variant Option<T> { Some(T), None } fun bad() -> Option<i32> { return Option::Missing; }",
            "unknown case `Missing` in variant `Option`",
        ),
        (
            "variant Option<T> { Some(T), None } fun bad() -> Option<i32> { return Option::Some; }",
            "expects 1 payload value(s), found 0",
        ),
        (
            "variant Option<T> { Some(T), None } fun bad() -> Option<i32> { return Option::Some(\"text\"); }",
            "expected `i32`, found `str`",
        ),
        (
            "variant Loop<T> { Next(T, Loop<T>), End }",
            "variant `Loop` has infinite size",
        ),
        (
            "variant Option<T> { Some(T), None } export(c) fun bad(value: Option<i32>) {}",
            "cannot expose variant parameter `value` directly",
        ),
        (
            "variant Option<T> { Some(T), None } export(c) fun bad() -> Option<i32> { return Option::None; }",
            "cannot return a variant directly",
        ),
        (
            "variant First { Value } variant Second { Value } fun bad() -> First { return Second::Value; }",
            "expected `First`, found `Second`",
        ),
    ];

    for (source, expected) in cases {
        let error = semantic_error(source);
        assert!(
            error.contains(expected),
            "expected `{expected}` in `{error}` for source `{source}`"
        );
    }
}

#[test]
fn enforces_variant_match_scope_uniqueness_and_exhaustiveness() {
    let cases = [
        (
            "variant Option<T> { Some(T), None } fun bad(value: Option<i32>) { match value { Option::Some(item) => {} } }",
            "non-exhaustive match on variant `Option<i32>`",
        ),
        (
            "variant Option<T> { Some(T), None } fun bad(value: Option<i32>) { match value { Option::Some(item) => {}, Option::Some(other) => {}, Option::None => {} } }",
            "duplicate variant case `Option::Some`",
        ),
        (
            "variant Pair { Values(i32, i32) } fun bad(value: Pair) { match value { Pair::Values(item, item) => {} } }",
            "duplicate pattern binding `item`",
        ),
        (
            "variant Option<T> { Some(T), None } fun bad(value: Option<i32>) { match value { _ => {}, Option::None => {} } }",
            "match pattern after wildcard is unreachable",
        ),
        (
            "variant Flag { Off, On } fun bad(value: Flag) { match value { Flag::Off => {}, Flag::On => {}, _ => {} } }",
            "previous variant patterns are exhaustive",
        ),
        (
            "variant Option<T> { Some(T), None } fun bad(value: Option<i32>) { match value { Option::Some() => {}, Option::None => {} } }",
            "expects 1 payload pattern(s), found 0",
        ),
        (
            "variant Option<T> { Some(T), None } fun bad(value: Option<i32>) { match value { Option::Some(item) => {}, Option::None => {} } var leaked: i32 = item; }",
            "undeclared identifier `item`",
        ),
        (
            "variant Inner { A, B } variant Outer { Wrap(Inner) } fun bad(value: Outer) { match value { Outer::Wrap(Inner::A) => {} } }",
            "patterns do not cover all payload shapes",
        ),
    ];

    for (source, expected) in cases {
        let error = semantic_error(source);
        assert!(
            error.contains(expected),
            "expected `{expected}` in `{error}` for source `{source}`"
        );
    }
}

#[test]
fn permits_pointer_indirection_in_recursive_variants() {
    lower(
        r#"
variant List<T> {
    Node(T, ptr<List<T>>),
    End
}
"#,
    );
}

#[test]
fn preserves_variant_types_through_generic_monomorphization() {
    let program = lower(
        r#"
variant Option<T> {
    Some(T),
    None
}

fun wrap<T>(value: T) -> Option<T> {
    return Option::Some(value);
}

fun concrete() -> Option<i64> {
    return wrap<i64>(7);
}
"#,
    );

    assert!(program.syntax().iter().any(|node| {
        matches!(
            node,
            ASTNode::Function(function)
                if function.name.contains("wrap$g$i64")
                    && function.return_type
                        == Some(WaveType::Variant("Option<i64>".into()))
        )
    }));
    assert!(program.syntax().iter().any(|node| {
        matches!(
            node,
            ASTNode::Variant(variant)
                if variant.name == "Option<i64>" && variant.generic_params.is_empty()
        )
    }));
}
