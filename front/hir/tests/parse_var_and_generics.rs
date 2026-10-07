//! Regression coverage for variable declarations and generic type syntax.

use hir::TypedProgram;
use lexer::Lexer;
use parser::ast::{ASTNode, Expression, StatementNode, Visibility, WaveType};
use parser::generics::monomorphize_generics;
use parser::parse_syntax_only;

fn parse_ok(src: &str) {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex should succeed");
    let parsed = parse_syntax_only(&tokens);
    if let Err(err) = parsed {
        let mut dump = String::new();
        for (idx, t) in tokens.iter().enumerate() {
            dump.push_str(&format!(
                "{:03}: line={} {:?} lexeme=`{}`\n",
                idx, t.line, t.token_type, t.lexeme
            ));
        }
        panic!("parse failed: {:?}\nsource:\n{}\ntokens:\n{}", err, src, dump);
    }
}

fn parse_nodes(src: &str) -> Vec<ASTNode> {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex should succeed");
    parse_syntax_only(&tokens).expect("parse should succeed")
}

fn lower_generics(src: &str) -> TypedProgram {
    let syntax = parse_nodes(src);
    let syntax = monomorphize_generics(syntax).expect("generic rewriting should succeed");
    TypedProgram::lower(syntax).expect("typed HIR lowering should succeed")
}

#[test]
fn parses_var_in_function_body() {
    parse_ok(
        r#"
fun main() {
    var x: i32;
    return;
}
"#,
    );
}

#[test]
fn parses_multigeneric_function_and_types() {
    parse_ok(
        r#"
struct Pair<A, B> {
    first: A;
    second: B;
}

fun make_pair<A, B>(a: A, b: B) -> Pair<A, B> {
    var pair_value: Pair<A, B>;
    return pair_value;
}
"#,
    );
}

#[test]
fn parses_generic_struct_literals_with_nested_type_arguments() {
    let nodes = parse_nodes(
        r#"
struct Box<T> {
    value: T;
}

struct Pair<A, B> {
    first: A;
    second: B;
}

fun make<T>(value: T) -> Box<T> {
    return Box<T> { value: value };
}

fun nested() -> Pair<i32, Box<i64>> {
    return Pair<i32, Box<i64>> {
        first: 7,
        second: Box<i64> { value: 9 }
    };
}
"#,
    );

    let ASTNode::Function(make) = &nodes[2] else {
        panic!("expected generic make function");
    };
    let ASTNode::Statement(StatementNode::Return(Some(Expression::StructLiteral { name, .. }))) =
        &make.body[0]
    else {
        panic!("expected generic struct literal return");
    };
    assert_eq!(name, "Box<T>");

    let ASTNode::Function(nested) = &nodes[3] else {
        panic!("expected nested function");
    };
    let ASTNode::Statement(StatementNode::Return(Some(Expression::StructLiteral { name, fields }))) =
        &nested.body[0]
    else {
        panic!("expected nested generic struct literal return");
    };
    assert_eq!(name, "Pair<i32,Box<i64>>");
    assert!(matches!(
        &fields[1].1,
        Expression::StructLiteral { name, .. } if name == "Box<i64>"
    ));
}

#[test]
fn specializes_generic_struct_literals_before_typed_hir() {
    let program = lower_generics(
        r#"
struct Box<T> {
    value: T;
}

struct Pair<A, B> {
    first: A;
    second: B;
}

fun make<T>(value: T) -> Box<T> {
    return Box<T> { value: value };
}

fun main() -> i32 {
    var direct: Box<i32> = Box<i32> { value: 7 };
    var made: Box<i64> = make<i64>(9);
    var nested: Pair<i32, Box<i64>> = Pair<i32, Box<i64>> {
        first: direct.value,
        second: Box<i64> { value: made.value }
    };
    return nested.first;
}
"#,
    );

    assert!(program.syntax().iter().any(|node| {
        matches!(
            node,
            ASTNode::Struct(structure) if structure.name == "Box$g$i32"
        )
    }));
    assert!(program.syntax().iter().any(|node| {
        matches!(
            node,
            ASTNode::Struct(structure)
                if structure.name == "Pair$g$i32$Box_g_i64"
        )
    }));
    assert!(program.syntax().iter().any(|node| {
        matches!(
            node,
            ASTNode::Function(function) if function.name == "make$g$i64"
        )
    }));
}

#[test]
fn parses_public_declarations_and_import_forms() {
    let nodes = parse_nodes(
        r#"
import("add");
import("add::math");
import("./helpers" as helpers);
import("add")::{sum, Point,};

pub fun sum(a: i32, b: i32) -> i32 { return a + b; }
pub struct Point {}
fun main() {
    var a: i32 = add::sum(1, 2);
    var b: i32 = sum(1, 2);
    var p: Point = Point();
}
"#,
    );

    let imports = nodes
        .iter()
        .filter_map(|node| match node {
            ASTNode::Statement(StatementNode::Import(import)) => Some(import),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(imports.len(), 4);
    assert_eq!(imports[0].path, "add");
    assert_eq!(imports[1].path, "add::math");
    assert_eq!(imports[2].alias.as_deref(), Some("helpers"));
    assert_eq!(imports[3].selections, ["sum", "Point"]);

    assert!(matches!(
        &nodes[4],
        ASTNode::Function(function) if function.visibility == Visibility::Public
    ));
    assert!(matches!(
        &nodes[5],
        ASTNode::Struct(structure) if structure.visibility == Visibility::Public
    ));

    let ASTNode::Function(main) = &nodes[6] else {
        panic!("expected main function");
    };
    let ASTNode::Variable(first) = &main.body[0] else {
        panic!("expected first variable");
    };
    assert_eq!(first.type_name, WaveType::Int(32));
    assert!(matches!(
        first.initial_value.as_ref(),
        Some(Expression::FunctionCall { name, .. }) if name == "add::sum"
    ));
}

#[test]
fn rejects_untyped_var_declarations() {
    let mut lexer = Lexer::new("fun main() { var value = 1; }\n");
    let tokens = lexer.tokenize().expect("lex should succeed");
    let error = parse_syntax_only(&tokens).expect_err("untyped variables must fail");
    assert_eq!(error.message(), "variable `value` requires an explicit type");
    assert_eq!(error.context(), Some("variable declaration"));
    assert_eq!(error.help(), Some("Wave does not infer variable types; add a `: Type` annotation"));
}

#[test]
fn parses_namespaced_variable_types() {
    parse_ok("fun main() { var value: option::Option<i32> = option::Option::Some(9); }\n");
}

#[test]
fn rejects_public_main() {
    let mut lexer = Lexer::new("pub fun main() {}\n");
    let tokens = lexer.tokenize().expect("lex should succeed");
    let error = parse_syntax_only(&tokens).expect_err("pub main must fail");
    assert_eq!(error.message(), "entry function `main` cannot be public");
}

#[test]
fn public_visibility_is_independent_from_c_abi_export() {
    let nodes = parse_nodes(
        "pub export(c) fun shared_value() -> i32 { return 1; }\nexport(c) fun abi_only() -> i32 { return 2; }\n",
    );
    let ASTNode::Function(shared) = &nodes[0] else {
        panic!("expected shared export");
    };
    let ASTNode::Function(abi_only) = &nodes[1] else {
        panic!("expected ABI-only export");
    };
    assert_eq!(shared.visibility, Visibility::Public);
    assert!(shared.export.is_some());
    assert_eq!(abi_only.visibility, Visibility::Private);
    assert!(abi_only.export.is_some());
}
