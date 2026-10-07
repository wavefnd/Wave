//! Expression failures retain the offending token through each surrounding syntax form.
use lexer::Lexer;
use parser::{parse_syntax_with_spans, ParseError};

fn failure(marked: &str, expected: &str, context: &str) -> ParseError {
    let start = marked.find('@').expect("mark the offending token");
    let source = marked.replace('@', "");
    let tokens = Lexer::new_with_file(&source, "expressions.wave").tokenize().unwrap();
    let error = parse_syntax_with_spans(&tokens).unwrap_err();
    assert_eq!(error.expected(), [expected], "{source}: {error:?}");
    assert_eq!(error.context(), Some(context), "{source}: {error:?}");
    let span = error.span().unwrap();
    assert_eq!(span.start, start, "{source}: {error:?}");
    assert_eq!(span.file, "expressions.wave");
    assert_eq!(error.line(), source[..start].bytes().filter(|b| *b == b'\n').count() + 1);
    assert_eq!(error.column(), source[..start].rsplit('\n').next().unwrap().chars().count() + 1);
    if span.start < source.len() {
        assert!(error.found().unwrap().contains(&source[span.start..span.end]), "{error:?}");
    } else {
        assert_eq!(error.found(), Some("Eof"));
    }
    error
}

#[test]
fn malformed_postfix_and_aggregate_forms_keep_their_construct_context() {
    for (expr, expected, context) in [
        ("object.@;", "identifier", "member access"),
        ("object.@)", "identifier", "member access"),
        ("pkg::@;", "identifier", "qualified name"),
        ("pkg::Type::@)", "identifier", "qualified name"),
        ("call(1 @;", "')'", "function call"),
        ("call<i32>(1 @;", "')'", "function call"),
        ("object.method(1 @;", "')'", "method call"),
        ("array[1 @;", "']'", "index expression"),
        ("(1 + 2 @;", "')'", "grouped expression"),
        ("[1, 2 @;", "']'", "array literal"),
        ("Point { @1: 2 }", "identifier", "struct literal field"),
        ("Point { x @1 }", "':'", "struct literal field"),
        ("Point { x: 1 @y: 2 }", "',' or '}'", "struct literal"),
        ("Point<i32> { x: 1 @y: 2 }", "',' or '}'", "struct literal"),
        ("call(1, @)", "expression", "primary expression"),
        ("[1, @]", "expression", "primary expression"),
        ("call(object.method([Point { x: item.@; }]))", "identifier", "member access"),
        ("call<Box<i32>>(item.@;)", "identifier", "member access"),
    ] {
        failure(&format!("// 한글\r\nfun f() {{\r\n    {expr}\r\n}}"), expected, context);
    }
}

#[test]
fn expression_errors_propagate_through_declarations_statements_and_asm() {
    for source in [
        "fun f() { var x: i32 = item.@; }",
        "const x: i32 = item.@;",
        "static x: i32 = item.@;",
        "fun f(x: i32 = item.@;) {}",
        "enum E -> i32 { X = item.@; }",
        "fun f() { return item.@; }",
        "fun f() { if (item.@;) {} }",
        "fun f() { while (item.@;) {} }",
        "fun f() { for (i: i32 = item.@; i < 2; i += 1) {} }",
        "fun f() { match (item.@;) { _ => {} } }",
        "fun f() { println(\"{}\", item.@;); }",
        "fun f() { print(\"{}\", item.@;); }",
        "fun f() { input(\"{}\", item.@;); }",
        "fun f() { asm { in(\"rax\") item.@; } }",
        "fun f() { var x: i32 = asm { in(\"rax\") item.@; }; }",
        "pub const x: i32 = item.@;",
        "pub static x: i32 = item.@;",
        "pub enum E -> i32 { X = item.@; }",
        "export(c) fun f() { return item.@; }",
        "struct S { fun f() { return item.@; } }",
    ] {
        failure(source, "identifier", "member access");
    }
}

#[test]
fn truncated_expression_delimiters_have_explicit_expectations() {
    for (expr, expected, context) in [
        ("call(@", "')'", "function call"),
        ("call(1@", "')'", "function call"),
        ("object.method(@", "')'", "method call"),
        ("array[1@", "']'", "index expression"),
        ("(1@", "')'", "grouped expression"),
        ("[@", "']'", "array literal"),
        ("[1@", "']'", "array literal"),
        ("Point {@", "'}'", "struct literal"),
        ("pkg::@", "identifier", "qualified name"),
        ("object.@", "identifier", "member access"),
    ] {
        failure(&format!("fun f() {{ {expr}"), expected, context);
    }
}

#[test]
fn nested_postfix_and_aggregate_forms_preserve_valid_syntax() {
    for expr in [
        "call().field[1].method(other(2), [3, 4])[0]",
        "generic<Box<i32>>(Point<i32> { x: 1, }).method()",
        "((a + b) * c)[index()]",
        "[[1, 2], [3, 4]][0][1]",
        "pkg::Type::call().field",
        "Point { x: 1, y: other().field, }",
        "array[1]++",
        "++array[1]",
        "a = b = call()",
    ] {
        let source = format!("fun f() {{ {expr}; }}");
        let tokens = Lexer::new(&source).tokenize().unwrap();
        parse_syntax_with_spans(&tokens).unwrap_or_else(|error| panic!("{source}: {error:?}"));
    }
}
