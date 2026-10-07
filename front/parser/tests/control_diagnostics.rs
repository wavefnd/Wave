//! Control-flow failures retain their cause through every enclosing declaration.
use lexer::Lexer;
use parser::{parse_syntax_only, parse_syntax_with_spans, ParseError};

fn diagnostic(source: &str) -> ParseError {
    let tokens = Lexer::new_with_file(source, "control.wave").tokenize().unwrap();
    parse_syntax_with_spans(&tokens).unwrap_err()
}

#[test]
fn missing_header_delimiters_identify_the_unexpected_token() {
    let cases = [
        ("if @true) {}", "'('", "if header"),
        ("if (true @{ }", "')'", "if header"),
        ("if (true) @return;", "'{'", "if header"),
        ("if (true) {} else if @false) {}", "'('", "else if header"),
        ("if (true) {} else if (false @{ }", "')'", "else if header"),
        ("if (true) {} else if (false) @return;", "'{'", "else if header"),
        ("if (true) {} else @return;", "'{'", "else header"),
        ("while @true) {}", "'('", "while header"),
        ("while (true @{ }", "')'", "while header"),
        ("while (true) @return;", "'{'", "while header"),
        ("for @i = 0; i < 2; i = i + 1) {}", "'('", "for header"),
        ("for (i = 0 @i < 2; i = i + 1) {}", "';'", "for initializer"),
        ("for (var i: i32 = 0 @i < 2; i = i + 1) {}", "';'", "for initializer"),
        ("for (i: i32 = 0 @i < 2; i = i + 1) {}", "';'", "for initializer"),
        ("for (i = 0; i < 2 @i = i + 1) {}", "';'", "for condition"),
        ("for (i = 0; i < 2; i = i + 1 @{ }", "')'", "for increment"),
        ("for (i = 0; i < 2; i = i + 1) @return;", "'{'", "for header"),
    ];
    for (body, expected, context) in cases {
        // Unicode and CRLF ensure byte offsets and source columns are not conflated.
        let marked = format!("// 이름\r\nfun main() {{\r\n    {body}\r\n}}");
        let start = marked.find('@').unwrap();
        let source = marked.replace('@', "");
        let error = diagnostic(&source);
        assert_eq!(error.expected(), [expected], "{source}: {error:?}");
        assert_eq!(error.context(), Some(context), "{source}: {error:?}");
        assert_eq!(error.span().unwrap().start, start, "{source}: {error:?}");
        assert_eq!(error.line(), 3);
        assert_eq!(
            error.column(),
            source[..start].rsplit('\n').next().unwrap().chars().count() + 1
        );
        assert!(error.found().unwrap().contains(&source[start..error.span().unwrap().end]));
        let tokens = Lexer::new(&source).tokenize().unwrap();
        let without_spans = parse_syntax_only(&tokens).unwrap_err();
        assert_eq!(without_spans.message(), error.message());
        assert_eq!(without_spans.expected(), error.expected());
        assert_eq!(without_spans.line(), error.line());
    }
}

#[test]
fn header_errors_survive_nested_blocks_exports_and_methods() {
    for source in [
        "fun f() { if (true) { while (true) { for (i = 0 i < 2; i = i + 1) {} } } }",
        "fun f() { match (1) { _ => { for (i = 0 i < 2; i = i + 1) {} } } }",
        "pub fun f() { for (i = 0 i < 2; i = i + 1) {} }",
        "export(c) fun f() { for (i = 0 i < 2; i = i + 1) {} }",
        "pub export(c) fun f() { for (i = 0 i < 2; i = i + 1) {} }",
        "export(c) { fun good() {} fun f() { for (i = 0 i < 2; i = i + 1) {} } }",
        "struct S { fun f() { for (i = 0 i < 2; i = i + 1) {} } }",
        "pub struct S { fun f() { for (i = 0 i < 2; i = i + 1) {} } }",
        "proto S { fun f() { for (i = 0 i < 2; i = i + 1) {} } }",
    ] {
        let error = diagnostic(source);
        assert_eq!(error.context(), Some("for initializer"), "{source}: {error:?}");
        assert_eq!(error.expected(), ["';'"]);
        assert_eq!(error.span().unwrap().start, source.find("i < 2").unwrap());
    }
}

#[test]
fn truncated_headers_report_eof_at_the_end_of_the_source() {
    for (body, expected, context) in [
        ("if", "'('", "if header"),
        ("if (1", "')'", "if header"),
        ("if (1)", "'{'", "if header"),
        ("if (1) {} else", "'{'", "else header"),
        ("if (1) {} else if (1", "')'", "else if header"),
        ("while (1", "')'", "while header"),
        ("for (i = 0", "';'", "for initializer"),
        ("for (i = 0; i < 2", "';'", "for condition"),
        ("for (i = 0; i < 2; i = i + 1", "')'", "for increment"),
    ] {
        let source = format!("fun f() {{\n    {body}");
        let error = diagnostic(&source);
        assert_eq!(error.expected(), [expected], "{source}: {error:?}");
        assert_eq!(error.context(), Some(context));
        assert_eq!(error.found(), Some("Eof"));
        assert_eq!(error.span().unwrap().start, source.len());
        assert_eq!(error.span().unwrap().end, source.len());
    }
}

#[test]
fn valid_headers_keep_expression_and_initializer_forms() {
    let source = r#"
fun f() {
    var i: i32 = 0;
    if (1) { while (i < 2) { i = i + 1; } }
    else if (0) {} else if (i) {} else {}
    for (i = 0; i < 2; i = i + 1) {}
    for (var j: i32 = 0; j < 2; j = j + 1) {}
    for (k: i32 = 0; k < 2; k = k + 1) {}
}
"#;
    let tokens = Lexer::new(source).tokenize().unwrap();
    parse_syntax_with_spans(&tokens).unwrap();
}
