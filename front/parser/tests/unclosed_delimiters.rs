//! EOF keeps its primary position and links to the innermost unmatched opener.
use lexer::Lexer;
use parser::{parse_syntax_only, parse_syntax_with_spans};

#[test]
fn unclosed_delimiters_retain_their_opening_span_at_every_depth() {
    for (marked, closer) in [
        ("fun f() @{", "}"),
        ("fun f() {\n if (1) @{", "}"),
        ("fun f() {\n while (1) @{\n", "}"),
        ("fun f@(", ")"),
        ("fun f@(a: i32", ")"),
        ("fun f@(a:", ")"),
        ("fun f@(a: i32,", ")"),
        ("fun f() { call@(", ")"),
        ("fun f() { call@(1", ")"),
        ("fun f() { object.method@(", ")"),
        ("fun f() { @(1 + 2", ")"),
        ("fun f() { var xs: array<i32, 2> = @[", "]"),
        ("fun f() { call([1, @[2, 3", "]"),
        ("fun f() { Point @{ x: 1", "}"),
        ("fun f() { Point { x: call@(", ")"),
        ("fun f() { \"({[\"; // ([{\n call@(", ")"),
        ("fun f() { /* ([{ */ call@(", ")"),
    ] {
        let marked = format!("// 한글\r\n{marked}");
        let start = marked.find('@').unwrap();
        let source = marked.replace('@', "");
        let tokens = Lexer::new_with_file(&source, "unclosed.wave").tokenize().unwrap();
        let error = parse_syntax_with_spans(&tokens).unwrap_err();
        assert_eq!(error.found(), Some("Eof"), "{source}: {error:?}");
        assert_eq!(error.span().unwrap().start, source.len(), "{source}: {error:?}");
        assert_eq!(error.related().len(), 1, "{source}: {error:?}");
        let related = &error.related()[0];
        assert_eq!(related.span.start, start, "{source}: {error:?}");
        assert_eq!(related.span.end, start + 1);
        assert_eq!(related.span.file, "unclosed.wave");
        assert!(related.message.contains(&format!("expected '{closer}'")), "{error:?}");
        let no_spans = parse_syntax_only(&tokens).unwrap_err();
        assert!(no_spans.note().unwrap().contains(&format!("expected '{closer}'")));
    }
}

#[test]
fn earlier_errors_do_not_acquire_unrelated_unclosed_delimiters() {
    for source in ["fun f() { object.;", "fun f() { call(1 ];"] {
        let tokens = Lexer::new(source).tokenize().unwrap();
        let error = parse_syntax_with_spans(&tokens).unwrap_err();
        assert!(error.related().is_empty(), "{source}: {error:?}");
    }
}

#[test]
fn balanced_nested_delimiters_parse_without_extra_validation_rules() {
    let source = "fun f(a: array<i32, 2>) { call(Box<i32> { value: (a[0] + a[1]) }); }";
    let tokens = Lexer::new(source).tokenize().unwrap();
    parse_syntax_with_spans(&tokens).unwrap();
}
