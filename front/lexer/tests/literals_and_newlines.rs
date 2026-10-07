use lexer::{token::TokenType, Lexer};

#[test]
fn every_source_newline_has_the_same_tokens_and_logical_positions() {
    for newline in ["\n", "\r\n", "\r"] {
        let source = ["// comment", "/* outer", "/* nested */ ordinary", "*/", "  이름", "  + 1"]
            .join(newline);
        let tokens = Lexer::new_with_file(&source, "lines.wave").tokenize().unwrap();
        assert_eq!(tokens[0].lexeme, "이름");
        assert_eq!(tokens[0].line, 5);
        assert_eq!(tokens[0].span.as_ref().unwrap().column, 3);
        assert_eq!(tokens[0].span.as_ref().unwrap().start, source.find("이름").unwrap());
        assert_eq!(tokens[1].line, 6);
        assert_eq!(tokens[1].span.as_ref().unwrap().column, 3);
        let bad = format!("// text{newline}  \"한\\q\"");
        let error = Lexer::new_with_file(&bad, "lines.wave").tokenize().unwrap_err();
        assert_eq!(error.code.as_deref(), Some("E1004"));
        assert_eq!((error.line, error.column), (2, 5));
        assert_eq!(
            &bad[error.span.as_ref().unwrap().start..error.span.as_ref().unwrap().end],
            "\\q"
        );
        for quote in ['\"', '\''] {
            let bad = format!("{quote}{newline}{quote}");
            let error = Lexer::new_with_file(&bad, "lines.wave").tokenize().unwrap_err();
            assert_eq!(error.code.as_deref(), Some(if quote == '\"' { "E1003" } else { "E1005" }));
            assert_eq!((error.line, error.column), (1, 1));
        }
    }
}

#[test]
fn nested_and_unterminated_comments_have_stable_diagnostics() {
    let tokens = Lexer::new("/* a /* b /* c */ b */ a */ value // tail").tokenize().unwrap();
    assert_eq!(tokens[0].lexeme, "value");
    assert_eq!(tokens[0].line, 1);
    let error = Lexer::new_with_file("/* a\n /* b */", "comment.wave").tokenize().unwrap_err();
    assert_eq!(error.code.as_deref(), Some("E1002"));
    assert_eq!(error.line, 2);
    assert_eq!(error.span.as_ref().unwrap().file, "comment.wave");
    assert!(error.help.as_deref().unwrap().contains("*/"));
}

#[test]
fn string_bytes_preserve_hex_and_utf8_without_an_embedded_nul() {
    let tokens = Lexer::new(r#""한\x7F\x80\xFF\n\t\r\\\"""#).tokenize().unwrap();
    let mut expected = "한".as_bytes().to_vec();
    expected.extend_from_slice(&[0x7f, 0x80, 0xff, b'\n', b'\t', b'\r', b'\\', b'"']);
    assert_eq!(tokens[0].token_type, TokenType::String(expected));
    for (source, column, fragment) in [(r#""\x00""#, 2, "\\x00"), ("\"한\0b\"", 3, "\0")] {
        let error = Lexer::new_with_file(source, "nul.wave").tokenize().unwrap_err();
        assert_eq!(error.code.as_deref(), Some("E1004"));
        assert!(error.message.contains("NUL"));
        assert_eq!((error.line, error.column), (1, column));
        let span = error.span.as_ref().unwrap();
        assert_eq!(&source[span.start..span.end], fragment);
    }
}

#[test]
fn valid_and_invalid_character_and_string_escapes_retain_locations() {
    for (source, value) in [
        (r"'\n'", '\n'),
        (r"'\t'", '\t'),
        (r"'\r'", '\r'),
        (r"'\\'", '\\'),
        (r"'\''", '\''),
        (r"'\xFF'", 'ÿ'),
        ("'é'", 'é'),
    ] {
        assert_eq!(
            Lexer::new(source).tokenize().unwrap()[0].token_type,
            TokenType::CharLiteral(value)
        );
    }
    for (source, code, message) in [
        ("\"open", "E1003", "unterminated string"),
        ("\"\\", "E1004", "trailing"),
        (r#""\xG0""#, "E1004", "invalid hex escape"),
        (r#""\xA""#, "E1004", "expected two hex digits"),
        ("''", "E1005", "invalid char literal"),
        ("'ab'", "E1005", "invalid char literal"),
        ("'\\", "E1005", "dangling escape"),
        (r"'\xGG'", "E1005", "invalid hex escape"),
        ("'한'", "E1005", "unsigned 8-bit"),
    ] {
        let source = format!("\n  {source}");
        let error = Lexer::new_with_file(&source, "literal.wave").tokenize().unwrap_err();
        assert_eq!(error.code.as_deref(), Some(code), "{source}: {error:?}");
        assert!(error.message.contains(message), "{source}: {error:?}");
        assert_eq!(error.line, 2);
        assert_eq!(error.span.as_ref().unwrap().file, "literal.wave");
        assert_eq!(error.column, if code == "E1004" { 4 } else { 3 });
    }
}
