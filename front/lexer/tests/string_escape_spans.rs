//! Invalid string escapes identify the consumed escape, including its backslash.
use lexer::{token::TokenType, Lexer};

#[test]
fn invalid_string_escapes_preserve_exact_utf8_ranges() {
    for (escape, suffix) in [
        ("\\q", "\"; }"),
        ("\\한", "\"; }"),
        ("\\xGG", "\"; }"),
        ("\\x", "\"; }"),
        ("\\xA", "\"; }"),
        ("\\xG", "\"; }"),
        ("\\x", "\r\nnext line"),
        ("\\xA", ""),
        ("\\x", ""),
        ("\\", ""),
    ] {
        for prefix in ["\"", "// 한글\r\nfun main() { \"앞"] {
            let source = format!("{prefix}{escape}{suffix}");
            let error = Lexer::new_with_file(&source, "escape.wave").tokenize().unwrap_err();
            assert_eq!(error.code.as_deref(), Some("E1004"), "{error:?}");
            let span = error.span.as_ref().unwrap();
            assert_eq!(span.file, "escape.wave");
            assert_eq!(span.start, prefix.len(), "{source:?}: {error:?}");
            assert_eq!(span.end, prefix.len() + escape.len(), "{source:?}: {error:?}");
            assert_eq!(&source[span.start..span.end], escape);
            assert_eq!(span.line, prefix.bytes().filter(|b| *b == b'\n').count() + 1);
            assert_eq!(span.column, prefix.rsplit('\n').next().unwrap().chars().count() + 1);
            assert_eq!(error.column, span.column);
            assert_eq!(error.span_len, escape.chars().count());
        }
    }
}

#[test]
fn supported_string_escapes_keep_their_values() {
    let source = r#""앞\n\t\r\\\"\x41""#;
    let tokens = Lexer::new(source).tokenize().unwrap();
    assert_eq!(tokens[0].token_type, TokenType::String("앞\n\t\r\\\"A".into()));
}
