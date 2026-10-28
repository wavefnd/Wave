//! Regression coverage for incremental Unicode column tracking

use lexer::{token::TokenType, Lexer};

#[test]
fn many_tokens_on_one_line_keep_utf8_columns_and_newline_positions() {
    let count = 8_192;
    for newline in ["\n", "\r\n", "\r"] {
        let source =
            format!("{}café += \"λ🦀\";{newline}  이름 += 1;", "x ".repeat(count));
        let tokens = Lexer::new_with_file(&source, "columns.wave").tokenize().unwrap();
        assert_eq!(tokens.len(), count + 9);

        for (index, spelling, line, column, end_column) in [
            (0, "x", 1, 1, 2),
            (count / 2, "x", 1, count + 1, count + 2),
            (count - 1, "x", 1, count * 2 - 1, count * 2),
            (count, "café", 1, count * 2 + 1, count * 2 + 5),
            (count + 1, "+=", 1, count * 2 + 6, count * 2 + 8),
            (count + 2, "\"λ🦀\"", 1, count * 2 + 9, count * 2 + 13),
            (count + 3, ";", 1, count * 2 + 13, count * 2 + 14),
            (count + 4, "이름", 2, 3, 5),
            (count + 5, "+=", 2, 6, 8),
            (count + 6, "1", 2, 9, 10),
            (count + 7, ";", 2, 10, 11),
            (count + 8, "", 2, 11, 11),
        ] {
            let token = &tokens[index];
            let span = token.span.as_ref().unwrap();
            assert_eq!(token.lexeme, spelling);
            assert_eq!((span.line, span.column), (line, column), "{spelling:?}");
            assert_eq!((span.end_line, span.end_column), (line, end_column), "{spelling:?}");
            assert_eq!(&source[span.start..span.end], spelling);
        }
        assert_eq!(tokens.last().unwrap().token_type, TokenType::Eof);
    }
}

#[test]
fn long_multibyte_comment_counts_scalars_not_bytes() {
    let source = format!("/* {} */ café", "🦀".repeat(4_096));
    let tokens = Lexer::new(&source).tokenize().unwrap();
    let span = tokens[0].span.as_ref().unwrap();

    assert_eq!(span.start, source.find("café").unwrap());
    assert_eq!(span.column, source[..span.start].chars().count() + 1);
    assert_eq!(span.end_column, span.column + 4);
    assert_eq!(tokens[0].lexeme, "café");
}

#[test]
fn long_line_errors_still_point_to_the_right_scalar() {
    let count = 8_192;
    let source = format!("{}é @", "x ".repeat(count));
    let error = Lexer::new_with_file(&source, "bad.wave").tokenize().unwrap_err();
    let span = error.span.as_ref().unwrap();
    assert_eq!(error.code.as_deref(), Some("E1001"));
    assert_eq!((error.line, error.column), (1, count * 2 + 3));
    assert_eq!((span.start, span.end), (source.find('@').unwrap(), source.len()));
    assert_eq!(span.column, count * 2 + 3);
}
