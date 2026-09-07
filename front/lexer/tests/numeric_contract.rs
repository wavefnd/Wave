//! Shared numeric grammar and conversions.
use lexer::number::{parse_float, IntegerLiteral};
use lexer::token::TokenType;
use lexer::Lexer;

#[test]
fn numeric_tokens_and_shared_values_agree() {
    for (raw, value) in [
        ("16", 16),
        ("0x10", 16),
        ("0Xf_F", 255),
        ("0o20", 16),
        ("0O2_0", 16),
        ("0b1_0000", 16),
        ("1_024", 1024),
    ] {
        let number = IntegerLiteral::parse(raw).unwrap();
        assert_eq!(number.to_i128(), Some(value));
        let tokens = Lexer::new(raw).tokenize().unwrap();
        let TokenType::IntLiteral(normalized) = &tokens[0].token_type else {
            panic!()
        };
        assert_eq!(IntegerLiteral::parse(normalized), Some(number));
    }
    assert_eq!(
        IntegerLiteral::parse("-170141183460469231731687303715884105728")
            .unwrap()
            .to_i128(),
        Some(i128::MIN)
    );
    assert_eq!(
        IntegerLiteral::parse("-9223372036854775808")
            .unwrap()
            .to_f64(),
        Some(i64::MIN as f64)
    );
    for (raw, value) in [
        ("1.0", 1.0),
        ("1e3", 1000.0),
        ("1E-3", 0.001),
        ("1_000.2_5e+2", 100025.0),
    ] {
        assert_eq!(parse_float(raw), Some(value));
        assert!(
            matches!(Lexer::new(raw).tokenize().unwrap()[0].token_type,TokenType::Float(n) if n == value)
        );
    }
}

#[test]
fn malformed_numbers_are_one_lexical_error() {
    for raw in [
        "0x", "0b2", "0b102", "0o8", "0xg", "0x_1", "1_", "1__2", "1_.2", "1._2", "1e", "1e+",
        "1e_2", "1e+-2", "1.2.3", "1.", "1u32", "1.0f64", "1e309", "0x1p2", "0b11name",
    ] {
        assert!(Lexer::new(raw).tokenize().is_err(), "{raw}");
    }
}
