//! Public token spellings, maximal munch, and UTF-8 positions.
use lexer::{
    token::{FloatType, IntegerType, TokenType as T, UnsignedIntegerType},
    Lexer,
};

#[test]
fn keywords_and_types_require_exact_identifier_boundaries() {
    let mut cases = vec![
        ("async", T::Async),
        ("await", T::Await),
        ("fun", T::Fun),
        ("extern", T::Extern),
        ("export", T::Export),
        ("pub", T::Pub),
        ("type", T::Type),
        ("enum", T::Enum),
        ("variant", T::Variant),
        ("static", T::Static),
        ("var", T::Var),
        ("deref", T::Deref),
        ("let", T::Let),
        ("mut", T::Mut),
        ("const", T::Const),
        ("if", T::If),
        ("else", T::Else),
        ("proto", T::Proto),
        ("struct", T::Struct),
        ("while", T::While),
        ("for", T::For),
        ("module", T::Module),
        ("class", T::Class),
        ("in", T::In),
        ("out", T::Out),
        ("clobber", T::Clobber),
        ("is", T::Is),
        ("as", T::As),
        ("asm", T::Asm),
        ("xnand", T::Xnand),
        ("import", T::Import),
        ("return", T::Return),
        ("continue", T::Continue),
        ("break", T::Break),
        ("input", T::Input),
        ("print", T::Print),
        ("println", T::Println),
        ("match", T::Match),
        ("true", T::BoolLiteral(true)),
        ("false", T::BoolLiteral(false)),
        ("null", T::Null),
        ("bool", T::Identifier("bool".into())),
        ("char", T::TypeChar),
        ("byte", T::TypeByte),
        ("str", T::TypeString),
        ("void", T::Identifier("void".into())),
        ("ptr", T::Identifier("ptr".into())),
        ("array", T::Identifier("array".into())),
    ];
    for (word, ty) in [
        ("i8", IntegerType::I8),
        ("i16", IntegerType::I16),
        ("i32", IntegerType::I32),
        ("i64", IntegerType::I64),
        ("i128", IntegerType::I128),
        ("i256", IntegerType::I256),
        ("i512", IntegerType::I512),
        ("i1024", IntegerType::I1024),
        ("isz", IntegerType::ISZ),
    ] {
        cases.push((word, T::TokenTypeInt(ty)));
    }
    for (word, ty) in [
        ("u8", UnsignedIntegerType::U8),
        ("u16", UnsignedIntegerType::U16),
        ("u32", UnsignedIntegerType::U32),
        ("u64", UnsignedIntegerType::U64),
        ("u128", UnsignedIntegerType::U128),
        ("u256", UnsignedIntegerType::U256),
        ("u512", UnsignedIntegerType::U512),
        ("u1024", UnsignedIntegerType::U1024),
        ("usz", UnsignedIntegerType::USZ),
    ] {
        cases.push((word, T::TokenTypeUint(ty)));
    }
    cases.extend([
        ("f32", T::TokenTypeFloat(FloatType::F32)),
        ("f64", T::TokenTypeFloat(FloatType::F64)),
    ]);
    for (word, expected) in cases {
        let source = format!("\n{word} {word}x x{word} {word}_");
        let tokens = Lexer::new(&source).tokenize().unwrap();
        assert_eq!(tokens[0].token_type, expected, "{word}");
        assert_eq!(tokens[0].lexeme, word);
        for token in &tokens[..4] {
            assert_eq!(token.line, 2);
        }
        for token in &tokens[1..4] {
            assert_eq!(token.token_type, T::Identifier(token.lexeme.clone()));
        }
    }
}

#[test]
fn operators_take_the_longest_token_even_at_eof() {
    let cases = [
        ("+", T::Plus),
        ("++", T::Increment),
        ("+=", T::PlusEq),
        ("-", T::Minus),
        ("--", T::Decrement),
        ("-=", T::MinusEq),
        ("->", T::Arrow),
        ("*", T::Star),
        ("*=", T::StarEq),
        ("/", T::Div),
        ("/=", T::DivEq),
        ("%", T::Remainder),
        ("%=", T::RemainderEq),
        ("=", T::Equal),
        ("==", T::EqualTwo),
        ("<", T::Lchevr),
        ("<=", T::LchevrEq),
        ("<<", T::Rol),
        (">", T::Rchevr),
        (">=", T::RchevrEq),
        (">>", T::Ror),
        ("&", T::AddressOf),
        ("&&", T::LogicalAnd),
        ("|", T::BitwiseOr),
        ("||", T::LogicalOr),
        ("!", T::Not),
        ("!=", T::NotEqual),
        ("!&", T::Nand),
        ("!|", T::Nor),
        ("^", T::Xor),
        ("~", T::BitwiseNot),
        ("~^", T::Xnor),
        ("?", T::Condition),
        ("??", T::NullCoalesce),
        (":", T::Colon),
        ("::", T::DoubleColon),
        (";", T::SemiColon),
        (",", T::Comma),
        (".", T::Dot),
        ("(", T::Lparen),
        (")", T::Rparen),
        ("[", T::Lbrack),
        ("]", T::Rbrack),
        ("{", T::Lbrace),
        ("}", T::Rbrace),
    ];
    for (spelling, expected) in cases {
        let source = format!("\n{spelling}");
        let tokens = Lexer::new(&source).tokenize().unwrap();
        assert_eq!(tokens.len(), 2, "{spelling}");
        assert_eq!(tokens[0].token_type, expected, "{spelling}");
        assert_eq!(tokens[0].lexeme, spelling);
        assert_eq!(tokens[0].line, 2);
    }
    for (source, expected) in [
        ("+++", vec![T::Increment, T::Plus]),
        ("???", vec![T::NullCoalesce, T::Condition]),
        (":::=>", vec![T::DoubleColon, T::Colon, T::Equal, T::Rchevr]),
        ("?:", vec![T::Condition, T::Colon]),
        ("<<=", vec![T::Rol, T::Equal]),
    ] {
        let mut tokens = Lexer::new(source).tokenize().unwrap();
        tokens.pop();
        assert_eq!(tokens.iter().map(|t| t.token_type.clone()).collect::<Vec<_>>(), expected);
        assert_eq!(tokens.iter().map(|t| t.lexeme.as_str()).collect::<String>(), source);
    }
}

#[test]
fn unicode_identifiers_and_lookahead_preserve_spelling_and_character_columns() {
    let source = "이름+=é;\n变量!=\"값\";";
    let tokens = Lexer::new_with_file(source, "unicode.wave").tokenize().unwrap();
    for (index, spelling, line, column) in [
        (0, "이름", 1, 1),
        (1, "+=", 1, 3),
        (2, "é", 1, 5),
        (4, "变量", 2, 1),
        (5, "!=", 2, 3),
        (6, "\"값\"", 2, 5),
    ] {
        let t = &tokens[index];
        assert_eq!(t.lexeme, spelling);
        assert_eq!(t.line, line);
        assert_eq!(t.span.as_ref().unwrap().column, column);
        assert_eq!(&source[t.span.as_ref().unwrap().start..t.span.as_ref().unwrap().end], spelling);
    }
    let error = Lexer::new_with_file("é한 @", "unicode.wave").tokenize().unwrap_err();
    assert_eq!(error.line, 1);
    assert_eq!(error.column, 4);
    assert_eq!(error.code.as_deref(), Some("E1001"));
}
