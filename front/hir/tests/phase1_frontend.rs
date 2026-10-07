use hir::TypedProgram;
use lexer::{token::TokenType, Lexer};
use parser::{
    ast::WaveType,
    parse_syntax_with_spans,
    parser::types::{parse_type, split_top_level_generic_args, token_type_to_wave_type},
};

fn syntax_error(marked: &str, context: &str, expected: Option<&str>) {
    let start = marked.find('@').unwrap();
    let source = marked.replace('@', "");
    let tokens = Lexer::new_with_file(&source, "phase1.wave").tokenize().unwrap();
    let error = parse_syntax_with_spans(&tokens).unwrap_err();
    assert_eq!(error.context(), Some(context), "{source}: {error:?}");
    assert_eq!(error.span().unwrap().start, start, "{source}: {error:?}");
    if let Some(expected) = expected {
        assert_eq!(error.expected(), [expected], "{error:?}");
    }
}

#[test]
fn local_storage_failures_identify_the_qualifier() {
    for qualifier in ["const", "static"] {
        syntax_error(
            &format!("fun f() {{ @{qualifier} x: i32 = 1; }}"),
            "block statement",
            Some("var declaration or expression"),
        );
        syntax_error(
            &format!("fun f() {{ for (@{qualifier} x: i32 = 0; x < 1; x += 1) {{}} }}"),
            "for initializer",
            Some("var declaration or expression"),
        );
    }
    let tokens =
        Lexer::new("const c: i32 = 1; static s: i32 = 2; fun main() {}").tokenize().unwrap();
    TypedProgram::lower(parse_syntax_with_spans(&tokens).unwrap()).unwrap();
}

#[test]
fn malformed_match_patterns_and_arms_retain_their_local_cause() {
    for (body, expected, context) in [
        ("(1 @{}", "')'", "match header"),
        ("(x) @return;", "'{'", "match header"),
        ("(x) { V::@=> {} }", "identifier", "match pattern"),
        ("(x) { V::A(a @b) => {} }", "',' or ')'", "variant pattern"),
        ("(x) { 1 @{} }", "'=>'", "match arm"),
        ("(x) { 1 = @{} }", "'=>'", "match arm"),
        ("(x) { 1 => @return; }", "'{'", "match arm body"),
        ("(x) { V::A(@,) => {} }", "integer literal, case name, or '_'", "match pattern"),
    ] {
        syntax_error(&format!("fun f() {{ match {body} }}"), context, Some(expected));
    }
    syntax_error("fun f() { match (x) { _ => {} @_ => {} } }", "match pattern", None);
}

#[test]
fn extern_types_preserve_the_type_start_in_single_and_block_forms() {
    for (declaration, context) in [
        ("fun f(x: @ptr<,>);", "extern parameter type"),
        ("fun f(@i24);", "extern parameter type"),
        ("fun f() -> @array<i32, -1>;", "extern return type"),
    ] {
        syntax_error(&format!("extern(c) {declaration}"), context, Some("type"));
        syntax_error(&format!("extern(c) {{ {declaration} }}"), context, Some("type"));
    }
}

#[test]
fn aliases_are_transparent_for_reads_writes_and_invalid_accesses() {
    let source = r#"
        struct Pair { value: i32; }
        type Values = array<i32, 2>; type Values2 = Values;
        type IntPtr = ptr<i32>; type IntPtr2 = IntPtr;
        type Alias = Pair; type Alias2 = Alias;
        fun main() -> i32 {
            var values: Values2 = [10, 20]; values[1] = 21;
            var p: IntPtr2 = &values[0]; deref p = 11; p[1] = 22;
            var pair: Alias2 = Pair { value: 1 }; pair.value = 2;
            var pp: ptr<Alias2> = &pair; pp.value = 3;
            return values[0] + values[1] + pair.value - 36;
        }
    "#;
    let tokens = Lexer::new(source).tokenize().unwrap();
    TypedProgram::lower(parse_syntax_with_spans(&tokens).unwrap()).unwrap();
    for (source, message) in [
        ("type N = i32; fun f(x: N) { x[0]; }", "index access requires"),
        ("type N = i32; fun f(x: N) { deref x; }", "deref expects"),
        ("struct S { a: i32; } type A = S; fun f(x: A) { x.missing; }", "has no field"),
    ] {
        let tokens = Lexer::new(source).tokenize().unwrap();
        let error = TypedProgram::lower(parse_syntax_with_spans(&tokens).unwrap()).unwrap_err();
        assert!(format!("{error:?}").contains(message), "{error:?}");
    }
}

#[test]
fn main_never_accepts_parameters_even_with_defaults() {
    for params in ["x: i32", "x: i32 = 42"] {
        let source = format!("fun main({params}) -> i32 {{ return x; }}");
        let tokens = Lexer::new(&source).tokenize().unwrap();
        let error = TypedProgram::lower(parse_syntax_with_spans(&tokens).unwrap()).unwrap_err();
        assert!(format!("{error:?}").contains("must have zero parameters"));
    }
    for source in [
        "fun main() {}",
        "fun main() -> i32 { return 0; }",
        "fun helper(x: i32 = 42) -> i32 { return x; } fun main() -> i32 { return helper() - 42; }",
    ] {
        let tokens = Lexer::new(source).tokenize().unwrap();
        TypedProgram::lower(parse_syntax_with_spans(&tokens).unwrap()).unwrap();
    }
}

#[test]
fn nested_generic_type_grammar_is_exact_and_rejects_malformed_input() {
    assert_eq!(
        parse_type("ptr<array<i32, 0x10>>"),
        Some(TokenType::TypePointer(Box::new(TokenType::TypeArray(
            Box::new(TokenType::TypeInt(32)),
            16
        ))))
    );
    assert_eq!(
        token_type_to_wave_type(&parse_type("pkg::Pair<ptr<u8>, array<other::T, 2>>").unwrap()),
        Some(WaveType::Struct("pkg::Pair<ptr<u8>, array<other::T, 2>>".into()))
    );
    assert_eq!(
        split_top_level_generic_args("ptr<array<i32,2>>, pkg::T<u8,u16>"),
        Some(vec!["ptr<array<i32,2>>".into(), "pkg::T<u8,u16>".into()])
    );
    for ty in [
        "",
        "ptr<>",
        "ptr<i32, u8>",
        "ptr<i32",
        "array<i32,-1>",
        "array<i32,4294967296>",
        "array<i32,1,2>",
        "X<,i32>",
        "X<i32,>",
        "X<i32,,u8>",
        "X<ptr<i32>>tail",
        "pkg::",
        "X<Y<i32>",
    ] {
        assert!(parse_type(ty).is_none(), "{ty}");
    }
    for ty in ["i32", "bool", "void", "pkg::T", "Future<ptr<array<u8,2>>>"] {
        assert!(parse_type(ty).is_some(), "{ty}");
    }
}

#[test]
fn byte_literals_in_text_only_constructs_fail_before_codegen() {
    for source in [
        r#"fun f() { print("{\xFF}", 1); }"#,
        r#"fun f() { asm { "\xFF" } }"#,
        r#"fun f() { asm { "nop" clobber("\xFF") } }"#,
        r#"extern(c) fun f() "\xFF";"#,
    ] {
        let tokens = Lexer::new(source).tokenize().unwrap();
        let error = parse_syntax_with_spans(&tokens).unwrap_err();
        assert!(error.message().contains("UTF-8"), "{error:?}");
    }
}
