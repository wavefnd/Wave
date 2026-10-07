use lexer::Lexer;
use parser::parse_syntax_with_spans;

fn failure(marked: &str, expected: &str, context: &str) {
    let marked = format!("// 한글\r\n{marked}");
    let start = marked.find('@').unwrap();
    let source = marked.replace('@', "");
    let tokens = Lexer::new_with_file(&source, "diagnostics.wave").tokenize().unwrap();
    let error = parse_syntax_with_spans(&tokens).unwrap_err();
    assert_eq!(error.span().unwrap().start, start, "{source}: {error:?}");
    assert_eq!(error.expected(), [expected], "{source}: {error:?}");
    assert_eq!(error.context(), Some(context), "{source}: {error:?}");
}

#[test]
fn declaration_errors_keep_the_failing_token_for_private_and_public_forms() {
    for (source, expected, context) in [
        ("type @= i32;", "identifier", "type alias"),
        ("type T @i32;", "'='", "type alias"),
        ("type T = @;", "type", "type alias"),
        ("type T = i32 @type U = i64;", "';'", "type alias"),
        ("type T = @ptr<,>;", "type", "type alias"),
        ("enum @-> i32 {}", "identifier", "enum declaration"),
        ("enum E @i32 {}", "'->'", "enum declaration"),
        ("enum E -> @{}", "type", "enum representation type"),
        ("enum E -> i32 @X", "'{'", "enum declaration"),
        ("enum E -> i32 { @1 }", "identifier", "enum case"),
        ("enum E -> i32 { A = @true }", "integer literal", "enum case value"),
        ("enum E -> i32 { A @B }", "',' or '}'", "enum declaration"),
        ("variant @{ A }", "identifier", "variant declaration"),
        ("variant V @A", "'{'", "variant declaration"),
        ("variant V<@> { A }", "identifier", "generic parameters"),
        ("variant V<T @U> { A }", "',' or '>'", "generic parameters"),
        ("variant V { @1 }", "identifier", "variant case"),
        ("variant V { A(@,) }", "type", "variant payload type"),
        ("variant V { A(i32 @i64) }", "',' or ')'", "variant payload"),
        ("variant V { A @B }", "',' or '}'", "variant declaration"),
        ("variant V { A(i32@", "',' or ')'", "variant payload"),
        ("enum E -> i32 { A@", "',' or '}'", "enum declaration"),
    ] {
        failure(source, expected, context);
        failure(&format!("pub {source}"), expected, context);
    }
}

#[test]
fn both_asm_forms_share_precise_clause_errors() {
    for (body, expected, context) in [
        ("@;", "'{'", "asm block"),
        ("{ @123 }", "instruction string, in, out, clobber, or '}'", "asm block"),
        ("{ in @rax }", "'('", "asm input clause"),
        ("{ in(@123) value }", "register string or identifier", "asm input clause"),
        ("{ in(rax @value }", "')'", "asm input clause"),
        ("{ out(rax) @123 }", "assignable expression", "asm output clause"),
        ("{ clobber @rax }", "'('", "asm clobber clause"),
        ("{ clobber(@1) }", "register string or identifier", "asm clobber clause"),
        ("{ clobber(rax @rcx) }", "',' or ')'", "asm clobber clause"),
        ("{ clobber(rax, @) }", "register string or identifier", "asm clobber clause"),
    ] {
        failure(&format!("fun f() {{ asm {body}; }}"), expected, context);
        failure(&format!("fun f() {{ var x: i32 = asm {body}; }}"), expected, context);
    }
    failure("fun f() { asm { @", "'}'", "asm block");
    failure("fun f() { var x: i32 = asm { @", "'}'", "asm block");
}

#[test]
fn valid_declarations_and_asm_keep_their_existing_syntax() {
    let source = r#"
        pub type P = ptr<array<i32, 0x10>>;
        enum E -> i32 { A = -1, B = 0x10, C, }
        pub variant V<T,> { Empty, Pair(T, ptr<T>,), Unit(), }
        fun f() {
            var x: i64 = 0;
            asm { "nop"; in(rax) x + 1, out("rax") x clobber() }
            var y: i64 = asm { "nop" in("rax") x out(rax) x clobber("memory", rcx) };
        }
    "#;
    let tokens = Lexer::new(source).tokenize().unwrap();
    parse_syntax_with_spans(&tokens).unwrap();
}
