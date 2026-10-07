use hir::TypedProgram;
use lexer::Lexer;
use parser::{generics::monomorphize_generics, parse_syntax_with_spans};

fn validate(source: &str) -> Result<TypedProgram, String> {
    let tokens =
        Lexer::new_with_file(source, "contracts.wave").tokenize().map_err(|e| format!("{e:?}"))?;
    let ast = parse_syntax_with_spans(&tokens).map_err(|e| format!("{e:?}"))?;
    let ast = monomorphize_generics(ast)?;
    TypedProgram::lower(ast).map_err(|e| format!("{e:?}"))
}

#[test]
fn format_checks_happen_before_backend_lowering() {
    for (source, cause) in [
        (r#"fun main() { println("{unknown}", 1); }"#, "unknown"),
        (r#"fun main() { print("{s}", 42); }"#, "incompatible"),
        (r#"fun main() { print("{d}", "text"); }"#, "incompatible"),
        (r#"fun main() { print("{p}", 12); }"#, "incompatible"),
        (r#"fun main() { print("{c}", 1.2); }"#, "incompatible"),
        (r#"fun main() { var n: i32 = 0; input("{x}", n); }"#, "unsupported format placeholder"),
        (r#"fun main() { print("{} {}", 1); }"#, "arguments"),
        (r#"fun main() { println("{}", 1, 2); }"#, "arguments"),
    ] {
        let error = validate(source).unwrap_err();
        assert!(error.contains(cause), "{source}: {error}");
        assert!(!error.contains("<unknown>"), "{error}");
    }
    assert!(validate(r#"fun main() { println("{}", 4294967296); }"#).is_err());
    for source in [
        r#"fun main() { print("{"); println("}"); println("{} {", 1); }"#,
        r#"fun f(x: u64, p: ptr<i32>, s: str) { println("{d} {x} {p} {s}", x, x, p, s); }"#,
        r#"fun f<T>(x: T) { println("{}", x); } fun main() { f<u64>(18446744073709551615); }"#,
    ] {
        validate(source).unwrap();
    }
}

#[test]
fn integer_patterns_are_range_checked_and_deduplicated_in_the_scrutinee_type() {
    for source in [
        "fun f(n: u8) { match (n) { 256 => {} _ => {} } }",
        "fun f(n: i8) { match (n) { 255 => {} _ => {} } }",
        "const N: u64 = 256; fun f(n: u8) { match (n) { N => {} _ => {} } }",
        "const N: i8 = -1; fun f(n: u8) { match (n) { N => {} _ => {} } }",
        "const N: u8 = 255; fun f(n: i8) { match (n) { N => {} _ => {} } }",
    ] {
        let error = validate(source).unwrap_err();
        assert!(error.contains("range") || error.contains("fit"), "{error}");
    }
    for source in [
        "const N: u64 = 1; fun f(n: u8) { match (n) { N => {} 0x01 => {} _ => {} } }",
        "const N: i8 = -1; fun f(n: i8) { match (n) { N => {} 0xff => {} _ => {} } }",
    ] {
        let error = validate(source).unwrap_err();
        assert!(error.contains("duplicate"), "{error}");
    }
    validate("const N: u64 = 255; fun f(n: u8) { match (n) { N => {} _ => {} } }").unwrap();
    validate(
        "fun f(n: u128) { match (n) { 340282366920938463463374607431768211455 => {} _ => {} } }",
    )
    .unwrap();
    let pattern = format!("0x8{}", "0".repeat(255));
    validate(&format!("fun f(n: u1024) {{ match (n) {{ {pattern} => {{}} _ => {{}} }} }}"))
        .unwrap();
}

#[test]
fn invalid_defaulted_method_arguments_are_still_rejected() {
    let prefix = "struct S { fun f(self: S, n: i32 = 1) -> i32 { return n; } }";
    for call in ["s.f(1, 2)", "s.f(\"bad\")"] {
        assert!(validate(&format!("{prefix} fun main() {{ var s: S = S {{}}; {call}; }}")).is_err());
    }
}
