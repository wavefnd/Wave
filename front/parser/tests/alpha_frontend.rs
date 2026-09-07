//! Alpha grammar regressions: malformed source must never be silently accepted.
use lexer::Lexer;
use parser::ast::{ASTNode, Expression, Literal};
use parser::generics::monomorphize_generics;
use parser::hir::TypedProgram;
use parser::import::{preprocess_target_attrs, TargetConditionContext};
use parser::parse_syntax_only;

fn syntax(src: &str) -> Result<Vec<ASTNode>, String> {
    let tokens = Lexer::new(src).tokenize().map_err(|e| format!("{e:?}"))?;
    parse_syntax_only(&tokens).map_err(|e| format!("{e:?}"))
}

#[test]
fn rejects_unterminated_and_unsupported_statements_at_every_depth() {
    for body in [
        "1 ? 2;",
        "ping() ping();",
        "return",
        "return 1",
        "break",
        "continue",
        "asm { ? }",
        "var x: i32 = asm { ? };",
    ] {
        for nested in [false, true] {
            let body = if nested {
                format!("if (true) {{ {body} }}")
            } else {
                body.into()
            };
            assert!(
                syntax(&format!("fun main() {{ {body} }}")).is_err(),
                "{body}"
            );
        }
    }
    assert!(syntax("fun main() { asm { \"nop\"").is_err());
    assert!(syntax("fun main() { var x: i32 = asm { \"nop\"").is_err());
}

#[test]
fn expression_statements_have_the_same_grammar_in_all_blocks() {
    for expression in [
        "true", "false", "!true", "~1", "-1", "+1", "&x", "deref p", "++x", "--x", "x++", "x = 2",
        "x += 2", "(1)", "[1, 2]", "null", "'a'", "\"text\"", "call()",
    ] {
        for depth in 0..=3 {
            let mut body = format!("{expression};");
            for _ in 0..depth {
                body = format!("if (true) {{ {body} }}");
            }
            syntax(&format!("fun main() {{ {body} }}")).unwrap_or_else(|e| panic!("{body}: {e}"));
        }
    }
}

#[test]
fn numeric_defaults_preserve_radix_and_large_values() {
    for value in [
        "0x10",
        "0b10000",
        "0o20",
        "16",
        "1_024",
        "-0x10",
        "18446744073709551616",
    ] {
        let nodes = syntax(&format!(
            "fun value(x: i128 = {value}) -> i128 {{ return x; }} fun main() {{ value(); }}"
        ))
        .unwrap();
        let ASTNode::Function(f) = &nodes[0] else {
            panic!()
        };
        assert!(matches!(
            &f.parameters[0].initial_value,
            Some(Expression::Literal(Literal::Int(_)))
        ));
        TypedProgram::lower(nodes.clone()).expect("defaults validate before expansion");
        TypedProgram::lower(monomorphize_generics(nodes).unwrap())
            .expect("defaults validate after expansion");
    }
    for declaration in [
        "x: i8 = 128",
        "x: u8 = -1",
        "x: i64 = 9999999999999999999999999999999999999999999999999999999999999",
        "x: i8 = 1, y: i8",
    ] {
        assert!(
            TypedProgram::lower(syntax(&format!("fun f({declaration}) {{}} ")).unwrap()).is_err(),
            "{declaration}"
        );
    }
    for declaration in [
        "x: i32 = 0x",
        "x: i32 = 0b102",
        "x: i32 = 12u8",
        "x: i32 = 1__2",
        "x: i32 =",
        "x i32",
        "x: i32 y: i32",
        "x: i32 = unknown",
    ] {
        assert!(
            syntax(&format!("fun f({declaration}) {{}} ")).is_err(),
            "{declaration}"
        );
    }
}

#[test]
fn target_filter_removes_complete_multiline_declarations() {
    let target = TargetConditionContext {
        arch: Some("amd64".into()),
        os: Some("linux".into()),
        ..Default::default()
    };
    for declaration in [
        "variant Choice {\n Empty,\n Value(i32),\n}",
        "pub variant Choice {\n Empty,\n Value(i32),\n}",
        "struct Pair {\n x: i32;\n}",
        "enum Mode {\n A,\n B,\n}",
        "fun f() {\n return;\n}",
        "type Item = i32;",
        "const ITEM: i32 = 1;",
        "static item: i32 = 1;",
        "import(\"absent\");",
        "extern(\"C\") fun f();",
        "export(\"C\") fun f() {\n return;\n}",
        "proto Pair {\n fun f(self: ptr<Pair>) {}\n}",
    ] {
        let filtered = preprocess_target_attrs(
            &format!("#[target(arch=\"arm64\")]\n{declaration}\nfun main() {{}}"),
            &target,
        );
        let nodes = syntax(&filtered).unwrap_or_else(|e| panic!("{declaration}: {e}"));
        assert_eq!(nodes.len(), 1, "{declaration}: {filtered}");
        assert!(matches!(&nodes[0], ASTNode::Function(f) if f.name == "main"));
    }
}

#[test]
fn target_sized_types_resolve_recursively_without_host_assumptions() {
    use parser::ast::WaveType;
    use parser::hir::resolve_target_types;
    use parser::types::{parse_type, token_type_to_wave_type};
    assert_eq!(
        token_type_to_wave_type(&parse_type("isz").unwrap()),
        Some(WaveType::Isz)
    );
    assert_eq!(
        token_type_to_wave_type(&parse_type("usz").unwrap()),
        Some(WaveType::Usz)
    );
    for invalid in [
        "i0",
        "i1",
        "i24",
        "u7",
        "u2048",
        "i9999999999999",
        "f16",
        "f128",
        "i032",
    ] {
        assert!(parse_type(invalid).is_none(), "{invalid}");
        assert!(
            syntax(&format!("fun f(x: {invalid}) {{}} ")).is_err(),
            "{invalid}"
        );
    }
    for name in ["item", "user", "file", "이름", "pkg::item"] {
        assert!(parse_type(name).is_some());
    }
    let source = "struct Box<T> { value: T; } fun id<T>(x: T) -> T { return x; } fun f(x: ptr<array<isz,2>>, y: Box<usz>) -> isz { var z: isz = id<isz>(16); return z; }";
    for bits in [32, 64] {
        let mut nodes = syntax(source).unwrap();
        resolve_target_types(&mut nodes, bits).unwrap();
        let ASTNode::Function(f) = &nodes[2] else {
            panic!()
        };
        assert_eq!(f.return_type, Some(WaveType::Int(bits)));
        assert_eq!(
            f.parameters[0].param_type,
            WaveType::Pointer(Box::new(WaveType::Array(Box::new(WaveType::Int(bits)), 2)))
        );
        assert_eq!(
            f.parameters[1].param_type,
            WaveType::Struct(format!("Box<u{bits}>"))
        );
        TypedProgram::lower(monomorphize_generics(nodes).unwrap()).unwrap();
    }
}

#[test]
fn never_returning_functions_must_terminate_without_returning() {
    for source in [
        "fun stop() -> ! { while (true) {} } fun value() -> i32 { stop(); }",
        "fun stop() -> ! { while (true) {} } fun forward() -> ! { stop(); }",
    ] {
        TypedProgram::lower(syntax(source).unwrap()).unwrap();
    }
    for source in [
        "fun stop() -> ! {}",
        "fun stop() -> ! { return; }",
        "fun stop() -> ! { return 1; }",
        "fun stop() -> ! { while (true) { break; } }",
        "fun bad(x: !) {}",
        "fun bad() { var x: !; }",
        "type Bad = !;",
    ] {
        assert!(
            TypedProgram::lower(syntax(source).unwrap()).is_err(),
            "{source}"
        );
    }
}

#[test]
fn asm_operands_keep_casts_and_projections_instead_of_skipping_tokens() {
    let nodes = syntax("fun f() { asm { in(\"r\") p as i64 out(\"r\") value.field } }").unwrap();
    let ASTNode::Function(f) = &nodes[0] else {
        panic!()
    };
    let ASTNode::Statement(parser::ast::StatementNode::AsmBlock {
        inputs, outputs, ..
    }) = &f.body[0]
    else {
        panic!()
    };
    assert!(matches!(inputs[0].1, Expression::Cast { .. }));
    assert!(matches!(outputs[0].1, Expression::FieldAccess { .. }));
}
