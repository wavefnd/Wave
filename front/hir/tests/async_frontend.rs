use hir::TypedProgram;
use lexer::Lexer;
use parser::{
    ast::{ASTNode, WaveType},
    generics::monomorphize_generics,
    parse_syntax_with_spans,
    verification::validate_program,
};
fn syntax(s: &str) -> Vec<ASTNode> {
    parse_syntax_with_spans(&Lexer::new_with_file(s, "async.wave").tokenize().unwrap()).unwrap()
}

fn typed(s: &str) -> TypedProgram {
    TypedProgram::lower(monomorphize_generics(syntax(s)).unwrap()).unwrap()
}
#[test]
fn async_and_await_preserve_types_spans_and_specialization() {
    let source="pub async fun value<T>(x:T) -> T { return x; }\nasync fun use_value() -> i64 { return await value<i64>(42); }\nfun main() {}";
    let program = typed(source);
    let sites = program.await_sites();
    assert_eq!(sites.len(), 1);
    assert_eq!(sites[0].1, WaveType::Int(64));
    let span = program.expression_span(sites[0].0).unwrap();
    assert_eq!(&source[span.start..span.start + 5], "await");
    let function = program
        .syntax()
        .iter()
        .find_map(|n| {
            if let ASTNode::Function(f) = n {
                if f.name == "use_value" {
                    Some(f)
                } else {
                    None
                }
            } else {
                None
            }
        })
        .unwrap();
    let plan = hir::async_lower::plan(&program, function).unwrap();
    assert!(plan
        .states
        .iter()
        .any(|s| matches!(s.transition, hir::async_lower::Transition::Await { .. })));
    // The ordinary output is itself type checked before any backend consumes it.
    TypedProgram::lower(hir::async_lower::lower_program(&program).unwrap()).unwrap();
}
#[test]
fn rejects_invalid_async_declarations_and_operands() {
    for source in [
        "async main() {}",
        "async fun main() {}",
        "pub async fun main() {}",
        "export(c) async fun work() {}",
        "async fun f() { await ; }",
    ] {
        let tokens = Lexer::new_with_file(source, "bad.wave").tokenize().unwrap();
        let error = parse_syntax_with_spans(&tokens).unwrap_err();
        assert!(error.span().is_some(), "{source}: {error:?}");
    }
    for (source, needle) in [
        ("fun f() { await 1; }", "only valid inside"),
        ("async fun f() { await 1; }", "requires a Future"),
        ("async fun f() -> i64 { return 1; } fun main(){var v:i64=f();}", "Future<i64>"),
        ("extern(c) fun send(f:Future<i64>); fun main(){}", "FFI"),
        ("type Bad = Future<Bad>; fun main(){}", "cyclic"),
    ] {
        let error = validate_program(&syntax(source)).unwrap_err();
        assert!(error.contains(needle), "{source}: {error}");
    }
}
#[test]
fn await_is_not_an_lvalue_and_short_circuit_has_distinct_states() {
    let program=typed("async fun flag()->bool{return true;} async fun f()->bool{return false && await flag();} fun main(){}");
    let f = program
        .syntax()
        .iter()
        .find_map(|n| match n {
            ASTNode::Function(f) if f.name == "f" => Some(f),
            _ => None,
        })
        .unwrap();
    let plan = hir::async_lower::plan(&program, f).unwrap();
    assert!(plan
        .states
        .iter()
        .any(|s| matches!(s.transition, hir::async_lower::Transition::Branch(..))));
    let bad = syntax(
        "async fun f()->i32{return 1;} async fun g(){var p:ptr<i32> = &(await f());} fun main(){}",
    );
    assert!(validate_program(&bad).unwrap_err().contains("non-lvalue"));
}
