use hir::TypedProgram;
use lexer::Lexer;
use parser::generics::monomorphize_generics;
use parser::{ast::ASTNode, parse_syntax_with_spans};

fn specialize(source: &str) -> Result<Vec<ASTNode>, String> {
    let tokens = Lexer::new_with_file(source, "methods.wave").tokenize().unwrap();
    monomorphize_generics(parse_syntax_with_spans(&tokens).unwrap())
}

#[test]
fn specializes_struct_and_proto_methods_with_enclosing_and_inferred_types() {
    let ast = specialize(
        r#"
        struct Box<T> {
            value: T;
            fun choose<U>(self: ptr<Box<T>>, value: U) -> U { return value; }
        }
        struct Plain { value: i32; }
        proto Plain { fun choose<U>(self: ptr<Plain>, value: U) -> U { return value; } }
        fun main() -> i32 {
            var box: Box<i32> = Box<i32> { value: 2 };
            var plain: Plain = Plain { value: 1 };
            var a: i64 = (&box).choose<i64>(4);
            var b: i64 = (&box).choose<i64>(5);
            var c: i32 = (&plain).choose(6);
            return c;
        }
    "#,
    )
    .unwrap();
    let methods = ast.iter().filter(|n| matches!(n.unspanned(), ASTNode::Function(f) if f.name.starts_with("$method$") && f.generic_params.is_empty())).count();
    assert_eq!(methods, 2, "identical calls share one instance");
    TypedProgram::lower(ast).unwrap();
}

#[test]
fn generic_method_failures_are_bounded_and_descriptive() {
    for (source, message) in [
        ("struct S<T> { fun f<T>(self: ptr<S<T>>, x: T) -> T { return x; } }", "duplicate generic parameter"),
        ("struct S { fun f<T>(self: ptr<S>) -> ptr<T> { return null; } } fun main() { var s: S = S {}; (&s).f(); }", "cannot infer generic parameter"),
        ("struct S { fun f<T>(self: ptr<S>, a: T, b: T) {} } fun main() { var s: S = S {}; var x: i64 = 1; (&s).f(1, x); }", "conflicting inferred types"),
        ("struct S { fun f<T>(self: ptr<S>, x: T) -> T { return x; } } fun main() { var s: S = S {}; (&s).f<i32, i64>(1); }", "expects 1 generic argument"),
        ("struct S { fun f<T>(self: ptr<S>, x: T) -> T { return x; } } fun main() { var s: S = S {}; (&s).f<i32>(\"bad\"); }", "type mismatch"),
    ] {
        let error = specialize(source).unwrap_err();
        assert!(error.contains(message), "{error}");
    }
}

#[test]
fn recursive_generic_methods_reuse_instances_and_expanding_recursion_is_rejected() {
    let ast = specialize(
        r#"
        struct S { fun recur<T>(self: ptr<S>, x: T, count: i32) -> T {
            if (count == 0) { return x; }
            return self.recur<T>(x, count - 1);
        } }
        fun main() -> i32 { var s: S = S {}; return (&s).recur<i32>(7, 3); }
    "#,
    )
    .unwrap();
    TypedProgram::lower(ast).unwrap();
    let error = specialize(
        r#"
        struct S { fun grow<T>(self: ptr<S>) { self.grow<ptr<T>>(); } }
        fun main() { var s: S = S {}; (&s).grow<i32>(); }
    "#,
    )
    .unwrap_err();
    assert!(error.contains("instantiation depth"), "{error}");
}

#[test]
fn nested_method_receivers_have_resolved_hir_types() {
    use hir::HirExpressionType;
    use parser::ast::{Expression, WaveType};
    let ast = specialize(
        r#"
        struct S {
            value: i32;
            fun identity(self: ptr<S>) -> ptr<S> { return self; }
            fun read(self: ptr<S>) -> i32 { return self.value; }
        }
        fun main() -> i32 {
            var s: S = S { value: 4 };
            var result: i32 = (&s).identity().identity().read();
            return result;
        }
    "#,
    )
    .unwrap();
    let program = TypedProgram::lower(ast).unwrap();
    let ASTNode::Function(main) = &program.syntax()[1] else {
        panic!("main");
    };
    let ASTNode::Variable(result) = &main.body[1] else {
        panic!("result");
    };
    let expression = result.initial_value.as_ref().unwrap();
    assert_eq!(program.type_of(expression), Some(&HirExpressionType::Resolved(WaveType::Int(32))));
    let Expression::MethodCall { object, .. } = expression else {
        panic!("read call");
    };
    let pointer =
        HirExpressionType::Resolved(WaveType::Pointer(Box::new(WaveType::Struct("S".into()))));
    assert_eq!(program.type_of(object), Some(&pointer));
    let Expression::MethodCall { object, .. } = object.as_ref() else {
        panic!("identity call");
    };
    assert_eq!(program.type_of(object), Some(&pointer));
}

#[test]
fn invalid_chains_point_at_the_first_failing_member() {
    let source = "struct S {} fun main() { var s: S = S {}; (&s).missing().later(); }";
    let ast = specialize(source).unwrap();
    let error = TypedProgram::lower(ast).unwrap_err();
    let diagnostic = error.diagnostic();
    assert!(diagnostic.message.contains("missing"), "{diagnostic:?}");
    let span = diagnostic.span.as_ref().unwrap();
    let focus = span.focus.as_deref().unwrap_or(span);
    assert_eq!(&source[focus.start..focus.end], "missing");
}

#[test]
fn finite_deep_generic_method_chains_do_not_use_the_compiler_call_stack() {
    let mut source = String::from("struct S {");
    for index in 0..40 {
        source.push_str(&format!("fun step{index}<T>(self: ptr<S>, x: T) -> T {{ "));
        if index == 39 {
            source.push_str("return x; }");
        } else {
            source.push_str(&format!("return self.step{}<T>(x); }}", index + 1));
        }
    }
    source.push_str("} fun main() -> i32 { var s: S = S {}; return (&s).step0<i32>(7); }");
    TypedProgram::lower(specialize(&source).unwrap()).unwrap();
}
