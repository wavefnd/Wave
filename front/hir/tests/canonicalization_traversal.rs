//! The extracted HIR helpers preserve the public target-width and identity contracts.
use hir::{resolve_target_types, TypedProgram};
use lexer::Lexer;
use parser::ast::{ASTNode, MatchPattern, StatementNode, WaveType};
use parser::generics::monomorphize_generics;
use parser::{parse_syntax_only, parse_syntax_with_spans};

#[test]
fn target_width_resolution_keeps_its_public_api_and_values() {
    for (width, signed, unsigned) in [
        (32, WaveType::Int(32), WaveType::Uint(32)),
        (64, WaveType::Int(64), WaveType::Uint(64)),
    ] {
        let tokens = Lexer::new("type Signed = isz; type Unsigned = usz;").tokenize().unwrap();
        let mut nodes = parse_syntax_only(&tokens).unwrap();
        resolve_target_types(&mut nodes, width).unwrap();
        let ASTNode::TypeAlias(s) = nodes[0].unspanned() else {
            panic!("signed alias")
        };
        let ASTNode::TypeAlias(u) = nodes[1].unspanned() else {
            panic!("unsigned alias")
        };
        assert_eq!(s.target, signed);
        assert_eq!(u.target, unsigned);
        assert!(resolve_target_types(&mut nodes, 16).is_err());
    }
}

#[test]
fn nested_patterns_keep_depth_first_ids_and_source_spans() {
    let source =
        "variant V { A(i32), B } fun f(v: V) { match (v) { V::A(x) => { x; }, V::B => {} } }";
    let tokens = Lexer::new_with_file(source, "patterns.wave").tokenize().unwrap();
    let syntax = parse_syntax_with_spans(&tokens).unwrap();
    let program = TypedProgram::lower(monomorphize_generics(syntax).unwrap()).unwrap();

    let ASTNode::Function(function) = &program.syntax()[1] else {
        panic!("function")
    };
    let ASTNode::Statement(StatementNode::Match { arms, .. }) = &function.body[0] else {
        panic!("match");
    };
    let outer = &arms[0].pattern;
    let MatchPattern::Variant { payloads, .. } = outer else {
        panic!("outer pattern")
    };
    let nested = &payloads[0];
    let sibling = &arms[1].pattern;

    let outer_id = program.pattern_id(outer).unwrap();
    let nested_id = program.pattern_id(nested).unwrap();
    let sibling_id = program.pattern_id(sibling).unwrap();
    assert_eq!(nested_id.index(), outer_id.index() + 1);
    assert_eq!(sibling_id.index(), nested_id.index() + 1);
    for (id, spelling) in [
        (outer_id, "V::A(x)"),
        (nested_id, "x"),
        (sibling_id, "V::B"),
    ] {
        let span = program.pattern_span(id).unwrap();
        assert_eq!(&source[span.start..span.end], spelling);
        assert_eq!(span.file, "patterns.wave");
    }
}
