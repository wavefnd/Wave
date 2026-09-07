//! Source occurrence identity must survive frontend lowering without text searches.
use lexer::Lexer;
use parser::ast::{ASTNode, Expression, StatementNode};
use parser::generics::monomorphize_generics;
use parser::hir::TypedProgram;
use parser::parse_syntax_with_spans;
use parser::verification::validate_program_detailed;

fn parse(source: &str) -> Vec<ASTNode> {
    let tokens = Lexer::new_with_file(source, "unicode.wave")
        .tokenize()
        .unwrap();
    parse_syntax_with_spans(&tokens).unwrap()
}

#[test]
fn tokens_preserve_raw_spelling_bytes_and_unicode_columns() {
    let source = "// 🙂\r\nfun 이름() { \"a\\n\"; 이름; }";
    let tokens = Lexer::new_with_file(source, "unicode.wave")
        .tokenize()
        .unwrap();
    for token in &tokens {
        let span = token.span.as_ref().unwrap();
        assert_eq!(&source[span.start..span.end], token.lexeme);
        assert_eq!(span.file, "unicode.wave");
        assert_eq!(
            source[..span.start].bytes().filter(|b| *b == b'\n').count() + 1,
            span.line
        );
        assert_eq!(
            source[..span.start]
                .rsplit('\n')
                .next()
                .unwrap()
                .chars()
                .count()
                + 1,
            span.column
        );
    }
    let string = tokens.iter().find(|t| t.lexeme == "\"a\\n\"").unwrap();
    assert_eq!(
        string.span.as_ref().unwrap().end - string.span.as_ref().unwrap().start,
        5
    );
}

#[test]
fn semantic_errors_identify_the_failing_occurrence() {
    let source = "fun main() { var 이름: i32 = 1; 이름; 이름 = \"bad\"; }";
    let diagnostic = validate_program_detailed(&parse(source)).unwrap_err();
    let span = diagnostic.span.unwrap();
    assert_eq!(&source[span.start..span.end], "\"bad\"");
    assert_eq!(span.column, source[..span.start].chars().count() + 1);
    let source = "fun main() { var x: i32 = 1; x; missing; missing; }";
    let diagnostic = validate_program_detailed(&parse(source)).unwrap_err();
    assert_eq!(
        diagnostic.span.unwrap().start,
        source.find("missing").unwrap()
    );
}

#[test]
fn hir_preserves_all_binary_operand_occurrences_and_default_origins() {
    let source = "fun sum(x: i32 = 16) -> i32 { return x + x + x; } fun main() { sum(); }";
    let hir = TypedProgram::lower(monomorphize_generics(parse(source)).unwrap()).unwrap();
    let ASTNode::Function(sum) = &hir.syntax()[0] else {
        panic!()
    };
    let ASTNode::Statement(StatementNode::Return(Some(expression))) = &sum.body[0] else {
        panic!()
    };
    fn visit(hir: &TypedProgram, expression: &Expression, spans: &mut Vec<usize>) {
        let id = hir.expression_id(expression).unwrap();
        let span = hir
            .expression_span(id)
            .expect("every physical expression has a span");
        if let Expression::Variable(_) = expression {
            spans.push(span.start);
        }
        if let Expression::BinaryExpression { left, right, .. } = expression {
            visit(hir, left, spans);
            visit(hir, right, spans);
        }
    }
    let mut spans = vec![];
    visit(&hir, expression, &mut spans);
    assert_eq!(spans.len(), 3);
    assert!(spans.windows(2).all(|w| w[0] < w[1]));
    let ASTNode::Function(main) = &hir.syntax()[1] else {
        panic!()
    };
    let ASTNode::Statement(StatementNode::Expression(Expression::FunctionCall { args, .. })) =
        &main.body[0]
    else {
        panic!()
    };
    let span = hir
        .expression_span(hir.expression_id(&args[0]).unwrap())
        .unwrap();
    assert_eq!(span.start, source.find("16").unwrap());
}

#[test]
fn syntax_errors_point_to_unexpected_token_not_function_start() {
    let source = "fun main() { 1 ? 2; }";
    let tokens = Lexer::new_with_file(source, "unicode.wave")
        .tokenize()
        .unwrap();
    let error = parse_syntax_with_spans(&tokens).unwrap_err();
    assert_eq!(error.span().unwrap().start, source.find('?').unwrap());
}

#[test]
fn target_filter_preserves_byte_offsets_including_crlf_and_unicode() {
    use parser::import::{preprocess_target_attrs, TargetConditionContext};
    let source = "#[target(arch=\"arm64\")]\r\nvariant 이름 {\r\n Value(i32),\r\n}\r\nfun main() { missing; }\r\n";
    let filtered = preprocess_target_attrs(
        source,
        &TargetConditionContext {
            arch: Some("amd64".into()),
            ..Default::default()
        },
    );
    assert_eq!(source.len(), filtered.len());
    let diagnostic = validate_program_detailed(&parse(&filtered)).unwrap_err();
    assert_eq!(
        diagnostic.span.unwrap().start,
        source.find("missing").unwrap()
    );
}

#[test]
fn generic_instances_and_synthetic_nodes_have_explicit_provenance() {
    let source = "fun identity<T>(x: T) -> T { return x; } fun main() { identity<i32>(1); identity<i64>(2); }";
    let hir = TypedProgram::lower(monomorphize_generics(parse(source)).unwrap()).unwrap();
    let mut ids = Vec::new();
    for node in hir.syntax() {
        let ASTNode::Function(f) = node else { continue };
        if !f.name.contains("identity") {
            continue;
        }
        let span = hir.node_span(hir.node_id(node).unwrap()).unwrap();
        assert!(!span.expansion.is_empty());
        let ASTNode::Statement(StatementNode::Return(Some(value))) = &f.body[0] else {
            panic!()
        };
        let id = hir.expression_id(value).unwrap();
        let span = hir.expression_span(id).unwrap();
        assert_eq!(&source[span.start..span.end], "x");
        assert!(!span.expansion.is_empty());
        ids.push(id);
    }
    assert_eq!(ids.len(), 2);
    assert_ne!(ids[0], ids[1]);
    let tokens = Lexer::new("fun main() {}").tokenize().unwrap();
    let synthetic = TypedProgram::lower(parser::parse_syntax_only(&tokens).unwrap()).unwrap();
    assert!(synthetic
        .node_span(synthetic.node_id(&synthetic.syntax()[0]).unwrap())
        .is_none());
}

#[test]
fn variant_pattern_ids_retain_recursive_source_ranges() {
    use parser::ast::MatchPattern;
    let source =
        "variant V { A(i32), B } fun f(v: V) { match (v) { V::A(x) => { x; }, V::B => {} } }";
    let hir = TypedProgram::lower(monomorphize_generics(parse(source)).unwrap()).unwrap();
    let ASTNode::Function(f) = &hir.syntax()[1] else {
        panic!()
    };
    let ASTNode::Statement(StatementNode::Match { arms, .. }) = &f.body[0] else {
        panic!()
    };
    let pattern = &arms[0].pattern;
    let span = hir.pattern_span(hir.pattern_id(pattern).unwrap()).unwrap();
    assert_eq!(&source[span.start..span.end], "V::A(x)");
    let MatchPattern::Variant { payloads, .. } = pattern else {
        panic!()
    };
    let span = hir
        .pattern_span(hir.pattern_id(&payloads[0]).unwrap())
        .unwrap();
    assert_eq!(&source[span.start..span.end], "x");
}
