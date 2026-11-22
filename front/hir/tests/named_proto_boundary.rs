use hir::TypedProgram;
use lexer::Lexer;
use parser::parse_syntax_with_spans;

fn syntax(source: &str) -> Vec<parser::ast::ASTNode> {
    let tokens = Lexer::new_with_file(source, "named-proto.wave").tokenize().unwrap();
    parse_syntax_with_spans(&tokens).unwrap()
}

#[test]
fn named_contracts_are_rejected_at_the_semantic_boundary_with_a_span() {
    for source in [
        "struct Buffer {} proto MissingContract: Buffer { fun value(self) -> i32 { return 7; } } fun main() {}",
        "struct Buffer {} proto First: Buffer { fun value(self) -> i32 { return 7; } } proto Second: Buffer { fun value(self) -> i32 { return 8; } } fun main() {}",
    ] {
        let error = TypedProgram::lower(syntax(source)).unwrap_err();
        assert!(
            error.diagnostic().message.contains("named proto contract"),
            "{}",
            error.diagnostic().message
        );
        assert!(error.diagnostic().span.is_some(), "missing source span");
    }
}

#[test]
fn inherent_proto_methods_still_lower_normally() {
    let source =
        "struct Buffer {} proto Buffer { fun value(self: ptr<Buffer>) -> i32 { return 7; } } fun main() {}";
    TypedProgram::lower(syntax(source)).unwrap();
}
