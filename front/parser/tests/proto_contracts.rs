//! Named proto syntax is parsed without changing inherent method attachments.
use lexer::Lexer;
use parser::{
    ast::{ASTNode, Visibility, WaveType},
    parse_syntax_with_spans,
};

fn parse(source: &str) -> Vec<ASTNode> {
    let tokens = Lexer::new_with_file(source, "proto.wave").tokenize().unwrap();
    parse_syntax_with_spans(&tokens).unwrap()
}

fn failure(source: &str, expected: &str) {
    let tokens = Lexer::new_with_file(source, "proto.wave").tokenize().unwrap();
    let error = parse_syntax_with_spans(&tokens).unwrap_err();
    assert!(
        error.expected().iter().any(|item| item.contains(expected))
            || error.message().contains(expected),
        "{source}: {error:?}"
    );
    assert!(error.span().is_some(), "{source}: missing source span");
}

#[test]
fn named_and_inherent_proto_forms_remain_distinct() {
    let source = "proto Printable : Buffer { fun render(self) { return; } }
                  proto Buffer { fun clear(self: ptr<Buffer>) {} }";
    let nodes = parse(source);
    assert_eq!(nodes.len(), 2);
    let ASTNode::ProtoImpl(named) = nodes[0].unspanned() else { panic!("named proto") };
    assert_eq!(named.name.as_deref(), Some("Printable"));
    assert_eq!(named.target, "Buffer");
    assert_eq!(named.visibility, Visibility::Private);
    assert_eq!(named.methods.len(), 1);
    assert_eq!(named.methods[0].parameters[0].name, "self");
    assert_eq!(named.methods[0].parameters[0].param_type, WaveType::Struct("Buffer".into()));
    assert!(!named.methods[0].body.is_empty());

    let ASTNode::ProtoImpl(inherent) = nodes[1].unspanned() else { panic!("inherent proto") };
    assert_eq!(inherent.name, None);
    assert_eq!(inherent.target, "Buffer");
    assert_eq!(
        inherent.methods[0].parameters[0].param_type,
        WaveType::Pointer(Box::new(WaveType::Struct("Buffer".into())))
    );
    assert!(nodes[0].clone().span().is_some());
}

#[test]
fn public_named_contract_preserves_target_generic_arguments_and_visibility() {
    let nodes = parse(
        "pub proto Printable: Buffer<i32> {
        fun render(self) {}
        fun copy(self: ptr<Buffer<i32>>) {}
    }",
    );
    let ASTNode::ProtoImpl(contract) = nodes[0].unspanned() else { panic!("proto") };
    assert_eq!(contract.name.as_deref(), Some("Printable"));
    assert_eq!(contract.target, "Buffer<i32>");
    assert_eq!(contract.visibility, Visibility::Public);
    assert_eq!(contract.methods.len(), 2);
    assert_eq!(
        contract.methods[0].parameters[0].param_type,
        WaveType::Struct("Buffer<i32>".into())
    );
    let cloned = nodes[0].clone();
    let ASTNode::ProtoImpl(copy) = cloned.unspanned() else { panic!("clone") };
    assert_eq!(copy.name, contract.name);
    assert_eq!(copy.visibility, Visibility::Public);
}

#[test]
fn malformed_named_proto_syntax_has_located_diagnostics() {
    for (source, expected) in [
        ("proto : Buffer {}", "identifier"),
        ("proto Printable Buffer {}", "'{' or ':'"),
        ("proto Printable: {}", "type"),
        ("proto Printable: Buffer", "'{'"),
        ("proto Printable: Buffer {", "'}'"),
        ("proto Printable: Buffer { fun render(self) }", "function"),
        ("pub proto Buffer {}", "only named proto"),
    ] {
        failure(source, expected);
    }
}
