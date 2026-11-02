//! Long acyclic aliases must not exhaust the Rust stack during frontend lowering.
use hir::TypedProgram;
use lexer::Lexer;
use parser::{ast::{ASTNode, WaveType}, parse_syntax_with_spans};

fn lower_chain(count: usize, reverse: bool) -> TypedProgram {
    let mut source = String::new();
    for index in 0..count {
        let declaration = if index == count - 1 {
            format!("type A{index} = i32;\n")
        } else {
            format!("type A{index} = A{};\n", index + 1)
        };
        if reverse {
            source.insert_str(0, &declaration);
        } else {
            source.push_str(&declaration);
        }
    }
    source.push_str("fun main() { var value: A0 = 0; }\n");
    let tokens = Lexer::new(&source).tokenize().unwrap();
    TypedProgram::lower(parse_syntax_with_spans(&tokens).unwrap())
        .unwrap_or_else(|e| panic!("{} aliases, reverse={reverse}: {e:?}", count))
}

#[test]
fn forward_and_backward_alias_chains_lower_to_concrete_types() {
    for reverse in [false, true] {
        let program = lower_chain(2048, reverse);
        let ASTNode::Function(function) = &program.syntax()[2048] else {
            panic!("missing function");
        };
        let ASTNode::Variable(variable) = &function.body[0] else {
            panic!("missing variable");
        };
        assert_eq!(variable.type_name, WaveType::Int(32));
    }
}

#[test]
fn recursive_aliases_keep_a_normal_semantic_error() {
    for source in [
        "type A = A; fun main() {}",
        "type A = B; type B = A; fun main() {}",
        "type A = ptr<B>; type B = A; fun main() {}",
    ] {
        let tokens = Lexer::new(source).tokenize().unwrap();
        let error =
            TypedProgram::lower(parse_syntax_with_spans(&tokens).unwrap()).unwrap_err();
        assert!(error.diagnostic().message.contains("cyclic type alias"), "{error:?}");
    }
}
