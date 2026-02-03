use lexer::Token;
use lexer::token::TokenType;
use crate::ast::ASTNode;
use crate::parser::decl::*;
use crate::parser::functions::parse_function;
use crate::parser::items::*;
use crate::verification::*;

pub fn parse(tokens: &Vec<Token>) -> Option<Vec<ASTNode>> {
    let mut iter = tokens.iter().peekable();
    let mut nodes = vec![];

    while let Some(token) = iter.peek() {
        match token.token_type {
            TokenType::Whitespace | TokenType::Newline => {
                iter.next();
                continue;
            }
            TokenType::Import => {
                iter.next();
                if let Some(path) = parse_import(&mut iter) {
                    nodes.push(path);
                } else {
                    return None;
                }
            }
            TokenType::Extern => {
                iter.next();
                if let Some(extern_nodes) = parse_extern(&mut iter) {
                    nodes.extend(extern_nodes);
                } else {
                    return None;
                }
            }
            TokenType::Const => {
                iter.next();
                if let Some(var) = parse_const(&mut iter) {
                    nodes.push(var);
                } else {
                    return None;
                }
            }
            TokenType::Proto => {
                iter.next();
                if let Some(proto_impl) = parse_proto(&mut iter) {
                    nodes.push(proto_impl);
                } else {
                    println!("Failed to parse proto impl");
                    return None;
                }
            }
            TokenType::Struct => {
                iter.next();
                if let Some(struct_node) = parse_struct(&mut iter) {
                    nodes.push(struct_node);
                } else {
                    println!("Failed to parse struct");
                    return None;
                }
            }
            TokenType::Fun => {
                if let Some(func) = parse_function(&mut iter) {
                    nodes.push(func);
                } else {
                    println!("❌ Failed to parse function");
                    return None;
                }
            }
            TokenType::Eof => break,
            _ => {
                println!("❌ Unexpected token at top level: {:?}", token);
                return None;
            }
        }
    }

    if let Err(e) = validate_program(&nodes) {
        println!("❌ {}", e);
        return None;
    }

    Some(nodes)
}
