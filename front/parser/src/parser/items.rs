use std::iter::Peekable;
use std::slice::Iter;
use lexer::Token;
use lexer::token::TokenType;
use crate::ast::{ASTNode, ProtoImplNode, StatementNode, StructNode, WaveType};
use crate::parser::functions::parse_function;
use crate::types::parse_type_from_stream;

fn skip_ws(tokens: &mut Peekable<Iter<Token>>) {
    while let Some(t) = tokens.peek() {
        match t.token_type {
            TokenType::Whitespace | TokenType::Newline => {
                tokens.next();
            }
            _ => break,
        }
    }
}

pub fn parse_import(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    if tokens.peek()?.token_type != TokenType::Lparen {
        println!("Error: Expected '(' after 'import'");
        return None;
    }
    tokens.next();

    let import_path = match tokens.next() {
        Some(Token {
                 token_type: TokenType::String(s),
                 ..
             }) => s.clone(),
        other => {
            println!(
                "Error: Expected string literal in import, found {:?}",
                other
            );
            return None;
        }
    };

    if tokens.peek()?.token_type != TokenType::Rparen {
        println!("Error: Expected ')' after 'import' condition");
        return None;
    }
    tokens.next();

    if tokens.peek()?.token_type != TokenType::SemiColon {
        println!("Error: Expected ';' after 'import' condition");
        return None;
    }
    tokens.next();

    Some(ASTNode::Statement(StatementNode::Import(import_path)))
}

pub fn parse_proto(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    let target_struct = match tokens.next() {
        Some(Token {
                 token_type: TokenType::Identifier(name),
                 ..
             }) => name.clone(),
        other => {
            println!(
                "Error: Expected struct name after 'proto', found {:?}",
                other
            );
            return None;
        }
    };

    if tokens.peek()?.token_type != TokenType::Lbrace {
        println!(
            "Error: Expected '{{' after proto target '{}'",
            target_struct
        );
        return None;
    }
    tokens.next(); // consume '{'

    let mut methods = Vec::new();

    loop {
        let token_type = if let Some(t) = tokens.peek() {
            t.token_type.clone()
        } else {
            println!(
                "Error: Unexpected end of file inside proto '{}' definition.",
                target_struct
            );
            return None;
        };

        match token_type {
            TokenType::Rbrace => {
                tokens.next();
                break;
            }

            TokenType::Fun => {
                if let Some(ASTNode::Function(mut func_node)) = parse_function(tokens) {
                    if func_node.return_type.is_none() {
                        func_node.return_type = Some(WaveType::Void);
                    }
                    methods.push(func_node);
                } else {
                    println!(
                        "Error: Failed to parse method inside proto '{}'.",
                        target_struct
                    );
                    return None;
                }
            }

            TokenType::Whitespace | TokenType::Newline => {
                tokens.next();
            }

            other => {
                println!("Error: Unexpected token inside proto body: {:?}", other);
                return None;
            }
        }
    }

    Some(ASTNode::ProtoImpl(ProtoImplNode {
        target: target_struct,
        methods,
    }))
}

pub fn parse_struct(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    let name = match tokens.next() {
        Some(Token {
                 token_type: TokenType::Identifier(name),
                 ..
             }) => name.clone(),
        _ => {
            println!("Error: Expected struct name after 'struct' keyword.");
            return None;
        }
    };

    if tokens
        .peek()
        .map_or(true, |t| t.token_type != TokenType::Lbrace)
    {
        println!("Error: Expected '{{' after struct name '{}'.", name);
        return None;
    }
    tokens.next();

    let mut fields = Vec::new();
    let mut methods = Vec::new();

    loop {
        skip_ws(tokens);

        let token_type = if let Some(t) = tokens.peek() {
            t.token_type.clone()
        } else {
            println!(
                "Error: Unexpected end of file inside struct '{}' definition.",
                name
            );
            return None;
        };

        match token_type {
            TokenType::Rbrace => {
                tokens.next();
                break;
            }

            TokenType::Whitespace | TokenType::Newline => {
                tokens.next();
            }

            TokenType::Fun => {
                if let Some(ASTNode::Function(func_node)) = parse_function(tokens) {
                    if func_node.return_type.is_none() {
                        let mut func_node_with_return = func_node.clone();
                        func_node_with_return.return_type = Some(WaveType::Void);
                        methods.push(func_node_with_return);
                    } else {
                        methods.push(func_node);
                    }
                } else {
                    println!("Error: Failed to parse method inside struct '{}'.", name);
                    return None;
                }
            }
            TokenType::Identifier(_) => {
                let mut lookahead = tokens.clone();
                lookahead.next();
                while let Some(t) = lookahead.peek() {
                    match t.token_type {
                        TokenType::Whitespace | TokenType::Newline => { lookahead.next(); }
                        _ => break,
                    }
                }

                if matches!(lookahead.peek().map(|t| &t.token_type), Some(TokenType::Colon)) {
                    let field_name = if let Some(Token { token_type: TokenType::Identifier(n), .. }) = tokens.next() {
                        n.clone()
                    } else {
                        unreachable!()
                    };

                    skip_ws(tokens);

                    // ':'
                    if tokens.peek().map_or(true, |t| t.token_type != TokenType::Colon) {
                        println!("Error: Expected ':' after field '{}' in struct '{}'.", field_name, name);
                        return None;
                    }
                    tokens.next(); // consume ':'

                    skip_ws(tokens);

                    let wave_type = match parse_type_from_stream(tokens) {
                        Some(t) => t,
                        None => {
                            println!(
                                "Error: Invalid type for field '{}' in struct '{}'.",
                                field_name, name
                            );
                            return None;
                        }
                    };

                    skip_ws(tokens);

                    if tokens.peek().map_or(true, |t| t.token_type != TokenType::SemiColon) {
                        println!(
                            "Error: Expected ';' after field declaration in struct '{}'.",
                            name
                        );
                        return None;
                    }
                    tokens.next(); // consume ';'

                    fields.push((field_name, wave_type));
                } else {
                    let id_str = if let TokenType::Identifier(id) = &tokens.peek().unwrap().token_type {
                        id.clone()
                    } else {
                        "".to_string()
                    };
                    println!(
                        "Error: Unexpected identifier '{}' in struct '{}' body. Expected field or method.",
                        id_str, name
                    );
                    return None;
                }
            }

            other_token => {
                println!(
                    "Error: Unexpected token inside struct body: {:?}",
                    other_token
                );
                return None;
            }
        }
    }

    Some(ASTNode::Struct(StructNode {
        name,
        fields,
        methods,
    }))
}