use std::iter::Peekable;
use std::slice::Iter;
use lexer::Token;
use lexer::token::TokenType;
use crate::ast::{ASTNode, StatementNode};

pub fn parse_asm_block(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    if tokens.peek()?.token_type != TokenType::Lbrace {
        println!("Expected '{{' after 'asm'");
        return None;
    }
    tokens.next();

    let mut instructions = vec![];
    let mut inputs = vec![];
    let mut outputs = vec![];

    while let Some(token) = tokens.next() {
        match &token.token_type {
            TokenType::Rbrace => break,

            TokenType::In | TokenType::Out => {
                let is_input = matches!(token.token_type, TokenType::In);

                if tokens.next().map(|t| t.token_type.clone()) != Some(TokenType::Lparen) {
                    println!("Expected '(' after in/out");
                    return None;
                }

                let reg_token = tokens.next();
                let reg = match reg_token {
                    Some(Token {
                             token_type: TokenType::String(s),
                             ..
                         }) => s.clone(),
                    Some(Token {
                             token_type: TokenType::Identifier(s),
                             ..
                         }) => s.clone(),
                    Some(other) => {
                        println!(
                            "Expected register string or identifier, got {:?}",
                            other.token_type
                        );
                        return None;
                    }
                    None => {
                        println!("Expected register in in/out(...)");
                        return None;
                    }
                };

                if tokens.next().map(|t| t.token_type.clone()) != Some(TokenType::Rparen) {
                    println!("Expected ')' after in/out");
                    return None;
                }

                let value_token = tokens.next();
                let value = match value_token {
                    Some(Token {
                             token_type: TokenType::Minus,
                             ..
                         }) => match tokens.next() {
                        Some(Token {
                                 token_type: TokenType::Number(n),
                                 ..
                             }) => format!("-{}", n),
                        Some(other) => {
                            println!("Expected number after '-', got {:?}", other.token_type);
                            return None;
                        }
                        None => {
                            println!("Expected number after '-'");
                            return None;
                        }
                    },
                    Some(Token {
                             token_type: TokenType::AddressOf,
                             ..
                         }) => match tokens.next() {
                        Some(Token {
                                 token_type: TokenType::Identifier(s),
                                 ..
                             }) => format!("&{}", s),
                        Some(other) => {
                            println!("Expected identifier after '&', got {:?}", other.token_type);
                            return None;
                        }
                        None => {
                            println!("Expected identifier after '&'");
                            return None;
                        }
                    },
                    Some(Token {
                             token_type: TokenType::Identifier(s),
                             ..
                         }) => s.clone(),
                    Some(Token {
                             token_type: TokenType::Number(n),
                             ..
                         }) => n.to_string(),
                    Some(Token {
                             token_type: TokenType::String(n),
                             ..
                         }) => n.to_string(),
                    Some(other) => {
                        println!(
                            "Expected identifier or number after in/out(...), got {:?}",
                            other.token_type
                        );
                        return None;
                    }
                    None => {
                        println!("Expected value after in/out(...)");
                        return None;
                    }
                };

                if is_input {
                    inputs.push((reg.clone(), value));
                } else {
                    outputs.push((reg.clone(), value));
                }
            }

            TokenType::String(s) => {
                instructions.push(s.clone());
            }

            other => {
                println!("Unexpected token in asm expression {:?}", other);
            }
        }
    }

    Some(ASTNode::Statement(StatementNode::AsmBlock {
        instructions,
        inputs,
        outputs,
    }))
}