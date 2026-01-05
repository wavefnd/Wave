use std::collections::HashSet;
use std::iter::Peekable;
use std::slice::Iter;
use lexer::Token;
use lexer::token::TokenType;
use crate::ast::{ASTNode, FunctionNode, ParameterNode, StatementNode, Value};
use crate::format::parse_expression;
use crate::parser::asm::*;
use crate::parser::control::*;
use crate::parser::decl::*;
use crate::parser::io::*;
use crate::parser::stmt::parse_assignment;
use crate::parser::types::parse_type_from_stream;

pub fn parse_parameters(tokens: &mut Peekable<Iter<Token>>) -> Vec<ParameterNode> {
    let mut params = vec![];
    while tokens
        .peek()
        .map_or(false, |t| t.token_type != TokenType::Rparen)
    {
        let name = if let Some(Token {
                                   token_type: TokenType::Identifier(n),
                                   ..
                               }) = tokens.next()
        {
            n.clone()
        } else {
            println!("Error: Expected parameter name");
            break;
        };

        if tokens
            .peek()
            .map_or(true, |t| t.token_type != TokenType::Colon)
        {
            println!("Error: Expected ':' after parameter name '{}'", name);
            break;
        }
        tokens.next();

        let param_type = match parse_type_from_stream(tokens) {
            Some(pt) => pt,
            None => {
                println!("Error: Failed to parse type for parameter '{}'", name);
                break;
            }
        };

        let initial_value = if tokens
            .peek()
            .map_or(false, |t| t.token_type == TokenType::Equal)
        {
            tokens.next(); // consume '='
            match tokens.next() {
                Some(Token {
                         token_type: TokenType::Number(n),
                         ..
                     }) => Some(Value::Int(*n)),
                Some(Token {
                         token_type: TokenType::Float(f),
                         ..
                     }) => Some(Value::Float(*f)),
                Some(Token {
                         token_type: TokenType::String(s),
                         ..
                     }) => Some(Value::Text(s.clone())),
                _ => {
                    println!("Error: Unsupported initializer for parameter '{}'", name);
                    None
                }
            }
        } else {
            None
        };

        params.push(ParameterNode {
            name,
            param_type,
            initial_value,
        });

        match tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Comma) => {
                tokens.next(); // consume ','
            }
            Some(TokenType::SemiColon) => {
                println!("Error: use `,` instead of `;` to separate parameters");
                break;
            }
            Some(TokenType::Rparen) => {
                // loop end
            }
            _ => {
                println!("Error: Expected ',' or ')' after parameter");
                break;
            }
        }
    }

    if tokens
        .peek()
        .map_or(true, |t| t.token_type != TokenType::Rparen)
    {
        println!("Error: Expected ')' or ',' in parameter list");
    } else {
        tokens.next();
    }

    params
}

pub fn parse_function(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    tokens.next();

    let name = match tokens.next() {
        Some(Token {
                 token_type: TokenType::Identifier(name),
                 ..
             }) => name.clone(),
        _ => return None,
    };

    if tokens.peek()?.token_type != TokenType::Lparen {
        return None;
    }

    tokens.next(); // consume '('
    let parameters = parse_parameters(tokens);

    let mut param_names = HashSet::new();
    for param in &parameters {
        if !param_names.insert(param.name.clone()) {
            println!(
                "Error: Parameter '{}' is declared multiple times",
                param.name
            );
            return None;
        }
    }

    let return_type = if let Some(Token {
                                      token_type: TokenType::Arrow,
                                      ..
                                  }) = tokens.peek()
    {
        tokens.next(); // consume '->'
        parse_type_from_stream(tokens)
    } else {
        None
    };

    let body = extract_body(tokens)?;
    Some(ASTNode::Function(FunctionNode {
        name,
        parameters,
        body,
        return_type,
    }))
}

pub fn extract_body(tokens: &mut Peekable<Iter<Token>>) -> Option<Vec<ASTNode>> {
    let mut body = vec![];

    if tokens.peek()?.token_type != TokenType::Lbrace {
        println!("❌ Expected '{{' at the beginning of function body");
        return None;
    }
    tokens.next(); // consume '{'

    while let Some(token) = tokens.peek() {
        match &token.token_type {
            TokenType::Whitespace => {
                tokens.next(); // ignore
            }
            TokenType::Rbrace => {
                tokens.next();
                break;
            }
            TokenType::Eof => {
                println!("❌ Unexpected EOF inside function body");
                return None;
            }
            TokenType::Asm => {
                tokens.next();
                body.push(parse_asm_block(tokens)?);
            }
            TokenType::Var => {
                tokens.next(); // consume 'var'
                body.push(parse_var(tokens)?);
            }
            TokenType::Let => {
                tokens.next(); // consume 'let'
                body.push(parse_let(tokens)?);
            }
            TokenType::Println => {
                tokens.next(); // consume 'println'
                let node = parse_println(tokens)?;
                // Added semicolon handling
                if let Some(Token {
                                token_type: TokenType::SemiColon,
                                ..
                            }) = tokens.peek()
                {
                    tokens.next();
                }
                body.push(node);
            }
            TokenType::Print => {
                tokens.next(); // consume 'print'
                let node = parse_print(tokens)?;
                // Added semicolon handling
                if let Some(Token {
                                token_type: TokenType::SemiColon,
                                ..
                            }) = tokens.peek()
                {
                    tokens.next();
                }
                body.push(node);
            }
            TokenType::Input => {
                tokens.next(); // consume 'input'
                let node = parse_input(tokens)?;
                // Added semicolon handling
                if let Some(Token {
                                token_type: TokenType::SemiColon,
                                ..
                            }) = tokens.peek()
                {
                    tokens.next();
                }
                body.push(node);
            }
            TokenType::If => {
                tokens.next();
                body.push(parse_if(tokens)?);
            }
            TokenType::For => {
                tokens.next();
                body.push(parse_for(tokens)?);
            }
            TokenType::While => {
                tokens.next();
                body.push(parse_while(tokens)?);
            }
            TokenType::Identifier(_) => {
                if let Some(expr) = parse_expression(tokens) {
                    if let Some(Token {
                                    token_type: TokenType::SemiColon,
                                    ..
                                }) = tokens.peek()
                    {
                        tokens.next(); // consume ';'
                    }
                    body.push(ASTNode::Statement(StatementNode::Expression(expr)));
                } else {
                    println!("❌ Failed to parse expression starting with identifier");
                    return None;
                }
            }
            TokenType::Break => {
                tokens.next(); // consume 'break'
                if let Some(Token {
                                token_type: TokenType::SemiColon,
                                ..
                            }) = tokens.peek()
                {
                    tokens.next(); // consume ;
                }
                body.push(ASTNode::Statement(StatementNode::Break));
            }
            TokenType::Continue => {
                tokens.next(); // consume 'continue'
                if let Some(Token {
                                token_type: TokenType::SemiColon,
                                ..
                            }) = tokens.peek()
                {
                    tokens.next(); // consume ;
                }
                body.push(ASTNode::Statement(StatementNode::Continue));
            }
            TokenType::Return => {
                tokens.next(); // consume 'return'

                let expr = if let Some(Token {
                                           token_type: TokenType::SemiColon,
                                           ..
                                       }) = tokens.peek()
                {
                    tokens.next(); // return;
                    None
                } else {
                    let value = match parse_expression(tokens) {
                        Some(v) => v,
                        None => {
                            println!("Error: Expected valid expression after 'return'");
                            return None;
                        }
                    };

                    if let Some(Token {
                                    token_type: TokenType::SemiColon,
                                    ..
                                }) = tokens.peek()
                    {
                        tokens.next();
                    } else {
                        println!("Error: Missing semicolon after return expression");
                        return None;
                    }
                    Some(value)
                };

                body.push(ASTNode::Statement(StatementNode::Return(expr)));
            }
            TokenType::Deref => {
                let token = (*token).clone();
                tokens.next();
                body.push(parse_assignment(tokens, &token)?);
            }
            _ => {
                if let Some(expr) = parse_expression(tokens) {
                    if let Some(Token {
                                    token_type: TokenType::SemiColon,
                                    ..
                                }) = tokens.peek()
                    {
                        tokens.next(); // consume ;
                    }
                    body.push(ASTNode::Statement(StatementNode::Expression(expr)));
                } else {
                    tokens.next(); // fallback skip
                }
            }
        }
    }

    Some(body)
}