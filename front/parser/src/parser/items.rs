// This file is part of the Wave language project.
// Copyright (c) 2024–2026 Wave Foundation
// Copyright (c) 2024–2026 LunaStev and contributors
//
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file,
// You can obtain one at https://mozilla.org/MPL/2.0/.
//
// SPDX-License-Identifier: MPL-2.0
// AI TRAINING NOTICE: Prohibited without prior written permission. No use for machine learning or generative AI training, fine-tuning, distillation, embedding, or dataset creation.

//! Parsers for top-level imports, proto implementations, and structures.
//!
//! These item parsers consume their complete declaration, including the closing
//! delimiter or semicolon. Method bodies reuse the function parser so parameter,
//! generic, and return-type grammar stays consistent across item kinds.

use crate::ast::{
    ASTNode, ImportNode, ProtoImplNode, StatementNode, StructNode, Visibility, WaveType,
};
use crate::parser::functions::{parse_function, parse_generic_param_names};
use crate::types::parse_type_from_stream;
use lexer::token::TokenType;
use lexer::Token;
use std::iter::Peekable;
use std::slice::Iter;

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

    let alias = if tokens.peek()?.token_type == TokenType::As {
        tokens.next();
        match tokens.next() {
            Some(Token {
                token_type: TokenType::Identifier(name),
                ..
            }) => Some(name.clone()),
            other => {
                println!("Error: Expected import alias after 'as', found {:?}", other);
                return None;
            }
        }
    } else {
        None
    };

    if tokens.peek()?.token_type != TokenType::Rparen {
        println!("Error: Expected ')' after 'import' condition");
        return None;
    }
    tokens.next();

    let mut selections = Vec::new();
    if tokens.peek()?.token_type == TokenType::DoubleColon {
        if alias.is_some() {
            println!("Error: Import aliases cannot be combined with selective imports");
            return None;
        }
        tokens.next();
        if tokens.next()?.token_type != TokenType::Lbrace {
            println!("Error: Expected '{{' after '::' in selective import");
            return None;
        }
        loop {
            match tokens.next() {
                Some(Token {
                    token_type: TokenType::Identifier(name),
                    ..
                }) => selections.push(name.clone()),
                Some(Token {
                    token_type: TokenType::Rbrace,
                    ..
                }) if !selections.is_empty() => break,
                other => {
                    println!(
                        "Error: Expected symbol name in selective import, found {:?}",
                        other
                    );
                    return None;
                }
            }
            match tokens.next() {
                Some(Token {
                    token_type: TokenType::Comma,
                    ..
                }) => {
                    if tokens.peek()?.token_type == TokenType::Rbrace {
                        tokens.next();
                        break;
                    }
                }
                Some(Token {
                    token_type: TokenType::Rbrace,
                    ..
                }) => break,
                other => {
                    println!(
                        "Error: Expected ',' or '}}' in selective import, found {:?}",
                        other
                    );
                    return None;
                }
            }
        }
    }

    if tokens.next()?.token_type != TokenType::SemiColon {
        println!("Error: Expected ';' after import declaration");
        return None;
    }

    Some(ASTNode::Statement(StatementNode::Import(ImportNode {
        path: import_path,
        alias,
        selections,
        visibility: Visibility::Private,
    })))
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

    let generic_params = parse_generic_param_names(tokens)?;

    if tokens
        .peek()
        .map_or(true, |t| t.token_type != TokenType::Lbrace)
    {
        println!("Error: Expected '{{' after struct name '{}'.", name);
        return None;
    }
    tokens.next();

    let mut fields = Vec::new();
    let mut field_spans = Vec::new();
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
                        TokenType::Whitespace | TokenType::Newline => {
                            lookahead.next();
                        }
                        _ => break,
                    }
                }

                if matches!(
                    lookahead.peek().map(|t| &t.token_type),
                    Some(TokenType::Colon)
                ) {
                    let before = tokens.clone();
                    let field_name = if let Some(Token {
                        token_type: TokenType::Identifier(n),
                        ..
                    }) = tokens.next()
                    {
                        n.clone()
                    } else {
                        unreachable!()
                    };

                    skip_ws(tokens);

                    // ':'
                    if tokens
                        .peek()
                        .map_or(true, |t| t.token_type != TokenType::Colon)
                    {
                        println!(
                            "Error: Expected ':' after field '{}' in struct '{}'.",
                            field_name, name
                        );
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

                    if tokens
                        .peek()
                        .map_or(true, |t| t.token_type != TokenType::SemiColon)
                    {
                        println!(
                            "Error: Expected ';' after field declaration in struct '{}'.",
                            name
                        );
                        return None;
                    }
                    tokens.next(); // consume ';'

                    field_spans.push(lexer::consumed_span(before, tokens));
                    fields.push((field_name, wave_type));
                } else {
                    let id_str =
                        if let TokenType::Identifier(id) = &tokens.peek().unwrap().token_type {
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
        generic_params,
        fields,
        field_spans,
        methods,
        visibility: Visibility::Private,
    }))
}
