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

//! Function declarations, generic parameters, parameters, and function bodies.
//!
//! Function parsing establishes syntax and declared types only. Duplicate names
//! within generic parameter lists are rejected here; program-wide symbol and
//! body type checks remain the semantic verifier's responsibility.

use crate::ast::{ASTNode, ExportAttribute, Expression, FunctionNode, ParameterNode, Visibility};
use crate::parser::decl::parse_ffi_header;
use crate::parser::types::parse_type_from_stream;
use lexer::token::TokenType;
use lexer::Token;
use std::collections::HashSet;
use std::iter::Peekable;
use std::slice::Iter;

fn skip_ws(tokens: &mut Peekable<Iter<Token>>) {
    while matches!(
        tokens.peek().map(|t| &t.token_type),
        Some(TokenType::Whitespace | TokenType::Newline)
    ) {
        tokens.next();
    }
}

pub fn parse_generic_param_names(tokens: &mut Peekable<Iter<Token>>) -> Option<Vec<String>> {
    skip_ws(tokens);
    if !matches!(
        tokens.peek().map(|t| &t.token_type),
        Some(TokenType::Lchevr)
    ) {
        return Some(Vec::new());
    }

    tokens.next(); // consume '<'
    let mut params: Vec<String> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();

    loop {
        skip_ws(tokens);

        if matches!(
            tokens.peek().map(|t| &t.token_type),
            Some(TokenType::Rchevr)
        ) {
            if params.is_empty() {
                return None;
            }
            tokens.next(); // consume '>'
            break;
        }

        let ident = match tokens.next() {
            Some(Token {
                token_type: TokenType::Identifier(name),
                ..
            }) => name.clone(),
            _ => {
                println!("Error: Expected generic parameter name inside '<...>'");
                return None;
            }
        };

        if !seen.insert(ident.clone()) {
            println!("Error: Duplicate generic parameter '{}'", ident);
            return None;
        }
        params.push(ident);

        skip_ws(tokens);
        match tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Comma) => {
                tokens.next();
            }
            Some(TokenType::Rchevr) => {
                tokens.next(); // consume '>'
                break;
            }
            _ => {
                println!("Error: Expected ',' or '>' in generic parameter list");
                return None;
            }
        }
    }

    Some(params)
}

pub fn parse_parameters(tokens: &mut Peekable<Iter<Token>>) -> Option<Vec<ParameterNode>> {
    let mut params = vec![];
    loop {
        skip_ws(tokens);

        if tokens
            .peek()
            .map_or(false, |t| t.token_type == TokenType::Rparen)
        {
            break;
        }

        let before = tokens.clone();
        let name = if let Some(Token {
            token_type: TokenType::Identifier(n),
            ..
        }) = tokens.next()
        {
            n.clone()
        } else {
            println!("Error: Expected parameter name");
            return None;
        };

        skip_ws(tokens);
        if tokens
            .peek()
            .map_or(true, |t| t.token_type != TokenType::Colon)
        {
            println!("Error: Expected ':' after parameter name '{}'", name);
            return None;
        }
        tokens.next();

        let param_type = match parse_type_from_stream(tokens) {
            Some(pt) => pt,
            None => {
                println!("Error: Failed to parse type for parameter '{}'", name);
                return None;
            }
        };

        let initial_value = if tokens
            .peek()
            .is_some_and(|t| t.token_type == TokenType::Equal)
        {
            tokens.next();
            let value = crate::expr::parse_expression(tokens)?;
            if !matches!(value.unspanned(), Expression::Literal(_) | Expression::Null) {
                return None;
            }
            Some(value)
        } else {
            None
        };

        params.push(ParameterNode {
            span: lexer::consumed_span(before, tokens),
            name,
            param_type,
            initial_value,
        });

        skip_ws(tokens);
        match tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Comma) => {
                tokens.next(); // consume ','
            }
            Some(TokenType::SemiColon) => {
                println!("Error: use `,` instead of `;` to separate parameters");
                return None;
            }
            Some(TokenType::Rparen) => {
                // loop end
            }
            _ => {
                println!("Error: Expected ',' or ')' after parameter");
                return None;
            }
        }
    }

    if tokens
        .peek()
        .map_or(true, |t| t.token_type != TokenType::Rparen)
    {
        println!("Error: Expected ')' or ',' in parameter list");
        return None;
    } else {
        tokens.next();
    }

    Some(params)
}

pub fn parse_function(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    parse_function_with_export(tokens, None)
}

pub fn parse_function_with_export(
    tokens: &mut Peekable<Iter<Token>>,
    export: Option<ExportAttribute>,
) -> Option<ASTNode> {
    let before = tokens.clone();
    tokens.next();

    skip_ws(tokens);

    let name = match tokens.next() {
        Some(Token {
            token_type: TokenType::Identifier(name),
            ..
        }) => name.clone(),
        _ => return None,
    };

    let generic_params = parse_generic_param_names(tokens)?;

    skip_ws(tokens);
    if tokens.peek()?.token_type != TokenType::Lparen {
        return None;
    }

    tokens.next(); // consume '('
    let parameters = parse_parameters(tokens)?;

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

    skip_ws(tokens);
    let mut return_type_span = None;
    let return_type = if let Some(Token {
        token_type: TokenType::Arrow,
        ..
    }) = tokens.peek()
    {
        tokens.next(); // consume '->'
        let before_type = tokens.clone();
        let ty = parse_type_from_stream(tokens)?;
        return_type_span = lexer::consumed_span(before_type, tokens);
        Some(ty)
    } else {
        None
    };

    skip_ws(tokens);
    let body = extract_body(tokens)?;
    Some(ASTNode::Function(FunctionNode {
        span: lexer::consumed_span(before, tokens),
        name,
        generic_params,
        parameters,
        body,
        return_type,
        return_type_span,
        export,
        visibility: Visibility::Private,
    }))
}

pub fn parse_export(tokens: &mut Peekable<Iter<Token>>) -> Option<Vec<ASTNode>> {
    let (abi, global_symbol) = parse_ffi_header(tokens, "export")?;
    let export = ExportAttribute {
        abi,
        symbol: global_symbol,
    };

    skip_ws(tokens);

    if tokens.peek().map(|t| t.token_type.clone()) == Some(TokenType::Lbrace) {
        if export.symbol.is_some() {
            println!("Error: export block cannot use a single symbol alias");
            return None;
        }

        tokens.next();

        let mut nodes = Vec::new();
        loop {
            skip_ws(tokens);

            match tokens.peek().map(|t| t.token_type.clone()) {
                Some(TokenType::Rbrace) => {
                    tokens.next();
                    break;
                }
                Some(TokenType::Fun) => {
                    let node = parse_function_with_export(tokens, Some(export.clone()))?;
                    if let ASTNode::Function(func) = &node {
                        if !func.generic_params.is_empty() {
                            println!("Error: exported functions cannot be generic");
                            return None;
                        }
                    }
                    nodes.push(node);
                }
                Some(TokenType::Whitespace) | Some(TokenType::Newline) => {
                    tokens.next();
                }
                other => {
                    println!("Error: Unexpected token in export block: {:?}", other);
                    return None;
                }
            }
        }

        skip_ws(tokens);
        if tokens.peek().map(|t| t.token_type.clone()) == Some(TokenType::SemiColon) {
            tokens.next();
        }

        Some(nodes)
    } else if tokens.peek().map(|t| t.token_type.clone()) == Some(TokenType::Fun) {
        let node = parse_function_with_export(tokens, Some(export))?;
        if let ASTNode::Function(func) = &node {
            if !func.generic_params.is_empty() {
                println!("Error: exported functions cannot be generic");
                return None;
            }
        }
        Some(vec![node])
    } else {
        println!("Error: Expected 'fun' or '{{' after export(...)");
        None
    }
}

pub fn extract_body(tokens: &mut Peekable<Iter<Token>>) -> Option<Vec<ASTNode>> {
    if tokens.peek()?.token_type != TokenType::Lbrace {
        return None;
    }
    tokens.next();
    crate::parser::stmt::parse_block(tokens)
}
