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
use crate::parser::ParseError;
use lexer::token::TokenType;
use lexer::Token;
use std::iter::Peekable;
use std::slice::Iter;

fn skip_ws(tokens: &mut Peekable<Iter<Token>>) {
    while let Some(t) = tokens.peek() {
        match t.token_type {
            TokenType::Whitespace | TokenType::Newline => {
                tokens.next();
            },
            _ => break,
        }
    }
}

pub fn parse_import(tokens: &mut Peekable<Iter<Token>>) -> Result<ASTNode, ParseError> {
    let anchor = tokens.peek().copied();
    let context = "import declaration";
    crate::expr::expect_token(tokens, anchor, TokenType::Lparen, "'('", context)?;
    skip_ws(tokens);
    let path_token = tokens.peek().copied();
    let Some(Token { token_type: TokenType::String(bytes), .. }) = path_token else {
        return Err(ParseError::expected_at(path_token, anchor, "string literal", context));
    };
    let import_path = String::from_utf8(bytes.clone()).map_err(|_| {
        ParseError::syntax_at(path_token, "import path must be valid UTF-8").with_context(context)
    })?;
    tokens.next();
    skip_ws(tokens);
    let alias = if tokens.peek().is_some_and(|t| t.token_type == TokenType::As) {
        tokens.next();
        Some(crate::expr::identifier(tokens, anchor, "import alias")?)
    } else {
        None
    };
    crate::expr::expect_token(tokens, anchor, TokenType::Rparen, "')'", context)?;
    skip_ws(tokens);
    let mut selections = Vec::new();
    if tokens.peek().is_some_and(|t| t.token_type == TokenType::DoubleColon) {
        if alias.is_some() {
            return Err(ParseError::syntax_at(
                tokens.peek().copied(),
                "import aliases cannot be combined with selective imports",
            )
            .with_context(context));
        }
        tokens.next();
        crate::expr::expect_token(tokens, anchor, TokenType::Lbrace, "'{'", "selective import")?;
        loop {
            selections.push(crate::expr::identifier(tokens, anchor, "selective import")?);
            skip_ws(tokens);
            if tokens.peek().is_some_and(|t| t.token_type == TokenType::Comma) {
                tokens.next();
                skip_ws(tokens);
                if !tokens.peek().is_some_and(|t| t.token_type == TokenType::Rbrace) {
                    continue;
                }
            }
            crate::expr::expect_token(
                tokens,
                anchor,
                TokenType::Rbrace,
                "',' or '}'",
                "selective import",
            )?;
            break;
        }
    }
    crate::expr::expect_token(tokens, anchor, TokenType::SemiColon, "';'", context)?;
    Ok(ASTNode::Statement(StatementNode::Import(ImportNode {
        path: import_path,
        alias,
        selections,
        visibility: Visibility::Private,
    })))
}

pub fn parse_proto(tokens: &mut Peekable<Iter<Token>>) -> Result<ASTNode, ParseError> {
    let anchor = tokens.peek().copied();
    let invalid = |token| {
        ParseError::syntax_at(anchor, "failed to parse proto implementation")
            .with_context("top-level proto block")
            .with_expected("proto Type { fun method(...); }")
            .with_found_token(token)
            .with_help("check braces and method declarations inside proto")
    };
    let target_struct = match tokens.next() {
        Some(Token { token_type: TokenType::Identifier(name), .. }) => name.clone(),
        other => {
            println!("Error: Expected struct name after 'proto', found {:?}", other);
            return Err(invalid(tokens.peek().copied()));
        },
    };

    if tokens.peek().ok_or_else(|| invalid(None))?.token_type != TokenType::Lbrace {
        println!("Error: Expected '{{' after proto target '{}'", target_struct);
        return Err(invalid(tokens.peek().copied()));
    }
    tokens.next(); // consume '{'

    let mut methods = Vec::new();

    loop {
        let token_type = if let Some(t) = tokens.peek() {
            t.token_type.clone()
        } else {
            println!("Error: Unexpected end of file inside proto '{}' definition.", target_struct);
            return Err(invalid(tokens.peek().copied()));
        };

        match token_type {
            TokenType::Rbrace => {
                tokens.next();
                break;
            },

            TokenType::Fun | TokenType::Async => {
                if let ASTNode::Function(mut func_node) = parse_function(tokens)? {
                    if func_node.return_type.is_none() {
                        func_node.return_type = Some(WaveType::Void);
                    }
                    methods.push(func_node);
                } else {
                    println!("Error: Failed to parse method inside proto '{}'.", target_struct);
                    return Err(invalid(tokens.peek().copied()));
                }
            },

            TokenType::Whitespace | TokenType::Newline => {
                tokens.next();
            },

            other => {
                println!("Error: Unexpected token inside proto body: {:?}", other);
                return Err(invalid(tokens.peek().copied()));
            },
        }
    }

    Ok(ASTNode::ProtoImpl(ProtoImplNode { target: target_struct, methods }))
}

pub fn parse_struct(tokens: &mut Peekable<Iter<Token>>) -> Result<ASTNode, ParseError> {
    let anchor = tokens.peek().copied();
    let name = crate::expr::identifier(tokens, anchor, "struct name")?;
    let generic_params = parse_generic_param_names(tokens)?;
    crate::expr::expect_token(tokens, anchor, TokenType::Lbrace, "'{'", "struct declaration")?;
    let mut fields = Vec::new();
    let mut field_spans = Vec::new();
    let mut methods = Vec::new();
    loop {
        skip_ws(tokens);
        match tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Rbrace) => {
                tokens.next();
                break;
            },
            Some(TokenType::Fun | TokenType::Async) => {
                let ASTNode::Function(mut method) = parse_function(tokens)? else { unreachable!() };
                if method.return_type.is_none() {
                    method.return_type = Some(WaveType::Void);
                }
                methods.push(method);
            },
            Some(_) => {
                let before = tokens.clone();
                let field_name = crate::expr::identifier(tokens, anchor, "struct field name")?;
                crate::expr::expect_token(tokens, anchor, TokenType::Colon, "':'", "struct field")?;
                skip_ws(tokens);
                let ty = crate::types::parse_type_checked(tokens, "struct field type")?;
                crate::expr::expect_token(
                    tokens,
                    anchor,
                    TokenType::SemiColon,
                    "';'",
                    "struct field",
                )?;
                field_spans.push(lexer::consumed_span(before, tokens));
                fields.push((field_name, ty));
            },
            None => return Err(ParseError::expected_at(None, anchor, "'}'", "struct declaration")),
        }
    }
    Ok(ASTNode::Struct(StructNode {
        name,
        generic_params,
        fields,
        field_spans,
        methods,
        visibility: Visibility::Private,
    }))
}
