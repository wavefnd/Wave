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
use crate::parser::ParseError;
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

pub fn parse_generic_param_names(
    tokens: &mut Peekable<Iter<Token>>,
) -> Result<Vec<String>, ParseError> {
    skip_ws(tokens);
    if !tokens.peek().is_some_and(|t| t.token_type == TokenType::Lchevr) {
        return Ok(vec![]);
    }
    let anchor = tokens.next();
    let context = "generic parameters";
    let mut params = Vec::new();
    let mut seen = HashSet::new();
    loop {
        skip_ws(tokens);
        if !params.is_empty() && tokens.peek().is_some_and(|t| t.token_type == TokenType::Rchevr) {
            tokens.next();
            break;
        }
        let at = tokens.peek().copied();
        let name = crate::expr::identifier(tokens, anchor, context)?;
        if !seen.insert(name.clone()) {
            return Err(ParseError::syntax_at(at, format!("duplicate generic parameter '{name}'"))
                .with_context(context)
                .with_found_token(at));
        }
        params.push(name);
        skip_ws(tokens);
        if tokens.peek().is_some_and(|t| t.token_type == TokenType::Comma) {
            tokens.next();
        } else {
            crate::expr::expect_token(tokens, anchor, TokenType::Rchevr, "',' or '>'", context)?;
            break;
        }
    }
    Ok(params)
}

pub fn parse_parameters(
    tokens: &mut Peekable<Iter<Token>>,
) -> Result<Vec<ParameterNode>, ParseError> {
    let anchor = tokens.peek().copied();
    let context = "function parameters";
    let mut params = Vec::new();
    let mut names = HashSet::new();
    loop {
        skip_ws(tokens);
        if tokens.peek().is_some_and(|t| t.token_type == TokenType::Rparen) {
            tokens.next();
            break;
        }
        let before = tokens.clone();
        let at = tokens.peek().copied();
        let name = crate::expr::identifier(tokens, anchor, context)?;
        if !names.insert(name.clone()) {
            return Err(ParseError::syntax_at(at, format!("duplicate parameter '{name}'"))
                .with_context(context));
        }
        crate::expr::expect_token(tokens, anchor, TokenType::Colon, "':'", context)?;
        skip_ws(tokens);
        let param_type = crate::types::parse_type_checked(tokens, "parameter type")?;
        skip_ws(tokens);
        let initial_value = if tokens.peek().is_some_and(|t| t.token_type == TokenType::Equal) {
            tokens.next();
            skip_ws(tokens);
            let at = tokens.peek().copied();
            let value = crate::expr::parse_expression(tokens)?;
            if !matches!(value.unspanned(), Expression::Literal(_) | Expression::Null) {
                return Err(ParseError::expected_at(at, anchor, "literal default value", context));
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
        if tokens.peek().is_some_and(|t| t.token_type == TokenType::Comma) {
            tokens.next();
        } else {
            crate::expr::expect_token(tokens, anchor, TokenType::Rparen, "',' or ')'", context)?;
            break;
        }
    }
    Ok(params)
}

pub fn parse_function(tokens: &mut Peekable<Iter<Token>>) -> Result<ASTNode, ParseError> {
    parse_function_with_export(tokens, None)
}

pub fn parse_function_with_export(
    tokens: &mut Peekable<Iter<Token>>,
    export: Option<ExportAttribute>,
) -> Result<ASTNode, ParseError> {
    let anchor = tokens.peek().copied();
    let invalid = |token: Option<&Token>| {
        ParseError::syntax_at(token.or(anchor), "failed to parse function declaration")
            .with_context("top-level function")
            .with_expected_many([
                "fun name(params) { ... }",
                "fun name(params) -> return_type { ... }",
            ])
            .with_found_token(token)
            .with_help("check parameter syntax, return type arrow, and function body braces")
    };
    let before = tokens.clone();
    let is_async = tokens.peek().is_some_and(|t| t.token_type == TokenType::Async);
    if is_async {
        tokens.next();
        skip_ws(tokens);
        crate::expr::expect_token(tokens, anchor, TokenType::Fun, "'fun'", "async function")?;
    } else {
        crate::expr::expect_token(tokens, anchor, TokenType::Fun, "'fun'", "function declaration")?;
    }

    skip_ws(tokens);

    let name = crate::expr::identifier(tokens, anchor, "function name")?;

    if is_async && (name == "main" || export.is_some()) {
        return Err(ParseError::syntax_at(
            anchor,
            if name == "main" {
                "entry function `main` must be synchronous; start the executor with task::block_on"
            } else {
                "async functions cannot be exported through an FFI ABI"
            },
        )
        .with_context("async function"));
    }
    let generic_params = parse_generic_param_names(tokens)?;

    skip_ws(tokens);
    if tokens.peek().ok_or_else(|| invalid(None))?.token_type != TokenType::Lparen {
        return Err(invalid(tokens.peek().copied()));
    }

    tokens.next(); // consume '('
    let parameters = parse_parameters(tokens)?;

    skip_ws(tokens);
    let mut return_type_span = None;
    let return_type = if let Some(Token { token_type: TokenType::Arrow, .. }) = tokens.peek() {
        tokens.next(); // consume '->'
        let before_type = tokens.clone();
        skip_ws(tokens);
        let ty = crate::types::parse_type_checked(tokens, "function return type")?;
        return_type_span = lexer::consumed_span(before_type, tokens);
        Some(ty)
    } else {
        None
    };

    skip_ws(tokens);
    let body = extract_body(tokens)?;
    Ok(ASTNode::Function(FunctionNode {
        is_async,
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

pub fn parse_export(tokens: &mut Peekable<Iter<Token>>) -> Result<Vec<ASTNode>, ParseError> {
    let anchor = tokens.peek().copied();
    let invalid = |token| {
        ParseError::syntax_at(anchor, "failed to parse export declaration")
            .with_context("top-level export block/declaration")
            .with_expected_many([
                "export(c) fun name(...) { ... }",
                "export(c, \"symbol\") fun name(...) { ... }",
                "export(c) { fun a(...) { ... } fun b(...) { ... } }",
            ])
            .with_found_token(token)
            .with_help("exports require a concrete non-generic function body")
    };
    let (abi, global_symbol) =
        parse_ffi_header(tokens, "export").ok_or_else(|| invalid(tokens.peek().copied()))?;
    let export = ExportAttribute { abi, symbol: global_symbol };

    skip_ws(tokens);

    if tokens.peek().map(|t| t.token_type.clone()) == Some(TokenType::Lbrace) {
        if export.symbol.is_some() {
            println!("Error: export block cannot use a single symbol alias");
            return Err(invalid(tokens.peek().copied()));
        }

        tokens.next();

        let mut nodes = Vec::new();
        loop {
            skip_ws(tokens);

            match tokens.peek().map(|t| t.token_type.clone()) {
                Some(TokenType::Rbrace) => {
                    tokens.next();
                    break;
                },
                Some(TokenType::Fun | TokenType::Async) => {
                    let node = parse_function_with_export(tokens, Some(export.clone()))?;
                    if let ASTNode::Function(func) = &node {
                        if !func.generic_params.is_empty() {
                            println!("Error: exported functions cannot be generic");
                            return Err(invalid(tokens.peek().copied()));
                        }
                    }
                    nodes.push(node);
                },
                Some(TokenType::Whitespace) | Some(TokenType::Newline) => {
                    tokens.next();
                },
                other => {
                    println!("Error: Unexpected token in export block: {:?}", other);
                    return Err(invalid(tokens.peek().copied()));
                },
            }
        }

        skip_ws(tokens);
        if tokens.peek().map(|t| t.token_type.clone()) == Some(TokenType::SemiColon) {
            tokens.next();
        }

        Ok(nodes)
    } else if matches!(
        tokens.peek().map(|t| &t.token_type),
        Some(TokenType::Fun | TokenType::Async)
    ) {
        let node = parse_function_with_export(tokens, Some(export))?;
        if let ASTNode::Function(func) = &node {
            if !func.generic_params.is_empty() {
                println!("Error: exported functions cannot be generic");
                return Err(invalid(tokens.peek().copied()));
            }
        }
        Ok(vec![node])
    } else {
        println!("Error: Expected 'fun' or '{{' after export(...)");
        Err(invalid(tokens.peek().copied()))
    }
}

pub fn extract_body(tokens: &mut Peekable<Iter<Token>>) -> Result<Vec<ASTNode>, ParseError> {
    let anchor = tokens.peek().copied();
    let invalid = |token| ParseError::expected_at(token, anchor, "'{'", "function body");
    if tokens.peek().ok_or_else(|| invalid(None))?.token_type != TokenType::Lbrace {
        return Err(invalid(tokens.peek().copied()));
    }
    tokens.next();
    crate::parser::stmt::parse_block(tokens)
}
