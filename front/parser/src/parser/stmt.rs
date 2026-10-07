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

//! Statement parsing and statement-level recovery boundaries.
//!
//! A statement parser consumes its complete terminator or block. Keeping that
//! ownership local prevents a failed statement from shifting the token stream
//! seen by the following declaration.

use crate::ast::{ASTNode, StatementNode};
use crate::expr::parse_expression;
use crate::parser::control::{parse_for, parse_if, parse_match, parse_while};
use crate::parser::decl::parse_var;
use crate::parser::io::*;
use crate::parser::types::is_expression_start;
use crate::parser::ParseError;
use lexer::token::TokenType;
use lexer::Token;
use std::iter::Peekable;
use std::slice::Iter;

fn semicolon(tokens: &mut Peekable<Iter<Token>>) -> Option<()> {
    if tokens.peek()?.token_type != TokenType::SemiColon {
        return None;
    }
    tokens.next();
    Some(())
}

pub fn parse_block(tokens: &mut Peekable<Iter<Token>>) -> Result<Vec<ASTNode>, ParseError> {
    let anchor = tokens.peek().copied();
    let mut body = vec![];
    loop {
        match tokens.peek().map(|token| &token.token_type) {
            Some(TokenType::Rbrace) => {
                tokens.next();
                return Ok(body);
            },
            None | Some(TokenType::Eof) => {
                return Err(ParseError::expected_at(
                    tokens.peek().copied(),
                    anchor,
                    "'}'",
                    "block",
                ));
            },
            _ => body.push(parse_statement(tokens)?),
        }
    }
}

pub fn parse_statement(tokens: &mut Peekable<Iter<Token>>) -> Result<ASTNode, ParseError> {
    let before = tokens.clone();
    let anchor = tokens.peek().copied();
    let result = match anchor.map(|token| &token.token_type) {
        Some(TokenType::If) => {
            tokens.next();
            parse_if(tokens)
        },
        Some(TokenType::For) => {
            tokens.next();
            parse_for(tokens)
        },
        Some(TokenType::While) => {
            tokens.next();
            parse_while(tokens)
        },
        Some(TokenType::Match) => {
            tokens.next();
            parse_match(tokens)
        },
        _ => parse_simple_statement(tokens),
    };
    result.map(|value: ASTNode| {
        let span = crate::source::node_span(before, tokens, &value);
        value.with_span(span)
    })
}

// Legacy statement forms still return Option; the caller supplies a structured fallback.
fn parse_simple_statement(tokens: &mut Peekable<Iter<Token>>) -> Result<ASTNode, ParseError> {
    let anchor = tokens.peek().copied();
    let invalid =
        |token| ParseError::expected_at(token, anchor, "valid block statement", "block statement");
    let token = match tokens.peek() {
        Some(t) => (*t).clone(),
        None => return Err(invalid(None)),
    };

    match token.token_type {
        TokenType::Var => {
            tokens.next();
            parse_var(tokens)
        },
        TokenType::Let | TokenType::Mut => {
            println!("Error: `let` and `let mut` declarations were removed; use `var`");
            Err(invalid(tokens.peek().copied()))
        },
        TokenType::Const | TokenType::Static => {
            Err(local_storage_error(tokens.peek().copied(), "block statement"))
        },
        TokenType::Println => {
            tokens.next();
            parse_println(tokens)
        },
        TokenType::Print => {
            tokens.next();
            parse_print(tokens)
        },
        TokenType::Input => {
            tokens.next();
            parse_input(tokens)
        },
        TokenType::Continue | TokenType::Break => {
            tokens.next();
            semicolon(tokens).ok_or_else(|| invalid(tokens.peek().copied()))?;
            Ok(ASTNode::Statement(if token.token_type == TokenType::Continue {
                StatementNode::Continue
            } else {
                StatementNode::Break
            }))
        },
        TokenType::Return => {
            tokens.next();
            let expr =
                if tokens.peek().ok_or_else(|| invalid(None))?.token_type == TokenType::SemiColon {
                    None
                } else {
                    Some(parse_expression(tokens)?)
                };
            semicolon(tokens).ok_or_else(|| invalid(tokens.peek().copied()))?;
            Ok(ASTNode::Statement(StatementNode::Return(expr)))
        },
        TokenType::Asm => {
            tokens.next();
            let node = crate::parser::asm::parse_asm_block(tokens)?;
            if tokens.peek().is_some_and(|t| t.token_type == TokenType::SemiColon) {
                tokens.next();
            }
            Ok(node)
        },
        TokenType::Rbrace => Err(invalid(tokens.peek().copied())),

        _ => {
            if is_expression_start(&token.token_type) {
                let expr = parse_expression(tokens)?;
                semicolon(tokens).ok_or_else(|| invalid(tokens.peek().copied()))?;
                Ok(ASTNode::Statement(StatementNode::Expression(expr)))
            } else {
                println!(
                    "Error: Unexpected token, cannot start a statement with: {:?}",
                    token.token_type
                );
                Err(invalid(tokens.peek().copied()))
            }
        },
    }
}

pub(crate) fn local_storage_error(token: Option<&Token>, context: &str) -> ParseError {
    ParseError::syntax_at(token, "`const` and `static` declarations are only allowed at top level")
        .with_context(context)
        .with_expected("var declaration or expression")
        .with_found_token(token)
        .with_help("use `var` for local storage, or move the declaration to top level")
}
