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

//! Inline-assembly syntax for instruction, input, output, and clobber clauses.
//!
//! This parser validates clause shape and assignable output expressions. Target
//! register names and stack contracts are intentionally deferred to the
//! architecture-aware backend planner.

use crate::ast::{ASTNode, Expression, StatementNode};
use crate::expr::is_assignable;
use crate::parser::ParseError;
use lexer::token::TokenType;
use lexer::Token;
use std::iter::Peekable;
use std::slice::Iter;

type AsmBody = (Vec<String>, Vec<(String, Expression)>, Vec<(String, Expression)>, Vec<String>);

pub fn parse_asm_block(tokens: &mut Peekable<Iter<'_, Token>>) -> Result<ASTNode, ParseError> {
    let (instructions, inputs, outputs, clobbers) = parse_asm_body(tokens)?;
    Ok(ASTNode::Statement(StatementNode::AsmBlock { instructions, inputs, outputs, clobbers }))
}

pub(crate) fn parse_asm_body<'a, T>(tokens: &mut Peekable<T>) -> Result<AsmBody, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let anchor = tokens.peek().copied();
    crate::expr::expect_token(tokens, anchor, TokenType::Lbrace, "'{'", "asm block")?;
    let (mut instructions, mut inputs, mut outputs, mut clobbers) =
        (vec![], vec![], vec![], vec![]);
    loop {
        match tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Rbrace) => {
                tokens.next();
                break;
            },
            Some(TokenType::SemiColon | TokenType::Comma) => {
                tokens.next();
            },
            Some(TokenType::String(s)) => {
                instructions.push(String::from_utf8(s.clone()).map_err(|_| {
                    ParseError::expected_at(
                        tokens.peek().copied(),
                        anchor,
                        "UTF-8 assembly text",
                        "asm block",
                    )
                })?);
                tokens.next();
            },
            Some(TokenType::In) => {
                tokens.next();
                parse_asm_inout_clause(tokens, true, &mut inputs, &mut outputs)?;
            },
            Some(TokenType::Out) => {
                tokens.next();
                parse_asm_inout_clause(tokens, false, &mut inputs, &mut outputs)?;
            },
            Some(TokenType::Clobber) => {
                tokens.next();
                parse_asm_clobber_clause(tokens, &mut clobbers)?;
            },
            Some(TokenType::Identifier(s)) if s == "in" || s == "out" || s == "clobber" => {
                let clause = s.clone();
                tokens.next();
                if clause == "clobber" {
                    parse_asm_clobber_clause(tokens, &mut clobbers)?;
                } else {
                    parse_asm_inout_clause(tokens, clause == "in", &mut inputs, &mut outputs)?;
                }
            },
            found => {
                return Err(ParseError::expected_at(
                    tokens.peek().copied(),
                    anchor,
                    if found.is_none() || found == Some(&TokenType::Eof) {
                        "'}'"
                    } else {
                        "instruction string, in, out, clobber, or '}'"
                    },
                    "asm block",
                ))
            },
        }
    }
    Ok((instructions, inputs, outputs, clobbers))
}

fn register<'a, T>(tokens: &mut Peekable<T>, context: &str) -> Result<String, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    match tokens.peek().copied() {
        Some(Token { token_type: TokenType::Identifier(s), .. }) => {
            let name = s.clone();
            tokens.next();
            Ok(name)
        },
        Some(Token { token_type: TokenType::String(s), .. }) => {
            let name = String::from_utf8(s.clone()).map_err(|_| {
                ParseError::expected_at(
                    tokens.peek().copied(),
                    tokens.peek().copied(),
                    "UTF-8 register name",
                    context,
                )
            })?;
            tokens.next();
            Ok(name)
        },
        found => {
            Err(ParseError::expected_at(found, found, "register string or identifier", context))
        },
    }
}

pub fn parse_asm_clobber_clause<'a, T>(
    tokens: &mut Peekable<T>,
    clobbers: &mut Vec<String>,
) -> Result<(), ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let anchor = tokens.peek().copied();
    let context = "asm clobber clause";
    crate::expr::expect_token(tokens, anchor, TokenType::Lparen, "'('", context)?;
    if tokens.peek().is_some_and(|t| t.token_type == TokenType::Rparen) {
        tokens.next();
        return Ok(());
    }
    loop {
        clobbers.push(register(tokens, context)?);
        if tokens.peek().is_some_and(|t| t.token_type == TokenType::Comma) {
            tokens.next();
        } else {
            crate::expr::expect_token(tokens, anchor, TokenType::Rparen, "',' or ')'", context)?;
            return Ok(());
        }
    }
}

pub fn parse_asm_inout_clause<'a, T>(
    tokens: &mut Peekable<T>,
    is_input: bool,
    inputs: &mut Vec<(String, Expression)>,
    outputs: &mut Vec<(String, Expression)>,
) -> Result<(), ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let anchor = tokens.peek().copied();
    let context = if is_input { "asm input clause" } else { "asm output clause" };
    crate::expr::expect_token(tokens, anchor, TokenType::Lparen, "'('", context)?;
    let reg = register(tokens, context)?;
    crate::expr::expect_token(tokens, anchor, TokenType::Rparen, "')'", context)?;
    let operand = tokens.peek().copied();
    let value = parse_asm_operand(tokens)?;
    if is_input {
        inputs.push((reg, value));
    } else {
        if !is_assignable(&value) {
            return Err(ParseError::expected_at(operand, anchor, "assignable expression", context));
        }
        outputs.push((reg, value));
    }
    Ok(())
}

pub(crate) fn parse_asm_operand<'a, T>(tokens: &mut Peekable<T>) -> Result<Expression, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    crate::expr::parse_expression(tokens)
}
