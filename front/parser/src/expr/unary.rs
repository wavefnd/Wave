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

//! Prefix unary parsing before primary and postfix expressions.
//!
//! Unary operators are folded right-to-left on a bounded explicit stack. Address-of and
//! prefix increment/decrement additionally require an assignable operand.

use crate::ast::{Expression, IncDecKind, Literal, Operator};
use crate::expr::is_assignable;
use crate::expr::primary::parse_primary_expression;
use crate::parser::ParseError;
use lexer::token::TokenType;
use lexer::Token;

pub fn parse_unary_expression<'a, T>(
    tokens: &mut std::iter::Peekable<T>,
) -> Result<Expression, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    if !tokens.peek().is_some_and(|token| {
        matches!(
            token.token_type,
            TokenType::Await
                | TokenType::Not
                | TokenType::BitwiseNot
                | TokenType::AddressOf
                | TokenType::Deref
                | TokenType::Increment
                | TokenType::Decrement
                | TokenType::Minus
                | TokenType::Plus
        )
    }) {
        return parse_primary_expression(tokens);
    }
    parse_prefix_expression(tokens)
}

fn parse_prefix_expression<'a, T>(
    tokens: &mut std::iter::Peekable<T>,
) -> Result<Expression, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let mut prefixes = Vec::new();
    while let Some(token) = tokens.peek().copied() {
        if !matches!(
            token.token_type,
            TokenType::Await
                | TokenType::Not
                | TokenType::BitwiseNot
                | TokenType::AddressOf
                | TokenType::Deref
                | TokenType::Increment
                | TokenType::Decrement
                | TokenType::Minus
                | TokenType::Plus
        ) {
            break;
        }
        let nesting = crate::expression_depth::Nesting::enter(Some(token))?;
        prefixes.push((tokens.clone(), token, nesting));
        tokens.next();
    }
    let mut value = parse_primary_expression(tokens)?;
    while let Some((before, token, nesting)) = prefixes.pop() {
        value = apply_prefix(token, value)?;
        drop(nesting);
        value = crate::expression_depth::parsed(
            value.with_span(lexer::consumed_span(before, tokens)),
            Some(token),
        )?;
    }
    Ok(value)
}

fn apply_prefix(token: &Token, inner: Expression) -> Result<Expression, ParseError> {
    Ok(match token.token_type {
        TokenType::Await => Expression::Await(Box::new(inner)),
        TokenType::Not => Expression::Unary { operator: Operator::Not, expr: Box::new(inner) },
        TokenType::BitwiseNot => {
            Expression::Unary { operator: Operator::BitwiseNot, expr: Box::new(inner) }
        },
        TokenType::AddressOf => Expression::AddressOf(Box::new(inner)),
        TokenType::Deref => Expression::Deref(Box::new(inner)),
        TokenType::Increment | TokenType::Decrement => {
            if !is_assignable(&inner) {
                return Err(ParseError::expected_at(
                    Some(token),
                    Some(token),
                    "assignable expression",
                    "prefix mutation",
                ));
            }
            Expression::IncDec {
                kind: if token.token_type == TokenType::Increment {
                    IncDecKind::PreInc
                } else {
                    IncDecKind::PreDec
                },
                target: Box::new(inner),
            }
        },
        TokenType::Minus => match inner.into_unspanned() {
            Expression::Literal(Literal::Int(s)) => Expression::Literal(Literal::Int(
                s.strip_prefix('-').map(str::to_string).unwrap_or_else(|| format!("-{s}")),
            )),
            Expression::Literal(Literal::Float(f)) => Expression::Literal(Literal::Float(-f)),
            other => Expression::Unary { operator: Operator::Neg, expr: Box::new(other) },
        },
        TokenType::Plus => inner,
        _ => unreachable!("prefix operator was checked"),
    })
}
