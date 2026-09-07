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

//! Precedence-climbing entry points for binary and cast expressions.
//!
//! Precedence is encoded by the call chain rather than a numeric table: each
//! function parses its tighter-binding child, then folds operators at its own
//! level from left to right. Insert a new operator at the intended layer instead
//! of handling it in the primary-expression parser.

use crate::ast::{Expression, Operator};
use crate::expr::unary::parse_unary_expression;
use crate::types::parse_type_from_stream;
use lexer::token::TokenType;
use lexer::Token;

pub fn parse_logical_or_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let mut left = parse_logical_and_expression(tokens)?;

    while matches!(
        tokens.peek().map(|t| &t.token_type),
        Some(TokenType::LogicalOr)
    ) {
        tokens.next();
        let right = parse_logical_and_expression(tokens)?;
        left = Expression::binary(left, Operator::LogicalOr, right);
    }

    Some(left)
}

pub fn parse_logical_and_expression<'a, T>(
    tokens: &mut std::iter::Peekable<T>,
) -> Option<Expression>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let mut left = parse_bitwise_or_expression(tokens)?;

    while matches!(
        tokens.peek().map(|t| &t.token_type),
        Some(TokenType::LogicalAnd)
    ) {
        tokens.next();
        let right = parse_bitwise_or_expression(tokens)?;
        left = Expression::binary(left, Operator::LogicalAnd, right);
    }

    Some(left)
}

pub fn parse_bitwise_or_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let mut left = parse_bitwise_xor_expression(tokens)?;

    while matches!(
        tokens.peek().map(|t| &t.token_type),
        Some(TokenType::BitwiseOr)
    ) {
        tokens.next();
        let right = parse_bitwise_xor_expression(tokens)?;
        left = Expression::binary(left, Operator::BitwiseOr, right);
    }

    Some(left)
}

pub fn parse_bitwise_xor_expression<'a, T>(
    tokens: &mut std::iter::Peekable<T>,
) -> Option<Expression>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let mut left = parse_bitwise_and_expression(tokens)?;

    while matches!(tokens.peek().map(|t| &t.token_type), Some(TokenType::Xor)) {
        tokens.next();
        let right = parse_bitwise_and_expression(tokens)?;
        left = Expression::binary(left, Operator::BitwiseXor, right);
    }

    Some(left)
}

pub fn parse_bitwise_and_expression<'a, T>(
    tokens: &mut std::iter::Peekable<T>,
) -> Option<Expression>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let mut left = parse_equality_expression(tokens)?;

    while matches!(
        tokens.peek().map(|t| &t.token_type),
        Some(TokenType::AddressOf)
    ) {
        tokens.next();
        let right = parse_equality_expression(tokens)?;
        left = Expression::binary(left, Operator::BitwiseAnd, right);
    }

    Some(left)
}

pub fn parse_equality_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let mut left = parse_relational_expression(tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.token_type {
            TokenType::EqualTwo => Operator::Equal,
            TokenType::NotEqual => Operator::NotEqual,
            _ => break,
        };
        tokens.next();
        let right = parse_relational_expression(tokens)?;
        left = Expression::binary(left, op, right);
    }

    Some(left)
}

pub fn parse_relational_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let mut left = parse_shift_expression(tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.token_type {
            TokenType::Rchevr => Operator::Greater,
            TokenType::RchevrEq => Operator::GreaterEqual,
            TokenType::Lchevr => Operator::Less,
            TokenType::LchevrEq => Operator::LessEqual,
            _ => break,
        };
        tokens.next();
        let right = parse_shift_expression(tokens)?;
        left = Expression::binary(left, op, right);
    }

    Some(left)
}

pub fn parse_shift_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let mut left = parse_additive_expression(tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.token_type {
            TokenType::Rol => Operator::ShiftLeft,
            TokenType::Ror => Operator::ShiftRight,
            _ => break,
        };

        tokens.next();
        let right = parse_additive_expression(tokens)?;
        left = Expression::binary(left, op, right);
    }

    Some(left)
}

pub fn parse_additive_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let mut left = parse_multiplicative_expression(tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.token_type {
            TokenType::Plus => Operator::Add,
            TokenType::Minus => Operator::Subtract,
            _ => break,
        };
        tokens.next();
        let right = parse_multiplicative_expression(tokens)?;
        left = Expression::binary(left, op, right);
    }

    Some(left)
}

pub fn parse_multiplicative_expression<'a, T>(
    tokens: &mut std::iter::Peekable<T>,
) -> Option<Expression>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let mut left = parse_cast_expression(tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.token_type {
            TokenType::Star => Operator::Multiply,
            TokenType::Div => Operator::Divide,
            TokenType::Remainder => Operator::Remainder,
            _ => break,
        };
        tokens.next();
        let right = parse_cast_expression(tokens)?;
        left = Expression::binary(left, op, right);
    }

    Some(left)
}

fn parse_cast_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let mut expr = parse_unary_expression(tokens)?;

    while matches!(tokens.peek().map(|t| &t.token_type), Some(TokenType::As)) {
        let before = tokens.clone();
        let first = expr.span().cloned();
        tokens.next(); // consume `as`
        let target_type = parse_type_from_stream(tokens)?;
        expr = Expression::Cast {
            expr: Box::new(expr),
            target_type,
        }
        .with_span(
            first
                .as_ref()
                .zip(lexer::consumed_span(before, tokens))
                .map(|(first, last)| first.through(&last)),
        );
    }

    Some(expr)
}
