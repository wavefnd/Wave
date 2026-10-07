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
//! Binary precedence is folded iteratively with a bounded operator stack.
//! Casts bind more tightly than every binary operator, as before.

use crate::ast::{Expression, Operator};
use crate::expr::unary::parse_unary_expression;
use crate::parser::ParseError;
use crate::types::parse_type_from_stream;
use lexer::token::TokenType;
use lexer::Token;

fn binary_operator(token: &TokenType) -> Option<(Operator, u8)> {
    Some(match token {
        TokenType::LogicalOr => (Operator::LogicalOr, 1),
        TokenType::LogicalAnd => (Operator::LogicalAnd, 2),
        TokenType::BitwiseOr => (Operator::BitwiseOr, 3),
        TokenType::Xor => (Operator::BitwiseXor, 4),
        TokenType::AddressOf => (Operator::BitwiseAnd, 5),
        TokenType::EqualTwo => (Operator::Equal, 6),
        TokenType::NotEqual => (Operator::NotEqual, 6),
        TokenType::Rchevr => (Operator::Greater, 7),
        TokenType::RchevrEq => (Operator::GreaterEqual, 7),
        TokenType::Lchevr => (Operator::Less, 7),
        TokenType::LchevrEq => (Operator::LessEqual, 7),
        TokenType::Rol => (Operator::ShiftLeft, 8),
        TokenType::Ror => (Operator::ShiftRight, 8),
        TokenType::Plus => (Operator::Add, 9),
        TokenType::Minus => (Operator::Subtract, 9),
        TokenType::Star => (Operator::Multiply, 10),
        TokenType::Div => (Operator::Divide, 10),
        TokenType::Remainder => (Operator::Remainder, 10),
        _ => return None,
    })
}

// Fold on an explicit operator stack. A parenthesized expression no longer
// consumes eleven native stack frames just to descend fixed precedence levels.
pub fn parse_logical_or_expression<'a, T>(
    tokens: &mut std::iter::Peekable<T>,
) -> Result<Expression, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let first = parse_cast_expression(tokens)?;
    parse_binary_tail(tokens, first)
}

fn parse_binary_tail<'a, T>(
    tokens: &mut std::iter::Peekable<T>,
    first: Expression,
) -> Result<Expression, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    fn fold(
        values: &mut Vec<Expression>,
        op: Operator,
        token: Option<&Token>,
    ) -> Result<(), ParseError> {
        let right = values.pop().expect("binary right operand");
        let left = values.pop().expect("binary left operand");
        values.push(crate::expression_depth::parsed(Expression::binary(left, op, right), token)?);
        Ok(())
    }
    let mut values = vec![first];
    let mut operators: Vec<(Operator, u8)> = Vec::new();
    while let Some((op, precedence)) =
        tokens.peek().and_then(|token| binary_operator(&token.token_type))
    {
        let anchor = tokens.next();
        while operators.last().is_some_and(|(_, previous)| *previous >= precedence) {
            let (previous, _) = operators.pop().unwrap();
            fold(&mut values, previous, anchor)?;
        }
        operators.push((op, precedence));
        values.push(parse_cast_expression(tokens)?);
    }
    while let Some((op, _)) = operators.pop() {
        fold(&mut values, op, tokens.peek().copied())?;
    }
    Ok(values.pop().expect("initial operand"))
}

fn parse_cast_expression<'a, T>(
    tokens: &mut std::iter::Peekable<T>,
) -> Result<Expression, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let expr = parse_unary_expression(tokens)?;
    parse_cast_tail(tokens, expr)
}

fn parse_cast_tail<'a, T>(
    tokens: &mut std::iter::Peekable<T>,
    mut expr: Expression,
) -> Result<Expression, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    while matches!(tokens.peek().map(|t| &t.token_type), Some(TokenType::As)) {
        let before = tokens.clone();
        let first = expr.span().cloned();
        tokens.next(); // consume `as`
        let anchor = tokens.peek().copied();
        let target_type = parse_type_from_stream(tokens).ok_or_else(|| {
            ParseError::expected_at(tokens.peek().copied(), anchor, "type", "cast expression")
        })?;
        expr = Expression::Cast { expr: Box::new(expr), target_type }.with_span(
            first
                .as_ref()
                .zip(lexer::consumed_span(before, tokens))
                .map(|(first, last)| first.through(&last)),
        );
        expr = crate::expression_depth::parsed(expr, anchor)?;
    }

    Ok(expr)
}
