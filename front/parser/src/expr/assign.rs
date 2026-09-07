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

//! Assignment expressions at the lowest precedence level.
//!
//! The right-hand side recurses at the same level, making chained assignment
//! right-associative. Assignability and mutability are validated after parsing.

use crate::ast::{AssignOperator, Expression};
use crate::expr::binary::parse_logical_or_expression;
use lexer::token::TokenType;
use lexer::Token;

pub fn parse_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    parse_assignment_expression(tokens)
}

pub fn parse_assignment_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let before = tokens.clone();
    let result = (|| {
        let left = parse_logical_or_expression(tokens)?;

        if let Some(token) = tokens.peek() {
            let op = match token.token_type {
                TokenType::Equal => AssignOperator::Assign,
                TokenType::PlusEq => AssignOperator::AddAssign,
                TokenType::MinusEq => AssignOperator::SubAssign,
                TokenType::StarEq => AssignOperator::MulAssign,
                TokenType::DivEq => AssignOperator::DivAssign,
                TokenType::RemainderEq => AssignOperator::RemAssign,
                _ => return Some(left),
            };

            tokens.next(); // consume op

            let right = parse_assignment_expression(tokens)?;
            return Some(Expression::AssignOperation {
                target: Box::new(left),
                operator: op,
                value: Box::new(right),
            });
        }

        Some(left)
    })();
    result.map(|value: Expression| value.with_span(lexer::consumed_span(before, tokens)))
}
