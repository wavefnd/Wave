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

//! Postfix chaining for fields, methods, indices, calls, and increment/decrement.
//!
//! The loop repeatedly wraps the expression parsed so far, allowing chains such
//! as field access followed by indexing. Postfix mutation is accepted only for
//! expressions classified as assignable by the shared expression helper.

use std::iter::Peekable;

use lexer::token::TokenType;
use lexer::Token;

use super::primary::{argument_list, expect_token, identifier, peek_is_generic_call};
use crate::ast::{Expression, IncDecKind};
use crate::expr::{is_assignable, parse_expression};
use crate::parser::ParseError;

pub fn parse_postfix_expression<'a, T>(
    tokens: &mut Peekable<T>,
    mut expr: Expression,
) -> Result<Expression, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let first = expr.span().cloned();
    let before = tokens.clone();
    loop {
        let mut focus = None;
        match tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Dot) => {
                let dot = tokens.next();
                focus = tokens.peek().and_then(|token| token.span.clone()).map(Box::new);
                let name = identifier(tokens, dot, "member access")?;
                let mut type_args = Vec::new();
                if peek_is_generic_call(tokens) {
                    tokens.next();
                    let inner = crate::decl::collect_generic_inner(tokens)
                        .expect("generic call lookahead validated the suffix");
                    for arg in crate::types::split_top_level_generic_args(&inner).unwrap() {
                        type_args.push(
                            crate::types::token_type_to_wave_type(
                                &crate::types::parse_type(&arg).unwrap(),
                            )
                            .unwrap(),
                        );
                    }
                }
                if tokens.peek().is_some_and(|token| token.token_type == TokenType::Lparen) {
                    let args = argument_list(tokens, TokenType::Rparen, "')'", "method call")?;
                    let base_expr = expr;
                    expr = Expression::MethodCall {
                        object: Box::new(base_expr),
                        name,
                        type_args,
                        args,
                    };
                } else {
                    // ----- FieldAccess -----
                    let base_expr = expr;
                    expr = Expression::FieldAccess { object: Box::new(base_expr), field: name };
                }
            },

            Some(TokenType::Lbrack) => {
                let _nesting = crate::expression_depth::Nesting::enter(tokens.peek().copied())?;
                let opener = tokens.next();
                let index_expr = parse_expression(tokens)?;
                expect_token(tokens, opener, TokenType::Rbrack, "']'", "index expression")?;

                let base_expr = expr;
                expr = Expression::IndexAccess {
                    target: Box::new(base_expr),
                    index: Box::new(index_expr),
                };
            },

            Some(TokenType::Increment) => {
                let operator = tokens.next(); // consume '++'

                if !is_assignable(&expr) {
                    return Err(ParseError::expected_at(
                        operator,
                        operator,
                        "assignable expression",
                        "postfix mutation",
                    ));
                }

                let base = expr;
                expr = Expression::IncDec { kind: IncDecKind::PostInc, target: Box::new(base) };

                return crate::expression_depth::parsed(expr, operator);
            },

            Some(TokenType::Decrement) => {
                let operator = tokens.next(); // consume '--'

                if !is_assignable(&expr) {
                    return Err(ParseError::expected_at(
                        operator,
                        operator,
                        "assignable expression",
                        "postfix mutation",
                    ));
                }

                let base = expr;
                expr = Expression::IncDec { kind: IncDecKind::PostDec, target: Box::new(base) };

                return crate::expression_depth::parsed(expr, operator);
            },

            _ => break,
        }
        let span = first.as_ref().zip(lexer::consumed_span(before.clone(), tokens)).map(
            |(first, last)| {
                let mut span = first.through(&last);
                span.focus = focus;
                span
            },
        );
        expr = crate::expression_depth::parsed(expr.with_span(span), tokens.peek().copied())?;
    }

    Ok(expr)
}
