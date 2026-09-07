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

//! Shared expression predicates and lvalue reconstruction helpers.
//!
//! Assignability is a syntax property here: variables, dereferences, fields, and
//! indices may form storage targets. Mutability and type legality are checked by
//! semantic validation.

use crate::ast::Expression;
use crate::expr::parse_expression;
use crate::expr::unary::parse_unary_expression;
use lexer::token::TokenType;
use lexer::Token;
use std::iter::Peekable;
use std::slice::Iter;

pub fn is_assignable(expr: &Expression) -> bool {
    match expr.unspanned() {
        Expression::Variable(_) => true,
        Expression::Deref(_) => true,
        Expression::FieldAccess { .. } => true,
        Expression::IndexAccess { .. } => true,

        Expression::Grouped(inner) => is_assignable(inner),

        _ => false,
    }
}

fn parse_lvalue_tail(
    mut base: Expression,
    tokens: &mut Peekable<Iter<Token>>,
) -> Option<Expression> {
    loop {
        match tokens.peek().map(|t| &t.token_type) {
            // a.b
            Some(TokenType::Dot) => {
                tokens.next(); // '.'
                let field = match tokens.next() {
                    Some(Token {
                        token_type: TokenType::Identifier(s),
                        ..
                    }) => s.clone(),
                    _ => {
                        println!("Error: Expected identifier after '.'");
                        return None;
                    }
                };

                base = Expression::FieldAccess {
                    object: Box::new(base),
                    field,
                };
            }

            // a[b]
            Some(TokenType::Lbrack) => {
                tokens.next(); // '['
                let idx = parse_expression(tokens)?;
                if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::Rbrack) {
                    println!("Error: Expected ']' after index expression");
                    return None;
                }
                tokens.next(); // ']'

                base = Expression::IndexAccess {
                    target: Box::new(base),
                    index: Box::new(idx),
                };
            }

            _ => break,
        }
    }

    Some(base)
}

pub fn parse_expression_from_token(
    first_token: &Token,
    tokens: &mut Peekable<Iter<Token>>,
) -> Option<Expression> {
    match &first_token.token_type {
        TokenType::Identifier(name) => {
            let base = Expression::Variable(name.clone());
            parse_lvalue_tail(base, tokens)
        }

        TokenType::Deref => {
            let inner = parse_unary_expression(tokens)?;
            Some(Expression::Deref(Box::new(inner)))
        }

        _ => None,
    }
}
