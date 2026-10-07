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

use super::primary::{expect_token, identifier};
use crate::ast::Expression;
use crate::expr::parse_expression;
use crate::expr::unary::parse_unary_expression;
use crate::parser::ParseError;
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
) -> Result<Expression, ParseError> {
    loop {
        let anchor = tokens.peek().copied();
        match tokens.peek().map(|t| &t.token_type) {
            // a.b
            Some(TokenType::Dot) => {
                let dot = tokens.next();
                let field = identifier(tokens, dot, "member access")?;

                base = Expression::FieldAccess { object: Box::new(base), field };
            },

            // a[b]
            Some(TokenType::Lbrack) => {
                let _nesting = crate::expression_depth::Nesting::enter(tokens.peek().copied())?;
                let opener = tokens.next();
                let idx = parse_expression(tokens)?;
                expect_token(tokens, opener, TokenType::Rbrack, "']'", "index expression")?;

                base = Expression::IndexAccess { target: Box::new(base), index: Box::new(idx) };
            },

            _ => break,
        }
        base = crate::expression_depth::parsed(base, anchor)?;
    }

    Ok(base)
}

pub fn parse_expression_from_token(
    first_token: &Token,
    tokens: &mut Peekable<Iter<Token>>,
) -> Result<Expression, ParseError> {
    match &first_token.token_type {
        TokenType::Identifier(name) => {
            let base = Expression::Variable(name.clone());
            parse_lvalue_tail(base, tokens)
        },

        TokenType::Deref => {
            let _nesting = crate::expression_depth::Nesting::enter(Some(first_token))?;
            let inner = parse_unary_expression(tokens)?;
            Ok(Expression::Deref(Box::new(inner)))
        },

        _ => Err(ParseError::expected_at(
            Some(first_token),
            Some(first_token),
            "lvalue",
            "assignment target",
        )),
    }
}
