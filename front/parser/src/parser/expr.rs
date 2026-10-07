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

//! Compatibility helpers for token-slice function calls and parenthesized spans.
//!
//! The main precedence parser handles current expression syntax. Keep these
//! helpers isolated so older parser consumers do not influence operator or
//! generic-call parsing.

use crate::ast::Expression;
use crate::expr::parse_expression;
use crate::parser::ParseError;
use lexer::token::TokenType;
use lexer::Token;
use std::iter::Peekable;
use std::slice::Iter;

pub fn parse_function_call(
    name: Option<String>,
    tokens: &mut Peekable<Iter<Token>>,
) -> Result<Expression, ParseError> {
    let anchor = tokens.peek().copied();
    let _nesting = crate::expression_depth::Nesting::enter(anchor)?;
    let invalid =
        |token| ParseError::expected_at(token, anchor, "valid function call", "function call");
    let name = name.ok_or_else(|| invalid(tokens.peek().copied()))?;

    if tokens.peek().ok_or_else(|| invalid(None))?.token_type != TokenType::Lparen {
        println!("❌ Expected '(' after function name '{}'", name);
        return Err(invalid(tokens.peek().copied()));
    }
    tokens.next(); // consume '('

    let mut args = vec![];

    while let Some(token) = tokens.peek() {
        if token.token_type == TokenType::Rparen {
            tokens.next(); // consume ')'
            break;
        }

        let arg = parse_expression(tokens)?;
        args.push(arg);

        match tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Comma) => {
                tokens.next(); // consume ','
            },
            Some(TokenType::Rparen) => continue,
            _ => {
                println!("❌ Unexpected token in function arguments: {:?}", tokens.peek());
                return Err(invalid(tokens.peek().copied()));
            },
        }
    }

    Ok(Expression::FunctionCall { name, type_args: Vec::new(), args })
}

pub fn parse_parentheses(tokens: &mut Peekable<Iter<Token>>) -> Vec<Token> {
    let mut param_tokens = vec![];
    let mut paren_depth = 1;

    while let Some(token) = tokens.next() {
        match token.token_type {
            TokenType::Lparen => paren_depth += 1,
            TokenType::Rparen => {
                paren_depth -= 1;
                if paren_depth == 0 {
                    break;
                }
            },
            _ => {},
        }
        param_tokens.push(token.clone());
    }
    param_tokens
}
