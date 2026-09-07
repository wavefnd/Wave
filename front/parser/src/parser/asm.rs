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
use lexer::token::TokenType;
use lexer::Token;
use std::iter::Peekable;
use std::slice::Iter;

pub fn parse_asm_block(tokens: &mut Peekable<Iter<'_, Token>>) -> Option<ASTNode> {
    if tokens.peek()?.token_type != TokenType::Lbrace {
        println!("Expected '{{' after 'asm'");
        return None;
    }
    tokens.next(); // consume '{'

    let mut instructions = vec![];
    let mut inputs: Vec<(String, Expression)> = vec![];
    let mut outputs: Vec<(String, Expression)> = vec![];
    let mut clobbers: Vec<String> = vec![];

    let mut closed = false;

    while let Some(tok) = tokens.peek() {
        match &tok.token_type {
            TokenType::Rbrace => {
                tokens.next(); // consume '}'
                closed = true;
                break;
            }

            TokenType::SemiColon | TokenType::Comma => {
                tokens.next();
            }

            TokenType::String(s) => {
                instructions.push(s.clone());
                tokens.next();
            }

            TokenType::In => {
                tokens.next(); // consume 'in'
                parse_asm_inout_clause(tokens, true, &mut inputs, &mut outputs)?;
            }

            TokenType::Out => {
                tokens.next(); // consume 'out'
                parse_asm_inout_clause(tokens, false, &mut inputs, &mut outputs)?;
            }

            TokenType::Clobber => {
                tokens.next();
                parse_asm_clobber_clause(tokens, &mut clobbers)?;
            }

            TokenType::Identifier(s) if s == "in" => {
                tokens.next();
                parse_asm_inout_clause(tokens, true, &mut inputs, &mut outputs)?;
            }
            TokenType::Identifier(s) if s == "out" => {
                tokens.next();
                parse_asm_inout_clause(tokens, false, &mut inputs, &mut outputs)?;
            }
            TokenType::Identifier(s) if s == "clobber" => {
                tokens.next();
                parse_asm_clobber_clause(tokens, &mut clobbers)?;
            }

            other => {
                println!("Unexpected token in asm block: {:?}", other);
                return None;
            }
        }
    }

    if !closed {
        println!("Expected '}}' to close asm block");
        return None;
    }

    Some(ASTNode::Statement(StatementNode::AsmBlock {
        instructions,
        inputs,
        outputs,
        clobbers,
    }))
}

/// clobber("rax, "rcx, "memory")
pub fn parse_asm_clobber_clause<'a, T>(
    tokens: &mut Peekable<T>,
    clobbers: &mut Vec<String>,
) -> Option<()>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    // expect '('
    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::Lparen) {
        println!("Expected '(' after 'clobber'");
        return None;
    }
    tokens.next(); // '('

    // empty: clobber()
    if tokens.peek().map(|t| &t.token_type) == Some(&TokenType::Rparen) {
        tokens.next(); // ')'
        return Some(());
    }

    loop {
        let item = match tokens.next() {
            Some(Token {
                token_type: TokenType::String(s),
                ..
            }) => s.clone(),
            Some(Token {
                token_type: TokenType::Identifier(s),
                ..
            }) => s.clone(),
            Some(other) => {
                println!(
                    "Expected clobber item (string/identifier), got {:?}",
                    other.token_type
                );
                return None;
            }
            None => {
                println!("Unexpected EOF while parsing clobber(...)");
                return None;
            }
        };

        clobbers.push(item);

        match tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Comma) => {
                tokens.next(); // ','
                continue;
            }
            Some(TokenType::Rparen) => {
                tokens.next(); // ')'
                break;
            }
            other => {
                println!("Expected ',' or ')' in clobber(...), got {:?}", other);
                return None;
            }
        }
    }

    Some(())
}

pub fn parse_asm_inout_clause<'a, T>(
    tokens: &mut Peekable<T>,
    is_input: bool,
    inputs: &mut Vec<(String, Expression)>,
    outputs: &mut Vec<(String, Expression)>,
) -> Option<()>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::Lparen) {
        println!("Expected '(' after in/out");
        return None;
    }
    tokens.next(); // '('

    let reg = match tokens.next() {
        Some(Token {
            token_type: TokenType::String(s),
            ..
        }) => s.clone(),
        Some(Token {
            token_type: TokenType::Identifier(s),
            ..
        }) => s.clone(),
        Some(other) => {
            println!(
                "Expected register string or identifier, got {:?}",
                other.token_type
            );
            return None;
        }
        None => {
            println!("Expected register in in/out(...)");
            return None;
        }
    };

    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::Rparen) {
        println!("Expected ')' after in/out(...)");
        return None;
    }
    tokens.next(); // ')'

    let value_expr = parse_asm_operand(tokens)?;

    if is_input {
        inputs.push((reg, value_expr));
    } else {
        if !is_assignable(&value_expr) {
            println!("Error: out(...) target must be assignable");
            return None;
        }
        outputs.push((reg, value_expr));
    }

    Some(())
}

pub(crate) fn parse_asm_operand<'a, T>(tokens: &mut Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    crate::expr::parse_expression(tokens)
}
