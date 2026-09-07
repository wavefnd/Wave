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

//! Statement parsing and statement-level recovery boundaries.
//!
//! A statement parser consumes its complete terminator or block. Keeping that
//! ownership local prevents a failed statement from shifting the token stream
//! seen by the following declaration.

use crate::ast::{ASTNode, StatementNode};
use crate::expr::parse_expression;
use crate::parser::control::{parse_for, parse_if, parse_match, parse_while};
use crate::parser::decl::parse_var;
use crate::parser::io::*;
use crate::parser::types::is_expression_start;
use lexer::token::TokenType;
use lexer::Token;
use std::iter::Peekable;
use std::slice::Iter;

fn semicolon(tokens: &mut Peekable<Iter<Token>>) -> Option<()> {
    if tokens.peek()?.token_type != TokenType::SemiColon {
        return None;
    }
    tokens.next();
    Some(())
}

pub fn parse_block(tokens: &mut Peekable<Iter<Token>>) -> Option<Vec<ASTNode>> {
    let mut body = vec![];

    while let Some(token) = tokens.peek() {
        if token.token_type == TokenType::Rbrace {
            break;
        }

        if let Some(node) = parse_statement(tokens) {
            body.push(node);
        } else {
            println!("Error: Failed to parse statement inside block.");
            return None;
        }
    }

    if let Some(token) = tokens.next() {
        if token.token_type != TokenType::Rbrace {
            println!(
                "Error: Expected '}}' to close the block, but found {:?}",
                token.token_type
            );
            return None;
        }
    } else {
        println!("Error: Unexpected end of file, expected '}}'");
        return None;
    }

    Some(body)
}

pub fn parse_statement(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    let before = tokens.clone();
    let result = (|| {
        let token = match tokens.peek() {
            Some(t) => (*t).clone(),
            None => return None,
        };

        let node = match token.token_type {
            TokenType::Var => {
                tokens.next();
                parse_var(tokens)
            }
            TokenType::Let | TokenType::Mut => {
                println!("Error: `let` and `let mut` declarations were removed; use `var`");
                None
            }
            TokenType::Const => {
                println!("Error: `const` is only allowed at top level");
                None
            }
            TokenType::Static => {
                println!("Error: `static` is only allowed at top level");
                None
            }
            TokenType::Println => {
                tokens.next();
                parse_println(tokens)
            }
            TokenType::Print => {
                tokens.next();
                parse_print(tokens)
            }
            TokenType::Input => {
                tokens.next();
                parse_input(tokens)
            }
            TokenType::If => {
                tokens.next();
                parse_if(tokens)
            }
            TokenType::For => {
                tokens.next();
                parse_for(tokens)
            }
            TokenType::While => {
                tokens.next();
                parse_while(tokens)
            }
            TokenType::Match => {
                tokens.next();
                parse_match(tokens)
            }
            TokenType::Continue | TokenType::Break => {
                tokens.next();
                semicolon(tokens)?;
                Some(ASTNode::Statement(
                    if token.token_type == TokenType::Continue {
                        StatementNode::Continue
                    } else {
                        StatementNode::Break
                    },
                ))
            }
            TokenType::Return => {
                tokens.next();
                let expr = if tokens.peek()?.token_type == TokenType::SemiColon {
                    None
                } else {
                    Some(parse_expression(tokens)?)
                };
                semicolon(tokens)?;
                Some(ASTNode::Statement(StatementNode::Return(expr)))
            }
            TokenType::Asm => {
                tokens.next();
                let node = crate::parser::asm::parse_asm_block(tokens)?;
                if tokens
                    .peek()
                    .is_some_and(|t| t.token_type == TokenType::SemiColon)
                {
                    tokens.next();
                }
                Some(node)
            }
            TokenType::Rbrace => None,

            _ => {
                if is_expression_start(&token.token_type) {
                    if let Some(expr) = parse_expression(tokens) {
                        semicolon(tokens)?;
                        Some(ASTNode::Statement(StatementNode::Expression(expr)))
                    } else {
                        println!("Error: Failed to parse expression statement.");
                        None
                    }
                } else {
                    println!(
                        "Error: Unexpected token, cannot start a statement with: {:?}",
                        token.token_type
                    );
                    None
                }
            }
        };

        node
    })();
    result.map(|value: ASTNode| {
        let span = crate::source::node_span(before, tokens, &value);
        value.with_span(span)
    })
}
