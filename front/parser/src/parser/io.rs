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

//! Parsing of `print`, `println`, and `input` statements.
//!
//! Placeholder counts are checked while parsing so the AST distinguishes plain
//! literal output from formatted calls. Argument types and C format conversion
//! remain semantic/backend responsibilities.

use crate::ast::{ASTNode, StatementNode};
use crate::expr::parse_expression;
use crate::parser::ParseError;
use lexer::token::TokenType;
use lexer::Token;
use std::iter::Peekable;
use std::slice::Iter;
use utils::formatx::*;

pub fn parse_println(tokens: &mut Peekable<Iter<Token>>) -> Result<ASTNode, ParseError> {
    let anchor = tokens.peek().copied();
    let invalid = |token| ParseError::expected_at(token, anchor, "valid println", "println");
    if tokens.peek().ok_or_else(|| invalid(None))?.token_type != TokenType::Lparen {
        println!("Error: Expected '(' after 'println'");
        return Err(invalid(tokens.peek().copied()));
    }
    tokens.next(); // Consume '('

    let content_token = tokens.peek().copied();
    let mut content =
        if let Some(Token { token_type: TokenType::String(content), .. }) = tokens.next() {
            content.clone()
        } else {
            println!("Error: Expected string literal in 'println'");
            return Err(invalid(tokens.peek().copied()));
        };

    validate_placeholder_text(&content, content_token, false)?;
    let placeholder_count = count_placeholders(&content);

    if placeholder_count == 0 {
        if tokens.peek().ok_or_else(|| invalid(None))?.token_type != TokenType::Rparen {
            println!("Error: Expected closing ')'");
            return Err(invalid(tokens.peek().copied()));
        }
        tokens.next(); // Consume ')'

        if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
            println!("Expected ';' after expression");
            return Err(invalid(tokens.peek().copied()));
        }
        tokens.next();

        content.push(b'\n');
        return Ok(ASTNode::Statement(StatementNode::Println(content)));
    }

    let mut args = Vec::new();
    while let Some(Token { token_type: TokenType::Comma, .. }) = tokens.peek() {
        tokens.next(); // Consume ','
        args.push(parse_expression(tokens)?);
    }

    if tokens.peek().ok_or_else(|| invalid(None))?.token_type != TokenType::Rparen {
        println!("Error: Expected closing ')'");
        return Err(invalid(tokens.peek().copied()));
    }
    tokens.next(); // Consume ')'

    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
        println!("Expected ';' after expression");
        return Err(invalid(tokens.peek().copied()));
    }
    tokens.next();

    if placeholder_count != args.len() {
        println!("Error: Expected {} arguments, found {}", placeholder_count, args.len());
        return Err(ParseError::syntax_at(
            content_token,
            format!("expected {placeholder_count} format arguments, found {}", args.len()),
        )
        .with_context("format arguments"));
    }

    content.push(b'\n');
    Ok(ASTNode::Statement(StatementNode::PrintlnFormat { format: content, args }))
}

// PRINT parsing
pub fn parse_print(tokens: &mut Peekable<Iter<Token>>) -> Result<ASTNode, ParseError> {
    let anchor = tokens.peek().copied();
    let invalid = |token| ParseError::expected_at(token, anchor, "valid print", "print");
    if tokens.peek().ok_or_else(|| invalid(None))?.token_type != TokenType::Lparen {
        println!("Error: Expected '(' after 'println'");
        return Err(invalid(tokens.peek().copied()));
    }
    tokens.next(); // Consume '('

    let content_token = tokens.peek().copied();
    let content = if let Some(Token { token_type: TokenType::String(content), .. }) = tokens.next()
    {
        content.clone()
    } else {
        println!("Error: Expected string literal in 'println'");
        return Err(invalid(tokens.peek().copied()));
    };

    validate_placeholder_text(&content, content_token, false)?;
    let placeholder_count = count_placeholders(&content);

    if placeholder_count == 0 {
        // No format → Print just a string
        if tokens.peek().ok_or_else(|| invalid(None))?.token_type != TokenType::Rparen {
            println!("Error: Expected closing ')'");
            return Err(invalid(tokens.peek().copied()));
        }
        tokens.next(); // Consume ')'

        if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
            println!("Expected ';' after expression");
            return Err(invalid(tokens.peek().copied()));
        }
        tokens.next();

        return Ok(ASTNode::Statement(StatementNode::Print(content)));
    }

    let mut args = Vec::new();
    while let Some(Token { token_type: TokenType::Comma, .. }) = tokens.peek() {
        tokens.next(); // Consume ','
        args.push(parse_expression(tokens)?);
    }

    if tokens.peek().ok_or_else(|| invalid(None))?.token_type != TokenType::Rparen {
        println!("Error: Expected closing ')'");
        return Err(invalid(tokens.peek().copied()));
    }
    tokens.next(); // Consume ')'

    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
        println!("Expected ';' after expression");
        return Err(invalid(tokens.peek().copied()));
    }
    tokens.next();

    if placeholder_count != args.len() {
        println!("Error: Expected {} arguments, found {}", placeholder_count, args.len());
        return Err(ParseError::syntax_at(
            content_token,
            format!("expected {placeholder_count} format arguments, found {}", args.len()),
        )
        .with_context("format arguments"));
    }

    Ok(ASTNode::Statement(StatementNode::PrintFormat { format: content, args }))
}

pub fn parse_input(tokens: &mut Peekable<Iter<Token>>) -> Result<ASTNode, ParseError> {
    let anchor = tokens.peek().copied();
    let invalid = |token| ParseError::expected_at(token, anchor, "valid input", "input");
    if tokens.peek().ok_or_else(|| invalid(None))?.token_type != TokenType::Lparen {
        println!("Error: Expected '(' after 'println'");
        return Err(invalid(tokens.peek().copied()));
    }
    tokens.next(); // Consume '('

    let content_token = tokens.peek().copied();
    let content = if let Some(Token { token_type: TokenType::String(content), .. }) = tokens.next()
    {
        content.clone() // Need clone() because it is String
    } else {
        println!("Error: Expected string literal in 'input'");
        return Err(invalid(tokens.peek().copied()));
    };

    validate_placeholder_text(&content, content_token, true)?;
    let placeholder_count = count_placeholders(&content);

    let mut args = Vec::new();
    while let Some(Token { token_type: TokenType::Comma, .. }) = tokens.peek() {
        tokens.next(); // Consume ','
        args.push(parse_expression(tokens)?);
    }

    if tokens.peek().ok_or_else(|| invalid(None))?.token_type != TokenType::Rparen {
        println!("Error: Expected closing ')'");
        return Err(invalid(tokens.peek().copied()));
    }
    tokens.next(); // Consume ')'

    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
        println!("Expected ';' after expression");
        return Err(invalid(tokens.peek().copied()));
    }
    tokens.next();

    if placeholder_count != args.len() {
        println!("Error: Expected {} arguments, found {}", placeholder_count, args.len());
        return Err(invalid(tokens.peek().copied()));
    }

    Ok(ASTNode::Statement(StatementNode::Input { format: content, args }))
}

// Literal bytes outside placeholders are unrestricted. Placeholder names are
// compiler text and must remain valid UTF-8 before backend format conversion.
fn validate_placeholder_text(
    content: &[u8],
    token: Option<&Token>,
    input: bool,
) -> Result<(), ParseError> {
    let invalid = |message: String| {
        ParseError::syntax_at(token, message)
            .with_context("format placeholder")
            .with_found_token(token)
    };
    let parts = format_fragments(content).map_err(|e| invalid(e.into()))?;
    for part in parts {
        if let FormatFragment::Placeholder(spec) = part {
            if !(spec.is_empty() || !input && matches!(spec, "c" | "x" | "p" | "s" | "d")) {
                return Err(invalid(format!("unsupported format placeholder `{{{spec}}}`")));
            }
        }
    }
    Ok(())
}
