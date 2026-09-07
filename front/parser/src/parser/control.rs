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

//! Parsing for conditional, loop, and match statements.
//!
//! Control-flow parsers delegate bodies to the shared block parser and return
//! syntax-only nodes. Match-arm `=>` is currently represented by two lexer
//! tokens, so this module owns consuming that pair as one delimiter.

use crate::ast::{
    ASTNode, Expression, MatchArm, MatchPattern, Mutability, StatementNode, VariableNode,
    Visibility,
};
use crate::expr::parse_expression;
use crate::parser::stmt::parse_block;
use crate::parser::types::parse_type_from_stream;
use lexer::token::TokenType;
use lexer::Token;
use std::iter::Peekable;
use std::slice::Iter;

fn skip_ws_and_newlines(tokens: &mut Peekable<Iter<Token>>) {
    while let Some(t) = tokens.peek() {
        match t.token_type {
            TokenType::Whitespace | TokenType::Newline => {
                tokens.next();
            }
            _ => break,
        }
    }
}

fn expect_fat_arrow(tokens: &mut Peekable<Iter<Token>>) -> bool {
    skip_ws_and_newlines(tokens);

    if !matches!(tokens.peek().map(|t| &t.token_type), Some(TokenType::Equal)) {
        println!("Error: Expected '=' in match arm (use `=>`)");
        return false;
    }
    tokens.next();

    skip_ws_and_newlines(tokens);

    if !matches!(
        tokens.peek().map(|t| &t.token_type),
        Some(TokenType::Rchevr)
    ) {
        println!("Error: Expected '>' in match arm (use `=>`)");
        return false;
    }
    tokens.next();

    true
}

fn parse_match_pattern(
    tokens: &mut Peekable<Iter<Token>>,
    payload_position: bool,
) -> Option<MatchPattern> {
    let before = tokens.clone();
    let result = (|| {
        skip_ws_and_newlines(tokens);

        match tokens.next()? {
            Token {
                token_type: TokenType::IntLiteral(v),
                ..
            } => Some(MatchPattern::Int(v.clone())),
            Token {
                token_type: TokenType::Identifier(name),
                ..
            } => {
                if name == "_" {
                    return Some(MatchPattern::Wildcard);
                }

                let mut segments = vec![name.clone()];
                loop {
                    skip_ws_and_newlines(tokens);
                    if !matches!(
                        tokens.peek().map(|token| &token.token_type),
                        Some(TokenType::DoubleColon)
                    ) {
                        break;
                    }
                    tokens.next();
                    skip_ws_and_newlines(tokens);
                    match tokens.next() {
                        Some(Token {
                            token_type: TokenType::Identifier(segment),
                            ..
                        }) => segments.push(segment.clone()),
                        _ => {
                            println!("Error: Expected case name after '::' in match pattern");
                            return None;
                        }
                    }
                }

                if segments.len() == 1 {
                    return if payload_position {
                        Some(MatchPattern::Binding(name.clone()))
                    } else {
                        Some(MatchPattern::Ident(name.clone()))
                    };
                }

                let case_name = segments.pop().unwrap();
                let variant_type = segments.join("::");
                let mut payloads = Vec::new();
                skip_ws_and_newlines(tokens);
                if matches!(
                    tokens.peek().map(|token| &token.token_type),
                    Some(TokenType::Lparen)
                ) {
                    tokens.next();
                    loop {
                        skip_ws_and_newlines(tokens);
                        if matches!(
                            tokens.peek().map(|token| &token.token_type),
                            Some(TokenType::Rparen)
                        ) {
                            tokens.next();
                            break;
                        }
                        payloads.push(parse_match_pattern(tokens, true)?);
                        skip_ws_and_newlines(tokens);
                        match tokens.peek().map(|token| &token.token_type) {
                            Some(TokenType::Comma) => {
                                tokens.next();
                            }
                            Some(TokenType::Rparen) => {
                                tokens.next();
                                break;
                            }
                            _ => {
                                println!("Error: Expected ',' or ')' in variant pattern");
                                return None;
                            }
                        }
                    }
                }

                Some(MatchPattern::Variant {
                    variant_type,
                    case_name,
                    payloads,
                })
            }
            other => {
                println!(
                "Error: Invalid match pattern {:?} (expected integer literal, enum variant, or `_`)",
                other.token_type
            );
                None
            }
        }
    })();
    result.map(|value: MatchPattern| value.with_span(lexer::consumed_span(before, tokens)))
}

pub fn parse_if(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    if tokens.peek()?.token_type != TokenType::Lparen {
        println!("Error: Expected '(' after 'if'");
        return None;
    }
    tokens.next(); // Consume '('

    let condition = parse_expression(tokens)?;

    if tokens.peek()?.token_type != TokenType::Rparen {
        println!("Error: Expected ')' after 'if' condition");
        return None;
    }
    tokens.next(); // Consume ')'

    if tokens.peek()?.token_type != TokenType::Lbrace {
        println!("Error: Expected '{{' after 'if' condition");
        return None;
    }
    tokens.next(); // Consume '{'
    let body = parse_block(tokens)?;

    let mut else_if_blocks: Vec<(Expression, Vec<ASTNode>)> = Vec::new(); // Changed to store conditions and bodies
    let mut else_block = None;

    while let Some(token) = tokens.peek() {
        if token.token_type != TokenType::Else {
            break;
        }
        tokens.next(); // consume 'else'

        if let Some(Token {
            token_type: TokenType::If,
            ..
        }) = tokens.peek()
        {
            tokens.next(); // consume 'if'

            if tokens.peek()?.token_type != TokenType::Lparen {
                println!("Error: Expected '(' after 'else if'");
                return None;
            }
            tokens.next();
            let else_if_condition = parse_expression(tokens)?;

            if tokens.peek()?.token_type != TokenType::Rparen {
                println!("Error: Expected ')' after 'else if' condition");
                return None;
            }
            tokens.next();

            if tokens.peek()?.token_type != TokenType::Lbrace {
                println!("Error: Expected '{{' after 'else if'");
                return None;
            }
            tokens.next();
            let else_if_body = parse_block(tokens)?;

            // Store condition and body directly instead of nested If node
            else_if_blocks.push((else_if_condition, else_if_body));
        } else {
            if tokens.peek()?.token_type != TokenType::Lbrace {
                println!("Error: Expected '{{' after 'else'");
                return None;
            }
            tokens.next();
            else_block = Some(Box::new(parse_block(tokens)?));
            break;
        }
    }

    Some(ASTNode::Statement(StatementNode::If {
        condition,
        body,
        else_if_blocks: if else_if_blocks.is_empty() {
            None
        } else {
            Some(Box::new(else_if_blocks))
        },
        else_block,
    }))
}

fn is_typed_for_initializer(tokens: &Peekable<Iter<Token>>) -> bool {
    let mut look = tokens.clone();
    matches!(
        look.next().map(|t| &t.token_type),
        Some(TokenType::Identifier(_))
    ) && matches!(look.next().map(|t| &t.token_type), Some(TokenType::Colon))
}

fn parse_typed_for_initializer(
    tokens: &mut Peekable<Iter<Token>>,
    mutability: Mutability,
) -> Option<ASTNode> {
    let name = match tokens.next() {
        Some(Token {
            token_type: TokenType::Identifier(name),
            ..
        }) => name.clone(),
        _ => {
            println!("Error: Expected identifier in for-loop initializer");
            return None;
        }
    };

    if tokens.peek()?.token_type != TokenType::Colon {
        println!(
            "Error: Expected ':' after '{}' in for-loop initializer",
            name
        );
        return None;
    }
    tokens.next(); // consume ':'

    let type_name = match parse_type_from_stream(tokens) {
        Some(ty) => ty,
        None => {
            println!("Error: Expected type in for-loop initializer");
            return None;
        }
    };

    let initial_value = if tokens.peek()?.token_type == TokenType::Equal {
        tokens.next(); // consume '='
        Some(parse_expression(tokens)?)
    } else {
        None
    };

    Some(ASTNode::Variable(VariableNode {
        name,
        type_name,
        initial_value,
        mutability,
        visibility: Visibility::Private,
    }))
}

fn parse_for_initializer(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    let before = tokens.clone();
    let result = (|| {
        match tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Var) => {
                tokens.next(); // consume `var`
                parse_typed_for_initializer(tokens, Mutability::Var)
            }
            Some(TokenType::Const) => {
                println!("Error: `const` is not allowed in local for-loop initializer");
                None
            }
            Some(TokenType::Static) => {
                println!("Error: `static` is not allowed in local for-loop initializer");
                None
            }
            _ if is_typed_for_initializer(tokens) => {
                parse_typed_for_initializer(tokens, Mutability::Var)
            }
            _ => {
                let expr = parse_expression(tokens)?;
                Some(ASTNode::Statement(StatementNode::Expression(expr)))
            }
        }
    })();
    result.map(|value: ASTNode| {
        let span = crate::source::node_span(before, tokens, &value);
        value.with_span(span)
    })
}

// FOR parsing
pub fn parse_for(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    if tokens.peek()?.token_type != TokenType::Lparen {
        println!("Error: Expected '(' after 'for'");
        return None;
    }
    tokens.next(); // Consume '('

    let initialization = parse_for_initializer(tokens)?;

    if tokens.peek()?.token_type != TokenType::SemiColon {
        println!("Error: Expected ';' after for-loop initializer");
        return None;
    }
    tokens.next(); // Consume ';'

    let condition = parse_expression(tokens)?;

    if tokens.peek()?.token_type != TokenType::SemiColon {
        println!("Error: Expected ';' after for-loop condition");
        return None;
    }
    tokens.next(); // Consume ';'

    let increment = parse_expression(tokens)?;

    if tokens.peek()?.token_type != TokenType::Rparen {
        println!("Error: Expected ')' after for-loop increment");
        return None;
    }
    tokens.next(); // Consume ')'

    if tokens.peek()?.token_type != TokenType::Lbrace {
        println!("Error: Expected '{{' after 'for' header");
        return None;
    }
    tokens.next(); // Consume '{'

    let body = parse_block(tokens)?;

    Some(ASTNode::Statement(StatementNode::For {
        initialization: Box::new(initialization),
        condition,
        increment,
        body,
    }))
}

// WHILE parsing
pub fn parse_while(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    if tokens.peek()?.token_type != TokenType::Lparen {
        println!("Error: Expected '(' after 'while'");
        return None;
    }
    tokens.next(); // Consume '('

    let condition = parse_expression(tokens)?;

    if tokens.peek()?.token_type != TokenType::Rparen {
        println!("Error: Expected ')' after 'while' condition");
        return None;
    }
    tokens.next(); // Consume ')'

    if tokens.peek()?.token_type != TokenType::Lbrace {
        println!("Error: Expected '{{' after 'while'");
        return None;
    }
    tokens.next(); // Consume '{'

    let body = parse_block(tokens)?;

    Some(ASTNode::Statement(StatementNode::While { condition, body }))
}

pub fn parse_match(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    skip_ws_and_newlines(tokens);

    let parenthesized = tokens.peek()?.token_type == TokenType::Lparen;
    if parenthesized {
        tokens.next();
    }
    let value = if parenthesized {
        parse_expression(tokens)?
    } else {
        let mut expression_tokens = Vec::new();
        while let Some(token) = tokens.peek() {
            if token.token_type == TokenType::Lbrace {
                break;
            }
            expression_tokens.push((*token).clone());
            tokens.next();
        }
        let mut expression_iter = expression_tokens.iter().peekable();
        let value = parse_expression(&mut expression_iter)?;
        while matches!(
            expression_iter.peek().map(|token| &token.token_type),
            Some(TokenType::Whitespace | TokenType::Newline)
        ) {
            expression_iter.next();
        }
        if expression_iter.peek().is_some() {
            println!("Error: Unexpected token after match value");
            return None;
        }
        value
    };
    if parenthesized {
        skip_ws_and_newlines(tokens);
        if tokens.peek()?.token_type != TokenType::Rparen {
            println!("Error: Expected ')' after match value");
            return None;
        }
        tokens.next();
    }

    skip_ws_and_newlines(tokens);
    if tokens.peek()?.token_type != TokenType::Lbrace {
        println!("Error: Expected '{{' after match header");
        return None;
    }
    tokens.next(); // consume '{'

    let mut arms: Vec<MatchArm> = Vec::new();
    let mut saw_wildcard = false;

    loop {
        skip_ws_and_newlines(tokens);

        if matches!(
            tokens.peek().map(|t| &t.token_type),
            Some(TokenType::Rbrace)
        ) {
            tokens.next(); // consume '}'
            break;
        }

        let before = tokens.clone();
        let pattern = parse_match_pattern(tokens, false)?;
        if matches!(pattern.unspanned(), MatchPattern::Wildcard) {
            if saw_wildcard {
                println!("Error: Duplicate wildcard arm `_` in match");
                return None;
            }
            saw_wildcard = true;
        }

        if !expect_fat_arrow(tokens) {
            return None;
        }

        skip_ws_and_newlines(tokens);
        if tokens.peek()?.token_type != TokenType::Lbrace {
            println!("Error: Expected '{{' to start match arm body");
            return None;
        }
        tokens.next(); // consume '{'

        let body = parse_block(tokens)?;
        arms.push(MatchArm {
            pattern,
            body,
            span: lexer::consumed_span(before, tokens),
        });

        skip_ws_and_newlines(tokens);
        if matches!(
            tokens.peek().map(|t| &t.token_type),
            Some(TokenType::Comma | TokenType::SemiColon)
        ) {
            tokens.next();
        }
    }

    Some(ASTNode::Statement(StatementNode::Match { value, arms }))
}
