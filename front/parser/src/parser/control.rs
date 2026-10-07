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
use crate::parser::ParseError;
use lexer::token::TokenType;
use lexer::Token;
use std::iter::Peekable;
use std::slice::Iter;

fn skip_ws_and_newlines(tokens: &mut Peekable<Iter<Token>>) {
    while let Some(t) = tokens.peek() {
        match t.token_type {
            TokenType::Whitespace | TokenType::Newline => {
                tokens.next();
            },
            _ => break,
        }
    }
}

fn expect_fat_arrow(tokens: &mut Peekable<Iter<Token>>) -> Result<(), ParseError> {
    let anchor = tokens.peek().copied();
    skip_ws_and_newlines(tokens);
    expect_header_token(tokens, anchor, TokenType::Equal, "'=>'", "match arm")?;
    skip_ws_and_newlines(tokens);
    expect_header_token(tokens, anchor, TokenType::Rchevr, "'=>'", "match arm")
}

fn parse_match_pattern(
    tokens: &mut Peekable<Iter<Token>>,
    payload_position: bool,
) -> Result<MatchPattern, ParseError> {
    skip_ws_and_newlines(tokens);
    let before = tokens.clone();
    let anchor = tokens.peek().copied();
    let value = match anchor.map(|t| &t.token_type) {
        Some(TokenType::Minus) => {
            tokens.next();
            skip_ws_and_newlines(tokens);
            let token = tokens.peek().copied();
            let Some(Token { token_type: TokenType::IntLiteral(value), .. }) = token else {
                return Err(ParseError::expected_at(
                    token,
                    anchor,
                    "integer literal after '-'",
                    "match pattern",
                ));
            };
            tokens.next();
            MatchPattern::Int(format!("-{value}"))
        },
        Some(TokenType::IntLiteral(value)) => {
            tokens.next();
            MatchPattern::Int(value.clone())
        },
        Some(TokenType::Identifier(name)) => {
            tokens.next();
            if name == "_" {
                MatchPattern::Wildcard
            } else {
                let mut segments = vec![name.clone()];
                loop {
                    skip_ws_and_newlines(tokens);
                    if !matches!(tokens.peek().map(|t| &t.token_type), Some(TokenType::DoubleColon))
                    {
                        break;
                    }
                    tokens.next();
                    skip_ws_and_newlines(tokens);
                    segments.push(crate::expr::identifier(tokens, anchor, "match pattern")?);
                }
                if segments.len() == 1 {
                    if payload_position {
                        MatchPattern::Binding(name.clone())
                    } else {
                        MatchPattern::Ident(name.clone())
                    }
                } else {
                    let case_name = segments.pop().unwrap();
                    let variant_type = segments.join("::");
                    let mut payloads = Vec::new();
                    skip_ws_and_newlines(tokens);
                    if matches!(tokens.peek().map(|t| &t.token_type), Some(TokenType::Lparen)) {
                        tokens.next();
                        loop {
                            skip_ws_and_newlines(tokens);
                            if matches!(
                                tokens.peek().map(|t| &t.token_type),
                                Some(TokenType::Rparen)
                            ) {
                                tokens.next();
                                break;
                            }
                            payloads.push(parse_match_pattern(tokens, true)?);
                            skip_ws_and_newlines(tokens);
                            match tokens.peek().map(|t| &t.token_type) {
                                Some(TokenType::Comma) => {
                                    tokens.next();
                                },
                                Some(TokenType::Rparen) => {
                                    tokens.next();
                                    break;
                                },
                                _ => {
                                    return Err(ParseError::expected_at(
                                        tokens.peek().copied(),
                                        anchor,
                                        "',' or ')'",
                                        "variant pattern",
                                    ))
                                },
                            }
                        }
                    }
                    MatchPattern::Variant { variant_type, case_name, payloads }
                }
            }
        },
        _ => {
            return Err(ParseError::expected_at(
                anchor,
                anchor,
                "integer literal, case name, or '_'",
                "match pattern",
            ))
        },
    };
    Ok(value.with_span(lexer::consumed_span(before, tokens)))
}

fn expect_header_token(
    tokens: &mut Peekable<Iter<Token>>,
    anchor: Option<&Token>,
    kind: TokenType,
    spelling: &str,
    context: &str,
) -> Result<(), ParseError> {
    if tokens.peek().is_some_and(|token| token.token_type == kind) {
        tokens.next();
        Ok(())
    } else {
        Err(ParseError::expected_at(tokens.peek().copied(), anchor, spelling, context))
    }
}

fn header_expression(
    tokens: &mut Peekable<Iter<Token>>,
    context: &str,
) -> Result<Expression, ParseError> {
    parse_expression(tokens).map_err(|error| {
        if error.context() == Some("primary expression") {
            error.with_context(context)
        } else {
            error
        }
    })
}

fn conditional_body(
    tokens: &mut Peekable<Iter<Token>>,
    context: &str,
) -> Result<(Expression, Vec<ASTNode>), ParseError> {
    let anchor = tokens.peek().copied();
    expect_header_token(tokens, anchor, TokenType::Lparen, "'('", context)?;
    let condition = header_expression(tokens, context)?;
    expect_header_token(tokens, anchor, TokenType::Rparen, "')'", context)?;
    expect_header_token(tokens, anchor, TokenType::Lbrace, "'{'", context)?;
    Ok((condition, parse_block(tokens)?))
}

pub fn parse_if(tokens: &mut Peekable<Iter<Token>>) -> Result<ASTNode, ParseError> {
    let (condition, body) = conditional_body(tokens, "if header")?;
    let mut else_if_blocks = Vec::new();
    let mut else_block = None;
    while tokens.peek().is_some_and(|token| token.token_type == TokenType::Else) {
        let anchor = tokens.next();
        if tokens.peek().is_some_and(|token| token.token_type == TokenType::If) {
            tokens.next();
            else_if_blocks.push(conditional_body(tokens, "else if header")?);
        } else {
            expect_header_token(tokens, anchor, TokenType::Lbrace, "'{'", "else header")?;
            else_block = Some(Box::new(parse_block(tokens)?));
            break;
        }
    }
    Ok(ASTNode::Statement(StatementNode::If {
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
    matches!(look.next().map(|t| &t.token_type), Some(TokenType::Identifier(_)))
        && matches!(look.next().map(|t| &t.token_type), Some(TokenType::Colon))
}

fn parse_typed_for_initializer(
    tokens: &mut Peekable<Iter<Token>>,
    mutability: Mutability,
) -> Result<ASTNode, ParseError> {
    let anchor = tokens.peek().copied();
    let name = match tokens.peek().copied() {
        Some(Token { token_type: TokenType::Identifier(name), .. }) => name.clone(),
        other => {
            return Err(ParseError::expected_at(other, anchor, "identifier", "for initializer"))
        },
    };
    tokens.next();
    expect_header_token(tokens, anchor, TokenType::Colon, "':'", "for initializer")?;
    let type_name = parse_type_from_stream(tokens).ok_or_else(|| {
        ParseError::expected_at(tokens.peek().copied(), anchor, "type", "for initializer")
    })?;
    let initial_value = if tokens.peek().is_some_and(|token| token.token_type == TokenType::Equal) {
        tokens.next();
        Some(header_expression(tokens, "for initializer")?)
    } else {
        None
    };

    Ok(ASTNode::Variable(VariableNode {
        name,
        type_name,
        initial_value,
        mutability,
        visibility: Visibility::Private,
    }))
}

fn parse_for_initializer(tokens: &mut Peekable<Iter<Token>>) -> Result<ASTNode, ParseError> {
    let before = tokens.clone();
    let result = (|| {
        match tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Var) => {
                tokens.next(); // consume `var`
                parse_typed_for_initializer(tokens, Mutability::Var)
            },
            Some(TokenType::Const | TokenType::Static) => Err(
                crate::parser::stmt::local_storage_error(tokens.peek().copied(), "for initializer"),
            ),
            _ if is_typed_for_initializer(tokens) => {
                parse_typed_for_initializer(tokens, Mutability::Var)
            },
            _ => {
                let expr = header_expression(tokens, "for initializer")?;
                Ok(ASTNode::Statement(StatementNode::Expression(expr)))
            },
        }
    })();
    result.map(|value: ASTNode| {
        let span = crate::source::node_span(before, tokens, &value);
        value.with_span(span)
    })
}

pub fn parse_for(tokens: &mut Peekable<Iter<Token>>) -> Result<ASTNode, ParseError> {
    let anchor = tokens.peek().copied();
    expect_header_token(tokens, anchor, TokenType::Lparen, "'('", "for header")?;
    let initialization = parse_for_initializer(tokens)?;
    expect_header_token(tokens, anchor, TokenType::SemiColon, "';'", "for initializer")?;
    let condition = header_expression(tokens, "for condition")?;
    expect_header_token(tokens, anchor, TokenType::SemiColon, "';'", "for condition")?;
    let increment = header_expression(tokens, "for increment")?;
    expect_header_token(tokens, anchor, TokenType::Rparen, "')'", "for increment")?;
    expect_header_token(tokens, anchor, TokenType::Lbrace, "'{'", "for header")?;
    let body = parse_block(tokens)?;
    Ok(ASTNode::Statement(StatementNode::For {
        initialization: Box::new(initialization),
        condition,
        increment,
        body,
    }))
}

pub fn parse_while(tokens: &mut Peekable<Iter<Token>>) -> Result<ASTNode, ParseError> {
    let (condition, body) = conditional_body(tokens, "while header")?;
    Ok(ASTNode::Statement(StatementNode::While { condition, body }))
}

pub fn parse_match(tokens: &mut Peekable<Iter<Token>>) -> Result<ASTNode, ParseError> {
    let anchor = tokens.peek().copied();
    let invalid =
        |token| ParseError::expected_at(token, anchor, "match value and arms", "match statement");
    skip_ws_and_newlines(tokens);

    let parenthesized = tokens.peek().ok_or_else(|| invalid(None))?.token_type == TokenType::Lparen;
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
            return Err(ParseError::expected_at(
                expression_iter.peek().copied(),
                anchor,
                "'{'",
                "match header",
            ));
        }
        value
    };
    if parenthesized {
        skip_ws_and_newlines(tokens);
        expect_header_token(tokens, anchor, TokenType::Rparen, "')'", "match header")?;
    }

    skip_ws_and_newlines(tokens);
    expect_header_token(tokens, anchor, TokenType::Lbrace, "'{'", "match header")?;

    let mut arms: Vec<MatchArm> = Vec::new();
    let mut saw_wildcard = false;

    loop {
        skip_ws_and_newlines(tokens);

        if matches!(tokens.peek().map(|t| &t.token_type), Some(TokenType::Rbrace)) {
            tokens.next(); // consume '}'
            break;
        }

        let before = tokens.clone();
        let pattern_token = tokens.peek().copied();
        let pattern = parse_match_pattern(tokens, false)?;
        if matches!(pattern.unspanned(), MatchPattern::Wildcard) {
            if saw_wildcard {
                return Err(ParseError::syntax_at(
                    pattern_token,
                    "duplicate wildcard arm `_` in match",
                )
                .with_context("match pattern")
                .with_found_token(pattern_token)
                .with_help("keep only one wildcard arm"));
            }
            saw_wildcard = true;
        }

        expect_fat_arrow(tokens)?;
        skip_ws_and_newlines(tokens);
        expect_header_token(tokens, anchor, TokenType::Lbrace, "'{'", "match arm body")?;

        let body = parse_block(tokens)?;
        arms.push(MatchArm { pattern, body, span: lexer::consumed_span(before, tokens) });

        skip_ws_and_newlines(tokens);
        if matches!(
            tokens.peek().map(|t| &t.token_type),
            Some(TokenType::Comma | TokenType::SemiColon)
        ) {
            tokens.next();
        }
    }

    Ok(ASTNode::Statement(StatementNode::Match { value, arms }))
}
