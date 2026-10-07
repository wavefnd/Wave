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

//! Primary expressions at the base of the precedence parser.
//!
//! This layer distinguishes literals, names, calls, struct literals, grouping,
//! and other atomic forms before postfix and binary operators are applied.

use std::iter::Peekable;

use lexer::token::TokenType;
use lexer::Token;

use crate::asm::parse_asm_body;
use crate::ast::{Expression, Literal};
use crate::decl::collect_generic_inner;
use crate::expr::parse_expression;
use crate::expr::postfix::parse_postfix_expression;
use crate::parser::ParseError;
use crate::types::{parse_type, split_top_level_generic_args, token_type_to_wave_type};

fn skip_ws<'a, T>(tokens: &mut Peekable<T>)
where
    T: Iterator<Item = &'a Token> + Clone,
{
    while matches!(
        tokens.peek().map(|t| &t.token_type),
        Some(TokenType::Whitespace | TokenType::Newline)
    ) {
        tokens.next();
    }
}

pub(super) fn peek_is_generic_call<'a, T>(tokens: &Peekable<T>) -> bool
where
    T: Iterator<Item = &'a Token> + Clone,
{
    peek_generic_suffix(tokens, TokenType::Lparen)
}

fn peek_is_generic_struct_literal<'a, T>(tokens: &Peekable<T>) -> bool
where
    T: Iterator<Item = &'a Token> + Clone,
{
    peek_generic_suffix(tokens, TokenType::Lbrace)
}

fn peek_generic_suffix<'a, T>(tokens: &Peekable<T>, suffix: TokenType) -> bool
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let mut probe = tokens.clone();
    if !matches!(probe.peek().map(|t| &t.token_type), Some(TokenType::Lchevr)) {
        return false;
    }
    probe.next(); // '<'
    let Some(inner) = collect_generic_inner(&mut probe) else {
        return false;
    };
    // A later comparison or match arrow can look like the closing '>'. Only
    // commit to a generic expression when its contents are valid type arguments.
    let Some(args) = split_top_level_generic_args(&inner) else {
        return false;
    };
    if args.is_empty() || args.iter().any(|arg| parse_type(arg).is_none()) {
        return false;
    }
    while matches!(
        probe.peek().map(|t| &t.token_type),
        Some(TokenType::Whitespace | TokenType::Newline)
    ) {
        probe.next();
    }
    probe.peek().is_some_and(|token| token.token_type == suffix)
}

pub(crate) fn expect_token<'a, T>(
    tokens: &mut Peekable<T>,
    anchor: Option<&Token>,
    kind: TokenType,
    expected: &str,
    context: &str,
) -> Result<(), ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    if tokens.peek().is_some_and(|token| token.token_type == kind) {
        tokens.next();
        Ok(())
    } else {
        Err(ParseError::expected_at(tokens.peek().copied(), anchor, expected, context))
    }
}

pub(crate) fn identifier<'a, T>(
    tokens: &mut Peekable<T>,
    anchor: Option<&Token>,
    context: &str,
) -> Result<String, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    match tokens.peek().copied() {
        Some(Token { token_type: TokenType::Identifier(name), .. }) => {
            let name = name.clone();
            tokens.next();
            Ok(name)
        },
        found => Err(ParseError::expected_at(found, anchor, "identifier", context)),
    }
}

pub(super) fn argument_list<'a, T>(
    tokens: &mut Peekable<T>,
    close: TokenType,
    spelling: &str,
    context: &str,
) -> Result<Vec<Expression>, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let _nesting = crate::expression_depth::Nesting::enter(tokens.peek().copied())?;
    let opener = tokens.next();
    let mut args = Vec::new();
    if tokens.peek().is_none_or(|token| token.token_type == TokenType::Eof) {
        return Err(ParseError::expected_at(tokens.peek().copied(), opener, spelling, context));
    }
    if tokens.peek().is_some_and(|token| token.token_type == close) {
        tokens.next();
        return Ok(args);
    }
    loop {
        args.push(parse_expression(tokens)?);
        if tokens.peek().is_some_and(|token| token.token_type == TokenType::Comma) {
            tokens.next();
        } else {
            expect_token(tokens, opener, close, spelling, context)?;
            return Ok(args);
        }
    }
}

fn parse_struct_literal_fields<'a, T>(
    tokens: &mut Peekable<T>,
) -> Result<Vec<(String, Expression)>, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let _nesting = crate::expression_depth::Nesting::enter(tokens.peek().copied())?;
    let opener = tokens.next();
    let mut fields = Vec::new();
    while !tokens.peek().is_some_and(|token| token.token_type == TokenType::Rbrace) {
        let name = struct_field_name(tokens, opener)?;
        fields.push((name, parse_expression(tokens)?));
        if finish_struct_field(tokens, opener)? {
            break;
        }
    }
    expect_token(tokens, opener, TokenType::Rbrace, "'}'", "struct literal")?;
    Ok(fields)
}

// Header/delimiter diagnostics do not need to stay on the recursive value
// parser's stack, especially when rejecting the 129th nested struct.
fn struct_field_name<'a, T>(
    tokens: &mut Peekable<T>,
    opener: Option<&Token>,
) -> Result<String, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    if tokens.peek().is_none_or(|token| token.token_type == TokenType::Eof) {
        return Err(ParseError::expected_at(
            tokens.peek().copied(),
            opener,
            "'}'",
            "struct literal",
        ));
    }
    let name = identifier(tokens, opener, "struct literal field")?;
    expect_token(tokens, opener, TokenType::Colon, "':'", "struct literal field")?;
    Ok(name)
}

fn finish_struct_field<'a, T>(
    tokens: &mut Peekable<T>,
    opener: Option<&Token>,
) -> Result<bool, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    match tokens.peek().map(|token| &token.token_type) {
        Some(TokenType::Comma) => {
            tokens.next();
            Ok(false)
        },
        Some(TokenType::Rbrace) => Ok(true),
        _ => Err(ParseError::expected_at(
            tokens.peek().copied(),
            opener,
            "',' or '}'",
            "struct literal",
        )),
    }
}

pub fn parse_primary_expression<'a, T>(tokens: &mut Peekable<T>) -> Result<Expression, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let before = tokens.clone();
    let token = tokens
        .peek()
        .copied()
        .ok_or_else(|| ParseError::expected_at(None, None, "expression", "primary expression"))?;
    let expr = parse_atom(tokens, token)?;
    finish_primary_expression(tokens, before, token, expr)
}

// Keep span/postfix temporaries out of the frame retained while parsing a
// nested atom. Debug builds must also fit the native Windows stack budget.
fn finish_primary_expression<'a, T>(
    tokens: &mut Peekable<T>,
    before: Peekable<T>,
    token: &Token,
    expr: Expression,
) -> Result<Expression, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let expr = crate::expression_depth::parsed(expr, Some(token))?;
    let base = expr.with_span(lexer::consumed_span(before.clone(), tokens));
    parse_postfix_expression(tokens, base).map(|value| {
        let mut span = lexer::consumed_span(before, tokens);
        if let Some(span) = &mut span {
            span.focus = value.span().and_then(|span| span.focus.clone());
        }
        value.with_span(span)
    })
}

fn parse_named_expression<'a, T>(
    tokens: &mut Peekable<T>,
    name: &str,
) -> Result<Expression, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let (name, type_args) = parse_expression_name(tokens, name)?;
    match tokens.peek().map(|token| &token.token_type) {
        Some(TokenType::Lparen) => argument_list(tokens, TokenType::Rparen, "')'", "function call")
            .map(|args| Expression::FunctionCall { name, type_args, args }),
        Some(TokenType::Lbrace) => parse_struct_literal_fields(tokens)
            .map(|fields| Expression::StructLiteral { name, fields }),
        _ => Ok(Expression::Variable(name)),
    }
}

fn parse_expression_name<'a, T>(
    tokens: &mut Peekable<T>,
    name: &str,
) -> Result<(String, Vec<crate::ast::WaveType>), ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let mut name = name.to_owned();
    tokens.next();
    while tokens.peek().is_some_and(|token| token.token_type == TokenType::DoubleColon) {
        let separator = tokens.next();
        let segment = identifier(tokens, separator, "qualified name")?;
        name.push_str("::");
        name.push_str(&segment);
    }
    let generic_call = peek_is_generic_call(tokens);
    let generic_struct = !generic_call && peek_is_generic_struct_literal(tokens);
    let mut type_args = Vec::new();
    if generic_call || generic_struct {
        let opener = tokens.next();
        let invalid =
            |found| ParseError::expected_at(found, opener, "type argument", "generic expression");
        let inner = collect_generic_inner(tokens).ok_or_else(|| invalid(tokens.peek().copied()))?;
        let arg_strs =
            split_top_level_generic_args(&inner).ok_or_else(|| invalid(tokens.peek().copied()))?;
        for arg in &arg_strs {
            let ty = parse_type(arg)
                .and_then(|ty| token_type_to_wave_type(&ty))
                .ok_or_else(|| invalid(tokens.peek().copied()))?;
            type_args.push(ty);
        }
        if generic_struct {
            name.push('<');
            name.push_str(&arg_strs.join(","));
            name.push('>');
        }
        skip_ws(tokens);
    }
    Ok((name, type_args))
}

fn parse_atom<'a, T>(tokens: &mut Peekable<T>, token: &Token) -> Result<Expression, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    match &token.token_type {
        TokenType::Identifier(name) => parse_named_expression(tokens, name),
        TokenType::Lparen => parse_group(tokens, token),
        TokenType::Lbrack => Ok(Expression::ArrayLiteral(argument_list(
            tokens,
            TokenType::Rbrack,
            "']'",
            "array literal",
        )?)),
        TokenType::Asm => parse_asm_expression(tokens, token),
        _ => parse_literal(tokens, token),
    }
}

fn parse_asm_expression<'a, T>(
    tokens: &mut Peekable<T>,
    token: &Token,
) -> Result<Expression, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let _nesting = crate::expression_depth::Nesting::enter(Some(token))?;
    tokens.next();
    let (instructions, inputs, outputs, clobbers) = parse_asm_body(tokens)?;
    Ok(Expression::AsmBlock { instructions, inputs, outputs, clobbers })
}

fn parse_literal<'a, T>(tokens: &mut Peekable<T>, token: &Token) -> Result<Expression, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    match &token.token_type {
        TokenType::IntLiteral(s) => {
            tokens.next();
            Ok(Expression::Literal(Literal::Int(s.clone())))
        },
        TokenType::Float(value) => {
            tokens.next();
            Ok(Expression::Literal(Literal::Float(*value)))
        },
        TokenType::CharLiteral(c) => {
            tokens.next();
            Ok(Expression::Literal(Literal::Char(*c)))
        },
        TokenType::BoolLiteral(b) => {
            tokens.next();
            Ok(Expression::Literal(Literal::Bool(*b)))
        },
        TokenType::String(s) => {
            tokens.next();
            Ok(Expression::Literal(Literal::String(s.clone())))
        },
        TokenType::Null => {
            tokens.next();
            Ok(Expression::Null)
        },
        _ => Err(ParseError::expected_at(
            Some(token),
            Some(token),
            "expression",
            "primary expression",
        )),
    }
}

fn parse_group<'a, T>(tokens: &mut Peekable<T>, token: &Token) -> Result<Expression, ParseError>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let _nesting = crate::expression_depth::Nesting::enter(Some(token))?;
    let opener = tokens.next();
    let inner = parse_expression(tokens)?;
    expect_token(tokens, opener, TokenType::Rparen, "')'", "grouped expression")?;
    Ok(Expression::Grouped(Box::new(inner)))
}
