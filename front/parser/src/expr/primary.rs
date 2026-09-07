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

use crate::asm::{parse_asm_clobber_clause, parse_asm_inout_clause};
use crate::ast::{Expression, Literal};
use crate::decl::collect_generic_inner;
use crate::expr::parse_expression;
use crate::expr::postfix::parse_postfix_expression;
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

fn peek_is_generic_call<'a, T>(tokens: &Peekable<T>) -> bool
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
    if collect_generic_inner(&mut probe).is_none() {
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

fn parse_struct_literal_fields<'a, T>(tokens: &mut Peekable<T>) -> Option<Vec<(String, Expression)>>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let mut fields = vec![];

    while tokens
        .peek()
        .is_some_and(|token| token.token_type != TokenType::Rbrace)
    {
        let field_name = if let Some(Token {
            token_type: TokenType::Identifier(name),
            ..
        }) = tokens.next()
        {
            name.clone()
        } else {
            println!("Error: Expected field name in struct literal.");
            return None;
        };

        if tokens
            .peek()
            .is_none_or(|token| token.token_type != TokenType::Colon)
        {
            println!("Error: Expected ':' after field name '{}'", field_name);
            return None;
        }
        tokens.next();

        let value = parse_expression(tokens)?;
        fields.push((field_name, value));

        if matches!(
            tokens.peek().map(|token| &token.token_type),
            Some(TokenType::Comma)
        ) {
            tokens.next();
        } else {
            break;
        }
    }

    if tokens
        .peek()
        .is_none_or(|token| token.token_type != TokenType::Rbrace)
    {
        println!("Error: Expected '}}' to close struct literal");
        return None;
    }
    tokens.next();

    Some(fields)
}

pub fn parse_primary_expression<'a, T>(tokens: &mut Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let before = tokens.clone();
    let result = (|| {
        let token = (*tokens.peek()?).clone();

        let expr = match &token.token_type {
            TokenType::IntLiteral(s) => {
                tokens.next();
                Some(Expression::Literal(Literal::Int(s.clone())))
            }
            TokenType::Float(value) => {
                tokens.next();
                Some(Expression::Literal(Literal::Float(*value)))
            }
            TokenType::CharLiteral(c) => {
                tokens.next();
                Some(Expression::Literal(Literal::Char(*c)))
            }
            TokenType::BoolLiteral(b) => {
                tokens.next();
                Some(Expression::Literal(Literal::Bool(*b)))
            }
            TokenType::Null => {
                tokens.next();
                Some(Expression::Null)
            }
            TokenType::Identifier(name) => {
                let mut name = name.clone();
                tokens.next();

                while matches!(
                    tokens.peek().map(|token| &token.token_type),
                    Some(TokenType::DoubleColon)
                ) {
                    tokens.next();
                    let segment = match tokens.next() {
                        Some(Token {
                            token_type: TokenType::Identifier(segment),
                            ..
                        }) => segment,
                        _ => {
                            println!("Error: Expected identifier after '::'");
                            return None;
                        }
                    };
                    name.push_str("::");
                    name.push_str(segment);
                }

                let expr = if let Some(peeked_token) = tokens.peek() {
                    match &peeked_token.token_type {
                        TokenType::Lchevr if peek_is_generic_call(tokens) => {
                            tokens.next(); // consume '<'
                            let inner = collect_generic_inner(tokens)?;
                            let arg_strs = split_top_level_generic_args(&inner)?;

                            let mut type_args = Vec::with_capacity(arg_strs.len());
                            for arg in arg_strs {
                                let tt = parse_type(&arg)?;
                                let wt = token_type_to_wave_type(&tt)?;
                                type_args.push(wt);
                            }

                            skip_ws(tokens);
                            if tokens
                                .peek()
                                .map_or(true, |t| t.token_type != TokenType::Lparen)
                            {
                                println!(
                                    "Error: Expected '(' after generic function type arguments"
                                );
                                return None;
                            }
                            tokens.next(); // consume '('

                            let mut args = vec![];
                            if tokens
                                .peek()
                                .map_or(false, |t| t.token_type != TokenType::Rparen)
                            {
                                loop {
                                    let arg = parse_expression(tokens)?;
                                    args.push(arg);

                                    if let Some(Token {
                                        token_type: TokenType::Comma,
                                        ..
                                    }) = tokens.peek()
                                    {
                                        tokens.next();
                                    } else {
                                        break;
                                    }
                                }
                            }

                            if tokens
                                .peek()
                                .map_or(true, |t| t.token_type != TokenType::Rparen)
                            {
                                println!("Error: Expected ')' after function call arguments");
                                return None;
                            }
                            tokens.next();

                            Expression::FunctionCall {
                                name,
                                type_args,
                                args,
                            }
                        }
                        TokenType::Lchevr if peek_is_generic_struct_literal(tokens) => {
                            tokens.next(); // consume '<'
                            let inner = collect_generic_inner(tokens)?;
                            let arg_strs = split_top_level_generic_args(&inner)?;

                            // Validate the application here so malformed type arguments
                            // fail as syntax instead of becoming an opaque struct name.
                            for arg in &arg_strs {
                                let token_type = parse_type(arg)?;
                                token_type_to_wave_type(&token_type)?;
                            }

                            skip_ws(tokens);
                            if tokens
                                .peek()
                                .is_none_or(|token| token.token_type != TokenType::Lbrace)
                            {
                                println!(
                                    "Error: Expected '{{' after generic struct type arguments"
                                );
                                return None;
                            }
                            tokens.next(); // consume '{'

                            name.push('<');
                            name.push_str(&arg_strs.join(","));
                            name.push('>');

                            Expression::StructLiteral {
                                name,
                                fields: parse_struct_literal_fields(tokens)?,
                            }
                        }
                        TokenType::Lparen => {
                            tokens.next();

                            let mut args = vec![];
                            if tokens
                                .peek()
                                .map_or(false, |t| t.token_type != TokenType::Rparen)
                            {
                                loop {
                                    let arg = parse_expression(tokens)?;
                                    args.push(arg);

                                    if let Some(Token {
                                        token_type: TokenType::Comma,
                                        ..
                                    }) = tokens.peek()
                                    {
                                        tokens.next();
                                    } else {
                                        break;
                                    }
                                }
                            }

                            if tokens
                                .peek()
                                .map_or(true, |t| t.token_type != TokenType::Rparen)
                            {
                                println!("Error: Expected ')' after function call arguments");
                                return None;
                            }
                            tokens.next();

                            Expression::FunctionCall {
                                name,
                                type_args: Vec::new(),
                                args,
                            }
                        }
                        TokenType::Lbrace => {
                            tokens.next();
                            Expression::StructLiteral {
                                name,
                                fields: parse_struct_literal_fields(tokens)?,
                            }
                        }
                        _ => Expression::Variable(name),
                    }
                } else {
                    Expression::Variable(name)
                };

                Some(expr)
            }
            TokenType::Lparen => {
                tokens.next();
                let inner_expr = parse_expression(tokens)?;
                if tokens
                    .peek()
                    .map_or(true, |t| t.token_type != TokenType::Rparen)
                {
                    println!("Error: Expected ')' to close grouped expression");
                    return None;
                }
                tokens.next();
                Some(Expression::Grouped(Box::new(inner_expr)))
            }
            TokenType::String(value) => {
                tokens.next();
                Some(Expression::Literal(Literal::String(value.clone())))
            }
            TokenType::Lbrack => {
                tokens.next();
                let mut elements = vec![];
                if tokens
                    .peek()
                    .map_or(false, |t| t.token_type != TokenType::Rbrack)
                {
                    loop {
                        elements.push(parse_expression(tokens)?);
                        if let Some(Token {
                            token_type: TokenType::Comma,
                            ..
                        }) = tokens.peek()
                        {
                            tokens.next();
                        } else {
                            break;
                        }
                    }
                }
                if tokens
                    .peek()
                    .map_or(true, |t| t.token_type != TokenType::Rbrack)
                {
                    println!("Error: Expected ']' to close array literal");
                    return None;
                }
                tokens.next();
                Some(Expression::ArrayLiteral(elements))
            }
            TokenType::Asm => {
                tokens.next();
                if tokens.peek()?.token_type != TokenType::Lbrace {
                    println!("Expected '{{' after 'asm'");
                    return None;
                }
                tokens.next();

                let mut instructions: Vec<String> = vec![];
                let mut inputs: Vec<(String, Expression)> = vec![];
                let mut outputs: Vec<(String, Expression)> = vec![];
                let mut clobbers: Vec<String> = vec![];

                let mut closed = false;
                while let Some(token) = tokens.peek() {
                    match &token.token_type {
                        TokenType::Rbrace => {
                            tokens.next();
                            closed = true;
                            break;
                        }

                        TokenType::In => {
                            tokens.next();
                            parse_asm_inout_clause(tokens, true, &mut inputs, &mut outputs)?;
                        }

                        TokenType::Out => {
                            tokens.next();
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

                        TokenType::String(s) => {
                            instructions.push(s.clone());
                            tokens.next();
                        }

                        TokenType::SemiColon | TokenType::Comma => {
                            tokens.next();
                        }
                        other => {
                            println!("Unexpected token in asm expression: {:?}", other);
                            return None;
                        }
                    }
                }

                if !closed {
                    return None;
                }
                Some(Expression::AsmBlock {
                    instructions,
                    inputs,
                    outputs,
                    clobbers,
                })
            }
            _ => match token.token_type {
                TokenType::Continue
                | TokenType::Break
                | TokenType::Return
                | TokenType::SemiColon => None,
                _ => {
                    println!(
                        "Error: Expected primary expression, found {:?}",
                        token.token_type
                    );
                    println!(
                        "Error: Expected primary expression, found {:?}",
                        token.lexeme
                    );
                    println!("Error: Expected primary expression, found {:?}", token.line);
                    None
                }
            },
        };

        let base = expr?.with_span(lexer::consumed_span(before.clone(), tokens));

        parse_postfix_expression(tokens, base)
    })();
    result.map(|value: Expression| value.with_span(lexer::consumed_span(before, tokens)))
}
