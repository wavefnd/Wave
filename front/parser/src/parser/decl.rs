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

//! Variable, external function, type-alias, and enum declarations.
//!
//! Declaration parsing records source types and ABI annotations without making
//! backend layout decisions. Nested generic text is consumed with delimiter
//! depth so inner commas and closing chevrons cannot terminate the outer type.

use crate::ast::{
    ASTNode, EnumNode, EnumVariantNode, Expression, ExternFunctionNode, Literal, Mutability,
    TypeAliasNode, VariableNode, VariantCaseNode, VariantNode, Visibility, WaveType,
};
use crate::expr::parse_expression;
use crate::parser::functions::parse_generic_param_names;
use crate::parser::ParseError;
use crate::types::parse_type_from_stream;
use lexer::token::TokenType;
use lexer::Token;
use std::iter::Peekable;
use std::slice::Iter;

pub fn collect_generic_inner<'a, T>(tokens: &mut Peekable<T>) -> Option<String>
where
    T: Iterator<Item = &'a Token>,
{
    let mut inner = String::new();
    let mut depth: i32 = 1;

    while let Some(t) = tokens.next() {
        match &t.token_type {
            TokenType::Lchevr => {
                depth += 1;
                inner.push('<');
                continue;
            },
            TokenType::Rchevr => {
                depth -= 1;
                if depth == 0 {
                    return Some(inner);
                }
                inner.push('>');
                continue;
            },
            _ => {},
        }

        let text: &str = if !t.lexeme.is_empty() {
            t.lexeme.as_str()
        } else if let TokenType::Identifier(name) = &t.token_type {
            name.as_str()
        } else {
            ""
        };

        for ch in text.chars() {
            match ch {
                '<' => {
                    depth += 1;
                    inner.push('<');
                },
                '>' => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(inner);
                    }
                    inner.push('>');
                },
                _ => inner.push(ch),
            }
        }
    }

    None
}

pub fn parse_const_decl(tokens: &mut Peekable<Iter<'_, Token>>) -> Result<ASTNode, ParseError> {
    let anchor = tokens.peek().copied();
    let invalid = |token| ParseError::expected_at(token, anchor, "valid const decl", "const decl");
    let mutability = Mutability::Const;
    skip_ws(tokens);
    let name = match tokens.next() {
        Some(Token { token_type: TokenType::Identifier(name), .. }) => name.clone(),
        _ => {
            println!("Expected identifier after `const`");
            return Err(invalid(tokens.peek().copied()));
        },
    };

    skip_ws(tokens);
    if !matches!(tokens.next().map(|t| &t.token_type), Some(TokenType::Colon)) {
        println!("Expected ':' after identifier");
        return Err(invalid(tokens.peek().copied()));
    }

    let wave_type = match parse_type_from_stream(tokens) {
        Some(wave_type) => wave_type,
        None => {
            println!("Expected a valid type after ':'");
            return Err(invalid(tokens.peek().copied()));
        },
    };

    skip_ws(tokens);
    let initial_value = if let Some(Token { token_type: TokenType::Equal, .. }) = tokens.peek() {
        tokens.next(); // consume '='
        let expr = parse_expression(tokens)?;
        Some(expr)
    } else {
        None
    };

    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
        println!("Expected ';' after expression");
        return Err(invalid(tokens.peek().copied()));
    }
    tokens.next();

    if let (WaveType::Array(_, expected_len), Some(Expression::ArrayLiteral(elements))) =
        (&wave_type, &initial_value.as_ref().map(Expression::unspanned))
    {
        if *expected_len != elements.len() as u32 {
            println!(
                "❌ Error: Array length mismatch. Expected {}, but got {} elements",
                expected_len,
                elements.len()
            );
            return Err(invalid(tokens.peek().copied()));
        }
    }

    Ok(ASTNode::Variable(VariableNode {
        name,
        type_name: wave_type,
        initial_value,
        mutability,
        visibility: Visibility::Private,
    }))
}

pub fn parse_const(tokens: &mut Peekable<Iter<'_, Token>>) -> Result<ASTNode, ParseError> {
    parse_const_decl(tokens)
}

pub fn parse_static(tokens: &mut Peekable<Iter<'_, Token>>) -> Result<ASTNode, ParseError> {
    let anchor = tokens.peek().copied();
    let invalid = |token| ParseError::expected_at(token, anchor, "valid static", "static");
    let node = parse_var(tokens)?;
    let ASTNode::Variable(mut v) = node else {
        return Err(invalid(tokens.peek().copied()));
    };
    v.mutability = Mutability::Static;
    Ok(ASTNode::Variable(v))
}

// VAR parsing
pub fn parse_var(tokens: &mut Peekable<Iter<'_, Token>>) -> Result<ASTNode, ParseError> {
    let anchor = tokens.peek().copied();
    let invalid = |token| ParseError::expected_at(token, anchor, "valid var", "var");
    let mutability = Mutability::Var;

    skip_ws(tokens);
    let name = match tokens.next() {
        Some(Token { token_type: TokenType::Identifier(name), .. }) => name.clone(),
        _ => {
            println!("Expected identifier");
            return Err(invalid(tokens.peek().copied()));
        },
    };

    skip_ws(tokens);
    if !matches!(tokens.next().map(|t| &t.token_type), Some(TokenType::Colon)) {
        println!(
            "Wave variable declarations require an explicit type: `var {}: Type = value;`",
            name
        );
        return Err(invalid(tokens.peek().copied()));
    }

    let wave_type = match parse_type_from_stream(tokens) {
        Some(wave_type) => wave_type,
        None => {
            println!("Expected a valid type after ':'");
            return Err(invalid(tokens.peek().copied()));
        },
    };

    skip_ws(tokens);
    let initial_value = if let Some(Token { token_type: TokenType::Equal, .. }) = tokens.peek() {
        tokens.next(); // consume '='
        let expr = parse_expression(tokens)?;
        Some(expr)
    } else {
        None
    };

    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
        println!("Expected ';' after expression");
        return Err(invalid(tokens.peek().copied()));
    }
    tokens.next();

    if let (WaveType::Array(_, expected_len), Some(Expression::ArrayLiteral(elements))) =
        (&wave_type, &initial_value.as_ref().map(Expression::unspanned))
    {
        if *expected_len != elements.len() as u32 {
            println!(
                "❌ Error: Array length mismatch. Expected {}, but got {} elements",
                expected_len,
                elements.len()
            );
            return Err(invalid(tokens.peek().copied()));
        }
    }

    Ok(ASTNode::Variable(VariableNode {
        name,
        type_name: wave_type,
        initial_value,
        mutability,
        visibility: Visibility::Private,
    }))
}

fn skip_ws(tokens: &mut Peekable<Iter<'_, Token>>) {
    while let Some(t) = tokens.peek() {
        match t.token_type {
            TokenType::Whitespace | TokenType::Newline => {
                tokens.next();
            },
            _ => break,
        }
    }
}

fn expect(tokens: &mut Peekable<Iter<'_, Token>>, ty: TokenType, msg: &str) -> bool {
    if tokens.peek().map(|t| t.token_type.clone()) == Some(ty) {
        tokens.next();
        true
    } else {
        println!("Error: {}", msg);
        false
    }
}

/// <keyword>(abi) ... / <keyword>(abi, "sym") ...
pub(super) fn parse_ffi_header(
    tokens: &mut Peekable<Iter<'_, Token>>,
    keyword: &str,
) -> Option<(String, Option<String>)> {
    skip_ws(tokens);

    if !expect(tokens, TokenType::Lparen, &format!("Expected '(' after '{}'", keyword)) {
        return None;
    }

    skip_ws(tokens);

    let abi = match tokens.next() {
        Some(Token { token_type: TokenType::Identifier(name), .. }) => name.clone(),
        other => {
            println!("Error: Expected ABI identifier in {}(...), found {:?}", keyword, other);
            return None;
        },
    };

    skip_ws(tokens);

    // optional: , "symbol"
    let mut global_symbol: Option<String> = None;
    if tokens.peek().map(|t| t.token_type.clone()) == Some(TokenType::Comma) {
        tokens.next(); // consume ','
        skip_ws(tokens);

        global_symbol = match tokens.next() {
            Some(Token { token_type: TokenType::String(s), .. }) => {
                Some(String::from_utf8(s.clone()).ok()?)
            },
            other => {
                println!(
                    "Error: Expected string literal after ',' in {}(...), found {:?}",
                    keyword, other
                );
                return None;
            },
        };

        skip_ws(tokens);
    }

    if !expect(tokens, TokenType::Rparen, &format!("Expected ')' to close {}(...)", keyword)) {
        return None;
    }

    Some((abi, global_symbol))
}

/// extern(abi) ... / extern(abi, "sym") ...
fn parse_extern_header(tokens: &mut Peekable<Iter<'_, Token>>) -> Option<(String, Option<String>)> {
    parse_ffi_header(tokens, "extern")
}

fn peek_non_ws_token_type(tokens: &Peekable<Iter<'_, Token>>) -> Option<TokenType> {
    let mut it = tokens.clone();
    while let Some(t) = it.peek() {
        match t.token_type {
            TokenType::Whitespace | TokenType::Newline => {
                it.next();
            },
            _ => return Some(t.token_type.clone()),
        }
    }
    None
}

fn parse_extern_fun_decl(
    tokens: &mut Peekable<Iter<'_, Token>>,
    abi: String,
    global_symbol: Option<&String>,
) -> Result<ExternFunctionNode, ParseError> {
    let anchor = tokens.peek().copied();
    let invalid = |token| {
        ParseError::expected_at(
            token,
            anchor,
            "valid extern function declaration",
            "extern function",
        )
    };
    skip_ws(tokens);

    // 'fun'
    match tokens.peek() {
        Some(Token { token_type: TokenType::Fun, .. }) => {
            tokens.next();
        },
        _ => {
            return Err(invalid(tokens.peek().copied()));
        },
    }

    skip_ws(tokens);

    // name
    let name = match tokens.next() {
        Some(Token { token_type: TokenType::Identifier(n), .. }) => n.clone(),
        _ => {
            return Err(invalid(tokens.peek().copied()));
        },
    };

    skip_ws(tokens);

    if !expect(tokens, TokenType::Lparen, "Expected '(' after extern function name") {
        return Err(invalid(tokens.peek().copied()));
    }

    // params
    let mut params: Vec<(String, WaveType)> = Vec::new();
    let mut idx: usize = 0;
    let mut variadic = false;

    loop {
        skip_ws(tokens);

        if tokens.peek().map(|t| t.token_type.clone()) == Some(TokenType::Rparen) {
            tokens.next(); // consume ')'
            break;
        }

        let mut lookahead = tokens.clone();
        let is_variadic = (0..3).all(|_| {
            matches!(lookahead.next().map(|token| &token.token_type), Some(TokenType::Dot))
        });
        if is_variadic {
            if params.is_empty() {
                return Err(invalid(tokens.peek().copied()));
            }
            for _ in 0..3 {
                tokens.next();
            }
            skip_ws(tokens);
            if !expect(
                tokens,
                TokenType::Rparen,
                "Expected ')' immediately after '...' in extern function",
            ) {
                return Err(invalid(tokens.peek().copied()));
            }
            variadic = true;
            break;
        }

        // named param? (Identifier ... :)
        let is_named = match tokens.peek() {
            Some(Token { token_type: TokenType::Identifier(_), .. }) => {
                let _next_ty = peek_non_ws_token_type(tokens);
                if let Some(TokenType::Identifier(_)) = tokens.peek().map(|t| t.token_type.clone())
                {
                    let mut la = tokens.clone();
                    la.next(); // consume identifier
                    while let Some(t) = la.peek() {
                        match t.token_type {
                            TokenType::Whitespace | TokenType::Newline => {
                                la.next();
                            },
                            _ => break,
                        }
                    }
                    la.peek().map(|t| t.token_type.clone()) == Some(TokenType::Colon)
                } else {
                    false
                }
            },
            _ => false,
        };

        if is_named {
            // name :
            let param_name =
                if let Some(Token { token_type: TokenType::Identifier(n), .. }) = tokens.next() {
                    n.clone()
                } else {
                    unreachable!();
                };

            skip_ws(tokens);

            if !expect(
                tokens,
                TokenType::Colon,
                "Expected ':' after parameter name in extern function",
            ) {
                return Err(invalid(tokens.peek().copied()));
            }

            skip_ws(tokens);

            let ty = crate::parser::types::parse_type_checked(tokens, "extern parameter type")?;

            params.push((param_name, ty));
        } else {
            // type-only param
            let ty = crate::parser::types::parse_type_checked(tokens, "extern parameter type")?;
            let param_name = format!("arg{}", idx);
            idx += 1;
            params.push((param_name, ty));
        }

        skip_ws(tokens);

        // ',' or ')'
        match tokens.peek().map(|t| t.token_type.clone()) {
            Some(TokenType::Comma) => {
                tokens.next();
                continue;
            },
            Some(TokenType::Rparen) => {
                tokens.next();
                break;
            },
            _ => {
                return Err(invalid(tokens.peek().copied()));
            },
        }
    }

    skip_ws(tokens);

    // return type: optional -> ty
    let return_type = match tokens.peek().map(|t| t.token_type.clone()) {
        Some(TokenType::Arrow) => {
            tokens.next(); // consume '->'
            skip_ws(tokens);

            crate::parser::types::parse_type_checked(tokens, "extern return type")?
        },
        _ => WaveType::Void,
    };

    skip_ws(tokens);

    // per-function symbol: optional string literal
    let mut symbol: Option<String> = None;
    if let Some(Token { token_type: TokenType::String(s), .. }) = tokens.peek() {
        let s = String::from_utf8(s.clone()).map_err(|_| {
            ParseError::expected_at(
                tokens.peek().copied(),
                anchor,
                "UTF-8 symbol name",
                "extern symbol",
            )
        })?;
        tokens.next();
        symbol = Some(s);
    }

    // apply global symbol if per-fn missing
    if symbol.is_none() {
        symbol = global_symbol.cloned();
    }

    skip_ws(tokens);

    if !expect(tokens, TokenType::SemiColon, "Expected ';' after extern function declaration") {
        return Err(invalid(tokens.peek().copied()));
    }

    Ok(ExternFunctionNode { name, abi, symbol, params, variadic, return_type })
}

pub fn parse_extern(tokens: &mut Peekable<Iter<'_, Token>>) -> Result<Vec<ASTNode>, ParseError> {
    let anchor = tokens.peek().copied();
    let invalid = |token| {
        ParseError::expected_at(token, anchor, "extern function or block", "extern declaration")
    };
    let (abi, global_symbol) =
        parse_extern_header(tokens).ok_or_else(|| invalid(tokens.peek().copied()))?;

    skip_ws(tokens);

    // block or single
    if tokens.peek().map(|t| t.token_type.clone()) == Some(TokenType::Lbrace) {
        tokens.next(); // consume '{'

        let mut nodes: Vec<ASTNode> = Vec::new();

        loop {
            skip_ws(tokens);

            match tokens.peek().map(|t| t.token_type.clone()) {
                Some(TokenType::Rbrace) => {
                    tokens.next(); // consume '}'
                    break;
                },
                Some(TokenType::Fun) => {
                    let ef = parse_extern_fun_decl(tokens, abi.clone(), global_symbol.as_ref())?;
                    nodes.push(ASTNode::ExternFunction(ef));
                },
                Some(TokenType::Whitespace) | Some(TokenType::Newline) => {
                    tokens.next();
                },
                _ => {
                    return Err(invalid(tokens.peek().copied()));
                },
            }
        }

        skip_ws(tokens);
        if tokens.peek().map(|t| t.token_type.clone()) == Some(TokenType::SemiColon) {
            tokens.next();
        }

        Ok(nodes)
    } else if tokens.peek().map(|t| t.token_type.clone()) == Some(TokenType::Fun) {
        let ef = parse_extern_fun_decl(tokens, abi, global_symbol.as_ref())?;
        Ok(vec![ASTNode::ExternFunction(ef)])
    } else {
        Err(invalid(tokens.peek().copied()))
    }
}

pub fn parse_type_alias(tokens: &mut Peekable<Iter<'_, Token>>) -> Result<ASTNode, ParseError> {
    let anchor = tokens.peek().copied();
    let context = "type alias";
    let name = crate::expr::identifier(tokens, anchor, context)?;
    crate::expr::expect_token(tokens, anchor, TokenType::Equal, "'='", context)?;
    let target = crate::types::parse_type_checked(tokens, context)?;
    crate::expr::expect_token(tokens, anchor, TokenType::SemiColon, "';'", context)?;
    Ok(ASTNode::TypeAlias(TypeAliasNode { name, target, visibility: Visibility::Private }))
}

pub fn parse_enum(tokens: &mut Peekable<Iter<'_, Token>>) -> Result<ASTNode, ParseError> {
    let anchor = tokens.peek().copied();
    let context = "enum declaration";
    let name = crate::expr::identifier(tokens, anchor, context)?;
    crate::expr::expect_token(tokens, anchor, TokenType::Arrow, "'->'", context)?;
    let repr_type = crate::types::parse_type_checked(tokens, "enum representation type")?;
    crate::expr::expect_token(tokens, anchor, TokenType::Lbrace, "'{'", context)?;
    let mut variants = vec![];
    while !tokens.peek().is_some_and(|t| t.token_type == TokenType::Rbrace) {
        let before = tokens.clone();
        let vname = crate::expr::identifier(tokens, anchor, "enum case")?;
        let explicit_value = if tokens.peek().is_some_and(|t| t.token_type == TokenType::Equal) {
            tokens.next();
            let at = tokens.peek().copied();
            let value = parse_expression(tokens)?;
            let Expression::Literal(Literal::Int(raw)) = value.unspanned() else {
                return Err(ParseError::expected_at(
                    at,
                    anchor,
                    "integer literal",
                    "enum case value",
                ));
            };
            Some(raw.clone())
        } else {
            None
        };
        variants.push(EnumVariantNode {
            span: lexer::consumed_span(before, tokens),
            name: vname,
            explicit_value,
        });
        match tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Comma) => {
                tokens.next();
            },
            Some(TokenType::Rbrace) => {},
            _ => {
                return Err(ParseError::expected_at(
                    tokens.peek().copied(),
                    anchor,
                    "',' or '}'",
                    context,
                ))
            },
        }
    }
    tokens.next();
    Ok(ASTNode::Enum(EnumNode { name, repr_type, variants, visibility: Visibility::Private }))
}

pub fn parse_variant(tokens: &mut Peekable<Iter<'_, Token>>) -> Result<ASTNode, ParseError> {
    skip_ws(tokens);
    let anchor = tokens.peek().copied();
    let context = "variant declaration";
    let name = crate::expr::identifier(tokens, anchor, context)?;
    let generic_params = parse_generic_param_names(tokens)?;
    skip_ws(tokens);
    crate::expr::expect_token(tokens, anchor, TokenType::Lbrace, "'{'", context)?;
    let mut cases = vec![];
    loop {
        skip_ws(tokens);
        if tokens.peek().is_some_and(|t| t.token_type == TokenType::Rbrace) {
            tokens.next();
            break;
        }
        let before = tokens.clone();
        let case_name = crate::expr::identifier(tokens, anchor, "variant case")?;
        skip_ws(tokens);
        let mut payload_types = vec![];
        if tokens.peek().is_some_and(|t| t.token_type == TokenType::Lparen) {
            tokens.next();
            loop {
                skip_ws(tokens);
                if tokens.peek().is_some_and(|t| t.token_type == TokenType::Rparen) {
                    tokens.next();
                    break;
                }
                payload_types
                    .push(crate::types::parse_type_checked(tokens, "variant payload type")?);
                skip_ws(tokens);
                if tokens.peek().is_some_and(|t| t.token_type == TokenType::Comma) {
                    tokens.next();
                } else {
                    crate::expr::expect_token(
                        tokens,
                        anchor,
                        TokenType::Rparen,
                        "',' or ')'",
                        "variant payload",
                    )?;
                    break;
                }
            }
        }
        cases.push(VariantCaseNode {
            span: lexer::consumed_span(before, tokens),
            name: case_name,
            payload_types,
        });
        skip_ws(tokens);
        match tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Comma) => {
                tokens.next();
            },
            Some(TokenType::Rbrace) => {},
            _ => {
                return Err(ParseError::expected_at(
                    tokens.peek().copied(),
                    anchor,
                    "',' or '}'",
                    context,
                ))
            },
        }
    }
    Ok(ASTNode::Variant(VariantNode {
        name,
        generic_params,
        cases,
        visibility: Visibility::Private,
    }))
}
