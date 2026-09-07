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

//! Wave type grammar used by declarations and explicit type arguments.
//!
//! Nested pointer, array, and generic forms are parsed structurally so commas
//! and closing chevrons are interpreted at the correct nesting depth.

use crate::ast::WaveType;
use crate::decl::collect_generic_inner;
use lexer::token::*;
use lexer::Token;
use std::iter::Peekable;

pub fn split_top_level_generic_args(inner: &str) -> Option<Vec<String>> {
    let mut parts: Vec<String> = Vec::new();
    let mut depth: i32 = 0;
    let mut start: usize = 0;

    for (i, c) in inner.char_indices() {
        match c {
            '<' => depth += 1,
            '>' => {
                depth -= 1;
                if depth < 0 {
                    return None;
                }
            }
            ',' if depth == 0 => {
                let part = inner[start..i].trim();
                if part.is_empty() {
                    return None;
                }
                parts.push(part.to_string());
                start = i + 1;
            }
            _ => {}
        }
    }

    if depth != 0 {
        return None;
    }

    let tail = inner[start..].trim();
    if tail.is_empty() {
        return None;
    }
    parts.push(tail.to_string());

    Some(parts)
}

pub fn token_type_to_wave_type(token_type: &TokenType) -> Option<WaveType> {
    match token_type {
        TokenType::TypeVoid => Some(WaveType::Void),
        TokenType::Not => Some(WaveType::Never),
        TokenType::TypeInt(bits) => Some(WaveType::Int(*bits)),
        TokenType::TokenTypeInt(int_type) => match int_type {
            IntegerType::I8 => Some(WaveType::Int(8)),
            IntegerType::I16 => Some(WaveType::Int(16)),
            IntegerType::I32 => Some(WaveType::Int(32)),
            IntegerType::I64 => Some(WaveType::Int(64)),
            IntegerType::I128 => Some(WaveType::Int(128)),
            IntegerType::I256 => Some(WaveType::Int(256)),
            IntegerType::I512 => Some(WaveType::Int(512)),
            IntegerType::I1024 => Some(WaveType::Int(1024)),
            IntegerType::ISZ => Some(WaveType::Isz),
        },
        TokenType::TypeUint(bits) => Some(WaveType::Uint(*bits)),
        TokenType::TokenTypeUint(uint_type) => match uint_type {
            UnsignedIntegerType::U8 => Some(WaveType::Uint(8)),
            UnsignedIntegerType::U16 => Some(WaveType::Uint(16)),
            UnsignedIntegerType::U32 => Some(WaveType::Uint(32)),
            UnsignedIntegerType::U64 => Some(WaveType::Uint(64)),
            UnsignedIntegerType::U128 => Some(WaveType::Uint(128)),
            UnsignedIntegerType::U256 => Some(WaveType::Uint(256)),
            UnsignedIntegerType::U512 => Some(WaveType::Uint(512)),
            UnsignedIntegerType::U1024 => Some(WaveType::Uint(1024)),
            UnsignedIntegerType::USZ => Some(WaveType::Usz),
        },
        TokenType::TokenTypeFloat(float_type) => match float_type {
            FloatType::F32 => Some(WaveType::Float(32)),
            FloatType::F64 => Some(WaveType::Float(64)),
        },
        TokenType::TypeFloat(bits) => Some(WaveType::Float(*bits)),
        TokenType::TypeBool => Some(WaveType::Bool),
        TokenType::TypeChar => Some(WaveType::Char),
        TokenType::TypeByte => Some(WaveType::Byte),
        TokenType::TypeString => Some(WaveType::String),
        TokenType::TypePointer(inner) => {
            token_type_to_wave_type(inner).map(|t| WaveType::Pointer(Box::new(t)))
        }
        TokenType::TypeArray(inner, size) => {
            token_type_to_wave_type(inner).map(|t| WaveType::Array(Box::new(t), *size))
        }
        TokenType::TypeCustom(name) => Some(WaveType::Struct(name.clone())),
        _ => None,
    }
}

pub fn is_expression_start(token_type: &TokenType) -> bool {
    matches!(
        token_type,
        TokenType::Identifier(_)
            | TokenType::IntLiteral(_)
            | TokenType::Float(_)
            | TokenType::Lparen
            | TokenType::String(_)
            | TokenType::Lbrack
            | TokenType::Asm
            | TokenType::Deref
            | TokenType::Null
            | TokenType::CharLiteral(_)
            | TokenType::BoolLiteral(_)
            | TokenType::Plus
            | TokenType::Minus
            | TokenType::Not
            | TokenType::BitwiseNot
            | TokenType::AddressOf
            | TokenType::Increment
            | TokenType::Decrement
    )
}

pub fn parse_type(type_str: &str) -> Option<TokenType> {
    let type_str = type_str.trim();

    if type_str == "!" {
        return Some(TokenType::Not);
    }
    if type_str == "void" {
        return Some(TokenType::TypeVoid);
    }

    if let Some(lt_index) = type_str.find('<') {
        if !type_str.ends_with('>') {
            return None;
        }

        let base = type_str[..lt_index].trim();
        if base.is_empty() {
            return None;
        }
        let inner = &type_str[lt_index + 1..type_str.len() - 1];

        if base == "array" {
            let args = split_top_level_generic_args(inner)?;
            if args.len() != 2 {
                return None;
            }
            let elem_type_str = args[0].trim();
            let size_str = args[1].trim();

            let elem_type = parse_type(elem_type_str)?;
            let size =
                u32::try_from(lexer::number::IntegerLiteral::parse(size_str)?.to_i128()?).ok()?;

            return Some(TokenType::TypeArray(Box::new(elem_type), size));
        }

        if base == "ptr" {
            let inner_type = parse_type(inner)?;
            return Some(TokenType::TypePointer(Box::new(inner_type)));
        }

        let args = split_top_level_generic_args(inner)?;
        for arg in &args {
            let _ = parse_type(arg)?;
        }
        return Some(TokenType::TypeCustom(type_str.to_string()));
    }

    match type_str {
        "isz" => return Some(TokenType::TokenTypeInt(IntegerType::ISZ)),
        "usz" => return Some(TokenType::TokenTypeUint(UnsignedIntegerType::USZ)),
        "bool" => return Some(TokenType::TypeBool),
        "char" => return Some(TokenType::TypeChar),
        "byte" => return Some(TokenType::TypeByte),
        "str" => return Some(TokenType::TypeString),
        _ => {}
    }
    if let Some(prefix @ ('i' | 'u' | 'f')) = type_str.chars().next() {
        let suffix = &type_str[1..];
        if !suffix.is_empty() && suffix.bytes().all(|ch| ch.is_ascii_digit()) {
            let bits = suffix.parse::<u16>().ok()?;
            if suffix != bits.to_string() {
                return None;
            }
            return match prefix {
                'i' if matches!(bits, 8 | 16 | 32 | 64 | 128 | 256 | 512 | 1024) => {
                    Some(TokenType::TypeInt(bits))
                }
                'u' if matches!(bits, 8 | 16 | 32 | 64 | 128 | 256 | 512 | 1024) => {
                    Some(TokenType::TypeUint(bits))
                }
                'f' if matches!(bits, 32 | 64) => Some(TokenType::TypeFloat(bits)),
                _ => None,
            };
        }
    }

    if type_str.split("::").all(|segment| {
        !segment.is_empty()
            && segment
                .chars()
                .next()
                .is_some_and(|c| c.is_alphabetic() || c == '_')
            && segment.chars().all(|c| c.is_alphanumeric() || c == '_')
    }) {
        return Some(TokenType::TypeCustom(type_str.to_string()));
    }

    None
}

pub fn validate_type(expected: &TokenType, actual: &TokenType) -> bool {
    match (expected, actual) {
        (TokenType::TypeInt(_), TokenType::TypeInt(_)) => true,
        (TokenType::TypeUint(_), TokenType::TypeUint(_)) => true,
        (TokenType::TypeFloat(_), TokenType::TypeFloat(_)) => true,
        (TokenType::TypeBool, TokenType::TypeBool) => true,
        (TokenType::TypeChar, TokenType::TypeChar) => true,
        (TokenType::TypeByte, TokenType::TypeByte) => true,
        (TokenType::TypePointer(inner1), TokenType::TypePointer(inner2)) => {
            validate_type(&**inner1, &**inner2) // Double dereference to get TokenType
        }
        (TokenType::TypeArray(inner1, size1), TokenType::TypeArray(inner2, size2)) => {
            validate_type(&**inner1, &**inner2) && size1 == size2 // Double dereference to get TokenType
        }
        (TokenType::TypeString, TokenType::TypeString) => true,
        _ => false,
    }
}

pub fn parse_type_from_token(token_opt: Option<&&Token>) -> Option<WaveType> {
    let token = token_opt?;

    match &token.token_type {
        ty @ TokenType::TypeInt(_)
        | ty @ TokenType::TypeUint(_)
        | ty @ TokenType::TypeFloat(_)
        | ty @ TokenType::TypeBool
        | ty @ TokenType::TypeChar
        | ty @ TokenType::TypeByte
        | ty @ TokenType::TypeString
        | ty @ TokenType::TypePointer(_)
        | ty @ TokenType::TypeArray(_, _)
        | ty @ TokenType::TokenTypeInt(_)
        | ty @ TokenType::TokenTypeUint(_)
        | ty @ TokenType::TokenTypeFloat(_) => token_type_to_wave_type(ty),

        TokenType::Identifier(name) => token_type_to_wave_type(&parse_type(name)?),

        _ => None,
    }
}

pub fn parse_type_from_stream<'a, T>(tokens: &mut Peekable<T>) -> Option<WaveType>
where
    T: Iterator<Item = &'a Token>,
{
    while matches!(
        tokens.peek().map(|t| &t.token_type),
        Some(TokenType::Whitespace)
    ) {
        tokens.next();
    }

    let type_token = tokens.next()?;

    if let TokenType::Identifier(name) = &type_token.token_type {
        let mut name = name.clone();
        while matches!(
            tokens.peek().map(|token| &token.token_type),
            Some(TokenType::DoubleColon)
        ) {
            tokens.next();
            match tokens.next().map(|token| &token.token_type) {
                Some(TokenType::Identifier(segment)) => {
                    name.push_str("::");
                    name.push_str(segment);
                }
                _ => return None,
            }
        }

        while matches!(
            tokens.peek().map(|t| &t.token_type),
            Some(TokenType::Whitespace | TokenType::Newline)
        ) {
            tokens.next();
        }

        if matches!(
            tokens.peek().map(|t| &t.token_type),
            Some(TokenType::Lchevr)
        ) {
            tokens.next(); // consume '<'

            let inner = collect_generic_inner(tokens)?;
            let full_type_str = format!("{}<{}>", name, inner);

            let parsed_tt = parse_type(&full_type_str)?;
            return token_type_to_wave_type(&parsed_tt);
        }

        let parsed_tt = parse_type(&name)?;
        return token_type_to_wave_type(&parsed_tt);
    }

    token_type_to_wave_type(&type_token.token_type)
}
