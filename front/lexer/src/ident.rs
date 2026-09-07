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

//! Identifier scanning and keyword classification.
//!
//! The lexer first consumes the complete identifier spelling, then maps exact
//! language keywords to dedicated tokens. Context-sensitive names such as
//! `ptr` and `array` remain identifiers for the parser's type grammar.

use crate::token::*;
use crate::{Lexer, Token};

impl<'a> Lexer<'a> {
    pub(crate) fn identifier(&mut self, first: char) -> String {
        let start = self.current - first.len_utf8();

        while !self.is_at_end() {
            let c = self.peek();
            if c.is_alphabetic() || c.is_numeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        self.source[start..self.current].to_string()
    }

    pub(crate) fn keyword_or_ident_token(&self, ident: String) -> Token {
        match ident.as_str() {
            "fun" => Token {
                token_type: TokenType::Fun,
                lexeme: "fun".to_string(),
                line: self.line,
                span: None,
            },
            "extern" => Token {
                token_type: TokenType::Extern,
                lexeme: "extern".to_string(),
                line: self.line,
                span: None,
            },
            "export" => Token {
                token_type: TokenType::Export,
                lexeme: "export".to_string(),
                line: self.line,
                span: None,
            },
            "pub" => Token {
                token_type: TokenType::Pub,
                lexeme: "pub".to_string(),
                line: self.line,
                span: None,
            },
            "type" => Token {
                token_type: TokenType::Type,
                lexeme: "type".to_string(),
                line: self.line,
                span: None,
            },
            "enum" => Token {
                token_type: TokenType::Enum,
                lexeme: "enum".to_string(),
                line: self.line,
                span: None,
            },
            "variant" => Token {
                token_type: TokenType::Variant,
                lexeme: "variant".to_string(),
                line: self.line,
                span: None,
            },
            "static" => Token {
                token_type: TokenType::Static,
                lexeme: "static".to_string(),
                line: self.line,
                span: None,
            },
            "var" => Token {
                token_type: TokenType::Var,
                lexeme: "var".to_string(),
                line: self.line,
                span: None,
            },
            "deref" => Token {
                token_type: TokenType::Deref,
                lexeme: "deref".to_string(),
                line: self.line,
                span: None,
            },
            "let" => Token {
                token_type: TokenType::Let,
                lexeme: "let".to_string(),
                line: self.line,
                span: None,
            },
            "mut" => Token {
                token_type: TokenType::Mut,
                lexeme: "mut".to_string(),
                line: self.line,
                span: None,
            },
            "const" => Token {
                token_type: TokenType::Const,
                lexeme: "const".to_string(),
                line: self.line,
                span: None,
            },
            "if" => Token {
                token_type: TokenType::If,
                lexeme: "if".to_string(),
                line: self.line,
                span: None,
            },
            "else" => Token {
                token_type: TokenType::Else,
                lexeme: "else".to_string(),
                line: self.line,
                span: None,
            },
            "proto" => Token {
                token_type: TokenType::Proto,
                lexeme: "proto".to_string(),
                line: self.line,
                span: None,
            },
            "struct" => Token {
                token_type: TokenType::Struct,
                lexeme: "struct".to_string(),
                line: self.line,
                span: None,
            },
            "while" => Token {
                token_type: TokenType::While,
                lexeme: "while".to_string(),
                line: self.line,
                span: None,
            },
            "for" => Token {
                token_type: TokenType::For,
                lexeme: "for".to_string(),
                line: self.line,
                span: None,
            },
            "module" => Token {
                token_type: TokenType::Module,
                lexeme: "module".to_string(),
                line: self.line,
                span: None,
            },
            "class" => Token {
                token_type: TokenType::Class,
                lexeme: "class".to_string(),
                line: self.line,
                span: None,
            },
            "in" => Token {
                token_type: TokenType::In,
                lexeme: "in".to_string(),
                line: self.line,
                span: None,
            },
            "out" => Token {
                token_type: TokenType::Out,
                lexeme: "out".to_string(),
                line: self.line,
                span: None,
            },
            "clobber" => Token {
                token_type: TokenType::Clobber,
                lexeme: "clobber".to_string(),
                line: self.line,
                span: None,
            },
            "is" => Token {
                token_type: TokenType::Is,
                lexeme: "is".to_string(),
                line: self.line,
                span: None,
            },
            "as" => Token {
                token_type: TokenType::As,
                lexeme: "as".to_string(),
                line: self.line,
                span: None,
            },
            "asm" => Token {
                token_type: TokenType::Asm,
                lexeme: "asm".to_string(),
                line: self.line,
                span: None,
            },
            "xnand" => Token {
                token_type: TokenType::Xnand,
                lexeme: "xnand".to_string(),
                line: self.line,
                span: None,
            },
            "import" => Token {
                token_type: TokenType::Import,
                lexeme: "import".to_string(),
                line: self.line,
                span: None,
            },
            "return" => Token {
                token_type: TokenType::Return,
                lexeme: "return".to_string(),
                line: self.line,
                span: None,
            },
            "continue" => Token {
                token_type: TokenType::Continue,
                lexeme: "continue".to_string(),
                line: self.line,
                span: None,
            },
            "print" => Token {
                token_type: TokenType::Print,
                lexeme: "print".to_string(),
                line: self.line,
                span: None,
            },
            "input" => Token {
                token_type: TokenType::Input,
                lexeme: "input".to_string(),
                line: self.line,
                span: None,
            },
            "println" => Token {
                token_type: TokenType::Println,
                lexeme: "println".to_string(),
                line: self.line,
                span: None,
            },
            "match" => Token {
                token_type: TokenType::Match,
                lexeme: "match".to_string(),
                line: self.line,
                span: None,
            },
            "char" => Token {
                token_type: TokenType::TypeChar,
                lexeme: "char".to_string(),
                line: self.line,
                span: None,
            },
            "byte" => Token {
                token_type: TokenType::TypeByte,
                lexeme: "byte".to_string(),
                line: self.line,
                span: None,
            },
            "ptr" => Token {
                token_type: TokenType::Identifier("ptr".to_string()),
                lexeme: "ptr".to_string(),
                line: self.line,
                span: None,
            },
            "array" => Token {
                token_type: TokenType::Identifier("array".to_string()),
                lexeme: "array".to_string(),
                line: self.line,
                span: None,
            },
            "isz" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::ISZ),
                lexeme: "isz".to_string(),
                line: self.line,
                span: None,
            },
            "i8" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::I8),
                lexeme: "i8".to_string(),
                line: self.line,
                span: None,
            },
            "i16" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::I16),
                lexeme: "i16".to_string(),
                line: self.line,
                span: None,
            },
            "i32" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::I32),
                lexeme: "i32".to_string(),
                line: self.line,
                span: None,
            },
            "i64" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::I64),
                lexeme: "i64".to_string(),
                line: self.line,
                span: None,
            },
            "i128" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::I128),
                lexeme: "i128".to_string(),
                line: self.line,
                span: None,
            },
            "i256" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::I256),
                lexeme: "i256".to_string(),
                line: self.line,
                span: None,
            },
            "i512" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::I512),
                lexeme: "i512".to_string(),
                line: self.line,
                span: None,
            },
            "i1024" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::I1024),
                lexeme: "i1024".to_string(),
                line: self.line,
                span: None,
            },
            "usz" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::USZ),
                lexeme: "usz".to_string(),
                line: self.line,
                span: None,
            },
            "u8" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U8),
                lexeme: "u8".to_string(),
                line: self.line,
                span: None,
            },
            "u16" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U16),
                lexeme: "u16".to_string(),
                line: self.line,
                span: None,
            },
            "u32" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U32),
                lexeme: "u32".to_string(),
                line: self.line,
                span: None,
            },
            "u64" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U64),
                lexeme: "u64".to_string(),
                line: self.line,
                span: None,
            },
            "u128" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U128),
                lexeme: "u128".to_string(),
                line: self.line,
                span: None,
            },
            "u256" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U256),
                lexeme: "u256".to_string(),
                line: self.line,
                span: None,
            },
            "u512" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U512),
                lexeme: "u512".to_string(),
                line: self.line,
                span: None,
            },
            "u1024" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U1024),
                lexeme: "u1024".to_string(),
                line: self.line,
                span: None,
            },
            "f32" => Token {
                token_type: TokenType::TokenTypeFloat(FloatType::F32),
                lexeme: "f32".to_string(),
                line: self.line,
                span: None,
            },
            "f64" => Token {
                token_type: TokenType::TokenTypeFloat(FloatType::F64),
                lexeme: "f64".to_string(),
                line: self.line,
                span: None,
            },
            "str" => Token {
                token_type: TokenType::TypeString,
                lexeme: "str".to_string(),
                line: self.line,
                span: None,
            },
            "break" => Token {
                token_type: TokenType::Break,
                lexeme: "break".to_string(),
                line: self.line,
                span: None,
            },
            "true" => Token {
                token_type: TokenType::BoolLiteral(true),
                lexeme: "true".to_string(),
                line: self.line,
                span: None,
            },
            "false" => Token {
                token_type: TokenType::BoolLiteral(false),
                lexeme: "false".to_string(),
                line: self.line,
                span: None,
            },
            "null" => Token {
                token_type: TokenType::Null,
                lexeme: "null".to_string(),
                line: self.line,
                span: None,
            },
            _ => Token {
                token_type: TokenType::Identifier(ident.clone()),
                lexeme: ident,
                line: self.line,
                span: None,
            },
        }
    }
}
