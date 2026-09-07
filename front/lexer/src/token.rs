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

//! Token vocabulary produced by the lexer and consumed by the parser.
//!
//! Built-in type spellings have dedicated variants, while user-defined types
//! remain `TypeCustom`. Preserve that distinction when adding keywords because
//! type parsing and context-sensitive identifier handling depend on it.

use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum IntegerType {
    I8,
    I16,
    I32,
    I64,
    I128,
    I256,
    I512,
    I1024,
    ISZ,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UnsignedIntegerType {
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    U512,
    U1024,
    USZ,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum FloatType {
    F32,
    F64,
}

impl fmt::Display for IntegerType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            IntegerType::I8 => "i8",
            IntegerType::I16 => "i16",
            IntegerType::I32 => "i32",
            IntegerType::I64 => "i64",
            IntegerType::I128 => "i128",
            IntegerType::I256 => "i256",
            IntegerType::I512 => "i512",
            IntegerType::I1024 => "i1024",
            IntegerType::ISZ => "isz",
        };
        write!(f, "{}", name)
    }
}

impl fmt::Display for UnsignedIntegerType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            UnsignedIntegerType::U8 => "u8",
            UnsignedIntegerType::U16 => "u16",
            UnsignedIntegerType::U32 => "u32",
            UnsignedIntegerType::U64 => "u64",
            UnsignedIntegerType::U128 => "u128",
            UnsignedIntegerType::U256 => "u256",
            UnsignedIntegerType::U512 => "u512",
            UnsignedIntegerType::U1024 => "u1024",
            UnsignedIntegerType::USZ => "usz",
        };
        write!(f, "{}", name)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Fun,
    Extern,
    Export,
    Pub,
    Type,
    Enum,
    Variant,
    Static,
    Var,
    Let,
    Mut,
    Deref,
    Const,
    If,
    Else,
    While,
    For,
    Import,
    Return,
    Continue,
    Input,
    Print,
    Println,
    Module,
    Class,
    Match,
    LogicalAnd,   // &&
    AddressOf,    // &
    LogicalOr,    // ||
    BitwiseOr,    // |
    NotEqual,     // !=
    Xor,          // ^
    Xnor,         // ~^
    BitwiseNot,   // ~
    Nand,         // !&
    Nor,          // !|
    Not,          // !
    Condition,    // ?
    NullCoalesce, // ??
    Conditional,  // ?:
    In,           // in
    Out,          // out
    Is,           // is
    As,           // as
    Asm,
    Rol,
    Ror,
    Xnand,
    Operator(String),
    TokenTypeInt(IntegerType),
    TokenTypeUint(UnsignedIntegerType),
    TokenTypeFloat(FloatType),
    TypeInt(u16),
    TypeUint(u16),
    TypeFloat(u16),
    TypeBool,
    TypeChar,
    TypeByte,
    TypeString,
    TypeCustom(String),
    TypePointer(Box<TokenType>),
    TypeArray(Box<TokenType>, u32),
    Identifier(String),
    String(String),
    IntLiteral(String),
    Float(f64),
    Plus,        // +
    Increment,   // ++
    PlusEq,      // +=
    Minus,       // -
    Decrement,   // --
    MinusEq,     // -=
    Star,        // *
    StarEq,      // *=
    Div,         // /
    DivEq,       // /=
    Remainder,   // %
    RemainderEq, // %=
    Equal,       // =
    EqualTwo,    // ==
    Comma,       // ,
    Dot,         // .
    SemiColon,   // ;
    Colon,       // :
    DoubleColon, // ::
    Lchevr,      // <
    LchevrEq,    // <=
    Rchevr,      // >
    RchevrEq,    // >=
    Lparen,      // (
    Rparen,      // )
    Lbrace,      // {
    Rbrace,      // }
    Lbrack,      // [
    Rbrack,      // ]
    Eof,         // End of file
    Error,
    Whitespace,
    Break,
    Arrow, // ->
    Array,
    Newline,
    Proto,
    Struct,
    TypeVoid,
    CharLiteral(char),
    BoolLiteral(bool),
    Null,
    Clobber,
}

impl TokenType {
    /// These reserved spellings are not implemented.
    pub fn reserved_spelling(&self) -> Option<&'static str> {
        match self {
            Self::Module => Some("module"),
            Self::Class => Some("class"),
            Self::Is => Some("is"),
            Self::Xnand => Some("xnand"),
            Self::Xnor => Some("~^"),
            Self::Nand => Some("!&"),
            Self::Nor => Some("!|"),
            Self::Condition => Some("?"),
            Self::NullCoalesce => Some("??"),
            Self::Conditional => Some("?:"),
            _ => None,
        }
    }
}
