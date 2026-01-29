use crate::token::*;
use super::{Lexer, Token};

impl<'a> Lexer<'a> {
    pub(crate) fn identifier(&mut self) -> String {
        let start = if self.current > 0 { self.current - 1 } else { 0 };

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
            },
            "var" => Token {
                token_type: TokenType::Var,
                lexeme: "var".to_string(),
                line: self.line,
            },
            "deref" => Token {
                token_type: TokenType::Deref,
                lexeme: "deref".to_string(),
                line: self.line,
            },
            "let" => Token {
                token_type: TokenType::Let,
                lexeme: "let".to_string(),
                line: self.line,
            },
            "mut" => Token {
                token_type: TokenType::Mut,
                lexeme: "mut".to_string(),
                line: self.line,
            },
            "const" => Token {
                token_type: TokenType::Const,
                lexeme: "const".to_string(),
                line: self.line,
            },
            "if" => Token {
                token_type: TokenType::If,
                lexeme: "if".to_string(),
                line: self.line,
            },
            "else" => Token {
                token_type: TokenType::Else,
                lexeme: "else".to_string(),
                line: self.line,
            },
            "proto" => Token {
                token_type: TokenType::Proto,
                lexeme: "proto".to_string(),
                line: self.line,
            },
            "struct" => Token {
                token_type: TokenType::Struct,
                lexeme: "struct".to_string(),
                line: self.line,
            },
            "while" => Token {
                token_type: TokenType::While,
                lexeme: "while".to_string(),
                line: self.line,
            },
            "for" => Token {
                token_type: TokenType::For,
                lexeme: "for".to_string(),
                line: self.line,
            },
            "module" => Token {
                token_type: TokenType::Module,
                lexeme: "module".to_string(),
                line: self.line,
            },
            "class" => Token {
                token_type: TokenType::Class,
                lexeme: "class".to_string(),
                line: self.line,
            },
            "in" => Token {
                token_type: TokenType::In,
                lexeme: "in".to_string(),
                line: self.line,
            },
            "out" => Token {
                token_type: TokenType::Out,
                lexeme: "out".to_string(),
                line: self.line,
            },
            "clobber" => Token {
                token_type: TokenType::Clobber,
                lexeme: "clobber".to_string(),
                line: self.line,
            },
            "is" => Token {
                token_type: TokenType::Is,
                lexeme: "is".to_string(),
                line: self.line,
            },
            "asm" => Token {
                token_type: TokenType::Asm,
                lexeme: "asm".to_string(),
                line: self.line,
            },
            "xnand" => Token {
                token_type: TokenType::Xnand,
                lexeme: "xnand".to_string(),
                line: self.line,
            },
            "import" => Token {
                token_type: TokenType::Import,
                lexeme: "import".to_string(),
                line: self.line,
            },
            "return" => Token {
                token_type: TokenType::Return,
                lexeme: "return".to_string(),
                line: self.line,
            },
            "continue" => Token {
                token_type: TokenType::Continue,
                lexeme: "continue".to_string(),
                line: self.line,
            },
            "print" => Token {
                token_type: TokenType::Print,
                lexeme: "print".to_string(),
                line: self.line,
            },
            "input" => Token {
                token_type: TokenType::Input,
                lexeme: "input".to_string(),
                line: self.line,
            },
            "println" => Token {
                token_type: TokenType::Println,
                lexeme: "println".to_string(),
                line: self.line,
            },
            "match" => Token {
                token_type: TokenType::Match,
                lexeme: "match".to_string(),
                line: self.line,
            },
            "char" => Token {
                token_type: TokenType::TypeChar,
                lexeme: "char".to_string(),
                line: self.line,
            },
            "byte" => Token {
                token_type: TokenType::TypeByte,
                lexeme: "byte".to_string(),
                line: self.line,
            },
            "ptr" => Token {
                token_type: TokenType::Identifier("ptr".to_string()),
                lexeme: "ptr".to_string(),
                line: self.line,
            },
            "array" => Token {
                token_type: TokenType::Identifier("array".to_string()),
                lexeme: "array".to_string(),
                line: self.line,
            },
            "isz" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::ISZ),
                lexeme: "isz".to_string(),
                line: self.line,
            },
            "i8" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::I8),
                lexeme: "i8".to_string(),
                line: self.line,
            },
            "i16" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::I16),
                lexeme: "i16".to_string(),
                line: self.line,
            },
            "i32" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::I32),
                lexeme: "i32".to_string(),
                line: self.line,
            },
            "i64" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::I64),
                lexeme: "i64".to_string(),
                line: self.line,
            },
            "i128" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::I128),
                lexeme: "i128".to_string(),
                line: self.line,
            },
            "i256" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::I256),
                lexeme: "i256".to_string(),
                line: self.line,
            },
            "i512" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::I512),
                lexeme: "i512".to_string(),
                line: self.line,
            },
            "i1024" => Token {
                token_type: TokenType::TokenTypeInt(IntegerType::I1024),
                lexeme: "i1024".to_string(),
                line: self.line,
            },
            "usz" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::USZ),
                lexeme: "usz".to_string(),
                line: self.line,
            },
            "u8" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U8),
                lexeme: "u8".to_string(),
                line: self.line,
            },
            "u16" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U16),
                lexeme: "u16".to_string(),
                line: self.line,
            },
            "u32" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U32),
                lexeme: "u32".to_string(),
                line: self.line,
            },
            "u64" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U64),
                lexeme: "u64".to_string(),
                line: self.line,
            },
            "u128" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U128),
                lexeme: "u128".to_string(),
                line: self.line,
            },
            "u256" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U256),
                lexeme: "u256".to_string(),
                line: self.line,
            },
            "u512" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U512),
                lexeme: "u512".to_string(),
                line: self.line,
            },
            "u1024" => Token {
                token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U1024),
                lexeme: "u1024".to_string(),
                line: self.line,
            },
            "f32" => Token {
                token_type: TokenType::TokenTypeFloat(FloatType::F32),
                lexeme: "f32".to_string(),
                line: self.line,
            },
            "f64" => Token {
                token_type: TokenType::TokenTypeFloat(FloatType::F64),
                lexeme: "f64".to_string(),
                line: self.line,
            },
            "str" => Token {
                token_type: TokenType::TypeString,
                lexeme: "str".to_string(),
                line: self.line,
            },
            "break" => Token {
                token_type: TokenType::Break,
                lexeme: "break".to_string(),
                line: self.line,
            },
            "true" => Token {
                token_type: TokenType::BoolLiteral(true),
                lexeme: "true".to_string(),
                line: self.line,
            },
            "false" => Token {
                token_type: TokenType::BoolLiteral(false),
                lexeme: "false".to_string(),
                line: self.line,
            },
            _ => Token {
                token_type: TokenType::Identifier(ident.clone()),
                lexeme: ident,
                line: self.line,
            },
        }
    }
}
