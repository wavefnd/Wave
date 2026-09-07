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

//! Top-level token dispatch for the Wave lexer.
//!
//! Trivia is consumed before every token. Multi-character operators must be
//! recognized before their one-character prefixes so the parser receives one
//! unambiguous token for each source operator.

use crate::token::*;
use crate::{Lexer, Token};
use error::{WaveError, WaveErrorKind};

impl<'a> Lexer<'a> {
    #[allow(clippy::never_loop)]
    /// Scans the next non-trivia token while preserving its source line.
    pub fn next_token(&mut self) -> Result<Token, WaveError> {
        self.skip_trivia()?;
        let (start, line, column) = (self.current, self.line, self.current_column());
        let mut token = self.scan_token()?;
        token.line = line;
        token.lexeme = self.source[start..self.current].to_string();
        token.span = Some(error::SourceSpan {
            file: self.file.clone(),
            start,
            end: self.current,
            line,
            column,
            end_line: self.line,
            end_column: self.current_column(),
            expansion: Vec::new(),
            focus: None,
        });
        Ok(token)
    }

    #[allow(clippy::never_loop)]
    fn scan_token(&mut self) -> Result<Token, WaveError> {
        loop {
            self.skip_trivia()?;

            if self.is_at_end() {
                return Ok(Token {
                    token_type: TokenType::Eof,
                    lexeme: String::new(),
                    line: self.line,
                    span: None,
                });
            }

            let c = self.advance();

            match c {
                '+' => {
                    if self.match_next('+') {
                        return Ok(Token {
                            token_type: TokenType::Increment,
                            lexeme: "++".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::PlusEq,
                            lexeme: "+=".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Plus,
                            lexeme: "+".to_string(),
                            line: self.line,
                            span: None,
                        });
                    }
                }
                '-' => {
                    if self.match_next('-') {
                        return Ok(Token {
                            token_type: TokenType::Decrement,
                            lexeme: "--".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else if self.match_next('>') {
                        return Ok(Token {
                            token_type: TokenType::Arrow,
                            lexeme: "->".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::MinusEq,
                            lexeme: "-=".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Minus,
                            lexeme: "-".to_string(),
                            line: self.line,
                            span: None,
                        });
                    }
                }
                '*' => {
                    if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::StarEq,
                            lexeme: "*=".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Star,
                            lexeme: "*".to_string(),
                            line: self.line,
                            span: None,
                        });
                    }
                }
                '.' => {
                    return Ok(Token {
                        token_type: TokenType::Dot,
                        lexeme: ".".to_string(),
                        line: self.line,
                        span: None,
                    })
                }
                '/' => {
                    if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::DivEq,
                            lexeme: "/=".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Div,
                            lexeme: "/".to_string(),
                            line: self.line,
                            span: None,
                        });
                    }
                }
                '%' => {
                    if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::RemainderEq,
                            lexeme: "%=".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Remainder,
                            lexeme: "%".to_string(),
                            line: self.line,
                            span: None,
                        });
                    }
                }
                ';' => {
                    return Ok(Token {
                        token_type: TokenType::SemiColon,
                        lexeme: ";".to_string(),
                        line: self.line,
                        span: None,
                    })
                }
                ':' => {
                    if self.match_next(':') {
                        return Ok(Token {
                            token_type: TokenType::DoubleColon,
                            lexeme: "::".to_string(),
                            line: self.line,
                            span: None,
                        });
                    }
                    return Ok(Token {
                        token_type: TokenType::Colon,
                        lexeme: ":".to_string(),
                        line: self.line,
                        span: None,
                    });
                }
                '<' => {
                    if self.match_next('<') {
                        return Ok(Token {
                            token_type: TokenType::Rol,
                            lexeme: "<<".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::LchevrEq,
                            lexeme: "<=".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Lchevr,
                            lexeme: "<".to_string(),
                            line: self.line,
                            span: None,
                        });
                    }
                }
                '>' => {
                    if self.match_next('>') {
                        return Ok(Token {
                            token_type: TokenType::Ror,
                            lexeme: ">>".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::RchevrEq,
                            lexeme: ">=".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Rchevr,
                            lexeme: ">".to_string(),
                            line: self.line,
                            span: None,
                        });
                    }
                }
                '(' => {
                    return Ok(Token {
                        token_type: TokenType::Lparen,
                        lexeme: "(".to_string(),
                        line: self.line,
                        span: None,
                    })
                }
                ')' => {
                    return Ok(Token {
                        token_type: TokenType::Rparen,
                        lexeme: ")".to_string(),
                        line: self.line,
                        span: None,
                    })
                }
                '{' => {
                    return Ok(Token {
                        token_type: TokenType::Lbrace,
                        lexeme: "{".to_string(),
                        line: self.line,
                        span: None,
                    })
                }
                '}' => {
                    return Ok(Token {
                        token_type: TokenType::Rbrace,
                        lexeme: "}".to_string(),
                        line: self.line,
                        span: None,
                    })
                }
                '[' => {
                    return Ok(Token {
                        token_type: TokenType::Lbrack,
                        lexeme: "[".to_string(),
                        line: self.line,
                        span: None,
                    })
                }
                ']' => {
                    return Ok(Token {
                        token_type: TokenType::Rbrack,
                        lexeme: "]".to_string(),
                        line: self.line,
                        span: None,
                    })
                }
                '=' => {
                    if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::EqualTwo,
                            lexeme: "==".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Equal,
                            lexeme: "=".to_string(),
                            line: self.line,
                            span: None,
                        });
                    }
                }
                '&' => {
                    if self.match_next('&') {
                        return Ok(Token {
                            token_type: TokenType::LogicalAnd,
                            lexeme: "&&".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::AddressOf,
                            lexeme: "&".to_string(),
                            line: self.line,
                            span: None,
                        });
                    }
                }
                '|' => {
                    if self.match_next('|') {
                        return Ok(Token {
                            token_type: TokenType::LogicalOr,
                            lexeme: "||".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::BitwiseOr,
                            lexeme: "|".to_string(),
                            line: self.line,
                            span: None,
                        });
                    }
                }
                '!' => {
                    if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::NotEqual,
                            lexeme: "!=".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else if self.match_next('&') {
                        return Ok(Token {
                            token_type: TokenType::Nand,
                            lexeme: "!&".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else if self.match_next('|') {
                        return Ok(Token {
                            token_type: TokenType::Nor,
                            lexeme: "!|".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Not,
                            lexeme: "!".to_string(),
                            line: self.line,
                            span: None,
                        });
                    }
                }
                '^' => {
                    return Ok(Token {
                        token_type: TokenType::Xor,
                        lexeme: "^".to_string(),
                        line: self.line,
                        span: None,
                    })
                }
                '~' => {
                    if self.match_next('^') {
                        return Ok(Token {
                            token_type: TokenType::Xnor,
                            lexeme: "~^".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::BitwiseNot,
                            lexeme: "~".to_string(),
                            line: self.line,
                            span: None,
                        });
                    }
                }
                '?' => {
                    if self.match_next('?') {
                        return Ok(Token {
                            token_type: TokenType::NullCoalesce,
                            lexeme: "??".to_string(),
                            line: self.line,
                            span: None,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Condition,
                            lexeme: "?".to_string(),
                            line: self.line,
                            span: None,
                        });
                    }
                }
                ',' => {
                    return Ok(Token {
                        token_type: TokenType::Comma,
                        lexeme: ",".to_string(),
                        line: self.line,
                        span: None,
                    })
                }
                '\'' => {
                    let value = self.char_literal()?;
                    return Ok(Token {
                        token_type: TokenType::CharLiteral(value),
                        lexeme: format!("'{}'", value),
                        line: self.line,
                        span: None,
                    });
                }
                '"' => {
                    let string_value = self.string()?;
                    return Ok(Token {
                        token_type: TokenType::String(string_value.clone()),
                        lexeme: format!("\"{}\"", string_value),
                        line: self.line,
                        span: None,
                    });
                }

                ch if ch.is_alphabetic() || ch == '_' => {
                    let ident = self.identifier(c);
                    return Ok(self.keyword_or_ident_token(ident));
                }

                '0'..='9' => {
                    let start = self.current - 1;
                    let radix =
                        c == '0' && matches!(self.peek(), 'x' | 'X' | 'o' | 'O' | 'b' | 'B');
                    while self.peek().is_ascii_alphanumeric()
                        || self.peek() == '_'
                        || (!radix && self.peek() == '.')
                        || (!radix
                            && matches!(self.peek(), '+' | '-')
                            && self.source[..self.current].ends_with(['e', 'E']))
                    {
                        self.advance();
                    }
                    let raw = &self.source[start..self.current];
                    let token_type = if crate::number::IntegerLiteral::parse(raw).is_some() {
                        Some(TokenType::IntLiteral(raw.replace('_', "")))
                    } else {
                        crate::number::parse_float(raw).map(TokenType::Float)
                    };
                    return token_type.map(|token_type| Token {
                        token_type, lexeme: raw.to_string(), line: self.line, span: None,
                    }).ok_or_else(|| self.make_error(
                        WaveErrorKind::InvalidNumber(raw.to_string()),
                        format!("invalid numeric literal `{raw}`"), self.line, self.column_at(start),
                    ).with_code("E1006").with_help("use binary, octal, decimal or hexadecimal digits; separate digits with a single underscore; floats use decimal fractions or exponents, without suffixes"));
                }

                _ => {
                    if c == '\0' {
                        return Err(self
                            .make_error_here(
                                WaveErrorKind::UnexpectedChar(c),
                                "null character (`\\0`) is not allowed in source",
                            )
                            .with_code("E1001")
                            .with_label("unexpected null byte in source")
                            .with_help(
                                "remove the null byte and save the file as plain UTF-8 text",
                            ));
                    } else if c == '\\' {
                        return Err(self
                            .make_error_here(
                                WaveErrorKind::UnexpectedChar(c),
                                "unexpected backslash outside of string literal",
                            )
                            .with_code("E1001")
                            .with_label("`\\` is only valid inside string/char literals")
                            .with_help("if you intended a string, wrap it with quotes"));
                    } else {
                        return Err(self
                            .make_error_here(
                                WaveErrorKind::UnexpectedChar(c),
                                format!("unexpected character `{}` (U+{:04X})", c, c as u32),
                            )
                            .with_code("E1001")
                            .with_label("this character is not valid in Wave syntax")
                            .with_help("remove it or replace it with a valid token"));
                    }
                }
            }
        }
    }
}
