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

use crate::token::*;
use crate::{Lexer, Token};
use error::{WaveError, WaveErrorKind};

impl<'a> Lexer<'a> {
    #[allow(clippy::never_loop)]
    pub fn next_token(&mut self) -> Result<Token, WaveError> {
        loop {
            self.skip_trivia()?;

            if self.is_at_end() {
                return Ok(Token {
                    token_type: TokenType::Eof,
                    lexeme: String::new(),
                    line: self.line,
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
                        });
                    } else if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::PlusEq,
                            lexeme: "+=".to_string(),
                            line: self.line,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Plus,
                            lexeme: "+".to_string(),
                            line: self.line,
                        });
                    }
                }
                '-' => {
                    if self.match_next('-') {
                        return Ok(Token {
                            token_type: TokenType::Decrement,
                            lexeme: "--".to_string(),
                            line: self.line,
                        });
                    } else if self.match_next('>') {
                        return Ok(Token {
                            token_type: TokenType::Arrow,
                            lexeme: "->".to_string(),
                            line: self.line,
                        });
                    } else if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::MinusEq,
                            lexeme: "-=".to_string(),
                            line: self.line,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Minus,
                            lexeme: "-".to_string(),
                            line: self.line,
                        });
                    }
                }
                '*' => {
                    if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::StarEq,
                            lexeme: "*=".to_string(),
                            line: self.line,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Star,
                            lexeme: "*".to_string(),
                            line: self.line,
                        });
                    }
                }
                '.' => {
                    return Ok(Token {
                        token_type: TokenType::Dot,
                        lexeme: ".".to_string(),
                        line: self.line,
                    })
                }
                '/' => {
                    if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::DivEq,
                            lexeme: "/=".to_string(),
                            line: self.line,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Div,
                            lexeme: "/".to_string(),
                            line: self.line,
                        });
                    }
                }
                '%' => {
                    if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::RemainderEq,
                            lexeme: "%=".to_string(),
                            line: self.line,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Remainder,
                            lexeme: "%".to_string(),
                            line: self.line,
                        });
                    }
                }
                ';' => {
                    return Ok(Token {
                        token_type: TokenType::SemiColon,
                        lexeme: ";".to_string(),
                        line: self.line,
                    })
                }
                ':' => {
                    return Ok(Token {
                        token_type: TokenType::Colon,
                        lexeme: ":".to_string(),
                        line: self.line,
                    })
                }
                '<' => {
                    if self.match_next('<') {
                        return Ok(Token {
                            token_type: TokenType::Rol,
                            lexeme: "<<".to_string(),
                            line: self.line,
                        });
                    } else if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::LchevrEq,
                            lexeme: "<=".to_string(),
                            line: self.line,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Lchevr,
                            lexeme: "<".to_string(),
                            line: self.line,
                        });
                    }
                }
                '>' => {
                    if self.match_next('>') {
                        return Ok(Token {
                            token_type: TokenType::Ror,
                            lexeme: ">>".to_string(),
                            line: self.line,
                        });
                    } else if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::RchevrEq,
                            lexeme: ">=".to_string(),
                            line: self.line,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Rchevr,
                            lexeme: ">".to_string(),
                            line: self.line,
                        });
                    }
                }
                '(' => {
                    return Ok(Token {
                        token_type: TokenType::Lparen,
                        lexeme: "(".to_string(),
                        line: self.line,
                    })
                }
                ')' => {
                    return Ok(Token {
                        token_type: TokenType::Rparen,
                        lexeme: ")".to_string(),
                        line: self.line,
                    })
                }
                '{' => {
                    return Ok(Token {
                        token_type: TokenType::Lbrace,
                        lexeme: "{".to_string(),
                        line: self.line,
                    })
                }
                '}' => {
                    return Ok(Token {
                        token_type: TokenType::Rbrace,
                        lexeme: "}".to_string(),
                        line: self.line,
                    })
                }
                '[' => {
                    return Ok(Token {
                        token_type: TokenType::Lbrack,
                        lexeme: "[".to_string(),
                        line: self.line,
                    })
                }
                ']' => {
                    return Ok(Token {
                        token_type: TokenType::Rbrack,
                        lexeme: "]".to_string(),
                        line: self.line,
                    })
                }
                '=' => {
                    if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::EqualTwo,
                            lexeme: "==".to_string(),
                            line: self.line,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Equal,
                            lexeme: "=".to_string(),
                            line: self.line,
                        });
                    }
                }
                '&' => {
                    if self.match_next('&') {
                        return Ok(Token {
                            token_type: TokenType::LogicalAnd,
                            lexeme: "&&".to_string(),
                            line: self.line,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::AddressOf,
                            lexeme: "&".to_string(),
                            line: self.line,
                        });
                    }
                }
                '|' => {
                    if self.match_next('|') {
                        return Ok(Token {
                            token_type: TokenType::LogicalOr,
                            lexeme: "||".to_string(),
                            line: self.line,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::BitwiseOr,
                            lexeme: "|".to_string(),
                            line: self.line,
                        });
                    }
                }
                '!' => {
                    if self.match_next('=') {
                        return Ok(Token {
                            token_type: TokenType::NotEqual,
                            lexeme: "!=".to_string(),
                            line: self.line,
                        });
                    } else if self.match_next('&') {
                        return Ok(Token {
                            token_type: TokenType::Nand,
                            lexeme: "!&".to_string(),
                            line: self.line,
                        });
                    } else if self.match_next('|') {
                        return Ok(Token {
                            token_type: TokenType::Nor,
                            lexeme: "!|".to_string(),
                            line: self.line,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Not,
                            lexeme: "!".to_string(),
                            line: self.line,
                        });
                    }
                }
                '^' => {
                    return Ok(Token {
                        token_type: TokenType::Xor,
                        lexeme: "^".to_string(),
                        line: self.line,
                    })
                }
                '~' => {
                    if self.match_next('^') {
                        return Ok(Token {
                            token_type: TokenType::Xnor,
                            lexeme: "~^".to_string(),
                            line: self.line,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::BitwiseNot,
                            lexeme: "~".to_string(),
                            line: self.line,
                        });
                    }
                }
                '?' => {
                    if self.match_next('?') {
                        return Ok(Token {
                            token_type: TokenType::NullCoalesce,
                            lexeme: "??".to_string(),
                            line: self.line,
                        });
                    } else {
                        return Ok(Token {
                            token_type: TokenType::Condition,
                            lexeme: "?".to_string(),
                            line: self.line,
                        });
                    }
                }
                ',' => {
                    return Ok(Token {
                        token_type: TokenType::Comma,
                        lexeme: ",".to_string(),
                        line: self.line,
                    })
                }
                '\'' => {
                    let value = self.char_literal()?;
                    return Ok(Token {
                        token_type: TokenType::CharLiteral(value),
                        lexeme: format!("'{}'", value),
                        line: self.line,
                    });
                }
                '"' => {
                    let string_value = self.string()?;
                    return Ok(Token {
                        token_type: TokenType::String(string_value.clone()),
                        lexeme: format!("\"{}\"", string_value),
                        line: self.line,
                    });
                }

                'a'..='z' | 'A'..='Z' | '_' => {
                    let ident = self.identifier();
                    return Ok(self.keyword_or_ident_token(ident));
                }

                '0'..='9' => {
                    if c == '0' && (self.peek() == 'b' || self.peek() == 'B') {
                        self.advance(); // consume 'b' or 'B'

                        let mut bin_str = String::new();
                        while self.peek() == '0' || self.peek() == '1' {
                            bin_str.push(self.advance());
                        }

                        if bin_str.is_empty() {
                            return Err(
                                self.make_error_here(
                                    WaveErrorKind::InvalidNumber("0b".to_string()),
                                    "invalid binary literal: expected at least one binary digit after `0b`",
                                )
                                .with_code("E1006")
                                .with_label("missing binary digits")
                                .with_help("example: `0b1011`"),
                            );
                        }

                        return Ok(Token {
                            token_type: TokenType::IntLiteral(format!("0b{}", bin_str)),
                            lexeme: format!("0b{}", bin_str),
                            line: self.line,
                        });
                    }

                    if c == '0' && (self.peek() == 'x' || self.peek() == 'X') {
                        self.advance(); // consume 'x' or 'X'

                        let mut hex_str = String::new();
                        while self.peek().is_ascii_hexdigit() {
                            hex_str.push(self.advance());
                        }

                        if hex_str.is_empty() {
                            return Err(
                                self.make_error_here(
                                    WaveErrorKind::InvalidNumber("0x".to_string()),
                                    "invalid hexadecimal literal: expected at least one hex digit after `0x`",
                                )
                                .with_code("E1006")
                                .with_label("missing hexadecimal digits")
                                .with_help("example: `0x1FF`"),
                            );
                        }

                        return Ok(Token {
                            token_type: TokenType::IntLiteral(format!("0x{}", hex_str)),
                            lexeme: format!("0x{}", hex_str),
                            line: self.line,
                        });
                    }

                    let mut num_str = c.to_string();
                    while self.peek().is_ascii_digit() {
                        num_str.push(self.advance());
                    }

                    let is_float = if self.peek() == '.' {
                        num_str.push('.');
                        self.advance();
                        while self.peek().is_ascii_digit() {
                            num_str.push(self.advance());
                        }
                        true
                    } else {
                        false
                    };

                    let token_type = if is_float {
                        match num_str.parse::<f64>() {
                            Ok(v) => TokenType::Float(v),
                            Err(_) => {
                                return Err(self
                                    .make_error_here(
                                        WaveErrorKind::InvalidNumber(num_str.clone()),
                                        format!("invalid floating-point literal `{}`", num_str),
                                    )
                                    .with_code("E1006")
                                    .with_label("cannot parse float literal")
                                    .with_help("check decimal point placement and digits"));
                            }
                        }
                    } else {
                        TokenType::IntLiteral(num_str.clone())
                    };

                    return Ok(Token {
                        token_type,
                        lexeme: num_str,
                        line: self.line,
                    });
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
