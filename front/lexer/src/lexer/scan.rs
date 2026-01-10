use crate::token::*;
use super::{Lexer, Token};

impl<'a> Lexer<'a> {
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        if self.is_at_end() {
            return Token { token_type: TokenType::Eof, lexeme: String::new(), line: self.line };
        }

        let c = self.advance();

        match c {
            '+' => {
                if self.match_next('+') {
                    Token {
                        token_type: TokenType::Increment,
                        lexeme: "++".to_string(),
                        line: self.line,
                    }
                } else if self.match_next('=') {
                    Token {
                        token_type: TokenType::PlusEq,
                        lexeme: "+=".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Plus,
                        lexeme: "+".to_string(),
                        line: self.line,
                    }
                }
            }
            '-' => {
                if self.match_next('-') {
                    Token {
                        token_type: TokenType::Decrement,
                        lexeme: "--".to_string(),
                        line: self.line,
                    }
                } else if self.match_next('>') {
                    Token {
                        token_type: TokenType::Arrow,
                        lexeme: "->".to_string(),
                        line: self.line,
                    }
                } else if self.match_next('=') {
                    Token {
                        token_type: TokenType::MinusEq,
                        lexeme: "-=".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Minus,
                        lexeme: "-".to_string(),
                        line: self.line,
                    }
                }
            }
            '*' => {
                if self.match_next('=') {
                    Token {
                        token_type: TokenType::StarEq,
                        lexeme: "*=".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Star,
                        lexeme: "*".to_string(),
                        line: self.line,
                    }
                }
            }
            '.' => Token {
                token_type: TokenType::Dot,
                lexeme: ".".to_string(),
                line: self.line,
            },
            '/' => {
                if self.match_next('/') {
                    self.skip_comment();
                    self.next_token()
                } else if self.match_next('*') {
                    self.skip_multiline_comment();
                    self.next_token()
                } else if self.match_next('=') {
                    Token {
                        token_type: TokenType::DivEq,
                        lexeme: "/=".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Div,
                        lexeme: "/".to_string(),
                        line: self.line,
                    }
                }
            }
            '%' => {
                if self.match_next('=') {
                    Token {
                        token_type: TokenType::RemainderEq,
                        lexeme: "%=".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Remainder,
                        lexeme: "%".to_string(),
                        line: self.line,
                    }
                }
            }
            ';' => Token {
                token_type: TokenType::SemiColon,
                lexeme: ";".to_string(),
                line: self.line,
            },
            ':' => Token {
                token_type: TokenType::Colon,
                lexeme: ":".to_string(),
                line: self.line,
            },
            '<' => {
                if self.match_next('<') {
                    Token {
                        token_type: TokenType::Rol,
                        lexeme: "<<".to_string(),
                        line: self.line,
                    }
                } else if self.match_next('=') {
                    Token {
                        token_type: TokenType::LchevrEq,
                        lexeme: "<=".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Lchevr,
                        lexeme: "<".to_string(),
                        line: self.line,
                    }
                }
            }
            '>' => {
                if self.match_next('>') {
                    Token {
                        token_type: TokenType::Ror,
                        lexeme: ">>".to_string(),
                        line: self.line,
                    }
                } else if self.match_next('=') {
                    Token {
                        token_type: TokenType::RchevrEq,
                        lexeme: ">=".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Rchevr,
                        lexeme: ">".to_string(),
                        line: self.line,
                    }
                }
            }
            '(' => Token {
                token_type: TokenType::Lparen,
                lexeme: "(".to_string(),
                line: self.line,
            },
            ')' => Token {
                token_type: TokenType::Rparen,
                lexeme: ")".to_string(),
                line: self.line,
            },
            '{' => Token {
                token_type: TokenType::Lbrace,
                lexeme: "{".to_string(),
                line: self.line,
            },
            '}' => Token {
                token_type: TokenType::Rbrace,
                lexeme: "}".to_string(),
                line: self.line,
            },
            '[' => Token {
                token_type: TokenType::Lbrack,
                lexeme: "[".to_string(),
                line: self.line,
            },
            ']' => Token {
                token_type: TokenType::Rbrack,
                lexeme: "]".to_string(),
                line: self.line,
            },
            '=' => {
                if self.match_next('=') {
                    Token {
                        token_type: TokenType::EqualTwo,
                        lexeme: "==".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Equal,
                        lexeme: "=".to_string(),
                        line: self.line,
                    }
                }
            }
            '&' => {
                if self.match_next('&') {
                    Token {
                        token_type: TokenType::LogicalAnd,
                        lexeme: "&&".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::AddressOf,
                        lexeme: "&".to_string(),
                        line: self.line,
                    }
                }
            }
            '|' => {
                if self.match_next('|') {
                    Token {
                        token_type: TokenType::LogicalOr,
                        lexeme: "||".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::BitwiseOr,
                        lexeme: "|".to_string(),
                        line: self.line,
                    }
                }
            }
            '!' => {
                if self.match_next('=') {
                    Token {
                        token_type: TokenType::NotEqual,
                        lexeme: "!=".to_string(),
                        line: self.line,
                    }
                } else if self.match_next('&') {
                    Token {
                        token_type: TokenType::Nand,
                        lexeme: "!&".to_string(),
                        line: self.line,
                    }
                } else if self.match_next('|') {
                    Token {
                        token_type: TokenType::Nor,
                        lexeme: "!|".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Not,
                        lexeme: "!".to_string(),
                        line: self.line,
                    }
                }
            }
            '^' => Token {
                token_type: TokenType::Xor,
                lexeme: "^".to_string(),
                line: self.line,
            },
            '~' => {
                if self.match_next('^') {
                    Token {
                        token_type: TokenType::Xnor,
                        lexeme: "~^".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::BitwiseNot,
                        lexeme: "~".to_string(),
                        line: self.line,
                    }
                }
            }
            '?' => {
                if self.match_next('?') {
                    Token {
                        token_type: TokenType::NullCoalesce,
                        lexeme: "??".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Condition,
                        lexeme: "?".to_string(),
                        line: self.line,
                    }
                }
            }
            ',' => Token {
                token_type: TokenType::Comma,
                lexeme: ",".to_string(),
                line: self.line,
            },
            '\'' => {
                let value = self.char_literal();
                Token {
                    token_type: TokenType::CharLiteral(value),
                    lexeme: format!("'{}'", value),
                    line: self.line,
                }
            },
            '"' => {
                let string_value = self.string();
                Token {
                    token_type: TokenType::String(string_value.clone()),
                    lexeme: format!("\"{}\"", string_value),
                    line: self.line,
                }
            }

            'a'..='z' | 'A'..='Z' | '_' => {
                let ident = self.identifier();
                self.keyword_or_ident_token(ident)
            }

            '0'..='9' => {
                if c == '0' && (self.peek() == 'b' || self.peek() == 'B') {
                    self.advance(); // consume 'b' or 'B'

                    let mut bin_str = String::new();
                    while self.peek() == '0' || self.peek() == '1' {
                        bin_str.push(self.advance());
                    }

                    let value = i64::from_str_radix(&bin_str, 2).unwrap_or(0);

                    return Token {
                        token_type: TokenType::IntLiteral(format!("0b{}", bin_str)),
                        lexeme: format!("0b{}", bin_str),
                        line: self.line,
                    };
                }

                if c == '0' && (self.peek() == 'x' || self.peek() == 'X') {
                    self.advance(); // consume 'x' or 'X'

                    let mut hex_str = String::new();
                    while self.peek().is_ascii_hexdigit() {
                        hex_str.push(self.advance());
                    }

                    let value = i64::from_str_radix(&hex_str, 16).unwrap_or(0);

                    return Token {
                        token_type: TokenType::IntLiteral(format!("0x{}", hex_str)),
                        lexeme: format!("0x{}", hex_str),
                        line: self.line,
                    };
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
                    num_str.parse::<f64>().map(TokenType::Float).unwrap()
                } else {
                    TokenType::IntLiteral(num_str.clone())
                };

                Token {
                    token_type,
                    lexeme: num_str,
                    line: self.line,
                }
            }

            _ => {
                if c == '\0' {
                    eprintln!("[eprintln] Null character encountered — likely unintended");
                    panic!("[panic] Null character (`\\0`) is not allowed in source");
                } else if c == '\\' {
                    eprintln!("[eprintln] Unexpected backslash outside of string");
                    panic!("[panic] Unexpected character: '\\' outside of string");
                } else {
                    eprintln!(
                        "[eprintln] Unexpected character: {:?} (code: {})",
                        c, c as u32
                    );
                    panic!("[panic] Unexpected character: {:?}", c);
                }
            }
        }
    }
}
