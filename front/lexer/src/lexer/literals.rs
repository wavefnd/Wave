use super::Lexer;

impl<'a> Lexer<'a> {
    pub(crate) fn string(&mut self) -> String {
        let mut string_literal = String::new();
        while !self.is_at_end() && self.peek() != '"' {
            if self.peek() == '\n' {
                panic!("Unterminated string (newline in string literal).");
            }

            let c = self.advance();

            if c == '\\' {
                let next = self.advance();
                match next {
                    'n' => string_literal.push('\n'),
                    't' => string_literal.push('\t'),
                    'r' => string_literal.push('\r'),
                    '\\' => string_literal.push('\\'),
                    '"' => string_literal.push('"'),
                    'x' => {
                        let h1 = self.advance();
                        let h2 = self.advance();

                        let hex = format!("{}{}", h1, h2);
                        let value = u8::from_str_radix(&hex, 16)
                            .unwrap_or_else(|_| panic!("Invalid hex escape: \\x{}", hex));

                        string_literal.push(value as char);
                    }
                    _ => {
                        panic!("Unknown escape sequence: \\{}", next);
                    }
                }
            }
            else {
                string_literal.push(c);
            }
        }

        if self.is_at_end() {
            panic!("Unterminated string.");
        }

        self.advance(); // closing quote
        string_literal
    }

    pub(crate) fn char_literal(&mut self) -> char {
        let c = if self.peek() == '\\' {
            self.advance();
            let escaped = self.advance();
            match escaped {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '\\' => '\\',
                '\'' => '\'',
                '"' => '"',
                _ => panic!("Invalid escape sequence in char literal"),
            }
        } else {
            self.advance()
        };

        if self.peek() != '\'' {
            panic!("Unterminated or invalid char literal");
        }
        self.advance(); // closing '
        c
    }
}
