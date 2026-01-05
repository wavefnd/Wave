use super::Lexer;

impl<'a> Lexer<'a> {
    pub(crate) fn string(&mut self) -> String {
        if self.peek() == '"' { self.advance(); }

        let mut string_literal = String::new();
        while !self.is_at_end() && self.peek() != '"' {
            let c = self.advance();

            if c == '\\' {
                let next = self.advance();
                match next {
                    'n' => string_literal.push('\n'),
                    't' => string_literal.push('\t'),
                    'r' => string_literal.push('\r'),
                    '\\' => string_literal.push('\\'),
                    '"' => string_literal.push('"'),
                    _ => {
                        string_literal.push('\\');
                        string_literal.push(next);
                    }
                }
            } else {
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
