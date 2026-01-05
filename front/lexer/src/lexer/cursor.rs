use super::{Lexer};

impl<'a> Lexer<'a> {
    pub(crate) fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    pub(crate) fn advance(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        let rest = &self.source[self.current..];
        let (ch, size) = match std::str::from_utf8(rest.as_ref()) {
            Ok(s) => {
                let mut chars = s.chars();
                if let Some(c) = chars.next() { (c, c.len_utf8()) } else { ('\0', 1) }
            }
            Err(_) => ('\0', 1),
        };

        self.current += size;
        ch
    }

    pub(crate) fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            let rest = &self.source[self.current..];
            match std::str::from_utf8(rest.as_ref()) {
                Ok(s) => s.chars().next().unwrap_or('\0'),
                Err(_) => '\0',
            }
        }
    }

    pub(crate) fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source.chars().nth(self.current + 1).unwrap_or('\0')
        }
    }

    pub(crate) fn match_next(&mut self, expected: char) -> bool {
        if self.is_at_end() { return false; }
        if self.peek() != expected { return false; }
        self.advance();
        true
    }
}
