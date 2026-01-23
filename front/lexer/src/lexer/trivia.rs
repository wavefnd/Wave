use super::Lexer;

impl<'a> Lexer<'a> {
    pub(crate) fn skip_trivia(&mut self) {
        loop {
            self.skip_whitespace();

            if self.is_at_end() {
                break;
            }

            // line comment //
            if self.peek() == '/' && self.peek_next() == '/' {
                self.advance(); // '/'
                self.advance(); // '/'
                self.skip_comment();
                continue;
            }

            // block comment /* */
            if self.peek() == '/' && self.peek_next() == '*' {
                self.advance(); // '/'
                self.advance(); // '*'
                self.skip_multiline_comment();
                continue;
            }

            break;
        }
    }

    pub(crate) fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            let c = self.peek();
            match c {
                ' ' | '\r' | '\t' => { self.advance(); }
                '\n' => { self.line += 1; self.advance(); }
                _ => break,
            }
        }
    }

    pub(crate) fn skip_comment(&mut self) {
        while !self.is_at_end() && self.peek() != '\n' {
            self.advance();
        }
    }

    pub(crate) fn skip_multiline_comment(&mut self) {
        while !self.is_at_end() {
            if self.peek() == '*' && self.peek_next() == '/' {
                self.advance();
                self.advance();
                break;
            }

            if self.peek() == '\n' {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            panic!("Unterminated block comment");
        }
    }
}
