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

//! Whitespace and comment consumption with line tracking.
//!
//! Block comments may nest. Every consumed newline updates both the logical line
//! and byte offset used for later token columns, including newlines inside
//! comments.

use super::Lexer;
use error::WaveError;
use error::WaveErrorKind;

impl<'a> Lexer<'a> {
    fn consume_newline(&mut self) {
        let first = self.advance();
        if first == '\r' && self.peek() == '\n' {
            self.advance();
        }
        self.line += 1;
        self.line_start = self.current;
        self.column = 1;
    }

    pub(crate) fn skip_trivia(&mut self) -> Result<(), WaveError> {
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
                self.skip_multiline_comment()?;
                continue;
            }

            break;
        }

        Ok(())
    }

    pub(crate) fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            let c = self.peek();
            match c {
                ' ' | '\t' => {
                    self.advance();
                },
                '\n' | '\r' => self.consume_newline(),
                _ => break,
            }
        }
    }

    pub(crate) fn skip_comment(&mut self) {
        while !self.is_at_end() && !matches!(self.peek(), '\n' | '\r') {
            self.advance();
        }
    }

    pub(crate) fn skip_multiline_comment(&mut self) -> Result<(), WaveError> {
        let opening = |lexer: &Self| (lexer.current - 2, lexer.line, lexer.current_column() - 2);
        let mut openers = vec![opening(self)];

        while !self.is_at_end() {
            if self.peek() == '/' && self.peek_next() == '*' {
                self.advance();
                self.advance();
                openers.push(opening(self));
                continue;
            }

            if self.peek() == '*' && self.peek_next() == '/' {
                self.advance();
                self.advance();
                openers.pop();
                if openers.is_empty() {
                    return Ok(());
                }
                continue;
            }

            if matches!(self.peek(), '\n' | '\r') {
                self.consume_newline();
                continue;
            }

            self.advance();
        }

        let (start, line, column) = *openers.last().expect("an unmatched comment remains");
        let opener = error::SourceSpan {
            file: self.file.clone(),
            start,
            end: start + 2,
            line,
            column,
            end_line: line,
            end_column: column + 2,
            expansion: Vec::new(),
            focus: None,
        };
        Err(self
            .make_error_here(
                WaveErrorKind::UnterminatedComment,
                "unterminated block comment; expected closing `*/`",
            )
            .with_code("E1002")
            .with_label("expected `*/` before end of file")
            .with_related([error::RelatedDiagnostic {
                message: "unclosed block comment opened here; expected `*/`".into(),
                span: opener,
            }])
            .with_help("add `*/` to close the block comment")
            .with_suggestion("if you intended a line comment, use `// ...`"))
    }
}
