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

//! Core token and lexer state.
//!
//! Cursor positions are UTF-8 byte offsets, while diagnostic columns count
//! Unicode scalar values from `line_start`. Cursor helpers must preserve that
//! distinction.

use crate::token::TokenType;
use error::{WaveError, WaveErrorKind};

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub span: Option<error::SourceSpan>,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize) -> Self {
        Token {
            token_type,
            lexeme,
            line,
            span: None,
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Token {
            token_type: TokenType::Eof,
            lexeme: String::new(),
            line: 0,
            span: None,
        }
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    pub source: &'a str,
    pub file: String,
    pub current: usize,
    pub line: usize,
    pub line_start: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        Lexer::new_with_file(source, "<input>")
    }

    pub fn new_with_file(source: &'a str, file: impl Into<String>) -> Lexer<'a> {
        Lexer {
            source,
            file: file.into(),
            current: 0,
            line: 1,
            line_start: 0,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, WaveError> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            if token.token_type == TokenType::Eof {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        Ok(tokens)
    }

    pub(crate) fn current_column(&self) -> usize {
        self.column_at(self.current)
    }

    pub(crate) fn column_at(&self, byte_index: usize) -> usize {
        let line_start = self.line_start.min(self.source.len());
        let end = byte_index.min(self.source.len());
        if end < line_start {
            return 1;
        }
        self.source[line_start..end].chars().count() + 1
    }

    pub(crate) fn make_error(
        &self,
        kind: WaveErrorKind,
        message: impl Into<String>,
        line: usize,
        column: usize,
    ) -> WaveError {
        let line_start: usize = self
            .source
            .split_inclusive('\n')
            .take(line.saturating_sub(1))
            .map(str::len)
            .sum();
        let line_text = self
            .source
            .get(line_start..)
            .unwrap_or("")
            .split('\n')
            .next()
            .unwrap_or("");
        let start = line_start
            + line_text
                .char_indices()
                .nth(column.saturating_sub(1))
                .map_or(line_text.len(), |(offset, _)| offset);
        let end = self.current.max(start).min(self.source.len());
        let prefix = &self.source[..end];
        let span = error::SourceSpan {
            file: self.file.clone(),
            start,
            end,
            line,
            column,
            end_line: prefix.bytes().filter(|byte| *byte == b'\n').count() + 1,
            end_column: prefix.rsplit('\n').next().unwrap_or("").chars().count() + 1,
            expansion: Vec::new(),
            focus: None,
        };
        WaveError::new(kind, message, self.file.clone(), line, column)
            .with_source_code(self.source.to_string())
            .with_span(Some(&span))
    }

    pub(crate) fn make_error_here(
        &self,
        kind: WaveErrorKind,
        message: impl Into<String>,
    ) -> WaveError {
        self.make_error(kind, message, self.line, self.current_column())
    }
}
