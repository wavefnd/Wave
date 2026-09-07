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

//! String, character, and numeric literal scanners.
//!
//! Literal escape validation belongs here rather than in the parser so invalid
//! source spellings retain precise lexer locations and diagnostic suggestions.

use super::Lexer;
use error::{WaveError, WaveErrorKind};

impl<'a> Lexer<'a> {
    pub(crate) fn string(&mut self) -> Result<String, WaveError> {
        let mut string_literal = String::new();
        let start_line = self.line;
        let start_col = self.current_column().saturating_sub(1).max(1);

        while !self.is_at_end() && self.peek() != '"' {
            if self.peek() == '\n' {
                return Err(self
                    .make_error(
                        WaveErrorKind::UnterminatedString,
                        "unterminated string literal (newline encountered before closing quote)",
                        start_line,
                        start_col,
                    )
                    .with_code("E1003")
                    .with_label("string literal starts here")
                    .with_help("close the string with `\"` before the end of line")
                    .with_suggestion("use `\\n` if you meant to embed a newline"));
            }

            let c = self.advance();

            if c == '\\' {
                if self.is_at_end() {
                    return Err(self
                        .make_error_here(
                            WaveErrorKind::InvalidString(
                                "dangling escape sequence in string literal".to_string(),
                            ),
                            "invalid escape sequence: trailing `\\` at end of string",
                        )
                        .with_code("E1004")
                        .with_label("escape sequence is incomplete")
                        .with_help(
                            "append a valid escape character such as `n`, `t`, `\"`, or `\\`",
                        ));
                }

                let next = self.advance();
                match next {
                    'n' => string_literal.push('\n'),
                    't' => string_literal.push('\t'),
                    'r' => string_literal.push('\r'),
                    '\\' => string_literal.push('\\'),
                    '"' => string_literal.push('"'),
                    'x' => {
                        if self.is_at_end() {
                            return Err(self
                                .make_error_here(
                                    WaveErrorKind::InvalidString(
                                        "incomplete hex escape sequence".to_string(),
                                    ),
                                    "invalid escape sequence: expected two hex digits after `\\x`",
                                )
                                .with_code("E1004")
                                .with_help("example: `\\x41` for `A`"));
                        }
                        let h1 = self.advance();
                        if self.is_at_end() {
                            return Err(self
                                .make_error_here(
                                    WaveErrorKind::InvalidString(
                                        "incomplete hex escape sequence".to_string(),
                                    ),
                                    "invalid escape sequence: expected two hex digits after `\\x`",
                                )
                                .with_code("E1004")
                                .with_help("example: `\\x41` for `A`"));
                        }
                        let h2 = self.advance();

                        let hex = format!("{}{}", h1, h2);
                        let value = match u8::from_str_radix(&hex, 16) {
                            Ok(v) => v,
                            Err(_) => {
                                return Err(self
                                    .make_error_here(
                                        WaveErrorKind::InvalidString(format!(
                                            "invalid hex escape: \\x{}",
                                            hex
                                        )),
                                        format!(
                                            "invalid hex escape sequence `\\x{}` in string literal",
                                            hex
                                        ),
                                    )
                                    .with_code("E1004")
                                    .with_label(
                                        "hex escapes must be exactly two hexadecimal digits",
                                    )
                                    .with_help("valid range: `00` to `FF`"));
                            }
                        };

                        string_literal.push(value as char);
                    }
                    _ => {
                        return Err(self
                            .make_error_here(
                                WaveErrorKind::InvalidString(format!(
                                    "unknown escape sequence: \\{}",
                                    next
                                )),
                                format!("unknown escape sequence `\\{}` in string literal", next),
                            )
                            .with_code("E1004")
                            .with_label("unsupported escape sequence")
                            .with_help("supported escapes: \\\\, \\\", \\n, \\t, \\r, \\xNN"));
                    }
                }
            } else {
                string_literal.push(c);
            }
        }

        if self.is_at_end() {
            return Err(self
                .make_error(
                    WaveErrorKind::UnterminatedString,
                    "unterminated string literal; missing closing quote",
                    start_line,
                    start_col,
                )
                .with_code("E1003")
                .with_label("string literal starts here")
                .with_help("add `\"` to close the string"));
        }

        self.advance(); // closing quote
        Ok(string_literal)
    }

    pub(crate) fn char_literal(&mut self) -> Result<char, WaveError> {
        let start_line = self.line;
        let start_col = self.current_column().saturating_sub(1).max(1);

        if self.is_at_end() {
            return Err(self
                .make_error(
                    WaveErrorKind::InvalidString("empty char literal".to_string()),
                    "unterminated char literal; expected a character before closing quote",
                    start_line,
                    start_col,
                )
                .with_code("E1005")
                .with_help("write a single character like `'a'` or an escape like `'\\n'`"));
        }

        let c = if self.peek() == '\\' {
            self.advance();

            if self.is_at_end() {
                return Err(self
                    .make_error(
                        WaveErrorKind::InvalidString("dangling char escape".to_string()),
                        "unterminated char literal; dangling escape sequence",
                        start_line,
                        start_col,
                    )
                    .with_code("E1005")
                    .with_help("complete the escape and close the char literal with `'`"));
            }

            let escaped = self.advance();
            match escaped {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '\\' => '\\',
                '\'' => '\'',
                '"' => '"',
                'x' => {
                    if self.is_at_end() {
                        return Err(self
                            .make_error(
                                WaveErrorKind::InvalidString(
                                    "incomplete hex escape in char literal".to_string(),
                                ),
                                "invalid char escape: expected two hex digits after `\\x`",
                                start_line,
                                start_col,
                            )
                            .with_code("E1005")
                            .with_help("example: `'\\x41'` for `A`"));
                    }
                    let h1 = self.advance();
                    if self.is_at_end() {
                        return Err(self
                            .make_error(
                                WaveErrorKind::InvalidString(
                                    "incomplete hex escape in char literal".to_string(),
                                ),
                                "invalid char escape: expected two hex digits after `\\x`",
                                start_line,
                                start_col,
                            )
                            .with_code("E1005")
                            .with_help("example: `'\\x41'` for `A`"));
                    }
                    let h2 = self.advance();
                    let hex = format!("{}{}", h1, h2);
                    let value = match u8::from_str_radix(&hex, 16) {
                        Ok(v) => v,
                        Err(_) => {
                            return Err(self
                                .make_error(
                                    WaveErrorKind::InvalidString(format!(
                                        "invalid hex escape in char literal: \\x{}",
                                        hex
                                    )),
                                    format!(
                                        "invalid hex escape sequence `\\x{}` in char literal",
                                        hex
                                    ),
                                    start_line,
                                    start_col,
                                )
                                .with_code("E1005")
                                .with_help("hex escapes must be two hexadecimal digits"));
                        }
                    };
                    value as char
                }
                _ => {
                    return Err(self
                        .make_error(
                            WaveErrorKind::InvalidString(format!(
                                "invalid escape sequence in char literal: \\{}",
                                escaped
                            )),
                            format!("invalid escape sequence `\\{}` in char literal", escaped),
                            start_line,
                            start_col,
                        )
                        .with_code("E1005")
                        .with_help("supported escapes: \\\\, \\\', \\n, \\t, \\r, \\xNN"));
                }
            }
        } else {
            self.advance()
        };

        if self.peek() != '\'' {
            return Err(self
                .make_error(
                    WaveErrorKind::InvalidString("unterminated char literal".to_string()),
                    "unterminated or invalid char literal",
                    start_line,
                    start_col,
                )
                .with_code("E1005")
                .with_label("char literal must contain exactly one character")
                .with_help("close with `'` and ensure exactly one character value"));
        }
        self.advance(); // closing quote
        if u32::from(c) > 255 {
            return Err(self
                .make_error(
                    WaveErrorKind::InvalidString("character is outside the byte range".into()),
                    "char literals must fit the unsigned 8-bit char type",
                    start_line,
                    start_col,
                )
                .with_code("E1005"));
        }
        Ok(c)
    }
}
