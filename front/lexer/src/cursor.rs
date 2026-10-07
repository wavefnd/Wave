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

//! UTF-8-safe cursor movement and lookahead for the lexer.
//!
//! `current` is a byte offset. All movement advances by the selected character's
//! encoded width; lookahead never treats it as a character index.

use crate::Lexer;

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
                if let Some(c) = chars.next() {
                    (c, c.len_utf8())
                } else {
                    ('\0', 1)
                }
            },
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
        if self.is_at_end() {
            return '\0';
        }

        // `current` is a byte offset, so we must look ahead using a sliced
        // char iterator instead of global char index.
        let rest = &self.source[self.current..];
        let mut it = rest.chars();
        let _cur = it.next();
        it.next().unwrap_or('\0')
    }

    pub(crate) fn match_next(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.peek() != expected {
            return false;
        }
        self.advance();
        true
    }
}
