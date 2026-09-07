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

//! Wave source lexer.
//!
//! The lexer converts UTF-8 source text into positioned tokens. Token spelling
//! is preserved for diagnostics, while literal and identifier modules perform
//! the validation needed before parsing begins.

// Lexer errors intentionally retain complete diagnostic context for rendering.
#![allow(clippy::result_large_err)]

pub mod core;
pub mod cursor;
pub mod ident;
pub mod literals;
pub mod number;
pub mod scan;
pub mod token;
pub mod trivia;

pub use crate::core::{Lexer, Token};

/// Range of exactly the tokens consumed between two parser cursors.
pub fn consumed_span<'a, T>(
    before: std::iter::Peekable<T>,
    after: &mut std::iter::Peekable<T>,
) -> Option<error::SourceSpan>
where
    T: Iterator<Item = &'a Token> + Clone,
{
    let stop = after.peek().copied();
    let mut consumed = before.take_while(|t| stop.is_none_or(|stop| !std::ptr::eq(*t, stop)));
    let first = consumed.next()?.span.as_ref()?.clone();
    Some(
        consumed
            .last()
            .and_then(|t| t.span.as_ref())
            .map_or(first.clone(), |last| first.through(last)),
    )
}
