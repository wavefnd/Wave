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

//! Lightweight Wave format-placeholder scanning without regular expressions.
//!
//! The supported form is a non-nested `{...}` pair with no escape processing.
//! Unterminated opening braces are ignored rather than counted as placeholders.

#[derive(Debug, Clone)]
pub struct Placeholder {
    pub spec: String,
}

/// Byte-preserving parts shared by frontend checks and backend lowering.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FormatFragment<'a> {
    Literal(&'a [u8]),
    Placeholder(&'a str),
}

/// Parse the fixed fractional precision in `{:.N}` output placeholders.
///
/// Other placeholder kinds return `None`. Precision is bounded by C printf's
/// signed int range, shared by native output and the WebAssembly output host.
pub fn float_precision(spec: &str) -> Result<Option<i32>, &'static str> {
    let Some(digits) = spec.strip_prefix(":.") else {
        return Ok(None);
    };
    if digits.is_empty() || !digits.bytes().all(|digit| digit.is_ascii_digit()) {
        return Err("float precision must be a nonnegative decimal integer in `{:.N}`");
    }
    let mut precision = 0i32;
    for digit in digits.bytes() {
        precision = precision
            .checked_mul(10)
            .and_then(|value| value.checked_add(i32::from(digit - b'0')))
            .ok_or("float precision exceeds 2147483647")?;
    }
    Ok(Some(precision))
}

pub fn format_fragments(input: &[u8]) -> Result<Vec<FormatFragment<'_>>, &'static str> {
    let mut parts = Vec::new();
    let mut start = 0;
    let mut position = 0;
    while position < input.len() {
        if input[position] != b'{' {
            position += 1;
            continue;
        }
        let Some(close) = input[position + 1..].iter().position(|b| *b == b'}') else {
            break;
        };
        let end = position + 1 + close;
        if start < position {
            parts.push(FormatFragment::Literal(&input[start..position]));
        }
        let spec = std::str::from_utf8(&input[position + 1..end])
            .map_err(|_| "format placeholder must contain UTF-8 text")?;
        parts.push(FormatFragment::Placeholder(spec.trim()));
        position = end + 1;
        start = position;
    }
    if start < input.len() {
        parts.push(FormatFragment::Literal(&input[start..]));
    }
    Ok(parts)
}

pub fn parse_placeholders(input: &str) -> Vec<Placeholder> {
    format_fragments(input.as_bytes())
        .expect("UTF-8 source")
        .into_iter()
        .filter_map(|part| match part {
            FormatFragment::Placeholder(spec) => Some(Placeholder { spec: spec.into() }),
            _ => None,
        })
        .collect()
}

pub fn count_placeholders(input: impl AsRef<[u8]>) -> usize {
    format_fragments(input.as_ref())
        .map(|parts| {
            parts.into_iter().filter(|p| matches!(p, FormatFragment::Placeholder(_))).count()
        })
        .unwrap_or(0)
}
