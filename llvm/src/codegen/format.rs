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

//! Translation of Wave formatting placeholders to C `printf`/`scanf` formats.
//!
//! Format selection combines LLVM value types with Wave-level pointer meaning;
//! opaque LLVM pointers alone cannot distinguish C strings from other pointers.

use parser::format::{format_fragments, FormatFragment};

pub fn escape_percent(bytes: &[u8]) -> Vec<u8> {
    let mut result = Vec::new();
    for byte in bytes {
        result.push(*byte);
        if *byte == b'%' {
            result.push(b'%');
        }
    }
    result
}

/// Argument formats are selected from semantic types and lowered values.
pub fn wave_format_to_c(format: &[u8], arg_formats: &[&str]) -> Vec<u8> {
    let mut result = Vec::new();
    let mut formats = arg_formats.iter();
    for part in format_fragments(format).expect("validated format") {
        match part {
            FormatFragment::Literal(bytes) => result.extend(escape_percent(bytes)),
            FormatFragment::Placeholder(_) => {
                result.extend_from_slice(formats.next().expect("validated arity").as_bytes())
            },
        }
    }
    assert!(formats.next().is_none(), "validated format arity");
    result
}
