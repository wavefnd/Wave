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

// Lexer errors intentionally retain complete diagnostic context for rendering.
#![allow(clippy::result_large_err)]

pub mod core;
pub mod cursor;
pub mod ident;
pub mod literals;
pub mod scan;
pub mod token;
pub mod trivia;

pub use crate::core::{Lexer, Token};
