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

//! Syntax parser modules and public parsing entry points.
//!
//! Submodules consume complete grammar units. `parse_syntax_only` exposes the
//! source-oriented AST used before import expansion and semantic validation.

pub mod asm;
pub mod control;
pub mod decl;
pub mod expr;
pub mod functions;
pub mod io;
pub mod items;
mod parse;
pub mod stmt;
pub mod types;

pub use parse::{parse, parse_syntax_only, parse_syntax_with_spans, ParseError};
