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

//! Wave syntax, AST, import expansion, generic specialization, and semantic validation.
//!
//! Parsing intentionally produces a source-oriented AST first. Imports and
//! generics are expanded before the semantic verifier establishes a
//! backend-neutral [`hir::TypedProgram`]. Backend lowering consumes that typed
//! boundary directly and queries semantic facts through stable HIR identities.

// These legacy parser APIs are being migrated incrementally; keep new lints fatal
// without forcing risky mechanical rewrites into a release hardening change.
#![allow(
    clippy::clone_on_copy,
    clippy::explicit_auto_deref,
    clippy::manual_strip,
    clippy::needless_borrow,
    clippy::ptr_arg,
    clippy::result_large_err,
    clippy::type_complexity,
    clippy::unnecessary_map_or,
    clippy::useless_format,
    clippy::while_let_on_iterator
)]

// Legacy parser modules still call `println!("Error: ...")` on failure paths.
// Keep diagnostics single-sourced through `ParseError` by silencing those prints.
macro_rules! println {
    ($($arg:tt)*) => {{
        let _ = format!($($arg)*);
    }};
}

pub mod arch;
pub mod ast;
pub mod expr;
pub mod format;
pub mod generics;
pub mod hir;
pub mod import;
pub mod os;
pub mod parser;
pub mod stdlib;
pub mod verification;

pub use parser::*;

pub mod source;
