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

// Backend lowering APIs mirror LLVM's explicit context and ABI structures.
// Refactor these lints separately from release hardening to avoid ABI regressions.
#![allow(
    clippy::box_collection,
    clippy::clone_on_copy,
    clippy::get_first,
    clippy::match_like_matches_macro,
    clippy::missing_safety_doc,
    clippy::needless_lifetimes,
    clippy::needless_range_loop,
    clippy::needless_return,
    clippy::only_used_in_recursion,
    clippy::redundant_closure,
    clippy::result_large_err,
    clippy::single_match,
    clippy::too_many_arguments,
    clippy::type_complexity,
    clippy::unnecessary_cast
)]

pub mod backend;
pub mod codegen;
pub mod expression;
pub mod importgen;
pub mod statement;
pub mod toolchain;

pub fn backend() -> Option<String> {
    let (mut major, mut minor, mut patch) = (0_u32, 0_u32, 0_u32);
    unsafe {
        llvm_sys::core::LLVMGetVersion(&mut major, &mut minor, &mut patch);
    }
    Some(format!("LLVM {}.{}.{}", major, minor, patch))
}
