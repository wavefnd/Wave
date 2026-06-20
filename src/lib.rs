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

// CLI tables and compiler phase boundaries intentionally favor explicit data.
#![allow(
    clippy::print_literal,
    clippy::result_large_err,
    clippy::too_many_arguments,
    clippy::type_complexity
)]

pub mod cli;
pub mod errors;
pub mod flags;
pub mod runner;
pub mod std;
pub mod version;

pub use flags::{DebugFlags, DepFlags, LinkFlags, LlvmFlags, WhaleFlags};
