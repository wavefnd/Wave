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

//! Small dependency-free utilities shared across Wave compiler crates.
//!
//! Keep this crate independent of frontend and backend types so formatting,
//! terminal color, and serialization helpers remain reusable at every compiler layer.

pub mod colorex;
pub mod const_int;
pub mod display_width;
pub mod formatx;
pub mod paths;
pub mod wson;

pub use colorex::Colorize;

pub mod suggestions;
