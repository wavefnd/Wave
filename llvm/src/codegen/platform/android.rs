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

//! Android NDK target-planning contract.

use super::{LinkerFamily, PlatformPlan};

pub const CANDIDATE_TRIPLES: &[&str] =
    &["x86_64-linux-android", "aarch64-linux-android", "riscv64-linux-android"];

pub const PLAN: PlatformPlan = PlatformPlan {
    os: "android",
    architectures: &["x86_64", "aarch64", "riscv64"],
    object_format: "elf",
    sdk: "Android NDK",
    linker: LinkerFamily::ElfLld,
};
