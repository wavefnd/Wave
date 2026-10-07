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

//! Typed option groups passed from CLI planning to compiler phases.
//!
//! These structures contain parsed values only. Cross-option contracts such as
//! target CPU, feature, ABI, and output compatibility are validated in the CLI
//! and target resolver before use.

#[derive(Default, Clone, Copy)]
pub struct DebugFlags {
    pub tokens: bool,
    pub ast: bool,
    pub ir: bool,
    pub mc: bool,
    pub hex: bool,
}

impl DebugFlags {
    /// --debug-wave=tokens
    /// --debug-wave=tokens,ir
    /// --debug-wave=all
    pub fn apply(&mut self, mode: &str) {
        if mode.trim().is_empty() {
            return;
        }

        for item in mode.split(',').map(|s| s.trim()).filter(|s| !s.is_empty()) {
            match item {
                "tokens" => self.tokens = true,
                "ast" => self.ast = true,
                "ir" => self.ir = true,
                "mc" => self.mc = true,
                "hex" => self.hex = true,
                "all" => {
                    self.tokens = true;
                    self.ast = true;
                    self.ir = true;
                    self.mc = true;
                    self.hex = true;
                },
                _ => {},
            }
        }
    }
}

#[derive(Default, Clone)]
pub struct LinkFlags {
    pub libs: Vec<String>,
    pub paths: Vec<String>,
}

#[derive(Clone)]
pub struct DepPackage {
    pub name: String,
    pub path: String,
}

#[derive(Default, Clone)]
pub struct DepFlags {
    pub resolved_std_root: Option<Result<parser::import::ResolvedStdRoot, error::WaveError>>,
    pub roots: Vec<String>,
    pub packages: Vec<DepPackage>,
}

#[derive(Default, Clone)]
pub struct LlvmFlags {
    pub target: Option<String>,
    pub target_explicit: bool,
    pub cpu: Option<String>,
    pub features: Option<String>,
    pub abi: Option<String>,
    pub isa: Option<String>,
    pub code_model: Option<String>,
    pub relocation_model: Option<String>,
    pub sysroot: Option<String>,
    pub sysroot_source: Option<String>,
    pub linker: Option<String>,
    pub link_args: Vec<String>,
    pub no_default_libs: bool,
    pub freestanding: bool,
}

#[derive(Default, Clone)]
pub struct WhaleFlags {
    pub enabled: bool,
}

pub fn validate_opt_flag(flag: &str) -> bool {
    matches!(flag, "-O0" | "-O1" | "-O2" | "-O3" | "-Os" | "-Oz" | "-Ofast")
}
