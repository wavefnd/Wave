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

//! AArch64 register aliases, inline-assembly clobbers, and stack effects.
//!
//! The platform register (`x18`), stack pointer, and zero register are excluded
//! from general operand allocation even though they are valid assembler names.

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-aarch64"))]
pub(crate) const CPUS: &[&str] = &["generic", "cortex-a53", "cortex-a72"];
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-aarch64"))]
pub(crate) const DARWIN_CPUS: &[&str] = &["generic", "apple-m1"];
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-aarch64"))]
pub(crate) const FEATURES: &[&str] = &["neon", "fp-armv8", "crypto"];
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-aarch64"))]
pub(crate) const DEFAULT_CPU: &str = "generic";
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-aarch64"))]
pub(crate) const DEFAULT_FEATURES: &[&str] = &[];

pub(crate) fn register_group(token: &str) -> Option<String> {
    match token {
        "fp" => return Some("x29".to_string()),
        "lr" => return Some("x30".to_string()),
        "ip0" => return Some("x16".to_string()),
        "ip1" => return Some("x17".to_string()),
        "sp" => return Some("sp".to_string()),
        "xzr" | "wzr" => return Some("xzr".to_string()),
        _ => {},
    }
    let (prefix, num) = token.split_at_checked(1)?;
    if !matches!(prefix, "x" | "w") || num.is_empty() || !num.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    let number = num.parse::<u32>().ok()?;
    (number <= 30).then(|| format!("x{}", number))
}

pub(crate) fn register_width_bits(token: &str) -> Option<u32> {
    let (prefix, num) = token.split_at_checked(1)?;
    if num.is_empty() || !num.chars().all(|c| c.is_ascii_digit()) || num.parse::<u32>().ok()? > 30 {
        return None;
    }
    match prefix {
        "w" => Some(32),
        "x" => Some(64),
        _ => None,
    }
}

pub(crate) fn operand_register_group(token: &str) -> Option<String> {
    let group = register_group(token)?;
    (!matches!(group.as_str(), "sp" | "xzr")).then_some(group)
}

pub(crate) fn default_clobbers() -> Vec<String> {
    vec!["~{memory}".to_string(), "~{cc}".to_string()]
}

pub(crate) fn allocatable_registers() -> Vec<String> {
    (0..=30).filter(|number| *number != 18).map(|number| format!("x{}", number)).collect()
}

pub(crate) fn normalize_special_clobber(token: &str) -> Option<String> {
    match token {
        "memory" => Some("~{memory}".to_string()),
        "cc" | "flags" | "eflags" | "rflags" => Some("~{cc}".to_string()),
        _ => None,
    }
}

pub(crate) fn stack_analysis(line: &str) -> super::StackAnalysis {
    let code = super::instruction_text(line, false);
    if code.is_empty() {
        return super::StackAnalysis::default();
    }
    let touches_stack = code == "ret"
        || code.starts_with("ret ")
        || code.starts_with("bl ")
        || code.starts_with("blr ")
        || code.contains(" sp,")
        || code.contains(", sp")
        || code.contains("[sp");
    super::StackAnalysis {
        touches_stack,
        unknown_stack_write: touches_stack && code.contains(" sp,"),
        nonreturning_branch: super::mnemonic(&code) == "br",
        ..Default::default()
    }
}
