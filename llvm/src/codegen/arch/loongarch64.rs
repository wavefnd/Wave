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

//! LoongArch64 target features, register aliases, and stack effects.
//!
//! Wave uses the ELF psABI register names accepted by LLVM. Reserved registers
//! (`$zero`, `$tp`, `$sp`, and `$r21`) can be named in assembly text but are
//! never selected as value operands.

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
pub(crate) const CPUS: &[&str] = &["generic", "generic-la64", "loongarch64", "la464", "la664"];
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
pub(crate) const FEATURES: &[&str] = &["f", "d", "lsx", "lasx", "ual", "relax"];
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
pub(crate) const ABIS: &[&str] = &["lp64s", "lp64f", "lp64d"];
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
pub(crate) const DEFAULT_CPU: &str = "loongarch64";
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
pub(crate) const DEFAULT_FEATURES: &[&str] = &["f", "d", "lsx", "ual"];
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
pub(crate) const DEFAULT_ABI: &str = "lp64d";

pub(crate) fn register_group(token: &str) -> Option<String> {
    if let Some(number) = floating_register_number(token) {
        return Some(format!("f{number}"));
    }

    let number = match token {
        "zero" => 0,
        "ra" => 1,
        "tp" => 2,
        "sp" => 3,
        "a0" => 4,
        "a1" => 5,
        "a2" => 6,
        "a3" => 7,
        "a4" => 8,
        "a5" => 9,
        "a6" => 10,
        "a7" => 11,
        "t0" => 12,
        "t1" => 13,
        "t2" => 14,
        "t3" => 15,
        "t4" => 16,
        "t5" => 17,
        "t6" => 18,
        "t7" => 19,
        "t8" => 20,
        "fp" | "s9" => 22,
        "s0" => 23,
        "s1" => 24,
        "s2" => 25,
        "s3" => 26,
        "s4" => 27,
        "s5" => 28,
        "s6" => 29,
        "s7" => 30,
        "s8" => 31,
        _ => {
            let raw = token.strip_prefix('r')?;
            if raw.is_empty() || !raw.chars().all(|ch| ch.is_ascii_digit()) {
                return None;
            }
            raw.parse::<u32>().ok()?
        },
    };
    (number <= 31).then(|| format!("r{number}"))
}

fn floating_register_number(token: &str) -> Option<u32> {
    let number = match token {
        "fa0" => 0,
        "fa1" => 1,
        "fa2" => 2,
        "fa3" => 3,
        "fa4" => 4,
        "fa5" => 5,
        "fa6" => 6,
        "fa7" => 7,
        "ft0" => 8,
        "ft1" => 9,
        "ft2" => 10,
        "ft3" => 11,
        "ft4" => 12,
        "ft5" => 13,
        "ft6" => 14,
        "ft7" => 15,
        "ft8" => 16,
        "ft9" => 17,
        "ft10" => 18,
        "ft11" => 19,
        "ft12" => 20,
        "ft13" => 21,
        "ft14" => 22,
        "ft15" => 23,
        "fs0" => 24,
        "fs1" => 25,
        "fs2" => 26,
        "fs3" => 27,
        "fs4" => 28,
        "fs5" => 29,
        "fs6" => 30,
        "fs7" => 31,
        _ => {
            let raw = token.strip_prefix('f')?;
            if raw.is_empty() || !raw.chars().all(|ch| ch.is_ascii_digit()) {
                return None;
            }
            raw.parse::<u32>().ok()?
        },
    };
    (number <= 31).then_some(number)
}

pub(crate) fn operand_register_group(token: &str) -> Option<String> {
    let group = register_group(token)?;
    if group.starts_with('f') {
        return Some(group);
    }
    (!matches!(group.as_str(), "r0" | "r2" | "r3" | "r21")).then_some(group)
}

pub(crate) fn register_width_bits(token: &str) -> Option<u32> {
    register_group(token).map(|_| 64)
}

pub(crate) fn default_clobbers() -> Vec<String> {
    vec!["~{memory}".to_string(), "~{cc}".to_string()]
}

pub(crate) fn allocatable_registers() -> Vec<String> {
    let mut registers = (0..=31)
        .filter(|number| !matches!(number, 0 | 2 | 3 | 21))
        .map(|number| format!("r{number}"))
        .collect::<Vec<_>>();
    registers.extend((0..=31).map(|number| format!("f{number}")));
    registers
}

pub(crate) fn normalize_special_clobber(token: &str) -> Option<String> {
    match token {
        "memory" => Some("~{memory}".to_string()),
        "cc" | "flags" => Some("~{cc}".to_string()),
        _ => None,
    }
}

pub(crate) fn stack_analysis(line: &str) -> super::StackAnalysis {
    let code = super::instruction_text(line, true);
    if code.is_empty() {
        return super::StackAnalysis::default();
    }
    let touches_stack = code == "ret"
        || code.starts_with("bl ")
        || code.starts_with("jirl ")
        || code.contains(" sp,")
        || code.contains(", sp")
        || code.contains("(sp)");
    let nonreturning_branch = super::mnemonic(&code) == "jr"
        || (super::mnemonic(&code) == "jirl"
            && code
                .split(|ch: char| ch.is_ascii_whitespace() || ch == ',')
                .filter(|part| !part.is_empty())
                .nth(1)
                .is_some_and(|rd| matches!(rd, "r0" | "zero")));
    super::StackAnalysis {
        touches_stack,
        unknown_stack_write: touches_stack && code.contains(" sp,"),
        nonreturning_branch,
        ..Default::default()
    }
}
