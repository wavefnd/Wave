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

//! RISC-V 64-bit target features, ABI names, registers, and stack effects.
//!
//! Integer and floating-point ABI aliases are normalized to physical register
//! numbers. Feature-to-ISA spelling lives here; compatibility between the
//! selected ISA and LP64/LP64F/LP64D is enforced by the target resolver.

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
pub(crate) const CPUS: &[&str] = &["generic", "generic-rv64", "rocket-rv64", "sifive-u74"];
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
pub(crate) const FEATURES: &[&str] = &["m", "a", "f", "d", "c", "zicsr", "zifencei"];
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
pub(crate) const ABIS: &[&str] = &["lp64", "lp64f", "lp64d"];
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
pub(crate) const DEFAULT_CPU: &str = "generic-rv64";
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
pub(crate) const LINUX_DEFAULT_FEATURES: &[&str] = &["m", "a", "f", "d", "c", "zicsr", "zifencei"];
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
pub(crate) const FREESTANDING_DEFAULT_FEATURES: &[&str] = &["m", "a", "c"];
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
pub(crate) const LINUX_DEFAULT_ABI: &str = "lp64d";
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
pub(crate) const FREESTANDING_DEFAULT_ABI: &str = "lp64";

pub(crate) fn isa_name(
    m: bool,
    a: bool,
    f: bool,
    d: bool,
    c: bool,
    zicsr: bool,
    zifencei: bool,
) -> String {
    // Prefer the standard `g` shorthand only when the complete general-purpose
    // extension set is present; otherwise preserve the precise extension set.
    if m && a && f && d && c && zicsr && zifencei {
        return "rv64gc".to_string();
    }

    let mut isa = String::from("rv64i");
    for (enabled, extension) in [(m, 'm'), (a, 'a'), (f, 'f'), (d, 'd'), (c, 'c')] {
        if enabled {
            isa.push(extension);
        }
    }
    if zicsr {
        isa.push_str("_zicsr");
    }
    if zifencei {
        isa.push_str("_zifencei");
    }
    isa
}

pub(crate) fn register_group(token: &str) -> Option<String> {
    if let Some(number) = floating_register_number(token) {
        return Some(format!("f{}", number));
    }

    let number = match token {
        "zero" => 0,
        "ra" => 1,
        "sp" => 2,
        "gp" => 3,
        "tp" => 4,
        "t0" => 5,
        "t1" => 6,
        "t2" => 7,
        "s0" | "fp" => 8,
        "s1" => 9,
        "a0" => 10,
        "a1" => 11,
        "a2" => 12,
        "a3" => 13,
        "a4" => 14,
        "a5" => 15,
        "a6" => 16,
        "a7" => 17,
        "s2" => 18,
        "s3" => 19,
        "s4" => 20,
        "s5" => 21,
        "s6" => 22,
        "s7" => 23,
        "s8" => 24,
        "s9" => 25,
        "s10" => 26,
        "s11" => 27,
        "t3" => 28,
        "t4" => 29,
        "t5" => 30,
        "t6" => 31,
        _ => {
            let raw = token.strip_prefix('x')?;
            if raw.is_empty() || !raw.chars().all(|c| c.is_ascii_digit()) {
                return None;
            }
            raw.parse::<u32>().ok()?
        },
    };
    (number <= 31).then(|| format!("x{}", number))
}

fn floating_register_number(token: &str) -> Option<u32> {
    let number = match token {
        "ft0" => 0,
        "ft1" => 1,
        "ft2" => 2,
        "ft3" => 3,
        "ft4" => 4,
        "ft5" => 5,
        "ft6" => 6,
        "ft7" => 7,
        "fs0" => 8,
        "fs1" => 9,
        "fa0" => 10,
        "fa1" => 11,
        "fa2" => 12,
        "fa3" => 13,
        "fa4" => 14,
        "fa5" => 15,
        "fa6" => 16,
        "fa7" => 17,
        "fs2" => 18,
        "fs3" => 19,
        "fs4" => 20,
        "fs5" => 21,
        "fs6" => 22,
        "fs7" => 23,
        "fs8" => 24,
        "fs9" => 25,
        "fs10" => 26,
        "fs11" => 27,
        "ft8" => 28,
        "ft9" => 29,
        "ft10" => 30,
        "ft11" => 31,
        _ => {
            let raw = token.strip_prefix('f')?;
            if raw.is_empty() || !raw.chars().all(|c| c.is_ascii_digit()) {
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
    (!matches!(group.as_str(), "x0" | "x2" | "x3" | "x4")).then_some(group)
}

pub(crate) fn register_width_bits(token: &str) -> Option<u32> {
    register_group(token).map(|_| 64)
}

pub(crate) fn default_clobbers() -> Vec<String> {
    vec!["~{memory}".to_string()]
}

pub(crate) fn allocatable_registers() -> Vec<String> {
    let mut registers = [
        1u32, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
        27, 28, 29, 30, 31,
    ]
    .into_iter()
    .map(|number| format!("x{}", number))
    .collect::<Vec<_>>();
    registers.extend((0..=31).map(|number| format!("f{}", number)));
    registers
}

pub(crate) fn normalize_special_clobber(token: &str) -> Option<String> {
    (token == "memory").then(|| "~{memory}".to_string())
}

pub(crate) fn stack_analysis(line: &str) -> super::StackAnalysis {
    let code = super::instruction_text(line, true);
    if code.is_empty() {
        return super::StackAnalysis::default();
    }
    let touches_stack = code == "ret"
        || code.starts_with("call ")
        || code.starts_with("jal ")
        || code.starts_with("jalr ")
        || code.contains(" sp,")
        || code.contains(", sp")
        || code.contains("(sp)");
    let mnemonic = super::mnemonic(&code);
    let nonreturning_branch = match mnemonic {
        "jr" | "tail" | "ret" => true,
        "jalr" => code
            .split(|ch: char| ch.is_ascii_whitespace() || ch == ',')
            .filter(|part| !part.is_empty())
            .nth(1)
            .is_some_and(|rd| matches!(rd, "x0" | "zero")),
        _ => false,
    };
    super::StackAnalysis {
        touches_stack,
        unknown_stack_write: touches_stack
            && (code.contains(" sp,") || code.starts_with("addi sp")),
        nonreturning_branch,
        ..Default::default()
    }
}
