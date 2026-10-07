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

//! Architecture-specific inline-assembly contracts.
//!
//! The shared codegen layer dispatches through this module for register aliases,
//! operand eligibility, clobbers, dialect selection, and conservative stack
//! analysis. Target-specific spelling must stay in the architecture modules so
//! accepting syntax for one ISA cannot silently affect another.

pub(crate) mod aarch64;
pub(crate) mod loongarch64;
pub(crate) mod riscv64;
pub(crate) mod wasm;
pub(crate) mod x86_64;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Architecture {
    X86_64,
    Aarch64,
    LoongArch64,
    Riscv64,
    Wasm32,
    Wasm64,
}

impl Architecture {
    pub const fn name(self) -> &'static str {
        match self {
            Self::X86_64 => "x86_64",
            Self::Aarch64 => "aarch64",
            Self::LoongArch64 => "loongarch64",
            Self::Riscv64 => "riscv64",
            Self::Wasm32 => "wasm32",
            Self::Wasm64 => "wasm64",
        }
    }
}

pub(crate) fn register_group(architecture: Architecture, token: &str) -> Option<String> {
    match architecture {
        Architecture::X86_64 => x86_64::register_group(token),
        Architecture::Aarch64 => aarch64::register_group(token),
        Architecture::LoongArch64 => loongarch64::register_group(token),
        Architecture::Riscv64 => riscv64::register_group(token),
        Architecture::Wasm32 | Architecture::Wasm64 => wasm::register_group(token),
    }
}

pub(crate) fn operand_register_group(architecture: Architecture, token: &str) -> Option<String> {
    match architecture {
        Architecture::X86_64 => x86_64::operand_register_group(token),
        Architecture::Aarch64 => aarch64::operand_register_group(token),
        Architecture::LoongArch64 => loongarch64::operand_register_group(token),
        Architecture::Riscv64 => riscv64::operand_register_group(token),
        Architecture::Wasm32 | Architecture::Wasm64 => wasm::operand_register_group(token),
    }
}

pub(crate) fn register_width_bits(architecture: Architecture, token: &str) -> Option<u32> {
    match architecture {
        Architecture::X86_64 => x86_64::register_width_bits(token),
        Architecture::Aarch64 => aarch64::register_width_bits(token),
        Architecture::LoongArch64 => loongarch64::register_width_bits(token),
        Architecture::Riscv64 => riscv64::register_width_bits(token),
        Architecture::Wasm32 | Architecture::Wasm64 => wasm::register_width_bits(token),
    }
}

pub(crate) const fn inline_asm_dialect(architecture: Architecture) -> inkwell::InlineAsmDialect {
    match architecture {
        Architecture::X86_64 => inkwell::InlineAsmDialect::Intel,
        Architecture::Aarch64
        | Architecture::LoongArch64
        | Architecture::Riscv64
        | Architecture::Wasm32
        | Architecture::Wasm64 => inkwell::InlineAsmDialect::ATT,
    }
}

pub(crate) fn default_clobbers(architecture: Architecture) -> Vec<String> {
    match architecture {
        Architecture::X86_64 => x86_64::default_clobbers(),
        Architecture::Aarch64 => aarch64::default_clobbers(),
        Architecture::LoongArch64 => loongarch64::default_clobbers(),
        Architecture::Riscv64 => riscv64::default_clobbers(),
        Architecture::Wasm32 | Architecture::Wasm64 => wasm::default_clobbers(),
    }
}

pub(crate) fn allocatable_registers(architecture: Architecture) -> Vec<String> {
    match architecture {
        Architecture::X86_64 => x86_64::allocatable_registers(),
        Architecture::Aarch64 => aarch64::allocatable_registers(),
        Architecture::LoongArch64 => loongarch64::allocatable_registers(),
        Architecture::Riscv64 => riscv64::allocatable_registers(),
        Architecture::Wasm32 | Architecture::Wasm64 => wasm::allocatable_registers(),
    }
}

pub(crate) fn normalize_special_clobber(architecture: Architecture, token: &str) -> Option<String> {
    match architecture {
        Architecture::X86_64 => x86_64::normalize_special_clobber(token),
        Architecture::Aarch64 => aarch64::normalize_special_clobber(token),
        Architecture::LoongArch64 => loongarch64::normalize_special_clobber(token),
        Architecture::Riscv64 => riscv64::normalize_special_clobber(token),
        Architecture::Wasm32 | Architecture::Wasm64 => wasm::normalize_special_clobber(token),
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct StackAnalysis {
    pub touches_stack: bool,
    pub unknown_stack_write: bool,
    pub unbalanced_delta: i64,
    pub nonreturning_branch: bool,
}

pub(crate) fn instruction_text(line: &str, hash_is_comment: bool) -> String {
    // `#` starts a comment on x86/RISC-V but introduces immediates on AArch64.
    // Callers choose the rule before labels and mnemonics are normalized.
    let without_slash_comment = line.split_once("//").map(|(code, _)| code).unwrap_or(line);
    let line = if hash_is_comment {
        without_slash_comment.split_once('#').map(|(code, _)| code).unwrap_or(without_slash_comment)
    } else {
        without_slash_comment
    };
    let mut code = line.trim().to_ascii_lowercase();

    while let Some((label, rest)) = code.split_once(':') {
        let label = label.trim();
        if label.is_empty()
            || !label.chars().all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '.')
        {
            break;
        }
        code = rest.trim().to_string();
    }
    code
}

pub(crate) fn mnemonic(code: &str) -> &str {
    code.split(|c: char| c.is_ascii_whitespace() || c == ';').next().unwrap_or("")
}

pub(crate) fn stack_analysis(architecture: Architecture, line: &str) -> StackAnalysis {
    // This is deliberately conservative. Unknown writes reject an inline-asm
    // contract instead of pretending that stack balance can be proven.
    match architecture {
        Architecture::X86_64 => x86_64::stack_analysis(line),
        Architecture::Aarch64 => aarch64::stack_analysis(line),
        Architecture::LoongArch64 => loongarch64::stack_analysis(line),
        Architecture::Riscv64 => riscv64::stack_analysis(line),
        Architecture::Wasm32 | Architecture::Wasm64 => wasm::stack_analysis(line),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn register_aliases_and_widths_are_architecture_local() {
        assert_eq!(register_group(Architecture::X86_64, "%eax").as_deref(), None);
        assert_eq!(register_group(Architecture::X86_64, "eax").as_deref(), Some("rax"));
        assert_eq!(register_width_bits(Architecture::X86_64, "r8d"), Some(32));

        assert_eq!(register_group(Architecture::Aarch64, "fp").as_deref(), Some("x29"));
        assert_eq!(register_width_bits(Architecture::Aarch64, "w30"), Some(32));

        assert_eq!(register_group(Architecture::Riscv64, "a7").as_deref(), Some("x17"));
        assert_eq!(register_group(Architecture::Riscv64, "fp").as_deref(), Some("x8"));
        assert_eq!(register_width_bits(Architecture::Riscv64, "x31"), Some(64));
        assert_eq!(operand_register_group(Architecture::Riscv64, "fa0").as_deref(), Some("f10"));
        for reserved in ["zero", "x0", "sp", "x2", "gp", "x3", "tp", "x4"] {
            assert_eq!(
                operand_register_group(Architecture::Riscv64, reserved),
                None,
                "{} must not be accepted as a value operand",
                reserved
            );
        }

        assert_eq!(register_group(Architecture::LoongArch64, "a0").as_deref(), Some("r4"));
        assert_eq!(register_group(Architecture::LoongArch64, "fp").as_deref(), Some("r22"));
        assert_eq!(operand_register_group(Architecture::LoongArch64, "fa0").as_deref(), Some("f0"));
        for reserved in ["zero", "r0", "tp", "r2", "sp", "r3", "r21"] {
            assert_eq!(
                operand_register_group(Architecture::LoongArch64, reserved),
                None,
                "{} must not be accepted as a value operand",
                reserved
            );
        }

        assert_eq!(register_group(Architecture::Wasm32, "i32"), None);
        assert_eq!(register_group(Architecture::Wasm64, "i64"), None);
    }

    #[test]
    fn stack_contract_analysis_dispatches_by_architecture() {
        let x86 = stack_analysis(Architecture::X86_64, "sub rsp, 16");
        assert!(x86.touches_stack);
        assert_eq!(x86.unbalanced_delta, -16);

        let arm = stack_analysis(Architecture::Aarch64, "sub sp, sp, #16");
        assert!(arm.touches_stack);
        assert!(arm.unknown_stack_write);

        let riscv = stack_analysis(Architecture::Riscv64, "addi sp, sp, -16");
        assert!(riscv.touches_stack);
        assert!(riscv.unknown_stack_write);
        assert!(stack_analysis(Architecture::Riscv64, "jr a0").nonreturning_branch);
        assert!(stack_analysis(Architecture::Riscv64, "jalr x0, 0(a0)").nonreturning_branch);
        assert!(stack_analysis(Architecture::Riscv64, "jalr zero, 0(a0)").nonreturning_branch);

        let loongarch = stack_analysis(Architecture::LoongArch64, "addi.d sp, sp, -16");
        assert!(loongarch.touches_stack);
        assert!(loongarch.unknown_stack_write);
        assert!(stack_analysis(Architecture::LoongArch64, "jr a0").nonreturning_branch);
    }

    #[test]
    fn architecture_comment_rules_preserve_aarch64_immediates() {
        assert_eq!(instruction_text("add x0, x0, #1 // note", false), "add x0, x0, #1");
        assert_eq!(instruction_text("addi a0, a0, 1 # note", true), "addi a0, a0, 1");
    }
}
