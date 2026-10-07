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

//! x86-64 register aliases, inline-assembly clobbers, and stack effects.
//!
//! Register groups collapse partial-register spellings to their physical
//! register so constraint and clobber validation cannot allocate overlapping
//! operands independently.

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-x86"))]
pub(crate) const CPUS: &[&str] = &["generic", "x86-64", "x86-64-v2", "x86-64-v3"];
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-x86"))]
pub(crate) const FEATURES: &[&str] = &["sse2", "sse4.1", "avx", "avx2"];
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-x86"))]
pub(crate) const DEFAULT_CPU: &str = "generic";
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-x86"))]
pub(crate) const DEFAULT_FEATURES: &[&str] = &[];

pub(crate) fn register_group(token: &str) -> Option<String> {
    let group = match token {
        "al" | "ah" | "ax" | "eax" | "rax" => "rax",
        "bl" | "bh" | "bx" | "ebx" | "rbx" => "rbx",
        "cl" | "ch" | "cx" | "ecx" | "rcx" => "rcx",
        "dl" | "dh" | "dx" | "edx" | "rdx" => "rdx",
        "sil" | "si" | "esi" | "rsi" => "rsi",
        "dil" | "di" | "edi" | "rdi" => "rdi",
        "bpl" | "bp" | "ebp" | "rbp" => "rbp",
        "spl" | "sp" | "esp" | "rsp" => "rsp",
        "r8b" | "r8w" | "r8d" | "r8" => "r8",
        "r9b" | "r9w" | "r9d" | "r9" => "r9",
        "r10b" | "r10w" | "r10d" | "r10" => "r10",
        "r11b" | "r11w" | "r11d" | "r11" => "r11",
        "r12b" | "r12w" | "r12d" | "r12" => "r12",
        "r13b" | "r13w" | "r13d" | "r13" => "r13",
        "r14b" | "r14w" | "r14d" | "r14" => "r14",
        "r15b" | "r15w" | "r15d" | "r15" => "r15",
        _ => return None,
    };
    Some(group.to_string())
}

pub(crate) fn register_width_bits(token: &str) -> Option<u32> {
    match token {
        "al" | "bl" | "cl" | "dl" | "sil" | "dil" | "r8b" | "r9b" | "r10b" | "r11b" | "r12b"
        | "r13b" | "r14b" | "r15b" => Some(8),
        "ax" | "bx" | "cx" | "dx" | "si" | "di" | "r8w" | "r9w" | "r10w" | "r11w" | "r12w"
        | "r13w" | "r14w" | "r15w" => Some(16),
        "eax" | "ebx" | "ecx" | "edx" | "esi" | "edi" | "r8d" | "r9d" | "r10d" | "r11d"
        | "r12d" | "r13d" | "r14d" | "r15d" => Some(32),
        "rax" | "rbx" | "rcx" | "rdx" | "rsi" | "rdi" | "rbp" | "rsp" | "r8" | "r9" | "r10"
        | "r11" | "r12" | "r13" | "r14" | "r15" => Some(64),
        _ => None,
    }
}

pub(crate) fn operand_register_group(token: &str) -> Option<String> {
    let group = register_group(token)?;
    (group != "rsp").then_some(group)
}

pub(crate) fn default_clobbers() -> Vec<String> {
    ["~{memory}", "~{dirflag}", "~{fpsr}", "~{flags}"].into_iter().map(String::from).collect()
}

pub(crate) fn allocatable_registers() -> Vec<String> {
    ["rax", "rbx", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]
        .into_iter()
        .map(String::from)
        .collect()
}

pub(crate) fn normalize_special_clobber(token: &str) -> Option<String> {
    match token {
        "memory" => Some("~{memory}".to_string()),
        "cc" | "flags" | "eflags" | "rflags" => Some("~{flags}".to_string()),
        "dirflag" => Some("~{dirflag}".to_string()),
        "fpsr" => Some("~{fpsr}".to_string()),
        _ => None,
    }
}

fn parse_immediate(raw: &str) -> Option<i64> {
    let mut value =
        raw.trim().trim_start_matches('$').trim_start_matches('#').trim_end_matches(',');
    let negative = value.starts_with('-');
    if negative {
        value = value.trim_start_matches('-');
    }
    if let Some(hex) = value.strip_prefix("0x") {
        let parsed = i64::from_str_radix(hex, 16).ok()?;
        Some(if negative { -parsed } else { parsed })
    } else {
        let parsed = value.parse::<i64>().ok()?;
        Some(if negative { -parsed } else { parsed })
    }
}

fn stack_adjustment(code: &str) -> Option<i64> {
    let mut parts =
        code.split(|c: char| c.is_ascii_whitespace() || c == ',').filter(|part| !part.is_empty());
    let op = parts.next()?;
    let first = parts.next()?;
    let second = parts.next()?;
    let sp_is_second = matches!(second, "rsp" | "%rsp" | "esp" | "%esp" | "sp" | "%sp");
    let sp_is_first = matches!(first, "rsp" | "%rsp" | "esp" | "%esp" | "sp" | "%sp");
    match op {
        "sub" | "subq" | "subl" if sp_is_second => parse_immediate(first).map(|value| -value),
        "add" | "addq" | "addl" if sp_is_second => parse_immediate(first),
        "sub" | "subq" | "subl" if sp_is_first => parse_immediate(second).map(|value| -value),
        "add" | "addq" | "addl" if sp_is_first => parse_immediate(second),
        _ => None,
    }
}

fn jump_is_indirect(code: &str) -> bool {
    let operand = code
        .split(|c: char| c.is_ascii_whitespace() || c == ',')
        .filter(|part| !part.is_empty())
        .nth(1);
    let Some(operand) = operand else {
        return false;
    };
    let operand = operand.trim_start_matches('*').trim_start_matches('%');
    operand.starts_with('[') || register_group(operand).is_some()
}

pub(crate) fn stack_analysis(line: &str) -> super::StackAnalysis {
    let code = super::instruction_text(line, true);
    if code.is_empty() {
        return super::StackAnalysis::default();
    }
    let mnemonic = super::mnemonic(&code);
    let mut out = super::StackAnalysis::default();
    match mnemonic {
        "call" | "callq" => out.touches_stack = true,
        "push" | "pushq" => {
            out.touches_stack = true;
            out.unbalanced_delta = -8;
        },
        "pop" | "popq" | "ret" | "retq" => {
            out.touches_stack = true;
            out.unbalanced_delta = 8;
        },
        "retf" | "retfq" => {
            out.touches_stack = true;
            out.unbalanced_delta = 16;
        },
        "iret" | "iretq" | "leave" | "enter" => {
            out.touches_stack = true;
            out.unknown_stack_write = true;
        },
        "jmp" | "jmpq" => out.nonreturning_branch = jump_is_indirect(&code),
        _ => {
            if let Some(delta) = stack_adjustment(&code) {
                out.touches_stack = true;
                out.unbalanced_delta = delta;
            } else {
                let writes_sp = ["mov", "movq", "and", "andq", "xor", "lea"].iter().any(|op| {
                    code.starts_with(&format!("{} rsp", op))
                        || code.starts_with(&format!("{} %rsp", op))
                });
                if writes_sp {
                    out.touches_stack = true;
                    out.unknown_stack_write = true;
                } else {
                    out.touches_stack = code.contains("rsp")
                        || code.contains("esp")
                        || code.contains("[sp")
                        || code.contains(" sp,")
                        || code.contains(", sp");
                }
            }
        },
    }
    out
}
