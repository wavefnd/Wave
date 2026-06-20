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

// codegen/asm/plan.rs
use crate::codegen::target::CodegenTarget;
use parser::ast::Expression;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct AsmPlan<'a> {
    pub asm_code: String,

    // outputs are first in constraints
    pub outputs: Vec<AsmOut<'a>>,

    // inputs are call arguments (constraints after outputs)
    pub inputs: Vec<AsmIn<'a>>,

    // clobbers go last
    pub clobbers: Vec<String>,

    pub has_side_effects: bool,
    pub align_stack: bool,
    pub noreturn: bool,
}

#[derive(Debug, Clone)]
pub struct AsmOut<'a> {
    pub reg_raw: String,            // user wrote (e.g. "rax", "%rax", "RAX", "r")
    pub reg_norm: String,           // normalized token (e.g. "rax", "r")
    pub phys_group: Option<String>, // Some("rax") for real regs (al/ax/eax/rax -> rax), None for constraint classes (r/rm/m/..)
    pub target: &'a Expression,
}

#[derive(Debug, Clone)]
pub struct AsmIn<'a> {
    pub constraint: String,         // "{rax}" or "r" or "0" (tied)
    pub phys_group: Option<String>, // Some("rax") if it binds a concrete reg token, None if it is a class constraint
    pub value: &'a Expression,
}

#[derive(Debug, Clone, Copy)]
pub enum AsmSafetyMode {
    ConservativeKernel,
}

#[derive(Debug, Clone)]
struct RegToken {
    raw_norm: String,           // normalized token (no %, no braces, lowercase)
    phys_group: Option<String>, // physical register group for real regs
}

/// Normalize user reg/constraint token:
/// - trims spaces
/// - strips leading '%'
/// - strips surrounding '{...}' if user wrote them
/// - lowercase
fn normalize_token(s: &str) -> String {
    let s = s.trim();
    let s = s.trim_start_matches('%');

    let s = if let Some(inner) = s.strip_prefix('{').and_then(|x| x.strip_suffix('}')) {
        inner
    } else {
        s
    };

    s.trim().to_ascii_lowercase()
}

fn reg_phys_group_x86_64(token: &str) -> Option<&'static str> {
    match token {
        "al" | "ah" | "ax" | "eax" | "rax" => Some("rax"),
        "bl" | "bh" | "bx" | "ebx" | "rbx" => Some("rbx"),
        "cl" | "ch" | "cx" | "ecx" | "rcx" => Some("rcx"),
        "dl" | "dh" | "dx" | "edx" | "rdx" => Some("rdx"),
        "sil" | "si" | "esi" | "rsi" => Some("rsi"),
        "dil" | "di" | "edi" | "rdi" => Some("rdi"),
        "bpl" | "bp" | "ebp" | "rbp" => Some("rbp"),
        "spl" | "sp" | "esp" | "rsp" => Some("rsp"),
        "r8b" | "r8w" | "r8d" | "r8" => Some("r8"),
        "r9b" | "r9w" | "r9d" | "r9" => Some("r9"),
        "r10b" | "r10w" | "r10d" | "r10" => Some("r10"),
        "r11b" | "r11w" | "r11d" | "r11" => Some("r11"),
        "r12b" | "r12w" | "r12d" | "r12" => Some("r12"),
        "r13b" | "r13w" | "r13d" | "r13" => Some("r13"),
        "r14b" | "r14w" | "r14d" | "r14" => Some("r14"),
        "r15b" | "r15w" | "r15d" | "r15" => Some("r15"),

        _ => None,
    }
}

fn reg_phys_group_arm64(token: &str) -> Option<String> {
    match token {
        "fp" => return Some("x29".to_string()),
        "lr" => return Some("x30".to_string()),
        "ip0" => return Some("x16".to_string()),
        "ip1" => return Some("x17".to_string()),
        "sp" => return Some("sp".to_string()),
        "xzr" | "wzr" => return Some("xzr".to_string()),
        _ => {}
    }

    if token.len() >= 2 {
        let (prefix, num) = token.split_at(1);
        if (prefix == "x" || prefix == "w")
            && num.chars().all(|c| c.is_ascii_digit())
            && !num.is_empty()
        {
            if let Ok(n) = num.parse::<u32>() {
                if n <= 30 {
                    return Some(format!("x{}", n));
                }
            }
        }
    }

    None
}

fn reg_phys_group_riscv64(token: &str) -> Option<String> {
    match token {
        "zero" => return Some("x0".to_string()),
        "ra" => return Some("x1".to_string()),
        "sp" => return Some("x2".to_string()),
        "gp" => return Some("x3".to_string()),
        "tp" => return Some("x4".to_string()),
        "t0" => return Some("x5".to_string()),
        "t1" => return Some("x6".to_string()),
        "t2" => return Some("x7".to_string()),
        "s0" | "fp" => return Some("x8".to_string()),
        "s1" => return Some("x9".to_string()),
        "a0" => return Some("x10".to_string()),
        "a1" => return Some("x11".to_string()),
        "a2" => return Some("x12".to_string()),
        "a3" => return Some("x13".to_string()),
        "a4" => return Some("x14".to_string()),
        "a5" => return Some("x15".to_string()),
        "a6" => return Some("x16".to_string()),
        "a7" => return Some("x17".to_string()),
        "s2" => return Some("x18".to_string()),
        "s3" => return Some("x19".to_string()),
        "s4" => return Some("x20".to_string()),
        "s5" => return Some("x21".to_string()),
        "s6" => return Some("x22".to_string()),
        "s7" => return Some("x23".to_string()),
        "s8" => return Some("x24".to_string()),
        "s9" => return Some("x25".to_string()),
        "s10" => return Some("x26".to_string()),
        "s11" => return Some("x27".to_string()),
        "t3" => return Some("x28".to_string()),
        "t4" => return Some("x29".to_string()),
        "t5" => return Some("x30".to_string()),
        "t6" => return Some("x31".to_string()),
        _ => {}
    }

    if let Some(num) = token.strip_prefix('x') {
        if num.chars().all(|c| c.is_ascii_digit()) && !num.is_empty() {
            if let Ok(n) = num.parse::<u32>() {
                if n <= 31 {
                    return Some(format!("x{}", n));
                }
            }
        }
    }

    None
}

/// Decide whether user token is a real register or a constraint class.
fn parse_token(target: CodegenTarget, raw: &str) -> RegToken {
    let raw_norm = normalize_token(raw);
    let phys_group = match target {
        CodegenTarget::LinuxX86_64
        | CodegenTarget::DarwinX86_64
        | CodegenTarget::WindowsX86_64Gnu
        | CodegenTarget::FreestandingX86_64 => {
            reg_phys_group_x86_64(&raw_norm).map(|s| s.to_string())
        }
        CodegenTarget::LinuxArm64
        | CodegenTarget::DarwinArm64
        | CodegenTarget::FreestandingArm64 => reg_phys_group_arm64(&raw_norm),
        CodegenTarget::FreestandingRISCV64 => reg_phys_group_riscv64(&raw_norm),
    };
    RegToken {
        raw_norm,
        phys_group,
    }
}

fn is_valid_constraint_class(token: &str) -> bool {
    matches!(token, "r" | "m" | "rm" | "i" | "ri" | "im" | "irm")
}

/// For conservative kernel mode:
/// - ALWAYS clobber memory + flags-ish
/// - If ANY operand uses a *class constraint* (no concrete phys reg),
///   DO NOT auto-clobber GPRs (otherwise allocator can't satisfy "r"/"rm").
/// - If all operands are concrete regs only, you may clobber the rest GPRs safely.
fn build_default_clobbers(
    target: CodegenTarget,
    mode: AsmSafetyMode,
    inputs: &[(String, Expression)],
    outputs: &[(String, Expression)],
) -> Vec<String> {
    match mode {
        AsmSafetyMode::ConservativeKernel => {
            let mut clobbers = match target {
                CodegenTarget::LinuxX86_64
                | CodegenTarget::DarwinX86_64
                | CodegenTarget::WindowsX86_64Gnu
                | CodegenTarget::FreestandingX86_64 => vec![
                    "~{memory}".to_string(),
                    "~{dirflag}".to_string(),
                    "~{fpsr}".to_string(),
                    "~{flags}".to_string(),
                ],
                CodegenTarget::LinuxArm64
                | CodegenTarget::DarwinArm64
                | CodegenTarget::FreestandingArm64 => {
                    vec!["~{memory}".to_string(), "~{cc}".to_string()]
                }
                CodegenTarget::FreestandingRISCV64 => vec!["~{memory}".to_string()],
            };

            // Empty barrier-like asm blocks must not implicitly clobber every GPR.
            // Users can still declare explicit register clobbers when needed.
            if inputs.is_empty() && outputs.is_empty() {
                return clobbers;
            }

            // Collect concrete used physical register groups
            let mut used_phys: HashSet<String> = HashSet::new();
            let mut has_class_constraint = false;

            for (r, _) in inputs {
                let t = parse_token(target, r);
                if let Some(pg) = t.phys_group {
                    used_phys.insert(pg);
                } else {
                    has_class_constraint = true;
                }
            }
            for (r, _) in outputs {
                let t = parse_token(target, r);
                if let Some(pg) = t.phys_group {
                    used_phys.insert(pg);
                } else {
                    has_class_constraint = true;
                }
            }

            // If any class constraint exists, don't auto-clobber GPRs.
            if has_class_constraint {
                return clobbers;
            }

            match target {
                CodegenTarget::LinuxX86_64
                | CodegenTarget::DarwinX86_64
                | CodegenTarget::WindowsX86_64Gnu
                | CodegenTarget::FreestandingX86_64 => {
                    const GPRS: [&str; 14] = [
                        "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12",
                        "r13", "r14", "r15",
                    ];

                    for r in GPRS {
                        if !used_phys.contains(r) {
                            clobbers.push(format!("~{{{}}}", r));
                        }
                    }
                }
                CodegenTarget::LinuxArm64
                | CodegenTarget::DarwinArm64
                | CodegenTarget::FreestandingArm64 => {
                    for n in 0..=30u32 {
                        if n == 18 {
                            continue;
                        }
                        let r = format!("x{}", n);
                        if !used_phys.contains(&r) {
                            clobbers.push(format!("~{{{}}}", r));
                        }
                    }
                }
                CodegenTarget::FreestandingRISCV64 => {
                    for n in [
                        1u32, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
                        23, 24, 25, 26, 27, 28, 29, 30, 31,
                    ] {
                        let r = format!("x{}", n);
                        if !used_phys.contains(&r) {
                            clobbers.push(format!("~{{{}}}", r));
                        }
                    }
                }
            }

            clobbers
        }
    }
}

fn gcc_percent_to_llvm_dollar(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let bytes = s.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        if bytes[i] == b'%' {
            // "%%" -> literal '%'
            if i + 1 < bytes.len() && bytes[i + 1] == b'%' {
                out.push('%');
                i += 2;
                continue;
            }

            // "%123" -> "$123"
            let mut j = i + 1;
            if j < bytes.len() && bytes[j].is_ascii_digit() {
                out.push('$');
                while j < bytes.len() && bytes[j].is_ascii_digit() {
                    out.push(bytes[j] as char);
                    j += 1;
                }
                i = j;
                continue;
            }
        }

        out.push(bytes[i] as char);
        i += 1;
    }

    out
}

fn normalize_special_clobber(target: CodegenTarget, token: &str) -> Option<String> {
    match target {
        CodegenTarget::LinuxX86_64
        | CodegenTarget::DarwinX86_64
        | CodegenTarget::WindowsX86_64Gnu
        | CodegenTarget::FreestandingX86_64 => match token {
            "memory" => Some("~{memory}".to_string()),
            "cc" | "flags" | "eflags" | "rflags" => Some("~{flags}".to_string()),
            "dirflag" => Some("~{dirflag}".to_string()),
            "fpsr" => Some("~{fpsr}".to_string()),
            _ => None,
        },
        CodegenTarget::LinuxArm64
        | CodegenTarget::DarwinArm64
        | CodegenTarget::FreestandingArm64 => match token {
            "memory" => Some("~{memory}".to_string()),
            "cc" | "flags" | "eflags" | "rflags" => Some("~{cc}".to_string()),
            _ => None,
        },
        CodegenTarget::FreestandingRISCV64 => match token {
            "memory" => Some("~{memory}".to_string()),
            _ => None,
        },
    }
}

fn is_stack_pseudo_clobber(token: &str) -> bool {
    matches!(
        normalize_token(token).as_str(),
        "stack" | "uses_stack" | "uses-stack"
    )
}

fn is_nostack_pseudo_clobber(token: &str) -> bool {
    matches!(
        normalize_token(token).as_str(),
        "nostack" | "no_stack" | "no-stack"
    )
}

fn is_noreturn_pseudo_clobber(token: &str) -> bool {
    matches!(
        normalize_token(token).as_str(),
        "noreturn" | "no_return" | "no-return"
    )
}

fn normalize_clobber_item(target: CodegenTarget, s: &str) -> String {
    let t = s.trim();

    if let Some(inner) = t.strip_prefix("~{").and_then(|x| x.strip_suffix('}')) {
        let n = normalize_token(inner);

        if let Some(special) = normalize_special_clobber(target, &n) {
            return special;
        }

        let reg = parse_token(target, &n);
        if let Some(pg) = reg.phys_group {
            return format!("~{{{}}}", pg);
        }

        panic!("Invalid clobber token: '{}'", inner);
    }

    if let Some(inner) = t.strip_prefix('{').and_then(|x| x.strip_suffix('}')) {
        let n = normalize_token(inner);

        if let Some(special) = normalize_special_clobber(target, &n) {
            return special;
        }

        let reg = parse_token(target, &n);
        if let Some(pg) = reg.phys_group {
            return format!("~{{{}}}", pg);
        }

        panic!("Invalid clobber token: '{}'", inner);
    }

    // specials (plain)
    let lower = t.to_ascii_lowercase();
    if let Some(special) = normalize_special_clobber(target, &lower) {
        return special;
    }

    let rt = parse_token(target, t);
    if let Some(pg) = rt.phys_group {
        return format!("~{{{}}}", pg);
    }

    panic!("Invalid clobber token: '{}'", t);
}

fn merge_clobbers(
    target: CodegenTarget,
    mut base: Vec<String>,
    user: &[String],
    used_phys: &HashSet<String>,
) -> Vec<String> {
    let mut seen: HashSet<String> = base.iter().cloned().collect();

    for raw in user {
        if is_stack_pseudo_clobber(raw)
            || is_nostack_pseudo_clobber(raw)
            || is_noreturn_pseudo_clobber(raw)
        {
            continue;
        }

        let c = normalize_clobber_item(target, raw);

        if let Some(inner) = c.strip_prefix("~{").and_then(|x| x.strip_suffix('}')) {
            let inner_norm = normalize_token(inner);
            if used_phys.contains(&inner_norm) {
                panic!(
                    "clobber '{}' conflicts with an input/output operand register",
                    raw
                );
            }
        }

        if seen.insert(c.clone()) {
            base.push(c);
        }
    }

    base
}

#[derive(Debug, Clone, Copy)]
struct StackContract {
    stack_declared: bool,
    nostack_declared: bool,
    noreturn_declared: bool,
}

fn stack_contract_from_user_clobbers(user: &[String]) -> StackContract {
    let mut stack_declared = false;
    let mut nostack_declared = false;
    let mut noreturn_declared = false;

    for item in user {
        if is_stack_pseudo_clobber(item) {
            stack_declared = true;
        }
        if is_nostack_pseudo_clobber(item) {
            nostack_declared = true;
        }
        if is_noreturn_pseudo_clobber(item) {
            noreturn_declared = true;
        }
    }

    if stack_declared && nostack_declared {
        panic!("asm cannot declare both clobber(\"stack\") and clobber(\"nostack\")");
    }

    StackContract {
        stack_declared,
        nostack_declared,
        noreturn_declared,
    }
}

fn strip_inline_asm_comment(line: &str) -> &str {
    line.split_once("//")
        .map(|(code, _)| code)
        .unwrap_or(line)
        .split_once('#')
        .map(|(code, _)| code)
        .unwrap_or(line)
}

fn asm_instruction_text(line: &str) -> String {
    let mut code = strip_inline_asm_comment(line).trim().to_ascii_lowercase();

    while let Some((label, rest)) = code.split_once(':') {
        let label = label.trim();
        if label.is_empty()
            || !label
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '.')
        {
            break;
        }

        code = rest.trim().to_string();
    }

    code
}

fn asm_mnemonic(code: &str) -> &str {
    code.split(|c: char| c.is_ascii_whitespace() || c == ';')
        .next()
        .unwrap_or("")
}

fn parse_x86_imm(raw: &str) -> Option<i64> {
    let mut s = raw.trim();
    s = s.trim_start_matches('$');
    s = s.trim_start_matches('#');
    s = s.trim_end_matches(',');

    if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("-0x")) {
        let negative = s.starts_with('-');
        let value = i64::from_str_radix(hex, 16).ok()?;
        return Some(if negative { -value } else { value });
    }

    s.parse::<i64>().ok()
}

fn parse_x86_rsp_adjustment(code: &str) -> Option<i64> {
    let mut parts = code
        .split(|c: char| c.is_ascii_whitespace() || c == ',')
        .filter(|p| !p.is_empty());

    let op = parts.next()?;
    let first = parts.next()?;
    let second = parts.next()?;

    let sp_is_second = matches!(second, "rsp" | "%rsp" | "esp" | "%esp" | "sp" | "%sp");
    let sp_is_first = matches!(first, "rsp" | "%rsp" | "esp" | "%esp" | "sp" | "%sp");

    match op {
        "sub" | "subq" | "subl" if sp_is_second => parse_x86_imm(first).map(|v| -v),
        "add" | "addq" | "addl" if sp_is_second => parse_x86_imm(first),
        "sub" | "subq" | "subl" if sp_is_first => parse_x86_imm(second).map(|v| -v),
        "add" | "addq" | "addl" if sp_is_first => parse_x86_imm(second),
        _ => None,
    }
}

fn x86_jmp_operand_is_indirect(code: &str) -> bool {
    let mut parts = code
        .split(|c: char| c.is_ascii_whitespace() || c == ',')
        .filter(|p| !p.is_empty());

    let _op = parts.next();
    let Some(operand) = parts.next() else {
        return false;
    };
    let operand = operand.trim_start_matches('*').trim_start_matches('%');

    operand.starts_with('[')
        || matches!(
            reg_phys_group_x86_64(operand),
            Some(
                "rax"
                    | "rbx"
                    | "rcx"
                    | "rdx"
                    | "rsi"
                    | "rdi"
                    | "rbp"
                    | "rsp"
                    | "r8"
                    | "r9"
                    | "r10"
                    | "r11"
                    | "r12"
                    | "r13"
                    | "r14"
                    | "r15"
            )
        )
}

#[derive(Debug, Default, Clone, Copy)]
struct StackAnalysis {
    touches_stack: bool,
    unknown_stack_write: bool,
    unbalanced_delta: i64,
    nonreturning_branch: bool,
}

fn x86_64_stack_analysis(line: &str) -> StackAnalysis {
    let code = asm_instruction_text(line);
    if code.is_empty() {
        return StackAnalysis::default();
    }

    let first = asm_mnemonic(&code);
    let mut out = StackAnalysis::default();

    match first {
        "call" | "callq" => {
            out.touches_stack = true;
            return out;
        }
        "push" | "pushq" => {
            out.touches_stack = true;
            out.unbalanced_delta = -8;
            return out;
        }
        "pop" | "popq" => {
            out.touches_stack = true;
            out.unbalanced_delta = 8;
            return out;
        }
        "ret" | "retq" => {
            out.touches_stack = true;
            out.unbalanced_delta = 8;
            return out;
        }
        "retf" | "retfq" => {
            out.touches_stack = true;
            out.unbalanced_delta = 16;
            return out;
        }
        "iret" | "iretq" => {
            out.touches_stack = true;
            out.unknown_stack_write = true;
            return out;
        }
        "leave" | "enter" => {
            out.touches_stack = true;
            out.unknown_stack_write = true;
            return out;
        }
        "jmp" | "jmpq" => {
            out.nonreturning_branch = x86_jmp_operand_is_indirect(&code);
            return out;
        }
        _ => {}
    }

    if let Some(delta) = parse_x86_rsp_adjustment(&code) {
        out.touches_stack = true;
        out.unbalanced_delta = delta;
        return out;
    }

    let writes_sp = code.starts_with("mov rsp")
        || code.starts_with("movq rsp")
        || code.starts_with("mov %rsp")
        || code.starts_with("movq %rsp")
        || code.starts_with("and rsp")
        || code.starts_with("andq rsp")
        || code.starts_with("and %rsp")
        || code.starts_with("andq %rsp")
        || code.starts_with("xor rsp")
        || code.starts_with("xor %rsp")
        || code.starts_with("lea rsp")
        || code.starts_with("lea %rsp");

    if writes_sp {
        out.touches_stack = true;
        out.unknown_stack_write = true;
        return out;
    }

    out.touches_stack = code.contains("rsp")
        || code.contains("esp")
        || code.contains("[sp")
        || code.contains(" sp,")
        || code.contains(", sp");
    out
}

fn aarch64_stack_analysis(line: &str) -> StackAnalysis {
    let code = asm_instruction_text(line);
    if code.is_empty() {
        return StackAnalysis::default();
    }

    let mut out = StackAnalysis::default();
    let first = asm_mnemonic(&code);

    out.nonreturning_branch = matches!(first, "br");
    out.touches_stack = code == "ret"
        || code.starts_with("ret ")
        || code.starts_with("bl ")
        || code.starts_with("blr ")
        || code.contains(" sp,")
        || code.contains(", sp")
        || code.contains("[sp");
    if out.touches_stack && code.contains(" sp,") {
        out.unknown_stack_write = true;
    }
    out
}

fn riscv64_stack_analysis(line: &str) -> StackAnalysis {
    let code = asm_instruction_text(line);
    if code.is_empty() {
        return StackAnalysis::default();
    }

    let mut out = StackAnalysis::default();
    let first = asm_mnemonic(&code);

    out.nonreturning_branch = matches!(first, "jr");
    out.touches_stack = code == "ret"
        || code.starts_with("call ")
        || code.starts_with("jal ")
        || code.starts_with("jalr ")
        || code.contains(" sp,")
        || code.contains(", sp")
        || code.contains("(sp)");
    if out.touches_stack && (code.contains(" sp,") || code.starts_with("addi sp")) {
        out.unknown_stack_write = true;
    }
    out
}

fn asm_stack_analysis(target: CodegenTarget, instructions: &[String]) -> StackAnalysis {
    let mut total = StackAnalysis::default();

    for line in instructions {
        let item = match target {
            CodegenTarget::LinuxX86_64
            | CodegenTarget::DarwinX86_64
            | CodegenTarget::WindowsX86_64Gnu
            | CodegenTarget::FreestandingX86_64 => x86_64_stack_analysis(line),
            CodegenTarget::LinuxArm64
            | CodegenTarget::DarwinArm64
            | CodegenTarget::FreestandingArm64 => aarch64_stack_analysis(line),
            CodegenTarget::FreestandingRISCV64 => riscv64_stack_analysis(line),
        };

        total.touches_stack |= item.touches_stack;
        total.unknown_stack_write |= item.unknown_stack_write;
        total.nonreturning_branch |= item.nonreturning_branch;
        total.unbalanced_delta += item.unbalanced_delta;
    }

    total
}

fn validate_stack_contract(
    target: CodegenTarget,
    instructions: &[String],
    contract: StackContract,
) {
    let analysis = asm_stack_analysis(target, instructions);

    if analysis.touches_stack && !contract.stack_declared {
        panic!(
            "asm touches the stack or performs a call/return; declare clobber(\"stack\") to make the stack contract explicit"
        );
    }

    if analysis.touches_stack && contract.nostack_declared {
        panic!("asm declares clobber(\"nostack\") but touches the stack or performs a call/return");
    }

    if analysis.nonreturning_branch && !contract.noreturn_declared {
        panic!(
            "asm contains a non-returning branch; declare clobber(\"noreturn\") so codegen can terminate the block explicitly"
        );
    }

    if analysis.unknown_stack_write && !contract.noreturn_declared {
        panic!(
            "asm writes the stack pointer in a way codegen cannot prove balanced; restore the original stack pointer or declare clobber(\"noreturn\")"
        );
    }

    if analysis.unbalanced_delta != 0 && !contract.noreturn_declared {
        panic!(
            "asm stack delta is not balanced ({} bytes); restore the stack pointer or declare clobber(\"noreturn\")",
            analysis.unbalanced_delta
        );
    }
}

impl<'a> AsmPlan<'a> {
    pub fn build(
        target: CodegenTarget,
        instructions: &'a [String],
        inputs_raw: &'a [(String, Expression)],
        outputs_raw: &'a [(String, Expression)],
        user_clobbers_raw: &'a [String],
        mode: AsmSafetyMode,
    ) -> Self {
        let asm_code = instructions.join("\n");
        let asm_code = gcc_percent_to_llvm_dollar(&asm_code);
        let stack_contract = stack_contract_from_user_clobbers(user_clobbers_raw);
        validate_stack_contract(target, instructions, stack_contract);

        // outputs
        let mut used_out_phys: HashSet<String> = HashSet::new();
        let mut out_index_by_exact_reg: HashMap<String, usize> = HashMap::new();
        let mut outputs: Vec<AsmOut<'a>> = Vec::with_capacity(outputs_raw.len());

        for (reg, out_target) in outputs_raw {
            let t = parse_token(target, reg);

            if t.phys_group.is_none() && !is_valid_constraint_class(&t.raw_norm) {
                panic!(
                    "asm output register/constraint '{}' is not valid for target {:?}",
                    reg, target
                );
            }

            // real reg outputs: disallow duplicates by physical group
            if let Some(pg) = &t.phys_group {
                if !used_out_phys.insert(pg.clone()) {
                    panic!(
                        "Register '{}' duplicated in asm outputs (same phys group '{}')",
                        reg, pg
                    );
                }
                // enable tied input only when exact same token used (ex: out("rax") + in("rax"))
                out_index_by_exact_reg.insert(t.raw_norm.clone(), outputs.len());
            }
            // class constraints (r/rm/m/...) -> allow duplicates

            let reg_norm = t.phys_group.clone().unwrap_or_else(|| t.raw_norm.clone());
            outputs.push(AsmOut {
                reg_raw: reg.clone(),
                reg_norm,
                phys_group: t.phys_group,
                target: out_target,
            });
        }

        // inputs
        let mut used_in_phys: HashSet<String> = HashSet::new();
        let mut inputs: Vec<AsmIn<'a>> = Vec::with_capacity(inputs_raw.len());

        for (reg, expr) in inputs_raw {
            let t = parse_token(target, reg);

            if t.phys_group.is_none() && !is_valid_constraint_class(&t.raw_norm) {
                panic!(
                    "asm input register/constraint '{}' is not valid for target {:?}",
                    reg, target
                );
            }

            // real reg inputs: disallow duplicates by physical group
            if let Some(pg) = &t.phys_group {
                if !used_in_phys.insert(pg.clone()) {
                    panic!(
                        "Register '{}' duplicated in asm inputs (same phys group '{}')",
                        reg, pg
                    );
                }

                // tied only when exact same reg token matches a real-reg output token
                if let Some(&out_idx) = out_index_by_exact_reg.get(&t.raw_norm) {
                    inputs.push(AsmIn {
                        constraint: out_idx.to_string(), // "0", "1", ...
                        phys_group: Some(pg.clone()),
                        value: expr,
                    });
                    continue;
                }

                inputs.push(AsmIn {
                    constraint: format!("{{{}}}", pg), // "{rax}", "{dl}", "{r8d}", ...
                    phys_group: Some(pg.clone()),
                    value: expr,
                });
                continue;
            }

            // class constraint: allow duplicates, pass through as-is
            inputs.push(AsmIn {
                constraint: t.raw_norm, // "r", "rm", "m", "i", ...
                phys_group: None,
                value: expr,
            });
        }

        let mut used_phys: HashSet<String> = HashSet::new();
        for o in &outputs {
            if let Some(pg) = &o.phys_group {
                used_phys.insert(pg.clone());
            }
        }
        for i in &inputs {
            if let Some(pg) = &i.phys_group {
                used_phys.insert(pg.clone());
            }
        }

        let default_clobbers = build_default_clobbers(target, mode, inputs_raw, outputs_raw);
        let clobbers = merge_clobbers(target, default_clobbers, user_clobbers_raw, &used_phys);

        Self {
            asm_code,
            outputs,
            inputs,
            clobbers,
            has_side_effects: true,
            align_stack: stack_contract.stack_declared,
            noreturn: stack_contract.noreturn_declared,
        }
    }

    pub fn constraints_string(&self) -> String {
        let mut parts: Vec<String> = Vec::new();

        // outputs first
        for o in &self.outputs {
            if o.phys_group.is_some() {
                // concrete register
                parts.push(format!("={{{}}}", o.reg_norm)); // "={rax}", "={dl}", ...
            } else {
                // class constraint
                parts.push(format!("={}", o.reg_norm)); // "=r", "=m", ...
            }
        }

        // inputs next
        for i in &self.inputs {
            parts.push(i.constraint.clone()); // "{rsi}" or "r" or "0" ...
        }

        // clobbers last
        for c in &self.clobbers {
            parts.push(c.clone());
        }

        parts.join(",")
    }
}
