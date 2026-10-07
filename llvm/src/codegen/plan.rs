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

//! Architecture-aware validation and normalization of inline assembly.
//!
//! An [`AsmPlan`] is the checked boundary between source syntax and LLVM inline
//! assembly. It normalizes register aliases, orders constraints, detects
//! conflicting operands and clobbers, and verifies stack/noreturn declarations
//! against conservative instruction analysis.
use crate::codegen::arch;
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

/// Decide whether user token is a real register or a constraint class.
fn parse_token(target: CodegenTarget, raw: &str) -> RegToken {
    let raw_norm = normalize_token(raw);
    let phys_group = arch::operand_register_group(target.architecture(), &raw_norm);
    RegToken { raw_norm, phys_group }
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
            let mut clobbers = arch::default_clobbers(target.architecture());

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

            for register in arch::allocatable_registers(target.architecture()) {
                if !used_phys.contains(&register) {
                    clobbers.push(format!("~{{{}}}", register));
                }
            }

            clobbers
        },
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
    arch::normalize_special_clobber(target.architecture(), token)
}

fn is_stack_pseudo_clobber(token: &str) -> bool {
    matches!(normalize_token(token).as_str(), "stack" | "uses_stack" | "uses-stack")
}

fn is_nostack_pseudo_clobber(token: &str) -> bool {
    matches!(normalize_token(token).as_str(), "nostack" | "no_stack" | "no-stack")
}

fn is_noreturn_pseudo_clobber(token: &str) -> bool {
    matches!(normalize_token(token).as_str(), "noreturn" | "no_return" | "no-return")
}

fn normalize_clobber_item(target: CodegenTarget, s: &str) -> Result<String, String> {
    let t = s.trim();

    if let Some(inner) = t.strip_prefix("~{").and_then(|x| x.strip_suffix('}')) {
        let n = normalize_token(inner);

        if let Some(special) = normalize_special_clobber(target, &n) {
            return Ok(special);
        }

        if let Some(pg) = arch::register_group(target.architecture(), &n) {
            return Ok(format!("~{{{}}}", pg));
        }

        return Err(format!("Invalid clobber token: '{}'", inner));
    }

    if let Some(inner) = t.strip_prefix('{').and_then(|x| x.strip_suffix('}')) {
        let n = normalize_token(inner);

        if let Some(special) = normalize_special_clobber(target, &n) {
            return Ok(special);
        }

        if let Some(pg) = arch::register_group(target.architecture(), &n) {
            return Ok(format!("~{{{}}}", pg));
        }

        return Err(format!("Invalid clobber token: '{}'", inner));
    }

    // specials (plain)
    let lower = t.to_ascii_lowercase();
    if let Some(special) = normalize_special_clobber(target, &lower) {
        return Ok(special);
    }

    if let Some(pg) = arch::register_group(target.architecture(), &normalize_token(t)) {
        return Ok(format!("~{{{}}}", pg));
    }

    return Err(format!("Invalid clobber token: '{}'", t));
}

fn merge_clobbers(
    target: CodegenTarget,
    mut base: Vec<String>,
    user: &[String],
    used_phys: &HashSet<String>,
) -> Result<Vec<String>, String> {
    let mut seen: HashSet<String> = base.iter().cloned().collect();

    for raw in user {
        if is_stack_pseudo_clobber(raw)
            || is_nostack_pseudo_clobber(raw)
            || is_noreturn_pseudo_clobber(raw)
        {
            continue;
        }

        let c = normalize_clobber_item(target, raw)?;

        if let Some(inner) = c.strip_prefix("~{").and_then(|x| x.strip_suffix('}')) {
            let inner_norm = normalize_token(inner);
            if used_phys.contains(&inner_norm) {
                return Err(format!(
                    "clobber '{}' conflicts with an input/output operand register",
                    raw
                ));
            }
        }

        if seen.insert(c.clone()) {
            base.push(c);
        }
    }

    Ok(base)
}

#[derive(Debug, Clone, Copy)]
struct StackContract {
    stack_declared: bool,
    nostack_declared: bool,
    noreturn_declared: bool,
}

fn stack_contract_from_user_clobbers(user: &[String]) -> Result<StackContract, String> {
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
        return Err(
            "asm cannot declare both clobber(\"stack\") and clobber(\"nostack\")".to_string()
        );
    }

    Ok(StackContract { stack_declared, nostack_declared, noreturn_declared })
}

fn asm_stack_analysis(target: CodegenTarget, instructions: &[String]) -> arch::StackAnalysis {
    let mut total = arch::StackAnalysis::default();

    for line in instructions {
        let item = arch::stack_analysis(target.architecture(), line);

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
) -> Result<(), String> {
    let analysis = asm_stack_analysis(target, instructions);

    if analysis.touches_stack && !contract.stack_declared {
        return Err("asm touches the stack or performs a call/return; declare clobber(\"stack\") to make the stack contract explicit".to_string());
    }

    if analysis.touches_stack && contract.nostack_declared {
        return Err(
            "asm declares clobber(\"nostack\") but touches the stack or performs a call/return"
                .to_string(),
        );
    }

    if analysis.nonreturning_branch && !contract.noreturn_declared {
        return Err("asm contains a non-returning branch; declare clobber(\"noreturn\") so codegen can terminate the block explicitly".to_string());
    }

    if analysis.unknown_stack_write && !contract.noreturn_declared {
        return Err("asm writes the stack pointer in a way codegen cannot prove balanced; restore the original stack pointer or declare clobber(\"noreturn\")".to_string());
    }

    if analysis.unbalanced_delta != 0 && !contract.noreturn_declared {
        return Err(format!(
            "asm stack delta is not balanced ({} bytes); restore the stack pointer or declare clobber(\"noreturn\")",
            analysis.unbalanced_delta
        ));
    }
    Ok(())
}

impl<'a> AsmPlan<'a> {
    /// Used only after the public emission boundary validates every asm block.
    pub fn build(
        target: CodegenTarget,
        instructions: &'a [String],
        inputs: &'a [(String, Expression)],
        outputs: &'a [(String, Expression)],
        clobbers: &'a [String],
        mode: AsmSafetyMode,
    ) -> Self {
        Self::try_build(target, instructions, inputs, outputs, clobbers, mode)
            .expect("asm contract was validated before LLVM lowering")
    }

    pub fn try_build(
        target: CodegenTarget,
        instructions: &'a [String],
        inputs_raw: &'a [(String, Expression)],
        outputs_raw: &'a [(String, Expression)],
        user_clobbers_raw: &'a [String],
        mode: AsmSafetyMode,
    ) -> Result<Self, String> {
        if matches!(
            target,
            CodegenTarget::Wasm32Unknown
                | CodegenTarget::Wasm32WasiP1
                | CodegenTarget::Wasm64Unknown
        ) {
            return Err(format!(
                "inline assembly is not supported for {}; use a WebAssembly host import instead",
                target.desc()
            ));
        }

        let asm_code = instructions.join("\n");
        // LoongArch assembly spells physical registers with a leading `$`.
        // LLVM uses the same character for inline-asm operand substitutions,
        // so source-level register sigils must be escaped before `%0`-style
        // placeholders are normalized below.
        let asm_code = if target.architecture() == arch::Architecture::LoongArch64 {
            asm_code.replace('$', "$$")
        } else {
            asm_code
        };
        let asm_code = gcc_percent_to_llvm_dollar(&asm_code);
        let stack_contract = stack_contract_from_user_clobbers(user_clobbers_raw)?;
        validate_stack_contract(target, instructions, stack_contract)?;

        // outputs
        let mut used_out_phys: HashSet<String> = HashSet::new();
        let mut out_index_by_exact_reg: HashMap<String, usize> = HashMap::new();
        let mut outputs: Vec<AsmOut<'a>> = Vec::with_capacity(outputs_raw.len());

        for (reg, out_target) in outputs_raw {
            let t = parse_token(target, reg);

            if t.phys_group.is_none() && !is_valid_constraint_class(&t.raw_norm) {
                return Err(format!(
                    "asm output register/constraint '{}' is not valid for target {:?}",
                    reg, target
                ));
            }

            // real reg outputs: disallow duplicates by physical group
            if let Some(pg) = &t.phys_group {
                if !used_out_phys.insert(pg.clone()) {
                    return Err(format!(
                        "Register '{}' duplicated in asm outputs (same phys group '{}')",
                        reg, pg
                    ));
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
                return Err(format!(
                    "asm input register/constraint '{}' is not valid for target {:?}",
                    reg, target
                ));
            }

            // real reg inputs: disallow duplicates by physical group
            if let Some(pg) = &t.phys_group {
                if !used_in_phys.insert(pg.clone()) {
                    return Err(format!(
                        "Register '{}' duplicated in asm inputs (same phys group '{}')",
                        reg, pg
                    ));
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
        let clobbers = merge_clobbers(target, default_clobbers, user_clobbers_raw, &used_phys)?;

        Ok(Self {
            asm_code,
            outputs,
            inputs,
            clobbers,
            has_side_effects: true,
            align_stack: stack_contract.stack_declared,
            noreturn: stack_contract.noreturn_declared,
        })
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
