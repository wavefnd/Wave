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

//! Inline-assembly expression lowering.
//!
//! [`AsmPlan`] performs target-specific validation before this module creates an
//! LLVM inline-asm value. Expressions may produce at most one value; statement
//! assembly handles the multi-output form separately.

use super::ExprGenEnv;
use crate::codegen::arch;
use crate::codegen::plan::*;
use crate::codegen::target::require_supported_target_from_module;
use crate::codegen::types::{wave_type_to_llvm_type, TypeFlavor};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{
    AsValueRef, BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue, ValueKind,
};
use parser::ast::{Expression, WaveType};

pub(crate) fn gen<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    instructions: &[String],
    inputs: &[(String, Expression)],
    outputs: &[(String, Expression)],
    clobbers: &[String],
) -> BasicValueEnum<'ctx> {
    let target = require_supported_target_from_module(env.module);
    let plan = AsmPlan::build(
        target,
        instructions,
        inputs,
        outputs,
        clobbers,
        AsmSafetyMode::ConservativeKernel,
    );
    let constraints_str = plan.constraints_string();

    if plan.noreturn {
        panic!("asm expression cannot declare clobber(\"noreturn\")");
    }

    let mut operand_vals: Vec<BasicMetadataValueEnum<'ctx>> = Vec::with_capacity(plan.inputs.len());
    for inp in &plan.inputs {
        let v = eval_asm_in_expr(env, inp.value);
        operand_vals.push(v.into());
    }

    let param_types: Vec<BasicMetadataTypeEnum<'ctx>> =
        operand_vals.iter().map(meta_val_type).collect();

    // void asm
    if plan.outputs.is_empty() {
        let fn_type = env.context.void_type().fn_type(&param_types, false);

        let inline_asm = env.context.create_inline_asm(
            fn_type,
            plan.asm_code.clone(),
            constraints_str,
            plan.has_side_effects,
            plan.align_stack,
            Some(arch::inline_asm_dialect(target.architecture())),
            false,
        );

        // SAFETY: `create_inline_asm` returns an LLVM value whose type is exactly
        // `fn_type`; wrapping that same value as a callee pointer preserves the
        // context and function signature used by `build_indirect_call` below.
        let callee = unsafe { PointerValue::new(inline_asm.as_value_ref()) };

        env.builder.build_indirect_call(fn_type, callee, &operand_vals, "inline_asm_void").unwrap();

        return env.context.i64_type().const_int(0, false).as_basic_value_enum();
    }

    // asm expr must have exactly 1 output
    if plan.outputs.len() != 1 {
        panic!("asm expression requires exactly 1 output (got {})", plan.outputs.len());
    }

    let out_ty = resolve_expr_out_type(env, plan.outputs[0].target);
    let fn_type = out_ty.fn_type(&param_types, false);

    let inline_asm = env.context.create_inline_asm(
        fn_type,
        plan.asm_code.clone(),
        constraints_str,
        plan.has_side_effects,
        plan.align_stack,
        Some(arch::inline_asm_dialect(target.architecture())),
        false,
    );

    // SAFETY: The inline-asm value was created in this context with `fn_type`,
    // which is also supplied to the indirect call immediately below.
    let callee = unsafe { PointerValue::new(inline_asm.as_value_ref()) };

    let call =
        env.builder.build_indirect_call(fn_type, callee, &operand_vals, "inline_asm_expr").unwrap();

    match call.try_as_basic_value() {
        ValueKind::Basic(v) => v,
        ValueKind::Instruction(_) => {
            panic!("inline asm expr expected to return a value, but got instruction-only result");
        },
    }
}

fn llvm_type_of_wave<'ctx, 'a>(env: &ExprGenEnv<'ctx, 'a>, wt: &WaveType) -> BasicTypeEnum<'ctx> {
    wave_type_to_llvm_type(env.context, wt, env.struct_types, TypeFlavor::Value)
}

fn resolve_expr_out_type<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    target: &Expression,
) -> BasicTypeEnum<'ctx> {
    llvm_type_of_wave(env, &env.wave_type(target).expect("asm output has a validated HIR type"))
}

fn eval_asm_in_expr<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    expression: &Expression,
) -> BasicValueEnum<'ctx> {
    let expected = env
        .wave_type(expression)
        .map(|ty| llvm_type_of_wave(env, &ty))
        .or_else(|| Some(env.context.i64_type().into()));
    env.gen(expression, expected)
}

fn meta_val_type<'ctx>(v: &BasicMetadataValueEnum<'ctx>) -> BasicMetadataTypeEnum<'ctx> {
    match v {
        BasicMetadataValueEnum::IntValue(iv) => iv.get_type().into(),
        BasicMetadataValueEnum::FloatValue(fv) => fv.get_type().into(),
        BasicMetadataValueEnum::PointerValue(pv) => pv.get_type().into(),
        BasicMetadataValueEnum::StructValue(sv) => sv.get_type().into(),
        BasicMetadataValueEnum::VectorValue(vv) => vv.get_type().into(),
        BasicMetadataValueEnum::ArrayValue(av) => av.get_type().into(),
        BasicMetadataValueEnum::ScalableVectorValue(svv) => svv.get_type().into(),
        BasicMetadataValueEnum::MetadataValue(_) => {
            panic!("MetadataValue cannot be used as an inline asm operand");
        },
    }
}
