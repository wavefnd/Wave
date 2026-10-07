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

//! Lowering for the legacy name-based assignment statement node.
//!
//! General lvalue assignment is represented as an expression and handled by
//! `expression::rvalue::assign`. This path accepts only a named mutable variable
//! and rejects constants.

use crate::codegen::abi_c::ExternCInfo;
use crate::codegen::types::TypeFlavor;
use crate::codegen::{wave_type_to_llvm_type, VariableInfo};
use crate::expression::rvalue::generate_expression_ir;
use crate::statement::variable::{coerce_basic_value, expression_is_unsigned, CoercionMode};
use hir::TypedProgram;
use inkwell::module::Module;
use inkwell::targets::TargetData;
use inkwell::types::{BasicTypeEnum, StructType};
use inkwell::values::BasicValueEnum;
use parser::ast::{Expression, Mutability};
use std::collections::HashMap;

pub(super) fn gen_assign_ir<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    variable: &str,
    value: &Expression,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
    target_data: &'ctx TargetData,
    extern_c_info: &HashMap<String, ExternCInfo<'ctx>>,
    program: &TypedProgram,
) {
    if variable == "deref" {
        panic!(
            "internal error: legacy StatementNode::Assign(\"deref\") reached codegen; parser must lower lvalue assignment to Expression::Assignment"
        );
    }

    let (dst_ptr, dst_mutability, dst_wave_ty) = {
        let info =
            variables.get(variable).unwrap_or_else(|| panic!("Variable {} not declared", variable));
        (info.ptr, info.mutability.clone(), info.ty.clone())
    };

    if matches!(dst_mutability, Mutability::Const) {
        panic!("Cannot assign to immutable variable '{}'", variable);
    }

    let element_type: BasicTypeEnum<'ctx> =
        wave_type_to_llvm_type(context, &dst_wave_ty, struct_types, TypeFlavor::Value);

    if matches!(value, Expression::Null)
        && !matches!(dst_wave_ty, parser::ast::WaveType::Pointer(_))
    {
        panic!(
            "null literal can only be assigned to ptr<T> (variable '{}': {:?})",
            variable, dst_wave_ty
        );
    }

    let val = generate_expression_ir(
        program,
        context,
        builder,
        value,
        variables,
        module,
        Some(element_type),
        global_consts,
        struct_types,
        struct_field_indices,
        target_data,
        extern_c_info,
    );

    let mut casted_val = val;

    if casted_val.get_type() != element_type {
        casted_val = coerce_basic_value(
            context,
            builder,
            casted_val,
            element_type,
            "assign_cast",
            CoercionMode::Implicit,
            expression_is_unsigned(program, value),
        );
    }

    builder.build_store(dst_ptr, casted_val).unwrap();
}
