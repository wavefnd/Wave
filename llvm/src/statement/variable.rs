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

//! Local variable storage, initialization, and value coercion.
//!
//! Allocations are placed in the function entry block even when declarations
//! appear in nested control flow. Coercion behavior is selected explicitly for
//! implicit assignment, explicit casts, and inline-assembly operands.

use crate::codegen::abi_c::ExternCInfo;
use crate::codegen::types::TypeFlavor;
use crate::codegen::{wave_type_to_llvm_type, VariableInfo};
use crate::expression::rvalue::generate_expression_ir;

use inkwell::module::Module;
use inkwell::targets::TargetData;
use inkwell::types::{BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, PointerValue};

use hir::{HirExpressionType, TypedProgram};
use parser::ast::{Expression, VariableNode, WaveType};

use std::collections::HashMap;

#[derive(Copy, Clone, Debug)]
pub enum CoercionMode {
    Implicit,
    Explicit,
    Asm,
    Abi,
}

pub(crate) fn wave_type_is_unsigned(ty: Option<&WaveType>) -> bool {
    matches!(ty, Some(WaveType::Uint(_) | WaveType::Bool | WaveType::Byte | WaveType::Char))
}

pub(crate) fn expression_is_unsigned(program: &TypedProgram, expr: &Expression) -> bool {
    match program.type_of(expr) {
        Some(HirExpressionType::Resolved(ty)) => wave_type_is_unsigned(Some(ty)),
        _ => false,
    }
}

pub fn coerce_basic_value<'ctx>(
    _context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    val: BasicValueEnum<'ctx>,
    expected: BasicTypeEnum<'ctx>,
    tag: &str,
    mode: CoercionMode,
    source_unsigned: bool,
) -> BasicValueEnum<'ctx> {
    if val.get_type() == expected {
        return val;
    }

    if matches!(mode, CoercionMode::Implicit) {
        if let (BasicValueEnum::IntValue(value), BasicTypeEnum::IntType(target)) = (val, expected) {
            if value.get_type().get_bit_width() == 1 && target.get_bit_width() == 8 {
                return builder.build_int_z_extend(value, target, tag).unwrap().into();
            }
        }
        if matches!(val, BasicValueEnum::IntValue(_) | BasicValueEnum::FloatValue(_))
            && matches!(expected, BasicTypeEnum::IntType(_) | BasicTypeEnum::FloatType(_))
        {
            panic!("ICE: numeric boundary conversion missing from HIR: {tag}");
        }
    }

    match (val, expected) {
        // int <-> int
        (BasicValueEnum::IntValue(iv), BasicTypeEnum::IntType(dst)) => {
            let src_bw = iv.get_type().get_bit_width();
            let dst_bw = dst.get_bit_width();

            if src_bw == dst_bw {
                iv.as_basic_value_enum()
            } else if src_bw > dst_bw {
                match mode {
                    CoercionMode::Implicit => {
                        panic!(
                            "implicit integer narrowing is forbidden: i{} -> i{}",
                            src_bw, dst_bw
                        );
                    },
                    CoercionMode::Asm | CoercionMode::Explicit | CoercionMode::Abi => {
                        builder.build_int_truncate(iv, dst, tag).unwrap().as_basic_value_enum()
                    },
                }
            } else if src_bw == 1 || source_unsigned {
                builder.build_int_z_extend(iv, dst, tag).unwrap().as_basic_value_enum()
            } else {
                builder.build_int_s_extend(iv, dst, tag).unwrap().as_basic_value_enum()
            }
        },

        // float <-> float
        (BasicValueEnum::FloatValue(fv), BasicTypeEnum::FloatType(dst)) => {
            builder.build_float_cast(fv, dst, tag).unwrap().as_basic_value_enum()
        },

        // float -> int
        (BasicValueEnum::FloatValue(fv), BasicTypeEnum::IntType(dst)) => {
            builder.build_float_to_signed_int(fv, dst, tag).unwrap().as_basic_value_enum()
        },

        // int -> float
        (BasicValueEnum::IntValue(iv), BasicTypeEnum::FloatType(dst)) => {
            if source_unsigned {
                builder.build_unsigned_int_to_float(iv, dst, tag).unwrap().as_basic_value_enum()
            } else {
                builder.build_signed_int_to_float(iv, dst, tag).unwrap().as_basic_value_enum()
            }
        },

        // ptr -> ptr (bitcast)
        (BasicValueEnum::PointerValue(pv), BasicTypeEnum::PointerType(dst)) => builder
            .build_bit_cast(pv.as_basic_value_enum(), dst.as_basic_type_enum(), tag)
            .unwrap()
            .as_basic_value_enum(),

        // int -> ptr
        (BasicValueEnum::IntValue(iv), BasicTypeEnum::PointerType(dst)) => match mode {
            CoercionMode::Implicit => {
                if iv.is_const() && iv.get_zero_extended_constant() == Some(0) {
                    dst.const_null().as_basic_value_enum()
                } else {
                    panic!("Implicit int->ptr is not allowed (use explicit cast).");
                }
            },
            CoercionMode::Asm | CoercionMode::Explicit | CoercionMode::Abi => {
                builder.build_int_to_ptr(iv, dst, tag).unwrap().as_basic_value_enum()
            },
        },

        // ptr -> int
        (BasicValueEnum::PointerValue(pv), BasicTypeEnum::IntType(dst)) => match mode {
            CoercionMode::Implicit => {
                panic!("Implicit ptr->int is not allowed during '{}' (use explicit cast).", tag);
            },
            CoercionMode::Asm | CoercionMode::Explicit | CoercionMode::Abi => {
                builder.build_ptr_to_int(pv, dst, tag).unwrap().as_basic_value_enum()
            },
        },

        _ => {
            panic!("Type mismatch: expected {:?}, got {:?}", expected, val.get_type());
        },
    }
}

pub(super) fn gen_variable_ir<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    var_node: &VariableNode,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
    target_data: &'ctx TargetData,
    extern_c_info: &HashMap<String, ExternCInfo<'ctx>>,
    program: &TypedProgram,
) {
    let VariableNode { name, type_name, initial_value, mutability, .. } = var_node;

    if matches!(initial_value, Some(Expression::Null)) && !matches!(type_name, WaveType::Pointer(_))
    {
        panic!(
            "null literal can only be assigned to ptr<T> (variable '{}': {:?})",
            name, type_name
        );
    }

    let llvm_type = wave_type_to_llvm_type(context, type_name, struct_types, TypeFlavor::AbiC);
    let alloca = build_entry_alloca(context, builder, llvm_type, name);

    // Array literal init: var a: array<T, N> = [ ... ]
    if let (WaveType::Array(element_type, size), Some(Expression::ArrayLiteral(values))) =
        (type_name, initial_value.as_ref())
    {
        if values.len() != *size as usize {
            panic!("❌ Array length mismatch: expected {}, got {}", size, values.len());
        }

        let array_ty = match llvm_type {
            BasicTypeEnum::ArrayType(a) => a,
            other => panic!("WaveType::Array must lower to LLVM array type, got {:?}", other),
        };

        let llvm_element_type =
            wave_type_to_llvm_type(context, element_type, struct_types, TypeFlavor::AbiC);

        let zero = context.i32_type().const_zero();

        for (i, value_expr) in values.iter().enumerate() {
            let raw = generate_expression_ir(
                program,
                context,
                builder,
                value_expr,
                variables,
                module,
                Some(llvm_element_type),
                global_consts,
                struct_types,
                struct_field_indices,
                target_data,
                extern_c_info,
            );

            let v = coerce_basic_value(
                context,
                builder,
                raw,
                llvm_element_type,
                &format!("arr_init_cast_{}", i),
                CoercionMode::Implicit,
                expression_is_unsigned(program, value_expr),
            );

            let idx = context.i32_type().const_int(i as u64, false);

            let gep = unsafe {
                builder
                    .build_in_bounds_gep(
                        array_ty,
                        alloca,
                        &[zero, idx],
                        &format!("array_idx_{}", i),
                    )
                    .unwrap()
            };

            builder.build_store(gep, v).unwrap();
        }

        variables.insert(
            name.clone(),
            VariableInfo { ptr: alloca, mutability: mutability.clone(), ty: type_name.clone() },
        );

        return;
    }

    // An ordinary initializer sees the enclosing binding. Assembly outputs
    // are the exception: the validator exposes their destination up front.
    let asm_initializer = matches!(initial_value, Some(Expression::AsmBlock { .. }));
    if asm_initializer {
        variables.insert(
            name.clone(),
            VariableInfo { ptr: alloca, mutability: mutability.clone(), ty: type_name.clone() },
        );
    }

    // normal init
    if let Some(init) = initial_value {
        let raw = generate_expression_ir(
            program,
            context,
            builder,
            init,
            variables,
            module,
            Some(llvm_type),
            global_consts,
            struct_types,
            struct_field_indices,
            target_data,
            extern_c_info,
        );

        let casted = coerce_basic_value(
            context,
            builder,
            raw,
            llvm_type,
            &format!("variable '{}' initializer", name),
            CoercionMode::Implicit,
            expression_is_unsigned(program, init),
        );

        builder.build_store(alloca, casted).unwrap();
    }

    if !asm_initializer {
        variables.insert(
            name.clone(),
            VariableInfo { ptr: alloca, mutability: mutability.clone(), ty: type_name.clone() },
        );
    }
}

pub(super) fn build_entry_alloca<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    ty: BasicTypeEnum<'ctx>,
    name: &str,
) -> PointerValue<'ctx> {
    let cur_block =
        builder.get_insert_block().unwrap_or_else(|| panic!("build_entry_alloca: no insert block"));
    let func = cur_block
        .get_parent()
        .unwrap_or_else(|| panic!("build_entry_alloca: insert block has no parent function"));
    let entry = func
        .get_first_basic_block()
        .unwrap_or_else(|| panic!("build_entry_alloca: function has no entry block"));

    let entry_builder = context.create_builder();
    if let Some(inst) = entry.get_first_instruction() {
        entry_builder.position_before(&inst);
    } else {
        entry_builder.position_at_end(entry);
    }

    entry_builder.build_alloca(ty, name).unwrap()
}
