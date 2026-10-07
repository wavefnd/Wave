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

//! Short-circuit logical expressions and pointer offsets.
//!
//! Numeric arithmetic is lowered through the verified HIR computation contract.
//! Pointer arithmetic uses HIR-converted indices and scales by the typed pointee
//! layout, retaining Wave's unchecked memory contract.

use super::{utils::to_bool, ExprGenEnv};
use crate::codegen::types::{wave_type_to_llvm_type, TypeFlavor};
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum, IntValue, PointerValue};
use inkwell::IntPredicate;
use parser::ast::{Expression, Operator, WaveType};

fn pointer_index(value: IntValue<'_>) -> IntValue<'_> {
    assert_eq!(value.get_type().get_bit_width(), 64, "ICE: pointer index missing HIR conversion");
    value
}

fn infer_ptr_pointee_ty<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    expr: &Expression,
) -> BasicTypeEnum<'ctx> {
    match env.wave_type(expr) {
        Some(WaveType::Pointer(inner)) => {
            wave_type_to_llvm_type(env.context, &inner, env.struct_types, TypeFlavor::Value)
        },
        Some(WaveType::String) => env.context.i8_type().as_basic_type_enum(),
        other => panic!("typed pointer arithmetic requires a pointee type, found {other:?}"),
    }
}

fn gep_with_i64_offset<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    ptr: PointerValue<'ctx>,
    ptr_expr: &Expression,
    idx_i64: IntValue<'ctx>,
    tag: &str,
) -> PointerValue<'ctx> {
    let pointee_ty = infer_ptr_pointee_ty(env, ptr_expr);
    // SAFETY: Wave pointer arithmetic is explicitly unchecked. A source program
    // must keep an inbounds result within the original allocation (or one past
    // it), which is the contract required by LLVM's `inbounds` GEP.
    unsafe { env.builder.build_in_bounds_gep(pointee_ty, ptr, &[idx_i64], tag).unwrap() }
}

pub(crate) fn gen<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    left: &Expression,
    operator: &Operator,
    right: &Expression,
    expected_type: Option<inkwell::types::BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    if matches!(operator, Operator::LogicalAnd | Operator::LogicalOr) {
        let left_value = env.gen(left, None).into_int_value();
        let left_bool = to_bool(env.builder, left_value);
        let left_block = env.builder.get_insert_block().unwrap();
        let function = left_block.get_parent().unwrap();
        let right_block = env.context.append_basic_block(function, "logical.rhs");
        let merge_block = env.context.append_basic_block(function, "logical.end");

        if matches!(operator, Operator::LogicalAnd) {
            env.builder.build_conditional_branch(left_bool, right_block, merge_block).unwrap();
        } else {
            env.builder.build_conditional_branch(left_bool, merge_block, right_block).unwrap();
        }

        env.builder.position_at_end(right_block);
        let right_value = env.gen(right, None).into_int_value();
        let right_bool = to_bool(env.builder, right_value);
        let right_end = env.builder.get_insert_block().unwrap();
        env.builder.build_unconditional_branch(merge_block).unwrap();

        env.builder.position_at_end(merge_block);
        let short_value = env
            .context
            .bool_type()
            .const_int(if matches!(operator, Operator::LogicalOr) { 1 } else { 0 }, false);
        let phi = env.builder.build_phi(env.context.bool_type(), "logical.result").unwrap();
        phi.add_incoming(&[(&short_value, left_block), (&right_bool, right_end)]);
        let mut result = phi.as_basic_value().into_int_value();

        if let Some(BasicTypeEnum::IntType(expected)) = expected_type {
            if result.get_type() != expected {
                result = env.builder.build_int_z_extend(result, expected, "logical.cast").unwrap();
            }
        }

        return result.as_basic_value_enum();
    }

    let left_val = env.gen(left, None);
    let right_val = env.gen(right, None);
    match (left_val, right_val) {
        (BasicValueEnum::PointerValue(lp), BasicValueEnum::PointerValue(rp)) => {
            let i64_ty = env.context.i64_type();
            let li = env.builder.build_ptr_to_int(lp, i64_ty, "l_ptr2int").unwrap();
            let ri = env.builder.build_ptr_to_int(rp, i64_ty, "r_ptr2int").unwrap();

            let mut result = match operator {
                Operator::Equal => {
                    env.builder.build_int_compare(IntPredicate::EQ, li, ri, "ptreq").unwrap()
                },
                Operator::NotEqual => {
                    env.builder.build_int_compare(IntPredicate::NE, li, ri, "ptrne").unwrap()
                },
                Operator::Subtract => env.builder.build_int_sub(li, ri, "ptrdiff").unwrap(),
                _ => panic!("Unsupported pointer operator: {:?}", operator),
            };

            match operator {
                Operator::Equal | Operator::NotEqual => {
                    if let Some(inkwell::types::BasicTypeEnum::IntType(target_ty)) = expected_type {
                        if result.get_type() != target_ty {
                            if result.get_type().get_bit_width() > target_ty.get_bit_width() {
                                panic!(
                                    "implicit integer narrowing is forbidden in binary result: i{} -> i{}",
                                    result.get_type().get_bit_width(),
                                    target_ty.get_bit_width()
                                );
                            }
                            result = env
                                .builder
                                .build_int_cast(result, target_ty, "cast_result")
                                .unwrap();
                        }
                    }
                },
                Operator::Subtract => {
                    if let Some(inkwell::types::BasicTypeEnum::IntType(target_ty)) = expected_type {
                        if result.get_type() != target_ty {
                            result = env
                                .builder
                                .build_int_cast(result, target_ty, "cast_result")
                                .unwrap();
                        }
                    }
                },
                _ => {},
            }

            return result.as_basic_value_enum();
        },

        (BasicValueEnum::PointerValue(lp), BasicValueEnum::IntValue(ri)) => {
            match operator {
                Operator::Add | Operator::Subtract => {
                    let mut idx = pointer_index(ri);
                    if matches!(operator, Operator::Subtract) {
                        idx = env.builder.build_int_neg(idx, "ptr_idx_neg").unwrap();
                    }
                    let p = gep_with_i64_offset(env, lp, left, idx, "ptr_gep");
                    return p.as_basic_value_enum();
                },
                _ => {},
            };

            let i64_ty = env.context.i64_type();
            let li = env.builder.build_ptr_to_int(lp, i64_ty, "l_ptr2int").unwrap();

            let ri = pointer_index(ri);

            let mut result = match operator {
                Operator::Equal => {
                    env.builder.build_int_compare(IntPredicate::EQ, li, ri, "ptreq0").unwrap()
                },
                Operator::NotEqual => {
                    env.builder.build_int_compare(IntPredicate::NE, li, ri, "ptrne0").unwrap()
                },
                _ => panic!("Unsupported ptr/int operator: {:?}", operator),
            };

            if let Some(inkwell::types::BasicTypeEnum::IntType(target_ty)) = expected_type {
                if result.get_type() != target_ty {
                    if result.get_type().get_bit_width() > target_ty.get_bit_width() {
                        panic!(
                            "implicit integer narrowing is forbidden in binary result: i{} -> i{}",
                            result.get_type().get_bit_width(),
                            target_ty.get_bit_width()
                        );
                    }
                    result = env.builder.build_int_cast(result, target_ty, "cast_result").unwrap();
                }
            }

            return result.as_basic_value_enum();
        },

        (BasicValueEnum::IntValue(li), BasicValueEnum::PointerValue(rp)) => {
            if matches!(operator, Operator::Add) {
                let idx = pointer_index(li);
                let p = gep_with_i64_offset(env, rp, right, idx, "ptr_gep");
                return p.as_basic_value_enum();
            }

            let i64_ty = env.context.i64_type();
            let li = pointer_index(li);

            let ri = env.builder.build_ptr_to_int(rp, i64_ty, "r_ptr2int").unwrap();

            let mut result = match operator {
                Operator::Equal => {
                    env.builder.build_int_compare(IntPredicate::EQ, li, ri, "ptreq0").unwrap()
                },
                Operator::NotEqual => {
                    env.builder.build_int_compare(IntPredicate::NE, li, ri, "ptrne0").unwrap()
                },
                _ => panic!("Unsupported int/ptr operator: {:?}", operator),
            };

            if let Some(inkwell::types::BasicTypeEnum::IntType(target_ty)) = expected_type {
                if result.get_type() != target_ty {
                    if result.get_type().get_bit_width() > target_ty.get_bit_width() {
                        panic!(
                            "implicit integer narrowing is forbidden in binary result: i{} -> i{}",
                            result.get_type().get_bit_width(),
                            target_ty.get_bit_width()
                        );
                    }
                    result = env.builder.build_int_cast(result, target_ty, "cast_result").unwrap();
                }
            }

            return result.as_basic_value_enum();
        },

        _ => panic!("Type mismatch in binary expression"),
    }
}
