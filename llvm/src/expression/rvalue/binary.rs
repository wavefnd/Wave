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

use super::{utils::to_bool, ExprGenEnv};
use crate::codegen::types::{wave_type_to_llvm_type, TypeFlavor};
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum, IntValue, PointerValue};
use inkwell::{FloatPredicate, IntPredicate};
use parser::ast::{Expression, Literal, Operator, WaveType};

fn is_numeric_literal(expr: &Expression) -> bool {
    match expr {
        Expression::Literal(Literal::Int(_)) | Expression::Literal(Literal::Float(_)) => true,
        Expression::Grouped(inner) => is_numeric_literal(inner),
        _ => false,
    }
}

fn value_numeric_basic_type<'ctx>(v: BasicValueEnum<'ctx>) -> Option<BasicTypeEnum<'ctx>> {
    match v {
        BasicValueEnum::IntValue(iv) => Some(iv.get_type().as_basic_type_enum()),
        BasicValueEnum::FloatValue(fv) => Some(fv.get_type().as_basic_type_enum()),
        _ => None,
    }
}

fn cast_int_to_i64<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    v: IntValue<'ctx>,
    tag: &str,
) -> IntValue<'ctx> {
    let i64_ty = env.context.i64_type();
    let src_bits = v.get_type().get_bit_width();

    if src_bits == 64 {
        v
    } else if src_bits < 64 {
        env.builder
            .build_int_s_extend(v, i64_ty, &format!("{}_sext", tag))
            .unwrap()
    } else {
        env.builder
            .build_int_truncate(v, i64_ty, &format!("{}_trunc", tag))
            .unwrap()
    }
}

fn infer_ptr_pointee_ty<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    expr: &Expression,
) -> BasicTypeEnum<'ctx> {
    match expr {
        Expression::Grouped(inner) => infer_ptr_pointee_ty(env, inner),

        Expression::Variable(name) => {
            if let Some(vi) = env.variables.get(name) {
                match &vi.ty {
                    WaveType::Pointer(inner) => wave_type_to_llvm_type(
                        env.context,
                        inner,
                        env.struct_types,
                        TypeFlavor::AbiC,
                    ),
                    WaveType::String => env.context.i8_type().as_basic_type_enum(),
                    _ => env.context.i8_type().as_basic_type_enum(),
                }
            } else {
                env.context.i8_type().as_basic_type_enum()
            }
        }

        Expression::AddressOf(inner) => {
            if let Expression::Variable(name) = &**inner {
                if let Some(vi) = env.variables.get(name) {
                    return wave_type_to_llvm_type(
                        env.context,
                        &vi.ty,
                        env.struct_types,
                        TypeFlavor::AbiC,
                    );
                }
            }
            env.context.i8_type().as_basic_type_enum()
        }

        Expression::Cast { target_type, .. } => match target_type {
            WaveType::Pointer(inner) => {
                wave_type_to_llvm_type(env.context, inner, env.struct_types, TypeFlavor::AbiC)
            }
            WaveType::String => env.context.i8_type().as_basic_type_enum(),
            _ => env.context.i8_type().as_basic_type_enum(),
        },

        _ => env.context.i8_type().as_basic_type_enum(),
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
    unsafe {
        env.builder
            .build_in_bounds_gep(pointee_ty, ptr, &[idx_i64], tag)
            .unwrap()
    }
}

pub(crate) fn gen<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    left: &Expression,
    operator: &Operator,
    right: &Expression,
    expected_type: Option<inkwell::types::BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    let numeric_expected = match expected_type {
        Some(BasicTypeEnum::IntType(_)) | Some(BasicTypeEnum::FloatType(_)) => expected_type,
        _ => None,
    };

    let (left_val, right_val) = if let Some(exp) = numeric_expected {
        (env.gen(left, Some(exp)), env.gen(right, Some(exp)))
    } else if is_numeric_literal(left) && is_numeric_literal(right) {
        (env.gen(left, None), env.gen(right, None))
    } else if is_numeric_literal(left) {
        let r = env.gen(right, None);
        let l = if let Some(hint) = value_numeric_basic_type(r) {
            env.gen(left, Some(hint))
        } else {
            env.gen(left, None)
        };
        (l, r)
    } else {
        let l = env.gen(left, None);
        let r = if is_numeric_literal(right) {
            if let Some(hint) = value_numeric_basic_type(l) {
                env.gen(right, Some(hint))
            } else {
                env.gen(right, None)
            }
        } else {
            env.gen(right, None)
        };
        (l, r)
    };

    match (left_val, right_val) {
        (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
            let l_type = l.get_type();
            let r_type = r.get_type();

            let (l_casted, r_casted) = match operator {
                Operator::ShiftLeft | Operator::ShiftRight => {
                    let r2 = if r_type != l_type {
                        env.builder.build_int_cast(r, l_type, "shamt").unwrap()
                    } else {
                        r
                    };
                    (l, r2)
                }
                _ => {
                    if l_type != r_type {
                        if l_type.get_bit_width() < r_type.get_bit_width() {
                            let new_l =
                                env.builder.build_int_z_extend(l, r_type, "zext_l").unwrap();
                            (new_l, r)
                        } else {
                            let new_r =
                                env.builder.build_int_z_extend(r, l_type, "zext_r").unwrap();
                            (l, new_r)
                        }
                    } else {
                        (l, r)
                    }
                }
            };

            let mut result = match operator {
                Operator::Add => env.builder.build_int_add(l_casted, r_casted, "addtmp"),
                Operator::Subtract => env.builder.build_int_sub(l_casted, r_casted, "subtmp"),
                Operator::Multiply => env.builder.build_int_mul(l_casted, r_casted, "multmp"),
                Operator::Divide => env
                    .builder
                    .build_int_signed_div(l_casted, r_casted, "divtmp"),
                Operator::Remainder => env
                    .builder
                    .build_int_signed_rem(l_casted, r_casted, "modtmp"),
                Operator::ShiftLeft => env.builder.build_left_shift(l_casted, r_casted, "shl"),
                Operator::ShiftRight => env
                    .builder
                    .build_right_shift(l_casted, r_casted, true, "shr"),
                Operator::BitwiseAnd => env.builder.build_and(l_casted, r_casted, "andtmp"),
                Operator::BitwiseOr => env.builder.build_or(l_casted, r_casted, "ortmp"),
                Operator::BitwiseXor => env.builder.build_xor(l_casted, r_casted, "xortmp"),

                Operator::Greater => {
                    env.builder
                        .build_int_compare(IntPredicate::SGT, l_casted, r_casted, "cmptmp")
                }
                Operator::Less => {
                    env.builder
                        .build_int_compare(IntPredicate::SLT, l_casted, r_casted, "cmptmp")
                }
                Operator::Equal => {
                    env.builder
                        .build_int_compare(IntPredicate::EQ, l_casted, r_casted, "cmptmp")
                }
                Operator::NotEqual => {
                    env.builder
                        .build_int_compare(IntPredicate::NE, l_casted, r_casted, "cmptmp")
                }
                Operator::GreaterEqual => {
                    env.builder
                        .build_int_compare(IntPredicate::SGE, l_casted, r_casted, "cmptmp")
                }
                Operator::LessEqual => {
                    env.builder
                        .build_int_compare(IntPredicate::SLE, l_casted, r_casted, "cmptmp")
                }

                Operator::LogicalAnd => {
                    let lb = to_bool(env.builder, l_casted);
                    let rb = to_bool(env.builder, r_casted);
                    env.builder.build_and(lb, rb, "land")
                }
                Operator::LogicalOr => {
                    let lb = to_bool(env.builder, l_casted);
                    let rb = to_bool(env.builder, r_casted);
                    env.builder.build_or(lb, rb, "lor")
                }

                _ => panic!("Unsupported binary operator"),
            }
            .unwrap();

            if let Some(inkwell::types::BasicTypeEnum::IntType(target_ty)) = expected_type {
                let result_ty = result.get_type();
                if result_ty != target_ty {
                    result = env
                        .builder
                        .build_int_cast(result, target_ty, "cast_result")
                        .unwrap();
                }
            }

            result.as_basic_value_enum()
        }

        (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
            let mut result: BasicValueEnum<'ctx> = match operator {
                Operator::Add => env
                    .builder
                    .build_float_add(l, r, "faddtmp")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Subtract => env
                    .builder
                    .build_float_sub(l, r, "fsubtmp")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Multiply => env
                    .builder
                    .build_float_mul(l, r, "fmultmp")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Divide => env
                    .builder
                    .build_float_div(l, r, "fdivtmp")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Remainder => env
                    .builder
                    .build_float_rem(l, r, "fmodtmp")
                    .unwrap()
                    .as_basic_value_enum(),

                Operator::Greater => env
                    .builder
                    .build_float_compare(FloatPredicate::OGT, l, r, "fcmpgt")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Less => env
                    .builder
                    .build_float_compare(FloatPredicate::OLT, l, r, "fcmplt")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Equal => env
                    .builder
                    .build_float_compare(FloatPredicate::OEQ, l, r, "fcmpeq")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::NotEqual => env
                    .builder
                    .build_float_compare(FloatPredicate::ONE, l, r, "fcmpne")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::GreaterEqual => env
                    .builder
                    .build_float_compare(FloatPredicate::OGE, l, r, "fcmpge")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::LessEqual => env
                    .builder
                    .build_float_compare(FloatPredicate::OLE, l, r, "fcmple")
                    .unwrap()
                    .as_basic_value_enum(),

                _ => panic!("Unsupported float operator"),
            };

            #[allow(clippy::collapsible_match)]
            if let Some(exp) = expected_type {
                match (result, exp) {
                    (
                        BasicValueEnum::FloatValue(fv),
                        inkwell::types::BasicTypeEnum::FloatType(target_ty),
                    ) => {
                        if fv.get_type() != target_ty {
                            result = env
                                .builder
                                .build_float_cast(fv, target_ty, "fcast_result")
                                .unwrap()
                                .as_basic_value_enum();
                        }
                    }
                    (
                        BasicValueEnum::IntValue(iv),
                        inkwell::types::BasicTypeEnum::IntType(target_ty),
                    ) => {
                        if iv.get_type() != target_ty {
                            result = env
                                .builder
                                .build_int_cast(iv, target_ty, "icast_result")
                                .unwrap()
                                .as_basic_value_enum();
                        }
                    }
                    _ => {}
                }
            }

            result
        }

        (BasicValueEnum::IntValue(int_val), BasicValueEnum::FloatValue(float_val)) => {
            let casted = env
                .builder
                .build_signed_int_to_float(int_val, float_val.get_type(), "cast_lhs")
                .unwrap();

            match operator {
                Operator::Add => env
                    .builder
                    .build_float_add(casted, float_val, "addtmp")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Subtract => env
                    .builder
                    .build_float_sub(casted, float_val, "subtmp")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Multiply => env
                    .builder
                    .build_float_mul(casted, float_val, "multmp")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Divide => env
                    .builder
                    .build_float_div(casted, float_val, "divtmp")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Remainder => env
                    .builder
                    .build_float_rem(casted, float_val, "modtmp")
                    .unwrap()
                    .as_basic_value_enum(),

                Operator::Greater => env
                    .builder
                    .build_float_compare(FloatPredicate::OGT, casted, float_val, "fcmpgt")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Less => env
                    .builder
                    .build_float_compare(FloatPredicate::OLT, casted, float_val, "fcmplt")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Equal => env
                    .builder
                    .build_float_compare(FloatPredicate::OEQ, casted, float_val, "fcmpeq")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::NotEqual => env
                    .builder
                    .build_float_compare(FloatPredicate::ONE, casted, float_val, "fcmpne")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::GreaterEqual => env
                    .builder
                    .build_float_compare(FloatPredicate::OGE, casted, float_val, "fcmpge")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::LessEqual => env
                    .builder
                    .build_float_compare(FloatPredicate::OLE, casted, float_val, "fcmple")
                    .unwrap()
                    .as_basic_value_enum(),

                _ => panic!("Unsupported mixed-type operator (int + float)"),
            }
        }

        (BasicValueEnum::FloatValue(float_val), BasicValueEnum::IntValue(int_val)) => {
            let casted = env
                .builder
                .build_signed_int_to_float(int_val, float_val.get_type(), "cast_rhs")
                .unwrap();

            match operator {
                Operator::Add => env
                    .builder
                    .build_float_add(float_val, casted, "addtmp")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Subtract => env
                    .builder
                    .build_float_sub(float_val, casted, "subtmp")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Multiply => env
                    .builder
                    .build_float_mul(float_val, casted, "multmp")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Divide => env
                    .builder
                    .build_float_div(float_val, casted, "divtmp")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Remainder => env
                    .builder
                    .build_float_rem(float_val, casted, "modtmp")
                    .unwrap()
                    .as_basic_value_enum(),

                Operator::Greater => env
                    .builder
                    .build_float_compare(FloatPredicate::OGT, float_val, casted, "fcmpgt")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Less => env
                    .builder
                    .build_float_compare(FloatPredicate::OLT, float_val, casted, "fcmplt")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Equal => env
                    .builder
                    .build_float_compare(FloatPredicate::OEQ, float_val, casted, "fcmpeq")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::NotEqual => env
                    .builder
                    .build_float_compare(FloatPredicate::ONE, float_val, casted, "fcmpne")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::GreaterEqual => env
                    .builder
                    .build_float_compare(FloatPredicate::OGE, float_val, casted, "fcmpge")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::LessEqual => env
                    .builder
                    .build_float_compare(FloatPredicate::OLE, float_val, casted, "fcmple")
                    .unwrap()
                    .as_basic_value_enum(),

                _ => panic!("Unsupported mixed-type operator (float + int)"),
            }
        }
        (BasicValueEnum::PointerValue(lp), BasicValueEnum::PointerValue(rp)) => {
            let i64_ty = env.context.i64_type();
            let li = env
                .builder
                .build_ptr_to_int(lp, i64_ty, "l_ptr2int")
                .unwrap();
            let ri = env
                .builder
                .build_ptr_to_int(rp, i64_ty, "r_ptr2int")
                .unwrap();

            let mut result = match operator {
                Operator::Equal => env
                    .builder
                    .build_int_compare(IntPredicate::EQ, li, ri, "ptreq")
                    .unwrap(),
                Operator::NotEqual => env
                    .builder
                    .build_int_compare(IntPredicate::NE, li, ri, "ptrne")
                    .unwrap(),
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
                }
                Operator::Subtract => {
                    if let Some(inkwell::types::BasicTypeEnum::IntType(target_ty)) = expected_type {
                        if result.get_type() != target_ty {
                            result = env
                                .builder
                                .build_int_cast(result, target_ty, "cast_result")
                                .unwrap();
                        }
                    }
                }
                _ => {}
            }

            return result.as_basic_value_enum();
        }

        (BasicValueEnum::PointerValue(lp), BasicValueEnum::IntValue(ri)) => {
            match operator {
                Operator::Add | Operator::Subtract => {
                    let mut idx = cast_int_to_i64(env, ri, "ptr_idx");
                    if matches!(operator, Operator::Subtract) {
                        idx = env.builder.build_int_neg(idx, "ptr_idx_neg").unwrap();
                    }
                    let p = gep_with_i64_offset(env, lp, left, idx, "ptr_gep");
                    return p.as_basic_value_enum();
                }
                _ => {}
            };

            let i64_ty = env.context.i64_type();
            let li = env
                .builder
                .build_ptr_to_int(lp, i64_ty, "l_ptr2int")
                .unwrap();

            let ri = cast_int_to_i64(env, ri, "r_i64");

            let mut result = match operator {
                Operator::Equal => env
                    .builder
                    .build_int_compare(IntPredicate::EQ, li, ri, "ptreq0")
                    .unwrap(),
                Operator::NotEqual => env
                    .builder
                    .build_int_compare(IntPredicate::NE, li, ri, "ptrne0")
                    .unwrap(),
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
                    result = env
                        .builder
                        .build_int_cast(result, target_ty, "cast_result")
                        .unwrap();
                }
            }

            return result.as_basic_value_enum();
        }

        (BasicValueEnum::IntValue(li), BasicValueEnum::PointerValue(rp)) => {
            if matches!(operator, Operator::Add) {
                let idx = cast_int_to_i64(env, li, "ptr_idx");
                let p = gep_with_i64_offset(env, rp, right, idx, "ptr_gep");
                return p.as_basic_value_enum();
            }

            let i64_ty = env.context.i64_type();
            let li = cast_int_to_i64(env, li, "l_i64");

            let ri = env
                .builder
                .build_ptr_to_int(rp, i64_ty, "r_ptr2int")
                .unwrap();

            let mut result = match operator {
                Operator::Equal => env
                    .builder
                    .build_int_compare(IntPredicate::EQ, li, ri, "ptreq0")
                    .unwrap(),
                Operator::NotEqual => env
                    .builder
                    .build_int_compare(IntPredicate::NE, li, ri, "ptrne0")
                    .unwrap(),
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
                    result = env
                        .builder
                        .build_int_cast(result, target_ty, "cast_result")
                        .unwrap();
                }
            }

            return result.as_basic_value_enum();
        }

        _ => panic!("Type mismatch in binary expression"),
    }
}
