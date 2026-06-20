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

use super::ExprGenEnv;
use crate::codegen::types::TypeFlavor;
use crate::codegen::{generate_address_ir, wave_type_to_llvm_type};
use crate::statement::variable::{coerce_basic_value, CoercionMode};
use inkwell::types::{AsTypeRef, BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum};
use inkwell::AddressSpace;
use parser::ast::{AssignOperator, Expression, WaveType};

fn normalize_struct_name(raw: &str) -> &str {
    raw.strip_prefix("struct.")
        .unwrap_or(raw)
        .trim_start_matches('%')
}

fn resolve_struct_key<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    st: inkwell::types::StructType<'ctx>,
) -> String {
    if let Some(raw) = st.get_name().and_then(|n| n.to_str().ok()) {
        return normalize_struct_name(raw).to_string();
    }

    let st_ref = st.as_type_ref();
    for (name, ty) in env.struct_types {
        if ty.as_type_ref() == st_ref {
            return name.clone();
        }
    }

    panic!("LLVM struct type has no name and cannot be matched to struct_types");
}

fn wave_to_basic<'ctx, 'a>(env: &ExprGenEnv<'ctx, 'a>, wt: &WaveType) -> BasicTypeEnum<'ctx> {
    wave_type_to_llvm_type(env.context, wt, env.struct_types, TypeFlavor::Value)
}

fn basic_to_wave<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    bt: BasicTypeEnum<'ctx>,
) -> Option<WaveType> {
    match bt {
        BasicTypeEnum::IntType(it) => {
            let bw = it.get_bit_width() as u16;
            if bw == 1 {
                Some(WaveType::Bool)
            } else {
                Some(WaveType::Int(bw))
            }
        }
        BasicTypeEnum::FloatType(ft) => Some(WaveType::Float(ft.get_bit_width() as u16)),
        BasicTypeEnum::PointerType(_) => Some(WaveType::Pointer(Box::new(WaveType::Byte))),
        BasicTypeEnum::ArrayType(at) => {
            let elem = basic_to_wave(env, at.get_element_type())?;
            Some(WaveType::Array(Box::new(elem), at.len()))
        }
        BasicTypeEnum::StructType(st) => Some(WaveType::Struct(resolve_struct_key(env, st))),
        _ => None,
    }
}

fn wave_type_of_lvalue<'ctx, 'a>(env: &ExprGenEnv<'ctx, 'a>, e: &Expression) -> Option<WaveType> {
    match e {
        Expression::Variable(name) => env.variables.get(name).map(|vi| vi.ty.clone()),
        Expression::Grouped(inner) => wave_type_of_lvalue(env, inner),
        Expression::AddressOf(inner) => {
            wave_type_of_lvalue(env, inner).map(|t| WaveType::Pointer(Box::new(t)))
        }
        Expression::Deref(inner) => {
            let inner_ty = wave_type_of_lvalue(env, inner)?;
            if matches!(
                inner.as_ref(),
                Expression::IndexAccess { .. } | Expression::FieldAccess { .. }
            ) {
                return Some(inner_ty);
            }
            match inner_ty {
                WaveType::Pointer(t) => Some(*t),
                WaveType::String => Some(WaveType::Byte),
                other => Some(other),
            }
        }
        Expression::IndexAccess { target, .. } => {
            let t = wave_type_of_lvalue(env, target)?;
            match t {
                WaveType::Array(inner, _) => Some(*inner),
                WaveType::Pointer(inner) => match *inner {
                    WaveType::Array(element, _) => Some(*element),
                    other => Some(other),
                },
                WaveType::String => Some(WaveType::Byte),
                _ => None,
            }
        }
        Expression::FieldAccess { object, field } => {
            let object_ty = wave_type_of_lvalue(env, object)?;
            let struct_name = match object_ty {
                WaveType::Struct(name) => name,
                WaveType::Pointer(inner) => match *inner {
                    WaveType::Struct(name) => name,
                    _ => return None,
                },
                _ => return None,
            };

            let st = *env.struct_types.get(&struct_name)?;
            let field_index = env
                .struct_field_indices
                .get(&struct_name)
                .and_then(|m| m.get(field))
                .copied()?;
            let field_bt = st.get_field_type_at_index(field_index)?;
            basic_to_wave(env, field_bt)
        }
        _ => None,
    }
}

fn is_null_expr(expr: &Expression) -> bool {
    match expr {
        Expression::Null => true,
        Expression::Grouped(inner) => is_null_expr(inner),
        _ => false,
    }
}

fn ensure_null_target_is_ptr<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    target: &Expression,
    value: &Expression,
) {
    if !is_null_expr(value) {
        return;
    }

    let target_ty = wave_type_of_lvalue(env, target)
        .unwrap_or_else(|| panic!("Cannot infer lvalue type for null assignment: {:?}", target));

    if !matches!(target_ty, WaveType::Pointer(_)) {
        panic!(
            "null literal can only be assigned to ptr<T> (target type: {:?})",
            target_ty
        );
    }
}

fn infer_lvalue_store_type<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    target: &Expression,
) -> BasicTypeEnum<'ctx> {
    match target {
        Expression::Grouped(inner) | Expression::AddressOf(inner) => {
            infer_lvalue_store_type(env, inner)
        }

        Expression::Variable(_) | Expression::Deref(_) | Expression::IndexAccess { .. } => {
            let wt = wave_type_of_lvalue(env, target)
                .unwrap_or_else(|| panic!("Cannot infer lvalue type: {:?}", target));
            wave_to_basic(env, &wt)
        }

        Expression::FieldAccess { object, field } => {
            let struct_name_opt = wave_type_of_lvalue(env, object).and_then(|wt| match wt {
                WaveType::Struct(name) => Some(name),
                WaveType::Pointer(inner) => match *inner {
                    WaveType::Struct(name) => Some(name),
                    _ => None,
                },
                _ => None,
            });

            if let Some(struct_name) = struct_name_opt {
                let st = *env
                    .struct_types
                    .get(&struct_name)
                    .unwrap_or_else(|| panic!("Struct type '{}' not found", struct_name));

                let field_index = env
                    .struct_field_indices
                    .get(&struct_name)
                    .and_then(|m| m.get(field))
                    .copied()
                    .unwrap_or_else(|| panic!("Unknown field '{}.{}'", struct_name, field));

                return st.get_field_type_at_index(field_index).unwrap_or_else(|| {
                    panic!(
                        "Invalid field index {} for struct {}",
                        field_index, struct_name
                    )
                });
            }

            let obj_ty = infer_lvalue_store_type(env, object);
            let st = match obj_ty {
                BasicTypeEnum::StructType(st) => st,
                other => panic!(
                    "FieldAccess base is not a struct value: {:?} (expr: {:?})",
                    other, object
                ),
            };

            let struct_key = resolve_struct_key(env, st);

            let field_index = env
                .struct_field_indices
                .get(&struct_key)
                .and_then(|m| m.get(field))
                .copied()
                .unwrap_or_else(|| panic!("Unknown field '{}.{}'", struct_key, field));

            st.get_field_type_at_index(field_index).unwrap_or_else(|| {
                panic!(
                    "Invalid field index {} for struct {}",
                    field_index, struct_key
                )
            })
        }

        _ => panic!("Expression is not an assignable lvalue: {:?}", target),
    }
}

fn materialize_for_store<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    rhs: BasicValueEnum<'ctx>,
    element_type: BasicTypeEnum<'ctx>,
    tag: &str,
) -> BasicValueEnum<'ctx> {
    match (rhs, element_type) {
        (BasicValueEnum::PointerValue(pv), BasicTypeEnum::ArrayType(at)) => env
            .builder
            .build_load(at, pv, tag)
            .unwrap()
            .as_basic_value_enum(),
        (BasicValueEnum::PointerValue(pv), BasicTypeEnum::StructType(st)) => env
            .builder
            .build_load(st, pv, tag)
            .unwrap()
            .as_basic_value_enum(),
        (v, _) => v,
    }
}

fn try_gen_store_through_rvalue_pointer<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    target: &Expression,
    value: &Expression,
) -> Option<BasicValueEnum<'ctx>> {
    let Expression::Deref(inner) = target else {
        return None;
    };

    if wave_type_of_lvalue(env, target).is_some() {
        return None;
    }

    if is_null_expr(value) {
        panic!(
            "cannot infer pointee type for null assignment through pointer expression: {:?}",
            target
        );
    }

    let mut rhs = env.gen(value, None);
    let element_type = rhs.get_type();
    rhs = materialize_for_store(env, rhs, element_type, "rvalue_ptr_assign_agg_load");

    let ptr_ty = env
        .context
        .ptr_type(AddressSpace::default())
        .as_basic_type_enum();
    let ptr_val = env.gen(inner, Some(ptr_ty));
    let BasicValueEnum::PointerValue(dst_ptr) = ptr_val else {
        panic!(
            "deref assignment target must evaluate to a pointer, got {:?}",
            ptr_val.get_type()
        );
    };

    env.builder.build_store(dst_ptr, rhs).unwrap();
    Some(rhs)
}

pub(crate) fn gen_assign_operation<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    target: &Expression,
    operator: &AssignOperator,
    value: &Expression,
) -> BasicValueEnum<'ctx> {
    if matches!(operator, AssignOperator::Assign) {
        if let Some(v) = try_gen_store_through_rvalue_pointer(env, target, value) {
            return v;
        }
    }

    let ptr = generate_address_ir(
        env.context,
        env.builder,
        target,
        env.variables,
        env.module,
        env.struct_types,
        env.struct_field_indices,
    );

    let element_type = infer_lvalue_store_type(env, target);

    if matches!(operator, AssignOperator::Assign) {
        ensure_null_target_is_ptr(env, target, value);

        let mut rhs = env.gen(value, Some(element_type));
        rhs = materialize_for_store(env, rhs, element_type, "assign_agg_load");

        if rhs.get_type() != element_type {
            let tag = format!("assignment to {:?}", target);
            rhs = coerce_basic_value(
                env.context,
                env.builder,
                rhs,
                element_type,
                &tag,
                CoercionMode::Implicit,
            );
        }

        env.builder.build_store(ptr, rhs).unwrap();
        return rhs;
    }

    // compound op: load current typed
    let current_val = env
        .builder
        .build_load(element_type, ptr, "load_current")
        .unwrap()
        .as_basic_value_enum();

    let new_val = env.gen(value, Some(current_val.get_type()));

    let (current_val, new_val) = match (current_val, new_val) {
        (BasicValueEnum::FloatValue(lhs), BasicValueEnum::IntValue(rhs)) => {
            let rhs_casted = env
                .builder
                .build_signed_int_to_float(rhs, lhs.get_type(), "int_to_float")
                .unwrap();
            (
                BasicValueEnum::FloatValue(lhs),
                BasicValueEnum::FloatValue(rhs_casted),
            )
        }
        (BasicValueEnum::IntValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
            let lhs_casted = env
                .builder
                .build_signed_int_to_float(lhs, rhs.get_type(), "int_to_float")
                .unwrap();
            (
                BasicValueEnum::FloatValue(lhs_casted),
                BasicValueEnum::FloatValue(rhs),
            )
        }
        other => other,
    };

    let result = match (current_val, new_val) {
        (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => match operator {
            AssignOperator::AddAssign => env
                .builder
                .build_int_add(lhs, rhs, "add_assign")
                .unwrap()
                .as_basic_value_enum(),
            AssignOperator::SubAssign => env
                .builder
                .build_int_sub(lhs, rhs, "sub_assign")
                .unwrap()
                .as_basic_value_enum(),
            AssignOperator::MulAssign => env
                .builder
                .build_int_mul(lhs, rhs, "mul_assign")
                .unwrap()
                .as_basic_value_enum(),
            AssignOperator::DivAssign => env
                .builder
                .build_int_signed_div(lhs, rhs, "div_assign")
                .unwrap()
                .as_basic_value_enum(),
            AssignOperator::RemAssign => env
                .builder
                .build_int_signed_rem(lhs, rhs, "rem_assign")
                .unwrap()
                .as_basic_value_enum(),
            AssignOperator::Assign => unreachable!(),
        },

        (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => match operator {
            AssignOperator::AddAssign => env
                .builder
                .build_float_add(lhs, rhs, "add_assign")
                .unwrap()
                .as_basic_value_enum(),
            AssignOperator::SubAssign => env
                .builder
                .build_float_sub(lhs, rhs, "sub_assign")
                .unwrap()
                .as_basic_value_enum(),
            AssignOperator::MulAssign => env
                .builder
                .build_float_mul(lhs, rhs, "mul_assign")
                .unwrap()
                .as_basic_value_enum(),
            AssignOperator::DivAssign => env
                .builder
                .build_float_div(lhs, rhs, "div_assign")
                .unwrap()
                .as_basic_value_enum(),
            AssignOperator::RemAssign => env
                .builder
                .build_float_rem(lhs, rhs, "rem_assign")
                .unwrap()
                .as_basic_value_enum(),
            AssignOperator::Assign => unreachable!(),
        },

        _ => panic!("AssignOperation (+=, -=, ...) only supports numeric types"),
    };

    let result_casted = match (result, element_type) {
        (BasicValueEnum::FloatValue(val), BasicTypeEnum::IntType(int_ty)) => env
            .builder
            .build_float_to_signed_int(val, int_ty, "float_to_int")
            .unwrap()
            .as_basic_value_enum(),
        (BasicValueEnum::IntValue(val), BasicTypeEnum::FloatType(float_ty)) => env
            .builder
            .build_signed_int_to_float(val, float_ty, "int_to_float")
            .unwrap()
            .as_basic_value_enum(),
        _ => result,
    };

    env.builder.build_store(ptr, result_casted).unwrap();
    result_casted
}

pub(crate) fn gen_assignment<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    target: &Expression,
    value: &Expression,
) -> BasicValueEnum<'ctx> {
    if let Some(v) = try_gen_store_through_rvalue_pointer(env, target, value) {
        return v;
    }

    let ptr = generate_address_ir(
        env.context,
        env.builder,
        target,
        env.variables,
        env.module,
        env.struct_types,
        env.struct_field_indices,
    );

    let element_type = infer_lvalue_store_type(env, target);

    ensure_null_target_is_ptr(env, target, value);

    let mut v = env.gen(value, Some(element_type));
    v = materialize_for_store(env, v, element_type, "assign_rhs_agg_load");

    if v.get_type() != element_type {
        let tag = format!("assignment to {:?}", target);
        v = coerce_basic_value(
            env.context,
            env.builder,
            v,
            element_type,
            &tag,
            CoercionMode::Implicit,
        );
    }

    env.builder.build_store(ptr, v).unwrap();
    v
}
