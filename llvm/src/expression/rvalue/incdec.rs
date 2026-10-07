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

//! Prefix and postfix increment/decrement lowering for assignable expressions.
//!
//! The target address is evaluated once, then the old and new values are kept
//! separate so postfix operations can store the update while returning the old
//! value. Pointer steps are scaled by their recovered Wave pointee type.

use super::ExprGenEnv;
use crate::codegen::generate_address_ir;
use crate::codegen::types::{wave_type_to_llvm_type, TypeFlavor};
use inkwell::types::{AsTypeRef, BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::{Expression, IncDecKind, WaveType};

fn normalize_struct_name(raw: &str) -> &str {
    raw.strip_prefix("struct.").unwrap_or(raw).trim_start_matches('%')
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

fn wave_type_of_lvalue<'ctx, 'a>(env: &ExprGenEnv<'ctx, 'a>, e: &Expression) -> Option<WaveType> {
    if let Some(ty) = env.wave_type(e) {
        return Some(ty);
    }
    match e {
        Expression::Variable(name) => env.variables.get(name).map(|vi| vi.ty.clone()),
        Expression::Grouped(inner) => wave_type_of_lvalue(env, inner),
        Expression::AddressOf(inner) => {
            wave_type_of_lvalue(env, inner).map(|t| WaveType::Pointer(Box::new(t)))
        },
        Expression::Deref(inner) => {
            let inner_ty = wave_type_of_lvalue(env, inner)?;
            match inner_ty {
                WaveType::Pointer(t) => Some(*t),
                WaveType::String => Some(WaveType::Byte),
                _ => None,
            }
        },
        Expression::IndexAccess { target, .. } => {
            let t = wave_type_of_lvalue(env, target)?;
            match t {
                WaveType::Array(inner, _) => Some(*inner),
                WaveType::Pointer(inner) => Some(*inner),
                WaveType::String => Some(WaveType::Byte),
                _ => None,
            }
        },
        _ => None,
    }
}

fn infer_lvalue_value_type<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    target: &Expression,
) -> BasicTypeEnum<'ctx> {
    match target {
        Expression::Grouped(inner) | Expression::AddressOf(inner) => {
            infer_lvalue_value_type(env, inner)
        },

        Expression::Variable(_) | Expression::Deref(_) | Expression::IndexAccess { .. } => {
            let wt = wave_type_of_lvalue(env, target)
                .unwrap_or_else(|| panic!("Cannot infer lvalue type: {:?}", target));
            wave_to_basic(env, &wt)
        },

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
                    panic!("Invalid field index {} for struct {}", field_index, struct_name)
                });
            }

            let obj_ty = infer_lvalue_value_type(env, object);
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
                panic!("Invalid field index {} for struct {}", field_index, struct_key)
            })
        },

        _ => panic!("Expression is not an assignable lvalue: {:?}", target),
    }
}

fn infer_ptr_pointee_type<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    target: &Expression,
) -> BasicTypeEnum<'ctx> {
    let wt = wave_type_of_lvalue(env, target)
        .unwrap_or_else(|| panic!("Cannot infer pointer pointee type: {:?}", target));

    match wt {
        WaveType::Pointer(inner) => wave_to_basic(env, &inner),
        WaveType::String => env.context.i8_type().as_basic_type_enum(),
        _ => env.context.i8_type().as_basic_type_enum(),
    }
}

pub(crate) fn gen<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    kind: &IncDecKind,
    target: &Expression,
) -> BasicValueEnum<'ctx> {
    let ptr = generate_address_ir(env, target);

    let element_type = infer_lvalue_value_type(env, target);
    let old_val =
        env.builder.build_load(element_type, ptr, "incdec_old").unwrap().as_basic_value_enum();

    let new_val: BasicValueEnum<'ctx> = match old_val {
        BasicValueEnum::IntValue(iv) => {
            if iv.get_type().get_bit_width() == 1 {
                panic!("++/-- not allowed on bool");
            }

            let one = iv.get_type().const_int(1, false);
            let nv = match kind {
                IncDecKind::PreInc | IncDecKind::PostInc => {
                    env.builder.build_int_add(iv, one, "inc").unwrap()
                },
                IncDecKind::PreDec | IncDecKind::PostDec => {
                    env.builder.build_int_sub(iv, one, "dec").unwrap()
                },
            };
            nv.as_basic_value_enum()
        },

        BasicValueEnum::FloatValue(fv) => {
            let one = fv.get_type().const_float(1.0);
            let nv = match kind {
                IncDecKind::PreInc | IncDecKind::PostInc => {
                    env.builder.build_float_add(fv, one, "finc").unwrap()
                },
                IncDecKind::PreDec | IncDecKind::PostDec => {
                    env.builder.build_float_sub(fv, one, "fdec").unwrap()
                },
            };
            nv.as_basic_value_enum()
        },

        BasicValueEnum::PointerValue(pv) => {
            let idx = match kind {
                IncDecKind::PreInc | IncDecKind::PostInc => {
                    env.context.i64_type().const_int(1, true)
                },
                IncDecKind::PreDec | IncDecKind::PostDec => {
                    env.context.i64_type().const_int((-1i64) as u64, true)
                },
            };

            let pointee_ty = infer_ptr_pointee_type(env, target);
            // SAFETY: Wave pointer arithmetic is unchecked and requires the
            // source pointer and stepped result to satisfy LLVM's inbounds GEP
            // contract for the same allocation.
            let gep = unsafe {
                env.builder.build_in_bounds_gep(pointee_ty, pv, &[idx], "pincdec").unwrap()
            };
            gep.as_basic_value_enum()
        },

        _ => panic!("Unsupported type for ++/--: {:?}", old_val),
    };

    env.builder.build_store(ptr, new_val).unwrap();

    match kind {
        IncDecKind::PreInc | IncDecKind::PreDec => new_val,
        IncDecKind::PostInc | IncDecKind::PostDec => old_val,
    }
}
