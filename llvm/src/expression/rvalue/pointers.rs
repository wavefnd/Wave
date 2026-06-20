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
use crate::codegen::types::{wave_type_to_llvm_type, TypeFlavor};
use crate::codegen::{generate_address_and_type_ir, generate_address_ir};
use crate::statement::variable::{coerce_basic_value, CoercionMode};
use inkwell::types::AsTypeRef;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::{Expression, WaveType};

#[allow(dead_code)]
fn push_deref_into_base(expr: &Expression) -> Expression {
    match expr {
        Expression::Grouped(inner) => Expression::Grouped(Box::new(push_deref_into_base(inner))),
        Expression::IndexAccess { target, index } => Expression::IndexAccess {
            target: Box::new(push_deref_into_base(target)),
            index: index.clone(),
        },
        Expression::FieldAccess { object, field } => Expression::FieldAccess {
            object: Box::new(push_deref_into_base(object)),
            field: field.clone(),
        },
        other => Expression::Deref(Box::new(other.clone())),
    }
}

fn normalize_struct_name(raw: &str) -> &str {
    raw.strip_prefix("struct.")
        .unwrap_or(raw)
        .trim_start_matches('%')
}

fn resolve_struct_key<'ctx>(
    st: inkwell::types::StructType<'ctx>,
    struct_types: &std::collections::HashMap<String, inkwell::types::StructType<'ctx>>,
) -> Option<String> {
    if let Some(raw) = st.get_name().and_then(|n| n.to_str().ok()) {
        return Some(normalize_struct_name(raw).to_string());
    }

    let st_ref = st.as_type_ref();
    for (name, ty) in struct_types {
        if ty.as_type_ref() == st_ref {
            return Some(name.clone());
        }
    }

    None
}

fn basic_ty_to_wave_ty<'ctx>(
    ty: BasicTypeEnum<'ctx>,
    struct_types: &std::collections::HashMap<String, inkwell::types::StructType<'ctx>>,
) -> Option<WaveType> {
    match ty {
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
            let elem = basic_ty_to_wave_ty(at.get_element_type(), struct_types)?;
            Some(WaveType::Array(Box::new(elem), at.len()))
        }
        BasicTypeEnum::StructType(st) => {
            let name = resolve_struct_key(st, struct_types)?;
            Some(WaveType::Struct(name))
        }
        _ => None,
    }
}

fn infer_wave_type_of_expr<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    expr: &Expression,
) -> Option<WaveType> {
    match expr {
        Expression::Variable(name) => env.variables.get(name).map(|vi| vi.ty.clone()),

        Expression::Grouped(inner) => infer_wave_type_of_expr(env, inner),

        Expression::AddressOf(inner) => {
            let inner_ty = infer_wave_type_of_expr(env, inner)?;
            Some(WaveType::Pointer(Box::new(inner_ty)))
        }

        Expression::Deref(inner) => {
            let inner_ty = infer_wave_type_of_expr(env, inner)?;
            if matches!(
                inner.as_ref(),
                Expression::IndexAccess { .. } | Expression::FieldAccess { .. }
            ) {
                return Some(inner_ty);
            }
            match inner_ty {
                WaveType::Pointer(t) => Some(*t),
                WaveType::String => Some(WaveType::Byte),
                _ => None,
            }
        }

        Expression::IndexAccess { target, .. } => {
            let target_ty = infer_wave_type_of_expr(env, target)?;
            match target_ty {
                WaveType::Array(inner, _) => Some(*inner),
                WaveType::Pointer(inner) => match *inner {
                    WaveType::Array(elem, _) => Some(*elem),
                    other => Some(other),
                },
                WaveType::String => Some(WaveType::Byte),
                _ => None,
            }
        }

        Expression::FieldAccess { object, field } => {
            let full = Expression::FieldAccess {
                object: Box::new((**object).clone()),
                field: field.clone(),
            };
            let (_, field_ty) = generate_address_and_type_ir(
                env.context,
                env.builder,
                &full,
                env.variables,
                env.module,
                env.struct_types,
                env.struct_field_indices,
            );
            basic_ty_to_wave_ty(field_ty, env.struct_types)
        }

        _ => None,
    }
}

fn infer_deref_load_ty<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    inner_expr: &Expression,
    expected_type: Option<BasicTypeEnum<'ctx>>,
) -> BasicTypeEnum<'ctx> {
    if let Some(t) = expected_type {
        return t;
    }

    let inferred = infer_wave_type_of_expr(env, inner_expr).unwrap_or_else(|| {
        panic!(
            "deref needs expected_type in opaque-pointer mode (cannot infer load type from {:?})",
            inner_expr
        )
    });

    match inferred {
        WaveType::Pointer(inner) => {
            wave_type_to_llvm_type(env.context, &inner, env.struct_types, TypeFlavor::Value)
        }
        WaveType::String => env.context.i8_type().as_basic_type_enum(),
        other => match inner_expr {
            // Preserve legacy behavior for lvalues like `deref visited[x]`.
            Expression::IndexAccess { .. }
            | Expression::FieldAccess { .. }
            | Expression::Grouped(_) => {
                wave_type_to_llvm_type(env.context, &other, env.struct_types, TypeFlavor::Value)
            }
            _ => panic!("deref expects pointer type, got {:?}", other),
        },
    }
}

pub(crate) fn gen_deref<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    inner_expr: &Expression,
    expected_type: Option<BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    let load_ty = infer_deref_load_ty(env, inner_expr, expected_type);

    match inner_expr {
        Expression::Grouped(inner) => return gen_deref(env, inner, Some(load_ty)),

        // lvalue (x[i], x.field) -> address -> typed load
        Expression::IndexAccess { .. } | Expression::FieldAccess { .. } => {
            let addr = generate_address_ir(
                env.context,
                env.builder,
                inner_expr,
                env.variables,
                env.module,
                env.struct_types,
                env.struct_field_indices,
            );

            return env
                .builder
                .build_load(load_ty, addr, "deref_load")
                .unwrap()
                .as_basic_value_enum();
        }

        _ => {}
    }

    // pointer value -> typed load
    let v = env.gen(inner_expr, None);
    if let BasicValueEnum::PointerValue(p) = v {
        return env
            .builder
            .build_load(load_ty, p, "deref_load")
            .unwrap()
            .as_basic_value_enum();
    }

    panic!(
        "deref expects pointer or lvalue (x[i], x.field), got: {:?}",
        inner_expr
    );
}

pub(crate) fn gen_addressof<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    inner_expr: &Expression,
    expected_type: Option<BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    // &[ ... ] : array literal address-of
    if let Expression::ArrayLiteral(elements) = inner_expr {
        let ptr_ty = match expected_type {
            Some(BasicTypeEnum::PointerType(p)) => p,
            _ => panic!("&[ ... ] needs an expected pointer type (e.g. ptr<i32>)"),
        };

        if elements.is_empty() {
            panic!("&[] cannot infer element type in opaque-pointer mode (empty array literal)");
        }

        let first_val0 = env.gen(&elements[0], None);
        let elem_ty = first_val0.get_type();

        let array_ty = elem_ty.array_type(elements.len() as u32);
        let arr_alloca = env.builder.build_alloca(array_ty, "tmp_array").unwrap();

        let zero = env.context.i32_type().const_zero();

        for (i, expr) in elements.iter().enumerate() {
            let mut val = if i == 0 {
                first_val0
            } else {
                env.gen(expr, Some(elem_ty))
            };

            if val.get_type() != elem_ty {
                val = coerce_basic_value(
                    env.context,
                    env.builder,
                    val,
                    elem_ty,
                    &format!("addrof_arr{}_cast", i),
                    CoercionMode::Implicit,
                );
            }

            let idx = env.context.i32_type().const_int(i as u64, false);
            let gep = unsafe {
                env.builder
                    .build_in_bounds_gep(
                        array_ty,
                        arr_alloca,
                        &[zero, idx],
                        &format!("array_idx_{}", i),
                    )
                    .unwrap()
            };

            env.builder.build_store(gep, val).unwrap();
        }

        // return pointer to first element (array decays)
        let first = unsafe {
            env.builder
                .build_in_bounds_gep(array_ty, arr_alloca, &[zero, zero], "array_first_ptr")
                .unwrap()
        };

        if first.get_type() != ptr_ty {
            return env
                .builder
                .build_bit_cast(
                    first.as_basic_value_enum(),
                    ptr_ty.as_basic_type_enum(),
                    "addrof_array_cast",
                )
                .unwrap()
                .as_basic_value_enum();
        }

        return first.as_basic_value_enum();
    }

    // normal &lvalue : address
    let addr = generate_address_ir(
        env.context,
        env.builder,
        inner_expr,
        env.variables,
        env.module,
        env.struct_types,
        env.struct_field_indices,
    );

    if let Some(BasicTypeEnum::PointerType(ptr_ty)) = expected_type {
        if addr.get_type() != ptr_ty {
            return env
                .builder
                .build_bit_cast(
                    addr.as_basic_value_enum(),
                    ptr_ty.as_basic_type_enum(),
                    "addrof_cast",
                )
                .unwrap()
                .as_basic_value_enum();
        }
    }

    addr.as_basic_value_enum()
}
