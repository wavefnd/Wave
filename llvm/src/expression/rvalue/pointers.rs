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

//! Address-of and dereference expression lowering.
//!
//! LLVM pointers are opaque, so dereference loads recover their value type from
//! typed HIR and the projected storage type. Address-of returns the existing lvalue
//! address and never allocates replacement storage.

use super::ExprGenEnv;
use crate::codegen::types::{wave_type_to_llvm_type, TypeFlavor};
use crate::codegen::{generate_address_and_type_ir, generate_address_ir};
use crate::statement::variable::{coerce_basic_value, wave_type_is_unsigned, CoercionMode};
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::{Expression, WaveType};

pub(crate) fn gen_deref<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    inner_expr: &Expression,
) -> BasicValueEnum<'ctx> {
    match inner_expr {
        Expression::Grouped(inner) => return gen_deref(env, inner),

        // Wave's projected deref reads the field/element itself. Its storage
        // type may be a pointer; do not strip another pointer layer or infer
        // the load width from the surrounding expression's expected type.
        Expression::IndexAccess { .. } | Expression::FieldAccess { .. } => {
            let (addr, load_ty) = generate_address_and_type_ir(
                env.context,
                env.builder,
                env.program,
                inner_expr,
                env.variables,
                env.module,
                env.struct_types,
                env.struct_field_indices,
            );
            return env.builder.build_load(load_ty, addr, "deref_load").unwrap();
        }
        _ => {}
    }

    let pointee = match env.wave_type(inner_expr) {
        Some(WaveType::Pointer(inner)) => *inner,
        Some(WaveType::String) => WaveType::Byte,
        other => panic!(
            "typed HIR did not provide a pointer type for deref: {:?}",
            other
        ),
    };
    let load_ty =
        wave_type_to_llvm_type(env.context, &pointee, env.struct_types, TypeFlavor::Value);
    let pointer = env.gen(inner_expr, None).into_pointer_value();
    env.builder
        .build_load(load_ty, pointer, "deref_load")
        .unwrap()
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
                    wave_type_is_unsigned(env.wave_type(expr).as_ref()),
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
        env.program,
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
