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
            let (addr, load_ty) = generate_address_and_type_ir(env, inner_expr);
            return env.builder.build_load(load_ty, addr, "deref_load").unwrap();
        },
        _ => {},
    }

    let pointee = match env.wave_type(inner_expr) {
        Some(WaveType::Pointer(inner)) => *inner,
        Some(WaveType::String) => WaveType::Byte,
        other => panic!("typed HIR did not provide a pointer type for deref: {:?}", other),
    };
    let load_ty =
        wave_type_to_llvm_type(env.context, &pointee, env.struct_types, TypeFlavor::Value);
    let pointer = env.gen(inner_expr, None).into_pointer_value();
    env.builder.build_load(load_ty, pointer, "deref_load").unwrap()
}

pub(crate) fn gen_addressof<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    inner_expr: &Expression,
    expected_type: Option<BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    // The frontend records the pointee array layout and every element's
    // conversion. Do not infer the storage width from the first LLVM value.
    if matches!(inner_expr, Expression::ArrayLiteral(_)) {
        let array_type = env
            .program
            .expected_type_of(inner_expr)
            .filter(|ty| matches!(ty, WaveType::Array(_, _)))
            .expect("ICE: addressed array literal missing its HIR array context");
        let array_type =
            wave_type_to_llvm_type(env.context, array_type, env.struct_types, TypeFlavor::Value);
        let value = env.gen(inner_expr, Some(array_type));
        let storage = env.builder.build_alloca(array_type, "addressed_array").unwrap();
        env.builder.build_store(storage, value).unwrap();
        return storage.into();
    }

    // normal &lvalue : address
    let addr = generate_address_ir(env, inner_expr);

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
