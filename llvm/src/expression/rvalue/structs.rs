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

//! Struct literal construction and field-value access.
//!
//! Field names are resolved through the declaration-built index map. Aggregate
//! fields remain addresses when required by later lowering; scalar fields are
//! loaded as values.

use super::ExprGenEnv;
use crate::statement::variable::{coerce_basic_value, wave_type_is_unsigned, CoercionMode};
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::{Expression, WaveType};

pub(crate) fn gen_struct_literal<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    name: &str,
    fields: &[(String, Expression)],
) -> BasicValueEnum<'ctx> {
    let struct_ty =
        *env.struct_types.get(name).unwrap_or_else(|| panic!("Struct type '{}' not found", name));

    let field_indices = env
        .struct_field_indices
        .get(name)
        .unwrap_or_else(|| panic!("Field index map for struct '{}' not found", name));

    let tmp_alloca = env.builder.build_alloca(struct_ty, &format!("tmp_{}_literal", name)).unwrap();

    for (field_name, field_expr) in fields {
        let idx = *field_indices
            .get(field_name)
            .unwrap_or_else(|| panic!("Field '{}' not found in struct '{}'", field_name, name));

        let expected_field_ty: BasicTypeEnum<'ctx> = struct_ty
            .get_field_type_at_index(idx)
            .unwrap_or_else(|| panic!("No field type at index {} for struct '{}'", idx, name));

        let field_val = env.gen(field_expr, Some(expected_field_ty));
        let field_val = coerce_basic_value(
            env.context,
            env.builder,
            field_val,
            expected_field_ty,
            &format!("{}_{}_literal_cast", name, field_name),
            CoercionMode::Implicit,
            wave_type_is_unsigned(env.wave_type(field_expr).as_ref()),
        );

        let field_ptr = env
            .builder
            .build_struct_gep(struct_ty, tmp_alloca, idx, &format!("{}.{}", name, field_name))
            .unwrap();

        env.builder.build_store(field_ptr, field_val).unwrap();
    }

    env.builder
        .build_load(struct_ty.as_basic_type_enum(), tmp_alloca, &format!("{}_literal_val", name))
        .unwrap()
        .as_basic_value_enum()
}

pub(crate) fn gen_field_access<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    object: &Expression,
    field: &str,
) -> BasicValueEnum<'ctx> {
    if let Some(value) = super::const_projection::try_gen_field_access(env, object, field) {
        return value;
    }

    // Keep the original receiver identity so typed HIR remains authoritative.
    // A returned struct is a value; reconstructing an lvalue would require an
    // address that function/method call results do not have.
    let receiver_type = env.wave_type(object).expect("field receiver has a resolved HIR type");
    let struct_name = match &receiver_type {
        WaveType::Struct(name) => name,
        WaveType::Pointer(inner) => match inner.as_ref() {
            WaveType::Struct(name) => name,
            other => panic!("field receiver points to non-struct type: {other:?}"),
        },
        other => panic!("field receiver is not a struct: {other:?}"),
    };
    let index = *env
        .struct_field_indices
        .get(struct_name)
        .and_then(|fields| fields.get(field))
        .expect("field index was established by semantic validation");
    match env.gen(object, None) {
        BasicValueEnum::StructValue(value) => env
            .builder
            .build_extract_value(value, index, &format!("field_{field}"))
            .expect("field index matches the resolved struct"),
        BasicValueEnum::PointerValue(pointer) => {
            let struct_type =
                *env.struct_types.get(struct_name).expect("resolved struct has an LLVM type");
            let field_type = struct_type
                .get_field_type_at_index(index)
                .expect("resolved field has an LLVM type");
            let pointer = env
                .builder
                .build_struct_gep(struct_type, pointer, index, &format!("field_ptr_{field}"))
                .expect("typed pointer receiver supports field projection");
            env.builder
                .build_load(field_type, pointer, &format!("load_field_{field}"))
                .expect("field pointer has the resolved field type")
        },
        other => panic!("resolved struct receiver lowered to an invalid LLVM value: {other:?}"),
    }
}
