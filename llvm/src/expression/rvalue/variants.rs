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

//! Construction of typed variant values from frontend-resolved case metadata.

use super::ExprGenEnv;
use crate::codegen::types::{wave_type_to_llvm_type, TypeFlavor};
use crate::statement::variable::{coerce_basic_value, wave_type_is_unsigned, CoercionMode};
use inkwell::types::BasicType;
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::{Expression, WaveType};

pub(crate) fn gen_constructor<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    expression: &Expression,
    args: &[Expression],
) -> BasicValueEnum<'ctx> {
    let construction = env
        .program
        .variant_construction_of(expression)
        .cloned()
        .expect("variant constructor reached LLVM without typed HIR metadata");
    let WaveType::Variant(name) = &construction.variant_type else {
        panic!("variant constructor metadata has a non-variant type");
    };
    let variant_ty =
        *env.struct_types.get(name).unwrap_or_else(|| panic!("variant type '{}' not found", name));
    let value_ptr =
        env.builder.build_alloca(variant_ty, &format!("variant.{}.value", name)).unwrap();
    env.builder.build_store(value_ptr, variant_ty.const_zero()).unwrap();

    let tag_ptr =
        env.builder.build_struct_gep(variant_ty, value_ptr, 0, "variant.tag.ptr").unwrap();
    env.builder
        .build_store(
            tag_ptr,
            env.context.i32_type().const_int(construction.discriminant as u64, false),
        )
        .unwrap();

    let payload_ty = crate::codegen::variants::payload_type(
        env.context,
        &construction.payload_types,
        env.struct_types,
    );
    if args.len() != construction.payload_types.len() {
        panic!(
            "variant constructor '{}::{}' payload count changed after semantic validation",
            name, construction.case_name
        );
    }
    let payload_ptr = env.builder.build_alloca(payload_ty, "variant.payload.value").unwrap();
    env.builder.build_store(payload_ptr, payload_ty.const_zero()).unwrap();

    for (index, (argument, payload_wave_type)) in
        args.iter().zip(&construction.payload_types).enumerate()
    {
        let expected = wave_type_to_llvm_type(
            env.context,
            payload_wave_type,
            env.struct_types,
            TypeFlavor::AbiC,
        );
        let raw = env.gen(argument, Some(expected));
        let value = coerce_basic_value(
            env.context,
            env.builder,
            raw,
            expected,
            &format!("variant.payload.{}.cast", index),
            CoercionMode::Implicit,
            wave_type_is_unsigned(env.wave_type(argument).as_ref()),
        );
        let field_ptr = env
            .builder
            .build_struct_gep(payload_ty, payload_ptr, index as u32, "variant.payload.ptr")
            .unwrap();
        env.builder.build_store(field_ptr, value).unwrap();
    }

    let payload = env
        .builder
        .build_load(payload_ty.as_basic_type_enum(), payload_ptr, "variant.payload")
        .unwrap();
    let case_ptr =
        env.builder.build_struct_gep(variant_ty, value_ptr, 2, "variant.case.ptr").unwrap();
    env.builder.build_store(case_ptr, payload).unwrap();
    env.builder
        .build_load(variant_ty.as_basic_type_enum(), value_ptr, "variant.value")
        .unwrap()
        .as_basic_value_enum()
}
