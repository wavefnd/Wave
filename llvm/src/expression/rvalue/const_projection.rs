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

//! Value-only projection for aggregate constants.
//!
//! Constants live in the LLVM value environment and deliberately have no
//! mutable storage address. Field and element reads rooted in a constant must
//! therefore stay on the rvalue path instead of entering address lowering.

use super::ExprGenEnv;
use crate::codegen::types::{wave_type_to_llvm_type, TypeFlavor};
use inkwell::types::BasicType;
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::{Expression, WaveType};

fn is_rooted_in_const(env: &ExprGenEnv<'_, '_>, expression: &Expression) -> bool {
    match expression {
        Expression::Variable(name) => {
            !env.variables.contains_key(name) && env.global_consts.contains_key(name)
        },
        Expression::Grouped(inner) => is_rooted_in_const(env, inner),
        Expression::FieldAccess { object, .. } => is_rooted_in_const(env, object),
        Expression::IndexAccess { target, .. } => is_rooted_in_const(env, target),
        _ => false,
    }
}

fn normalize_struct_name(raw: &str) -> &str {
    raw.strip_prefix("struct.").unwrap_or(raw).trim_start_matches('%')
}

pub(crate) fn try_gen_field_access<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    object: &Expression,
    field: &str,
) -> Option<BasicValueEnum<'ctx>> {
    if !is_rooted_in_const(env, object) {
        return None;
    }

    let object_value = env.gen(object, None);
    match object_value {
        BasicValueEnum::StructValue(value) => {
            let struct_type = value.get_type();
            let raw_name = struct_type
                .get_name()
                .and_then(|name| name.to_str().ok())
                .unwrap_or_else(|| panic!("constant field projection requires a named struct"));
            let struct_name = normalize_struct_name(raw_name);
            let field_index = env
                .struct_field_indices
                .get(struct_name)
                .and_then(|fields| fields.get(field))
                .copied()
                .unwrap_or_else(|| {
                    panic!(
                        "typed HIR field '{}.{}' is missing from the LLVM field map",
                        struct_name, field
                    )
                });

            Some(env.builder.build_extract_value(value, field_index, "const.field").unwrap_or_else(
                |error| {
                    panic!(
                        "typed HIR produced an invalid constant field projection '{}.{}': {}",
                        struct_name, field, error
                    )
                },
            ))
        },
        BasicValueEnum::PointerValue(pointer) => {
            let WaveType::Pointer(pointee) = env.wave_type(object).unwrap_or_else(|| {
                panic!("typed HIR omitted the type of constant pointer field access")
            }) else {
                panic!("constant pointer field access has a non-pointer semantic type");
            };
            let WaveType::Struct(struct_name) = pointee.as_ref() else {
                panic!("constant pointer field access does not point to a struct");
            };
            let struct_type = *env
                .struct_types
                .get(struct_name)
                .unwrap_or_else(|| panic!("struct type '{}' not found", struct_name));
            let field_index = env
                .struct_field_indices
                .get(struct_name)
                .and_then(|fields| fields.get(field))
                .copied()
                .unwrap_or_else(|| panic!("unknown field '{}.{}'", struct_name, field));
            let field_type = struct_type
                .get_field_type_at_index(field_index)
                .unwrap_or_else(|| panic!("invalid field index for '{}.{}'", struct_name, field));
            let field_pointer = env
                .builder
                .build_struct_gep(struct_type, pointer, field_index, "const.ptr.field")
                .unwrap();

            Some(
                env.builder
                    .build_load(field_type, field_pointer, "const.ptr.field.load")
                    .unwrap()
                    .as_basic_value_enum(),
            )
        },
        other => panic!(
            "typed HIR allowed field access on a non-aggregate constant value: {:?}",
            other.get_type()
        ),
    }
}

pub(crate) fn try_gen_index_access<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    target: &Expression,
    index: &Expression,
) -> Option<BasicValueEnum<'ctx>> {
    if !is_rooted_in_const(env, target) {
        return None;
    }

    let target_value = env.gen(target, None);
    let index_value = crate::codegen::address::generate_index_ir(env, index);

    match target_value {
        BasicValueEnum::ArrayValue(array) => {
            let array_type = array.get_type();
            if let Some(index) = index_value.get_zero_extended_constant() {
                if index < u64::from(array_type.len()) {
                    return Some(
                        env.builder
                            .build_extract_value(array, index as u32, "const.element")
                            .unwrap(),
                    );
                }
            }

            // LLVM extractvalue requires a fixed immediate index. Keep dynamic
            // indexing value-only by materializing an internal temporary whose
            // address is never entered in the Wave variable environment.
            let storage = env.builder.build_alloca(array_type, "const.array.storage").unwrap();
            env.builder.build_store(storage, array).unwrap();
            let zero = env.context.i32_type().const_zero();
            let element_pointer = unsafe {
                env.builder
                    .build_in_bounds_gep(
                        array_type,
                        storage,
                        &[zero, index_value],
                        "const.element.ptr",
                    )
                    .unwrap()
            };
            Some(
                env.builder
                    .build_load(
                        array_type.get_element_type(),
                        element_pointer,
                        "const.element.load",
                    )
                    .unwrap()
                    .as_basic_value_enum(),
            )
        },
        BasicValueEnum::PointerValue(pointer) => {
            let target_type = env.wave_type(target).unwrap_or_else(|| {
                panic!("typed HIR omitted the type of constant pointer index access")
            });
            let (element_type, element_pointer) = match target_type {
                WaveType::Pointer(pointee) => match pointee.as_ref() {
                    WaveType::Array(element, length) => {
                        let array_type = wave_type_to_llvm_type(
                            env.context,
                            &WaveType::Array(element.clone(), *length),
                            env.struct_types,
                            TypeFlavor::Value,
                        )
                        .into_array_type();
                        let zero = env.context.i32_type().const_zero();
                        let element_pointer = unsafe {
                            env.builder
                                .build_in_bounds_gep(
                                    array_type,
                                    pointer,
                                    &[zero, index_value],
                                    "const.ptr.array.element",
                                )
                                .unwrap()
                        };
                        (array_type.get_element_type(), element_pointer)
                    },
                    element => {
                        let element_type = wave_type_to_llvm_type(
                            env.context,
                            element,
                            env.struct_types,
                            TypeFlavor::Value,
                        );
                        let element_pointer = unsafe {
                            env.builder
                                .build_in_bounds_gep(
                                    element_type,
                                    pointer,
                                    &[index_value],
                                    "const.ptr.element",
                                )
                                .unwrap()
                        };
                        (element_type, element_pointer)
                    },
                },
                WaveType::String => {
                    let element_type = env.context.i8_type().as_basic_type_enum();
                    let element_pointer = unsafe {
                        env.builder
                            .build_in_bounds_gep(
                                element_type,
                                pointer,
                                &[index_value],
                                "const.string.element",
                            )
                            .unwrap()
                    };
                    (element_type, element_pointer)
                },
                other => panic!("constant pointer index access has unsupported type {:?}", other),
            };

            Some(
                env.builder
                    .build_load(element_type, element_pointer, "const.ptr.element.load")
                    .unwrap()
                    .as_basic_value_enum(),
            )
        },
        other => panic!(
            "typed HIR allowed index access on a non-indexable constant value: {:?}",
            other.get_type()
        ),
    }
}
