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

//! Resolution and loading of constants, locals, and static variables.
//!
//! Arrays evaluate to their storage address rather than an aggregate load, while
//! other variables load using their Wave type. Expected pointer types guide
//! opaque-pointer loads without redefining the variable's semantic type.

use super::ExprGenEnv;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum};

use crate::codegen::types::{wave_type_to_llvm_type, TypeFlavor};
use parser::ast::WaveType;

pub(crate) fn gen<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    var_name: &str,
    expected_type: Option<BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    if var_name == "true" {
        return env.context.bool_type().const_int(1, false).as_basic_value_enum();
    } else if var_name == "false" {
        return env.context.bool_type().const_int(0, false).as_basic_value_enum();
    }

    if let Some(var_info) = env.variables.get(var_name) {
        let ptr = var_info.ptr;

        if let Some(et) = expected_type {
            if et.is_pointer_type() {
                let loaded =
                    env.builder.build_load(et, ptr, &format!("load_{}", var_name)).unwrap();

                let expected_ptr = et.into_pointer_type();
                if loaded.get_type() != BasicTypeEnum::from(expected_ptr) {
                    return env
                        .builder
                        .build_bit_cast(loaded, expected_ptr, &format!("{}_as_ptr", var_name))
                        .unwrap()
                        .as_basic_value_enum();
                }

                return loaded;
            }
        }

        match &var_info.ty {
            WaveType::Array(_, _) => ptr.as_basic_value_enum(),

            _ => {
                let load_ty = wave_type_to_llvm_type(
                    env.context,
                    &var_info.ty,
                    env.struct_types,
                    TypeFlavor::Value,
                );

                env.builder
                    .build_load(load_ty, ptr, &format!("load_{}", var_name))
                    .unwrap()
                    .as_basic_value_enum()
            },
        }
    } else if let Some(const_val) = env.global_consts.get(var_name) {
        *const_val
    } else if env.module.get_function(var_name).is_some() {
        panic!("Error: '{}' is a function name, not a variable", var_name);
    } else {
        panic!("variable '{}' not found in current scope", var_name);
    }
}
