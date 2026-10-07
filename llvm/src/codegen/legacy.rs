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

//! Compatibility helpers for the former token-based code-generation API.
//!
//! Current lowering uses semantic [`WaveType`](parser::ast::WaveType) values.
//! Do not extend this module with new language types; migrate remaining callers
//! to `codegen::types` instead.

use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{FunctionValue, PointerValue};
use inkwell::AddressSpace;

use lexer::token::TokenType;

pub fn get_llvm_type<'a>(context: &'a Context, ty: &TokenType) -> BasicTypeEnum<'a> {
    match ty {
        TokenType::TypeInt(bits) => {
            context.custom_width_int_type(*bits as u32).as_basic_type_enum()
        },
        TokenType::TypeUint(bits) => {
            context.custom_width_int_type(*bits as u32).as_basic_type_enum()
        },
        TokenType::TypeFloat(bits) => match bits {
            32 => context.f32_type().as_basic_type_enum(),
            64 => context.f64_type().as_basic_type_enum(),
            128 => context.f128_type().as_basic_type_enum(),
            _ => panic!("Unsupported float size: {}", bits),
        },
        TokenType::TypeBool => context.bool_type().as_basic_type_enum(),
        TokenType::TypeChar => context.i8_type().as_basic_type_enum(),
        TokenType::TypeByte => context.i8_type().as_basic_type_enum(),
        TokenType::TypePointer(inner_type) => {
            let _inner_llvm_type = get_llvm_type(context, inner_type);
            context.ptr_type(AddressSpace::default()).as_basic_type_enum()
        },
        TokenType::TypeArray(inner_type, size) => {
            let inner_llvm_type = get_llvm_type(context, inner_type);
            inner_llvm_type.array_type(*size as u32).as_basic_type_enum()
        },
        TokenType::TypeString => context.ptr_type(AddressSpace::default()).as_basic_type_enum(),
        _ => panic!("Unsupported type: {:?}", ty),
    }
}

#[allow(dead_code)]
/// Creates a legacy `i32` stack slot at the builder's current insertion point.
///
/// # Safety
///
/// This function retains an unsafe signature for API compatibility and imposes
/// no additional caller-side memory-safety requirements.
pub unsafe fn create_alloc<'a>(
    context: &'a Context,
    builder: &'a inkwell::builder::Builder<'a>,
    _function: FunctionValue<'a>,
    name: &'a str,
) -> PointerValue<'a> {
    let alloca = builder.build_alloca(context.i32_type(), name).unwrap();
    alloca
}
