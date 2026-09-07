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

//! LLVM constants and globals for source literals.
//!
//! Unsuffixed numeric literals require an expected type when context determines
//! width or signedness. `null` likewise requires a pointer expectation; codegen
//! does not invent a pointee type for an untyped null literal.

use super::ExprGenEnv;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum};
use inkwell::AddressSpace;
use parser::ast::Literal;

fn parse_int_as_f64(s: &str) -> Option<f64> {
    lexer::number::IntegerLiteral::parse(s)?.to_f64()
}

fn is_zero_int_literal(s: &str) -> bool {
    lexer::number::IntegerLiteral::parse(s).is_some_and(|n| n.is_zero())
}

pub(crate) fn gen_null<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    expected_type: Option<BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    match expected_type {
        Some(BasicTypeEnum::PointerType(ptr_ty)) => ptr_ty.const_null().as_basic_value_enum(),
        Some(other) => panic!(
            "null literal can only be used with pointer expected type, got {:?}",
            other
        ),
        None => env
            .context
            .ptr_type(AddressSpace::default())
            .const_null()
            .as_basic_value_enum(),
    }
}

pub(crate) fn gen<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    lit: &Literal,
    expected_type: Option<BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    match lit {
        Literal::Int(v) => match expected_type {
            Some(BasicTypeEnum::IntType(int_ty)) => {
                let s = v.as_str();
                let (neg, radix, digits) =
                    crate::codegen::number::parse_integer(s).expect("validated integer literal");

                let mut iv = int_ty
                    .const_int_from_string(&digits, radix)
                    .unwrap_or_else(|| panic!("invalid int literal: {}", s));

                if neg {
                    iv = iv.const_neg();
                }

                iv.as_basic_value_enum()
            }

            Some(BasicTypeEnum::ArrayType(at)) => {
                let elem = at.get_element_type();
                return gen(env, lit, Some(elem));
            }

            Some(BasicTypeEnum::PointerType(ptr_ty)) => {
                if is_zero_int_literal(v) {
                    ptr_ty.const_null().as_basic_value_enum()
                } else {
                    panic!(
                        "integer literals cannot initialize pointers; use `null` or an explicit cast",
                    )
                }
            }

            Some(BasicTypeEnum::FloatType(ft)) => {
                let f = parse_int_as_f64(v)
                    .unwrap_or_else(|| panic!("invalid float literal from int token: {}", v));
                ft.const_float(f).as_basic_value_enum()
            }

            None => {
                // Default untyped integer literal to i32 when no contextual type exists.
                let int_ty = env.context.i32_type();
                let s = v.as_str();
                let (neg, radix, digits) =
                    crate::codegen::number::parse_integer(s).expect("validated integer literal");

                let mut iv = int_ty
                    .const_int_from_string(&digits, radix)
                    .unwrap_or_else(|| panic!("invalid int literal: {}", s));

                if neg {
                    iv = iv.const_neg();
                }

                iv.as_basic_value_enum()
            }

            _ => panic!(
                "Unsupported expected_type for int literal: {:?}",
                expected_type
            ),
        },

        Literal::Float(value) => match expected_type {
            Some(BasicTypeEnum::FloatType(float_ty)) => {
                float_ty.const_float(*value).as_basic_value_enum()
            }
            Some(BasicTypeEnum::IntType(int_ty)) => env
                .builder
                .build_float_to_signed_int(
                    env.context.f32_type().const_float(*value),
                    int_ty,
                    "f32_to_int",
                )
                .unwrap()
                .as_basic_value_enum(),
            None => env
                .context
                .f32_type()
                .const_float(*value)
                .as_basic_value_enum(),
            _ => panic!("Unsupported expected_type for float"),
        },

        Literal::String(value) => {
            let bytes = value.as_bytes();
            let mut null_terminated = bytes.to_vec();
            null_terminated.push(0);

            let global_name = format!("str_lit_{}", value.replace(" ", "_"));

            let str_type = env
                .context
                .i8_type()
                .array_type(null_terminated.len() as u32);

            let global = env.module.add_global(str_type, None, &global_name);
            global.set_initializer(&env.context.const_string(&null_terminated, false));
            global.set_constant(true);

            let zero = env.context.i32_type().const_zero();
            let indices = [zero, zero];

            let gep = unsafe {
                env.builder
                    .build_in_bounds_gep(str_type, global.as_pointer_value(), &indices, "str_gep")
                    .unwrap()
            };

            gep.as_basic_value_enum()
        }

        Literal::Bool(v) => env
            .context
            .bool_type()
            .const_int(if *v { 1 } else { 0 }, false)
            .as_basic_value_enum(),

        Literal::Char(c) => env
            .context
            .i8_type()
            .const_int(*c as u64, false)
            .as_basic_value_enum(),
        Literal::Byte(b) => env
            .context
            .i8_type()
            .const_int(*b as u64, false)
            .as_basic_value_enum(),
    }
}
