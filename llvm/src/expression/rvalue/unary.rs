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

//! Numeric negation, logical negation, and bitwise complement lowering.
//!
//! Logical negation normalizes non-boolean integers by comparing them with zero;
//! bitwise complement preserves the operand width.

use super::ExprGenEnv;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum};
use inkwell::IntPredicate;
use parser::ast::{Expression, Operator};

pub(crate) fn gen<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    operator: &Operator,
    expr: &Expression,
    expected_type: Option<BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    let val = env.gen(expr, expected_type);

    match (operator, val) {
        // - (negation)
        (Operator::Neg, BasicValueEnum::IntValue(iv)) => {
            let zero = iv.get_type().const_zero();
            env.builder.build_int_sub(zero, iv, "neg").unwrap().as_basic_value_enum()
        },

        (Operator::Neg, BasicValueEnum::FloatValue(fv)) => {
            env.builder.build_float_neg(fv, "fneg").unwrap().as_basic_value_enum()
        },

        // ! (logical not)
        (Operator::LogicalNot, BasicValueEnum::IntValue(iv))
        | (Operator::Not, BasicValueEnum::IntValue(iv)) => {
            let bw = iv.get_type().get_bit_width();
            if bw == 1 {
                env.builder.build_not(iv, "lnot").unwrap().as_basic_value_enum()
            } else {
                let zero = iv.get_type().const_zero();
                env.builder
                    .build_int_compare(IntPredicate::EQ, iv, zero, "lnot")
                    .unwrap()
                    .as_basic_value_enum()
            }
        },

        // ~ (bitwise not)
        (Operator::BitwiseNot, BasicValueEnum::IntValue(iv)) => {
            env.builder.build_not(iv, "bnot").unwrap().as_basic_value_enum()
        },

        _ => panic!("Unsupported unary operator {:?} for value {:?}", operator, val),
    }
}
