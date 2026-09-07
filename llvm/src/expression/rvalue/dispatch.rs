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

//! Exhaustive dispatch from Wave expression nodes to specialized lowerers.
//!
//! New expression variants must be routed here and added to semantic analysis.
//! The expected LLVM type is forwarded only to forms whose value representation
//! may depend on surrounding context.

use super::*;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;
use parser::ast::Expression;

pub(crate) fn gen_expr<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    expr: &Expression,
    expected_type: Option<BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    match expr {
        Expression::Located { .. } => unreachable!("typed HIR detaches source wrappers"),
        Expression::Literal(lit) => literals::gen(env, lit, expected_type),
        Expression::Null => literals::gen_null(env, expected_type),
        Expression::Variable(name) => {
            if env.program.variant_construction_of(expr).is_some() {
                variants::gen_constructor(env, expr, &[])
            } else {
                variables::gen(env, name, expected_type)
            }
        }

        Expression::Deref(inner) => pointers::gen_deref(env, inner),
        Expression::AddressOf(inner) => pointers::gen_addressof(env, inner, expected_type),

        Expression::MethodCall { object, name, args } => {
            calls::gen_method_call(env, object, name, args)
        }
        Expression::FunctionCall {
            name,
            type_args,
            args,
        } => {
            if env.program.variant_construction_of(expr).is_some() {
                variants::gen_constructor(env, expr, args)
            } else {
                calls::gen_function_call(env, name, type_args, args, expected_type)
            }
        }
        Expression::Cast { expr, target_type } => cast::gen(env, expr, target_type),

        Expression::AssignOperation {
            target,
            operator,
            value,
        } => assign::gen_assign_operation(env, target, operator, value),
        Expression::Assignment { target, value } => assign::gen_assignment(env, target, value),

        Expression::BinaryExpression {
            left,
            operator,
            right,
        } => binary::gen(env, left, operator, right, expected_type),

        Expression::IndexAccess { target, index } => index::gen(env, target, index),

        Expression::AsmBlock {
            instructions,
            inputs,
            outputs,
            clobbers,
        } => asm::gen(env, instructions, inputs, outputs, clobbers),

        Expression::StructLiteral { name, fields } => {
            structs::gen_struct_literal(env, name, fields)
        }
        Expression::FieldAccess { object, field } => structs::gen_field_access(env, object, field),

        Expression::Unary { operator, expr } => unary::gen(env, operator, expr, expected_type),
        Expression::IncDec { kind, target } => incdec::gen(env, kind, target),

        Expression::Grouped(inner) => env.gen(inner, expected_type),
        Expression::ArrayLiteral(elements) => {
            arrays::gen_array_literal(env, elements, expected_type)
        }
    }
}
