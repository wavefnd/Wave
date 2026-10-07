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

//! Lower already-decided Wave numeric semantics. No promotion policy lives here.
use super::types::{wave_type_to_llvm_type, TypeFlavor};
use hir::conversions::{unsigned, ConversionInfo, ConversionKind};
use inkwell::{
    builder::Builder,
    context::Context,
    intrinsics::Intrinsic,
    module::Module,
    types::StructType,
    values::{BasicValue, BasicValueEnum},
    FloatPredicate, IntPredicate,
};
use parser::ast::{Operator, WaveType};
use std::collections::HashMap;

pub(crate) fn apply<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    structs: &HashMap<String, StructType<'ctx>>,
    value: BasicValueEnum<'ctx>,
    info: &ConversionInfo,
) -> BasicValueEnum<'ctx> {
    use ConversionKind::*;
    let source = wave_type_to_llvm_type(context, &info.source_type, structs, TypeFlavor::Value);
    let target = wave_type_to_llvm_type(context, &info.target_type, structs, TypeFlavor::Value);
    assert_eq!(value.get_type(), source, "ICE: conversion source differs from verified HIR");
    match info.kind {
        Identity | ReinterpretInteger => {
            assert_eq!(source, target);
            value
        },
        SignExtend => builder
            .build_int_s_extend(value.into_int_value(), target.into_int_type(), "convert.sext")
            .unwrap()
            .into(),
        ZeroExtend => builder
            .build_int_z_extend(value.into_int_value(), target.into_int_type(), "convert.zext")
            .unwrap()
            .into(),
        IntegerToBool => builder
            .build_int_compare(
                IntPredicate::NE,
                value.into_int_value(),
                source.into_int_type().const_zero(),
                "convert.bool",
            )
            .unwrap()
            .into(),
        FloatToBool => builder
            .build_float_compare(
                FloatPredicate::UNE,
                value.into_float_value(),
                source.into_float_type().const_zero(),
                "convert.bool",
            )
            .unwrap()
            .into(),
        Truncate => builder
            .build_int_truncate(value.into_int_value(), target.into_int_type(), "convert.trunc")
            .unwrap()
            .into(),
        SignedToFloat => builder
            .build_signed_int_to_float(
                value.into_int_value(),
                target.into_float_type(),
                "convert.sitofp",
            )
            .unwrap()
            .into(),
        UnsignedToFloat => builder
            .build_unsigned_int_to_float(
                value.into_int_value(),
                target.into_float_type(),
                "convert.uitofp",
            )
            .unwrap()
            .into(),
        FloatToSigned | FloatToUnsigned => {
            let input = value.into_float_value();
            let ty = input.get_type();
            let signed = info.kind == FloatToSigned;
            let bits = target.into_int_type().get_bit_width();
            let range = hir::conversions::float_integer_range(&info.source_type, &info.target_type);
            let low = builder
                .build_float_compare(
                    if range.lower_inclusive { FloatPredicate::OGE } else { FloatPredicate::OGT },
                    input,
                    ty.const_float(range.lower),
                    "convert.lower",
                )
                .unwrap();
            let high = builder
                .build_float_compare(
                    FloatPredicate::OLT,
                    input,
                    ty.const_float(range.upper),
                    "convert.upper",
                )
                .unwrap();
            // Very wide integer bounds may round to infinity in the source float.
            let finite_low = builder
                .build_float_compare(
                    FloatPredicate::OGT,
                    input,
                    ty.const_float(f64::NEG_INFINITY),
                    "convert.finite",
                )
                .unwrap();
            let valid = builder.build_and(low, high, "convert.range").unwrap();
            let valid = builder.build_and(valid, finite_low, "convert.valid").unwrap();
            trap_unless(context, builder, module, valid);
            if bits > 64 && input.get_constant().is_none() {
                return wide_float_to_int(context, builder, input, target.into_int_type());
            }
            if signed {
                builder
                    .build_float_to_signed_int(input, target.into_int_type(), "convert.fptosi")
                    .unwrap()
                    .into()
            } else {
                builder
                    .build_float_to_unsigned_int(input, target.into_int_type(), "convert.fptoui")
                    .unwrap()
                    .into()
            }
        },
        FloatExtend | FloatTruncate => builder
            .build_float_cast(value.into_float_value(), target.into_float_type(), "convert.float")
            .unwrap()
            .into(),
        PointerToInteger => builder
            .build_ptr_to_int(value.into_pointer_value(), target.into_int_type(), "convert.ptrint")
            .unwrap()
            .into(),
        IntegerToPointer => builder
            .build_int_to_ptr(value.into_int_value(), target.into_pointer_type(), "convert.intptr")
            .unwrap()
            .into(),
        PointerCast => builder
            .build_pointer_cast(
                value.into_pointer_value(),
                target.into_pointer_type(),
                "convert.ptr",
            )
            .unwrap()
            .into(),
    }
}

pub(crate) fn binary<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    left: BasicValueEnum<'ctx>,
    operator: &Operator,
    right: BasicValueEnum<'ctx>,
    computation: &WaveType,
    shift_count_type: Option<&WaveType>,
) -> BasicValueEnum<'ctx> {
    if let Some(count_type) = shift_count_type {
        let lhs = left.into_int_value();
        let rhs = right.into_int_value();
        let count_ty = context.custom_width_int_type(rhs.get_type().get_bit_width().max(16));
        let count = builder
            .build_int_cast_sign_flag(rhs, count_ty, !unsigned(count_type), "shift.count")
            .unwrap();
        let valid = builder
            .build_int_compare(
                IntPredicate::ULT,
                count,
                count_ty.const_int(lhs.get_type().get_bit_width() as u64, false),
                "shift.valid",
            )
            .unwrap();
        trap_unless(context, builder, module, valid);
        let count = builder.build_int_cast(count, lhs.get_type(), "shift.narrow").unwrap();
        return match operator {
            Operator::ShiftLeft => builder.build_left_shift(lhs, count, "shl").unwrap().into(),
            Operator::ShiftRight => {
                builder.build_right_shift(lhs, count, !unsigned(computation), "shr").unwrap().into()
            },
            _ => panic!("ICE: shift facts on a non-shift operation"),
        };
    }
    assert_eq!(
        left.get_type(),
        right.get_type(),
        "ICE: HIR operands must have the computation type"
    );
    let operation_unsigned = unsigned(computation);
    match (left, right) {
        (BasicValueEnum::IntValue(l_casted), BasicValueEnum::IntValue(r_casted)) => {
            let result = match operator {
                Operator::Add => builder.build_int_add(l_casted, r_casted, "addtmp"),
                Operator::Subtract => builder.build_int_sub(l_casted, r_casted, "subtmp"),
                Operator::Multiply => builder.build_int_mul(l_casted, r_casted, "multmp"),
                Operator::Divide if operation_unsigned => {
                    builder.build_int_unsigned_div(l_casted, r_casted, "divtmp")
                },
                Operator::Divide => builder.build_int_signed_div(l_casted, r_casted, "divtmp"),
                Operator::Remainder if operation_unsigned => {
                    builder.build_int_unsigned_rem(l_casted, r_casted, "modtmp")
                },
                Operator::Remainder => builder.build_int_signed_rem(l_casted, r_casted, "modtmp"),
                Operator::ShiftLeft => builder.build_left_shift(l_casted, r_casted, "shl"),
                Operator::ShiftRight => {
                    let arithmetic = !operation_unsigned;
                    builder.build_right_shift(l_casted, r_casted, arithmetic, "shr")
                },
                Operator::BitwiseAnd => builder.build_and(l_casted, r_casted, "andtmp"),
                Operator::BitwiseOr => builder.build_or(l_casted, r_casted, "ortmp"),
                Operator::BitwiseXor => builder.build_xor(l_casted, r_casted, "xortmp"),

                Operator::Greater => {
                    let predicate =
                        if operation_unsigned { IntPredicate::UGT } else { IntPredicate::SGT };
                    builder.build_int_compare(predicate, l_casted, r_casted, "cmptmp")
                },
                Operator::Less => {
                    let predicate =
                        if operation_unsigned { IntPredicate::ULT } else { IntPredicate::SLT };
                    builder.build_int_compare(predicate, l_casted, r_casted, "cmptmp")
                },
                Operator::Equal => {
                    builder.build_int_compare(IntPredicate::EQ, l_casted, r_casted, "cmptmp")
                },
                Operator::NotEqual => {
                    builder.build_int_compare(IntPredicate::NE, l_casted, r_casted, "cmptmp")
                },
                Operator::GreaterEqual => {
                    let predicate =
                        if operation_unsigned { IntPredicate::UGE } else { IntPredicate::SGE };
                    builder.build_int_compare(predicate, l_casted, r_casted, "cmptmp")
                },
                Operator::LessEqual => {
                    let predicate =
                        if operation_unsigned { IntPredicate::ULE } else { IntPredicate::SLE };
                    builder.build_int_compare(predicate, l_casted, r_casted, "cmptmp")
                },

                Operator::LogicalAnd | Operator::LogicalOr => unreachable!(),

                _ => panic!("Unsupported binary operator"),
            }
            .unwrap();

            result.into()
        },
        (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
            let result: BasicValueEnum<'ctx> = match operator {
                Operator::Add => {
                    builder.build_float_add(l, r, "faddtmp").unwrap().as_basic_value_enum()
                },
                Operator::Subtract => {
                    builder.build_float_sub(l, r, "fsubtmp").unwrap().as_basic_value_enum()
                },
                Operator::Multiply => {
                    builder.build_float_mul(l, r, "fmultmp").unwrap().as_basic_value_enum()
                },
                Operator::Divide => {
                    builder.build_float_div(l, r, "fdivtmp").unwrap().as_basic_value_enum()
                },
                Operator::Remainder => {
                    builder.build_float_rem(l, r, "fmodtmp").unwrap().as_basic_value_enum()
                },

                Operator::Greater => builder
                    .build_float_compare(FloatPredicate::OGT, l, r, "fcmpgt")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Less => builder
                    .build_float_compare(FloatPredicate::OLT, l, r, "fcmplt")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::Equal => builder
                    .build_float_compare(FloatPredicate::OEQ, l, r, "fcmpeq")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::NotEqual => builder
                    .build_float_compare(FloatPredicate::UNE, l, r, "fcmpne")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::GreaterEqual => builder
                    .build_float_compare(FloatPredicate::OGE, l, r, "fcmpge")
                    .unwrap()
                    .as_basic_value_enum(),
                Operator::LessEqual => builder
                    .build_float_compare(FloatPredicate::OLE, l, r, "fcmple")
                    .unwrap()
                    .as_basic_value_enum(),

                _ => panic!("Unsupported float operator"),
            };

            result
        },
        _ => panic!("ICE: nonnumeric computation"),
    }
}

// Branch before any potentially poison-producing operation. Constant valid
// guards need no blocks, allowing the same conversions in global initializers.
fn trap_unless<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    valid: inkwell::values::IntValue<'ctx>,
) {
    if valid.get_zero_extended_constant() == Some(1) {
        return;
    }
    let parent = builder.get_insert_block().unwrap().get_parent().unwrap();
    let ok = context.append_basic_block(parent, "numeric.valid");
    let bad = context.append_basic_block(parent, "numeric.invalid");
    builder.build_conditional_branch(valid, ok, bad).unwrap();
    builder.position_at_end(bad);
    let trap = Intrinsic::find("llvm.trap").unwrap().get_declaration(module, &[]).unwrap();
    builder.build_call(trap, &[], "").unwrap();
    builder.build_unreachable().unwrap();
    builder.position_at_end(ok);
}

// Decode an already checked IEEE value, discarding fractional bits. This also covers i256..i1024
// without relying on target-specific compiler-rt/libgcc conversion helpers.
fn wide_float_to_int<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    value: inkwell::values::FloatValue<'ctx>,
    target: inkwell::types::IntType<'ctx>,
) -> BasicValueEnum<'ctx> {
    let single = value.get_type() == context.f32_type();
    let (bits, fraction, bias, exponent_mask) =
        if single { (32, 23, 127, 255) } else { (64, 52, 1023, 2047) };
    let storage = context.custom_width_int_type(bits);
    let raw = builder.build_bit_cast(value, storage, "convert.ieee").unwrap().into_int_value();
    let negative = builder
        .build_int_compare(IntPredicate::SLT, raw, storage.const_zero(), "convert.negative")
        .unwrap();
    let exponent = builder
        .build_right_shift(raw, storage.const_int(fraction, false), false, "convert.exponent")
        .unwrap();
    let exponent = builder
        .build_and(exponent, storage.const_int(exponent_mask, false), "convert.exponent.bits")
        .unwrap();
    let shift = builder
        .build_int_sub(exponent, storage.const_int(bias + fraction, false), "convert.shift")
        .unwrap();
    let leftward = builder
        .build_int_compare(IntPredicate::SGE, shift, storage.const_zero(), "convert.leftward")
        .unwrap();
    let left_count = builder
        .build_select(leftward, shift, storage.const_zero(), "convert.left.count")
        .unwrap()
        .into_int_value();
    let neg_shift = builder.build_int_neg(shift, "convert.right.shift").unwrap();
    let right_count = builder
        .build_select(leftward, storage.const_zero(), neg_shift, "convert.right.count")
        .unwrap()
        .into_int_value();
    let too_large = builder
        .build_int_compare(
            IntPredicate::UGT,
            right_count,
            storage.const_int((bits - 1) as u64, false),
            "convert.zero",
        )
        .unwrap();
    let right_count = builder
        .build_select(
            too_large,
            storage.const_int((bits - 1) as u64, false),
            right_count,
            "convert.bounded",
        )
        .unwrap()
        .into_int_value();
    let significand = builder
        .build_and(raw, storage.const_int((1u64 << fraction) - 1, false), "convert.fraction")
        .unwrap();
    let significand = builder
        .build_or(significand, storage.const_int(1u64 << fraction, false), "convert.significand")
        .unwrap();
    let low = builder.build_right_shift(significand, right_count, false, "convert.low").unwrap();
    let magnitude = builder.build_int_z_extend(low, target, "convert.wide").unwrap();
    let left_count = builder.build_int_z_extend(left_count, target, "convert.wide.count").unwrap();
    let magnitude = builder.build_left_shift(magnitude, left_count, "convert.magnitude").unwrap();
    let negated = builder.build_int_neg(magnitude, "convert.negate").unwrap();
    builder.build_select(negative, negated, magnitude, "convert.integer").unwrap()
}
