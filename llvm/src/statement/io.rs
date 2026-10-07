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

//! Typed formatting and checked scalar input using the existing C I/O runtime.
use super::{integer_io, variable::build_entry_alloca};
use crate::codegen::abi_c::ExternCInfo;
use crate::codegen::{escape_percent, wave_format_to_c, VariableInfo};
use crate::expression::lvalue::generate_lvalue_ir;
use crate::expression::rvalue::generate_expression_ir;
use hir::{HirExpressionType, TypedProgram};
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::targets::TargetData;
use inkwell::types::{BasicType, StructType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, IntValue, PointerValue};
use inkwell::{AddressSpace, IntPredicate};
use parser::ast::{Expression, WaveType};
use parser::format::{format_fragments, FormatFragment};
use std::collections::HashMap;

fn semantic_type(program: &TypedProgram, expression: &Expression) -> WaveType {
    match program.type_of(expression) {
        Some(HirExpressionType::Resolved(ty)) => ty.clone(),
        Some(HirExpressionType::IntegerLiteral) => WaveType::Int(32),
        Some(HirExpressionType::FloatLiteral) => WaveType::Float(32),
        Some(HirExpressionType::Null) => WaveType::Pointer(Box::new(WaveType::Void)),
        ty => panic!("validated I/O argument has no concrete type: {ty:?}"),
    }
}

fn c_string<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    bytes: &[u8],
) -> PointerValue<'ctx> {
    let value = context.const_string(bytes, true);
    let global = module.add_global(value.get_type(), None, "$io.str");
    global.set_linkage(Linkage::Private);
    global.set_constant(true);
    global.set_initializer(&value);
    global.as_pointer_value()
}

fn printf<'ctx>(
    context: &'ctx Context,
    builder: &inkwell::builder::Builder<'ctx>,
    module: &Module<'ctx>,
    format: &[u8],
    args: &[BasicMetadataValueEnum<'ctx>],
) {
    let ty = context.i32_type().fn_type(&[context.ptr_type(AddressSpace::default()).into()], true);
    let function =
        module.get_function("printf").unwrap_or_else(|| module.add_function("printf", ty, None));
    let mut values = vec![c_string(context, module, format).into()];
    values.extend_from_slice(args);
    builder.build_call(function, &values, "printf_call").unwrap();
}

fn scanf<'ctx>(
    context: &'ctx Context,
    builder: &inkwell::builder::Builder<'ctx>,
    module: &Module<'ctx>,
    format: &[u8],
    pointer: PointerValue<'ctx>,
) -> IntValue<'ctx> {
    let ty = context.i32_type().fn_type(&[context.ptr_type(AddressSpace::default()).into()], true);
    let function =
        module.get_function("scanf").unwrap_or_else(|| module.add_function("scanf", ty, None));
    builder
        .build_call(
            function,
            &[c_string(context, module, format).into(), pointer.into()],
            "scanf_call",
        )
        .unwrap()
        .try_as_basic_value()
        .basic()
        .unwrap()
        .into_int_value()
}

fn require_input<'ctx>(
    context: &'ctx Context,
    builder: &inkwell::builder::Builder<'ctx>,
    module: &Module<'ctx>,
    ok: IntValue<'ctx>,
) {
    let function = builder.get_insert_block().unwrap().get_parent().unwrap();
    let success = context.append_basic_block(function, "input.ok");
    let failure = context.append_basic_block(function, "input.fail");
    builder.build_conditional_branch(ok, success, failure).unwrap();
    builder.position_at_end(failure);
    let ty = context.void_type().fn_type(&[context.i32_type().into()], false);
    let exit = module.get_function("exit").unwrap_or_else(|| module.add_function("exit", ty, None));
    builder.build_call(exit, &[context.i32_type().const_int(1, false).into()], "").unwrap();
    builder.build_unreachable().unwrap();
    builder.position_at_end(success);
}

pub(super) fn gen_print_literal_ir<'ctx>(
    context: &'ctx Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    _string_counter: &mut usize,
    message: &[u8],
) {
    printf(context, builder, module, &escape_percent(message), &[]);
}

pub(super) fn gen_print_format_ir<'ctx>(
    context: &'ctx Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    _string_counter: &mut usize,
    format: &[u8],
    args: &[Expression],
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
    _struct_field_types: &HashMap<String, HashMap<String, WaveType>>,
    target_data: &'ctx TargetData,
    extern_c_info: &HashMap<String, ExternCInfo<'ctx>>,
    program: &TypedProgram,
) {
    let parts = format_fragments(format).expect("validated format");
    let specs = parts.into_iter().filter_map(|part| match part {
        FormatFragment::Placeholder(s) => Some(s),
        _ => None,
    });
    let mut values: Vec<BasicMetadataValueEnum> = Vec::new();
    let mut formats = Vec::new();
    for (argument, spec) in args.iter().zip(specs) {
        let ty = semantic_type(program, argument);
        let value = generate_expression_ir(
            program,
            context,
            builder,
            argument,
            variables,
            module,
            None,
            global_consts,
            struct_types,
            struct_field_indices,
            target_data,
            extern_c_info,
        );
        match value {
            BasicValueEnum::IntValue(mut integer) => {
                if spec == "c" {
                    integer =
                        builder.build_int_cast(integer, context.i32_type(), "print.char").unwrap();
                    values.push(integer.into());
                    formats.push("%c");
                } else {
                    let bits = integer.get_type().get_bit_width().max(8);
                    if integer.get_type().get_bit_width() < bits {
                        integer = builder
                            .build_int_z_extend(
                                integer,
                                context.custom_width_int_type(bits),
                                "print.bool",
                            )
                            .unwrap();
                    }
                    let signed = matches!(ty, WaveType::Int(_));
                    let hex = spec == "x";
                    let function = integer_io::formatter(context, module, bits, signed, hex);
                    let buffer = build_entry_alloca(
                        context,
                        builder,
                        context.i8_type().array_type(integer_io::output_capacity(bits, hex)).into(),
                        "print.integer",
                    );
                    let text = builder
                        .build_call(function, &[integer.into(), buffer.into()], "print.text")
                        .unwrap()
                        .try_as_basic_value()
                        .basic()
                        .unwrap();
                    values.push(text.into());
                    formats.push("%s");
                }
            },
            BasicValueEnum::FloatValue(number) => {
                let number =
                    builder.build_float_cast(number, context.f64_type(), "print.double").unwrap();
                values.push(number.into());
                formats.push("%f");
            },
            BasicValueEnum::PointerValue(pointer) => {
                let string = matches!(ty, WaveType::String)
                    || matches!(ty, WaveType::Pointer(ref t) if matches!(t.as_ref(), WaveType::Byte | WaveType::Char));
                values.push(pointer.into());
                formats.push(if spec == "s" || spec.is_empty() && string { "%s" } else { "%p" });
            },
            _ => unreachable!("frontend validates scalar output"),
        }
    }
    printf(context, builder, module, &wave_format_to_c(format, &formats), &values);
}

pub(super) fn gen_input_ir<'ctx>(
    context: &'ctx Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    _string_counter: &mut usize,
    format: &[u8],
    args: &[Expression],
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
    _struct_field_types: &HashMap<String, HashMap<String, WaveType>>,
    target_data: &'ctx TargetData,
    extern_c_info: &HashMap<String, ExternCInfo<'ctx>>,
    program: &TypedProgram,
) {
    // Resolve every destination once, before any input, matching prior evaluation order.
    let destinations: Vec<_> = args
        .iter()
        .map(|argument| {
            (
                generate_lvalue_ir(
                    program,
                    context,
                    builder,
                    argument,
                    variables,
                    module,
                    global_consts,
                    struct_types,
                    struct_field_indices,
                    target_data,
                    extern_c_info,
                ),
                semantic_type(program, argument),
            )
        })
        .collect();
    let mut destinations = destinations.into_iter();
    for part in format_fragments(format).expect("validated input format") {
        match part {
            FormatFragment::Literal(bytes) => {
                // scanf returns zero both on literal success and mismatch. %n
                // distinguishes reaching the end without consuming another byte.
                let mut literal = escape_percent(bytes);
                literal.extend_from_slice(b"%n");
                let count = build_entry_alloca(
                    context,
                    builder,
                    context.i32_type().into(),
                    "input.literal.count",
                );
                builder.build_store(count, context.i32_type().const_all_ones()).unwrap();
                scanf(context, builder, module, &literal, count);
                let read = builder
                    .build_load(context.i32_type(), count, "input.literal.read")
                    .unwrap()
                    .into_int_value();
                let ok = builder
                    .build_int_compare(
                        IntPredicate::SGE,
                        read,
                        context.i32_type().const_zero(),
                        "input.literal.ok",
                    )
                    .unwrap();
                require_input(context, builder, module, ok);
            },
            FormatFragment::Placeholder(_) => {
                let (destination, ty) = destinations.next().expect("validated arity");
                let ok = match ty {
                    WaveType::Bool | WaveType::Byte | WaveType::Int(_) | WaveType::Uint(_) => {
                        let (bits, signed, boolean) = match ty {
                            WaveType::Int(bits) => (u32::from(bits), true, false),
                            WaveType::Uint(bits) => (u32::from(bits), false, false),
                            WaveType::Bool => (8, false, true),
                            _ => (8, false, false),
                        };
                        let function = integer_io::scanner(context, module, bits, signed, boolean);
                        builder
                            .build_call(function, &[destination.into()], "input.integer.ok")
                            .unwrap()
                            .try_as_basic_value()
                            .basic()
                            .unwrap()
                            .into_int_value()
                    },
                    WaveType::Char | WaveType::Float(_) => {
                        let (element, format) = match ty {
                            WaveType::Char => {
                                (context.i8_type().as_basic_type_enum(), b"%c".as_slice())
                            },
                            WaveType::Float(32) => {
                                (context.f32_type().as_basic_type_enum(), b"%f".as_slice())
                            },
                            _ => (context.f64_type().as_basic_type_enum(), b"%lf".as_slice()),
                        };
                        let temporary =
                            build_entry_alloca(context, builder, element, "input.scalar");
                        let count = scanf(context, builder, module, format, temporary);
                        let ok = builder
                            .build_int_compare(
                                IntPredicate::EQ,
                                count,
                                context.i32_type().const_int(1, false),
                                "input.scalar.ok",
                            )
                            .unwrap();
                        require_input(context, builder, module, ok);
                        let value =
                            builder.build_load(element, temporary, "input.scalar.value").unwrap();
                        builder.build_store(destination, value).unwrap();
                        context.bool_type().const_int(1, false)
                    },
                    _ => unreachable!("frontend validates input destinations"),
                };
                require_input(context, builder, module, ok);
            },
        }
    }
}
