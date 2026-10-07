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

//! Loading values from Wave lvalue expressions.
//!
//! Variables, dereferences, indices, and fields first resolve to a typed storage
//! address. Loads then use the recovered Wave type, avoiding guesses from
//! LLVM's opaque pointer type and preserving aggregate storage representation.

use hir::{HirExpressionType, TypedProgram};
use inkwell::targets::TargetData;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicTypeEnum, StructType},
    values::{BasicValueEnum, PointerValue},
    AddressSpace,
};
use parser::ast::{Expression, WaveType};
use std::collections::HashMap;

use crate::codegen::abi_c::ExternCInfo;
use crate::codegen::types::{wave_type_to_llvm_type, TypeFlavor};
use crate::codegen::VariableInfo;
use crate::expression::rvalue::generate_expression_ir;

fn llvm_basic_to_wave_type<'ctx>(bt: BasicTypeEnum<'ctx>) -> WaveType {
    match bt {
        BasicTypeEnum::IntType(it) => {
            let w = it.get_bit_width();
            if w == 1 {
                WaveType::Bool
            } else {
                WaveType::Int(w as usize as u16)
            }
        },
        BasicTypeEnum::FloatType(ft) => {
            let bw = ft.get_bit_width();
            match bw {
                32 => WaveType::Float(32),
                64 => WaveType::Float(64),
                _ => panic!("Unsupported float width: {}", bw),
            }
        },
        BasicTypeEnum::ArrayType(at) => {
            let elem = at.get_element_type();
            WaveType::Array(Box::new(llvm_basic_to_wave_type(elem)), at.len() as usize as u32)
        },
        BasicTypeEnum::StructType(st) => {
            let raw = st
                .get_name()
                .and_then(|c| c.to_str().ok())
                .unwrap_or_else(|| panic!("Unnamed struct type; cannot map to WaveType"));
            let name = raw.strip_prefix("struct.").unwrap_or(raw).to_string();
            WaveType::Struct(name)
        },
        BasicTypeEnum::PointerType(_) => WaveType::Pointer(Box::new(WaveType::Void)),
        BasicTypeEnum::VectorType(_) | BasicTypeEnum::ScalableVectorType(_) => {
            panic!("Vector types are not supported in WaveType mapping yet");
        },
    }
}

fn load_ptr_value<'ctx>(
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    addr_of_ptr: PointerValue<'ctx>,
    name: &str,
) -> PointerValue<'ctx> {
    let ptr_ty = context.ptr_type(AddressSpace::default());
    builder.build_load(ptr_ty, addr_of_ptr, name).unwrap().into_pointer_value()
}

fn generate_lvalue_ir_typed<'ctx>(
    program: &TypedProgram,
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    expr: &Expression,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    module: &'ctx Module<'ctx>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
    target_data: &'ctx TargetData,
    extern_c_info: &HashMap<String, ExternCInfo<'ctx>>,
) -> (PointerValue<'ctx>, WaveType) {
    match expr {
        Expression::Variable(name) => {
            if global_consts.contains_key(name) {
                panic!("Cannot use constant '{}' as lvalue", name);
            }

            let info =
                variables.get(name).unwrap_or_else(|| panic!("Undefined variable '{}'", name));

            (info.ptr, info.ty.clone())
        },

        Expression::Grouped(inner) => generate_lvalue_ir_typed(
            program,
            context,
            builder,
            inner,
            variables,
            module,
            global_consts,
            struct_types,
            struct_field_indices,
            target_data,
            extern_c_info,
        ),

        Expression::Deref(inner) => {
            if matches!(
                inner.as_ref(),
                Expression::IndexAccess { .. } | Expression::FieldAccess { .. }
            ) {
                return generate_lvalue_ir_typed(
                    program,
                    context,
                    builder,
                    inner,
                    variables,
                    module,
                    global_consts,
                    struct_types,
                    struct_field_indices,
                    target_data,
                    extern_c_info,
                );
            }

            let v = generate_expression_ir(
                program,
                context,
                builder,
                inner,
                variables,
                module,
                None,
                global_consts,
                struct_types,
                struct_field_indices,
                target_data,
                extern_c_info,
            );

            let p = match v {
                BasicValueEnum::PointerValue(p) => p,
                _ => panic!("Deref target is not a pointer: {:?}", inner),
            };

            let pointee_ty = match program.type_of(inner) {
                Some(HirExpressionType::Resolved(WaveType::Pointer(inner_ty))) => *inner_ty.clone(),
                Some(HirExpressionType::Resolved(WaveType::String)) => WaveType::Byte,
                semantic_type => panic!(
                    "typed HIR did not provide a pointer type for deref expression {:?}: {:?}",
                    inner, semantic_type
                ),
            };

            (p, pointee_ty)
        },

        Expression::AddressOf(inner) => generate_lvalue_ir_typed(
            program,
            context,
            builder,
            inner,
            variables,
            module,
            global_consts,
            struct_types,
            struct_field_indices,
            target_data,
            extern_c_info,
        ),

        Expression::IndexAccess { target, index } => {
            let idx = crate::codegen::address::generate_index_ir(
                &mut crate::expression::rvalue::ExprGenEnv {
                    program,
                    context,
                    builder,
                    variables,
                    module,
                    global_consts,
                    struct_types,
                    struct_field_indices,
                    target_data,
                    extern_c_info,
                },
                index,
            );

            let (base_addr, base_ty) = match &**target {
                Expression::Variable(_)
                | Expression::Deref(_)
                | Expression::AddressOf(_)
                | Expression::IndexAccess { .. }
                | Expression::FieldAccess { .. }
                | Expression::Grouped(_) => generate_lvalue_ir_typed(
                    program,
                    context,
                    builder,
                    target,
                    variables,
                    module,
                    global_consts,
                    struct_types,
                    struct_field_indices,
                    target_data,
                    extern_c_info,
                ),
                _ => {
                    let v = generate_expression_ir(
                        program,
                        context,
                        builder,
                        target,
                        variables,
                        module,
                        None,
                        global_consts,
                        struct_types,
                        struct_field_indices,
                        target_data,
                        extern_c_info,
                    );
                    match v {
                        BasicValueEnum::PointerValue(_p) => {
                            panic!(
                                "IndexAccess needs pointee type under opaque pointers (LLVM15+). \
                                 Target expression type info is missing: {:?}",
                                target
                            );
                        },
                        _ => panic!("Index target is not a pointer/array: {:?}", target),
                    }
                },
            };

            let zero = idx.get_type().const_zero();

            match base_ty {
                WaveType::Array(inner, _size) => {
                    let arr_llvm = wave_type_to_llvm_type(
                        context,
                        &WaveType::Array(inner.clone(), _size),
                        struct_types,
                        TypeFlavor::Value,
                    );

                    let gep = unsafe {
                        builder
                            .build_gep(arr_llvm, base_addr, &[zero, idx], "array_elem_ptr")
                            .unwrap()
                    };
                    (gep, *inner)
                },

                WaveType::Pointer(inner) => {
                    let base_ptr = load_ptr_value(context, builder, base_addr, "load_index_base");

                    match *inner {
                        WaveType::Array(element, size) => {
                            let array_ty = wave_type_to_llvm_type(
                                context,
                                &WaveType::Array(element.clone(), size),
                                struct_types,
                                TypeFlavor::Value,
                            );
                            let gep = unsafe {
                                builder
                                    .build_gep(
                                        array_ty,
                                        base_ptr,
                                        &[zero, idx],
                                        "ptr_array_elem_ptr",
                                    )
                                    .unwrap()
                            };
                            (gep, *element)
                        },
                        element => {
                            let elem_llvm = wave_type_to_llvm_type(
                                context,
                                &element,
                                struct_types,
                                TypeFlavor::Value,
                            );
                            let gep = unsafe {
                                builder
                                    .build_gep(elem_llvm, base_ptr, &[idx], "ptr_elem_ptr")
                                    .unwrap()
                            };
                            (gep, element)
                        },
                    }
                },

                WaveType::String => {
                    let base_ptr = load_ptr_value(context, builder, base_addr, "load_index_base");

                    let gep = unsafe {
                        builder
                            .build_gep(context.i8_type(), base_ptr, &[idx], "str_elem_ptr")
                            .unwrap()
                    };
                    (gep, WaveType::Byte)
                },

                other => {
                    panic!("IndexAccess target is not array/pointer/string: {:?}", other);
                },
            }
        },

        Expression::FieldAccess { object, field } => {
            let (obj_addr, obj_ty) = generate_lvalue_ir_typed(
                program,
                context,
                builder,
                object,
                variables,
                module,
                global_consts,
                struct_types,
                struct_field_indices,
                target_data,
                extern_c_info,
            );

            let (obj_ptr, struct_name) = match obj_ty {
                WaveType::Struct(name) => (obj_addr, name),

                WaveType::Pointer(inner) => match *inner {
                    WaveType::Struct(name) => {
                        let p = load_ptr_value(context, builder, obj_addr, "load_struct_ptr");
                        (p, name)
                    },
                    other => panic!("FieldAccess base pointer is not ptr<struct>: {:?}", other),
                },

                other => panic!("FieldAccess base is not a struct or ptr<struct>: {:?}", other),
            };

            let struct_ty = struct_types
                .get(&struct_name)
                .unwrap_or_else(|| panic!("Struct type '{}' not found", struct_name));

            let field_index = struct_field_indices
                .get(&struct_name)
                .and_then(|m| m.get(field))
                .copied()
                .unwrap_or_else(|| panic!("Unknown field '{}.{}'", struct_name, field));

            let field_ptr =
                builder.build_struct_gep(*struct_ty, obj_ptr, field_index, "field_ptr").unwrap();

            let field_bt = struct_ty
                .get_field_type_at_index(field_index)
                .unwrap_or_else(|| panic!("Invalid field index for {}", struct_name));

            // LLVM storage alone loses variant identity, pointer pointees and
            // integer signedness. Keep the frontend's resolved field type.
            let field_wave_ty = match program.type_of(expr) {
                Some(HirExpressionType::Resolved(ty)) => ty.clone(),
                _ => llvm_basic_to_wave_type(field_bt),
            };

            (field_ptr, field_wave_ty)
        },

        _ => {
            panic!("Expression is not an lvalue (not assignable): {:?}", expr);
        },
    }
}

pub fn generate_lvalue_ir<'ctx>(
    program: &TypedProgram,
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    expr: &Expression,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    module: &'ctx Module<'ctx>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
    target_data: &'ctx TargetData,
    extern_c_info: &HashMap<String, ExternCInfo<'ctx>>,
) -> PointerValue<'ctx> {
    let (p, _ty) = generate_lvalue_ir_typed(
        program,
        context,
        builder,
        expr,
        variables,
        module,
        global_consts,
        struct_types,
        struct_field_indices,
        target_data,
        extern_c_info,
    );
    p
}
