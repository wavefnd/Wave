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

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{AsTypeRef, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{IntValue, PointerValue};
use parser::ast::{Expression, Literal, WaveType};

use std::collections::HashMap;

use crate::codegen::types::TypeFlavor;
use crate::codegen::wave_type_to_llvm_type;

use super::types::VariableInfo;

fn normalize_struct_name(raw: &str) -> &str {
    raw.strip_prefix("struct.")
        .unwrap_or(raw)
        .trim_start_matches('%')
}

fn cast_int_to_i64<'ctx>(
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    v: IntValue<'ctx>,
) -> IntValue<'ctx> {
    let i64_ty = context.i64_type();
    let src_bits = v.get_type().get_bit_width();

    if src_bits == 64 {
        v
    } else if src_bits < 64 {
        builder.build_int_s_extend(v, i64_ty, "idx_sext").unwrap()
    } else {
        builder.build_int_truncate(v, i64_ty, "idx_trunc").unwrap()
    }
}

fn resolve_struct_key<'ctx>(
    st: StructType<'ctx>,
    struct_types: &HashMap<String, StructType<'ctx>>,
) -> String {
    if let Some(raw) = st.get_name().and_then(|n| n.to_str().ok()) {
        return normalize_struct_name(raw).to_string();
    }

    let st_ref = st.as_type_ref();
    for (name, ty) in struct_types {
        if ty.as_type_ref() == st_ref {
            return name.clone();
        }
    }

    panic!("LLVM struct type has no name and cannot be matched to struct_types");
}

fn storage_ty_of_var<'ctx>(
    context: &'ctx Context,
    vi: &VariableInfo<'ctx>,
    struct_types: &HashMap<String, StructType<'ctx>>,
) -> BasicTypeEnum<'ctx> {
    wave_type_to_llvm_type(context, &vi.ty, struct_types, TypeFlavor::AbiC)
}

fn load_ptr_from_slot<'ctx>(
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    slot_ptr: PointerValue<'ctx>,
    name: &str,
) -> PointerValue<'ctx> {
    let aspace = slot_ptr.get_type().get_address_space();
    let ptr_ty = context.ptr_type(aspace);
    builder
        .build_load(ptr_ty, slot_ptr, name)
        .unwrap()
        .into_pointer_value()
}

fn pointee_ty_of_ptr_expr<'ctx>(
    context: &'ctx Context,
    expr: &Expression,
    variables: &HashMap<String, VariableInfo<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
) -> BasicTypeEnum<'ctx> {
    match expr {
        Expression::Grouped(inner) => {
            pointee_ty_of_ptr_expr(context, inner, variables, struct_types)
        }

        Expression::Variable(name) => {
            let vi = variables
                .get(name)
                .unwrap_or_else(|| panic!("Variable {} not found", name));

            match &vi.ty {
                WaveType::Pointer(inner) => {
                    wave_type_to_llvm_type(context, inner, struct_types, TypeFlavor::AbiC)
                }
                WaveType::String => context.i8_type().as_basic_type_enum(),
                other => panic!(
                    "deref/index expects pointer type, got {:?} for {}",
                    other, name
                ),
            }
        }

        // ptr coming from field/index: LLVM pointer is opaque -> pointee unknown
        _ => context.i8_type().as_basic_type_enum(),
    }
}

fn struct_ty_of_ptr_expr<'ctx>(
    context: &'ctx Context,
    expr: &Expression,
    variables: &HashMap<String, VariableInfo<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
) -> StructType<'ctx> {
    match expr {
        Expression::Grouped(inner) => {
            struct_ty_of_ptr_expr(context, inner, variables, struct_types)
        }

        Expression::Variable(name) => {
            let vi = variables
                .get(name)
                .unwrap_or_else(|| panic!("Variable {} not found", name));

            match &vi.ty {
                WaveType::Pointer(inner) => match inner.as_ref() {
                    WaveType::Struct(sname) => *struct_types
                        .get(sname)
                        .unwrap_or_else(|| panic!("Struct type '{}' not found", sname)),
                    other => panic!(
                        "pointer does not point to struct: {:?} (var {})",
                        other, name
                    ),
                },
                other => panic!(
                    "expected pointer-to-struct var, got {:?} (var {})",
                    other, name
                ),
            }
        }

        // ptr coming from field/index is opaque; we can't know struct type here without field WaveType info
        other => panic!(
            "Cannot resolve struct type for pointer expr {:?}. \
             Need struct field WaveType info (or restrict to ptr vars).",
            other
        ),
    }
}

/// internal: returns (address, value_type_at_address)
fn addr_and_ty<'ctx>(
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    expr: &Expression,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    module: &'ctx Module<'ctx>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
) -> (PointerValue<'ctx>, BasicTypeEnum<'ctx>) {
    match expr {
        Expression::Grouped(inner) => addr_and_ty(
            context,
            builder,
            inner,
            variables,
            module,
            struct_types,
            struct_field_indices,
        ),

        Expression::Variable(name) => {
            let vi = variables
                .get(name)
                .unwrap_or_else(|| panic!("Variable {} not found", name));
            (vi.ptr, storage_ty_of_var(context, vi, struct_types))
        }

        // legacy behavior: treat &x as "address of x" when someone asks for address again
        Expression::AddressOf(inner) => addr_and_ty(
            context,
            builder,
            inner,
            variables,
            module,
            struct_types,
            struct_field_indices,
        ),

        // lvalue "*p" => address is the pointer value stored in p
        Expression::Deref(inner) => {
            let (slot_ptr, slot_ty) = addr_and_ty(
                context,
                builder,
                inner,
                variables,
                module,
                struct_types,
                struct_field_indices,
            );

            if matches!(
                inner.as_ref(),
                Expression::IndexAccess { .. } | Expression::FieldAccess { .. }
            ) {
                return (slot_ptr, slot_ty);
            }

            if !slot_ty.is_pointer_type() {
                // Legacy compatibility:
                // allow redundant `deref` on already-addressable lvalues
                // like `deref q.rear` and `deref visited[x]`.
                return (slot_ptr, slot_ty);
            }

            let pv = load_ptr_from_slot(context, builder, slot_ptr, "deref_target");

            let pointee_ty = pointee_ty_of_ptr_expr(context, inner, variables, struct_types);
            (pv, pointee_ty)
        }

        Expression::FieldAccess { object, field } => {
            let (obj_addr, obj_ty) = addr_and_ty(
                context,
                builder,
                object,
                variables,
                module,
                struct_types,
                struct_field_indices,
            );

            // object can be: struct-by-value (addr points to struct)
            // or: pointer-to-struct stored in a slot (addr points to ptr, must load ptr)
            let (struct_ptr, struct_ty) = match obj_ty {
                BasicTypeEnum::StructType(st) => (obj_addr, st),
                BasicTypeEnum::PointerType(_) => {
                    let p = load_ptr_from_slot(context, builder, obj_addr, "obj_load");
                    let st = struct_ty_of_ptr_expr(context, object, variables, struct_types);
                    (p, st)
                }
                other => panic!("FieldAccess on non-struct object type: {:?}", other),
            };

            let sname = resolve_struct_key(struct_ty, struct_types);

            let idx = *struct_field_indices
                .get(&sname)
                .unwrap_or_else(|| panic!("Struct '{}' missing in struct_field_indices", sname))
                .get(field)
                .unwrap_or_else(|| {
                    panic!(
                        "Field '{}.{}' missing in struct_field_indices",
                        sname, field
                    )
                });

            let field_ty = struct_ty
                .get_field_type_at_index(idx)
                .unwrap_or_else(|| panic!("No field type at index {} for struct '{}'", idx, sname));

            let field_ptr = builder
                .build_struct_gep(struct_ty, struct_ptr, idx, "field_ptr")
                .unwrap();

            (field_ptr, field_ty)
        }

        Expression::IndexAccess { target, index } => {
            let (t_addr, t_ty) = addr_and_ty(
                context,
                builder,
                target,
                variables,
                module,
                struct_types,
                struct_field_indices,
            );

            let idx_i64 = int_expr_as_i64(
                context,
                builder,
                index,
                variables,
                module,
                struct_types,
                struct_field_indices,
            );

            match t_ty {
                BasicTypeEnum::ArrayType(at) => {
                    let zero = context.i64_type().const_int(0, false);
                    let ep = unsafe {
                        builder
                            .build_in_bounds_gep(at, t_addr, &[zero, idx_i64], "arr_gep")
                            .unwrap()
                    };
                    (ep, at.get_element_type())
                }

                BasicTypeEnum::PointerType(_) => {
                    let base_ptr = load_ptr_from_slot(context, builder, t_addr, "idx_base_load");
                    let pointee = pointee_ty_of_ptr_expr(context, target, variables, struct_types);

                    // ptr-to-array: gep [0, idx]
                    if let BasicTypeEnum::ArrayType(at) = pointee {
                        let zero = context.i64_type().const_int(0, false);
                        let ep = unsafe {
                            builder
                                .build_in_bounds_gep(at, base_ptr, &[zero, idx_i64], "ptr_arr_gep")
                                .unwrap()
                        };
                        (ep, at.get_element_type())
                    } else {
                        let ep = unsafe {
                            builder
                                .build_in_bounds_gep(pointee, base_ptr, &[idx_i64], "ptr_gep")
                                .unwrap()
                        };
                        (ep, pointee)
                    }
                }

                other => panic!("IndexAccess on non-array/non-pointer: {:?}", other),
            }
        }

        other => panic!("Cannot take address of this expression: {:?}", other),
    }
}

fn int_expr_as_i64<'ctx>(
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    expr: &Expression,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    module: &'ctx Module<'ctx>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
) -> IntValue<'ctx> {
    match expr {
        Expression::Grouped(inner) => int_expr_as_i64(
            context,
            builder,
            inner,
            variables,
            module,
            struct_types,
            struct_field_indices,
        ),

        Expression::Literal(Literal::Int(s)) => {
            let n: i64 = s.parse().unwrap();
            context.i64_type().const_int(n as u64, true)
        }

        // load lvalue int and cast
        Expression::Variable(_)
        | Expression::FieldAccess { .. }
        | Expression::IndexAccess { .. }
        | Expression::Deref(_)
        | Expression::AddressOf(_) => {
            let (addr, ty) = addr_and_ty(
                context,
                builder,
                expr,
                variables,
                module,
                struct_types,
                struct_field_indices,
            );

            let int_ty = match ty {
                BasicTypeEnum::IntType(it) => it,
                other => panic!("Index int expr expected int, got {:?}", other),
            };

            let loaded = builder
                .build_load(int_ty, addr, "idx_load")
                .unwrap()
                .into_int_value();

            cast_int_to_i64(context, builder, loaded)
        }

        other => panic!("Index int expr not supported yet: {:?}", other),
    }
}

pub fn generate_address_ir<'ctx>(
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    expr: &Expression,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    module: &'ctx Module<'ctx>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
) -> PointerValue<'ctx> {
    addr_and_ty(
        context,
        builder,
        expr,
        variables,
        module,
        struct_types,
        struct_field_indices,
    )
    .0
}

pub fn generate_address_and_type_ir<'ctx>(
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    expr: &Expression,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    module: &'ctx Module<'ctx>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
) -> (PointerValue<'ctx>, BasicTypeEnum<'ctx>) {
    addr_and_ty(
        context,
        builder,
        expr,
        variables,
        module,
        struct_types,
        struct_field_indices,
    )
}
