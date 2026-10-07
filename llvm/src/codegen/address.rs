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

//! Address calculation for assignable expressions.
//!
//! LLVM pointers are opaque, so lvalue lowering must recover pointee and field
//! types from Wave semantic types rather than from the LLVM pointer itself.
//! This module returns both the address and its storage type to keep subsequent
//! loads and stores consistent. Index evaluation does not add bounds checks:
//! in-bounds GEP still requires the resulting address to stay within its source
//! allocation (negative pointer offsets may address earlier elements).

use crate::expression::rvalue::ExprGenEnv;
use hir::TypedProgram;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::{AsTypeRef, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{IntValue, PointerValue};
use parser::ast::{Expression, WaveType};

use std::collections::HashMap;

use crate::codegen::types::TypeFlavor;
use crate::codegen::wave_type_to_llvm_type;

use super::types::VariableInfo;

fn normalize_struct_name(raw: &str) -> &str {
    raw.strip_prefix("struct.").unwrap_or(raw).trim_start_matches('%')
}

/// Evaluate once in the expression's own integer type, then adapt the offset
/// to the target address width. In particular, u8/u32 offsets must not become
/// negative when LLVM sign-extends a narrow GEP index.
pub(crate) fn generate_index_ir<'ctx>(
    env: &mut ExprGenEnv<'ctx, '_>,
    expr: &Expression,
) -> IntValue<'ctx> {
    let index_ty = env.context.ptr_sized_int_type(env.target_data, None);
    let expected = match env.program.type_of(expr) {
        Some(hir::HirExpressionType::IntegerLiteral) => Some(index_ty.into()),
        _ => None,
    };
    let value = env.gen(expr, expected).into_int_value();
    let source_unsigned = matches!(
        env.wave_type(expr),
        Some(WaveType::Uint(_) | WaveType::Byte | WaveType::Char | WaveType::Bool)
    );
    match value.get_type().get_bit_width().cmp(&index_ty.get_bit_width()) {
        std::cmp::Ordering::Equal => value,
        std::cmp::Ordering::Less if source_unsigned => {
            env.builder.build_int_z_extend(value, index_ty, "idx_zext").unwrap()
        },
        std::cmp::Ordering::Less => {
            env.builder.build_int_s_extend(value, index_ty, "idx_sext").unwrap()
        },
        std::cmp::Ordering::Greater => {
            env.builder.build_int_truncate(value, index_ty, "idx_trunc").unwrap()
        },
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
    builder.build_load(ptr_ty, slot_ptr, name).unwrap().into_pointer_value()
}

fn pointee_ty_of_ptr_expr<'ctx>(
    context: &'ctx Context,
    expr: &Expression,
    program: &TypedProgram,
    variables: &HashMap<String, VariableInfo<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
) -> BasicTypeEnum<'ctx> {
    if let Some(hir::HirExpressionType::Resolved(WaveType::Pointer(inner))) = program.type_of(expr)
    {
        return wave_type_to_llvm_type(context, inner, struct_types, TypeFlavor::AbiC);
    }
    match expr {
        Expression::Grouped(inner) => {
            pointee_ty_of_ptr_expr(context, inner, program, variables, struct_types)
        },

        Expression::Variable(name) => {
            let vi = variables.get(name).unwrap_or_else(|| panic!("Variable {} not found", name));

            match &vi.ty {
                WaveType::Pointer(inner) => {
                    wave_type_to_llvm_type(context, inner, struct_types, TypeFlavor::AbiC)
                },
                WaveType::String => context.i8_type().as_basic_type_enum(),
                other => panic!("deref/index expects pointer type, got {:?} for {}", other, name),
            }
        },

        // ptr coming from field/index: LLVM pointer is opaque -> pointee unknown
        _ => context.i8_type().as_basic_type_enum(),
    }
}

fn struct_ty_of_ptr_expr<'ctx>(
    context: &'ctx Context,
    expr: &Expression,
    program: &TypedProgram,
    variables: &HashMap<String, VariableInfo<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
) -> StructType<'ctx> {
    if let Some(hir::HirExpressionType::Resolved(WaveType::Pointer(inner))) = program.type_of(expr)
    {
        if let WaveType::Struct(name) = inner.as_ref() {
            return struct_types[name];
        }
    }
    match expr {
        Expression::Grouped(inner) => {
            struct_ty_of_ptr_expr(context, inner, program, variables, struct_types)
        },

        Expression::Variable(name) => {
            let vi = variables.get(name).unwrap_or_else(|| panic!("Variable {} not found", name));

            match &vi.ty {
                WaveType::Pointer(inner) => match inner.as_ref() {
                    WaveType::Struct(sname) => *struct_types
                        .get(sname)
                        .unwrap_or_else(|| panic!("Struct type '{}' not found", sname)),
                    other => panic!("pointer does not point to struct: {:?} (var {})", other, name),
                },
                other => panic!("expected pointer-to-struct var, got {:?} (var {})", other, name),
            }
        },

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
    env: &mut ExprGenEnv<'ctx, '_>,
    expr: &Expression,
) -> (PointerValue<'ctx>, BasicTypeEnum<'ctx>) {
    match expr {
        Expression::Cast { expr: inner, target_type: WaveType::Pointer(_) }
        | Expression::Grouped(inner) => addr_and_ty(env, inner),

        Expression::Variable(name) => {
            let vi =
                env.variables.get(name).unwrap_or_else(|| panic!("Variable {} not found", name));
            (vi.ptr, storage_ty_of_var(env.context, vi, env.struct_types))
        },

        // legacy behavior: treat &x as "address of x" when someone asks for address again
        Expression::AddressOf(inner) => addr_and_ty(env, inner),

        // A dereference consumes a pointer value, which may be returned by a
        // call or computed expression rather than stored in a variable slot.
        Expression::Deref(inner) => {
            if matches!(
                inner.as_ref(),
                Expression::IndexAccess { .. } | Expression::FieldAccess { .. }
            ) {
                return addr_and_ty(env, inner);
            }
            let pointer = env.gen(inner, None).into_pointer_value();
            let pointee = pointee_ty_of_ptr_expr(
                env.context,
                inner,
                env.program,
                env.variables,
                env.struct_types,
            );
            (pointer, pointee)
        },

        Expression::FieldAccess { object, field } => {
            let (obj_addr, obj_ty) = addr_and_ty(env, object);

            // object can be: struct-by-value (addr points to struct)
            // or: pointer-to-struct stored in a slot (addr points to ptr, must load ptr)
            let (struct_ptr, struct_ty) = match obj_ty {
                BasicTypeEnum::StructType(st) => (obj_addr, st),
                BasicTypeEnum::PointerType(_) => {
                    let p = load_ptr_from_slot(env.context, env.builder, obj_addr, "obj_load");
                    let st = struct_ty_of_ptr_expr(
                        env.context,
                        object,
                        env.program,
                        env.variables,
                        env.struct_types,
                    );
                    (p, st)
                },
                other => panic!("FieldAccess on non-struct object type: {:?}", other),
            };

            let sname = resolve_struct_key(struct_ty, env.struct_types);

            let idx = *env
                .struct_field_indices
                .get(&sname)
                .unwrap_or_else(|| panic!("Struct '{}' missing in struct_field_indices", sname))
                .get(field)
                .unwrap_or_else(|| {
                    panic!("Field '{}.{}' missing in struct_field_indices", sname, field)
                });

            let field_ty = struct_ty
                .get_field_type_at_index(idx)
                .unwrap_or_else(|| panic!("No field type at index {} for struct '{}'", idx, sname));

            let field_ptr =
                env.builder.build_struct_gep(struct_ty, struct_ptr, idx, "field_ptr").unwrap();

            (field_ptr, field_ty)
        },

        Expression::IndexAccess { target, index } => {
            let (t_addr, t_ty) = addr_and_ty(env, target);

            let idx = generate_index_ir(env, index);

            match t_ty {
                BasicTypeEnum::ArrayType(at) => {
                    let zero = idx.get_type().const_zero();
                    let ep = unsafe {
                        env.builder
                            .build_in_bounds_gep(at, t_addr, &[zero, idx], "arr_gep")
                            .unwrap()
                    };
                    (ep, at.get_element_type())
                },

                BasicTypeEnum::PointerType(_) => {
                    let base_ptr =
                        load_ptr_from_slot(env.context, env.builder, t_addr, "idx_base_load");
                    let pointee = pointee_ty_of_ptr_expr(
                        env.context,
                        target,
                        env.program,
                        env.variables,
                        env.struct_types,
                    );

                    // ptr-to-array: gep [0, idx]
                    if let BasicTypeEnum::ArrayType(at) = pointee {
                        let zero = idx.get_type().const_zero();
                        let ep = unsafe {
                            env.builder
                                .build_in_bounds_gep(at, base_ptr, &[zero, idx], "ptr_arr_gep")
                                .unwrap()
                        };
                        (ep, at.get_element_type())
                    } else {
                        let ep = unsafe {
                            env.builder
                                .build_in_bounds_gep(pointee, base_ptr, &[idx], "ptr_gep")
                                .unwrap()
                        };
                        (ep, pointee)
                    }
                },

                other => panic!("IndexAccess on non-array/non-pointer: {:?}", other),
            }
        },

        other => panic!("Cannot take address of this expression: {:?}", other),
    }
}

pub(crate) fn generate_address_ir<'ctx>(
    env: &mut ExprGenEnv<'ctx, '_>,
    expr: &Expression,
) -> PointerValue<'ctx> {
    addr_and_ty(env, expr).0
}

pub(crate) fn generate_address_and_type_ir<'ctx>(
    env: &mut ExprGenEnv<'ctx, '_>,
    expr: &Expression,
) -> (PointerValue<'ctx>, BasicTypeEnum<'ctx>) {
    addr_and_ty(env, expr)
}
