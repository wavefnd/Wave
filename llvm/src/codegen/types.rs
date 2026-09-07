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

//! Mapping between Wave semantic types and LLVM value/storage types.
//!
//! [`TypeFlavor`] keeps ordinary value lowering separate from C ABI-facing type
//! construction. Calling-convention transport decisions are not made here; the
//! C ABI classifier consumes these object representations afterward.

use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::PointerValue;
use inkwell::AddressSpace;

use inkwell::targets::TargetData;
use parser::ast::{Mutability, WaveType};
use std::collections::HashMap;

pub type StructFieldMap = HashMap<String, HashMap<String, u32>>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TypeFlavor {
    Value,
    AbiC,
}

pub fn build_field_map(fields: &[(String, parser::ast::WaveType)]) -> HashMap<String, u32> {
    let mut m = HashMap::new();
    for (i, (name, _ty)) in fields.iter().enumerate() {
        m.insert(name.clone(), i as u32);
    }
    m
}

pub fn get_field_index(struct_fields: &StructFieldMap, struct_name: &str, field: &str) -> u32 {
    *struct_fields
        .get(struct_name)
        .unwrap_or_else(|| panic!("Struct '{}' field map not found", struct_name))
        .get(field)
        .unwrap_or_else(|| panic!("Field '{}' not found in struct '{}'", field, struct_name))
}

pub fn wave_type_to_llvm_type<'ctx>(
    context: &'ctx Context,
    wave_type: &WaveType,
    struct_types: &HashMap<String, inkwell::types::StructType<'ctx>>,
    flavor: TypeFlavor,
) -> BasicTypeEnum<'ctx> {
    match wave_type {
        WaveType::Isz | WaveType::Usz | WaveType::Never => {
            unreachable!("typed HIR must resolve target-sized integers")
        }
        WaveType::Int(bits) | WaveType::Uint(bits) => context
            .custom_width_int_type(*bits as u32)
            .as_basic_type_enum(),

        WaveType::Float(bits) => match bits {
            32 => context.f32_type().as_basic_type_enum(),
            64 => context.f64_type().as_basic_type_enum(),
            _ => panic!("Unsupported float bit width: {}", bits),
        },

        WaveType::Bool => {
            if flavor == TypeFlavor::AbiC {
                context.i8_type().as_basic_type_enum()
            } else {
                context.bool_type().as_basic_type_enum()
            }
        }

        WaveType::Char | WaveType::Byte => context.i8_type().as_basic_type_enum(),

        WaveType::Void => context.i8_type().as_basic_type_enum(),

        WaveType::Pointer(_inner) => context
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum(),

        WaveType::Array(inner, size) => {
            let inner_ty = wave_type_to_llvm_type(context, inner, struct_types, flavor);
            inner_ty.array_type(*size as u32).as_basic_type_enum()
        }

        WaveType::String => context
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum(),

        WaveType::Struct(name) => struct_types
            .get(name)
            .unwrap_or_else(|| panic!("Struct type '{}' not found", name))
            .as_basic_type_enum(),
        WaveType::Variant(name) => struct_types
            .get(name)
            .unwrap_or_else(|| panic!("Variant type '{}' not found", name))
            .as_basic_type_enum(),
    }
}

fn flatten_leaves<'ctx>(t: BasicTypeEnum<'ctx>, out: &mut Vec<BasicTypeEnum<'ctx>>) {
    match t {
        BasicTypeEnum::StructType(st) => {
            for i in 0..st.count_fields() {
                let f = st.get_field_type_at_index(i).unwrap();
                flatten_leaves(f, out);
            }
        }
        BasicTypeEnum::ArrayType(at) => {
            let elem = at.get_element_type();
            for _ in 0..at.len() {
                flatten_leaves(elem, out);
            }
        }
        _ => out.push(t),
    }
}

fn is_integer_only_aggregate<'ctx>(t: BasicTypeEnum<'ctx>) -> bool {
    match t {
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_) => {
            let mut leaves = vec![];
            flatten_leaves(t, &mut leaves);
            leaves.iter().all(|lt| {
                matches!(
                    lt,
                    BasicTypeEnum::IntType(_) | BasicTypeEnum::PointerType(_)
                )
            })
        }
        _ => false,
    }
}

pub fn abi_c_lower_extern_param_type<'ctx>(
    context: &'ctx inkwell::context::Context,
    td: &TargetData,
    layout_ty: BasicTypeEnum<'ctx>,
) -> BasicTypeEnum<'ctx> {
    if is_integer_only_aggregate(layout_ty) {
        let size = td.get_store_size(&layout_ty) as u64;
        if size > 0 && size <= 16 {
            let bits = (size * 8) as u32;
            return context.custom_width_int_type(bits).as_basic_type_enum();
        }
    }
    layout_ty
}

pub fn abi_c_lower_extern_ret_type<'ctx>(
    context: &'ctx inkwell::context::Context,
    td: &TargetData,
    layout_ty: BasicTypeEnum<'ctx>,
) -> BasicTypeEnum<'ctx> {
    abi_c_lower_extern_param_type(context, td, layout_ty)
}

#[derive(Clone)]
pub struct VariableInfo<'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub mutability: Mutability,
    pub ty: WaveType,
}
