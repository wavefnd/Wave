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

//! C ABI transport records and target-independent layout helpers.
use inkwell::context::Context;
use inkwell::targets::TargetData;
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::AddressSpace;
use parser::ast::WaveType;

#[derive(Clone)]
pub struct AbiPart<'ctx> {
    pub ty: BasicTypeEnum<'ctx>,
    pub offset: u64,
}

#[derive(Clone)]
pub enum ParamLowering<'ctx> {
    Ignore,
    /// Pass the value as one LLVM parameter of this transport type.
    Direct(BasicTypeEnum<'ctx>),
    /// Decompose one Wave parameter into multiple LLVM parameters.
    Split(Vec<BasicTypeEnum<'ctx>>),
    /// Expand an aggregate into scalar parameters read from explicit byte
    /// offsets. LoongArch uses this for FAR/GAR-eligible structures.
    CoerceAndExpand(Vec<AbiPart<'ctx>>),
    /// Pass a pointer without attaching the C `byval` attribute.
    Indirect {
        ty: AnyTypeEnum<'ctx>,
    },
    /// Pass a pointer carrying the C `byval` size/alignment contract.
    ByVal {
        ty: AnyTypeEnum<'ctx>,
        align: u32,
    },
}

#[derive(Clone)]
pub enum RetLowering<'ctx> {
    Void,
    Direct(BasicTypeEnum<'ctx>),
    /// Return through a hidden first parameter with the `sret` attribute.
    SRet {
        ty: AnyTypeEnum<'ctx>,
        align: u32,
    },
}

#[derive(Clone, Copy)]
pub enum IntegerExtension {
    Sign,
    Zero,
}

#[derive(Clone)]
pub struct ExternCInfo<'ctx> {
    /// Actual symbol name emitted to LLVM.
    pub llvm_name: String,
    /// Source return type, retained when an `sret` function returns LLVM void.
    pub wave_ret: WaveType,
    pub ret: RetLowering<'ctx>,
    pub ret_extension: Option<IntegerExtension>,
    /// One classification for each source-level parameter.
    pub params: Vec<ParamLowering<'ctx>>,
    /// Narrow-integer extension contract for each source-level parameter.
    pub param_extensions: Vec<Option<IntegerExtension>>,
    /// Final LLVM parameter list, including hidden, split, and indirect values.
    pub llvm_param_types: Vec<BasicMetadataTypeEnum<'ctx>>,
    pub variadic: bool,
    pub variadic_integer_extension: Option<IntegerExtension>,
}

pub struct LoweredExtern<'ctx> {
    pub info: ExternCInfo<'ctx>,
    pub llvm_name: String,
    pub fn_type: inkwell::types::FunctionType<'ctx>,
}

pub(super) fn is_float_ty<'ctx>(td: &TargetData, t: BasicTypeEnum<'ctx>) -> Option<u32> {
    match t {
        BasicTypeEnum::FloatType(_) => Some(td.get_store_size(&t) as u32), // 4 or 8
        _ => None,
    }
}

pub(super) fn any_ptr_basic<'ctx>(
    context: &'ctx Context,
    ty: AnyTypeEnum<'ctx>,
) -> BasicTypeEnum<'ctx> {
    let aspace = AddressSpace::default();
    match ty {
        AnyTypeEnum::ArrayType(_)
        | AnyTypeEnum::FloatType(_)
        | AnyTypeEnum::FunctionType(_)
        | AnyTypeEnum::IntType(_)
        | AnyTypeEnum::PointerType(_)
        | AnyTypeEnum::StructType(_)
        | AnyTypeEnum::VectorType(_) => context.ptr_type(aspace).as_basic_type_enum(),
        _ => panic!("unsupported AnyTypeEnum for ptr"),
    }
}

pub(super) fn flatten_leaf_types<'ctx>(t: BasicTypeEnum<'ctx>, out: &mut Vec<BasicTypeEnum<'ctx>>) {
    match t {
        BasicTypeEnum::StructType(st) => {
            for i in 0..st.count_fields() {
                let f = st.get_field_type_at_index(i).unwrap();
                flatten_leaf_types(f, out);
            }
        },
        BasicTypeEnum::ArrayType(at) => {
            let elem = at.get_element_type();
            for _ in 0..at.len() {
                flatten_leaf_types(elem, out);
            }
        },
        _ => out.push(t),
    }
}
