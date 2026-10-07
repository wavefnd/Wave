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

//! aarch64 C ABI classification; consumes shared transport records.
use super::shared::*;
use inkwell::context::Context;
use inkwell::targets::TargetData;
use inkwell::types::{AnyType, BasicType, BasicTypeEnum};

fn is_homogeneous_float_aggregate<'ctx>(td: &TargetData, t: BasicTypeEnum<'ctx>) -> bool {
    let mut leaves = Vec::new();
    flatten_leaf_types(t, &mut leaves);
    if leaves.is_empty() || leaves.len() > 4 {
        return false;
    }

    let Some(first_size) = is_float_ty(td, leaves[0]) else {
        return false;
    };
    leaves.iter().all(|leaf| is_float_ty(td, *leaf) == Some(first_size))
}

pub(super) fn classify_param_arm64<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    t: BasicTypeEnum<'ctx>,
    allow_hfa: bool,
) -> ParamLowering<'ctx> {
    let size = td.get_store_size(&t) as u64;
    let is_agg = matches!(t, BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_));

    // AAPCS64 stage B checks HFAs before the general >16-byte rule. Four
    // doubles still travel in FP registers (or together on stack when exhausted).
    if is_agg && allow_hfa && is_homogeneous_float_aggregate(td, t) {
        let mut leaves = Vec::new();
        flatten_leaf_types(t, &mut leaves);
        return ParamLowering::Direct(
            leaves[0].into_float_type().array_type(leaves.len() as u32).as_basic_type_enum(),
        );
    }

    if is_agg && size > 16 {
        return ParamLowering::Indirect { ty: t.as_any_type_enum() };
    }

    if is_agg && size == 0 {
        return ParamLowering::Ignore;
    }

    if is_agg {
        let mut leaves = Vec::new();
        flatten_leaf_types(t, &mut leaves);
        if size <= 8 && leaves.len() == 1 {
            if let BasicTypeEnum::PointerType(pointer) = leaves[0] {
                return ParamLowering::Direct(pointer.as_basic_type_enum());
            }
        }
        if size <= 8 {
            // AAPCS64 transports a non-HFA aggregate occupying at most one
            // general-purpose register in a full 64-bit ABI slot. The object
            // representation remains its original (possibly odd) byte size.
            return ParamLowering::Direct(context.i64_type().as_basic_type_enum());
        }

        if td.get_abi_alignment(&t) >= 16 {
            return ParamLowering::Direct(context.i128_type().as_basic_type_enum());
        }
        return ParamLowering::Direct(context.i64_type().array_type(2).as_basic_type_enum());
    }

    ParamLowering::Direct(t)
}

pub(super) fn classify_ret_arm64<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    t: Option<BasicTypeEnum<'ctx>>,
) -> RetLowering<'ctx> {
    let Some(t) = t else {
        return RetLowering::Void;
    };
    let size = td.get_store_size(&t) as u64;
    let is_agg = matches!(t, BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_));

    // HFA results use v0-v3 even when their total size exceeds 16 bytes.
    if is_agg && is_homogeneous_float_aggregate(td, t) {
        return RetLowering::Direct(t);
    }

    if is_agg && size > 16 {
        let align = td.get_abi_alignment(&t) as u32;
        return RetLowering::SRet { ty: t.as_any_type_enum(), align };
    }

    if is_agg && size == 0 {
        return RetLowering::Void;
    }

    if is_agg {
        if size <= 8 {
            return RetLowering::Direct(
                context.custom_width_int_type((size * 8) as u32).as_basic_type_enum(),
            );
        }

        return RetLowering::Direct(context.i64_type().array_type(2).as_basic_type_enum());
    }

    RetLowering::Direct(t)
}
