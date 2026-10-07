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

//! wasm C ABI classification; consumes shared transport records.
use super::shared::*;
use inkwell::targets::TargetData;
use inkwell::types::{AnyType, BasicTypeEnum};

fn wasm_single_leaf<'ctx>(t: BasicTypeEnum<'ctx>) -> Option<BasicTypeEnum<'ctx>> {
    let mut leaves = Vec::new();
    flatten_leaf_types(t, &mut leaves);
    (leaves.len() == 1).then_some(leaves[0])
}

pub(super) fn classify_param_wasm<'ctx>(
    td: &TargetData,
    t: BasicTypeEnum<'ctx>,
) -> ParamLowering<'ctx> {
    let is_aggregate = matches!(t, BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_));
    if !is_aggregate {
        return ParamLowering::Direct(t);
    }
    if td.get_store_size(&t) == 0 {
        return ParamLowering::Ignore;
    }
    if let Some(leaf) = wasm_single_leaf(t) {
        return ParamLowering::Direct(leaf);
    }
    ParamLowering::ByVal { ty: t.as_any_type_enum(), align: td.get_abi_alignment(&t) }
}

pub(super) fn classify_ret_wasm<'ctx>(
    td: &TargetData,
    t: Option<BasicTypeEnum<'ctx>>,
) -> RetLowering<'ctx> {
    let Some(t) = t else {
        return RetLowering::Void;
    };
    let is_aggregate = matches!(t, BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_));
    if !is_aggregate {
        return RetLowering::Direct(t);
    }
    if td.get_store_size(&t) == 0 {
        return RetLowering::Void;
    }
    if let Some(leaf) = wasm_single_leaf(t) {
        return RetLowering::Direct(leaf);
    }
    RetLowering::SRet { ty: t.as_any_type_enum(), align: td.get_abi_alignment(&t) }
}
