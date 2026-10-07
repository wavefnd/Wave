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

//! windows C ABI classification; consumes shared transport records.
use super::shared::*;
use inkwell::context::Context;
use inkwell::targets::TargetData;
use inkwell::types::{AnyType, BasicType, BasicTypeEnum};

pub(super) fn classify_param_x86_64_windows<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    t: BasicTypeEnum<'ctx>,
) -> ParamLowering<'ctx> {
    let size = td.get_store_size(&t) as u64;

    match t {
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_) => match size {
            0 => ParamLowering::Ignore,
            1 | 2 | 4 | 8 => ParamLowering::Direct(
                context.custom_width_int_type((size * 8) as u32).as_basic_type_enum(),
            ),
            _ => ParamLowering::Indirect { ty: t.as_any_type_enum() },
        },
        _ => ParamLowering::Direct(t),
    }
}

pub(super) fn classify_ret_x86_64_windows<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    t: Option<BasicTypeEnum<'ctx>>,
) -> RetLowering<'ctx> {
    let Some(t) = t else {
        return RetLowering::Void;
    };
    let size = td.get_store_size(&t) as u64;

    match t {
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_) => match size {
            0 => RetLowering::Void,
            1 | 2 | 4 | 8 => RetLowering::Direct(
                context.custom_width_int_type((size * 8) as u32).as_basic_type_enum(),
            ),
            _ => RetLowering::SRet {
                ty: t.as_any_type_enum(),
                align: td.get_abi_alignment(&t) as u32,
            },
        },
        _ => RetLowering::Direct(t),
    }
}
