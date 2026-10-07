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

//! loongarch C ABI classification; consumes shared transport records.
use super::shared::*;
use inkwell::context::Context;
use inkwell::targets::TargetData;
use inkwell::types::{AnyType, BasicType, BasicTypeEnum};

fn flatten_loongarch_fields<'ctx>(
    td: &TargetData,
    ty: BasicTypeEnum<'ctx>,
    base_offset: u64,
    frlen_bytes: u64,
    fields: &mut Vec<AbiPart<'ctx>>,
) -> bool {
    if fields.len() > 2 {
        return false;
    }
    match ty {
        BasicTypeEnum::StructType(struct_ty) => {
            for index in 0..struct_ty.count_fields() {
                let Some(field_ty) = struct_ty.get_field_type_at_index(index) else {
                    return false;
                };
                let Some(offset) = td.offset_of_element(&struct_ty, index) else {
                    return false;
                };
                if !flatten_loongarch_fields(
                    td,
                    field_ty,
                    base_offset + offset,
                    frlen_bytes,
                    fields,
                ) {
                    return false;
                }
            }
            true
        },
        BasicTypeEnum::ArrayType(array_ty) => {
            let element = array_ty.get_element_type();
            let stride = td.get_abi_size(&element);
            for index in 0..array_ty.len() {
                if !flatten_loongarch_fields(
                    td,
                    element,
                    base_offset + u64::from(index) * stride,
                    frlen_bytes,
                    fields,
                ) {
                    return false;
                }
            }
            true
        },
        BasicTypeEnum::IntType(int_ty) if int_ty.get_bit_width() <= 64 => {
            fields.push(AbiPart { ty, offset: base_offset });
            fields.len() <= 2
        },
        BasicTypeEnum::FloatType(_)
            if frlen_bytes != 0 && td.get_store_size(&ty) <= frlen_bytes =>
        {
            fields.push(AbiPart { ty, offset: base_offset });
            fields.len() <= 2
        },
        _ => false,
    }
}

fn loongarch_fars_eligible_struct<'ctx>(
    td: &TargetData,
    ty: BasicTypeEnum<'ctx>,
    frlen_bytes: u64,
) -> Option<(Vec<AbiPart<'ctx>>, usize, usize)> {
    if !matches!(ty, BasicTypeEnum::StructType(_)) {
        return None;
    }
    let mut fields = Vec::new();
    if !flatten_loongarch_fields(td, ty, 0, frlen_bytes, &mut fields) || fields.is_empty() {
        return None;
    }
    let fars =
        fields.iter().filter(|field| matches!(field.ty, BasicTypeEnum::FloatType(_))).count();
    let gars = fields.len() - fars;
    if fars == 0 || gars > 1 {
        return None;
    }
    Some((fields, gars, fars))
}

fn consume_loongarch_gars(td: &TargetData, ty: BasicTypeEnum<'_>, gars_left: &mut usize) {
    let size = td.get_store_size(&ty);
    let required = if size > 16 {
        1
    } else if size > 8 {
        2
    } else {
        1
    };
    *gars_left = gars_left.saturating_sub(required.min(*gars_left));
}

pub(super) fn loongarch_frlen_bytes(target_abi: Option<&str>) -> u64 {
    match target_abi.unwrap_or("lp64d") {
        "lp64s" => 0,
        "lp64f" => 4,
        "lp64d" => 8,
        abi => panic!("unsupported LoongArch ABI reached C ABI lowering: {abi}"),
    }
}

pub(super) fn classify_param_loongarch64<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    ty: BasicTypeEnum<'ctx>,
    frlen_bytes: u64,
    gars_left: &mut usize,
    fars_left: &mut usize,
) -> ParamLowering<'ctx> {
    let size = td.get_store_size(&ty);
    let is_aggregate = matches!(ty, BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_));
    if is_aggregate && size == 0 {
        return ParamLowering::Ignore;
    }

    if matches!(ty, BasicTypeEnum::FloatType(_))
        && frlen_bytes != 0
        && size <= frlen_bytes
        && *fars_left > 0
    {
        *fars_left -= 1;
        return ParamLowering::Direct(ty);
    }

    if let Some((fields, needed_gars, needed_fars)) =
        loongarch_fars_eligible_struct(td, ty, frlen_bytes)
    {
        if needed_gars <= *gars_left && needed_fars <= *fars_left {
            *gars_left -= needed_gars;
            *fars_left -= needed_fars;
            return ParamLowering::CoerceAndExpand(fields);
        }
    }

    consume_loongarch_gars(td, ty, gars_left);
    if is_aggregate && size > 16 {
        return ParamLowering::Indirect { ty: ty.as_any_type_enum() };
    }
    if is_aggregate {
        return if size <= 8 {
            ParamLowering::Direct(context.i64_type().as_basic_type_enum())
        } else {
            ParamLowering::Direct(context.i64_type().array_type(2).as_basic_type_enum())
        };
    }
    ParamLowering::Direct(ty)
}

pub(super) fn classify_ret_loongarch64<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    ty: Option<BasicTypeEnum<'ctx>>,
    frlen_bytes: u64,
) -> RetLowering<'ctx> {
    let Some(ty) = ty else {
        return RetLowering::Void;
    };
    let size = td.get_store_size(&ty);
    let is_aggregate = matches!(ty, BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_));
    if is_aggregate && size == 0 {
        return RetLowering::Void;
    }
    if let Some((fields, needed_gars, needed_fars)) =
        loongarch_fars_eligible_struct(td, ty, frlen_bytes)
    {
        if needed_gars <= 2 && needed_fars <= 2 {
            if fields.len() == 1 {
                return RetLowering::Direct(fields[0].ty);
            }
            return RetLowering::Direct(
                context
                    .struct_type(&fields.iter().map(|field| field.ty).collect::<Vec<_>>(), false)
                    .as_basic_type_enum(),
            );
        }
    }
    if is_aggregate && size > 16 {
        return RetLowering::SRet { ty: ty.as_any_type_enum(), align: td.get_abi_alignment(&ty) };
    }
    if is_aggregate {
        return if size <= 8 {
            RetLowering::Direct(context.i64_type().as_basic_type_enum())
        } else {
            RetLowering::Direct(context.i64_type().array_type(2).as_basic_type_enum())
        };
    }
    RetLowering::Direct(ty)
}

// Clang's WebAssembly C ABI unwraps aggregates that contain exactly one scalar
// leaf. Other non-empty aggregates are passed by value through linear memory
// and returned through an sret pointer.
