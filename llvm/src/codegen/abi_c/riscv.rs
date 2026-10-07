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

//! riscv C ABI classification; consumes shared transport records.
use super::shared::*;
use inkwell::context::Context;
use inkwell::targets::TargetData;
use inkwell::types::{AnyType, BasicType, BasicTypeEnum};

// Floating-point aggregate eligibility is independent of object layout. Ineligible
// or register-exhausted aggregates use the integer calling convention.
fn float_fields<'ctx>(
    td: &TargetData,
    ty: BasicTypeEnum<'ctx>,
    offset: u64,
    flen: u64,
    fields: &mut Vec<AbiPart<'ctx>>,
) -> bool {
    if fields.len() > 2 {
        return false;
    }
    match ty {
        BasicTypeEnum::StructType(st) => {
            for (i, field) in st.get_field_types().into_iter().enumerate() {
                if !float_fields(
                    td,
                    field,
                    offset + td.offset_of_element(&st, i as u32).unwrap(),
                    flen,
                    fields,
                ) {
                    return false;
                }
            }
        },
        BasicTypeEnum::ArrayType(array) => {
            let element = array.get_element_type();
            for i in 0..array.len() {
                if !float_fields(
                    td,
                    element,
                    offset + u64::from(i) * td.get_abi_size(&element),
                    flen,
                    fields,
                ) {
                    return false;
                }
            }
        },
        BasicTypeEnum::FloatType(_) if flen > 0 && td.get_store_size(&ty) <= flen => {
            fields.push(AbiPart { ty, offset })
        },
        BasicTypeEnum::IntType(integer) if integer.get_bit_width() <= 64 => {
            fields.push(AbiPart { ty, offset })
        },
        // The hardware FP convention accepts integer fields, not pointer fields.
        BasicTypeEnum::PointerType(_) => return false,
        _ => return false,
    }
    fields.len() <= 2
}

fn eligible<'ctx>(
    td: &TargetData,
    ty: BasicTypeEnum<'ctx>,
    flen: u64,
) -> Option<(Vec<AbiPart<'ctx>>, usize, usize)> {
    if !matches!(ty, BasicTypeEnum::StructType(_)) {
        return None;
    }
    let mut fields = Vec::new();
    if !float_fields(td, ty, 0, flen, &mut fields) {
        return None;
    }
    let fp = fields.iter().filter(|f| matches!(f.ty, BasicTypeEnum::FloatType(_))).count();
    let gp = fields.len() - fp;
    (fp > 0 && gp <= 1).then_some((fields, gp, fp))
}

pub(super) fn classify_param_riscv64<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    ty: BasicTypeEnum<'ctx>,
    flen: u64,
    gp_left: &mut usize,
    fp_left: &mut usize,
) -> ParamLowering<'ctx> {
    let size = td.get_store_size(&ty);
    let aggregate = matches!(ty, BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_));
    if aggregate && size == 0 {
        return ParamLowering::Ignore;
    }
    if matches!(ty, BasicTypeEnum::FloatType(_)) && size <= flen && *fp_left > 0 {
        *fp_left -= 1;
        return ParamLowering::Direct(ty);
    }
    if let Some((parts, gp, fp)) = eligible(td, ty, flen) {
        if gp <= *gp_left && fp <= *fp_left {
            *gp_left -= gp;
            *fp_left -= fp;
            return ParamLowering::CoerceAndExpand(parts);
        }
    }
    *gp_left = gp_left.saturating_sub(if size > 8 && size <= 16 { 2 } else { 1 });
    if aggregate {
        if size > 16 {
            return ParamLowering::Indirect { ty: ty.as_any_type_enum() };
        }
        return ParamLowering::Direct(if size <= 8 {
            context.i64_type().as_basic_type_enum()
        } else {
            context.i64_type().array_type(2).as_basic_type_enum()
        });
    }
    ParamLowering::Direct(ty)
}

pub(super) fn classify_ret_riscv64<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    ty: Option<BasicTypeEnum<'ctx>>,
    flen: u64,
) -> RetLowering<'ctx> {
    let Some(ty) = ty else {
        return RetLowering::Void;
    };
    let size = td.get_store_size(&ty);
    let aggregate = matches!(ty, BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_));
    if aggregate && size == 0 {
        return RetLowering::Void;
    }
    if let Some((parts, _, _)) = eligible(td, ty, flen) {
        return RetLowering::Direct(if parts.len() == 1 {
            parts[0].ty
        } else {
            context
                .struct_type(&parts.iter().map(|part| part.ty).collect::<Vec<_>>(), false)
                .as_basic_type_enum()
        });
    }
    if aggregate {
        if size > 16 {
            return RetLowering::SRet {
                ty: ty.as_any_type_enum(),
                align: td.get_abi_alignment(&ty),
            };
        }
        return RetLowering::Direct(if size <= 8 {
            context.i64_type().as_basic_type_enum()
        } else {
            context.i64_type().array_type(2).as_basic_type_enum()
        });
    }
    RetLowering::Direct(ty)
}
