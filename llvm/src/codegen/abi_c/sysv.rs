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

//! sysv C ABI classification; consumes shared transport records.
use super::shared::*;
use inkwell::context::Context;
use inkwell::targets::TargetData;
use inkwell::types::{AnyType, BasicType, BasicTypeEnum};

pub(super) fn classify_param_x86_64_sysv<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    t: BasicTypeEnum<'ctx>,
) -> ParamLowering<'ctx> {
    let size = td.get_store_size(&t) as u64;

    // large aggregates => byval
    if matches!(t, BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)) && size > 16 {
        let align = td.get_abi_alignment(&t) as u32;
        return ParamLowering::ByVal { ty: t.as_any_type_enum(), align };
    }

    if matches!(t, BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)) && size == 0 {
        return ParamLowering::Ignore;
    }

    // small aggregates: try integer-only or homogeneous float
    if matches!(t, BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)) && size <= 16 {
        let mut leaves = vec![];
        flatten_leaf_types(t, &mut leaves);

        if size <= 8 && leaves.len() == 1 {
            if let BasicTypeEnum::PointerType(pointer) = leaves[0] {
                return ParamLowering::Direct(pointer.as_basic_type_enum());
            }
        }

        // homogeneous float aggregate
        let mut float_kind: Option<u32> = None;
        let mut all_float = true;
        for lt in &leaves {
            if let Some(sz) = is_float_ty(td, *lt) {
                float_kind.get_or_insert(sz);
                if float_kind != Some(sz) {
                    all_float = false;
                    break;
                }
            } else {
                all_float = false;
                break;
            }
        }

        if all_float {
            let count = leaves.len();
            let fsz = float_kind.unwrap_or(0);
            if fsz == 4 {
                let f = context.f32_type();
                return match count {
                    1 => ParamLowering::Direct(f.as_basic_type_enum()),
                    2 => ParamLowering::Direct(f.vec_type(2).as_basic_type_enum()),
                    3 => ParamLowering::Split(vec![
                        f.vec_type(2).as_basic_type_enum(),
                        f.as_basic_type_enum(),
                    ]),
                    4 => ParamLowering::Split(vec![
                        f.vec_type(2).as_basic_type_enum(),
                        f.vec_type(2).as_basic_type_enum(),
                    ]),
                    _ => {
                        let align = td.get_abi_alignment(&t) as u32;
                        ParamLowering::ByVal { ty: t.as_any_type_enum(), align }
                    },
                };
            } else if fsz == 8 {
                let f = context.f64_type();
                return match count {
                    1 => ParamLowering::Direct(f.as_basic_type_enum()),
                    2 => ParamLowering::Split(vec![f.as_basic_type_enum(), f.as_basic_type_enum()]),
                    _ => {
                        let align = td.get_abi_alignment(&t) as u32;
                        ParamLowering::ByVal { ty: t.as_any_type_enum(), align }
                    },
                };
            }
        }

        // integer-only aggregate: coerce to i{size*8}
        let mut all_intlike = true;
        for lt in &leaves {
            match lt {
                BasicTypeEnum::IntType(_) | BasicTypeEnum::PointerType(_) => {},
                _ => {
                    all_intlike = false;
                    break;
                },
            }
        }
        if all_intlike {
            if size <= 8 {
                let bits = (size * 8) as u32;
                let it = context.custom_width_int_type(bits);
                return ParamLowering::Direct(it.as_basic_type_enum());
            }

            let rem_bits = ((size - 8) * 8) as u32;
            let hi = context.i64_type().as_basic_type_enum();
            let lo = if rem_bits == 64 {
                context.i64_type().as_basic_type_enum()
            } else {
                context.custom_width_int_type(rem_bits).as_basic_type_enum()
            };
            return ParamLowering::Split(vec![hi, lo]);
        }

        // Classify each eightbyte at its storage offset. INTEGER wins when
        // integer and floating members share an eightbyte.
        let mut classes = [(false, false); 2];
        sysv_eightbyte_classes(td, t, 0, &mut classes);
        return ParamLowering::CoerceAndExpand(
            classes
                .iter()
                .enumerate()
                .filter_map(|(index, &(integer, sse))| {
                    let offset = index as u64 * 8;
                    if !integer && !sse {
                        return None;
                    }
                    let bytes = (size - offset).min(8);
                    let ty = if integer {
                        context.custom_width_int_type(bytes as u32 * 8).as_basic_type_enum()
                    } else if bytes <= 4 {
                        context.f32_type().as_basic_type_enum()
                    } else {
                        context.f64_type().as_basic_type_enum()
                    };
                    Some(AbiPart { ty, offset })
                })
                .collect(),
        );
    }

    // non-aggregate: direct
    ParamLowering::Direct(t)
}

fn sysv_eightbyte_classes(
    td: &TargetData,
    ty: BasicTypeEnum<'_>,
    offset: u64,
    classes: &mut [(bool, bool); 2],
) {
    match ty {
        BasicTypeEnum::StructType(structure) => {
            for (index, field) in structure.get_field_types().into_iter().enumerate() {
                sysv_eightbyte_classes(
                    td,
                    field,
                    offset + td.offset_of_element(&structure, index as u32).unwrap(),
                    classes,
                );
            }
        },
        BasicTypeEnum::ArrayType(array) => {
            let element = array.get_element_type();
            for index in 0..array.len() {
                sysv_eightbyte_classes(
                    td,
                    element,
                    offset + u64::from(index) * td.get_abi_size(&element),
                    classes,
                );
            }
        },
        _ => {
            let end = offset + td.get_store_size(&ty);
            for byte in offset..end {
                let class = &mut classes[(byte / 8) as usize];
                if matches!(ty, BasicTypeEnum::FloatType(_) | BasicTypeEnum::VectorType(_)) {
                    class.1 = true;
                } else {
                    class.0 = true;
                }
            }
        },
    }
}

fn sysv_registers(ty: BasicTypeEnum<'_>) -> (u32, u32) {
    match ty {
        BasicTypeEnum::IntType(integer) => (integer.get_bit_width().div_ceil(64), 0),
        BasicTypeEnum::PointerType(_) => (1, 0),
        BasicTypeEnum::FloatType(_) | BasicTypeEnum::VectorType(_) => (0, 1),
        _ => unreachable!("SysV transport must be scalar or vector"),
    }
}

pub(super) fn classify_param_sysv_with_registers<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    ty: BasicTypeEnum<'ctx>,
    gp_left: &mut u32,
    sse_left: &mut u32,
) -> ParamLowering<'ctx> {
    let lowering = classify_param_x86_64_sysv(context, td, ty);
    let (gp, sse) = match &lowering {
        ParamLowering::Direct(ty) => sysv_registers(*ty),
        ParamLowering::Split(parts) => {
            parts.iter().map(|ty| sysv_registers(*ty)).fold((0, 0), |a, b| (a.0 + b.0, a.1 + b.1))
        },
        ParamLowering::CoerceAndExpand(parts) => parts
            .iter()
            .map(|part| sysv_registers(part.ty))
            .fold((0, 0), |a, b| (a.0 + b.0, a.1 + b.1)),
        _ => (0, 0),
    };
    if matches!(ty, BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_))
        && (gp > *gp_left || sse > *sse_left)
    {
        // Roll back the entire aggregate so later arguments can use the
        // remaining registers. A byval stack slot consumes neither budget.
        return ParamLowering::ByVal {
            ty: ty.as_any_type_enum(),
            align: td.get_abi_alignment(&ty).max(8),
        };
    }
    *gp_left = gp_left.saturating_sub(gp);
    *sse_left = sse_left.saturating_sub(sse);
    lowering
}

pub(super) fn classify_ret_x86_64_sysv<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    t: Option<BasicTypeEnum<'ctx>>,
) -> RetLowering<'ctx> {
    let Some(t) = t else {
        return RetLowering::Void;
    };

    let size = td.get_store_size(&t) as u64;
    let is_agg = matches!(t, BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_));

    if is_agg && size > 16 {
        let align = td.get_abi_alignment(&t) as u32;
        return RetLowering::SRet { ty: t.as_any_type_enum(), align };
    }

    if is_agg && size == 0 {
        return RetLowering::Void;
    }

    if is_agg && size <= 16 {
        // integer-only ret => i{size*8}
        let mut leaves = vec![];
        flatten_leaf_types(t, &mut leaves);

        if size <= 8 && leaves.len() == 1 {
            if let BasicTypeEnum::PointerType(pointer) = leaves[0] {
                return RetLowering::Direct(pointer.as_basic_type_enum());
            }
        }

        let mut all_intlike = true;
        for lt in &leaves {
            match lt {
                BasicTypeEnum::IntType(_) | BasicTypeEnum::PointerType(_) => {},
                _ => {
                    all_intlike = false;
                    break;
                },
            }
        }
        if all_intlike {
            if size <= 8 {
                let bits = (size * 8) as u32;
                let it = context.custom_width_int_type(bits);
                return RetLowering::Direct(it.as_basic_type_enum());
            }

            let rem_bits = ((size - 8) * 8) as u32;
            let hi = context.i64_type().as_basic_type_enum();
            let lo = if rem_bits == 64 {
                context.i64_type().as_basic_type_enum()
            } else {
                context.custom_width_int_type(rem_bits).as_basic_type_enum()
            };
            let tuple = context.struct_type(&[hi, lo], false).as_basic_type_enum();
            return RetLowering::Direct(tuple);
        }

        // homogeneous float ret (2 or 4 only to avoid multi-reg return complexity)
        let mut float_kind: Option<u32> = None;
        let mut all_float = true;
        for lt in &leaves {
            if let Some(sz) = is_float_ty(td, *lt) {
                float_kind.get_or_insert(sz);
                if float_kind != Some(sz) {
                    all_float = false;
                    break;
                }
            } else {
                all_float = false;
                break;
            }
        }
        if all_float {
            let count = leaves.len();
            if float_kind == Some(4) {
                let f = context.f32_type();
                return match count {
                    1 => RetLowering::Direct(f.as_basic_type_enum()),
                    2 => RetLowering::Direct(f.vec_type(2).as_basic_type_enum()),
                    3 => {
                        let tuple = context
                            .struct_type(
                                &[f.vec_type(2).as_basic_type_enum(), f.as_basic_type_enum()],
                                false,
                            )
                            .as_basic_type_enum();
                        RetLowering::Direct(tuple)
                    },
                    4 => {
                        let tuple = context
                            .struct_type(
                                &[
                                    f.vec_type(2).as_basic_type_enum(),
                                    f.vec_type(2).as_basic_type_enum(),
                                ],
                                false,
                            )
                            .as_basic_type_enum();
                        RetLowering::Direct(tuple)
                    },
                    _ => {
                        let align = td.get_abi_alignment(&t) as u32;
                        RetLowering::SRet { ty: t.as_any_type_enum(), align }
                    },
                };
            }
            if float_kind == Some(8) {
                let f = context.f64_type();
                return match count {
                    1 => RetLowering::Direct(f.as_basic_type_enum()),
                    2 => {
                        let tuple = context
                            .struct_type(&[f.as_basic_type_enum(), f.as_basic_type_enum()], false)
                            .as_basic_type_enum();
                        RetLowering::Direct(tuple)
                    },
                    _ => {
                        let align = td.get_abi_alignment(&t) as u32;
                        RetLowering::SRet { ty: t.as_any_type_enum(), align }
                    },
                };
            }
        }

        // mixed small aggregate ret: keep direct aggregate value.
        // Let LLVM's C ABI lowering pick mixed INTEGER/SSE return registers.
        return RetLowering::Direct(t);
    }

    RetLowering::Direct(t)
}
