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

//! Target-specific C calling-convention classification.
//!
//! Wave object representation and C ABI transport representation are separate:
//! an aggregate may be passed directly, split across registers, ignored, or
//! addressed indirectly without changing its in-memory layout. Classification
//! happens here; call and function emission only apply the resulting contract.
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::context::Context;
use inkwell::targets::TargetData;
use inkwell::types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, CallSiteValue, FunctionValue};
use inkwell::AddressSpace;
use std::collections::HashMap;

use parser::ast::{ExternFunctionNode, WaveType};

use super::target::CodegenTarget;
use super::types::{wave_type_to_llvm_type, TypeFlavor};

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

fn integer_extension_for_target(target: CodegenTarget, ty: &WaveType) -> Option<IntegerExtension> {
    let narrow_extension = || match ty {
        WaveType::Int(bits) if *bits < 32 => Some(IntegerExtension::Sign),
        WaveType::Uint(bits) if *bits < 32 => Some(IntegerExtension::Zero),
        WaveType::Bool | WaveType::Byte | WaveType::Char => Some(IntegerExtension::Zero),
        _ => None,
    };

    match target {
        CodegenTarget::LinuxX86_64
        | CodegenTarget::DarwinX86_64
        | CodegenTarget::FreeBsdX86_64
        | CodegenTarget::FreestandingX86_64
        | CodegenTarget::DarwinArm64 => narrow_extension(),
        CodegenTarget::LinuxRISCV64
        | CodegenTarget::FreestandingRISCV64
        | CodegenTarget::LinuxLoongArch64 => match ty {
            WaveType::Int(bits) if *bits <= 32 => Some(IntegerExtension::Sign),
            WaveType::Uint(bits) if *bits < 32 => Some(IntegerExtension::Zero),
            // RV64 widens u32 to 32 bits and then sign-extends it to XLEN.
            WaveType::Uint(32) => Some(IntegerExtension::Sign),
            WaveType::Bool | WaveType::Byte | WaveType::Char => Some(IntegerExtension::Zero),
            _ => None,
        },
        CodegenTarget::LinuxArm64
        | CodegenTarget::FreestandingArm64
        | CodegenTarget::WindowsX86_64Gnu
        | CodegenTarget::WindowsArm64Gnu => None,
        CodegenTarget::Wasm32Unknown
        | CodegenTarget::Wasm32WasiP1
        | CodegenTarget::Wasm64Unknown => narrow_extension(),
    }
}

pub struct LoweredExtern<'ctx> {
    pub info: ExternCInfo<'ctx>,
    pub llvm_name: String,
    pub fn_type: inkwell::types::FunctionType<'ctx>,
}

fn is_float_ty<'ctx>(td: &TargetData, t: BasicTypeEnum<'ctx>) -> Option<u32> {
    match t {
        BasicTypeEnum::FloatType(_) => Some(td.get_store_size(&t) as u32), // 4 or 8
        _ => None,
    }
}

fn any_ptr_basic<'ctx>(context: &'ctx Context, ty: AnyTypeEnum<'ctx>) -> BasicTypeEnum<'ctx> {
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

fn flatten_leaf_types<'ctx>(t: BasicTypeEnum<'ctx>, out: &mut Vec<BasicTypeEnum<'ctx>>) {
    match t {
        BasicTypeEnum::StructType(st) => {
            for i in 0..st.count_fields() {
                let f = st.get_field_type_at_index(i).unwrap();
                flatten_leaf_types(f, out);
            }
        }
        BasicTypeEnum::ArrayType(at) => {
            let elem = at.get_element_type();
            for _ in 0..at.len() {
                flatten_leaf_types(elem, out);
            }
        }
        _ => out.push(t),
    }
}

fn classify_param_x86_64_sysv<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    t: BasicTypeEnum<'ctx>,
) -> ParamLowering<'ctx> {
    let size = td.get_store_size(&t) as u64;

    // large aggregates => byval
    if matches!(
        t,
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)
    ) && size > 16
    {
        let align = td.get_abi_alignment(&t) as u32;
        return ParamLowering::ByVal {
            ty: t.as_any_type_enum(),
            align,
        };
    }

    if matches!(
        t,
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)
    ) && size == 0
    {
        return ParamLowering::Ignore;
    }

    // small aggregates: try integer-only or homogeneous float
    if matches!(
        t,
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)
    ) && size <= 16
    {
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
                        ParamLowering::ByVal {
                            ty: t.as_any_type_enum(),
                            align,
                        }
                    }
                };
            } else if fsz == 8 {
                let f = context.f64_type();
                return match count {
                    1 => ParamLowering::Direct(f.as_basic_type_enum()),
                    2 => ParamLowering::Split(vec![f.as_basic_type_enum(), f.as_basic_type_enum()]),
                    _ => {
                        let align = td.get_abi_alignment(&t) as u32;
                        ParamLowering::ByVal {
                            ty: t.as_any_type_enum(),
                            align,
                        }
                    }
                };
            }
        }

        // integer-only aggregate: coerce to i{size*8}
        let mut all_intlike = true;
        for lt in &leaves {
            match lt {
                BasicTypeEnum::IntType(_) | BasicTypeEnum::PointerType(_) => {}
                _ => {
                    all_intlike = false;
                    break;
                }
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

        // mixed small aggregate: keep as direct aggregate value.
        // Let LLVM's C ABI lowering split/register-assign correctly.
        return ParamLowering::Direct(t);
    }

    // non-aggregate: direct
    ParamLowering::Direct(t)
}

fn classify_ret_x86_64_sysv<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    t: Option<BasicTypeEnum<'ctx>>,
) -> RetLowering<'ctx> {
    let Some(t) = t else {
        return RetLowering::Void;
    };

    let size = td.get_store_size(&t) as u64;
    let is_agg = matches!(
        t,
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)
    );

    if is_agg && size > 16 {
        let align = td.get_abi_alignment(&t) as u32;
        return RetLowering::SRet {
            ty: t.as_any_type_enum(),
            align,
        };
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
                BasicTypeEnum::IntType(_) | BasicTypeEnum::PointerType(_) => {}
                _ => {
                    all_intlike = false;
                    break;
                }
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
                    }
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
                    }
                    _ => {
                        let align = td.get_abi_alignment(&t) as u32;
                        RetLowering::SRet {
                            ty: t.as_any_type_enum(),
                            align,
                        }
                    }
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
                    }
                    _ => {
                        let align = td.get_abi_alignment(&t) as u32;
                        RetLowering::SRet {
                            ty: t.as_any_type_enum(),
                            align,
                        }
                    }
                };
            }
        }

        // mixed small aggregate ret: keep direct aggregate value.
        // Let LLVM's C ABI lowering pick mixed INTEGER/SSE return registers.
        return RetLowering::Direct(t);
    }

    RetLowering::Direct(t)
}

fn classify_param_x86_64_windows<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    t: BasicTypeEnum<'ctx>,
) -> ParamLowering<'ctx> {
    let size = td.get_store_size(&t) as u64;

    match t {
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_) => match size {
            0 => ParamLowering::Ignore,
            1 | 2 | 4 | 8 => ParamLowering::Direct(
                context
                    .custom_width_int_type((size * 8) as u32)
                    .as_basic_type_enum(),
            ),
            _ => ParamLowering::ByVal {
                ty: t.as_any_type_enum(),
                align: td.get_abi_alignment(&t) as u32,
            },
        },
        _ => ParamLowering::Direct(t),
    }
}

fn classify_ret_x86_64_windows<'ctx>(
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
                context
                    .custom_width_int_type((size * 8) as u32)
                    .as_basic_type_enum(),
            ),
            _ => RetLowering::SRet {
                ty: t.as_any_type_enum(),
                align: td.get_abi_alignment(&t) as u32,
            },
        },
        _ => RetLowering::Direct(t),
    }
}

fn is_homogeneous_float_aggregate<'ctx>(td: &TargetData, t: BasicTypeEnum<'ctx>) -> bool {
    let mut leaves = Vec::new();
    flatten_leaf_types(t, &mut leaves);
    if leaves.is_empty() || leaves.len() > 4 {
        return false;
    }

    let Some(first_size) = is_float_ty(td, leaves[0]) else {
        return false;
    };
    leaves
        .iter()
        .all(|leaf| is_float_ty(td, *leaf) == Some(first_size))
}

fn classify_param_arm64<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    t: BasicTypeEnum<'ctx>,
) -> ParamLowering<'ctx> {
    let size = td.get_store_size(&t) as u64;
    let is_agg = matches!(
        t,
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)
    );

    if is_agg && size > 16 {
        return ParamLowering::Indirect {
            ty: t.as_any_type_enum(),
        };
    }

    if is_agg && size == 0 {
        return ParamLowering::Ignore;
    }

    if is_agg && !is_homogeneous_float_aggregate(td, t) {
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

        return ParamLowering::Direct(context.i64_type().array_type(2).as_basic_type_enum());
    }

    ParamLowering::Direct(t)
}

fn classify_param_riscv64<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    t: BasicTypeEnum<'ctx>,
) -> ParamLowering<'ctx> {
    let size = td.get_store_size(&t) as u64;
    let is_agg = matches!(
        t,
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)
    );

    if is_agg && size > 16 {
        let align = td.get_abi_alignment(&t) as u32;
        return ParamLowering::ByVal {
            ty: t.as_any_type_enum(),
            align,
        };
    }

    if is_agg && size == 0 {
        return ParamLowering::Ignore;
    }

    if is_agg {
        let mut leaves = Vec::new();
        flatten_leaf_types(t, &mut leaves);
        let integer_only = leaves.iter().all(|leaf| {
            matches!(
                leaf,
                BasicTypeEnum::IntType(_) | BasicTypeEnum::PointerType(_)
            )
        });

        if integer_only {
            if size <= 8 {
                // The RV64 psABI uses an XLEN-sized transport slot for an
                // integer aggregate that fits in one argument register.
                return ParamLowering::Direct(context.i64_type().as_basic_type_enum());
            }

            return ParamLowering::Direct(context.i64_type().array_type(2).as_basic_type_enum());
        }
    }

    ParamLowering::Direct(t)
}

fn classify_ret_arm64<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    t: Option<BasicTypeEnum<'ctx>>,
) -> RetLowering<'ctx> {
    let Some(t) = t else {
        return RetLowering::Void;
    };
    let size = td.get_store_size(&t) as u64;
    let is_agg = matches!(
        t,
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)
    );

    if is_agg && size > 16 {
        let align = td.get_abi_alignment(&t) as u32;
        return RetLowering::SRet {
            ty: t.as_any_type_enum(),
            align,
        };
    }

    if is_agg && size == 0 {
        return RetLowering::Void;
    }

    if is_agg && !is_homogeneous_float_aggregate(td, t) {
        if size <= 8 {
            return RetLowering::Direct(
                context
                    .custom_width_int_type((size * 8) as u32)
                    .as_basic_type_enum(),
            );
        }

        return RetLowering::Direct(context.i64_type().array_type(2).as_basic_type_enum());
    }

    RetLowering::Direct(t)
}

fn classify_ret_riscv64<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    t: Option<BasicTypeEnum<'ctx>>,
) -> RetLowering<'ctx> {
    let Some(t) = t else {
        return RetLowering::Void;
    };
    let size = td.get_store_size(&t) as u64;
    let is_agg = matches!(
        t,
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)
    );

    if is_agg && size > 16 {
        let align = td.get_abi_alignment(&t) as u32;
        return RetLowering::SRet {
            ty: t.as_any_type_enum(),
            align,
        };
    }

    if is_agg && size == 0 {
        return RetLowering::Void;
    }

    if is_agg {
        let mut leaves = Vec::new();
        flatten_leaf_types(t, &mut leaves);
        let integer_only = leaves.iter().all(|leaf| {
            matches!(
                leaf,
                BasicTypeEnum::IntType(_) | BasicTypeEnum::PointerType(_)
            )
        });

        if integer_only {
            if size <= 8 {
                // Keep the aggregate object size separate from its XLEN-sized
                // ABI return transport representation.
                return RetLowering::Direct(context.i64_type().as_basic_type_enum());
            }

            return RetLowering::Direct(context.i64_type().array_type(2).as_basic_type_enum());
        }
    }

    RetLowering::Direct(t)
}

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
        }
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
        }
        BasicTypeEnum::IntType(int_ty) if int_ty.get_bit_width() <= 64 => {
            fields.push(AbiPart {
                ty,
                offset: base_offset,
            });
            fields.len() <= 2
        }
        BasicTypeEnum::FloatType(_)
            if frlen_bytes != 0 && td.get_store_size(&ty) <= frlen_bytes =>
        {
            fields.push(AbiPart {
                ty,
                offset: base_offset,
            });
            fields.len() <= 2
        }
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
    let fars = fields
        .iter()
        .filter(|field| matches!(field.ty, BasicTypeEnum::FloatType(_)))
        .count();
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

fn loongarch_frlen_bytes(target_abi: Option<&str>) -> u64 {
    match target_abi.unwrap_or("lp64d") {
        "lp64s" => 0,
        "lp64f" => 4,
        "lp64d" => 8,
        abi => panic!("unsupported LoongArch ABI reached C ABI lowering: {abi}"),
    }
}

fn classify_param_loongarch64<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    ty: BasicTypeEnum<'ctx>,
    frlen_bytes: u64,
    gars_left: &mut usize,
    fars_left: &mut usize,
) -> ParamLowering<'ctx> {
    let size = td.get_store_size(&ty);
    let is_aggregate = matches!(
        ty,
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)
    );
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
        return ParamLowering::Indirect {
            ty: ty.as_any_type_enum(),
        };
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

fn classify_ret_loongarch64<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    ty: Option<BasicTypeEnum<'ctx>>,
    frlen_bytes: u64,
) -> RetLowering<'ctx> {
    let Some(ty) = ty else {
        return RetLowering::Void;
    };
    let size = td.get_store_size(&ty);
    let is_aggregate = matches!(
        ty,
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)
    );
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
                    .struct_type(
                        &fields.iter().map(|field| field.ty).collect::<Vec<_>>(),
                        false,
                    )
                    .as_basic_type_enum(),
            );
        }
    }
    if is_aggregate && size > 16 {
        return RetLowering::SRet {
            ty: ty.as_any_type_enum(),
            align: td.get_abi_alignment(&ty),
        };
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
fn wasm_single_leaf<'ctx>(t: BasicTypeEnum<'ctx>) -> Option<BasicTypeEnum<'ctx>> {
    let mut leaves = Vec::new();
    flatten_leaf_types(t, &mut leaves);
    (leaves.len() == 1).then_some(leaves[0])
}

fn classify_param_wasm<'ctx>(td: &TargetData, t: BasicTypeEnum<'ctx>) -> ParamLowering<'ctx> {
    let is_aggregate = matches!(
        t,
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)
    );
    if !is_aggregate {
        return ParamLowering::Direct(t);
    }
    if td.get_store_size(&t) == 0 {
        return ParamLowering::Ignore;
    }
    if let Some(leaf) = wasm_single_leaf(t) {
        return ParamLowering::Direct(leaf);
    }
    ParamLowering::ByVal {
        ty: t.as_any_type_enum(),
        align: td.get_abi_alignment(&t),
    }
}

fn classify_ret_wasm<'ctx>(td: &TargetData, t: Option<BasicTypeEnum<'ctx>>) -> RetLowering<'ctx> {
    let Some(t) = t else {
        return RetLowering::Void;
    };
    let is_aggregate = matches!(
        t,
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)
    );
    if !is_aggregate {
        return RetLowering::Direct(t);
    }
    if td.get_store_size(&t) == 0 {
        return RetLowering::Void;
    }
    if let Some(leaf) = wasm_single_leaf(t) {
        return RetLowering::Direct(leaf);
    }
    RetLowering::SRet {
        ty: t.as_any_type_enum(),
        align: td.get_abi_alignment(&t),
    }
}

fn classify_param<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    target: CodegenTarget,
    t: BasicTypeEnum<'ctx>,
) -> ParamLowering<'ctx> {
    match target {
        CodegenTarget::LinuxX86_64
        | CodegenTarget::DarwinX86_64
        | CodegenTarget::FreeBsdX86_64
        | CodegenTarget::FreestandingX86_64 => classify_param_x86_64_sysv(context, td, t),
        CodegenTarget::WindowsX86_64Gnu => classify_param_x86_64_windows(context, td, t),
        CodegenTarget::LinuxArm64
        | CodegenTarget::DarwinArm64
        | CodegenTarget::WindowsArm64Gnu
        | CodegenTarget::FreestandingArm64 => classify_param_arm64(context, td, t),
        CodegenTarget::LinuxRISCV64 | CodegenTarget::FreestandingRISCV64 => {
            classify_param_riscv64(context, td, t)
        }
        // LoongArch needs stateful GAR/FAR accounting and is classified in
        // `lower_extern_c` instead.
        CodegenTarget::LinuxLoongArch64 => unreachable!("stateful LoongArch classifier"),
        CodegenTarget::Wasm32Unknown
        | CodegenTarget::Wasm32WasiP1
        | CodegenTarget::Wasm64Unknown => classify_param_wasm(td, t),
    }
}

fn classify_ret<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    target: CodegenTarget,
    t: Option<BasicTypeEnum<'ctx>>,
    target_abi: Option<&str>,
) -> RetLowering<'ctx> {
    match target {
        CodegenTarget::LinuxX86_64
        | CodegenTarget::DarwinX86_64
        | CodegenTarget::FreeBsdX86_64
        | CodegenTarget::FreestandingX86_64 => classify_ret_x86_64_sysv(context, td, t),
        CodegenTarget::WindowsX86_64Gnu => classify_ret_x86_64_windows(context, td, t),
        CodegenTarget::LinuxArm64
        | CodegenTarget::DarwinArm64
        | CodegenTarget::WindowsArm64Gnu
        | CodegenTarget::FreestandingArm64 => classify_ret_arm64(context, td, t),
        CodegenTarget::LinuxRISCV64 | CodegenTarget::FreestandingRISCV64 => {
            classify_ret_riscv64(context, td, t)
        }
        CodegenTarget::LinuxLoongArch64 => {
            classify_ret_loongarch64(context, td, t, loongarch_frlen_bytes(target_abi))
        }
        CodegenTarget::Wasm32Unknown
        | CodegenTarget::Wasm32WasiP1
        | CodegenTarget::Wasm64Unknown => classify_ret_wasm(td, t),
    }
}

pub fn lower_extern_c<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    target: CodegenTarget,
    target_abi: Option<&str>,
    ext: &ExternFunctionNode,
    struct_types: &HashMap<String, inkwell::types::StructType<'ctx>>,
) -> LoweredExtern<'ctx> {
    let llvm_name = ext
        .symbol
        .as_deref()
        .unwrap_or(ext.name.as_str())
        .to_string();
    let info_llvm_name = llvm_name.clone();

    // wave types -> layout types
    let wave_param_layout: Vec<BasicTypeEnum<'ctx>> = ext
        .params
        .iter()
        .map(|(_, ty)| wave_type_to_llvm_type(context, ty, struct_types, TypeFlavor::AbiC))
        .collect();

    let wave_ret_layout: Option<BasicTypeEnum<'ctx>> = match &ext.return_type {
        WaveType::Void | WaveType::Never => None,
        ty => Some(wave_type_to_llvm_type(
            context,
            ty,
            struct_types,
            TypeFlavor::AbiC,
        )),
    };

    let ret = classify_ret(context, td, target, wave_ret_layout, target_abi);
    let ret_extension = integer_extension_for_target(target, &ext.return_type);
    let mut params: Vec<ParamLowering<'ctx>> = vec![];
    if target == CodegenTarget::LinuxLoongArch64 {
        let frlen_bytes = loongarch_frlen_bytes(target_abi);
        let mut gars_left = if matches!(ret, RetLowering::SRet { .. }) {
            7
        } else {
            8
        };
        let mut fars_left = if frlen_bytes == 0 { 0 } else { 8 };
        for param in wave_param_layout {
            params.push(classify_param_loongarch64(
                context,
                td,
                param,
                frlen_bytes,
                &mut gars_left,
                &mut fars_left,
            ));
        }
    } else {
        for param in wave_param_layout {
            params.push(classify_param(context, td, target, param));
        }
    }
    let param_extensions = ext
        .params
        .iter()
        .map(|(_, ty)| integer_extension_for_target(target, ty))
        .collect();

    // build lowered param list (sret first, then params possibly split)
    let mut llvm_param_types: Vec<BasicMetadataTypeEnum<'ctx>> = vec![];

    if let RetLowering::SRet { ty, .. } = &ret {
        // sret param is ptr to the return aggregate
        let ptr = any_ptr_basic(context, ty.clone());
        llvm_param_types.push(ptr.into());
    }

    for p in &params {
        match p {
            ParamLowering::Ignore => {}
            ParamLowering::Direct(t) => llvm_param_types.push((*t).into()),
            ParamLowering::Split(parts) => {
                for pt in parts {
                    llvm_param_types.push((*pt).into());
                }
            }
            ParamLowering::CoerceAndExpand(parts) => {
                for part in parts {
                    llvm_param_types.push(part.ty.into());
                }
            }
            ParamLowering::Indirect { ty } | ParamLowering::ByVal { ty, .. } => {
                let ptr = any_ptr_basic(context, ty.clone());
                llvm_param_types.push(ptr.into());
            }
        }
    }

    let fn_type = match &ret {
        RetLowering::Void | RetLowering::SRet { .. } => {
            context.void_type().fn_type(&llvm_param_types, ext.variadic)
        }
        RetLowering::Direct(t) => t.fn_type(&llvm_param_types, ext.variadic),
    };

    LoweredExtern {
        llvm_name,
        fn_type,
        info: ExternCInfo {
            llvm_name: info_llvm_name,
            wave_ret: ext.return_type.clone(),
            ret,
            ret_extension,
            params,
            param_extensions,
            llvm_param_types,
            variadic: ext.variadic,
            variadic_integer_extension: matches!(
                target,
                CodegenTarget::LinuxRISCV64
                    | CodegenTarget::FreestandingRISCV64
                    | CodegenTarget::LinuxLoongArch64
            )
            .then_some(IntegerExtension::Sign),
        },
    }
}

fn integer_extension_attr<'ctx>(context: &'ctx Context, extension: IntegerExtension) -> Attribute {
    let name = match extension {
        IntegerExtension::Sign => "signext",
        IntegerExtension::Zero => "zeroext",
    };
    context.create_enum_attribute(Attribute::get_named_enum_kind_id(name), 0)
}

pub fn apply_extern_c_attrs<'ctx>(
    context: &'ctx Context,
    f: FunctionValue<'ctx>,
    info: &ExternCInfo<'ctx>,
) {
    if info.wave_ret == WaveType::Never {
        f.add_attribute(
            AttributeLoc::Function,
            context.create_enum_attribute(Attribute::get_named_enum_kind_id("noreturn"), 0),
        );
    }

    let mut llvm_param_index: u32 = 0;

    if let Some(extension) = info.ret_extension {
        f.add_attribute(
            AttributeLoc::Return,
            integer_extension_attr(context, extension),
        );
    }

    // sret first param
    if let RetLowering::SRet { ty, align } = &info.ret {
        let sret_kind = Attribute::get_named_enum_kind_id("sret");
        let sret_attr = context.create_type_attribute(sret_kind, *ty);
        f.add_attribute(AttributeLoc::Param(0), sret_attr);

        let align_kind = Attribute::get_named_enum_kind_id("align");
        let align_attr = context.create_enum_attribute(align_kind, *align as u64);
        f.add_attribute(AttributeLoc::Param(0), align_attr);

        llvm_param_index += 1;
    }

    for (p, extension) in info.params.iter().zip(info.param_extensions.iter()) {
        match p {
            ParamLowering::Ignore => {}
            ParamLowering::Direct(_) => {
                if let Some(extension) = extension {
                    f.add_attribute(
                        AttributeLoc::Param(llvm_param_index),
                        integer_extension_attr(context, *extension),
                    );
                }
                llvm_param_index += 1;
            }
            ParamLowering::Split(parts) => {
                llvm_param_index += parts.len() as u32;
            }
            ParamLowering::CoerceAndExpand(parts) => {
                llvm_param_index += parts.len() as u32;
            }
            ParamLowering::Indirect { .. } => {
                llvm_param_index += 1;
            }
            ParamLowering::ByVal { ty, align } => {
                let byval_kind = Attribute::get_named_enum_kind_id("byval");
                let byval_attr = context.create_type_attribute(byval_kind, *ty);
                f.add_attribute(AttributeLoc::Param(llvm_param_index), byval_attr);

                let align_kind = Attribute::get_named_enum_kind_id("align");
                let align_attr = context.create_enum_attribute(align_kind, *align as u64);
                f.add_attribute(AttributeLoc::Param(llvm_param_index), align_attr);

                llvm_param_index += 1;
            }
        }
    }
}

pub fn apply_extern_c_callsite_attrs<'ctx>(
    context: &'ctx Context,
    call: CallSiteValue<'ctx>,
    info: &ExternCInfo<'ctx>,
) {
    let mut llvm_param_index: u32 = 0;

    if let Some(extension) = info.ret_extension {
        call.add_attribute(
            AttributeLoc::Return,
            integer_extension_attr(context, extension),
        );
    }

    if let RetLowering::SRet { ty, align } = &info.ret {
        let sret_kind = Attribute::get_named_enum_kind_id("sret");
        call.add_attribute(
            AttributeLoc::Param(0),
            context.create_type_attribute(sret_kind, *ty),
        );

        let align_kind = Attribute::get_named_enum_kind_id("align");
        call.add_attribute(
            AttributeLoc::Param(0),
            context.create_enum_attribute(align_kind, *align as u64),
        );

        llvm_param_index += 1;
    }

    for (p, extension) in info.params.iter().zip(info.param_extensions.iter()) {
        match p {
            ParamLowering::Ignore => {}
            ParamLowering::Direct(_) => {
                if let Some(extension) = extension {
                    call.add_attribute(
                        AttributeLoc::Param(llvm_param_index),
                        integer_extension_attr(context, *extension),
                    );
                }
                llvm_param_index += 1;
            }
            ParamLowering::Split(parts) => {
                llvm_param_index += parts.len() as u32;
            }
            ParamLowering::CoerceAndExpand(parts) => {
                llvm_param_index += parts.len() as u32;
            }
            ParamLowering::Indirect { .. } => {
                llvm_param_index += 1;
            }
            ParamLowering::ByVal { ty, align } => {
                let byval_kind = Attribute::get_named_enum_kind_id("byval");
                call.add_attribute(
                    AttributeLoc::Param(llvm_param_index),
                    context.create_type_attribute(byval_kind, *ty),
                );

                let align_kind = Attribute::get_named_enum_kind_id("align");
                call.add_attribute(
                    AttributeLoc::Param(llvm_param_index),
                    context.create_enum_attribute(align_kind, *align as u64),
                );

                llvm_param_index += 1;
            }
        }
    }
}

pub fn apply_extern_c_variadic_callsite_attrs<'ctx>(
    context: &'ctx Context,
    call: CallSiteValue<'ctx>,
    info: &ExternCInfo<'ctx>,
    arguments: &[BasicValueEnum<'ctx>],
) {
    let Some(extension) = info.variadic_integer_extension else {
        return;
    };
    let first_index = info.llvm_param_types.len() as u32;
    for (index, argument) in arguments.iter().enumerate() {
        if matches!(argument, BasicValueEnum::IntValue(value) if value.get_type().get_bit_width() == 32)
        {
            call.add_attribute(
                AttributeLoc::Param(first_index + index as u32),
                integer_extension_attr(context, extension),
            );
        }
    }
}
