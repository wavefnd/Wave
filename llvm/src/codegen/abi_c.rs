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
mod aarch64;
mod attributes;
mod loongarch;
mod riscv;
mod shared;
mod sysv;
mod wasm;
mod windows;
use aarch64::{classify_param_arm64, classify_ret_arm64};
pub use attributes::{
    apply_extern_c_attrs, apply_extern_c_callsite_attrs, apply_extern_c_variadic_callsite_attrs,
};
use loongarch::{classify_param_loongarch64, classify_ret_loongarch64, loongarch_frlen_bytes};
use riscv::{classify_param_riscv64, classify_ret_riscv64};
use shared::any_ptr_basic;
pub use shared::{
    AbiPart, ExternCInfo, IntegerExtension, LoweredExtern, ParamLowering, RetLowering,
};
use sysv::{
    classify_param_sysv_with_registers, classify_param_x86_64_sysv, classify_ret_x86_64_sysv,
};
use wasm::{classify_param_wasm, classify_ret_wasm};
use windows::{classify_param_x86_64_windows, classify_ret_x86_64_windows};

use inkwell::context::Context;
use inkwell::targets::TargetData;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use std::collections::HashMap;

use parser::ast::{ExternFunctionNode, WaveType};

use super::target::CodegenTarget;
use super::types::{wave_type_to_llvm_type, TypeFlavor};

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
        CodegenTarget::FreeBsdRISCV64
        | CodegenTarget::LinuxRISCV64
        | CodegenTarget::FreestandingRISCV64
        | CodegenTarget::LinuxLoongArch64 => match ty {
            WaveType::Int(bits) if *bits <= 32 => Some(IntegerExtension::Sign),
            WaveType::Uint(bits) if *bits < 32 => Some(IntegerExtension::Zero),
            // RV64 widens u32 to 32 bits and then sign-extends it to XLEN.
            WaveType::Uint(32) => Some(IntegerExtension::Sign),
            WaveType::Bool | WaveType::Byte | WaveType::Char => Some(IntegerExtension::Zero),
            _ => None,
        },
        CodegenTarget::FreeBsdArm64
        | CodegenTarget::LinuxArm64
        | CodegenTarget::FreestandingArm64
        | CodegenTarget::WindowsX86_64Msvc
        | CodegenTarget::WindowsArm64Msvc => None,
        CodegenTarget::Wasm32Unknown
        | CodegenTarget::Wasm32WasiP1
        | CodegenTarget::Wasm64Unknown => narrow_extension(),
    }
}

fn riscv_flen(target: CodegenTarget, abi: Option<&str>) -> u64 {
    match abi.unwrap_or(if target == CodegenTarget::FreestandingRISCV64 { "lp64" } else { "lp64d" })
    {
        "lp64" => 0,
        "lp64f" => 4,
        "lp64d" => 8,
        abi => panic!("invalid RISC-V ABI reached C lowering: {abi}"),
    }
}

fn classify_param<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    target: CodegenTarget,
    t: BasicTypeEnum<'ctx>,
    variadic: bool,
) -> ParamLowering<'ctx> {
    match target {
        CodegenTarget::LinuxX86_64
        | CodegenTarget::DarwinX86_64
        | CodegenTarget::FreeBsdX86_64
        | CodegenTarget::FreestandingX86_64 => classify_param_x86_64_sysv(context, td, t),
        CodegenTarget::WindowsX86_64Msvc => classify_param_x86_64_windows(context, td, t),
        CodegenTarget::FreeBsdArm64
        | CodegenTarget::LinuxArm64
        | CodegenTarget::DarwinArm64
        | CodegenTarget::WindowsArm64Msvc
        | CodegenTarget::FreestandingArm64 => classify_param_arm64(
            context,
            td,
            t,
            // Windows variadic functions treat even named HFAs as ordinary
            // composites. Linux/Darwin retain HFA treatment for named args.
            !variadic || !matches!(target, CodegenTarget::WindowsArm64Msvc),
        ),
        CodegenTarget::FreeBsdRISCV64
        | CodegenTarget::LinuxRISCV64
        | CodegenTarget::FreestandingRISCV64 => unreachable!("stateful RISC-V classifier"),
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
        CodegenTarget::WindowsX86_64Msvc => classify_ret_x86_64_windows(context, td, t),
        CodegenTarget::FreeBsdArm64
        | CodegenTarget::LinuxArm64
        | CodegenTarget::DarwinArm64
        | CodegenTarget::WindowsArm64Msvc
        | CodegenTarget::FreestandingArm64 => classify_ret_arm64(context, td, t),
        CodegenTarget::FreeBsdRISCV64
        | CodegenTarget::LinuxRISCV64
        | CodegenTarget::FreestandingRISCV64 => {
            classify_ret_riscv64(context, td, t, riscv_flen(target, target_abi))
        },
        CodegenTarget::LinuxLoongArch64 => {
            classify_ret_loongarch64(context, td, t, loongarch_frlen_bytes(target_abi))
        },
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
    let llvm_name = ext.symbol.as_deref().unwrap_or(ext.name.as_str()).to_string();
    let info_llvm_name = llvm_name.clone();

    // wave types -> layout types
    let wave_param_layout: Vec<BasicTypeEnum<'ctx>> = ext
        .params
        .iter()
        .map(|(_, ty)| wave_type_to_llvm_type(context, ty, struct_types, TypeFlavor::AbiC))
        .collect();

    let wave_ret_layout: Option<BasicTypeEnum<'ctx>> = match &ext.return_type {
        WaveType::Void | WaveType::Never => None,
        ty => Some(wave_type_to_llvm_type(context, ty, struct_types, TypeFlavor::AbiC)),
    };

    let ret = classify_ret(context, td, target, wave_ret_layout, target_abi);
    let ret_extension = integer_extension_for_target(target, &ext.return_type);
    let mut params: Vec<ParamLowering<'ctx>> = vec![];
    if matches!(
        target,
        CodegenTarget::LinuxRISCV64
            | CodegenTarget::FreeBsdRISCV64
            | CodegenTarget::FreestandingRISCV64
    ) {
        let flen = riscv_flen(target, target_abi);
        let mut gp_left = if matches!(ret, RetLowering::SRet { .. }) { 7 } else { 8 };
        let mut fp_left = if flen == 0 { 0 } else { 8 };
        for param in wave_param_layout {
            params.push(classify_param_riscv64(
                context,
                td,
                param,
                flen,
                &mut gp_left,
                &mut fp_left,
            ));
        }
    } else if target == CodegenTarget::LinuxLoongArch64 {
        let frlen_bytes = loongarch_frlen_bytes(target_abi);
        let mut gars_left = if matches!(ret, RetLowering::SRet { .. }) { 7 } else { 8 };
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
    } else if matches!(
        target,
        CodegenTarget::LinuxX86_64
            | CodegenTarget::DarwinX86_64
            | CodegenTarget::FreeBsdX86_64
            | CodegenTarget::FreestandingX86_64
    ) {
        let mut gp_left = if matches!(ret, RetLowering::SRet { .. }) { 5 } else { 6 };
        let mut sse_left = 8;
        for param in wave_param_layout {
            params.push(classify_param_sysv_with_registers(
                context,
                td,
                param,
                &mut gp_left,
                &mut sse_left,
            ));
        }
    } else {
        for param in wave_param_layout {
            params.push(classify_param(context, td, target, param, ext.variadic));
        }
    }
    let param_extensions =
        ext.params.iter().map(|(_, ty)| integer_extension_for_target(target, ty)).collect();

    // build lowered param list (sret first, then params possibly split)
    let mut llvm_param_types: Vec<BasicMetadataTypeEnum<'ctx>> = vec![];

    if let RetLowering::SRet { ty, .. } = &ret {
        // sret param is ptr to the return aggregate
        let ptr = any_ptr_basic(context, ty.clone());
        llvm_param_types.push(ptr.into());
    }

    for p in &params {
        match p {
            ParamLowering::Ignore => {},
            ParamLowering::Direct(t) => llvm_param_types.push((*t).into()),
            ParamLowering::Split(parts) => {
                for pt in parts {
                    llvm_param_types.push((*pt).into());
                }
            },
            ParamLowering::CoerceAndExpand(parts) => {
                for part in parts {
                    llvm_param_types.push(part.ty.into());
                }
            },
            ParamLowering::Indirect { ty } | ParamLowering::ByVal { ty, .. } => {
                let ptr = any_ptr_basic(context, ty.clone());
                llvm_param_types.push(ptr.into());
            },
        }
    }

    let fn_type = match &ret {
        RetLowering::Void | RetLowering::SRet { .. } => {
            context.void_type().fn_type(&llvm_param_types, ext.variadic)
        },
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
                CodegenTarget::FreeBsdRISCV64
                    | CodegenTarget::LinuxRISCV64
                    | CodegenTarget::FreestandingRISCV64
                    | CodegenTarget::LinuxLoongArch64
            )
            .then_some(IntegerExtension::Sign),
        },
    }
}
