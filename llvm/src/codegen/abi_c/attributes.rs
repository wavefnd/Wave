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

//! Apply already-classified ABI contracts to functions and calls.
use super::shared::*;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::context::Context;
use inkwell::values::{BasicValueEnum, CallSiteValue, FunctionValue};
use parser::ast::WaveType;

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
        f.add_attribute(AttributeLoc::Return, integer_extension_attr(context, extension));
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
            ParamLowering::Ignore => {},
            ParamLowering::Direct(_) => {
                if let Some(extension) = extension {
                    f.add_attribute(
                        AttributeLoc::Param(llvm_param_index),
                        integer_extension_attr(context, *extension),
                    );
                }
                llvm_param_index += 1;
            },
            ParamLowering::Split(parts) => {
                llvm_param_index += parts.len() as u32;
            },
            ParamLowering::CoerceAndExpand(parts) => {
                llvm_param_index += parts.len() as u32;
            },
            ParamLowering::Indirect { .. } => {
                llvm_param_index += 1;
            },
            ParamLowering::ByVal { ty, align } => {
                let byval_kind = Attribute::get_named_enum_kind_id("byval");
                let byval_attr = context.create_type_attribute(byval_kind, *ty);
                f.add_attribute(AttributeLoc::Param(llvm_param_index), byval_attr);

                let align_kind = Attribute::get_named_enum_kind_id("align");
                let align_attr = context.create_enum_attribute(align_kind, *align as u64);
                f.add_attribute(AttributeLoc::Param(llvm_param_index), align_attr);

                llvm_param_index += 1;
            },
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
        call.add_attribute(AttributeLoc::Return, integer_extension_attr(context, extension));
    }

    if let RetLowering::SRet { ty, align } = &info.ret {
        let sret_kind = Attribute::get_named_enum_kind_id("sret");
        call.add_attribute(AttributeLoc::Param(0), context.create_type_attribute(sret_kind, *ty));

        let align_kind = Attribute::get_named_enum_kind_id("align");
        call.add_attribute(
            AttributeLoc::Param(0),
            context.create_enum_attribute(align_kind, *align as u64),
        );

        llvm_param_index += 1;
    }

    for (p, extension) in info.params.iter().zip(info.param_extensions.iter()) {
        match p {
            ParamLowering::Ignore => {},
            ParamLowering::Direct(_) => {
                if let Some(extension) = extension {
                    call.add_attribute(
                        AttributeLoc::Param(llvm_param_index),
                        integer_extension_attr(context, *extension),
                    );
                }
                llvm_param_index += 1;
            },
            ParamLowering::Split(parts) => {
                llvm_param_index += parts.len() as u32;
            },
            ParamLowering::CoerceAndExpand(parts) => {
                llvm_param_index += parts.len() as u32;
            },
            ParamLowering::Indirect { .. } => {
                llvm_param_index += 1;
            },
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
            },
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
