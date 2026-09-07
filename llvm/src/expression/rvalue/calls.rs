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

//! Function and method call lowering across Wave and C ABI boundaries.
//!
//! Ordinary Wave calls use source parameter types directly. `extern(c)` calls
//! instead apply the classifier's direct, split, indirect, or `sret` transport
//! plan and attach call-site attributes. Variadic arguments use semantic
//! expression types for C default promotions.

use super::ExprGenEnv;
use crate::codegen::abi_c::{
    apply_extern_c_callsite_attrs, apply_extern_c_variadic_callsite_attrs, AbiPart, ParamLowering,
    RetLowering,
};
use crate::statement::variable::{coerce_basic_value, wave_type_is_unsigned, CoercionMode};
use inkwell::types::{AnyTypeEnum, AsTypeRef, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue, ValueKind,
};
use parser::ast::{Expression, WaveType};

fn meta_to_basic<'ctx>(m: BasicMetadataTypeEnum<'ctx>) -> BasicTypeEnum<'ctx> {
    match m {
        BasicMetadataTypeEnum::ArrayType(t) => t.as_basic_type_enum(),
        BasicMetadataTypeEnum::FloatType(t) => t.as_basic_type_enum(),
        BasicMetadataTypeEnum::IntType(t) => t.as_basic_type_enum(),
        BasicMetadataTypeEnum::PointerType(t) => t.as_basic_type_enum(),
        BasicMetadataTypeEnum::StructType(t) => t.as_basic_type_enum(),
        BasicMetadataTypeEnum::VectorType(t) => t.as_basic_type_enum(),
        BasicMetadataTypeEnum::ScalableVectorType(t) => t.as_basic_type_enum(),
        other => panic!("Unsupported metadata param type: {:?}", other),
    }
}

fn opt_meta_to_opt_basic<'ctx>(
    m: Option<BasicMetadataTypeEnum<'ctx>>,
) -> Option<BasicTypeEnum<'ctx>> {
    m.map(meta_to_basic)
}

fn meta_into_ptr<'ctx>(m: BasicMetadataTypeEnum<'ctx>) -> inkwell::types::PointerType<'ctx> {
    match m {
        BasicMetadataTypeEnum::PointerType(p) => p,
        other => panic!("Expected pointer param type, got {:?}", other),
    }
}

fn callsite_to_ret<'ctx>(
    call_site: inkwell::values::CallSiteValue<'ctx>,
    expect_ret: bool,
    what: &str,
) -> Option<BasicValueEnum<'ctx>> {
    match call_site.try_as_basic_value() {
        ValueKind::Basic(v) => Some(v),
        ValueKind::Instruction(_inst) => {
            if expect_ret {
                panic!("Expected a return value from {}", what);
            }
            None
        }
    }
}

fn pack_agg_to_int<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    agg: BasicValueEnum<'ctx>,
    dst: inkwell::types::IntType<'ctx>,
    tag: &str,
) -> BasicValueEnum<'ctx> {
    let agg_ty = agg.get_type();

    let agg_tmp = env
        .builder
        .build_alloca(agg_ty, &format!("{}_agg_tmp", tag))
        .unwrap();
    env.builder.build_store(agg_tmp, agg).unwrap();

    let int_tmp = env
        .builder
        .build_alloca(dst, &format!("{}_int_tmp", tag))
        .unwrap();

    // The ABI transport slot may be wider than the aggregate object (for
    // example a 3-byte RV64 aggregate transported in i64). Keep padding bits
    // deterministic instead of loading uninitialized stack bytes.
    env.builder.build_store(int_tmp, dst.const_zero()).unwrap();

    let bytes = env.target_data.get_store_size(&agg_ty) as u64;
    let size_v = env.context.i64_type().const_int(bytes, false);

    env.builder
        .build_memcpy(int_tmp, 1, agg_tmp, 1, size_v)
        .unwrap();

    env.builder
        .build_load(dst, int_tmp, &format!("{}_agg_i", tag))
        .unwrap()
        .as_basic_value_enum()
}

fn unpack_int_to_agg<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    iv: inkwell::values::IntValue<'ctx>,
    dst_agg_ty: BasicTypeEnum<'ctx>,
    tag: &str,
) -> BasicValueEnum<'ctx> {
    let agg_tmp = env
        .builder
        .build_alloca(dst_agg_ty, &format!("{}_agg_tmp", tag))
        .unwrap();
    let int_tmp = env
        .builder
        .build_alloca(iv.get_type(), &format!("{}_int_tmp", tag))
        .unwrap();

    env.builder.build_store(int_tmp, iv).unwrap();

    let bytes = env.target_data.get_store_size(&dst_agg_ty) as u64;
    let size_v = env.context.i64_type().const_int(bytes, false);

    env.builder
        .build_memcpy(agg_tmp, 1, int_tmp, 1, size_v)
        .unwrap();

    env.builder
        .build_load(dst_agg_ty, agg_tmp, &format!("{}_i2agg_load", tag))
        .unwrap()
        .as_basic_value_enum()
}

fn normalize_struct_name(raw: &str) -> &str {
    raw.strip_prefix("struct.")
        .unwrap_or(raw)
        .trim_start_matches('%')
}

fn resolve_struct_key<'ctx>(
    st: inkwell::types::StructType<'ctx>,
    struct_types: &std::collections::HashMap<String, inkwell::types::StructType<'ctx>>,
) -> String {
    if let Some(raw) = st.get_name().and_then(|n| n.to_str().ok()) {
        return normalize_struct_name(raw).to_string();
    }

    let st_ref = st.as_type_ref();
    for (name, ty) in struct_types {
        if ty.as_type_ref() == st_ref {
            return name.clone();
        }
    }

    panic!("LLVM struct type has no name and cannot be matched to struct_types");
}

fn lower_c_variadic_argument<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    expression: &Expression,
    index: usize,
) -> BasicValueEnum<'ctx> {
    if matches!(expression, Expression::Null) {
        panic!("untyped null reached C variadic codegen after semantic validation");
    }
    let semantic_type = env.wave_type(expression);
    let value = env.gen(expression, None);
    match value {
        BasicValueEnum::FloatValue(float) if float.get_type() == env.context.f32_type() => env
            .builder
            .build_float_ext(
                float,
                env.context.f64_type(),
                &format!("vararg{}_f64", index),
            )
            .unwrap()
            .as_basic_value_enum(),
        BasicValueEnum::IntValue(integer) if integer.get_type().get_bit_width() < 32 => {
            let target = env.context.i32_type();
            let signed = matches!(semantic_type, Some(WaveType::Int(_)));
            if signed {
                env.builder
                    .build_int_s_extend(integer, target, &format!("vararg{}_sext", index))
                    .unwrap()
                    .as_basic_value_enum()
            } else {
                env.builder
                    .build_int_z_extend(integer, target, &format!("vararg{}_zext", index))
                    .unwrap()
                    .as_basic_value_enum()
            }
        }
        BasicValueEnum::IntValue(_)
        | BasicValueEnum::FloatValue(_)
        | BasicValueEnum::PointerValue(_) => value,
        _ => panic!("C variadic argument {} must be a scalar value", index + 1),
    }
}

fn infer_struct_name_for_method<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    object: &Expression,
    obj_preview: BasicValueEnum<'ctx>,
) -> Option<String> {
    match obj_preview.get_type() {
        BasicTypeEnum::StructType(st) => return Some(resolve_struct_key(st, env.struct_types)),
        _ => {}
    }

    let wt = env.wave_type(object)?;
    match wt {
        WaveType::Struct(name) => Some(name),
        WaveType::Pointer(inner) => match *inner {
            WaveType::Struct(name) => Some(name),
            _ => None,
        },
        _ => None,
    }
}

pub(crate) fn gen_method_call<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    object: &Expression,
    name: &str,
    args: &[Expression],
) -> BasicValueEnum<'ctx> {
    // struct method sugar: obj.method(...)
    if let Expression::Variable(var_name) = object {
        if let Some(var_info) = env.variables.get(var_name) {
            if let WaveType::Struct(struct_name) = &var_info.ty {
                let fn_name = format!("{}_{}", struct_name, name);

                let function = env
                    .module
                    .get_function(&fn_name)
                    .unwrap_or_else(|| panic!("Function '{}' not found", fn_name));

                let fn_type = function.get_type();
                let param_types = fn_type.get_param_types();
                let expected_self = opt_meta_to_opt_basic(param_types.get(0).cloned());

                let obj_val = env.gen(object, expected_self);

                let mut call_args: Vec<BasicMetadataValueEnum> = Vec::new();
                call_args.push(obj_val.into());

                for (i, arg_expr) in args.iter().enumerate() {
                    let expected_ty = opt_meta_to_opt_basic(param_types.get(i + 1).cloned());
                    let mut arg_val = env.gen(arg_expr, expected_ty);
                    if let Some(et) = expected_ty {
                        arg_val = coerce_basic_value(
                            env.context,
                            env.builder,
                            arg_val,
                            et,
                            &format!("arg{}_cast", i),
                            CoercionMode::Implicit,
                            wave_type_is_unsigned(env.wave_type(arg_expr).as_ref()),
                        );
                    }
                    call_args.push(arg_val.into());
                }

                let call_site = env
                    .builder
                    .build_call(function, &call_args, &format!("call_{}", fn_name))
                    .unwrap();

                if function.get_type().get_return_type().is_some() {
                    return callsite_to_ret(call_site, true, "struct method").unwrap();
                } else {
                    return env.context.i32_type().const_zero().as_basic_value_enum();
                }
            }
        }
    }

    // Attempt "Struct_Method" dispatch by looking at object type (WaveType or LLVM struct value)
    {
        let obj_preview = env.gen(object, None);

        if let Some(struct_name) = infer_struct_name_for_method(env, object, obj_preview) {
            let fn_name = format!("{}_{}", struct_name, name);

            if let Some(function) = env.module.get_function(&fn_name) {
                let fn_type = function.get_type();
                let param_types = fn_type.get_param_types();
                let expected_self = opt_meta_to_opt_basic(param_types.get(0).cloned());

                let mut obj_val = obj_preview;
                if let Some(et) = expected_self {
                    obj_val = coerce_basic_value(
                        env.context,
                        env.builder,
                        obj_val,
                        et,
                        "self_cast",
                        CoercionMode::Implicit,
                        wave_type_is_unsigned(env.wave_type(object).as_ref()),
                    );
                }

                let mut call_args: Vec<BasicMetadataValueEnum> = Vec::new();
                call_args.push(obj_val.into());

                for (i, arg_expr) in args.iter().enumerate() {
                    let expected_ty = opt_meta_to_opt_basic(param_types.get(i + 1).cloned());
                    let mut arg_val = env.gen(arg_expr, expected_ty);
                    if let Some(et) = expected_ty {
                        arg_val = coerce_basic_value(
                            env.context,
                            env.builder,
                            arg_val,
                            et,
                            &format!("arg{}_cast", i),
                            CoercionMode::Implicit,
                            wave_type_is_unsigned(env.wave_type(arg_expr).as_ref()),
                        );
                    }
                    call_args.push(arg_val.into());
                }

                let call_site = env
                    .builder
                    .build_call(function, &call_args, &format!("call_{}", fn_name))
                    .unwrap();

                if function.get_type().get_return_type().is_some() {
                    return callsite_to_ret(call_site, true, "method dispatch").unwrap();
                } else {
                    return env.context.i32_type().const_zero().as_basic_value_enum();
                }
            }
        }
    }

    // method-style call: fn(self, ...)
    let function = env
        .module
        .get_function(name)
        .unwrap_or_else(|| panic!("Function '{}' not found for method-style call", name));

    let fn_type = function.get_type();
    let param_types = fn_type.get_param_types();

    if param_types.is_empty() {
        panic!(
            "Method-style call {}() requires at least 1 parameter (self)",
            name
        );
    }

    let expected_self = opt_meta_to_opt_basic(param_types.get(0).cloned());
    let obj_val = env.gen(object, expected_self);

    let mut call_args: Vec<BasicMetadataValueEnum> = Vec::new();
    call_args.push(obj_val.into());

    for (i, arg_expr) in args.iter().enumerate() {
        let expected_ty = opt_meta_to_opt_basic(param_types.get(i + 1).cloned());
        let mut arg_val = env.gen(arg_expr, expected_ty);
        if let Some(et) = expected_ty {
            arg_val = coerce_basic_value(
                env.context,
                env.builder,
                arg_val,
                et,
                &format!("arg{}_cast", i),
                CoercionMode::Implicit,
                wave_type_is_unsigned(env.wave_type(arg_expr).as_ref()),
            );
        }
        call_args.push(arg_val.into());
    }

    let call_site = env
        .builder
        .build_call(function, &call_args, &format!("call_{}", name))
        .unwrap();

    if function.get_type().get_return_type().is_some() {
        callsite_to_ret(call_site, true, "method-style call").unwrap()
    } else {
        env.context.i32_type().const_zero().as_basic_value_enum()
    }
}

pub(crate) fn gen_function_call<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    name: &str,
    type_args: &[WaveType],
    args: &[Expression],
    expected_type: Option<BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    if !type_args.is_empty() {
        panic!(
            "generic call '{}<...>(...)' reached codegen without monomorphization",
            name
        );
    }

    if let Some(info) = env.extern_c_info.get(name) {
        let function = env.module.get_function(&info.llvm_name).unwrap_or_else(|| {
            panic!(
                "Extern function '{}' not found in module (symbol alias?)",
                name
            )
        });

        if (!info.variadic && args.len() != info.params.len())
            || (info.variadic && args.len() < info.params.len())
        {
            panic!(
                "Extern `{}` expects {}{} arguments (wave-level), got {}",
                name,
                info.params.len(),
                if info.variadic { " or more" } else { "" },
                args.len()
            );
        }

        let fn_type = function.get_type();
        let llvm_param_types = fn_type.get_param_types();

        let mut lowered_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();
        let mut variadic_args: Vec<BasicValueEnum<'ctx>> = Vec::new();
        let mut llvm_pi: usize = 0;

        // 1) sret hidden param
        let mut sret_tmp: Option<PointerValue<'ctx>> = None;
        if let RetLowering::SRet { ty, .. } = &info.ret {
            let agg = any_agg_to_basic(*ty);
            let tmp = env
                .builder
                .build_alloca(agg, &format!("{}_sret_tmp", name))
                .unwrap();

            let expected_ptr = meta_into_ptr(llvm_param_types[0]);
            let tmp2 = coerce_ptr_to(env, tmp, expected_ptr, &format!("{}_sret_ptrcast", name));

            lowered_args.push(tmp2.as_basic_value_enum().into());
            sret_tmp = Some(tmp);
            llvm_pi += 1;
        }

        // 2) wave params
        for (i, (arg_expr, p)) in args.iter().zip(info.params.iter()).enumerate() {
            match p {
                ParamLowering::Ignore => {
                    env.gen(arg_expr, None);
                }
                ParamLowering::Direct(t) => {
                    let source_unsigned = wave_type_is_unsigned(env.wave_type(arg_expr).as_ref());
                    let mut v = env.gen(arg_expr, Some(*t));
                    v = coerce_to_expected(env, v, *t, name, i, source_unsigned);
                    lowered_args.push(v.into());
                    llvm_pi += 1;
                }

                ParamLowering::Indirect { ty } | ParamLowering::ByVal { ty, .. } => {
                    let agg = any_agg_to_basic(*ty);
                    let v = env.gen(arg_expr, Some(agg));
                    let tmp = env
                        .builder
                        .build_alloca(agg, &format!("{}_byval_tmp_{}", name, i))
                        .unwrap();
                    env.builder.build_store(tmp, v).unwrap();

                    let expected_ptr = meta_into_ptr(llvm_param_types[llvm_pi]);
                    let tmp2 = coerce_ptr_to(
                        env,
                        tmp,
                        expected_ptr,
                        &format!("{}_byval_ptrcast_{}", name, i),
                    );
                    lowered_args.push(tmp2.as_basic_value_enum().into());
                    llvm_pi += 1;
                }

                ParamLowering::Split(parts) => {
                    let agg_val = env.gen(arg_expr, None);
                    let split_vals = split_agg_parts_from_agg(
                        env,
                        agg_val,
                        parts,
                        &format!("{}_split_{}", name, i),
                    );

                    for sv in split_vals {
                        let et = meta_to_basic(llvm_param_types[llvm_pi]);
                        let vv = coerce_basic_value(
                            env.context,
                            env.builder,
                            sv,
                            et,
                            "split_cast",
                            CoercionMode::Implicit,
                            true,
                        );
                        lowered_args.push(vv.into());
                        llvm_pi += 1;
                    }
                }
                ParamLowering::CoerceAndExpand(parts) => {
                    let aggregate = env.gen(arg_expr, None);
                    for value in coerce_and_expand_agg_parts(
                        env,
                        aggregate,
                        parts,
                        &format!("{}_loong_expand_{}", name, i),
                    ) {
                        lowered_args.push(value.into());
                        llvm_pi += 1;
                    }
                }
            }
        }

        for (index, expression) in args.iter().enumerate().skip(info.params.len()) {
            let value = lower_c_variadic_argument(env, expression, index);
            lowered_args.push(value.into());
            variadic_args.push(value);
        }

        let call_name = match info.ret {
            RetLowering::Void | RetLowering::SRet { .. } => String::new(),
            _ => format!("call_{}", name),
        };

        let call_site = env
            .builder
            .build_call(function, &lowered_args, &call_name)
            .unwrap();
        apply_extern_c_callsite_attrs(env.context, call_site, info);
        apply_extern_c_variadic_callsite_attrs(env.context, call_site, info, &variadic_args);

        // 3) return
        match &info.ret {
            RetLowering::Void => {
                if !matches!(info.wave_ret, WaveType::Void | WaveType::Never) {
                    return expected_type.map_or_else(
                        || env.context.i32_type().const_zero().as_basic_value_enum(),
                        BasicTypeEnum::const_zero,
                    );
                }
                if expected_type.is_some() {
                    panic!(
                        "Extern '{}' returns void and cannot be used as a value",
                        name
                    );
                }
                return env.context.i32_type().const_zero().as_basic_value_enum();
            }

            RetLowering::SRet { ty, .. } => {
                let tmp = sret_tmp.expect("SRet lowering requires sret tmp");
                let agg = any_agg_to_basic(*ty);
                let v = env
                    .builder
                    .build_load(agg, tmp, &format!("{}_sret_load", name))
                    .unwrap();

                if let Some(et) = expected_type {
                    return coerce_lowered_ret_to_expected(
                        env,
                        v.as_basic_value_enum(),
                        et,
                        "sret_ret",
                    );
                }
                return v.as_basic_value_enum();
            }

            RetLowering::Direct(_t) => {
                let rv = callsite_to_ret(call_site, true, "extern direct ret").unwrap();

                if let Some(et) = expected_type {
                    return coerce_lowered_ret_to_expected(env, rv, et, "direct_ret");
                }
                return rv;
            }
        }
    }

    let function = env
        .module
        .get_function(name)
        .unwrap_or_else(|| panic!("Function '{}' not found", name));

    let fn_type = function.get_type();
    let param_types: Vec<BasicTypeEnum<'ctx>> = fn_type
        .get_param_types()
        .into_iter()
        .map(meta_to_basic)
        .collect();
    let ret_type: Option<BasicTypeEnum> = fn_type.get_return_type();

    if args.len() != param_types.len() {
        panic!(
            "Function `{}` expects {} arguments, got {}",
            name,
            param_types.len(),
            args.len()
        );
    }

    let mut call_args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());

    for (i, arg) in args.iter().enumerate() {
        let expected_param_ty = param_types[i];
        let source_unsigned = wave_type_is_unsigned(env.wave_type(arg).as_ref());
        let mut val = env.gen(arg, Some(expected_param_ty));
        val = coerce_to_expected(env, val, expected_param_ty, name, i, source_unsigned);
        call_args.push(val.into());
    }

    let call_name = if ret_type.is_some() {
        format!("call_{}", name)
    } else {
        String::new()
    };

    let call_site = env
        .builder
        .build_call(function, &call_args, &call_name)
        .unwrap();

    match ret_type {
        Some(_) => callsite_to_ret(call_site, true, "function call").unwrap(),
        None => {
            if expected_type.is_some() {
                panic!(
                    "Function '{}' returns void and cannot be used as a value",
                    name
                );
            }
            env.context.i32_type().const_zero().as_basic_value_enum()
        }
    }
}

fn coerce_to_expected<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    val: BasicValueEnum<'ctx>,
    expected: BasicTypeEnum<'ctx>,
    name: &str,
    arg_index: usize,
    source_unsigned: bool,
) -> BasicValueEnum<'ctx> {
    let got = val.get_type();
    if got == expected {
        return val;
    }

    match (got, expected) {
        // 0) ptr -> int (ptrtoint)  (needed for syscall wrappers that take i64 registers)
        (BasicTypeEnum::PointerType(_), BasicTypeEnum::IntType(dst))
            if dst.get_bit_width() == 64 && name.starts_with("syscall") =>
        {
            let pv = val.into_pointer_value();
            env.builder
                .build_ptr_to_int(pv, dst, &format!("arg{}_p2i", arg_index))
                .unwrap()
                .as_basic_value_enum()
        }

        // 0.1) int -> ptr (inttoptr) (useful when passing raw addresses)
        (BasicTypeEnum::IntType(src), BasicTypeEnum::PointerType(dst))
            if src.get_bit_width() == 64 && name.starts_with("syscall") =>
        {
            let iv = val.into_int_value();
            env.builder
                .build_int_to_ptr(iv, dst, &format!("arg{}_i2p", arg_index))
                .unwrap()
                .as_basic_value_enum()
        }

        // 1) int -> int
        (BasicTypeEnum::IntType(src), BasicTypeEnum::IntType(dst)) => {
            let src_bw = src.get_bit_width();
            let dst_bw = dst.get_bit_width();
            let iv = val.into_int_value();

            if src_bw < dst_bw {
                if source_unsigned || src_bw == 1 {
                    env.builder
                        .build_int_z_extend(iv, dst, &format!("arg{}_zext", arg_index))
                        .unwrap()
                        .as_basic_value_enum()
                } else {
                    env.builder
                        .build_int_s_extend(iv, dst, &format!("arg{}_sext", arg_index))
                        .unwrap()
                        .as_basic_value_enum()
                }
            } else if src_bw > dst_bw {
                panic!(
                    "implicit integer narrowing is forbidden for arg {} of '{}': i{} -> i{}",
                    arg_index, name, src_bw, dst_bw
                );
            } else {
                iv.as_basic_value_enum()
            }
        }

        // 2) ptr -> array value (load)
        (BasicTypeEnum::PointerType(_), BasicTypeEnum::ArrayType(a)) => {
            let ptr = val.into_pointer_value();
            env.builder
                .build_load(a, ptr, &format!("arg{}_arr_load", arg_index))
                .unwrap()
                .as_basic_value_enum()
        }

        // 3) ptr -> struct value (load)
        (BasicTypeEnum::PointerType(_), BasicTypeEnum::StructType(s)) => {
            let ptr = val.into_pointer_value();
            env.builder
                .build_load(s, ptr, &format!("arg{}_st_load", arg_index))
                .unwrap()
                .as_basic_value_enum()
        }

        // 3.1) a single-pointer aggregate transported as the pointer value
        // itself by x86_64 SysV and AArch64 argument lowering.
        (
            got_agg @ (BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)),
            BasicTypeEnum::PointerType(dst),
        ) => {
            let source = env
                .builder
                .build_alloca(got_agg, &format!("arg{}_ptr_agg", arg_index))
                .unwrap();
            env.builder.build_store(source, val).unwrap();
            env.builder
                .build_load(dst, source, &format!("arg{}_ptr_transport", arg_index))
                .unwrap()
                .as_basic_value_enum()
        }

        // 4) ptr -> ptr (bitcast)
        (BasicTypeEnum::PointerType(_), BasicTypeEnum::PointerType(dst)) => env
            .builder
            .build_bit_cast(val, dst, &format!("arg{}_ptrcast", arg_index))
            .unwrap()
            .as_basic_value_enum(),

        // 4.4) agg(struct/array) -> int (ABI: small structs passed as INTEGER)
        (
            got_agg @ (BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)),
            BasicTypeEnum::IntType(dst),
        ) => {
            let sz = env.target_data.get_store_size(&got_agg) as u64;
            let bits = (sz * 8) as u32;

            if bits <= dst.get_bit_width() {
                return pack_agg_to_int(env, val, dst, &format!("arg{}_pack", arg_index));
            }

            panic!(
                "Cannot pack aggregate to int: agg bits {} > dst bits {} (arg {} of {})",
                bits,
                dst.get_bit_width(),
                arg_index,
                name
            );
        }

        // 4.45) object aggregate -> aggregate-shaped ABI transport slot.
        // AArch64 and RV64 represent 9..16-byte integer aggregates as a
        // two-XLEN array in LLVM IR. Copy only the object bytes and keep the
        // transport padding deterministic.
        (
            got_agg @ (BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)),
            dst_agg @ (BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)),
        ) => {
            let got_size = env.target_data.get_store_size(&got_agg);
            let dst_size = env.target_data.get_store_size(&dst_agg);
            if got_size > dst_size {
                panic!(
                    "Cannot pack aggregate transport: object size {} > slot size {} (arg {} of {})",
                    got_size, dst_size, arg_index, name
                );
            }

            let source = env
                .builder
                .build_alloca(got_agg, &format!("arg{}_agg_source", arg_index))
                .unwrap();
            env.builder.build_store(source, val).unwrap();
            let transport = env
                .builder
                .build_alloca(dst_agg, &format!("arg{}_agg_transport", arg_index))
                .unwrap();
            env.builder
                .build_store(transport, dst_agg.const_zero())
                .unwrap();
            let bytes = env.context.i64_type().const_int(got_size, false);
            env.builder
                .build_memcpy(
                    transport,
                    env.target_data.get_abi_alignment(&dst_agg),
                    source,
                    env.target_data.get_abi_alignment(&got_agg),
                    bytes,
                )
                .unwrap();
            env.builder
                .build_load(
                    dst_agg,
                    transport,
                    &format!("arg{}_agg_transport_load", arg_index),
                )
                .unwrap()
                .as_basic_value_enum()
        }

        // 4.5) agg(struct/array) -> vector (HFA/ABI: e.g. Vector2 passed as <2 x float>)
        (
            got_agg @ (BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)),
            BasicTypeEnum::VectorType(vt),
        ) => {
            // size check (ABI layout must match)
            let got_sz = env.target_data.get_store_size(&got_agg);
            let exp_sz = env
                .target_data
                .get_store_size(&BasicTypeEnum::VectorType(vt));
            if got_sz != exp_sz {
                panic!(
                    "Cannot coerce agg->vector: size mismatch {} vs {} (arg {} of {})",
                    got_sz, exp_sz, arg_index, name
                );
            }

            let tmp = env
                .builder
                .build_alloca(got_agg, &format!("arg{}_agg_tmp", arg_index))
                .unwrap();
            env.builder.build_store(tmp, val).unwrap();

            env.builder
                .build_load(vt, tmp, &format!("arg{}_agg2v", arg_index))
                .unwrap()
                .as_basic_value_enum()
        }

        // 4.6) vector -> agg(struct/array) (reverse of above)
        (
            BasicTypeEnum::VectorType(vt),
            dst_agg @ (BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)),
        ) => {
            let got_sz = env
                .target_data
                .get_store_size(&BasicTypeEnum::VectorType(vt));
            let exp_sz = env.target_data.get_store_size(&dst_agg);
            if got_sz != exp_sz {
                panic!(
                    "Cannot coerce vector->agg: size mismatch {} vs {} (arg {} of {})",
                    got_sz, exp_sz, arg_index, name
                );
            }

            let tmp = env
                .builder
                .build_alloca(dst_agg, &format!("arg{}_v2agg_tmp", arg_index))
                .unwrap();

            env.builder.build_store(tmp, val).unwrap();

            env.builder
                .build_load(dst_agg, tmp, &format!("arg{}_v2agg", arg_index))
                .unwrap()
                .as_basic_value_enum()
        }

        // 4.7) ptr -> vector  (used for ptr-to-agg cases)
        (BasicTypeEnum::PointerType(_), BasicTypeEnum::VectorType(vt)) => {
            let pv = val.into_pointer_value();
            env.builder
                .build_load(vt, pv, &format!("arg{}_p2v", arg_index))
                .unwrap()
                .as_basic_value_enum()
        }

        (
            BasicTypeEnum::IntType(src),
            dst_agg @ (BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)),
        ) => {
            let sz = env.target_data.get_store_size(&dst_agg) as u64;
            let bits = (sz * 8) as u32;
            if bits == src.get_bit_width() {
                let iv = val.into_int_value();
                return unpack_int_to_agg(env, iv, dst_agg, &format!("arg{}_unpack", arg_index));
            }
            panic!(
                "Cannot unpack int to aggregate: int bits {} != agg bits {} (arg {} of {})",
                src.get_bit_width(),
                bits,
                arg_index,
                name
            );
        }

        _ => {
            panic!(
                "Type mismatch for arg {} of '{}': expected {:?}, got {:?}",
                arg_index, name, expected, got
            );
        }
    }
}

fn any_agg_to_basic<'ctx>(ty: AnyTypeEnum<'ctx>) -> BasicTypeEnum<'ctx> {
    match ty {
        AnyTypeEnum::StructType(st) => st.as_basic_type_enum(),
        AnyTypeEnum::ArrayType(at) => at.as_basic_type_enum(),
        _ => panic!(
            "Expected aggregate AnyTypeEnum (struct/array), got {:?}",
            ty
        ),
    }
}

fn coerce_ptr_to<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    pv: PointerValue<'ctx>,
    expected_ptr_ty: inkwell::types::PointerType<'ctx>,
    tag: &str,
) -> PointerValue<'ctx> {
    if pv.get_type() == expected_ptr_ty {
        return pv;
    }
    env.builder
        .build_bit_cast(
            pv.as_basic_value_enum(),
            expected_ptr_ty.as_basic_type_enum(),
            tag,
        )
        .unwrap()
        .into_pointer_value()
}

fn split_agg_parts_from_agg<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    agg_val: BasicValueEnum<'ctx>,
    parts: &[BasicTypeEnum<'ctx>],
    tag: &str,
) -> Vec<BasicValueEnum<'ctx>> {
    let mut out = Vec::with_capacity(parts.len());
    let total_bytes: u64 = parts
        .iter()
        .map(|t| env.target_data.get_store_size(t) as u64)
        .sum();
    if total_bytes == 0 {
        panic!("Split lowering got zero-sized parts");
    }

    let aggregate_type = agg_val.get_type();
    let aggregate_size = env.target_data.get_store_size(&aggregate_type);
    let staging_type = env.context.i8_type().array_type(total_bytes as u32);
    let staging = env
        .builder
        .build_alloca(staging_type, &format!("{tag}_staging"))
        .unwrap();
    env.builder
        .build_store(staging, staging_type.const_zero())
        .unwrap();

    let aggregate = env
        .builder
        .build_alloca(aggregate_type, &format!("{tag}_aggregate"))
        .unwrap();
    env.builder.build_store(aggregate, agg_val).unwrap();
    env.builder
        .build_memcpy(
            staging,
            1,
            aggregate,
            env.target_data.get_abi_alignment(&aggregate_type),
            env.context
                .i64_type()
                .const_int(aggregate_size.min(total_bytes), false),
        )
        .unwrap();

    let mut offset: u64 = 0;
    for (pi, part_ty) in parts.iter().enumerate() {
        let part_size = env.target_data.get_store_size(part_ty) as u64;
        let dst = env
            .builder
            .build_alloca(*part_ty, &format!("{tag}_part_dst_{pi}"))
            .unwrap();

        let off = env.context.i64_type().const_int(offset, false);
        let src_off = unsafe {
            env.builder
                .build_gep(
                    env.context.i8_type(),
                    staging,
                    &[off],
                    &format!("{tag}_src_gep_{pi}"),
                )
                .unwrap()
        };

        let sz = env.context.i64_type().const_int(part_size, false);
        env.builder.build_memcpy(dst, 1, src_off, 1, sz).unwrap();

        let part_val = env
            .builder
            .build_load(*part_ty, dst, &format!("{tag}_part_load_{pi}"))
            .unwrap()
            .as_basic_value_enum();
        out.push(part_val);
        offset += part_size;
    }

    out
}

fn coerce_and_expand_agg_parts<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    aggregate: BasicValueEnum<'ctx>,
    parts: &[AbiPart<'ctx>],
    tag: &str,
) -> Vec<BasicValueEnum<'ctx>> {
    let aggregate_type = aggregate.get_type();
    let aggregate_size = env.target_data.get_store_size(&aggregate_type);
    let source = env
        .builder
        .build_alloca(aggregate_type, &format!("{tag}_aggregate"))
        .unwrap();
    env.builder.build_store(source, aggregate).unwrap();

    parts
        .iter()
        .enumerate()
        .map(|(index, part)| {
            let size = env.target_data.get_store_size(&part.ty);
            assert!(
                part.offset + size <= aggregate_size,
                "LoongArch ABI part extends past its aggregate"
            );
            let destination = env
                .builder
                .build_alloca(part.ty, &format!("{tag}_part_{index}"))
                .unwrap();
            let offset = env.context.i64_type().const_int(part.offset, false);
            // SAFETY: classification derives every offset from the target data
            // layout and verifies the complete part remains in the allocation.
            let source_at_offset = unsafe {
                env.builder
                    .build_gep(
                        env.context.i8_type(),
                        source,
                        &[offset],
                        &format!("{tag}_offset_{index}"),
                    )
                    .unwrap()
            };
            env.builder
                .build_memcpy(
                    destination,
                    env.target_data.get_abi_alignment(&part.ty),
                    source_at_offset,
                    1,
                    env.context.i64_type().const_int(size, false),
                )
                .unwrap();
            env.builder
                .build_load(part.ty, destination, &format!("{tag}_load_{index}"))
                .unwrap()
                .as_basic_value_enum()
        })
        .collect()
}

fn coerce_lowered_ret_to_expected<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    lowered_ret: BasicValueEnum<'ctx>,
    expected: BasicTypeEnum<'ctx>,
    tag: &str,
) -> BasicValueEnum<'ctx> {
    if lowered_ret.get_type() == expected {
        return lowered_ret;
    }

    match (lowered_ret.get_type(), expected) {
        (
            BasicTypeEnum::PointerType(_),
            BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_),
        ) => {
            let destination = env
                .builder
                .build_alloca(expected, &format!("{tag}_ptr_agg"))
                .unwrap();
            env.builder.build_store(destination, lowered_ret).unwrap();
            env.builder
                .build_load(expected, destination, &format!("{tag}_ptr_agg_load"))
                .unwrap()
                .as_basic_value_enum()
        }
        (BasicTypeEnum::IntType(_), BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)) => {
            let iv = lowered_ret.into_int_value();
            unpack_int_to_agg(env, iv, expected, tag)
        }

        // vector(float) -> struct/array (HFA ret)
        (
            BasicTypeEnum::VectorType(_vt),
            BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_),
        ) => {
            let tmp = env
                .builder
                .build_alloca(expected, &format!("{tag}_v2agg_tmp"))
                .unwrap();
            env.builder.build_store(tmp, lowered_ret).unwrap();
            env.builder
                .build_load(expected, tmp, &format!("{tag}_v2agg_load"))
                .unwrap()
                .as_basic_value_enum()
        }

        (
            BasicTypeEnum::FloatType(_ft),
            BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_),
        ) => {
            let tmp = env
                .builder
                .build_alloca(expected, &format!("{tag}_f2agg_tmp"))
                .unwrap();
            env.builder.build_store(tmp, lowered_ret).unwrap();
            env.builder
                .build_load(expected, tmp, &format!("{tag}_f2agg_load"))
                .unwrap()
                .as_basic_value_enum()
        }

        // lowered tuple-struct (e.g. {double,double}, {<2xf>,float}) -> expected aggregate
        (
            BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_),
            BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_),
        ) => {
            let got = lowered_ret.get_type();
            let got_sz = env.target_data.get_store_size(&got);
            let exp_sz = env.target_data.get_store_size(&expected);

            if got_sz < exp_sz {
                return lowered_ret;
            }

            let src = env
                .builder
                .build_alloca(got, &format!("{tag}_agg_src"))
                .unwrap();
            env.builder.build_store(src, lowered_ret).unwrap();

            let dst = env
                .builder
                .build_alloca(expected, &format!("{tag}_agg_dst"))
                .unwrap();

            let bytes = env.context.i64_type().const_int(exp_sz as u64, false);
            env.builder.build_memcpy(dst, 1, src, 1, bytes).unwrap();

            env.builder
                .build_load(expected, dst, &format!("{tag}_agg_cast"))
                .unwrap()
                .as_basic_value_enum()
        }

        _ => lowered_ret,
    }
}
