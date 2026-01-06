use inkwell::types::BasicTypeEnum;
use super::ExprGenEnv;
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum};
use parser::ast::{Expression, WaveType};
use crate::llvm_temporary::statement::variable::{coerce_basic_value, CoercionMode};

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
                let expected_self = param_types.get(0).cloned();

                let obj_val = env.gen(object, expected_self);

                let mut call_args: Vec<BasicMetadataValueEnum> = Vec::new();
                call_args.push(obj_val.into());

                for (i, arg_expr) in args.iter().enumerate() {
                    let expected_ty = param_types.get(i + 1).cloned();
                    let mut arg_val = env.gen(arg_expr, expected_ty);
                    if let Some(et) = expected_ty {
                        arg_val = coerce_basic_value(
                            env.context, env.builder, arg_val, et, &format!("arg{}_cast", i),
                            CoercionMode::Implicit
                        );
                    }
                    call_args.push(arg_val.into());
                }

                let call_site = env
                    .builder
                    .build_call(function, &call_args, &format!("call_{}", fn_name))
                    .unwrap();

                if function.get_type().get_return_type().is_some() {
                    return call_site
                        .try_as_basic_value()
                        .left()
                        .expect("Expected a return value from struct method");
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

    let expected_self = param_types.get(0).cloned();
    let obj_val = env.gen(object, expected_self);

    let mut call_args: Vec<BasicMetadataValueEnum> = Vec::new();
    call_args.push(obj_val.into());

    for (i, arg_expr) in args.iter().enumerate() {
        let expected_ty = param_types.get(i + 1).cloned();
        let mut arg_val = env.gen(arg_expr, expected_ty);
        if let Some(et) = expected_ty {
            arg_val = coerce_basic_value(
                env.context, env.builder, arg_val, et, &format!("arg{}_cast", i),
                CoercionMode::Implicit
            );
        }
        call_args.push(arg_val.into());
    }

    let call_site = env
        .builder
        .build_call(function, &call_args, &format!("call_{}", name))
        .unwrap();

    if function.get_type().get_return_type().is_some() {
        call_site.try_as_basic_value().left().unwrap()
    } else {
        env.context.i32_type().const_zero().as_basic_value_enum()
    }
}

pub(crate) fn gen_function_call<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    name: &str,
    args: &[Expression],
    expected_type: Option<inkwell::types::BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    let function = env
        .module
        .get_function(name)
        .unwrap_or_else(|| panic!("Function '{}' not found", name));

    let fn_type = function.get_type();
    let param_types: Vec<inkwell::types::BasicTypeEnum> = fn_type.get_param_types();
    let ret_type: Option<inkwell::types::BasicTypeEnum> = fn_type.get_return_type();

    if args.len() != param_types.len() {
        panic!(
            "Function `{}` expects {} arguments, got {}",
            name,
            param_types.len(),
            args.len()
        );
    }

    let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::with_capacity(args.len());

    for (i, arg) in args.iter().enumerate() {
        let expected_param_ty = param_types[i];
        let mut val = env.gen(arg, Some(expected_param_ty));
        val = coerce_to_expected(env, val, expected_param_ty, name, i);

        call_args.push(val.into());
    }

    let call_name = if ret_type.is_some() { format!("call_{}", name) } else { String::new() };

    let call_site = env
        .builder
        .build_call(function, &call_args, &call_name)
        .unwrap();

    match ret_type {
        Some(_) => call_site.try_as_basic_value().left().unwrap(),
        None => {
            if expected_type.is_some() {
                panic!("Function '{}' returns void and cannot be used as a value", name);
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
) -> BasicValueEnum<'ctx> {
    let got = val.get_type();
    if got == expected {
        return val;
    }

    match (got, expected) {
        (BasicTypeEnum::IntType(src), BasicTypeEnum::IntType(dst)) => {
            let src_bw = src.get_bit_width();
            let dst_bw = dst.get_bit_width();
            let iv = val.into_int_value();

            if src_bw < dst_bw {
                env.builder
                    .build_int_s_extend(iv, dst, &format!("arg{}_sext", arg_index))
                    .unwrap()
                    .as_basic_value_enum()
            } else if src_bw > dst_bw {
                env.builder
                    .build_int_truncate(iv, dst, &format!("arg{}_trunc", arg_index))
                    .unwrap()
                    .as_basic_value_enum()
            } else {
                iv.as_basic_value_enum()
            }
        }

        (BasicTypeEnum::PointerType(_), BasicTypeEnum::PointerType(dst)) => {
            env.builder
                .build_bit_cast(val, dst, &format!("arg{}_ptrcast", arg_index))
                .unwrap()
                .as_basic_value_enum()
        }

        _ => {
            panic!(
                "Type mismatch for arg {} of '{}': expected {:?}, got {:?}",
                arg_index, name, expected, got
            );
        }
    }
}
