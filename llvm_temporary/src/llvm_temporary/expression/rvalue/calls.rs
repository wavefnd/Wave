use super::ExprGenEnv;
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum};
use parser::ast::{Expression, WaveType};

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
                    let arg_val = env.gen(arg_expr, expected_ty);
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
        let arg_val = env.gen(arg_expr, expected_ty);
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
        let val = env.gen(arg, Some(expected_param_ty));

        if val.get_type() != expected_param_ty {
            panic!(
                "Type mismatch for arg {} of '{}': expected {:?}, got {:?}",
                i,
                name,
                expected_param_ty,
                val.get_type()
            );
        }

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
