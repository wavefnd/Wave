use inkwell::AddressSpace;
use super::ExprGenEnv;
use inkwell::types::AnyTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum};

pub(crate) fn gen<'ctx, 'a>(env: &mut ExprGenEnv<'ctx, 'a>, var_name: &str) -> BasicValueEnum<'ctx> {
    if var_name == "true" {
        return env.context.bool_type().const_int(1, false).as_basic_value_enum();
    } else if var_name == "false" {
        return env.context.bool_type().const_int(0, false).as_basic_value_enum();
    } else if var_name == "null" {
        return env.context
            .i8_type()
            .ptr_type(AddressSpace::from(0))
            .const_null()
            .as_basic_value_enum();
    }

    if let Some(const_val) = env.global_consts.get(var_name) {
        return *const_val;
    }

    if let Some(var_info) = env.variables.get(var_name) {
        let var_type = var_info.ptr.get_type().get_element_type();

        match var_type {
            AnyTypeEnum::ArrayType(_) => var_info.ptr.as_basic_value_enum(),
            _ => env
                .builder
                .build_load(var_info.ptr, var_name)
                .unwrap()
                .as_basic_value_enum(),
        }
    } else if env.module.get_function(var_name).is_some() {
        panic!("Error: '{}' is a function name, not a variable", var_name);
    } else {
        panic!("variable '{}' not found in current scope", var_name);
    }
}
