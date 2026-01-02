use super::ExprGenEnv;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::Literal;

pub(crate) fn gen<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    lit: &Literal,
    expected_type: Option<BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    match lit {
        Literal::Number(v) => match expected_type {
            Some(BasicTypeEnum::IntType(int_ty)) => {
                int_ty.const_int(*v as u64, false).as_basic_value_enum()
            }
            None => env
                .context
                .i64_type()
                .const_int(*v as u64, false)
                .as_basic_value_enum(),
            _ => panic!("Expected integer type for numeric literal, got {:?}", expected_type),
        },

        Literal::Float(value) => match expected_type {
            Some(BasicTypeEnum::FloatType(float_ty)) => float_ty.const_float(*value).as_basic_value_enum(),
            Some(BasicTypeEnum::IntType(int_ty)) => env
                .builder
                .build_float_to_signed_int(
                    env.context.f32_type().const_float(*value),
                    int_ty,
                    "f32_to_int",
                )
                .unwrap()
                .as_basic_value_enum(),
            None => env.context.f32_type().const_float(*value).as_basic_value_enum(),
            _ => panic!("Unsupported expected_type for float"),
        },

        Literal::String(value) => unsafe {
            let bytes = value.as_bytes();
            let mut null_terminated = bytes.to_vec();
            null_terminated.push(0);

            let global_name = format!("str_lit_{}", value.replace(" ", "_"));
            let str_type = env
                .context
                .i8_type()
                .array_type(null_terminated.len() as u32);

            let global = env.module.add_global(str_type, None, &global_name);
            global.set_initializer(&env.context.const_string(&null_terminated, false));
            global.set_constant(true);

            let zero = env.context.i32_type().const_zero();
            let indices = [zero, zero];
            let gep = env
                .builder
                .build_gep(global.as_pointer_value(), &indices, "str_gep")
                .unwrap();

            gep.as_basic_value_enum()
        },

        Literal::Bool(v) => env
            .context
            .bool_type()
            .const_int(if *v { 1 } else { 0 }, false)
            .as_basic_value_enum(),

        Literal::Char(c) => env.context.i8_type().const_int(*c as u64, false).as_basic_value_enum(),
        Literal::Byte(b) => env.context.i8_type().const_int(*b as u64, false).as_basic_value_enum(),

        _ => unimplemented!("Unsupported literal type"),
    }
}
