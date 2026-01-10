use super::ExprGenEnv;
use inkwell::types::{BasicTypeEnum, StringRadix};
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::Literal;

fn parse_signed_decimal<'a>(s: &'a str) -> (bool, &'a str) {
    if let Some(rest) = s.strip_prefix('-') {
        (true, rest)
    } else {
        (false, s)
    }
}

fn is_zero_decimal(s: &str) -> bool {
    let s = s.trim();
    let s = s.strip_prefix('+').unwrap_or(s);
    let s = s.strip_prefix('-').unwrap_or(s);

    !s.is_empty() && s.chars().all(|c| c == '0')
}

fn parse_int_radix(s: &str) -> (StringRadix, &str) {
    if let Some(rest) = s.strip_prefix("0b").or_else(|| s.strip_prefix("0B")) {
        (StringRadix::Binary, rest)
    } else if let Some(rest) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        (StringRadix::Hexadecimal, rest)
    } else if let Some(rest) = s.strip_prefix("0o").or_else(|| s.strip_prefix("0O")) {
        (StringRadix::Octal, rest)
    } else {
        (StringRadix::Decimal, s)
    }
}

pub(crate) fn gen<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    lit: &Literal,
    expected_type: Option<BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    match lit {
        Literal::Int(v) => match expected_type {
            Some(BasicTypeEnum::IntType(int_ty)) => {
                let s = v.as_str();
                let (neg, raw) = parse_signed_decimal(s);
                let (radix, digits) = parse_int_radix(raw);

                let mut iv = int_ty
                    .const_int_from_string(digits, radix)
                    .unwrap_or_else(|| panic!("invalid int literal: {}", s));

                if neg {
                    iv = iv.const_neg();
                }

                iv.as_basic_value_enum()
            }

            Some(BasicTypeEnum::PointerType(ptr_ty)) => {
                if is_zero_decimal(v.as_str()) {
                    ptr_ty.const_null().as_basic_value_enum()
                } else {
                    panic!("Only 0 can be used as null pointer literal");
                }
            }

            Some(BasicTypeEnum::FloatType(ft)) => {
                let f = v
                    .parse::<f64>()
                    .unwrap_or_else(|_| panic!("invalid float literal from int token: {}", v));
                ft.const_float(f).as_basic_value_enum()
            }

            None => {
                let s = v.as_str();
                let (neg, raw) = parse_signed_decimal(s);
                let (radix, digits) = parse_int_radix(raw);

                let mut iv = env
                    .context
                    .i64_type()
                    .const_int_from_string(digits, radix)
                    .unwrap_or_else(|| panic!("invalid int literal: {}", s));

                if neg {
                    iv = iv.const_neg();
                }

                iv.as_basic_value_enum()
            },

            _ => panic!("Unsupported expected_type for int literal: {:?}", expected_type),
        }

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
    }
}
