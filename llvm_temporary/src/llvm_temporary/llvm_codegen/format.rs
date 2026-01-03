use inkwell::context::Context;
use inkwell::types::{AnyTypeEnum, BasicTypeEnum};

pub fn wave_format_to_c<'ctx>(
    context: &'ctx Context,
    format: &str,
    arg_types: &[BasicTypeEnum<'ctx>],
) -> String {
    let mut result = String::new();
    let mut chars = format.chars().peekable();
    let mut arg_index = 0;

    while let Some(c) = chars.next() {
        if c == '{' {
            if let Some('}') = chars.peek() {
                chars.next(); // consume '}'

                let ty = arg_types
                    .get(arg_index)
                    .unwrap_or_else(|| panic!("Missing argument for format"));

                let fmt = match ty {
                    BasicTypeEnum::IntType(int_ty) => {
                        let bits = int_ty.get_bit_width();
                        match bits {
                            8 => "%hhd",
                            16 => "%hd",
                            32 => "%d",
                            64 => "%ld",
                            128 => "%lld",
                            _ => "%d",
                        }
                    }
                    BasicTypeEnum::FloatType(float_ty) => {
                        if *float_ty == context.f32_type() {
                            "%f"
                        } else {
                            "%lf"
                        }
                    }
                    BasicTypeEnum::PointerType(_) => "%p",
                    _ => panic!("Unsupported type in format"),
                };

                result.push_str(fmt);
                arg_index += 1;
                continue;
            }
        }

        result.push(c);
    }

    result
}

pub fn wave_format_to_scanf(format: &str, arg_types: &[AnyTypeEnum]) -> String {
    let mut result = String::new();
    let mut chars = format.chars().peekable();
    let mut arg_index = 0;

    while let Some(c) = chars.next() {
        if c == '{' {
            if let Some('}') = chars.peek() {
                chars.next(); // consume '}'

                let ty = arg_types
                    .get(arg_index)
                    .unwrap_or_else(|| panic!("Missing argument for format"));

                let fmt = match ty {
                    AnyTypeEnum::IntType(_) => "%d",
                    AnyTypeEnum::FloatType(_) => "%f",
                    AnyTypeEnum::PointerType(_) => {
                        panic!("Cannot input into a pointer type directly")
                    }
                    _ => panic!("Unsupported type in scanf format"),
                };

                result.push_str(fmt);
                arg_index += 1;
                continue;
            }
        }

        result.push(c);
    }

    result
}
