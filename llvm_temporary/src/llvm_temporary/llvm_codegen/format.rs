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
            let mut spec = String::new();
            while let Some(&p) = chars.peek() {
                chars.next(); // consume '}'
                if p == '}' { break; }
                spec.push(p);
            }

            let spec = spec.trim();

            let ty = arg_types
                .get(arg_index)
                .unwrap_or_else(|| panic!("Missing argument for format"));

            let fmt = if spec.is_empty() {
                match ty {
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
                    BasicTypeEnum::PointerType(ptr_ty) => {
                        let elem = ptr_ty.get_element_type();
                        if elem.is_int_type() && elem.into_int_type().get_bit_width() == 8 {
                            "%s"   // i8* => C string
                        } else {
                            "%p"
                        }
                    }
                    _ => panic!("Unsupported type in format"),
                }
            } else {
                match spec {
                    "c" => "%c",
                    "x" => "%x",
                    "p" => "%p",
                    "s" => "%s",
                    "d" => "%d",
                    _ => panic!("Unknown format spec: {{{}}}", spec),
                }
            };
            result.push_str(fmt);
            arg_index += 1;
            continue;
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
                    AnyTypeEnum::IntType(int_ty) => {
                        if int_ty.get_bit_width() == 8 {
                            "%c"
                        } else {
                            "%d"
                        }
                    }
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
