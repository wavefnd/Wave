use inkwell::context::Context;
use inkwell::types::{BasicTypeEnum, StringRadix};
use inkwell::values::{BasicValue, BasicValueEnum};

use parser::ast::{Expression, Literal, WaveType};
use std::collections::HashMap;

use super::types::wave_type_to_llvm_type;

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

pub(super) fn create_llvm_const_value<'ctx>(
    context: &'ctx Context,
    ty: &WaveType,
    expr: &Expression,
) -> BasicValueEnum<'ctx> {
    let struct_types = HashMap::new();
    let llvm_type = wave_type_to_llvm_type(context, ty, &struct_types);

    match (expr, llvm_type) {
        // new: int literal is string-based
        (Expression::Literal(Literal::Int(s)), BasicTypeEnum::IntType(int_ty)) => {
            let (neg, digits) = parse_signed_decimal(s.as_str());

            let mut iv = int_ty
                .const_int_from_string(digits, StringRadix::Decimal)
                .unwrap_or_else(|| panic!("invalid int literal: {}", s));

            if neg {
                iv = iv.const_neg();
            }

            iv.as_basic_value_enum()
        }

        (Expression::Literal(Literal::Float(f)), BasicTypeEnum::FloatType(float_ty)) => {
            float_ty.const_float(*f).as_basic_value_enum()
        }

        // allow const null pointer only via 0
        (Expression::Literal(Literal::Int(s)), BasicTypeEnum::PointerType(ptr_ty)) => {
            if is_zero_decimal(s) {
                ptr_ty.const_null().as_basic_value_enum()
            } else {
                panic!("Only 0 can be used as a const null pointer literal");
            }
        }

        _ => panic!("Constant expression must be a literal of a compatible type."),
    }
}