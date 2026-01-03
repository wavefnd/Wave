use inkwell::context::Context;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum};

use parser::ast::{Expression, Literal, WaveType};
use std::collections::HashMap;

use super::types::wave_type_to_llvm_type;

pub(super) fn create_llvm_const_value<'ctx>(
    context: &'ctx Context,
    ty: &WaveType,
    expr: &Expression,
) -> BasicValueEnum<'ctx> {
    let struct_types = HashMap::new();
    let llvm_type = wave_type_to_llvm_type(context, ty, &struct_types);
    match (expr, llvm_type) {
        (Expression::Literal(Literal::Number(n)), BasicTypeEnum::IntType(int_ty)) => {
            int_ty.const_int(*n as u64, true).as_basic_value_enum()
        }
        (Expression::Literal(Literal::Float(f)), BasicTypeEnum::FloatType(float_ty)) => {
            float_ty.const_float(*f).as_basic_value_enum()
        }
        _ => panic!("Constant expression must be a literal of a compatible type."),
    }
}
