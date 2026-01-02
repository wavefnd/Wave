use super::ExprGenEnv;
use crate::llvm_temporary::llvm_codegen::generate_address_ir;
use inkwell::types::{AnyTypeEnum, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::{AssignOperator, Expression};

pub(crate) fn gen_assign_operation<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    target: &Expression,
    operator: &AssignOperator,
    value: &Expression,
) -> BasicValueEnum<'ctx> {
    let ptr = generate_address_ir(env.context, env.builder, target, env.variables, env.module);

    let current_val = env.builder.build_load(ptr, "load_current").unwrap();

    let new_val = env.gen(value, Some(current_val.get_type()));

    let (current_val, new_val) = match (current_val, new_val) {
        (BasicValueEnum::FloatValue(lhs), BasicValueEnum::IntValue(rhs)) => {
            let rhs_casted = env
                .builder
                .build_signed_int_to_float(rhs, lhs.get_type(), "int_to_float")
                .unwrap();
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs_casted))
        }
        (BasicValueEnum::IntValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
            let lhs_casted = env
                .builder
                .build_signed_int_to_float(lhs, rhs.get_type(), "int_to_float")
                .unwrap();
            (BasicValueEnum::FloatValue(lhs_casted), BasicValueEnum::FloatValue(rhs))
        }
        other => other,
    };

    let result = match (current_val, new_val) {
        (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => match operator {
            AssignOperator::Assign => rhs.as_basic_value_enum(),
            AssignOperator::AddAssign => env.builder.build_int_add(lhs, rhs, "add_assign").unwrap().as_basic_value_enum(),
            AssignOperator::SubAssign => env.builder.build_int_sub(lhs, rhs, "sub_assign").unwrap().as_basic_value_enum(),
            AssignOperator::MulAssign => env.builder.build_int_mul(lhs, rhs, "mul_assign").unwrap().as_basic_value_enum(),
            AssignOperator::DivAssign => env.builder.build_int_signed_div(lhs, rhs, "div_assign").unwrap().as_basic_value_enum(),
            AssignOperator::RemAssign => env.builder.build_int_signed_rem(lhs, rhs, "rem_assign").unwrap().as_basic_value_enum(),
        },

        (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => match operator {
            AssignOperator::Assign => rhs.as_basic_value_enum(),
            AssignOperator::AddAssign => env.builder.build_float_add(lhs, rhs, "add_assign").unwrap().as_basic_value_enum(),
            AssignOperator::SubAssign => env.builder.build_float_sub(lhs, rhs, "sub_assign").unwrap().as_basic_value_enum(),
            AssignOperator::MulAssign => env.builder.build_float_mul(lhs, rhs, "mul_assign").unwrap().as_basic_value_enum(),
            AssignOperator::DivAssign => env.builder.build_float_div(lhs, rhs, "div_assign").unwrap().as_basic_value_enum(),
            AssignOperator::RemAssign => env.builder.build_float_rem(lhs, rhs, "rem_assign").unwrap().as_basic_value_enum(),
        },

        _ => panic!("Type mismatch or unsupported type in AssignOperation"),
    };

    let element_type = match ptr.get_type().get_element_type() {
        AnyTypeEnum::IntType(t) => BasicTypeEnum::IntType(t),
        AnyTypeEnum::FloatType(t) => BasicTypeEnum::FloatType(t),
        AnyTypeEnum::PointerType(t) => BasicTypeEnum::PointerType(t),
        AnyTypeEnum::ArrayType(t) => BasicTypeEnum::ArrayType(t),
        AnyTypeEnum::StructType(t) => BasicTypeEnum::StructType(t),
        AnyTypeEnum::VectorType(t) => BasicTypeEnum::VectorType(t),
        _ => panic!("Unsupported LLVM element type"),
    };

    let result_casted = match (result, element_type) {
        (BasicValueEnum::FloatValue(val), BasicTypeEnum::IntType(int_ty)) => env
            .builder
            .build_float_to_signed_int(val, int_ty, "float_to_int")
            .unwrap()
            .as_basic_value_enum(),
        (BasicValueEnum::IntValue(val), BasicTypeEnum::FloatType(float_ty)) => env
            .builder
            .build_signed_int_to_float(val, float_ty, "int_to_float")
            .unwrap()
            .as_basic_value_enum(),
        _ => result,
    };

    env.builder.build_store(ptr, result_casted).unwrap();
    result
}

pub(crate) fn gen_assignment<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    target: &Expression,
    value: &Expression,
) -> BasicValueEnum<'ctx> {
    let ptr = generate_address_ir(env.context, env.builder, target, env.variables, env.module);

    let value = env.gen(
        value,
        Some(ptr.get_type().get_element_type().try_into().unwrap()),
    );

    let value = match value {
        BasicValueEnum::IntValue(v) => v.as_basic_value_enum(),
        BasicValueEnum::FloatValue(v) => v.as_basic_value_enum(),
        BasicValueEnum::PointerValue(v) => v.as_basic_value_enum(),
        _ => panic!("Unsupported assignment value"),
    };

    env.builder.build_store(ptr, value).unwrap();
    value
}
