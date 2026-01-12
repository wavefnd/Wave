use crate::llvm_temporary::expression::rvalue::generate_expression_ir;
use crate::llvm_temporary::llvm_codegen::{generate_address_ir, VariableInfo};
use inkwell::module::Module;
use inkwell::types::{AnyTypeEnum, BasicTypeEnum, StructType};
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::{Expression, Mutability};
use std::collections::HashMap;

pub(super) fn gen_assign_ir<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    variable: &str,
    value: &Expression,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
) {
    if variable == "deref" {
        if let Expression::BinaryExpression { left, right, .. } = value {
            if let Expression::Deref(inner_expr) = &**left {
                let target_ptr =
                    generate_address_ir(context, builder, inner_expr, variables, module, struct_types, struct_field_indices);

                let val = generate_expression_ir(
                    context,
                    builder,
                    right,
                    variables,
                    module,
                    None,
                    global_consts,
                    struct_types,
                    struct_field_indices,
                );

                builder.build_store(target_ptr, val).unwrap();
            }
        }
        return;
    }

    let (dst_ptr, dst_mutability) = {
        let info = variables
            .get(variable)
            .unwrap_or_else(|| panic!("Variable {} not declared", variable));
        (info.ptr, info.mutability.clone())
    };

    if matches!(dst_mutability, Mutability::Let) {
        panic!("Cannot assign to immutable variable '{}'", variable);
    }

    let element_type: BasicTypeEnum<'ctx> = match dst_ptr.get_type().get_element_type() {
        AnyTypeEnum::IntType(t) => BasicTypeEnum::IntType(t),
        AnyTypeEnum::FloatType(t) => BasicTypeEnum::FloatType(t),
        AnyTypeEnum::PointerType(t) => BasicTypeEnum::PointerType(t),
        AnyTypeEnum::ArrayType(t) => BasicTypeEnum::ArrayType(t),
        AnyTypeEnum::StructType(t) => BasicTypeEnum::StructType(t),
        AnyTypeEnum::VectorType(t) => BasicTypeEnum::VectorType(t),
        _ => panic!("Unsupported LLVM type in assignment"),
    };

    let val = generate_expression_ir(
        context,
        builder,
        value,
        variables,
        module,
        Some(element_type),
        global_consts,
        struct_types,
        struct_field_indices,
    );

    let casted_val = match (val, element_type) {
        (BasicValueEnum::FloatValue(v), BasicTypeEnum::IntType(t)) => builder
            .build_float_to_signed_int(v, t, "float_to_int")
            .unwrap()
            .as_basic_value_enum(),
        (BasicValueEnum::IntValue(v), BasicTypeEnum::FloatType(t)) => builder
            .build_signed_int_to_float(v, t, "int_to_float")
            .unwrap()
            .as_basic_value_enum(),
        _ => val,
    };

    builder.build_store(dst_ptr, casted_val).unwrap();
}
