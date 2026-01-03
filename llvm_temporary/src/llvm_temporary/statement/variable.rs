use crate::llvm_temporary::expression::rvalue::generate_expression_ir;
use crate::llvm_temporary::llvm_codegen::{wave_type_to_llvm_type, VariableInfo};
use inkwell::module::{Linkage, Module};
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicValue, BasicValueEnum};
use inkwell::{AddressSpace};
use parser::ast::{Expression, Literal, VariableNode, WaveType};
use std::collections::HashMap;

pub(super) fn gen_variable_ir<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    var_node: &VariableNode,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
) {
    let VariableNode {
        name,
        type_name,
        initial_value,
        mutability,
    } = var_node;

    unsafe {
        let llvm_type = wave_type_to_llvm_type(context, type_name, struct_types);
        let alloca = builder.build_alloca(llvm_type, name).unwrap();

        if let (WaveType::Array(element_type, size), Some(Expression::ArrayLiteral(values))) =
            (type_name, initial_value.as_ref())
        {
            if values.len() != *size as usize {
                panic!(
                    "❌ Array length mismatch: expected {}, got {}",
                    size,
                    values.len()
                );
            }

            let llvm_element_type = wave_type_to_llvm_type(context, element_type, struct_types);

            for (i, value_expr) in values.iter().enumerate() {
                let value = generate_expression_ir(
                    context,
                    builder,
                    value_expr,
                    variables,
                    module,
                    Some(llvm_element_type),
                    global_consts,
                    struct_types,
                    struct_field_indices,
                );

                let gep = builder
                    .build_in_bounds_gep(
                        alloca,
                        &[
                            context.i32_type().const_zero(),
                            context.i32_type().const_int(i as u64, false),
                        ],
                        &format!("array_idx_{}", i),
                    )
                    .unwrap();

                builder.build_store(gep, value).unwrap();
            }

            variables.insert(
                name.clone(),
                VariableInfo {
                    ptr: alloca,
                    mutability: mutability.clone(),
                    ty: type_name.clone(),
                },
            );

            return;
        }

        variables.insert(
            name.clone(),
            VariableInfo {
                ptr: alloca,
                mutability: mutability.clone(),
                ty: type_name.clone(),
            },
        );

        if let Some(init) = initial_value {
            match (init, llvm_type) {
                (
                    Expression::Literal(Literal::Number(value)),
                    BasicTypeEnum::IntType(int_type),
                ) => {
                    let init_value = int_type.const_int(*value as u64, false);
                    let _ = builder.build_store(alloca, init_value);
                }

                (
                    Expression::Literal(Literal::Float(value)),
                    BasicTypeEnum::FloatType(float_type),
                ) => {
                    let init_value = float_type.const_float(*value);
                    builder.build_store(alloca, init_value).unwrap();
                }

                (
                    Expression::Literal(Literal::Bool(v)),
                    BasicTypeEnum::IntType(int_ty),
                ) => {
                    let val = int_ty.const_int(if *v { 1 } else { 0 }, false);
                    builder.build_store(alloca, val).unwrap();
                }

                (
                    Expression::Literal(Literal::Char(c)),
                    BasicTypeEnum::IntType(int_ty),
                ) => {
                    let val = int_ty.const_int(*c as u64, false);
                    builder.build_store(alloca, val).unwrap();
                }

                (
                    Expression::Literal(Literal::Byte(b)),
                    BasicTypeEnum::IntType(int_ty),
                ) => {
                    let val = int_ty.const_int(*b as u64, false);
                    builder.build_store(alloca, val).unwrap();
                }

                (Expression::Literal(Literal::Float(value)), _) => {
                    let float_value = context.f32_type().const_float(*value);

                    let casted_value = match llvm_type {
                        BasicTypeEnum::IntType(int_ty) => builder
                            .build_float_to_signed_int(float_value, int_ty, "float_to_int")
                            .unwrap()
                            .as_basic_value_enum(),
                        BasicTypeEnum::FloatType(_) => float_value.as_basic_value_enum(),
                        _ => panic!("Unsupported type for float literal initialization"),
                    };

                    builder.build_store(alloca, casted_value).unwrap();
                }

                (
                    Expression::Literal(Literal::String(value)),
                    BasicTypeEnum::PointerType(_),
                ) => unsafe {
                    let string_name = format!("str_init_{}", name);
                    let mut bytes = value.as_bytes().to_vec();
                    bytes.push(0);

                    let const_str = context.const_string(&bytes, false);
                    let global = module.add_global(
                        context.i8_type().array_type(bytes.len() as u32),
                        None,
                        &string_name,
                    );
                    global.set_initializer(&const_str);
                    global.set_linkage(Linkage::Private);
                    global.set_constant(true);

                    let zero = context.i32_type().const_zero();
                    let indices = [zero, zero];
                    let gep = builder
                        .build_gep(global.as_pointer_value(), &indices, "str_gep")
                        .unwrap();

                    let _ = builder.build_store(alloca, gep);
                },

                (Expression::AddressOf(inner_expr), BasicTypeEnum::PointerType(_)) => {
                    match &**inner_expr {
                        Expression::Variable(var_name) => {
                            let ptr = variables
                                .get(var_name)
                                .unwrap_or_else(|| panic!("Variable {} not found", var_name));
                            builder.build_store(alloca, ptr.ptr).unwrap();
                        }

                        Expression::ArrayLiteral(elements) => {
                            let elem_type = match llvm_type {
                                BasicTypeEnum::PointerType(ptr_ty) => match ptr_ty.get_element_type() {
                                    AnyTypeEnum::ArrayType(arr_ty) => arr_ty.get_element_type(),
                                    _ => panic!("Expected pointer to array type"),
                                },
                                _ => panic!("Expected pointer-to-array type for array literal"),
                            };

                            let array_type = elem_type.array_type(elements.len() as u32);
                            let tmp_alloca =
                                builder.build_alloca(array_type, "tmp_array").unwrap();

                            for (i, expr) in elements.iter().enumerate() {
                                let val = generate_expression_ir(
                                    context,
                                    builder,
                                    expr,
                                    variables,
                                    module,
                                    Some(elem_type),
                                    global_consts,
                                    struct_types,
                                    struct_field_indices,
                                );

                                let gep = builder
                                    .build_in_bounds_gep(
                                        tmp_alloca,
                                        &[
                                            context.i32_type().const_zero(),
                                            context.i32_type().const_int(i as u64, false),
                                        ],
                                        &format!("array_idx_{}", i),
                                    )
                                    .unwrap();

                                builder.build_store(gep, val).unwrap();
                            }

                            builder.build_store(alloca, tmp_alloca).unwrap();
                        }

                        _ => panic!("& operator must be used on variable name or array literal"),
                    }
                }

                (Expression::Deref(inner_expr), BasicTypeEnum::IntType(_)) => {
                    let target_ptr = match &**inner_expr {
                        Expression::Variable(var_name) => {
                            let ptr_to_value = variables.get(var_name).unwrap().ptr;
                            builder
                                .build_load(ptr_to_value, "load_ptr")
                                .unwrap()
                                .into_pointer_value()
                        }
                        _ => panic!("Invalid deref in variable init"),
                    };

                    let val = builder.build_load(target_ptr, "deref_value").unwrap();
                    let _ = builder.build_store(alloca, val);
                }

                (Expression::IndexAccess { .. }, _) => {
                    let val = generate_expression_ir(
                        context,
                        builder,
                        init,
                        variables,
                        module,
                        Some(llvm_type),
                        global_consts,
                        struct_types,
                        struct_field_indices,
                    );
                    builder.build_store(alloca, val).unwrap();
                }

                (Expression::FunctionCall { .. } | Expression::MethodCall { .. }, _) => {
                    let val = generate_expression_ir(
                        context,
                        builder,
                        init,
                        variables,
                        module,
                        Some(llvm_type),
                        global_consts,
                        struct_types,
                        struct_field_indices,
                    );

                    if val.get_type() != llvm_type {
                        panic!(
                            "Initializer type mismatch: expected {:?}, got {:?}",
                            llvm_type,
                            val.get_type()
                        );
                    }

                    builder.build_store(alloca, val).unwrap();
                }

                (Expression::BinaryExpression { .. }, _) => {
                    let val = generate_expression_ir(
                        context,
                        builder,
                        init,
                        variables,
                        module,
                        Some(llvm_type),
                        global_consts,
                        struct_types,
                        struct_field_indices,
                    );

                    let casted_val = match (val, llvm_type) {
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

                    builder.build_store(alloca, casted_val).unwrap();
                }

                (Expression::Variable(var_name), _) => {
                    let source_var = variables
                        .get(var_name)
                        .unwrap_or_else(|| panic!("Variable {} not found", var_name));

                    let loaded_value = builder
                        .build_load(source_var.ptr, &format!("load_{}", var_name))
                        .unwrap();

                    let loaded_type = loaded_value.get_type();

                    let casted_value = match (loaded_type, llvm_type) {
                        (BasicTypeEnum::IntType(_), BasicTypeEnum::FloatType(float_ty)) => builder
                            .build_signed_int_to_float(
                                loaded_value.into_int_value(),
                                float_ty,
                                "int_to_float",
                            )
                            .unwrap()
                            .as_basic_value_enum(),
                        (BasicTypeEnum::FloatType(_), BasicTypeEnum::IntType(int_ty)) => builder
                            .build_float_to_signed_int(
                                loaded_value.into_float_value(),
                                int_ty,
                                "float_to_int",
                            )
                            .unwrap()
                            .as_basic_value_enum(),
                        _ => loaded_value,
                    };

                    builder.build_store(alloca, casted_value).unwrap();
                }

                (
                    Expression::AsmBlock {
                        instructions,
                        inputs,
                        outputs,
                    },
                    BasicTypeEnum::IntType(_),
                ) => {
                    use inkwell::values::{BasicMetadataValueEnum, CallableValue};
                    use inkwell::InlineAsmDialect;

                    let asm_code: String = instructions.join("\n");
                    let mut operand_vals: Vec<BasicMetadataValueEnum> = vec![];
                    let mut constraint_parts = vec![];

                    for (reg, var) in inputs {
                        let val = if let Expression::Literal(Literal::Number(n)) = var {
                            context.i64_type().const_int(*n as u64, true).into()
                        } else if let Some(name) = var.as_identifier() {
                            if let Some(info) = variables.get(name) {
                                builder.build_load(info.ptr, name).unwrap().into()
                            } else {
                                panic!("Variable '{}' not found", name);
                            }
                        } else {
                            panic!("Unsupported expression in statement: {:?}", var);
                        };

                        operand_vals.push(val);
                        constraint_parts.push(format!("{{{}}}", reg));
                    }

                    for (reg, _) in outputs {
                        constraint_parts.insert(0, format!("={{{}}}", reg));
                    }

                    let constraint_str = constraint_parts.join(",");

                    let (fn_type, expects_return) = if outputs.is_empty() {
                        (context.void_type().fn_type(&[], false), false)
                    } else {
                        (context.i64_type().fn_type(&[], false), true)
                    };

                    let inline_asm_ptr = context.create_inline_asm(
                        fn_type,
                        asm_code,
                        constraint_str,
                        true,
                        false,
                        Some(InlineAsmDialect::Intel),
                        false,
                    );

                    let inline_asm_fn = CallableValue::try_from(inline_asm_ptr)
                        .expect("Failed to cast inline asm to CallableValue");

                    let call = builder
                        .build_call(inline_asm_fn, &operand_vals, "inline_asm")
                        .unwrap();

                    if expects_return {
                        let result = call
                            .try_as_basic_value()
                            .left()
                            .expect("Expected return value from inline asm but got none");

                        builder.build_store(alloca, result).unwrap();
                    }
                }

                (init_expr @ Expression::StructLiteral { .. }, _) => {
                    let val = generate_expression_ir(
                        context,
                        builder,
                        init_expr,
                        variables,
                        module,
                        Some(llvm_type),
                        global_consts,
                        struct_types,
                        struct_field_indices,
                    );

                    builder.build_store(alloca, val).unwrap();
                }

                _ => {
                    panic!(
                        "Unsupported type/value combination for initialization: {:?}",
                        init
                    );
                }
            }
        }
    }
}
