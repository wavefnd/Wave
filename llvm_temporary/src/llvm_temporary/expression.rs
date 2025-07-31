use std::collections::HashMap;
use inkwell::context::Context;
use inkwell::{FloatPredicate, IntPredicate};
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::{AssignOperator, Expression, Literal, Operator};
use crate::llvm_temporary::llvm_codegen::{generate_address_ir, VariableInfo};

pub fn generate_expression_ir<'ctx>(
    context: &'ctx Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    expr: &Expression,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    module: &'ctx inkwell::module::Module<'ctx>,
    expected_type: Option<BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    match expr {
        Expression::Literal(lit) => match lit {
            Literal::Number(v) => {
                match expected_type {
                    Some(BasicTypeEnum::IntType(int_ty)) => {
                        int_ty.const_int(*v as u64, false).as_basic_value_enum()
                    }
                    None => {
                        context.i64_type().const_int(*v as u64, false).as_basic_value_enum()
                    }
                    _ => panic!("Expected integer type for numeric literal, got {:?}", expected_type),
                }
            }
            Literal::Float(value) => {
                match expected_type {
                    Some(BasicTypeEnum::FloatType(float_ty)) => float_ty.const_float(*value).as_basic_value_enum(),
                    Some(BasicTypeEnum::IntType(int_ty)) => builder.build_float_to_signed_int(context.f32_type().const_float(*value), int_ty, "f32_to_int").unwrap().as_basic_value_enum(),
                    None => context.f32_type().const_float(*value).as_basic_value_enum(),
                    _ => panic!("Unsupported expected_type for float"),
                }
            }
            Literal::String(value) => unsafe {
                let bytes = value.as_bytes();
                let mut null_terminated = bytes.to_vec();
                null_terminated.push(0);

                let global_name = format!("str_lit_{}", value.replace(" ", "_"));
                let str_type = context.i8_type().array_type(null_terminated.len() as u32);
                let global = module.add_global(str_type, None, &global_name);
                global.set_initializer(&context.const_string(&null_terminated, false));
                global.set_constant(true);

                let zero = context.i32_type().const_zero();
                let indices = [zero, zero];
                let gep = builder.build_gep(global.as_pointer_value(), &indices, "str_gep").unwrap();

                gep.as_basic_value_enum()
            }
            _ => unimplemented!("Unsupported literal type"),
        },

        Expression::Variable(var_name) => {
            if var_name == "true" {
                return context.bool_type().const_int(1, false).as_basic_value_enum();
            } else if var_name == "false" {
                return context.bool_type().const_int(0, false).as_basic_value_enum();
            }

            if let Some(var_info) = variables.get(var_name) {
                let var_type = var_info.ptr.get_type().get_element_type();

                match var_type {
                    AnyTypeEnum::ArrayType(_) => {
                        var_info.ptr.as_basic_value_enum()
                    }
                    _ => {
                        builder.build_load(var_info.ptr, var_name).unwrap().as_basic_value_enum()
                    }
                }
            } else if module.get_function(var_name).is_some() {
                panic!("Error: '{}' is a function name, not a variable", var_name);
            } else {
                panic!("variable '{}' not found in current scope", var_name);
            }
        }

        Expression::Deref(inner_expr) => {
            match &**inner_expr {
                Expression::Variable(var_name) => {
                    let ptr_to_value = variables.get(var_name).unwrap().ptr;
                    let actual_ptr = builder.build_load(ptr_to_value, "deref_target").unwrap().into_pointer_value();
                    builder.build_load(actual_ptr, "deref_load").unwrap().as_basic_value_enum()
                }
                _ => {
                    let ptr_val = generate_expression_ir(context, builder, inner_expr, variables, module, None);
                    let ptr = ptr_val.into_pointer_value();
                    builder.build_load(ptr, "deref_load").unwrap().as_basic_value_enum()
                }
            }
        }

        Expression::AddressOf(inner_expr) => {
            if let Some(BasicTypeEnum::PointerType(ptr_ty)) = expected_type {
                match &**inner_expr {
                    Expression::ArrayLiteral(elements) => unsafe {
                        let array_type = ptr_ty.get_element_type().into_array_type();
                        let elem_type = array_type.get_element_type();

                        let array_type = elem_type.array_type(elements.len() as u32);
                        let tmp_alloca = builder.build_alloca(array_type, "tmp_array").unwrap();

                        for (i, expr) in elements.iter().enumerate() {
                            let val = generate_expression_ir(
                                context,
                                builder,
                                expr,
                                variables,
                                module,
                                Some(elem_type),
                            );
                            let gep = builder.build_in_bounds_gep(
                                tmp_alloca,
                                &[
                                    context.i32_type().const_zero(),
                                    context.i32_type().const_int(i as u64, false),
                                ],
                                &format!("array_idx_{}", i),
                            ).unwrap();
                            builder.build_store(gep, val).unwrap();
                        }

                        let alloca = builder.build_alloca(tmp_alloca.get_type(), "tmp_array_ptr").unwrap();
                        builder.build_store(alloca, tmp_alloca).unwrap();
                        alloca.as_basic_value_enum()
                    }

                    Expression::Variable(var_name) => {
                        let ptr = variables.get(var_name)
                            .unwrap_or_else(|| panic!("Variable {} not found", var_name));
                        ptr.ptr.as_basic_value_enum()
                    }

                    _ => panic!("& operator must be used on variable name or array literal"),
                }
            } else {
                panic!("Expected pointer type for AddressOf");
            }
        }

        Expression::FunctionCall { name, args } => {
            let function = module
                .get_function(name)
                .unwrap_or_else(|| panic!("Function '{}' not found", name));

            let function_type = function.get_type();
            let param_types: Vec<BasicTypeEnum> = function_type
                .get_param_types()
                .iter()
                .map(|t| t.clone().into())
                .collect();

            let mut compiled_args = vec![];
            for (i, arg) in args.iter().enumerate() {
                let expected = param_types.get(i).copied();
                let val = generate_expression_ir(context, builder, arg, variables, module, expected);
                compiled_args.push(val.into());
            }

            let call_site = builder.build_call(function, &compiled_args, "calltmp").unwrap();

            if function_type.get_return_type().is_some() {
                if let Some(ret_val) = call_site.try_as_basic_value().left() {
                    ret_val
                } else {
                    panic!("Function '{}' should return a value but didn't", name);
                }
            } else {
                context.i32_type().const_zero().as_basic_value_enum()
            }
        }

        Expression::MethodCall { object, name, args } => {
            let function = module
                .get_function(name)
                .unwrap_or_else(|| panic!("Function '{}' not found as a global function", name));

            let function_type = function.get_type();
            let param_types: Vec<BasicTypeEnum> = function_type
                .get_param_types()
                .iter()
                .map(|t| (*t).into())
                .collect();

            let object_expected_type = param_types.get(0).copied();
            let object_val = generate_expression_ir(context, builder, object, variables, module, object_expected_type);

            let mut compiled_args: Vec<inkwell::values::BasicMetadataValueEnum<'ctx>> = vec![object_val.into()];

            for (i, arg) in args.iter().enumerate() {
                let expected = param_types.get(i + 1).copied();
                let val = generate_expression_ir(context, builder, arg, variables, module, expected);

                compiled_args.push(val.into());
            }

            let call_site = builder.build_call(function, &compiled_args, &format!("call_{}", name)).unwrap();

            if function_type.get_return_type().is_some() {
                if let Some(ret_val) = call_site.try_as_basic_value().left() {
                    ret_val
                } else {
                    panic!("Method '{}' should return a value but didn't", name);
                }
            } else {
                context.i32_type().const_zero().as_basic_value_enum()
            }
        }

        Expression::AssignOperation { target, operator, value } => {
            let ptr = generate_address_ir(context, builder, target, variables, module);

            let current_val = builder.build_load(ptr, "load_current").unwrap();

            let new_val = generate_expression_ir(context, builder, value, variables, module, Some(current_val.get_type()));

            let (current_val, new_val) = match (current_val, new_val) {
                (BasicValueEnum::FloatValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                    let rhs_casted = builder.build_signed_int_to_float(rhs, lhs.get_type(), "int_to_float").unwrap();
                    (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs_casted))
                }
                (BasicValueEnum::IntValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                    let lhs_casted = builder.build_signed_int_to_float(lhs, rhs.get_type(), "int_to_float").unwrap();
                    (BasicValueEnum::FloatValue(lhs_casted), BasicValueEnum::FloatValue(rhs))
                }
                other => other,
            };

            let result = match (current_val, new_val) {
                (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                    match operator {
                        AssignOperator::Assign => rhs.as_basic_value_enum(),
                        AssignOperator::AddAssign => builder.build_int_add(lhs, rhs, "add_assign").unwrap().as_basic_value_enum(),
                        AssignOperator::SubAssign => builder.build_int_sub(lhs, rhs, "sub_assign").unwrap().as_basic_value_enum(),
                        AssignOperator::MulAssign => builder.build_int_mul(lhs, rhs, "mul_assign").unwrap().as_basic_value_enum(),
                        AssignOperator::DivAssign => builder.build_int_signed_div(lhs, rhs, "div_assign").unwrap().as_basic_value_enum(),
                        AssignOperator::RemAssign => builder.build_int_signed_rem(lhs, rhs, "rem_assign").unwrap().as_basic_value_enum(),
                    }
                }
                (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                    match operator {
                        AssignOperator::Assign => rhs.as_basic_value_enum(),
                        AssignOperator::AddAssign => builder.build_float_add(lhs, rhs, "add_assign").unwrap().as_basic_value_enum(),
                        AssignOperator::SubAssign => builder.build_float_sub(lhs, rhs, "sub_assign").unwrap().as_basic_value_enum(),
                        AssignOperator::MulAssign => builder.build_float_mul(lhs, rhs, "mul_assign").unwrap().as_basic_value_enum(),
                        AssignOperator::DivAssign => builder.build_float_div(lhs, rhs, "div_assign").unwrap().as_basic_value_enum(),
                        AssignOperator::RemAssign => builder.build_float_rem(lhs, rhs, "rem_assign").unwrap().as_basic_value_enum(),
                    }
                }
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
                (BasicValueEnum::FloatValue(val), BasicTypeEnum::IntType(int_ty)) => {
                    builder.build_float_to_signed_int(val, int_ty, "float_to_int").unwrap().as_basic_value_enum()
                }
                (BasicValueEnum::IntValue(val), BasicTypeEnum::FloatType(float_ty)) => {
                    builder.build_signed_int_to_float(val, float_ty, "int_to_float").unwrap().as_basic_value_enum()
                }
                _ => result,
            };

            builder.build_store(ptr, result_casted).unwrap();
            result
        }

        Expression::Assignment { target, value } => {
            let ptr = generate_address_ir(context, builder, target, variables, module); // → PointerValue

            let value = generate_expression_ir(
                context,
                builder,
                value,
                variables,
                module,
                Some(ptr.get_type().get_element_type().try_into().unwrap())
            );

            let value = match value {
                BasicValueEnum::IntValue(v) => v.as_basic_value_enum(),
                BasicValueEnum::FloatValue(v) => v.as_basic_value_enum(),
                BasicValueEnum::PointerValue(v) => v.as_basic_value_enum(),
                _ => panic!("Unsupported assignment value"),
            };

            builder.build_store(ptr, value).unwrap();
            value
        }

        Expression::BinaryExpression { left, operator, right } => {
            let left_val = generate_expression_ir(context, builder, left, variables, module, None);
            let right_val = generate_expression_ir(context, builder, right, variables, module, None);

            // Branch after Type Examination
            match (left_val, right_val) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                    let l_type = l.get_type();
                    let r_type = r.get_type();

                    let (l_casted, r_casted) = if l_type != r_type {
                        if l_type.get_bit_width() < r_type.get_bit_width() {
                            let new_l = builder.build_int_z_extend(l, r_type, "zext_l").unwrap();
                            (new_l, r)
                        } else {
                            let new_r = builder.build_int_z_extend(r, l_type, "zext_r").unwrap();
                            (l, new_r)
                        }
                    } else {
                        (l, r)
                    };

                    let mut result = match operator {
                        Operator::Add => builder.build_int_add(l_casted, r_casted, "addtmp"),
                        Operator::Subtract => builder.build_int_sub(l_casted, r_casted, "subtmp"),
                        Operator::Multiply => builder.build_int_mul(l_casted, r_casted, "multmp"),
                        Operator::Divide => builder.build_int_signed_div(l_casted, r_casted, "divtmp"),
                        Operator::Remainder => builder.build_int_signed_rem(l_casted, r_casted, "modtmp"),
                        Operator::Greater => builder.build_int_compare(IntPredicate::SGT, l_casted, r_casted, "cmptmp"),
                        Operator::Less => builder.build_int_compare(IntPredicate::SLT, l_casted, r_casted, "cmptmp"),
                        Operator::Equal => builder.build_int_compare(IntPredicate::EQ, l_casted, r_casted, "cmptmp"),
                        Operator::NotEqual => builder.build_int_compare(IntPredicate::NE, l_casted, r_casted, "cmptmp"),
                        Operator::GreaterEqual => builder.build_int_compare(IntPredicate::SGE, l_casted, r_casted, "cmptmp"),
                        Operator::LessEqual => builder.build_int_compare(IntPredicate::SLE, l_casted, r_casted, "cmptmp"),
                        _ => panic!("Unsupported binary operator"),
                    }.unwrap();

                    if let Some(BasicTypeEnum::IntType(target_ty)) = expected_type {
                        let result_ty = result.get_type();

                        if result_ty != target_ty {
                            result = builder.build_int_cast(result, target_ty, "cast_result").unwrap();
                        }
                    }

                    result.as_basic_value_enum()
                }

                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                    match operator {
                        Operator::Greater => builder.build_float_compare(FloatPredicate::OGT, l, r, "fcmpgt").unwrap().as_basic_value_enum(),
                        Operator::Less => builder.build_float_compare(FloatPredicate::OLT, l, r, "fcmplt").unwrap().as_basic_value_enum(),
                        Operator::Equal => builder.build_float_compare(FloatPredicate::OEQ, l, r, "fcmpeq").unwrap().as_basic_value_enum(),
                        Operator::NotEqual => builder.build_float_compare(FloatPredicate::ONE, l, r, "fcmpne").unwrap().as_basic_value_enum(),
                        Operator::GreaterEqual => builder.build_float_compare(FloatPredicate::OGE, l, r, "fcmpge").unwrap().as_basic_value_enum(),
                        Operator::LessEqual => builder.build_float_compare(FloatPredicate::OLE, l, r, "fcmple").unwrap().as_basic_value_enum(),
                        Operator::Remainder => builder.build_float_rem(l, r, "modtmp").unwrap().as_basic_value_enum(),
                        _ => panic!("Unsupported float operator"),
                    }
                }

                (BasicValueEnum::IntValue(int_val), BasicValueEnum::FloatValue(float_val)) => {
                    let casted = builder.build_signed_int_to_float(int_val, float_val.get_type(), "cast_lhs").unwrap();
                    match operator {
                        Operator::Add => builder.build_float_add(casted, float_val, "addtmp").unwrap().as_basic_value_enum(),
                        Operator::Subtract => builder.build_float_sub(casted, float_val, "subtmp").unwrap().as_basic_value_enum(),
                        Operator::Multiply => builder.build_float_mul(casted, float_val, "multmp").unwrap().as_basic_value_enum(),
                        Operator::Divide => builder.build_float_div(casted, float_val, "divtmp").unwrap().as_basic_value_enum(),
                        Operator::Remainder => builder.build_float_rem(casted, float_val, "modtmp").unwrap().as_basic_value_enum(),
                        Operator::Greater => builder.build_float_compare(FloatPredicate::OGT, casted, float_val, "fcmpgt").unwrap().as_basic_value_enum(),
                        Operator::Less => builder.build_float_compare(FloatPredicate::OLT, casted, float_val, "fcmplt").unwrap().as_basic_value_enum(),
                        Operator::Equal => builder.build_float_compare(FloatPredicate::OEQ, casted, float_val, "fcmpeq").unwrap().as_basic_value_enum(),
                        Operator::NotEqual => builder.build_float_compare(FloatPredicate::ONE, casted, float_val, "fcmpne").unwrap().as_basic_value_enum(),
                        Operator::GreaterEqual => builder.build_float_compare(FloatPredicate::OGE, casted, float_val, "fcmpge").unwrap().as_basic_value_enum(),
                        Operator::LessEqual => builder.build_float_compare(FloatPredicate::OLE, casted, float_val, "fcmple").unwrap().as_basic_value_enum(),
                        _ => panic!("Unsupported mixed-type operator (int + float)"),
                    }
                }

                (BasicValueEnum::FloatValue(float_val), BasicValueEnum::IntValue(int_val)) => {
                    let casted = builder.build_signed_int_to_float(int_val, float_val.get_type(), "cast_rhs").unwrap();
                    match operator {
                        Operator::Add => builder.build_float_add(float_val, casted, "addtmp").unwrap().as_basic_value_enum(),
                        Operator::Subtract => builder.build_float_sub(float_val, casted, "subtmp").unwrap().as_basic_value_enum(),
                        Operator::Multiply => builder.build_float_mul(float_val, casted, "multmp").unwrap().as_basic_value_enum(),
                        Operator::Divide => builder.build_float_div(float_val, casted, "divtmp").unwrap().as_basic_value_enum(),
                        Operator::Remainder => builder.build_float_rem(float_val, casted, "modtmp").unwrap().as_basic_value_enum(),
                        Operator::Greater => builder.build_float_compare(FloatPredicate::OGT, float_val, casted, "fcmpgt").unwrap().as_basic_value_enum(),
                        Operator::Less => builder.build_float_compare(FloatPredicate::OLT, float_val, casted, "fcmplt").unwrap().as_basic_value_enum(),
                        Operator::Equal => builder.build_float_compare(FloatPredicate::OEQ, float_val, casted, "fcmpeq").unwrap().as_basic_value_enum(),
                        Operator::NotEqual => builder.build_float_compare(FloatPredicate::ONE, float_val, casted, "fcmpne").unwrap().as_basic_value_enum(),
                        Operator::GreaterEqual => builder.build_float_compare(FloatPredicate::OGE, float_val, casted, "fcmpge").unwrap().as_basic_value_enum(),
                        Operator::LessEqual => builder.build_float_compare(FloatPredicate::OLE, float_val, casted, "fcmple").unwrap().as_basic_value_enum(),
                        _ => panic!("Unsupported mixed-type operator (float + int)"),
                    }
                }

                _ => panic!("Type mismatch in binary expression"),
            }
        }

        Expression::IndexAccess { target, index } => unsafe {
            let target_val = generate_expression_ir(context, builder, target, variables, module, None);

            let index_val = generate_expression_ir(context, builder, index, variables, module, None);
            let index_int = match index_val {
                BasicValueEnum::IntValue(i) => i,
                _ => panic!("Index must be an integer"),
            };

            let zero = context.i32_type().const_zero();

            match target_val {
                BasicValueEnum::PointerValue(ptr_val) => {
                    let element_type = ptr_val.get_type().get_element_type();

                    if element_type.is_array_type() {
                        let gep = builder.build_in_bounds_gep(
                            ptr_val,
                            &[zero, index_int],
                            "array_index_gep",
                        ).unwrap();

                        let elem_type = element_type.into_array_type().get_element_type();

                        if elem_type.is_pointer_type() {
                            builder.build_load(gep, "load_ptr_from_array").unwrap().as_basic_value_enum()
                        } else {
                            builder.build_load(gep, "load_array_elem").unwrap().as_basic_value_enum()
                        }
                    } else {
                        let gep = builder.build_in_bounds_gep(
                            ptr_val,
                            &[index_int],
                            "ptr_index_gep",
                        ).unwrap();

                        builder.build_load(gep, "load_ptr_elem").unwrap().as_basic_value_enum()
                    }
                }

                _ => panic!("Unsupported target in IndexAccess"),
            }
        }

        Expression::AsmBlock { instructions, inputs, outputs } => {
            use inkwell::InlineAsmDialect;
            use inkwell::values::{BasicMetadataValueEnum, CallableValue};
            use std::collections::HashSet;

            let asm_code: String = instructions.join("\n");

            let mut operand_vals: Vec<BasicMetadataValueEnum> = vec![];
            let mut constraint_parts: Vec<String> = vec![];

            let input_regs: HashSet<_> = inputs.iter().map(|(reg, _)| reg.to_string()).collect();
            let mut seen_regs: HashSet<String> = HashSet::new();

            for (reg, var) in outputs {
                if input_regs.contains(reg) {
                    panic!("Register '{}' used in both input and output in inline asm", reg);
                }

                if !seen_regs.insert(reg.to_string()) {
                    panic!("Register '{}' duplicated in outputs", reg);
                }

                let info = variables
                    .get(var)
                    .unwrap_or_else(|| panic!("Output variable '{}' not found", var));
                let dummy_val = builder.build_load(info.ptr, var).unwrap().into();
                operand_vals.push(dummy_val);
                constraint_parts.push(format!("={{{}}}", reg));
            }

            for (reg, var) in inputs {
                if !seen_regs.insert(reg.to_string()) {
                    panic!("Register '{}' duplicated in inputs", reg);
                }

                let val: BasicMetadataValueEnum = if let Ok(value) = var.parse::<i64>() {
                    context.i64_type().const_int(value as u64, value < 0).into()
                } else {
                    let info = variables.get(var)
                        .unwrap_or_else(|| panic!("Input variable '{}' not found", var));
                    builder.build_load(info.ptr, var).unwrap().into()
                };

                operand_vals.push(val);
                constraint_parts.push(format!("{{{}}}", reg));
            }

            let constraints_str = constraint_parts.join(",");

            for (reg, _) in outputs {
                constraint_parts.push(format!("={}", reg))
            }

            for (reg, _) in inputs {
                constraint_parts.push(reg.to_string());
            }

            let fn_type = if outputs.is_empty() {
                context.void_type().fn_type(&[], false)
            } else {
                context.i64_type().fn_type(&[], false)
            };

            let inline_asm_ptr = context.create_inline_asm(
                fn_type,
                asm_code,
                constraints_str,
                true,
                false,
                Some(InlineAsmDialect::Intel),
                false,
            );

            let inline_asm_fn = CallableValue::try_from(inline_asm_ptr)
                .expect("Failed to convert inline asm to CallableValue");

            let call = builder
                .build_call(inline_asm_fn, &operand_vals, "inline_asm_expr")
                .unwrap();

            call.try_as_basic_value().left().unwrap()
        }

        _ => unimplemented!("Unsupported expression type"),
    }
}