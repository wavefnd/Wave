use crate::llvm_temporary::llvm_codegen::{generate_address_ir, VariableInfo};
use inkwell::context::Context;
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, IntValue};
use inkwell::{FloatPredicate, IntPredicate};
use parser::ast::{ASTNode, AssignOperator, Expression, Literal, Operator, WaveType};
use std::collections::HashMap;
use inkwell::builder::Builder;

pub struct ProtoInfo<'ctx> {
    pub vtable_ty: StructType<'ctx>,
    pub fat_ty: StructType<'ctx>,
    pub methods: Vec<String>,
}

fn to_bool<'ctx>(builder: &Builder<'ctx>, v: IntValue<'ctx>) -> IntValue<'ctx> {
    if v.get_type().get_bit_width() == 1 {
        return v;
    }

    let zero = v.get_type().const_zero();
    builder
        .build_int_compare(IntPredicate::NE, v, zero, "tobool")
        .unwrap()
}


pub fn generate_expression_ir<'ctx>(
    context: &'ctx Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    expr: &Expression,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    module: &'ctx inkwell::module::Module<'ctx>,
    expected_type: Option<BasicTypeEnum<'ctx>>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
) -> BasicValueEnum<'ctx> {
    match expr {
        Expression::Literal(lit) => match lit {
            Literal::Number(v) => match expected_type {
                Some(BasicTypeEnum::IntType(int_ty)) => {
                    int_ty.const_int(*v as u64, false).as_basic_value_enum()
                }
                None => context
                    .i64_type()
                    .const_int(*v as u64, false)
                    .as_basic_value_enum(),
                _ => panic!(
                    "Expected integer type for numeric literal, got {:?}",
                    expected_type
                ),
            },
            Literal::Float(value) => match expected_type {
                Some(BasicTypeEnum::FloatType(float_ty)) => {
                    float_ty.const_float(*value).as_basic_value_enum()
                }
                Some(BasicTypeEnum::IntType(int_ty)) => builder
                    .build_float_to_signed_int(
                        context.f32_type().const_float(*value),
                        int_ty,
                        "f32_to_int",
                    )
                    .unwrap()
                    .as_basic_value_enum(),
                None => context.f32_type().const_float(*value).as_basic_value_enum(),
                _ => panic!("Unsupported expected_type for float"),
            },
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
                let gep = builder
                    .build_gep(global.as_pointer_value(), &indices, "str_gep")
                    .unwrap();

                gep.as_basic_value_enum()
            },
            Literal::Bool(v) => {
                context
                    .bool_type()
                    .const_int(if *v { 1 } else { 0 }, false)
                    .as_basic_value_enum()
            },
            Literal::Char(c) => {
                context
                    .i8_type()
                    .const_int(*c as u64, false)
                    .as_basic_value_enum()
            },
            Literal::Byte(b) => {
                context
                    .i8_type()
                    .const_int(*b as u64, false)
                    .as_basic_value_enum()
            }
            _ => unimplemented!("Unsupported literal type"),
        },

        Expression::Variable(var_name) => {
            if var_name == "true" {
                return context
                    .bool_type()
                    .const_int(1, false)
                    .as_basic_value_enum();
            } else if var_name == "false" {
                return context
                    .bool_type()
                    .const_int(0, false)
                    .as_basic_value_enum();
            }

            if let Some(const_val) = global_consts.get(var_name) {
                return *const_val;
            }

            if let Some(var_info) = variables.get(var_name) {
                let var_type = var_info.ptr.get_type().get_element_type();

                match var_type {
                    AnyTypeEnum::ArrayType(_) => var_info.ptr.as_basic_value_enum(),
                    _ => builder
                        .build_load(var_info.ptr, var_name)
                        .unwrap()
                        .as_basic_value_enum(),
                }
            } else if module.get_function(var_name).is_some() {
                panic!("Error: '{}' is a function name, not a variable", var_name);
            } else {
                panic!("variable '{}' not found in current scope", var_name);
            }
        }

        Expression::Deref(inner_expr) => match &**inner_expr {
            Expression::Variable(var_name) => {
                let ptr_to_value = variables.get(var_name).unwrap().ptr;
                let actual_ptr = builder
                    .build_load(ptr_to_value, "deref_target")
                    .unwrap()
                    .into_pointer_value();
                builder
                    .build_load(actual_ptr, "deref_load")
                    .unwrap()
                    .as_basic_value_enum()
            }
            _ => {
                let ptr_val = generate_expression_ir(
                    context,
                    builder,
                    inner_expr,
                    variables,
                    module,
                    None,
                    global_consts,
                    &struct_types,
                    struct_field_indices,
                );
                let ptr = ptr_val.into_pointer_value();
                builder
                    .build_load(ptr, "deref_load")
                    .unwrap()
                    .as_basic_value_enum()
            }
        },

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
                                global_consts,
                                &struct_types,
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

                        let alloca = builder
                            .build_alloca(tmp_alloca.get_type(), "tmp_array_ptr")
                            .unwrap();
                        builder.build_store(alloca, tmp_alloca).unwrap();
                        alloca.as_basic_value_enum()
                    },

                    Expression::Variable(var_name) => {
                        let ptr = variables
                            .get(var_name)
                            .unwrap_or_else(|| panic!("Variable {} not found", var_name));
                        ptr.ptr.as_basic_value_enum()
                    }

                    _ => panic!("& operator must be used on variable name or array literal"),
                }
            } else {
                panic!("Expected pointer type for AddressOf");
            }
        }

        Expression::MethodCall { object, name, args } => {
            if let Expression::Variable(var_name) = &**object {
                if let Some(var_info) = variables.get(var_name) {
                    if let WaveType::Struct(struct_name) = &var_info.ty {
                        let fn_name = format!("{}_{}", struct_name, name);

                        let function = module
                            .get_function(&fn_name)
                            .unwrap_or_else(|| panic!("Function '{}' not found", fn_name));

                        let fn_type = function.get_type();
                        let param_types = fn_type.get_param_types();

                        let expected_self = param_types.get(0).cloned();

                        let obj_val = generate_expression_ir(
                            context,
                            builder,
                            object,
                            variables,
                            module,
                            expected_self,
                            global_consts,
                            struct_types,
                            struct_field_indices,
                        );

                        let mut call_args: Vec<BasicMetadataValueEnum> = Vec::new();
                        call_args.push(obj_val.into());

                        for (i, arg_expr) in args.iter().enumerate() {
                            let expected_ty = param_types.get(i + 1).cloned();

                            let arg_val = generate_expression_ir(
                                context,
                                builder,
                                arg_expr,
                                variables,
                                module,
                                expected_ty,
                                global_consts,
                                struct_types,
                                struct_field_indices,
                            );
                            call_args.push(arg_val.into());
                        }

                        let call_site = builder
                            .build_call(function, &call_args, &format!("call_{}", fn_name))
                            .unwrap();

                        if function.get_type().get_return_type().is_some() {
                            return call_site
                                .try_as_basic_value()
                                .left()
                                .expect("Expected a return value from struct method");
                        } else {
                            return context.i32_type().const_zero().as_basic_value_enum();
                        }
                    }
                }
            }

            let function = module
                .get_function(name)
                .unwrap_or_else(|| panic!("Function '{}' not found for method-style call", name));

            let fn_type = function.get_type();
            let param_types = fn_type.get_param_types();

            if param_types.is_empty() {
                panic!(
                    "Method-style call {}() requires at least 1 parameter (self)",
                    name
                );
            }

            let expected_self = param_types.get(0).cloned();

            let obj_val = generate_expression_ir(
                context,
                builder,
                object,
                variables,
                module,
                expected_self,
                global_consts,
                struct_types,
                struct_field_indices,
            );

            let mut call_args: Vec<BasicMetadataValueEnum> = Vec::new();
            call_args.push(obj_val.into());

            for (i, arg_expr) in args.iter().enumerate() {
                let expected_ty = param_types.get(i + 1).cloned();

                let arg_val = generate_expression_ir(
                    context,
                    builder,
                    arg_expr,
                    variables,
                    module,
                    expected_ty,
                    global_consts,
                    struct_types,
                    struct_field_indices,
                );
                call_args.push(arg_val.into());
            }

            let call_site = builder
                .build_call(function, &call_args, &format!("call_{}", name))
                .unwrap();

            if function.get_type().get_return_type().is_some() {
                call_site.try_as_basic_value().left().unwrap()
            } else {
                context.i32_type().const_zero().as_basic_value_enum()
            }
        }

        Expression::FunctionCall { name, args } => {
            let function = module
                .get_function(name)
                .unwrap_or_else(|| panic!("Function '{}' not found", name));

            let fn_type = function.get_type();
            let param_types: Vec<BasicTypeEnum> = fn_type.get_param_types();

            let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();

            for (i, arg) in args.iter().enumerate() {
                let expected = param_types.get(i).cloned();

                let val = generate_expression_ir(
                    context,
                    builder,
                    arg,
                    variables,
                    module,
                    expected,
                    global_consts,
                    struct_types,
                    struct_field_indices,
                );
                call_args.push(val.into());
            }

            let call_site = builder
                .build_call(function, &call_args, &format!("call_{}", name))
                .unwrap();

            if function.get_type().get_return_type().is_some() {
                call_site.try_as_basic_value().left().unwrap()
            } else {
                context.i32_type().const_zero().as_basic_value_enum()
            }
        }

        Expression::AssignOperation {
            target,
            operator,
            value,
        } => {
            let ptr = generate_address_ir(context, builder, target, variables, module);

            let current_val = builder.build_load(ptr, "load_current").unwrap();

            let new_val = generate_expression_ir(
                context,
                builder,
                value,
                variables,
                module,
                Some(current_val.get_type()),
                global_consts,
                &struct_types,
                struct_field_indices,
            );

            let (current_val, new_val) = match (current_val, new_val) {
                (BasicValueEnum::FloatValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                    let rhs_casted = builder
                        .build_signed_int_to_float(rhs, lhs.get_type(), "int_to_float")
                        .unwrap();
                    (
                        BasicValueEnum::FloatValue(lhs),
                        BasicValueEnum::FloatValue(rhs_casted),
                    )
                }
                (BasicValueEnum::IntValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                    let lhs_casted = builder
                        .build_signed_int_to_float(lhs, rhs.get_type(), "int_to_float")
                        .unwrap();
                    (
                        BasicValueEnum::FloatValue(lhs_casted),
                        BasicValueEnum::FloatValue(rhs),
                    )
                }
                other => other,
            };

            let result = match (current_val, new_val) {
                (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => match operator {
                    AssignOperator::Assign => rhs.as_basic_value_enum(),
                    AssignOperator::AddAssign => builder
                        .build_int_add(lhs, rhs, "add_assign")
                        .unwrap()
                        .as_basic_value_enum(),
                    AssignOperator::SubAssign => builder
                        .build_int_sub(lhs, rhs, "sub_assign")
                        .unwrap()
                        .as_basic_value_enum(),
                    AssignOperator::MulAssign => builder
                        .build_int_mul(lhs, rhs, "mul_assign")
                        .unwrap()
                        .as_basic_value_enum(),
                    AssignOperator::DivAssign => builder
                        .build_int_signed_div(lhs, rhs, "div_assign")
                        .unwrap()
                        .as_basic_value_enum(),
                    AssignOperator::RemAssign => builder
                        .build_int_signed_rem(lhs, rhs, "rem_assign")
                        .unwrap()
                        .as_basic_value_enum(),
                },
                (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                    match operator {
                        AssignOperator::Assign => rhs.as_basic_value_enum(),
                        AssignOperator::AddAssign => builder
                            .build_float_add(lhs, rhs, "add_assign")
                            .unwrap()
                            .as_basic_value_enum(),
                        AssignOperator::SubAssign => builder
                            .build_float_sub(lhs, rhs, "sub_assign")
                            .unwrap()
                            .as_basic_value_enum(),
                        AssignOperator::MulAssign => builder
                            .build_float_mul(lhs, rhs, "mul_assign")
                            .unwrap()
                            .as_basic_value_enum(),
                        AssignOperator::DivAssign => builder
                            .build_float_div(lhs, rhs, "div_assign")
                            .unwrap()
                            .as_basic_value_enum(),
                        AssignOperator::RemAssign => builder
                            .build_float_rem(lhs, rhs, "rem_assign")
                            .unwrap()
                            .as_basic_value_enum(),
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
                (BasicValueEnum::FloatValue(val), BasicTypeEnum::IntType(int_ty)) => builder
                    .build_float_to_signed_int(val, int_ty, "float_to_int")
                    .unwrap()
                    .as_basic_value_enum(),
                (BasicValueEnum::IntValue(val), BasicTypeEnum::FloatType(float_ty)) => builder
                    .build_signed_int_to_float(val, float_ty, "int_to_float")
                    .unwrap()
                    .as_basic_value_enum(),
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
                Some(ptr.get_type().get_element_type().try_into().unwrap()),
                global_consts,
                &struct_types,
                struct_field_indices,
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

        Expression::BinaryExpression {
            left,
            operator,
            right,
        } => {
            let left_val = generate_expression_ir(
                context,
                builder,
                left,
                variables,
                module,
                None,
                global_consts,
                &struct_types,
                struct_field_indices,
            );
            let right_val = generate_expression_ir(
                context,
                builder,
                right,
                variables,
                module,
                None,
                global_consts,
                &struct_types,
                struct_field_indices,
            );

            // Branch after Type Examination
            match (left_val, right_val) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                    let l_type = l.get_type();
                    let r_type = r.get_type();

                    let (l_casted, r_casted) = match operator {
                        Operator::ShiftLeft | Operator::ShiftRight => {
                            let r2 = if r_type != l_type {
                                builder.build_int_cast(r, l_type, "shamt").unwrap()
                            } else {
                                r
                            };
                            (l, r2)
                        }
                        _ => {
                            if l_type != r_type {
                                if l_type.get_bit_width() < r_type.get_bit_width() {
                                    let new_l = builder.build_int_z_extend(l, r_type, "zext_l").unwrap();
                                    (new_l, r)
                                } else {
                                    let new_r = builder.build_int_z_extend(r, l_type, "zext_r").unwrap();
                                    (l, new_r)
                                }
                            } else {
                                (l, r)
                            }
                        }
                    };

                    let mut result = match operator {
                        Operator::Add => builder.build_int_add(l_casted, r_casted, "addtmp"),
                        Operator::Subtract => builder.build_int_sub(l_casted, r_casted, "subtmp"),
                        Operator::Multiply => builder.build_int_mul(l_casted, r_casted, "multmp"),
                        Operator::Divide => {
                            builder.build_int_signed_div(l_casted, r_casted, "divtmp")
                        }
                        Operator::Remainder => {
                            builder.build_int_signed_rem(l_casted, r_casted, "modtmp")
                        }
                        Operator::ShiftLeft => builder.build_left_shift(l_casted, r_casted, "shl"),
                        Operator::ShiftRight => {
                            builder.build_right_shift(l_casted, r_casted, true, "shr")
                        }
                        Operator::BitwiseAnd => builder.build_and(l_casted, r_casted, "andtmp"),
                        Operator::BitwiseOr  => builder.build_or(l_casted, r_casted, "ortmp"),
                        Operator::BitwiseXor => builder.build_xor(l_casted, r_casted, "xortmp"),
                        Operator::Greater => builder.build_int_compare(
                            IntPredicate::SGT,
                            l_casted,
                            r_casted,
                            "cmptmp",
                        ),
                        Operator::Less => builder.build_int_compare(
                            IntPredicate::SLT,
                            l_casted,
                            r_casted,
                            "cmptmp",
                        ),
                        Operator::Equal => builder.build_int_compare(
                            IntPredicate::EQ,
                            l_casted,
                            r_casted,
                            "cmptmp",
                        ),
                        Operator::NotEqual => builder.build_int_compare(
                            IntPredicate::NE,
                            l_casted,
                            r_casted,
                            "cmptmp",
                        ),
                        Operator::GreaterEqual => builder.build_int_compare(
                            IntPredicate::SGE,
                            l_casted,
                            r_casted,
                            "cmptmp",
                        ),
                        Operator::LessEqual => builder.build_int_compare(
                            IntPredicate::SLE,
                            l_casted,
                            r_casted,
                            "cmptmp",
                        ),
                        Operator::LogicalAnd => {
                            let lb = to_bool(builder, l_casted);
                            let rb = to_bool(builder, r_casted);
                            builder.build_and(lb, rb, "land")
                        }
                        Operator::LogicalOr => {
                            let lb = to_bool(builder, l_casted);
                            let rb = to_bool(builder, r_casted);
                            builder.build_or(lb, rb, "lor")
                        }
                        _ => panic!("Unsupported binary operator"),
                    }
                    .unwrap();

                    if let Some(BasicTypeEnum::IntType(target_ty)) = expected_type {
                        let result_ty = result.get_type();
                        if result_ty != target_ty {
                            result = builder.build_int_cast(result, target_ty, "cast_result").unwrap();
                        }
                    }

                    result.as_basic_value_enum()
                }

                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => match operator {
                    Operator::Greater => builder
                        .build_float_compare(FloatPredicate::OGT, l, r, "fcmpgt")
                        .unwrap()
                        .as_basic_value_enum(),
                    Operator::Less => builder
                        .build_float_compare(FloatPredicate::OLT, l, r, "fcmplt")
                        .unwrap()
                        .as_basic_value_enum(),
                    Operator::Equal => builder
                        .build_float_compare(FloatPredicate::OEQ, l, r, "fcmpeq")
                        .unwrap()
                        .as_basic_value_enum(),
                    Operator::NotEqual => builder
                        .build_float_compare(FloatPredicate::ONE, l, r, "fcmpne")
                        .unwrap()
                        .as_basic_value_enum(),
                    Operator::GreaterEqual => builder
                        .build_float_compare(FloatPredicate::OGE, l, r, "fcmpge")
                        .unwrap()
                        .as_basic_value_enum(),
                    Operator::LessEqual => builder
                        .build_float_compare(FloatPredicate::OLE, l, r, "fcmple")
                        .unwrap()
                        .as_basic_value_enum(),
                    Operator::Remainder => builder
                        .build_float_rem(l, r, "modtmp")
                        .unwrap()
                        .as_basic_value_enum(),
                    _ => panic!("Unsupported float operator"),
                },

                (BasicValueEnum::IntValue(int_val), BasicValueEnum::FloatValue(float_val)) => {
                    let casted = builder
                        .build_signed_int_to_float(int_val, float_val.get_type(), "cast_lhs")
                        .unwrap();
                    match operator {
                        Operator::Add => builder
                            .build_float_add(casted, float_val, "addtmp")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::Subtract => builder
                            .build_float_sub(casted, float_val, "subtmp")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::Multiply => builder
                            .build_float_mul(casted, float_val, "multmp")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::Divide => builder
                            .build_float_div(casted, float_val, "divtmp")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::Remainder => builder
                            .build_float_rem(casted, float_val, "modtmp")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::Greater => builder
                            .build_float_compare(FloatPredicate::OGT, casted, float_val, "fcmpgt")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::Less => builder
                            .build_float_compare(FloatPredicate::OLT, casted, float_val, "fcmplt")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::Equal => builder
                            .build_float_compare(FloatPredicate::OEQ, casted, float_val, "fcmpeq")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::NotEqual => builder
                            .build_float_compare(FloatPredicate::ONE, casted, float_val, "fcmpne")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::GreaterEqual => builder
                            .build_float_compare(FloatPredicate::OGE, casted, float_val, "fcmpge")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::LessEqual => builder
                            .build_float_compare(FloatPredicate::OLE, casted, float_val, "fcmple")
                            .unwrap()
                            .as_basic_value_enum(),
                        _ => panic!("Unsupported mixed-type operator (int + float)"),
                    }
                }

                (BasicValueEnum::FloatValue(float_val), BasicValueEnum::IntValue(int_val)) => {
                    let casted = builder
                        .build_signed_int_to_float(int_val, float_val.get_type(), "cast_rhs")
                        .unwrap();
                    match operator {
                        Operator::Add => builder
                            .build_float_add(float_val, casted, "addtmp")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::Subtract => builder
                            .build_float_sub(float_val, casted, "subtmp")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::Multiply => builder
                            .build_float_mul(float_val, casted, "multmp")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::Divide => builder
                            .build_float_div(float_val, casted, "divtmp")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::Remainder => builder
                            .build_float_rem(float_val, casted, "modtmp")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::Greater => builder
                            .build_float_compare(FloatPredicate::OGT, float_val, casted, "fcmpgt")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::Less => builder
                            .build_float_compare(FloatPredicate::OLT, float_val, casted, "fcmplt")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::Equal => builder
                            .build_float_compare(FloatPredicate::OEQ, float_val, casted, "fcmpeq")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::NotEqual => builder
                            .build_float_compare(FloatPredicate::ONE, float_val, casted, "fcmpne")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::GreaterEqual => builder
                            .build_float_compare(FloatPredicate::OGE, float_val, casted, "fcmpge")
                            .unwrap()
                            .as_basic_value_enum(),
                        Operator::LessEqual => builder
                            .build_float_compare(FloatPredicate::OLE, float_val, casted, "fcmple")
                            .unwrap()
                            .as_basic_value_enum(),
                        _ => panic!("Unsupported mixed-type operator (float + int)"),
                    }
                }

                _ => panic!("Type mismatch in binary expression"),
            }
        }

        Expression::IndexAccess { target, index } => unsafe {
            let target_val = generate_expression_ir(
                context,
                builder,
                target,
                variables,
                module,
                None,
                global_consts,
                &struct_types,
                struct_field_indices,
            );

            let index_val = generate_expression_ir(
                context,
                builder,
                index,
                variables,
                module,
                None,
                global_consts,
                &struct_types,
                struct_field_indices,
            );
            let index_int = match index_val {
                BasicValueEnum::IntValue(i) => i,
                _ => panic!("Index must be an integer"),
            };

            let zero = context.i32_type().const_zero();

            match target_val {
                BasicValueEnum::PointerValue(ptr_val) => {
                    let element_type = ptr_val.get_type().get_element_type();

                    if element_type.is_array_type() {
                        let gep = builder
                            .build_in_bounds_gep(ptr_val, &[zero, index_int], "array_index_gep")
                            .unwrap();

                        let elem_type = element_type.into_array_type().get_element_type();

                        if elem_type.is_pointer_type() {
                            builder
                                .build_load(gep, "load_ptr_from_array")
                                .unwrap()
                                .as_basic_value_enum()
                        } else {
                            builder
                                .build_load(gep, "load_array_elem")
                                .unwrap()
                                .as_basic_value_enum()
                        }
                    } else {
                        let gep = builder
                            .build_in_bounds_gep(ptr_val, &[index_int], "ptr_index_gep")
                            .unwrap();

                        builder
                            .build_load(gep, "load_ptr_elem")
                            .unwrap()
                            .as_basic_value_enum()
                    }
                }

                _ => panic!("Unsupported target in IndexAccess"),
            }
        },

        Expression::AsmBlock {
            instructions,
            inputs,
            outputs,
        } => {
            use inkwell::values::{BasicMetadataValueEnum, CallableValue};
            use inkwell::InlineAsmDialect;
            use std::collections::HashSet;

            let asm_code: String = instructions.join("\n");

            let mut operand_vals: Vec<BasicMetadataValueEnum> = vec![];
            let mut constraint_parts: Vec<String> = vec![];

            let input_regs: HashSet<_> = inputs.iter().map(|(reg, _)| reg.to_string()).collect();
            let mut seen_regs: HashSet<String> = HashSet::new();

            for (reg, var) in outputs {
                if input_regs.contains(reg) {
                    panic!(
                        "Register '{}' used in both input and output in inline asm",
                        reg
                    );
                }

                if !seen_regs.insert(reg.to_string()) {
                    panic!("Register '{}' duplicated in outputs", reg);
                }

                if let Some(name) = var.as_identifier() {
                    let info = variables
                        .get(name)
                        .unwrap_or_else(|| panic!("Output variable '{}' not found", name));
                    let dummy_val = builder.build_load(info.ptr, name).unwrap().into();
                    operand_vals.push(dummy_val);
                    constraint_parts.push(format!("={{{}}}", reg));
                } else {
                    panic!("Unsupported asm output: {:?}", var);
                }
            }

            for (reg, var) in inputs {
                if !seen_regs.insert(reg.to_string()) {
                    panic!("Register '{}' duplicated in inputs", reg);
                }

                let val: BasicMetadataValueEnum =
                    if let Expression::Literal(Literal::Number(n)) = var {
                        context.i64_type().const_int(*n as u64, true).into()
                    } else if let Some(name) = var.as_identifier() {
                        if let Some(info) = variables.get(name) {
                            builder.build_load(info.ptr, name).unwrap().into()
                        } else {
                            panic!("Input variable '{}' not found", name);
                        }
                    } else {
                        panic!("Unsupported expression in variable context: {:?}", var);
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

        Expression::StructLiteral { name, fields } => {
            let struct_ty = *struct_types
                .get(name)
                .unwrap_or_else(|| panic!("Struct type '{}' not found", name));

            let field_indices = struct_field_indices
                .get(name)
                .unwrap_or_else(|| panic!("Field index map for struct '{}' not found", name));

            let tmp_alloca = builder
                .build_alloca(struct_ty, &format!("tmp_{}_literal", name))
                .unwrap();

            for (field_name, field_expr) in fields {
                let idx = field_indices.get(field_name).unwrap_or_else(|| {
                    panic!("Field '{}' not found in struct '{}'", field_name, name)
                });

                let field_val = generate_expression_ir(
                    context,
                    builder,
                    field_expr,
                    variables,
                    module,
                    None,
                    global_consts,
                    struct_types,
                    struct_field_indices,
                );

                let field_ptr = builder
                    .build_struct_gep(tmp_alloca, *idx, &format!("{}.{}", name, field_name))
                    .unwrap();

                builder.build_store(field_ptr, field_val).unwrap();
            }

            builder
                .build_load(tmp_alloca, &format!("{}_literal_val", name))
                .unwrap()
                .as_basic_value_enum()
        }

        Expression::FieldAccess { object, field } => {
            let var_name = match &**object {
                Expression::Variable(name) => name,
                other => panic!(
                    "FieldAccess on non-variable object not supported yet: {:?}",
                    other
                ),
            };

            let var_info = variables
                .get(var_name)
                .unwrap_or_else(|| panic!("Variable '{}' not found for field access", var_name));

            let struct_name = match &var_info.ty {
                WaveType::Struct(name) => name,
                other_ty => panic!(
                    "Field access on non-struct type {:?} for variable '{}'",
                    other_ty, var_name
                ),
            };

            let field_indices = struct_field_indices.get(struct_name).unwrap_or_else(|| {
                panic!("Field index map for struct '{}' not found", struct_name)
            });

            let idx = field_indices.get(field).unwrap_or_else(|| {
                panic!("Field '{}' not found in struct '{}'", field, struct_name)
            });

            let field_ptr = builder
                .build_struct_gep(var_info.ptr, *idx, &format!("{}.{}", var_name, field))
                .unwrap();

            builder
                .build_load(field_ptr, &format!("load_{}_{}", var_name, field))
                .unwrap()
                .as_basic_value_enum()
        }

        Expression::Unary { operator, expr } => {
            let val = generate_expression_ir(
                context,
                builder,
                expr,
                variables,
                module,
                None,
                global_consts,
                struct_types,
                struct_field_indices,
            );

            match (operator, val) {
                // ! (logical not)
                (Operator::LogicalNot, BasicValueEnum::IntValue(iv))
                | (Operator::Not,       BasicValueEnum::IntValue(iv)) => {
                    let bw = iv.get_type().get_bit_width();
                    if bw == 1 {
                        builder.build_not(iv, "lnot").unwrap().as_basic_value_enum()
                    } else {
                        let zero = iv.get_type().const_zero();
                        builder
                            .build_int_compare(IntPredicate::EQ, iv, zero, "lnot")
                            .unwrap()
                            .as_basic_value_enum()
                    }
                }

                // ~ (bitwise not)
                (Operator::BitwiseNot, BasicValueEnum::IntValue(iv)) => {
                    builder.build_not(iv, "bnot").unwrap().as_basic_value_enum()
                }

                _ => panic!(
                    "Unsupported unary operator {:?} for value {:?}",
                    operator, val
                ),
            }
        }

        Expression::Grouped(inner) => {
            generate_expression_ir(
                context,
                builder,
                inner,
                variables,
                module,
                expected_type,
                global_consts,
                struct_types,
                struct_field_indices,
            )
        }
        
        _ => unimplemented!("Unsupported expression type"),
    }
}
