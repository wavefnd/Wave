use crate::llvm_temporary::llvm_codegen::VariableInfo;
use inkwell::module::Module;
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallableValue};
use inkwell::{InlineAsmDialect};
use parser::ast::{Expression, Literal};
use std::collections::{HashMap, HashSet};
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StringRadix};

pub(super) fn gen_asm_stmt_ir<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    instructions: &[String],
    inputs: &[(String, Expression)],
    outputs: &[(String, Expression)],
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
) {
    let asm_code: String = instructions.join("\n");

    let mut operand_vals: Vec<BasicMetadataValueEnum<'ctx>> = vec![];
    let mut param_types: Vec<BasicMetadataTypeEnum<'ctx>> = vec![];

    let mut constraint_parts: Vec<String> = vec![];

    let mut seen_out_regs: HashSet<String> = HashSet::new();
    for (reg, _) in outputs {
        if !seen_out_regs.insert(reg.to_string()) {
            panic!("Register '{}' duplicated in outputs", reg);
        }
        constraint_parts.push(format!("={{{}}}", reg));
    }

    let mut seen_in_regs: HashSet<String> = HashSet::new();
    for (reg, expr) in inputs {
        if !seen_in_regs.insert(reg.to_string()) {
            panic!("Register '{}' duplicated in inputs", reg);
        }

        let val = asm_operand_to_value(context, builder, variables, global_consts, expr);
        param_types.push(val.get_type().into());
        operand_vals.push(val.into());

        constraint_parts.push(format!("{{{}}}", reg));
    }

    let constraints_str = constraint_parts.join(",");

    let fn_type = if outputs.is_empty() {
        context.void_type().fn_type(&param_types, false)
    } else {
        let mut tys: Vec<BasicTypeEnum<'ctx>> = Vec::new();

        for (_, target_expr) in outputs {
            let var_name = asm_output_target(target_expr);
            let info = variables
                .get(var_name)
                .unwrap_or_else(|| panic!("Output variable '{}' not found", var_name));

            // var alloca pointer element type == var's LLVM type
            let elem_any = info.ptr.get_type().get_element_type(); // AnyTypeEnum
            let elem_ty = any_to_basic_type(elem_any, "asm output var element type");
            tys.push(elem_ty);
        }

        if tys.len() == 1 {
            tys[0].fn_type(&param_types, false)
        } else {
            let st = context.struct_type(&tys, false);
            st.fn_type(&param_types, false)
        }
    };

    let inline_asm_ptr = context.create_inline_asm(
        fn_type,
        asm_code,
        constraints_str,
        true,  // has_side_effects
        false, // is_align_stack
        Some(InlineAsmDialect::Intel),
        false, // can_throw
    );

    let inline_asm_fn =
        CallableValue::try_from(inline_asm_ptr).expect("Failed to convert inline asm");

    let call = builder
        .build_call(inline_asm_fn, &operand_vals, "inline_asm")
        .unwrap();

    if outputs.is_empty() {
        return;
    }

    let ret_val = call.try_as_basic_value().left().unwrap();

    if outputs.len() == 1 {
        let (_, target_expr) = &outputs[0];
        let var_name = asm_output_target(target_expr);
        let info = variables.get(var_name).unwrap();

        store_asm_output(context, builder, info, ret_val, var_name);
        return;
    }

    let struct_val = ret_val.into_struct_value();
    for (idx, (_, target_expr)) in outputs.iter().enumerate() {
        let out_elem = builder
            .build_extract_value(struct_val, idx as u32, "asm_out")
            .unwrap();

        let var_name = asm_output_target(target_expr);
        let info = variables.get(var_name).unwrap();

        store_asm_output(context, builder, info, out_elem, var_name);
    }
}

fn asm_output_target<'a>(expr: &'a Expression) -> &'a str {
    match expr {
        Expression::Variable(name) => name.as_str(),
        _ => panic!("out(...) target must be a variable for now: {:?}", expr),
    }
}

fn store_asm_output<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    info: &VariableInfo<'ctx>,
    value: BasicValueEnum<'ctx>,
    var_name: &str,
) {
    let dst_any = info.ptr.get_type().get_element_type(); // AnyTypeEnum
    let dst_ty = any_to_basic_type(dst_any, "asm output dst type");
    let v = coerce_basic_value_for_store(context, builder, value, dst_ty, var_name);
    builder.build_store(info.ptr, v).unwrap();
}

fn coerce_basic_value_for_store<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    value: BasicValueEnum<'ctx>,
    dst_ty: BasicTypeEnum<'ctx>,
    name: &str,
) -> BasicValueEnum<'ctx> {
    if value.get_type() == dst_ty {
        return value;
    }

    // pointer <- pointer/int
    if dst_ty.is_pointer_type() {
        let dst_ptr = dst_ty.into_pointer_type();

        if value.is_pointer_value() {
            return builder
                .build_bit_cast(value.into_pointer_value(), dst_ptr, "asm_ptr_cast")
                .unwrap()
                .as_basic_value_enum();
        }

        if value.is_int_value() {
            return builder
                .build_int_to_ptr(value.into_int_value(), dst_ptr, "asm_int_to_ptr")
                .unwrap()
                .as_basic_value_enum();
        }

        panic!(
            "Cannot coerce asm output '{}' from {:?} to pointer {:?}",
            name,
            value.get_type(),
            dst_ty
        );
    }

    // int <- int/pointer/float
    if dst_ty.is_float_type() {
        let dst_float = dst_ty.into_float_type();

        if value.is_float_value() {
            let v = value.into_float_value();

            if let Ok(tr) = builder.build_float_trunc(v, dst_float, "asm_fptrunc") {
                return tr.as_basic_value_enum();
            }
            if let Ok(ex) = builder.build_float_ext(v, dst_float, "asm_fpext") {
                return ex.as_basic_value_enum();
            }

            panic!(
                "Cannot coerce asm output '{}' from {:?} to float {:?}",
                name,
                value.get_type(),
                dst_ty
            );
        }

        if value.is_int_value() {
            return builder
                .build_signed_int_to_float(value.into_int_value(), dst_float, "asm_sitofp")
                .unwrap()
                .as_basic_value_enum();
        }

        panic!(
            "Cannot coerce asm output '{}' from {:?} to float {:?}",
            name,
            value.get_type(),
            dst_ty
        );
    }

    // float <- float/int
    if dst_ty.is_float_type() {
        let dst_float = dst_ty.into_float_type();

        if value.is_float_value() {
            let v = value.into_float_value();

            if let Ok(tr) = builder.build_float_trunc(v, dst_float, "asm_fptrunc") {
                return tr.as_basic_value_enum();
            }
            if let Ok(ex) = builder.build_float_ext(v, dst_float, "asm_fpext") {
                return ex.as_basic_value_enum();
            }

            panic!(
                "Cannot coerce asm output '{}' from {:?} to float {:?}",
                name,
                value.get_type(),
                dst_ty
            );
        }

        if value.is_int_value() {
            return builder
                .build_signed_int_to_float(value.into_int_value(), dst_float, "asm_sitofp")
                .unwrap()
                .as_basic_value_enum();
        }

        panic!(
            "Cannot coerce asm output '{}' from {:?} to float {:?}",
            name,
            value.get_type(),
            dst_ty
        );
    }

    panic!(
        "Unsupported destination type for asm output '{}': {:?}",
        name,
        dst_ty
    );
}

fn asm_operand_to_value<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    variables: &HashMap<String, VariableInfo<'ctx>>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    expr: &Expression,
) -> BasicValueEnum<'ctx> {
    match expr {
        Expression::Literal(Literal::Int(n)) => {
            let s = n.as_str();
            let (neg, digits) = if let Some(rest) = s.strip_prefix('-') {
                (true, rest)
            } else {
                (false, s)
            };

            let mut iv = context
                .i64_type()
                .const_int_from_string(digits, StringRadix::Decimal)
                .unwrap_or_else(|| panic!("invalid int literal: {}", s));

            if neg {
                iv = iv.const_neg();
            }

            iv.as_basic_value_enum()
        }

        Expression::Variable(name) => {
            if let Some(const_val) = global_consts.get(name) {
                *const_val
            } else {
                let info = variables
                    .get(name)
                    .unwrap_or_else(|| panic!("Input variable '{}' not found", name));
                builder.build_load(info.ptr, name).unwrap()
            }
        }

        Expression::AddressOf(inner) => {
            match inner.as_ref() {
                Expression::Variable(name) => {
                    let info = variables
                        .get(name)
                        .unwrap_or_else(|| panic!("Input variable '{}' not found", name));
                    info.ptr.as_basic_value_enum()
                }
                _ => panic!("Unsupported asm address-of operand: {:?}", inner),
            }
        }

        Expression::Grouped(inner) => {
            asm_operand_to_value(context, builder, variables, global_consts, inner)
        }

        _ => panic!("Unsupported asm operand expression: {:?}", expr),
    }
}

fn any_to_basic_type<'ctx>(ty: AnyTypeEnum<'ctx>, what: &str) -> BasicTypeEnum<'ctx> {
    match ty {
        AnyTypeEnum::IntType(t) => t.as_basic_type_enum(),
        AnyTypeEnum::FloatType(t) => t.as_basic_type_enum(),
        AnyTypeEnum::PointerType(t) => t.as_basic_type_enum(),
        AnyTypeEnum::StructType(t) => t.as_basic_type_enum(),
        AnyTypeEnum::ArrayType(t) => t.as_basic_type_enum(),
        AnyTypeEnum::VectorType(t) => t.as_basic_type_enum(),

        other => panic!("{}: expected basic type, got {:?}", what, other),
    }
}