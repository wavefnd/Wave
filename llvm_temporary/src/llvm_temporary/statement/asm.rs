use crate::llvm_temporary::llvm_codegen::VariableInfo;
use inkwell::module::Module;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, CallableValue};
use inkwell::{AddressSpace, InlineAsmDialect};
use parser::ast::{Expression, Literal, WaveType};
use std::collections::{HashMap, HashSet};
use inkwell::types::{BasicType, StringRadix};

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
    let mut operand_vals: Vec<BasicMetadataValueEnum> = vec![];
    let mut constraint_parts: Vec<String> = vec![];

    let input_regs: HashSet<_> = inputs.iter().map(|(reg, _)| reg.to_string()).collect();
    let mut seen_regs: HashSet<String> = HashSet::new();

    for (reg, _) in outputs {
        if input_regs.contains(reg) && reg != "rax" {
            panic!("Register '{}' used in both input and output", reg);
        }
        if !seen_regs.insert(reg.to_string()) {
            panic!("Register '{}' duplicated in outputs", reg);
        }
        constraint_parts.push(format!("={{{}}}", reg));
    }

    for (reg, expr) in inputs {
        if !seen_regs.insert(reg.to_string()) {
            if reg != "rax" {
                panic!("Register '{}' duplicated in inputs", reg);
            }
        }

        let val = asm_operand_to_value(context, builder, variables, global_consts, expr);
        operand_vals.push(val);
        constraint_parts.push(format!("{{{}}}", reg));
    }

    let constraints_str = constraint_parts.join(",");

    let (fn_type, out_kinds): (inkwell::types::FunctionType<'ctx>, Vec<WaveType>) = if outputs.is_empty() {
        (context.void_type().fn_type(&[], false), vec![])
    } else {
        let mut tys = Vec::new();
        let mut wave_tys = Vec::new();

        for (_, target_expr) in outputs {
            let var_name = asm_output_target(target_expr);
            let info = variables
                .get(var_name)
                .unwrap_or_else(|| panic!("Output variable '{}' not found", var_name));

            wave_tys.push(info.ty.clone());

            let bt = match &info.ty {
                WaveType::Int(64) => context.i64_type().as_basic_type_enum(),
                WaveType::Pointer(inner) => match **inner {
                    WaveType::Int(8) => context.i8_type().ptr_type(AddressSpace::from(0)).as_basic_type_enum(),
                    _ => panic!("Unsupported pointer inner type in asm output"),
                },
                _ => panic!("Unsupported asm output type: {:?}", info.ty),
            };
            tys.push(bt);
        }

        if tys.len() == 1 {
            (tys[0].fn_type(&[], false), wave_tys)
        } else {
            let st = context.struct_type(&tys, false);
            (st.fn_type(&[], false), wave_tys)
        }
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
    match &info.ty {
        WaveType::Int(64) => {
            builder.build_store(info.ptr, value).unwrap();
        }
        WaveType::Pointer(inner) => match **inner {
            WaveType::Int(8) => {
                if value.is_pointer_value() {
                    builder.build_store(info.ptr, value.into_pointer_value()).unwrap();
                    return;
                }

                let casted_ptr = builder
                    .build_int_to_ptr(
                        value.into_int_value(),
                        context.i8_type().ptr_type(AddressSpace::from(0)),
                        "casted_ptr",
                    )
                    .unwrap();
                builder.build_store(info.ptr, casted_ptr).unwrap();
            }
            _ => panic!("Unsupported pointer inner type in inline asm output"),
        },
        _ => panic!("Unsupported return type from inline asm output var '{}': {:?}", var_name, info.ty),
    }
}

fn asm_operand_to_value<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    variables: &HashMap<String, VariableInfo<'ctx>>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    expr: &Expression,
) -> BasicMetadataValueEnum<'ctx> {
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

            iv.into()
        }


        Expression::Variable(name) => {
            if let Some(const_val) = global_consts.get(name) {
                (*const_val).into()
            } else {
                let info = variables
                    .get(name)
                    .unwrap_or_else(|| panic!("Input variable '{}' not found", name));
                builder.build_load(info.ptr, name).unwrap().into()
            }
        }

        Expression::AddressOf(inner) => {
            match inner.as_ref() {
                Expression::Variable(name) => {
                    let info = variables
                        .get(name)
                        .unwrap_or_else(|| panic!("Input variable '{}' not found", name));
                    builder
                        .build_bit_cast(
                            info.ptr,
                            context.i8_type().ptr_type(AddressSpace::from(0)),
                            "addr_ptr",
                        )
                        .unwrap()
                        .into()
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
