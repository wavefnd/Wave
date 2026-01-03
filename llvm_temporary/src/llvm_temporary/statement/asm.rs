use crate::llvm_temporary::llvm_codegen::VariableInfo;
use inkwell::module::Module;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, CallableValue};
use inkwell::{AddressSpace, InlineAsmDialect};
use parser::ast::WaveType;
use std::collections::{HashMap, HashSet};

pub(super) fn gen_asm_stmt_ir<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    instructions: &[String],
    inputs: &[(String, String)],
    outputs: &[(String, String)],
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

    for (reg, var) in inputs {
        if !seen_regs.insert(reg.to_string()) {
            if reg != "rax" {
                panic!("Register '{}' duplicated in inputs", reg);
            }
        }

        let clean_var = if var.starts_with('&') { &var[1..] } else { var.as_str() };

        let val: BasicMetadataValueEnum = if let Ok(value) = var.parse::<i64>() {
            context.i64_type().const_int(value as u64, value < 0).into()
        } else if let Some(const_val) = global_consts.get(var) {
            (*const_val).into()
        } else {
            let info = variables
                .get(clean_var)
                .unwrap_or_else(|| panic!("Input variable '{}' not found", clean_var));

            if var.starts_with('&') {
                builder
                    .build_bit_cast(
                        info.ptr,
                        context.i8_type().ptr_type(AddressSpace::from(0)),
                        "addr_ptr",
                    )
                    .unwrap()
                    .into()
            } else {
                builder.build_load(info.ptr, var).unwrap().into()
            }
        };

        operand_vals.push(val);
        constraint_parts.push(format!("{{{}}}", reg));
    }

    let constraints_str = constraint_parts.join(",");

    let (fn_type, expects_return) = if !outputs.is_empty() {
        (context.i64_type().fn_type(&[], false), true)
    } else {
        (context.void_type().fn_type(&[], false), false)
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

    if expects_return {
        let ret_val = call.try_as_basic_value().left().unwrap();
        let (_, var) = outputs.iter().next().unwrap();
        let info = variables
            .get(var)
            .unwrap_or_else(|| panic!("Output variable '{}' not found", var));

        match &info.ty {
            WaveType::Int(64) => {
                builder.build_store(info.ptr, ret_val).unwrap();
            }
            WaveType::Pointer(inner) => match **inner {
                WaveType::Int(8) => {
                    let casted_ptr = builder
                        .build_int_to_ptr(
                            ret_val.into_int_value(),
                            context.i8_type().ptr_type(AddressSpace::from(0)),
                            "casted_ptr",
                        )
                        .unwrap();
                    builder.build_store(info.ptr, casted_ptr).unwrap();
                }
                _ => panic!("Unsupported pointer inner type in inline asm output"),
            },
            _ => panic!("Unsupported return type from inline asm: {:?}", info.ty),
        }
    }
}
