use super::ExprGenEnv;
use inkwell::values::{BasicMetadataValueEnum, CallableValue};
use inkwell::InlineAsmDialect;
use parser::ast::Expression;
use std::collections::HashSet;

pub(crate) fn gen<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    instructions: &[String],
    inputs: &[(String, Expression)],
    outputs: &[(String, Expression)],
) -> inkwell::values::BasicValueEnum<'ctx> {
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

        if let Some(name) = var.as_identifier() {
            let info = env
                .variables
                .get(name)
                .unwrap_or_else(|| panic!("Output variable '{}' not found", name));
            let dummy_val = env.builder.build_load(info.ptr, name).unwrap().into();
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
            if let Expression::Literal(parser::ast::Literal::Number(n)) = var {
                env.context.i64_type().const_int(*n as u64, true).into()
            } else if let Some(name) = var.as_identifier() {
                if let Some(info) = env.variables.get(name) {
                    env.builder.build_load(info.ptr, name).unwrap().into()
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
        env.context.void_type().fn_type(&[], false)
    } else {
        env.context.i64_type().fn_type(&[], false)
    };

    let inline_asm_ptr = env.context.create_inline_asm(
        fn_type,
        asm_code,
        constraints_str,
        true,
        false,
        Some(InlineAsmDialect::Intel),
        false,
    );

    let inline_asm_fn =
        CallableValue::try_from(inline_asm_ptr).expect("Failed to convert inline asm to CallableValue");

    let call = env
        .builder
        .build_call(inline_asm_fn, &operand_vals, "inline_asm_expr")
        .unwrap();

    call.try_as_basic_value().left().unwrap()
}
