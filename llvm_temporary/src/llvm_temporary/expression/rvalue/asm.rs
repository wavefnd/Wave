use super::ExprGenEnv;
use inkwell::types::{BasicMetadataTypeEnum, StringRadix};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallableValue};
use inkwell::InlineAsmDialect;
use parser::ast::{Expression, Literal};
use std::collections::HashSet;

pub(crate) fn gen<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    instructions: &[String],
    inputs: &[(String, Expression)],
    outputs: &[(String, Expression)],
) -> BasicValueEnum<'ctx> {
    let asm_code: String = instructions.join("\n");

    if outputs.len() > 1 {
        panic!("asm expression supports at most 1 output for now (got {})", outputs.len());
    }

    let mut constraint_parts: Vec<String> = vec![];
    let mut seen_regs: HashSet<String> = HashSet::new();

    if let Some((out_reg, _out_expr)) = outputs.first() {
        if !seen_regs.insert(out_reg.to_string()) {
            panic!("Register '{}' duplicated in outputs", out_reg);
        }
        // output constraint
        constraint_parts.push(format!("={{{}}}", out_reg));
    }

    // inputs: operand + constraint
    let mut operand_vals: Vec<BasicMetadataValueEnum<'ctx>> = Vec::with_capacity(inputs.len());

    for (reg, var) in inputs {
        if !seen_regs.insert(reg.to_string()) {
            panic!("Register '{}' duplicated in asm operands", reg);
        }

        let val: BasicMetadataValueEnum<'ctx> = match var {
            Expression::Literal(Literal::Int(n)) => {
                let s = n.as_str();

                let (neg, digits) = if let Some(rest) = s.strip_prefix('-') {
                    (true, rest)
                } else {
                    (false, s)
                };

                let ty = env.context.i64_type();

                let mut iv = ty
                    .const_int_from_string(digits, StringRadix::Decimal)
                    .unwrap_or_else(|| panic!("invalid int literal: {}", s));

                if neg {
                    iv = iv.const_neg();
                }

                iv.as_basic_value_enum().into()
            }
            Expression::Literal(Literal::Float(_)) => {
                panic!("float literal in asm input not supported yet");
            }
            _ => {
                if let Some(name) = var.as_identifier() {
                    let info = env
                        .variables
                        .get(name)
                        .unwrap_or_else(|| panic!("Input variable '{}' not found", name));
                    env.builder.build_load(info.ptr, name).unwrap().into()
                } else {
                    panic!("Unsupported asm input expr: {:?}", var);
                }
            }
        };

        operand_vals.push(val);
        constraint_parts.push(format!("{{{}}}", reg));
    }

    let constraints_str = constraint_parts.join(",");

    let param_types: Vec<BasicMetadataTypeEnum<'ctx>> =
        operand_vals.iter().map(meta_val_type).collect();

    let fn_type = if outputs.is_empty() {
        env.context.void_type().fn_type(&param_types, false)
    } else {
        env.context.i64_type().fn_type(&param_types, false)
    };

    let inline_asm = env.context.create_inline_asm(
        fn_type,
        asm_code,
        constraints_str,
        true,  // sideeffect
        false, // alignstack
        Some(InlineAsmDialect::Intel),
        false,
    );

    let callable =
        CallableValue::try_from(inline_asm).expect("Failed to convert inline asm to CallableValue");

    let call = env
        .builder
        .build_call(callable, &operand_vals, "inline_asm_expr")
        .unwrap();

    if outputs.is_empty() {
        panic!("asm expression must have an output");
    }

    call.try_as_basic_value().left().unwrap()
}

fn meta_val_type<'ctx>(v: &BasicMetadataValueEnum<'ctx>) -> BasicMetadataTypeEnum<'ctx> {
    match v {
        BasicMetadataValueEnum::IntValue(iv) => iv.get_type().into(),
        BasicMetadataValueEnum::FloatValue(fv) => fv.get_type().into(),
        BasicMetadataValueEnum::PointerValue(pv) => pv.get_type().into(),
        BasicMetadataValueEnum::StructValue(sv) => sv.get_type().into(),
        BasicMetadataValueEnum::VectorValue(vv) => vv.get_type().into(),
        BasicMetadataValueEnum::ArrayValue(av) => av.get_type().into(),

        BasicMetadataValueEnum::MetadataValue(_) => {
            panic!("MetadataValue cannot be used as an inline asm operand");
        }
    }
}