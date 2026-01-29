use super::ExprGenEnv;
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StringRadix};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallableValue};
use inkwell::InlineAsmDialect;
use parser::ast::{Expression, Literal};
use crate::llvm_temporary::llvm_codegen::plan::*;

pub(crate) fn gen<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    instructions: &[String],
    inputs: &[(String, Expression)],
    outputs: &[(String, Expression)],
    clobbers: &[String],
) -> BasicValueEnum<'ctx> {
    let plan = AsmPlan::build(instructions, inputs, outputs, clobbers, AsmSafetyMode::ConservativeKernel);
    let constraints_str = plan.constraints_string();

    let mut operand_vals: Vec<BasicMetadataValueEnum<'ctx>> = Vec::with_capacity(plan.inputs.len());
    for inp in &plan.inputs {
        let v = eval_asm_in_expr(env, inp.value);
        operand_vals.push(v.into());
    }

    let param_types: Vec<BasicMetadataTypeEnum<'ctx>> =
        operand_vals.iter().map(meta_val_type).collect();

    if plan.outputs.is_empty() {
        let fn_type = env.context.void_type().fn_type(&param_types, false);

        let inline_asm = env.context.create_inline_asm(
            fn_type,
            plan.asm_code.clone(),     // inkwell 0.5.0 -> String
            constraints_str,           // String
            plan.has_side_effects,
            false,
            Some(InlineAsmDialect::Intel),
            false,
        );

        let callable =
            CallableValue::try_from(inline_asm).expect("Failed to convert inline asm");

        env.builder
            .build_call(callable, &operand_vals, "inline_asm_void")
            .unwrap();

        return env.context.i64_type().const_int(0, false).as_basic_value_enum();
    }

    if plan.outputs.len() != 1 {
        panic!("asm expression requires exactly 1 output (got {})", plan.outputs.len());
    }

    let out_ty = resolve_expr_out_type(env, plan.outputs[0].target);
    let fn_type = out_ty.fn_type(&param_types, false);

    let inline_asm = env.context.create_inline_asm(
        fn_type,
        plan.asm_code.clone(),
        constraints_str,
        plan.has_side_effects,
        false,
        Some(InlineAsmDialect::Intel),
        false,
    );

    let callable =
        CallableValue::try_from(inline_asm).expect("Failed to convert inline asm");

    let call = env
        .builder
        .build_call(callable, &operand_vals, "inline_asm_expr")
        .unwrap();

    call.try_as_basic_value().left().unwrap()
}

fn resolve_expr_out_type<'ctx, 'a>(
    env: &ExprGenEnv<'ctx, 'a>,
    target: &Expression,
) -> BasicTypeEnum<'ctx> {
    match target {
        Expression::Variable(name) => {
            let info = env.variables.get(name).unwrap_or_else(|| panic!("Output var '{}' not found", name));
            let elem_any = info.ptr.get_type().get_element_type();
            any_to_basic_type(elem_any, "asm expr output var element type")
        }
        Expression::Deref(inner) => {
            match inner.as_ref() {
                Expression::Variable(name) => {
                    let info = env.variables.get(name).unwrap_or_else(|| panic!("Pointer var '{}' not found", name));
                    let loaded = env.builder.build_load(info.ptr, "asm_expr_out_ptr").unwrap();
                    if !loaded.is_pointer_value() {
                        panic!("deref output '{}' is not a pointer", name);
                    }
                    let p = loaded.into_pointer_value();
                    let elem_any = p.get_type().get_element_type();
                    any_to_basic_type(elem_any, "asm expr deref output element type")
                }
                other => panic!("Unsupported expr deref output: {:?}", other),
            }
        }
        other => panic!("asm expr out(...) target must be variable/deref var for now: {:?}", other),
    }
}

fn eval_asm_in_expr<'ctx, 'a>(env: &mut ExprGenEnv<'ctx, 'a>, e: &Expression) -> BasicValueEnum<'ctx> {
    match e {
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
            iv.as_basic_value_enum()
        }

        Expression::Variable(name) => {
            let info = env.variables.get(name).unwrap_or_else(|| panic!("Input variable '{}' not found", name));
            env.builder.build_load(info.ptr, name).unwrap()
        }

        Expression::AddressOf(inner) => {
            match inner.as_ref() {
                Expression::Variable(name) => {
                    let info = env.variables.get(name).unwrap_or_else(|| panic!("Input variable '{}' not found", name));
                    info.ptr.as_basic_value_enum()
                }
                _ => panic!("Unsupported address-of input: {:?}", inner),
            }
        }

        Expression::Deref(inner) => {
            match inner.as_ref() {
                Expression::Variable(name) => {
                    let info = env.variables.get(name).unwrap_or_else(|| panic!("Input pointer '{}' not found", name));
                    let pv = env.builder.build_load(info.ptr, "asm_in_ptr").unwrap();
                    if !pv.is_pointer_value() {
                        panic!("deref input '{}' is not a pointer", name);
                    }
                    env.builder.build_load(pv.into_pointer_value(), "asm_in_deref").unwrap()
                }
                _ => panic!("Unsupported deref input: {:?}", inner),
            }
        }

        other => panic!("Unsupported asm input expr: {:?}", other),
    }
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