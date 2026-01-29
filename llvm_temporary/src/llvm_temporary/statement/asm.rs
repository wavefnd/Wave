use crate::llvm_temporary::llvm_codegen::VariableInfo;
use inkwell::module::Module;
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallableValue, PointerValue};
use inkwell::{InlineAsmDialect};
use parser::ast::{Expression, Literal};
use std::collections::{HashMap, HashSet};
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StringRadix};
use crate::llvm_temporary::llvm_codegen::plan::*;

enum AsmOutPlace<'ctx> {
    VarAlloca {
        ptr: PointerValue<'ctx>
    },
    MemPtr {
        ptr: PointerValue<'ctx>,
        elem_ty: BasicTypeEnum<'ctx>
    },
}

fn reg_width_bits(reg: &str) -> Option<u32> {
    match reg {
        // 8-bit
        "al" | "bl" | "cl" | "dl" |
        "sil" | "dil" |
        "r8b" | "r9b" | "r10b" | "r11b" |
        "r12b" | "r13b" | "r14b" | "r15b" => Some(8),

        // 16-bit
        "ax" | "bx" | "cx" | "dx" |
        "si" | "di" |
        "r8w" | "r9w" | "r10w" | "r11w" |
        "r12w" | "r13w" | "r14w" | "r15w" => Some(16),

        // 32-bit
        "eax" | "ebx" | "ecx" | "edx" |
        "esi" | "edi" |
        "r8d" | "r9d" | "r10d" | "r11d" |
        "r12d" | "r13d" | "r14d" | "r15d" => Some(32),

        // 64-bit
        "rax" | "rbx" | "rcx" | "rdx" |
        "rsi" | "rdi" | "rbp" | "rsp" |
        "r8" | "r9" | "r10" | "r11" |
        "r12" | "r13" | "r14" | "r15" => Some(64),

        _ => None,
    }
}

fn extract_reg_from_constraint(c: &str) -> Option<String> {
    // "{rdx}" → rdx
    if let Some(inner) = c.strip_prefix('{').and_then(|s| s.strip_suffix('}')) {
        return Some(inner.to_ascii_lowercase());
    }
    None
}

pub(super) fn gen_asm_stmt_ir<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    instructions: &[String],
    inputs: &[(String, Expression)],
    outputs: &[(String, Expression)],
    clobbers: &[String],
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
) {
    let plan = AsmPlan::build(instructions, inputs, outputs, clobbers, AsmSafetyMode::ConservativeKernel);
    let constraints_str = plan.constraints_string();

    let mut operand_vals: Vec<BasicMetadataValueEnum<'ctx>> = Vec::with_capacity(plan.inputs.len());
    let mut param_types: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::with_capacity(plan.inputs.len());

    for inp in &plan.inputs {
        let mut val =
            asm_operand_to_value(context, builder, variables, global_consts, inp.value);

        if let Some(reg) = extract_reg_from_constraint(&inp.constraint) {
            if let Some(bits) = reg_width_bits(&reg) {
                if val.is_int_value() {
                    let iv = val.into_int_value();
                    let target_ty = context.custom_width_int_type(bits);

                    if iv.get_type() != target_ty {
                        val = builder
                            .build_int_truncate(iv, target_ty, "asm_in_trunc")
                            .unwrap()
                            .as_basic_value_enum();
                    }
                }
            }
        }

        param_types.push(val.get_type().into());
        operand_vals.push(val.into());
    }


    let mut out_places: Vec<AsmOutPlace<'ctx>> = Vec::with_capacity(plan.outputs.len());
    let mut out_tys: Vec<BasicTypeEnum<'ctx>> = Vec::with_capacity(plan.outputs.len());

    for o in &plan.outputs {
        let (place, ty) = resolve_out_place_and_type(context, builder, variables, o.target);
        out_places.push(place);
        out_tys.push(ty);
    }

    let fn_type = if out_tys.is_empty() {
        context.void_type().fn_type(&param_types, false)
    } else if out_tys.len() == 1 {
        out_tys[0].fn_type(&param_types, false)
    } else {
        let st = context.struct_type(&out_tys, false);
        st.fn_type(&param_types, false)
    };

    let inline_asm_ptr = context.create_inline_asm(
        fn_type,
        plan.asm_code.clone(),
        constraints_str,
        plan.has_side_effects,
        false, // alignstack
        Some(InlineAsmDialect::Intel),
        false,
    );

    let inline_asm_fn =
        CallableValue::try_from(inline_asm_ptr).expect("Failed to convert inline asm");

    let call = builder
        .build_call(inline_asm_fn, &operand_vals, "inline_asm")
        .unwrap();

    if out_places.is_empty() {
        return;
    }

    let ret_val = call.try_as_basic_value().left().unwrap();

    if out_places.len() == 1 {
        store_asm_out_place(context, builder, &out_places[0], ret_val, "asm_out");
        return;
    }

    let struct_val = ret_val.into_struct_value();
    for (idx, place) in out_places.iter().enumerate() {
        let elem = builder
            .build_extract_value(struct_val, idx as u32, "asm_out_elem")
            .unwrap();
        store_asm_out_place(context, builder, place, elem, "asm_out");
    }
}

fn resolve_out_place_and_type<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    variables: &HashMap<String, VariableInfo<'ctx>>,
    target: &Expression,
) -> (AsmOutPlace<'ctx>, BasicTypeEnum<'ctx>) {
    match target {
        Expression::Variable(name) => {
            let info = variables.get(name).unwrap_or_else(|| panic!("Output var '{}' not found", name));
            let elem_any = info.ptr.get_type().get_element_type();
            let elem_ty = any_to_basic_type(elem_any, "asm output var element type");
            (AsmOutPlace::VarAlloca { ptr: info.ptr }, elem_ty)
        }

        // allow: out("rax") deref p
        Expression::Deref(inner) => {
            match inner.as_ref() {
                Expression::Variable(name) => {
                    let info = variables.get(name).unwrap_or_else(|| panic!("Pointer var '{}' not found", name));
                    let loaded = builder.build_load(info.ptr, "asm_out_ptr").unwrap();

                    if !loaded.is_pointer_value() {
                        panic!("deref target '{}' is not a pointer value", name);
                    }

                    let dst_ptr = loaded.into_pointer_value();
                    let elem_any = dst_ptr.get_type().get_element_type();
                    let elem_ty = any_to_basic_type(elem_any, "asm deref output element type");

                    (AsmOutPlace::MemPtr { ptr: dst_ptr, elem_ty }, elem_ty)
                }
                other => panic!("Unsupported deref out target: {:?}", other),
            }
        }

        other => panic!("out(...) target must be variable or deref var for now: {:?}", other),
    }
}

fn store_asm_out_place<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    place: &AsmOutPlace<'ctx>,
    value: BasicValueEnum<'ctx>,
    name: &str,
) {
    match place {
        AsmOutPlace::VarAlloca { ptr } => {
            let dst_any = ptr.get_type().get_element_type();
            let dst_ty = any_to_basic_type(dst_any, "asm output dst type");
            let v = coerce_basic_value_for_store(context, builder, value, dst_ty, name);
            builder.build_store(*ptr, v).unwrap();
        }

        AsmOutPlace::MemPtr { ptr, elem_ty } => {
            let v = coerce_basic_value_for_store(context, builder, value, *elem_ty, name);
            builder.build_store(*ptr, v).unwrap();
        }
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

        Expression::Deref(inner) => {
            match inner.as_ref() {
                Expression::Variable(name) => {
                    let info = variables
                        .get(name)
                        .unwrap_or_else(|| panic!("Input pointer var '{}' not found", name));

                    let ptr_val = builder.build_load(info.ptr, "asm_in_ptr").unwrap();
                    if !ptr_val.is_pointer_value() {
                        panic!("deref input '{}' is not a pointer", name);
                    }
                    let p = ptr_val.into_pointer_value();
                    builder.build_load(p, "asm_in_deref").unwrap()
                }
                _ => panic!("Unsupported asm deref input: {:?}", inner),
            }
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