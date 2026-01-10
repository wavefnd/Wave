use crate::llvm_temporary::expression::rvalue::generate_expression_ir;
use crate::llvm_temporary::llvm_codegen::{wave_type_to_llvm_type, VariableInfo};
use inkwell::module::{Module};
use inkwell::types::{BasicTypeEnum, StructType};
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::{Expression, VariableNode, WaveType};
use std::collections::HashMap;

#[derive(Copy, Clone, Debug)]
pub enum CoercionMode {
    Implicit,
    Explicit,
    Asm,
}


pub fn coerce_basic_value<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    val: BasicValueEnum<'ctx>,
    expected: BasicTypeEnum<'ctx>,
    tag: &str,
    mode: CoercionMode,
) -> BasicValueEnum<'ctx> {
    if val.get_type() == expected {
        return val;
    }

    match (val, expected) {
        // int <-> int
        (BasicValueEnum::IntValue(iv), BasicTypeEnum::IntType(dst)) => {
            let src_bw = iv.get_type().get_bit_width();
            let dst_bw = dst.get_bit_width();

            if src_bw == dst_bw {
                iv.as_basic_value_enum()
            } else if src_bw > dst_bw {
                builder.build_int_truncate(iv, dst, tag).unwrap().as_basic_value_enum()
            } else {
                builder.build_int_s_extend(iv, dst, tag).unwrap().as_basic_value_enum()
            }
        }

        // float -> int
        (BasicValueEnum::FloatValue(fv), BasicTypeEnum::IntType(dst)) => builder
            .build_float_to_signed_int(fv, dst, tag)
            .unwrap()
            .as_basic_value_enum(),

        // int -> float
        (BasicValueEnum::IntValue(iv), BasicTypeEnum::FloatType(dst)) => builder
            .build_signed_int_to_float(iv, dst, tag)
            .unwrap()
            .as_basic_value_enum(),

        // ptr -> ptr
        (BasicValueEnum::PointerValue(pv), BasicTypeEnum::PointerType(dst)) => builder
            .build_bit_cast(pv, dst, tag)
            .unwrap()
            .as_basic_value_enum(),

        (BasicValueEnum::IntValue(iv), BasicTypeEnum::PointerType(dst)) => {
            match mode {
                CoercionMode::Implicit => {
                    if iv.is_const() && iv.get_zero_extended_constant() == Some(0) {
                        dst.const_null().as_basic_value_enum()
                    } else {
                        panic!("Implicit int->ptr is not allowed (use explicit cast).");
                    }
                }
                CoercionMode::Asm | CoercionMode::Explicit => builder
                    .build_int_to_ptr(iv, dst, tag)
                    .unwrap()
                    .as_basic_value_enum(),
            }
        }

        (BasicValueEnum::PointerValue(pv), BasicTypeEnum::IntType(dst)) => {
            match mode {
                CoercionMode::Implicit => {
                    panic!("Implicit ptr->int is not allowed (use explicit cast).");
                }
                CoercionMode::Asm | CoercionMode::Explicit => builder
                    .build_ptr_to_int(pv, dst, tag)
                    .unwrap()
                    .as_basic_value_enum(),
            }
        }

        _ => {
            panic!("Type mismatch: expected {:?}, got {:?}", expected, val.get_type());
        }
    }
}

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
            let raw = generate_expression_ir(
                context, builder, init, variables, module,
                Some(llvm_type),
                global_consts, struct_types, struct_field_indices,
            );

            let casted = coerce_basic_value(
                context, builder, raw, llvm_type, "init_cast", CoercionMode::Implicit
            );

            builder.build_store(alloca, casted).unwrap();
        }
    }
}
