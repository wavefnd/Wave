use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::StructType,
    values::{BasicValueEnum, PointerValue},
};
use std::collections::HashMap;
use parser::ast::Expression;
use crate::llvm_temporary::expression::rvalue::generate_expression_ir;
use crate::llvm_temporary::llvm_codegen::VariableInfo;

pub fn generate_lvalue_ir<'ctx>(
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    expr: &Expression,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    module: &'ctx Module<'ctx>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
) -> PointerValue<'ctx> {
    match expr {
        Expression::Variable(name) => {
            if global_consts.contains_key(name) {
                panic!("Cannot use constant '{}' as lvalue", name);
            }

            let info = variables
                .get(name)
                .unwrap_or_else(|| panic!("Undefined variable '{}'", name));

            info.ptr
        }

        Expression::Grouped(inner) => generate_lvalue_ir(
            context,
            builder,
            inner,
            variables,
            module,
            global_consts,
            struct_types,
            struct_field_indices,
        ),

        Expression::Deref(inner) => {
            let v = generate_expression_ir(
                context,
                builder,
                inner,
                variables,
                module,
                None,
                global_consts,
                struct_types,
                struct_field_indices,
            );

            match v {
                BasicValueEnum::PointerValue(p) => p,
                _ => panic!("Deref target is not a pointer: {:?}", inner),
            }
        }

        Expression::AddressOf(inner) => generate_lvalue_ir(
            context,
            builder,
            inner,
            variables,
            module,
            global_consts,
            struct_types,
            struct_field_indices,
        ),

        Expression::IndexAccess { target, index } => {
            let idx_val = generate_expression_ir(
                context,
                builder,
                index,
                variables,
                module,
                None,
                global_consts,
                struct_types,
                struct_field_indices,
            );

            let mut idx = match idx_val {
                BasicValueEnum::IntValue(iv) => iv,
                _ => panic!("Index is not an integer: {:?}", index),
            };

            if idx.get_type().get_bit_width() != 32 {
                idx = builder
                    .build_int_cast(idx, context.i32_type(), "idx_i32")
                    .unwrap();
            }

            let base_ptr: PointerValue<'ctx> = match &**target {
                Expression::Variable(_)
                | Expression::Deref(_)
                | Expression::AddressOf(_)
                | Expression::IndexAccess { .. }
                | Expression::FieldAccess { .. }
                | Expression::Grouped(_) => {
                    let lv = generate_lvalue_ir(
                        context,
                        builder,
                        target,
                        variables,
                        module,
                        global_consts,
                        struct_types,
                        struct_field_indices,
                    );

                    let elem_ty = lv.get_type().get_element_type();
                    if elem_ty.is_pointer_type() {
                        let loaded = builder.build_load(lv, "load_index_base").unwrap();
                        match loaded {
                            BasicValueEnum::PointerValue(p) => p,
                            _ => panic!("Index base is not a pointer"),
                        }
                    } else {
                        lv
                    }
                }
                _ => {
                    let v = generate_expression_ir(
                        context,
                        builder,
                        target,
                        variables,
                        module,
                        None,
                        global_consts,
                        struct_types,
                        struct_field_indices,
                    );
                    match v {
                        BasicValueEnum::PointerValue(p) => p,
                        _ => panic!("Index target is not a pointer/array: {:?}", target),
                    }
                }
            };

            let zero = context.i32_type().const_zero();
            let base_elem_ty = base_ptr.get_type().get_element_type();

            if base_elem_ty.is_array_type() {
                unsafe { builder.build_gep(base_ptr, &[zero, idx], "array_elem_ptr").unwrap() }
            } else {
                unsafe { builder.build_gep(base_ptr, &[idx], "ptr_elem_ptr").unwrap() }
            }
        }

        Expression::FieldAccess { object, field } => {
            let mut obj_ptr = generate_lvalue_ir(
                context,
                builder,
                object,
                variables,
                module,
                global_consts,
                struct_types,
                struct_field_indices,
            );

            let obj_elem_ty = obj_ptr.get_type().get_element_type();
            if obj_elem_ty.is_pointer_type() {
                let loaded = builder.build_load(obj_ptr, "load_struct_ptr").unwrap();
                obj_ptr = match loaded {
                    BasicValueEnum::PointerValue(p) => p,
                    _ => panic!("FieldAccess base is not a pointer"),
                };
            }

            let struct_ty = obj_ptr
                .get_type()
                .get_element_type()
                .into_struct_type();

            let raw_name = struct_ty
                .get_name()
                .and_then(|c| c.to_str().ok())
                .unwrap_or_else(|| panic!("Unnamed struct type; cannot resolve field '{}'", field));

            let struct_name = raw_name.strip_prefix("struct.").unwrap_or(raw_name);

            let field_index = struct_field_indices
                .get(struct_name)
                .and_then(|m| m.get(field))
                .copied()
                .unwrap_or_else(|| panic!("Unknown field '{}.{}'", struct_name, field));

            builder
                .build_struct_gep(obj_ptr, field_index, "field_ptr")
                .unwrap()
        }

        _ => {
            panic!("Expression is not an lvalue (not assignable): {:?}", expr);
        }
    }
}