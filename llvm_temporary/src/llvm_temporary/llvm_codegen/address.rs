use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{IntValue, PointerValue};
use parser::ast::{Expression, Literal};
use std::collections::HashMap;
use inkwell::types::{AsTypeRef, StructType};
use super::types::VariableInfo;

fn normalize_struct_name(raw: &str) -> &str {
    raw.strip_prefix("struct.").unwrap_or(raw).trim_start_matches('%')
}

fn cast_int_to_i64<'ctx>(
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    v: IntValue<'ctx>,
) -> IntValue<'ctx> {
    let i64_ty = context.i64_type();
    let src_bits = v.get_type().get_bit_width();

    if src_bits == 64 {
        v
    } else if src_bits < 64 {
        builder.build_int_s_extend(v, i64_ty, "idx_sext").unwrap()
    } else {
        builder.build_int_truncate(v, i64_ty, "idx_trunc").unwrap()
    }
}

fn int_expr_as_i64<'ctx>(
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    expr: &Expression,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    module: &'ctx Module<'ctx>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
) -> IntValue<'ctx> {
    match expr {
        Expression::Grouped(inner) => {
            int_expr_as_i64(context, builder, inner, variables, module, struct_types, struct_field_indices)
        }

        Expression::Literal(Literal::Int(s)) => {
            let n: i64 = s.parse().unwrap();
            context.i64_type().const_int(n as u64, true)
        }

        Expression::Variable(_) | Expression::FieldAccess { .. } | Expression::IndexAccess { .. } => {
            let addr = generate_address_ir(context, builder, expr, variables, module, struct_types, struct_field_indices);
            let loaded = builder.build_load(addr, "idx_load").unwrap().into_int_value();
            cast_int_to_i64(context, builder, loaded)
        }

        Expression::Deref(inner) => {
            let addr = generate_address_ir(context, builder, inner, variables, module, struct_types, struct_field_indices);
            let loaded = builder.build_load(addr, "deref_load").unwrap().into_int_value();
            cast_int_to_i64(context, builder, loaded)
        }

        other => panic!("Index int expr not supported yet: {:?}", other),
    }
}

fn resolve_struct_key<'ctx>(
    st: StructType<'ctx>,
    struct_types: &HashMap<String, StructType<'ctx>>,
) -> String {
    if let Some(raw) = st.get_name().and_then(|n| n.to_str().ok()) {
        return normalize_struct_name(raw).to_string();
    }

    let st_ref = st.as_type_ref();
    for (name, ty) in struct_types {
        if ty.as_type_ref() == st_ref {
            return name.clone();
        }
    }

    panic!("LLVM struct type has no name and cannot be matched to struct_types");
}

pub fn generate_address_ir<'ctx>(
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    expr: &Expression,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    module: &'ctx Module<'ctx>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
) -> PointerValue<'ctx> {
    match expr {
        Expression::Grouped(inner) => {
            generate_address_ir(context, builder, inner, variables, module, struct_types, struct_field_indices)
        }

        Expression::Variable(name) => {
            let var = variables.get(name).unwrap_or_else(|| panic!("Variable {} not found", name));
            var.ptr
        }

        Expression::AddressOf(inner) => {
            generate_address_ir(context, builder, inner, variables, module, struct_types, struct_field_indices)
        }

        Expression::Deref(inner) => {
            match inner.as_ref() {
                Expression::FieldAccess { .. } | Expression::IndexAccess { .. } | Expression::Grouped(_) => {
                    generate_address_ir(context, builder, inner, variables, module, struct_types, struct_field_indices)
                }

                Expression::Variable(name) => {
                    let var = variables.get(name).unwrap_or_else(|| panic!("Variable {} not found", name));
                    builder.build_load(var.ptr, "deref_target").unwrap().into_pointer_value()
                }

                other => panic!("Cannot deref this expression: {:?}", other),
            }
        }

        Expression::FieldAccess { object, field } => {
            let obj_addr = generate_address_ir(context, builder, object, variables, module, struct_types, struct_field_indices);
            let obj_elem_ty = obj_addr.get_type().get_element_type();

            let struct_ptr = if obj_elem_ty.is_pointer_type() {
                builder.build_load(obj_addr, "obj_load").unwrap().into_pointer_value()
            } else {
                obj_addr
            };

            let st = struct_ptr.get_type().get_element_type().into_struct_type();

            let sname = resolve_struct_key(st, struct_types);

            let idx = *struct_field_indices
                .get(&sname)
                .unwrap_or_else(|| panic!("Struct '{}' missing in struct_field_indices", sname))
                .get(field)
                .unwrap_or_else(|| panic!("Field '{}.{}' missing in struct_field_indices", sname, field));

            builder.build_struct_gep(struct_ptr, idx, "field_ptr").unwrap()
        }

        Expression::IndexAccess { target, index } => {
            let t_addr = generate_address_ir(context, builder, target, variables, module, struct_types, struct_field_indices);
            let t_elem_ty = t_addr.get_type().get_element_type();

            let base_ptr = if t_elem_ty.is_pointer_type() {
                builder.build_load(t_addr, "idx_base_load").unwrap().into_pointer_value()
            } else {
                t_addr
            };

            let idx_i64 = int_expr_as_i64(context, builder, index, variables, module, struct_types, struct_field_indices);

            let base_elem = base_ptr.get_type().get_element_type();
            if base_elem.is_array_type() {
                let zero = context.i64_type().const_int(0, false);
                unsafe { builder.build_in_bounds_gep(base_ptr, &[zero, idx_i64], "arr_gep").unwrap() }
            } else {
                unsafe { builder.build_in_bounds_gep(base_ptr, &[idx_i64], "ptr_gep").unwrap() }
            }
        }

        _ => panic!("Cannot take address of this expression: {:?}", expr),
    }
}
