use super::ExprGenEnv;
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::{Expression, WaveType};
use crate::llvm_temporary::llvm_codegen::generate_address_ir;

pub(crate) fn gen_struct_literal<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    name: &str,
    fields: &[(String, Expression)],
) -> BasicValueEnum<'ctx> {
    let struct_ty = *env
        .struct_types
        .get(name)
        .unwrap_or_else(|| panic!("Struct type '{}' not found", name));

    let field_indices = env
        .struct_field_indices
        .get(name)
        .unwrap_or_else(|| panic!("Field index map for struct '{}' not found", name));

    let tmp_alloca = env
        .builder
        .build_alloca(struct_ty, &format!("tmp_{}_literal", name))
        .unwrap();

    for (field_name, field_expr) in fields {
        let idx = field_indices
            .get(field_name)
            .unwrap_or_else(|| panic!("Field '{}' not found in struct '{}'", field_name, name));

        let expected_field_ty = struct_ty
            .get_field_type_at_index(*idx)
            .unwrap_or_else(|| panic!("No field type at index {} for struct '{}'", idx, name));

        let field_val = env.gen(field_expr, Some(expected_field_ty));

        let field_ptr = env
            .builder
            .build_struct_gep(tmp_alloca, *idx, &format!("{}.{}", name, field_name))
            .unwrap();

        env.builder.build_store(field_ptr, field_val).unwrap();
    }

    env.builder
        .build_load(tmp_alloca, &format!("{}_literal_val", name))
        .unwrap()
        .as_basic_value_enum()
}

pub(crate) fn gen_field_access<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    object: &Expression,
    field: &str,
) -> BasicValueEnum<'ctx> {
    let full = Expression::FieldAccess {
        object: Box::new(object.clone()),
        field: field.to_string(),
    };

    let ptr = generate_address_ir(
        env.context,
        env.builder,
        &full,
        env.variables,
        env.module,
        env.struct_types,
        env.struct_field_indices,
    );

    env.builder
        .build_load(ptr, &format!("load_field_{}", field))
        .unwrap()
        .as_basic_value_enum()
}

