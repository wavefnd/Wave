use super::ExprGenEnv;
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::{Expression, WaveType};

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

        let field_val = env.gen(field_expr, None);

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
    let var_name = match object {
        Expression::Variable(name) => name,
        other => panic!("FieldAccess on non-variable object not supported yet: {:?}", other),
    };

    let var_info = env
        .variables
        .get(var_name)
        .unwrap_or_else(|| panic!("Variable '{}' not found for field access", var_name));

    let struct_name = match &var_info.ty {
        WaveType::Struct(name) => name,
        other_ty => panic!(
            "Field access on non-struct type {:?} for variable '{}'",
            other_ty, var_name
        ),
    };

    let field_indices = env
        .struct_field_indices
        .get(struct_name)
        .unwrap_or_else(|| panic!("Field index map for struct '{}' not found", struct_name));

    let idx = field_indices
        .get(field)
        .unwrap_or_else(|| panic!("Field '{}' not found in struct '{}'", field, struct_name));

    let field_ptr = env
        .builder
        .build_struct_gep(var_info.ptr, *idx, &format!("{}.{}", var_name, field))
        .unwrap();

    env.builder
        .build_load(field_ptr, &format!("load_{}_{}", var_name, field))
        .unwrap()
        .as_basic_value_enum()
}
