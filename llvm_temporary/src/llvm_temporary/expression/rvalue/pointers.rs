use super::ExprGenEnv;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::Expression;

pub(crate) fn gen_deref<'ctx, 'a>(env: &mut ExprGenEnv<'ctx, 'a>, inner_expr: &Expression) -> BasicValueEnum<'ctx> {
    match inner_expr {
        Expression::Variable(var_name) => {
            let ptr_to_value = env.variables.get(var_name).unwrap().ptr;
            let actual_ptr = env
                .builder
                .build_load(ptr_to_value, "deref_target")
                .unwrap()
                .into_pointer_value();
            env.builder
                .build_load(actual_ptr, "deref_load")
                .unwrap()
                .as_basic_value_enum()
        }
        _ => {
            let ptr_val = env.gen(inner_expr, None);
            let ptr = ptr_val.into_pointer_value();
            env.builder
                .build_load(ptr, "deref_load")
                .unwrap()
                .as_basic_value_enum()
        }
    }
}

pub(crate) fn gen_addressof<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    inner_expr: &Expression,
    expected_type: Option<BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    if let Some(BasicTypeEnum::PointerType(ptr_ty)) = expected_type {
        match inner_expr {
            Expression::ArrayLiteral(elements) => unsafe {
                let array_type = ptr_ty.get_element_type().into_array_type();
                let elem_type = array_type.get_element_type();

                let array_type = elem_type.array_type(elements.len() as u32);
                let tmp_alloca = env.builder.build_alloca(array_type, "tmp_array").unwrap();

                for (i, expr) in elements.iter().enumerate() {
                    let val = env.gen(expr, Some(elem_type));

                    let gep = env
                        .builder
                        .build_in_bounds_gep(
                            tmp_alloca,
                            &[
                                env.context.i32_type().const_zero(),
                                env.context.i32_type().const_int(i as u64, false),
                            ],
                            &format!("array_idx_{}", i),
                        )
                        .unwrap();

                    env.builder.build_store(gep, val).unwrap();
                }

                let alloca = env
                    .builder
                    .build_alloca(tmp_alloca.get_type(), "tmp_array_ptr")
                    .unwrap();

                env.builder.build_store(alloca, tmp_alloca).unwrap();
                alloca.as_basic_value_enum()
            },

            Expression::Variable(var_name) => {
                let ptr = env
                    .variables
                    .get(var_name)
                    .unwrap_or_else(|| panic!("Variable {} not found", var_name));
                ptr.ptr.as_basic_value_enum()
            }

            _ => panic!("& operator must be used on variable name or array literal"),
        }
    } else {
        panic!("Expected pointer type for AddressOf");
    }
}
