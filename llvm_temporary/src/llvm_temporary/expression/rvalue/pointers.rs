use super::ExprGenEnv;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::Expression;
use crate::llvm_temporary::llvm_codegen::generate_address_ir;

fn push_deref_into_base(expr: &Expression) -> Expression {
    match expr {
        Expression::Grouped(inner) => {
            Expression::Grouped(Box::new(push_deref_into_base(inner)))
        }

        Expression::IndexAccess { target, index } => {
            Expression::IndexAccess {
                target: Box::new(push_deref_into_base(target)),
                index: index.clone(),
            }
        }

        Expression::FieldAccess { object, field } => {
            Expression::FieldAccess {
                object: Box::new(push_deref_into_base(object)),
                field: field.clone(),
            }
        }

        other => Expression::Deref(Box::new(other.clone())),
    }
}

pub(crate) fn gen_deref<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    inner_expr: &Expression,
) -> BasicValueEnum<'ctx> {
    match inner_expr {
        Expression::Grouped(inner) => {
            return gen_deref(env, inner);
        }

        Expression::IndexAccess { .. } | Expression::FieldAccess { .. } => {
            let addr = generate_address_ir(
                env.context,
                env.builder,
                inner_expr,
                env.variables,
                env.module,
                env.struct_types,
                env.struct_field_indices,
            );

            return env.builder
                .build_load(addr, "deref_load")
                .unwrap()
                .as_basic_value_enum();
        }

        _ => {}
    }

    let v = env.gen(inner_expr, None);
    if let BasicValueEnum::PointerValue(p) = v {
        return env.builder
            .build_load(p, "deref_load")
            .unwrap()
            .as_basic_value_enum();
    }

    panic!("deref expects pointer or lvalue (x[i], x.field), got: {:?}", inner_expr);
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
