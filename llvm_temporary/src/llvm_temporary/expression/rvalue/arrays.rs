use super::ExprGenEnv;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::Expression;

use crate::llvm_temporary::statement::variable::{coerce_basic_value, CoercionMode};

pub(crate) fn gen_array_literal<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    elements: &[Expression],
    expected_type: Option<BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    let arr_ty = match expected_type {
        Some(BasicTypeEnum::ArrayType(a)) => a,

        Some(BasicTypeEnum::PointerType(p)) if p.get_element_type().is_array_type() => {
            p.get_element_type().into_array_type()
        }

        Some(t) => panic!("ArrayLiteral expected array type, got {:?}", t),
        None => panic!("ArrayLiteral requires expected_type (array type)"),
    };

    let elem_ty = arr_ty.get_element_type();

    let alloca = env.builder.build_alloca(arr_ty, "arr_lit").unwrap();
    let zero = env.context.i32_type().const_zero();

    for (i, e) in elements.iter().enumerate() {
        let mut v = env.gen(e, Some(elem_ty));

        if v.get_type() != elem_ty {
            v = coerce_basic_value(
                env.context,
                env.builder,
                v,
                elem_ty,
                &format!("arr{}_cast", i),
                CoercionMode::Implicit,
            );
        }

        let gep = unsafe {
            env.builder
                .build_in_bounds_gep(
                    alloca,
                    &[
                        zero,
                        env.context.i32_type().const_int(i as u64, false),
                    ],
                    &format!("arr_gep_{}", i),
                )
                .unwrap()
        };

        env.builder.build_store(gep, v).unwrap();
    }

    env.builder
        .build_load(alloca, "arr_lit_load")
        .unwrap()
        .as_basic_value_enum()
}
