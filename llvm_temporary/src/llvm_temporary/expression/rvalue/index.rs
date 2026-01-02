use super::ExprGenEnv;
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::Expression;

pub(crate) fn gen<'ctx, 'a>(env: &mut ExprGenEnv<'ctx, 'a>, target: &Expression, index: &Expression) -> BasicValueEnum<'ctx> {
    unsafe {
        let target_val = env.gen(target, None);
        let index_val = env.gen(index, None);

        let index_int = match index_val {
            BasicValueEnum::IntValue(i) => i,
            _ => panic!("Index must be an integer"),
        };

        let zero = env.context.i32_type().const_zero();

        match target_val {
            BasicValueEnum::PointerValue(ptr_val) => {
                let element_type = ptr_val.get_type().get_element_type();

                if element_type.is_array_type() {
                    let gep = env
                        .builder
                        .build_in_bounds_gep(ptr_val, &[zero, index_int], "array_index_gep")
                        .unwrap();

                    let elem_type = element_type.into_array_type().get_element_type();

                    if elem_type.is_pointer_type() {
                        env.builder.build_load(gep, "load_ptr_from_array").unwrap().as_basic_value_enum()
                    } else {
                        env.builder.build_load(gep, "load_array_elem").unwrap().as_basic_value_enum()
                    }
                } else {
                    let gep = env
                        .builder
                        .build_in_bounds_gep(ptr_val, &[index_int], "ptr_index_gep")
                        .unwrap();

                    env.builder.build_load(gep, "load_ptr_elem").unwrap().as_basic_value_enum()
                }
            }

            _ => panic!("Unsupported target in IndexAccess"),
        }
    }
}
