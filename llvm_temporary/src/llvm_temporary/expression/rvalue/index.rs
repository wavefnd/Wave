use super::ExprGenEnv;
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::Expression;

fn idx_to_i32<'ctx>(
    env: &mut ExprGenEnv<'ctx, '_>,
    idx: inkwell::values::IntValue<'ctx>,
) -> inkwell::values::IntValue<'ctx> {
    let i32t = env.context.i32_type();
    let w = idx.get_type().get_bit_width();
    if w == 32 {
        idx
    } else if w < 32 {
        env.builder.build_int_s_extend(idx, i32t, "idx_sext").unwrap()
    } else {
        env.builder.build_int_truncate(idx, i32t, "idx_trunc").unwrap()
    }
}

pub(crate) fn gen<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    target: &Expression,
    index: &Expression,
) -> BasicValueEnum<'ctx> {
    unsafe {
        let target_val = env.gen(target, None);
        let index_val = env.gen(index, None);

        let index_int = match index_val {
            BasicValueEnum::IntValue(i) => idx_to_i32(env, i),
            _ => panic!("Index must be an integer"),
        };

        let zero = env.context.i32_type().const_zero();

        match target_val {
            BasicValueEnum::PointerValue(ptr_val) => {
                let pointee_ty = ptr_val.get_type().get_element_type();

                // case 1) ptr -> [array]
                if pointee_ty.is_array_type() {
                    let arr_ty = pointee_ty.into_array_type();
                    let elem_ty = arr_ty.get_element_type();

                    let gep = env
                        .builder
                        .build_in_bounds_gep(ptr_val, &[zero, index_int], "array_index_gep")
                        .unwrap();

                    if elem_ty.is_array_type() || elem_ty.is_struct_type() {
                        return gep.as_basic_value_enum();
                    }

                    return env
                        .builder
                        .build_load(gep, "load_array_elem")
                        .unwrap()
                        .as_basic_value_enum();
                }

                let gep = env
                    .builder
                    .build_in_bounds_gep(ptr_val, &[index_int], "ptr_index_gep")
                    .unwrap();

                if pointee_ty.is_array_type() || pointee_ty.is_struct_type() {
                    return gep.as_basic_value_enum();
                }

                env.builder
                    .build_load(gep, "load_ptr_elem")
                    .unwrap()
                    .as_basic_value_enum()
            }

            BasicValueEnum::ArrayValue(arr_val) => {
                // arr_val: [N x T]
                let tmp = env.builder
                    .build_alloca(arr_val.get_type(), "tmp_arr")
                    .unwrap();

                env.builder.build_store(tmp, arr_val).unwrap();

                let elem_ty = arr_val.get_type().get_element_type();
                let gep = env.builder
                    .build_in_bounds_gep(tmp, &[zero, index_int], "array_index_gep_tmp")
                    .unwrap();

                if elem_ty.is_array_type() || elem_ty.is_struct_type() {
                    return gep.as_basic_value_enum();
                }

                env.builder
                    .build_load(gep, "load_array_elem_tmp")
                    .unwrap()
                    .as_basic_value_enum()
            }

            other => {
                panic!("Unsupported target in IndexAccess: {:?}", other)
            }
        }
    }
}
