use super::{utils::to_bool, ExprGenEnv};
use inkwell::values::{BasicValue, BasicValueEnum};
use inkwell::{FloatPredicate, IntPredicate};
use parser::ast::{Expression, Operator};

pub(crate) fn gen<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    left: &Expression,
    operator: &Operator,
    right: &Expression,
    expected_type: Option<inkwell::types::BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    let left_val = env.gen(left, None);
    let right_val = env.gen(right, None);

    match (left_val, right_val) {
        (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
            let l_type = l.get_type();
            let r_type = r.get_type();

            let (l_casted, r_casted) = match operator {
                Operator::ShiftLeft | Operator::ShiftRight => {
                    let r2 = if r_type != l_type {
                        env.builder.build_int_cast(r, l_type, "shamt").unwrap()
                    } else {
                        r
                    };
                    (l, r2)
                }
                _ => {
                    if l_type != r_type {
                        if l_type.get_bit_width() < r_type.get_bit_width() {
                            let new_l = env.builder.build_int_z_extend(l, r_type, "zext_l").unwrap();
                            (new_l, r)
                        } else {
                            let new_r = env.builder.build_int_z_extend(r, l_type, "zext_r").unwrap();
                            (l, new_r)
                        }
                    } else {
                        (l, r)
                    }
                }
            };

            let mut result = match operator {
                Operator::Add => env.builder.build_int_add(l_casted, r_casted, "addtmp"),
                Operator::Subtract => env.builder.build_int_sub(l_casted, r_casted, "subtmp"),
                Operator::Multiply => env.builder.build_int_mul(l_casted, r_casted, "multmp"),
                Operator::Divide => env.builder.build_int_signed_div(l_casted, r_casted, "divtmp"),
                Operator::Remainder => env.builder.build_int_signed_rem(l_casted, r_casted, "modtmp"),
                Operator::ShiftLeft => env.builder.build_left_shift(l_casted, r_casted, "shl"),
                Operator::ShiftRight => env.builder.build_right_shift(l_casted, r_casted, true, "shr"),
                Operator::BitwiseAnd => env.builder.build_and(l_casted, r_casted, "andtmp"),
                Operator::BitwiseOr => env.builder.build_or(l_casted, r_casted, "ortmp"),
                Operator::BitwiseXor => env.builder.build_xor(l_casted, r_casted, "xortmp"),

                Operator::Greater => env.builder.build_int_compare(IntPredicate::SGT, l_casted, r_casted, "cmptmp"),
                Operator::Less => env.builder.build_int_compare(IntPredicate::SLT, l_casted, r_casted, "cmptmp"),
                Operator::Equal => env.builder.build_int_compare(IntPredicate::EQ, l_casted, r_casted, "cmptmp"),
                Operator::NotEqual => env.builder.build_int_compare(IntPredicate::NE, l_casted, r_casted, "cmptmp"),
                Operator::GreaterEqual => env.builder.build_int_compare(IntPredicate::SGE, l_casted, r_casted, "cmptmp"),
                Operator::LessEqual => env.builder.build_int_compare(IntPredicate::SLE, l_casted, r_casted, "cmptmp"),

                Operator::LogicalAnd => {
                    let lb = to_bool(env.builder, l_casted);
                    let rb = to_bool(env.builder, r_casted);
                    env.builder.build_and(lb, rb, "land")
                }
                Operator::LogicalOr => {
                    let lb = to_bool(env.builder, l_casted);
                    let rb = to_bool(env.builder, r_casted);
                    env.builder.build_or(lb, rb, "lor")
                }

                _ => panic!("Unsupported binary operator"),
            }
                .unwrap();

            if let Some(inkwell::types::BasicTypeEnum::IntType(target_ty)) = expected_type {
                let result_ty = result.get_type();
                if result_ty != target_ty {
                    result = env.builder.build_int_cast(result, target_ty, "cast_result").unwrap();
                }
            }

            result.as_basic_value_enum()
        }

        (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => match operator {
            Operator::Greater => env.builder.build_float_compare(FloatPredicate::OGT, l, r, "fcmpgt").unwrap().as_basic_value_enum(),
            Operator::Less => env.builder.build_float_compare(FloatPredicate::OLT, l, r, "fcmplt").unwrap().as_basic_value_enum(),
            Operator::Equal => env.builder.build_float_compare(FloatPredicate::OEQ, l, r, "fcmpeq").unwrap().as_basic_value_enum(),
            Operator::NotEqual => env.builder.build_float_compare(FloatPredicate::ONE, l, r, "fcmpne").unwrap().as_basic_value_enum(),
            Operator::GreaterEqual => env.builder.build_float_compare(FloatPredicate::OGE, l, r, "fcmpge").unwrap().as_basic_value_enum(),
            Operator::LessEqual => env.builder.build_float_compare(FloatPredicate::OLE, l, r, "fcmple").unwrap().as_basic_value_enum(),
            Operator::Remainder => env.builder.build_float_rem(l, r, "modtmp").unwrap().as_basic_value_enum(),
            _ => panic!("Unsupported float operator"),
        },

        (BasicValueEnum::IntValue(int_val), BasicValueEnum::FloatValue(float_val)) => {
            let casted = env
                .builder
                .build_signed_int_to_float(int_val, float_val.get_type(), "cast_lhs")
                .unwrap();

            match operator {
                Operator::Add => env.builder.build_float_add(casted, float_val, "addtmp").unwrap().as_basic_value_enum(),
                Operator::Subtract => env.builder.build_float_sub(casted, float_val, "subtmp").unwrap().as_basic_value_enum(),
                Operator::Multiply => env.builder.build_float_mul(casted, float_val, "multmp").unwrap().as_basic_value_enum(),
                Operator::Divide => env.builder.build_float_div(casted, float_val, "divtmp").unwrap().as_basic_value_enum(),
                Operator::Remainder => env.builder.build_float_rem(casted, float_val, "modtmp").unwrap().as_basic_value_enum(),

                Operator::Greater => env.builder.build_float_compare(FloatPredicate::OGT, casted, float_val, "fcmpgt").unwrap().as_basic_value_enum(),
                Operator::Less => env.builder.build_float_compare(FloatPredicate::OLT, casted, float_val, "fcmplt").unwrap().as_basic_value_enum(),
                Operator::Equal => env.builder.build_float_compare(FloatPredicate::OEQ, casted, float_val, "fcmpeq").unwrap().as_basic_value_enum(),
                Operator::NotEqual => env.builder.build_float_compare(FloatPredicate::ONE, casted, float_val, "fcmpne").unwrap().as_basic_value_enum(),
                Operator::GreaterEqual => env.builder.build_float_compare(FloatPredicate::OGE, casted, float_val, "fcmpge").unwrap().as_basic_value_enum(),
                Operator::LessEqual => env.builder.build_float_compare(FloatPredicate::OLE, casted, float_val, "fcmple").unwrap().as_basic_value_enum(),

                _ => panic!("Unsupported mixed-type operator (int + float)"),
            }
        }

        (BasicValueEnum::FloatValue(float_val), BasicValueEnum::IntValue(int_val)) => {
            let casted = env
                .builder
                .build_signed_int_to_float(int_val, float_val.get_type(), "cast_rhs")
                .unwrap();

            match operator {
                Operator::Add => env.builder.build_float_add(float_val, casted, "addtmp").unwrap().as_basic_value_enum(),
                Operator::Subtract => env.builder.build_float_sub(float_val, casted, "subtmp").unwrap().as_basic_value_enum(),
                Operator::Multiply => env.builder.build_float_mul(float_val, casted, "multmp").unwrap().as_basic_value_enum(),
                Operator::Divide => env.builder.build_float_div(float_val, casted, "divtmp").unwrap().as_basic_value_enum(),
                Operator::Remainder => env.builder.build_float_rem(float_val, casted, "modtmp").unwrap().as_basic_value_enum(),

                Operator::Greater => env.builder.build_float_compare(FloatPredicate::OGT, float_val, casted, "fcmpgt").unwrap().as_basic_value_enum(),
                Operator::Less => env.builder.build_float_compare(FloatPredicate::OLT, float_val, casted, "fcmplt").unwrap().as_basic_value_enum(),
                Operator::Equal => env.builder.build_float_compare(FloatPredicate::OEQ, float_val, casted, "fcmpeq").unwrap().as_basic_value_enum(),
                Operator::NotEqual => env.builder.build_float_compare(FloatPredicate::ONE, float_val, casted, "fcmpne").unwrap().as_basic_value_enum(),
                Operator::GreaterEqual => env.builder.build_float_compare(FloatPredicate::OGE, float_val, casted, "fcmpge").unwrap().as_basic_value_enum(),
                Operator::LessEqual => env.builder.build_float_compare(FloatPredicate::OLE, float_val, casted, "fcmple").unwrap().as_basic_value_enum(),

                _ => panic!("Unsupported mixed-type operator (float + int)"),
            }
        }

        _ => panic!("Type mismatch in binary expression"),
    }
}
