use super::ExprGenEnv;
use inkwell::values::{BasicValue, BasicValueEnum};
use inkwell::IntPredicate;
use parser::ast::{Expression, Operator};

pub(crate) fn gen<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    operator: &Operator,
    expr: &Expression,
) -> BasicValueEnum<'ctx> {
    let val = env.gen(expr, None);

    match (operator, val) {
        // ! (logical not)
        (Operator::LogicalNot, BasicValueEnum::IntValue(iv))
        | (Operator::Not, BasicValueEnum::IntValue(iv)) => {
            let bw = iv.get_type().get_bit_width();
            if bw == 1 {
                env.builder.build_not(iv, "lnot").unwrap().as_basic_value_enum()
            } else {
                let zero = iv.get_type().const_zero();
                env.builder
                    .build_int_compare(IntPredicate::EQ, iv, zero, "lnot")
                    .unwrap()
                    .as_basic_value_enum()
            }
        }

        // ~ (bitwise not)
        (Operator::BitwiseNot, BasicValueEnum::IntValue(iv)) => {
            env.builder.build_not(iv, "bnot").unwrap().as_basic_value_enum()
        }

        _ => panic!("Unsupported unary operator {:?} for value {:?}", operator, val),
    }
}
