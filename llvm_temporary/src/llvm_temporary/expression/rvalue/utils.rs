use inkwell::builder::Builder;
use inkwell::values::IntValue;
use inkwell::IntPredicate;

pub(crate) fn to_bool<'ctx>(builder: &Builder<'ctx>, v: IntValue<'ctx>) -> IntValue<'ctx> {
    if v.get_type().get_bit_width() == 1 {
        return v;
    }

    let zero = v.get_type().const_zero();
    builder
        .build_int_compare(IntPredicate::NE, v, zero, "tobool")
        .unwrap()
}
