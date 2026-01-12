use super::ExprGenEnv;
use crate::llvm_temporary::llvm_codegen::generate_address_ir;
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::{Expression, IncDecKind};

pub(crate) fn gen<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    kind: &IncDecKind,
    target: &Expression,
) -> BasicValueEnum<'ctx> {
    let ptr = generate_address_ir(
        env.context, env.builder, target, env.variables, env.module, env.struct_types, env.struct_field_indices
    );
    let old_val = env.builder.build_load(ptr, "incdec_old").unwrap();

    let new_val: BasicValueEnum<'ctx> = match old_val {
        BasicValueEnum::IntValue(iv) => {
            if iv.get_type().get_bit_width() == 1 {
                panic!("++/-- not allowed on bool");
            }

            let one = iv.get_type().const_int(1, false);
            let nv = match kind {
                IncDecKind::PreInc | IncDecKind::PostInc => env.builder.build_int_add(iv, one, "inc").unwrap(),
                IncDecKind::PreDec | IncDecKind::PostDec => env.builder.build_int_sub(iv, one, "dec").unwrap(),
            };
            nv.as_basic_value_enum()
        }

        BasicValueEnum::FloatValue(fv) => {
            let one = fv.get_type().const_float(1.0);
            let nv = match kind {
                IncDecKind::PreInc | IncDecKind::PostInc => env.builder.build_float_add(fv, one, "finc").unwrap(),
                IncDecKind::PreDec | IncDecKind::PostDec => env.builder.build_float_sub(fv, one, "fdec").unwrap(),
            };
            nv.as_basic_value_enum()
        }

        BasicValueEnum::PointerValue(pv) => {
            let idx = match kind {
                IncDecKind::PreInc | IncDecKind::PostInc => env.context.i64_type().const_int(1, true),
                IncDecKind::PreDec | IncDecKind::PostDec => env.context.i64_type().const_int((-1i64) as u64, true),
            };
            let gep = unsafe { env.builder.build_in_bounds_gep(pv, &[idx], "pincdec").unwrap() };
            gep.as_basic_value_enum()
        }

        _ => panic!("Unsupported type for ++/--: {:?}", old_val),
    };

    env.builder.build_store(ptr, new_val).unwrap();

    match kind {
        IncDecKind::PreInc | IncDecKind::PreDec => new_val,
        IncDecKind::PostInc | IncDecKind::PostDec => old_val,
    }
}
