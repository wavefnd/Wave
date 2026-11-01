// SPDX-License-Identifier: MPL-2.0
//! Check IEEE bit patterns with the upstream integer oracle. This executes a
//! bit-preserving integer-carrier projection, not Whale floating arithmetic.
use super::module;
use whale_ir::*;

fn carrier(ty: &Type) -> Type {
    match ty {
        Type::F32 => Type::U32,
        Type::F64 => Type::U64,
        Type::Ptr(inner) => Type::ptr_to(carrier(inner)),
        _ => ty.clone(),
    }
}

fn integer_carrier(mut ir: Module) -> Module {
    for declaration in &mut ir.declarations {
        declaration.signature.ret = carrier(&declaration.signature.ret);
        for ty in &mut declaration.signature.params {
            *ty = carrier(ty);
        }
    }
    for function in &mut ir.functions {
        function.ret_ty = carrier(&function.ret_ty);
        for parameter in &mut function.params {
            parameter.ty = carrier(&parameter.ty);
        }
        for (_, ty) in &mut function.value_types {
            *ty = carrier(ty);
        }
        for block in &mut function.blocks {
            for instruction in &mut block.instructions {
                match instruction {
                    Instruction::Alloca { ty, .. }
                    | Instruction::Store { ty, .. }
                    | Instruction::Load { ty, .. } => *ty = carrier(ty),
                    Instruction::Cast { op: CastOp::Bitcast, src_ty, dst_ty, .. } => {
                        *src_ty = carrier(src_ty);
                        *dst_ty = carrier(dst_ty);
                    },
                    Instruction::Const { ty, value: ConstValue::U(_), .. }
                    | Instruction::Bin { op: BinOp::Xor, ty, .. } => *ty = carrier(ty),
                    other => panic!("not a bit-preserving operation: {other:?}"),
                }
            }
            let Some(Terminator::Ret { ty, .. }) = &mut block.terminator else {
                panic!("negation probe must return its bits")
            };
            *ty = carrier(ty);
        }
    }
    verify_module(&ir).unwrap();
    ir
}

#[test]
fn floating_negation_flips_only_the_sign_bit_and_double_negation_restores_payloads() {
    for (bits, patterns) in [
        (
            32,
            vec![
                0, 0x80000000, 1, 0x80000001, 0x3f800000, 0x7f7fffff, 0x7f800000, 0xff800000,
                0x7f800001, 0xff800001, 0x7fc01234, 0xffc01234,
            ],
        ),
        (
            64,
            vec![
                0,
                0x8000000000000000,
                1,
                0x8000000000000001,
                0x3ff0000000000000,
                0x7fefffffffffffff,
                0x7ff0000000000000,
                0xfff0000000000000,
                0x7ff0000000000001,
                0xfff0000000000001,
                0x7ff8000000001234,
                0xfff8000000001234,
            ],
        ),
    ] {
        for (expression, sign) in [("-((n))", 1u128 << (bits - 1)), ("-(-n)", 0)] {
            let verified =
                module(&format!("fun probe(n: f{bits}) -> f{bits} {{ return {expression}; }}"));
            let ir = integer_carrier(verified);
            for pattern in &patterns {
                let options = InterpreterOptions {
                    max_steps: 100,
                    max_call_depth: 1,
                    max_frames: 1,
                    max_live_values: 128,
                    max_trace_frames: 32,
                    ir_limits: IrLimits { max_nodes: 1000, ..IrLimits::default() },
                    memory_limits: MemoryLimits {
                        max_bytes: 64,
                        max_allocations: 4,
                        max_pointer_fragments: 4,
                        max_work: 1000,
                    },
                };
                let result = super::interpret_checked(
                    &ir,
                    ir.functions[0].id,
                    &[ConstValue::U(*pattern)],
                    options,
                )
                .unwrap();
                assert_eq!(
                    result.value,
                    Some(ConstValue::U(pattern ^ sign)),
                    "f{bits} {expression}({pattern:#x})"
                );
            }
        }
    }
}
