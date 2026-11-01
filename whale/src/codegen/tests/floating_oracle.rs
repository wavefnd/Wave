// SPDX-License-Identifier: MPL-2.0
//! Execute the actual generated floating IR. Hex expectations are IEEE storage
//! boundaries, not results computed through the frontend's f64 literal path.
use super::{interpret_checked, module};
use whale_ir::*;

fn run(ir: &Module, arguments: &[ConstValue]) -> Result<Option<ConstValue>, InterpreterError> {
    interpret_checked(
        ir,
        ir.functions[0].id,
        arguments,
        InterpreterOptions {
            max_steps: 2000,
            max_call_depth: 16,
            max_frames: 64,
            max_live_values: 4096,
            max_trace_frames: 16,
            ir_limits: IrLimits { max_nodes: 20000, ..IrLimits::default() },
            memory_limits: MemoryLimits {
                max_bytes: 4096,
                max_allocations: 128,
                max_pointer_fragments: 128,
                max_work: 100000,
            },
        },
    )
    .map(|result| result.value)
}

fn f32(bits: u32) -> ConstValue {
    ConstValue::F(FloatBits::F32(bits))
}

fn f64(bits: u64) -> ConstValue {
    ConstValue::F(FloatBits::F64(bits))
}

#[test]
fn f32_rounding_subnormals_signed_zero_and_special_values_have_exact_results() {
    for (operator, a, b, expected) in [
        ("+", 0x3f800000, 0x33800000, 0x3f800000), // tie -> even
        ("+", 0x3f800001, 0x33800000, 0x3f800002),
        ("+", 1, 1, 2),
        ("*", 0x7f7fffff, 0x40000000, 0x7f800000),
        ("*", 0x80000000, 0x3f800000, 0x80000000),
        ("/", 0x3f800000, 0, 0x7f800000),
        ("/", 0, 0, 0x7fc00000),
        ("%", 0xc0a00000, 0x40000000, 0xbf800000), // -5 fmod 2 = -1
    ] {
        let ir = module(&format!("fun probe(a: f32, b: f32) -> f32 {{ return a {operator} b; }}"));
        assert_eq!(
            run(&ir, &[f32(a), f32(b)]).unwrap(),
            Some(f32(expected)),
            "{operator}: {a:x} {b:x}"
        );
    }
}

#[test]
fn f64_rounding_subnormals_and_ordered_width_conversions_are_explicit() {
    let add = module("fun probe(a: f64, b: f64) -> f64 { return a + b; }");
    for (a, b, c) in [
        (0x3ff0000000000000, 0x3ca0000000000000, 0x3ff0000000000000),
        (0x3ff0000000000001, 0x3ca0000000000000, 0x3ff0000000000002),
        (1, 1, 2),
    ] {
        assert_eq!(run(&add, &[f64(a), f64(b)]).unwrap(), Some(f64(c)));
    }
    let cast = module("fun probe(n: f64) -> f64 { return (n as f32) as f64; }");
    assert_eq!(run(&cast, &[f64(0x3ff0000010000000)]).unwrap(), Some(f64(0x3ff0000000000000)));
}

#[test]
fn negation_preserves_nan_payloads_and_truthiness_distinguishes_only_zero() {
    for bits in [32, 64] {
        let neg = module(&format!("fun probe(n: f{bits}) -> f{bits} {{ return -n; }}"));
        let truth = module(&format!("fun probe(n: f{bits}) -> bool {{ return n as bool; }}"));
        let (sign, infinity, nan) = if bits == 32 {
            (1u64 << 31, 0x7f800000, 0x7f800123)
        } else {
            (1u64 << 63, 0x7ff0000000000000, 0x7ff0000000000123)
        };
        let pack = |value| if bits == 32 { f32(value as u32) } else { f64(value) };
        for raw in [0, sign, 1, infinity, nan] {
            assert_eq!(run(&neg, &[pack(raw)]).unwrap(), Some(pack(raw ^ sign)));
            assert_eq!(
                run(&truth, &[pack(raw)]).unwrap(),
                Some(ConstValue::Bool(raw & !sign != 0))
            );
        }
    }
}

#[test]
fn float_to_integer_traps_are_executed_and_short_circuiting_skips_invalid_casts() {
    let cast = module("fun probe(n: f64) -> i32 { return n as i32; }");
    for raw in [0x7ff8000000000000, 0x7ff0000000000000, 0x41e0000000000000] {
        assert!(matches!(run(&cast, &[f64(raw)]), Err(InterpreterError::Trap(_))));
    }
    assert_eq!(
        run(&cast, &[ConstValue::F(FloatBits::from_f64(64, -1.75).unwrap())]).unwrap(),
        Some(ConstValue::I(-1))
    );
    let skipped =
        module("fun probe(flag: bool, n: f64) -> bool { return flag && ((n as i32) > 0); }");
    assert_eq!(
        run(&skipped, &[ConstValue::Bool(false), f64(0x7ff8000000000000)]).unwrap(),
        Some(ConstValue::Bool(false))
    );
    assert!(run(&skipped, &[ConstValue::Bool(true), f64(0x7ff8000000000000)]).is_err());
}
