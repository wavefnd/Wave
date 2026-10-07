// SPDX-License-Identifier: MPL-2.0
use super::*;
use utils::const_int::ConstInt;

fn module(source: &str) -> Module {
    let tokens = lexer::Lexer::new_with_file(source, "numeric.wave").tokenize().unwrap();
    let syntax = parser::parse_syntax_with_spans(&tokens).unwrap();
    let program = TypedProgram::lower(syntax).unwrap();
    let module = lower(&program).unwrap();
    let text = print_module(&module);
    let parsed = parse_module(&text).unwrap_or_else(|e| panic!("{e:?}\n{text}"));
    assert_eq!(print_module(&parsed), text, "IR serialization must round-trip exactly");
    parsed
}

fn width(ty: &Type) -> u32 {
    match ty {
        Type::I8 | Type::U8 => 8,
        Type::I16 | Type::U16 => 16,
        Type::I32 | Type::U32 => 32,
        Type::I64 | Type::U64 => 64,
        Type::I128 | Type::U128 => 128,
        _ => panic!("not an integer: {ty}"),
    }
}

fn mask(bits: u32) -> u128 {
    u128::MAX >> (128 - bits)
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Probe {
    Bits(u128),
    Float(f64),
    Bool(bool),
}

// Evaluate only the emitted straight-line guard prefix, stopping before the
// guarded operation. This is a test probe, not a Whale execution backend.
fn guard_accepts(function: &Function, arguments: &[Probe]) -> bool {
    let mut values = HashMap::new();
    let mut slots = HashMap::new();
    assert_eq!(function.blocks.len(), 1);
    assert_eq!(function.params.len(), arguments.len());
    for (parameter, argument) in function.params.iter().zip(arguments) {
        values.insert(parameter.id, *argument);
    }
    let mut traps = 0;
    for instruction in &function.blocks[0].instructions {
        let (dst, value) = match instruction {
            Instruction::Alloca { .. } => continue,
            Instruction::Store { ptr, value, .. } => {
                slots.insert(*ptr, values[value]);
                continue;
            },
            Instruction::Load { dst, ptr, .. } => (*dst, slots[ptr]),
            Instruction::Const { dst, ty, value } => (
                *dst,
                match value {
                    ConstValue::I(n) => Probe::Bits(*n as u128 & mask(width(ty))),
                    ConstValue::U(n) => Probe::Bits(*n & mask(width(ty))),
                    ConstValue::Bool(b) => Probe::Bool(*b),
                    ConstValue::F(f) => Probe::Float(f.to_f64()),
                },
            ),
            Instruction::Cast { op: CastOp::FToI_S | CastOp::FToI_U, .. }
            | Instruction::Bin { op: BinOp::Shl | BinOp::LShr | BinOp::AShr, .. } => {
                assert!(traps > 0, "guarded operation must follow a trap check");
                return true;
            },
            Instruction::Cast { dst, src, src_ty, dst_ty, op } => {
                let Probe::Bits(mut n) = values[src] else { panic!("integer cast expected") };
                if *op == CastOp::SExt && n & (1 << (width(src_ty) - 1)) != 0 {
                    n |= !mask(width(src_ty));
                }
                assert!(matches!(
                    op,
                    CastOp::SExt | CastOp::ZExt | CastOp::Trunc | CastOp::Bitcast
                ));
                if width(dst_ty) < width(src_ty) {
                    assert!(traps > 0, "count narrowing must follow the range check");
                }
                (*dst, Probe::Bits(n & mask(width(dst_ty))))
            },
            Instruction::ICmp { dst, pred, lhs, rhs, .. } => {
                let result = match pred {
                    ICmpPred::Eq => values[lhs] == values[rhs],
                    ICmpPred::Ult => {
                        let (Probe::Bits(a), Probe::Bits(b)) = (values[lhs], values[rhs]) else {
                            panic!("integer comparison expected")
                        };
                        a < b
                    },
                    _ => panic!("unexpected guard predicate: {pred:?}"),
                };
                (*dst, Probe::Bool(result))
            },
            Instruction::FCmp { dst, pred, lhs, rhs, .. } => {
                let (Probe::Float(a), Probe::Float(b)) = (values[lhs], values[rhs]) else {
                    panic!("float comparison expected")
                };
                let result = match pred {
                    FCmpPred::Ogt => a > b,
                    FCmpPred::Oge => a >= b,
                    FCmpPred::Olt => a < b,
                    _ => panic!("unexpected guard predicate: {pred:?}"),
                };
                (*dst, Probe::Bool(result))
            },
            Instruction::TrapIf { cond, .. } => {
                traps += 1;
                match values[cond] {
                    Probe::Bool(true) => return false,
                    Probe::Bool(false) => continue,
                    _ => panic!("non-bool trap condition"),
                }
            },
            _ => panic!("unexpected guard instruction: {instruction:?}"),
        };
        values.insert(dst, value);
    }
    panic!("missing guarded operation");
}

#[test]
fn shift_guards_check_original_counts_before_narrowing() {
    for lhs_bits in [8, 16, 32, 64, 128] {
        for rhs_bits in [8, 16, 32, 64, 128] {
            for signed in [false, true] {
                let count_type = if signed { "i" } else { "u" };
                let module = module(&format!(
                    "fun probe(x: u{lhs_bits}, n: {count_type}{rhs_bits}) -> u{lhs_bits} {{ return x << n; }}"
                ));
                let function = &module.functions[0];
                for raw in [0, 1, lhs_bits - 1, lhs_bits, lhs_bits + 1, 256, u128::MAX] {
                    let bits = raw & mask(rhs_bits);
                    let negative = signed && bits & (1 << (rhs_bits - 1)) != 0;
                    assert_eq!(
                        guard_accepts(function, &[Probe::Bits(1), Probe::Bits(bits)]),
                        !negative && bits < lhs_bits,
                        "u{lhs_bits} << {count_type}{rhs_bits}({bits})"
                    );
                }
            }
        }
    }
}

#[test]
fn right_shift_opcode_and_result_follow_the_left_operand() {
    for (lhs, op) in [("i8", BinOp::AShr), ("u8", BinOp::LShr)] {
        let module = module(&format!("fun probe(x: {lhs}, n: u128) -> {lhs} {{ return x >> n; }}"));
        let function = &module.functions[0];
        let shift = function.blocks[0]
            .instructions
            .iter()
            .find_map(|i| match i {
                Instruction::Bin { op, ty, .. } => Some((op, ty)),
                _ => None,
            })
            .unwrap();
        assert_eq!(shift.0, &op);
        assert_eq!(shift.1, &function.ret_ty);
        assert_eq!(width(shift.1), 8);
    }
}

#[test]
fn float_guards_match_exact_truncated_integer_range() {
    for source in [32, 64] {
        for bits in [8, 16, 32, 64, 128] {
            for signed in [false, true] {
                let destination = format!("{}{bits}", if signed { "i" } else { "u" });
                let module = module(&format!(
                    "fun probe(n: f{source}) -> {destination} {{ return n as {destination}; }}"
                ));
                let upper = 2f64.powi(i32::from(bits - u16::from(signed)));
                let mut samples = vec![
                    0.0,
                    -0.0,
                    0.9,
                    -0.9,
                    1.0,
                    -1.0,
                    -1.1,
                    127.9,
                    -128.9,
                    -129.0,
                    255.9,
                    256.0,
                    upper,
                    -upper,
                    upper - 1.0,
                    -upper - 1.0,
                    f64::NAN,
                    f64::INFINITY,
                    f64::NEG_INFINITY,
                ];
                if source == 32 {
                    let raw = (upper as f32).to_bits();
                    for raw in [raw - 1, raw, raw + 1] {
                        let value = f32::from_bits(raw) as f64;
                        samples.extend([value, -value]);
                    }
                } else {
                    let raw = upper.to_bits();
                    for raw in [raw - 1, raw, raw + 1] {
                        let value = f64::from_bits(raw);
                        samples.extend([value, -value]);
                    }
                }
                for value in samples {
                    let value = if source == 32 { (value as f32) as f64 } else { value };
                    let expected = ConstInt::from_f64(value).is_some_and(|n| n.fits(bits, signed));
                    assert_eq!(
                        guard_accepts(&module.functions[0], &[Probe::Float(value)]),
                        expected,
                        "f{source}({value}) -> {destination}"
                    );
                }
                let cast = module.functions[0].blocks[0].instructions.last().unwrap();
                assert!(matches!(cast, Instruction::Cast { op, .. }
                    if *op == if signed { CastOp::FToI_S } else { CastOp::FToI_U }));
            }
        }
    }
}

#[test]
fn text_reader_accepts_wave_control_flow_calls_and_ordered_conversions() {
    module(
        r#"
fun narrow(x: i32) -> i32 { return (x as u8) as i32; }
fun probe(x: i32, n: u64, f: f64) -> i32 {
    var value: i32 = x;
    while (value > 0) {
        value = value - 1;
        if (value == 2) { continue; }
        if (value == 1) { break; }
    }
    if (value == 0 || (n < 4 && f > 0.0)) { return narrow(x >> n); }
    return f as i32;
}
"#,
    );
}

#[test]
fn wide_literal_counts_round_trip_without_enabling_wide_runtime_integers() {
    let module = module(
        r#"
fun literal(x: u128) -> u128 { return x << (127); }
fun maximum() -> u128 { return 340282366920938463463374607431768211455; }
fun skipped(x: i32) -> bool {
    return true || (x << 340282366920938463463374607431768211456) == 0;
}
fun nested(x: u8) -> u8 { return (x << 1) >> 1; }
"#,
    );
    assert!(guard_accepts(&module.functions[0], &[Probe::Bits(1)]));
    let skipped = &module.functions[2];
    assert!(!skipped.blocks[0]
        .instructions
        .iter()
        .any(|i| matches!(i, Instruction::TrapIf { .. })));
    assert!(skipped
        .blocks
        .iter()
        .skip(1)
        .any(|b| b.instructions.iter().any(|i| matches!(i, Instruction::TrapIf { .. }))));
}

#[test]
fn checked_float_cast_keeps_following_integer_conversions_in_order() {
    let module = module("fun probe(n: f64) -> i32 { return (n as i8) as i32; }");
    let instructions = &module.functions[0].blocks[0].instructions;
    let casts: Vec<_> = instructions
        .iter()
        .filter_map(|i| match i {
            Instruction::Cast { op, src_ty, dst_ty, .. } => Some((op, src_ty, dst_ty)),
            _ => None,
        })
        .collect();
    assert_eq!(
        casts,
        vec![(&CastOp::FToI_S, &Type::F64, &Type::I8), (&CastOp::SExt, &Type::I8, &Type::I32),]
    );
    assert!(!guard_accepts(&module.functions[0], &[Probe::Float(128.0)]));
    assert!(guard_accepts(&module.functions[0], &[Probe::Float(-128.9)]));
}
