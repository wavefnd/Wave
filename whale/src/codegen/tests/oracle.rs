// SPDX-License-Identifier: MPL-2.0
//! Execute the supported integer subset with the upstream bounded interpreter.
//! Direct integer/void calls share execution budgets. Direct floating coverage
//! lives in floating_oracle; external calls remain explicit unsupported outcomes.
use super::{mask, module};
use whale_ir::*;

fn options() -> InterpreterOptions {
    InterpreterOptions {
        max_steps: 10_000,
        max_call_depth: 32,
        max_frames: 256,
        max_live_values: 8192,
        max_trace_frames: 32,
        ir_limits: IrLimits { max_nodes: 20_000, ..IrLimits::default() },
        memory_limits: MemoryLimits {
            max_bytes: 4096,
            max_allocations: 128,
            max_pointer_fragments: 128,
            max_work: 100_000,
        },
    }
}

fn run(module: &Module, arguments: &[ConstValue]) -> Result<InterpreterResult, InterpreterError> {
    super::interpret_checked(module, module.functions[0].id, arguments, options())
}

fn integer(raw: u128, bits: u32, signed: bool) -> ConstValue {
    let raw = raw & mask(bits);
    if signed {
        let shift = 128 - bits;
        ConstValue::I(((raw << shift) as i128) >> shift)
    } else {
        ConstValue::U(raw)
    }
}

#[test]
fn wrapping_arithmetic_executes_at_every_supported_integer_width() {
    for bits in [8, 16, 32, 64, 128] {
        for signed in [false, true] {
            let ty = format!("{}{bits}", if signed { "i" } else { "u" });
            let ir = module(&format!(
                "fun probe(a: {ty}, b: {ty}) -> {ty} {{ return (a + b) * b - b; }}"
            ));
            for (a, b) in
                [(0, 1), (mask(bits), 2), (1 << (bits - 1), 3), (mask(bits) - 1, mask(bits))]
            {
                let expected = a.wrapping_add(b).wrapping_mul(b).wrapping_sub(b);
                let result =
                    run(&ir, &[integer(a, bits, signed), integer(b, bits, signed)]).unwrap();
                assert_eq!(result.value, Some(integer(expected, bits, signed)), "{ty}: {a}, {b}");
                assert!(result.steps > 0 && result.steps < options().max_steps);
            }
        }
    }
}

#[test]
fn ordered_truncation_extension_and_equal_width_signedness_are_observable() {
    let ir = module("fun probe(n: i32) -> i32 { return (n as u8) as i32; }");
    for n in [-257, -1, 0, 255, 256, 257, i32::MAX] {
        assert_eq!(
            run(&ir, &[ConstValue::I(n as i128)]).unwrap().value,
            Some(ConstValue::I((n as u8) as i128))
        );
    }
    let ir = module("fun probe(n: u32) -> i32 { return n as i32; }");
    for n in [0, 1, 0x80000000, u32::MAX] {
        assert_eq!(
            run(&ir, &[ConstValue::U(n as u128)]).unwrap().value,
            Some(ConstValue::I((n as i32) as i128))
        );
    }
    let ir = module("fun probe(n: i8) -> u64 { return n as u64; }");
    assert_eq!(
        run(&ir, &[ConstValue::I(-1)]).unwrap().value,
        Some(ConstValue::U(u64::MAX as u128))
    );
}

#[test]
fn shifts_execute_or_trap_on_original_counts_before_narrowing() {
    for bits in [8, 16, 32, 64, 128] {
        for signed in [false, true] {
            let ty = format!("{}{bits}", if signed { "i" } else { "u" });
            let raw = mask(bits) - 1;
            for count_bits in [8, 16, 32, 64, 128] {
                for count_signed in [false, true] {
                    let rhs = format!("{}{count_bits}", if count_signed { "i" } else { "u" });
                    for op in ["<<", ">>"] {
                        let ir = module(&format!(
                            "fun probe(x: {ty}, n: {rhs}) -> {ty} {{ return x {op} n; }}"
                        ));
                        for count in
                            [0, 1, bits as u128 - 1, bits as u128, bits as u128 + 1, 256, u128::MAX]
                        {
                            let count = count & mask(count_bits);
                            let negative = count_signed && count & (1 << (count_bits - 1)) != 0;
                            let result = run(
                                &ir,
                                &[
                                    integer(raw, bits, signed),
                                    integer(count, count_bits, count_signed),
                                ],
                            );
                            if negative || count >= bits as u128 {
                                assert!(
                                    matches!(result, Err(InterpreterError::Trap(InterpreterTrap {
                                    reason: TrapReason::Explicit(ref reason), ..
                                })) if reason == "shift count out of range"),
                                    "{ty} {op} {rhs}({count}): {result:?}"
                                );
                            } else {
                                let shift = count as u32;
                                let expected = if op == "<<" {
                                    raw << shift
                                } else if signed {
                                    let ConstValue::I(n) = integer(raw, bits, true) else {
                                        unreachable!()
                                    };
                                    (n >> shift) as u128
                                } else {
                                    raw >> shift
                                };
                                assert_eq!(
                                    result.unwrap().value,
                                    Some(integer(expected, bits, signed)),
                                    "{ty} {op} {rhs}({count})"
                                );
                            }
                        }
                    }
                }
            }
        }
    }
}

#[test]
fn logical_conversions_short_circuit_traps_and_loop_storage_stays_initialized() {
    let ir =
        module("fun probe(flag: i32, n: u64) -> bool { return flag && (((1 as u8) << n) != 0); }");
    assert_eq!(
        run(&ir, &[ConstValue::I(0), ConstValue::U(8)]).unwrap().value,
        Some(ConstValue::Bool(false))
    );
    assert_eq!(
        run(&ir, &[ConstValue::I(-1), ConstValue::U(7)]).unwrap().value,
        Some(ConstValue::Bool(true))
    );
    assert!(matches!(
        run(&ir, &[ConstValue::I(1), ConstValue::U(8)]),
        Err(InterpreterError::Trap(_))
    ));
    let ir =
        module("fun probe(flag: i32, n: u64) -> bool { return flag || (((1 as u8) << n) != 0); }");
    assert_eq!(
        run(&ir, &[ConstValue::I(3), ConstValue::U(8)]).unwrap().value,
        Some(ConstValue::Bool(true))
    );
    let ir = module(
        r#"
fun probe(limit: i32) -> i32 {
    var n: i32 = 0;
    var sum: i32 = 0;
    while (n < limit) {
        n = n + 1;
        if (n == 2) { continue; }
        if (n == 5) { break; }
        var sum: i32 = sum + 10;
        if (sum < 10) { return -1; }
    }
    return sum + n;
}
"#,
    );
    for (limit, expected) in [(0, 0), (1, 1), (3, 3), (6, 5)] {
        assert_eq!(run(&ir, &[ConstValue::I(limit)]).unwrap().value, Some(ConstValue::I(expected)));
    }
}

#[test]
fn numeric_condition_truthiness_executes_without_reinferring_hir_types() {
    for ty in ["i8", "i32", "i64", "i128", "u8", "u64", "u128", "byte"] {
        let signed = ty.starts_with('i');
        let ir = module(&format!(
            "fun probe(n: {ty}) -> i32 {{ if ((n)) {{ return 7; }} else if (2) {{ return 3; }} else {{ return 9; }} }}"
        ));
        for n in [0, 1, 2] {
            let arg = if signed { ConstValue::I(n) } else { ConstValue::U(n as u128) };
            assert_eq!(
                run(&ir, &[arg]).unwrap().value,
                Some(ConstValue::I(if n == 0 { 3 } else { 7 }))
            );
        }
    }
    let ir = module("fun probe(n: i32) -> i32 { var x: i32 = n; while (x) { x = 0; } return x; }");
    assert_eq!(run(&ir, &[ConstValue::I(-3)]).unwrap().value, Some(ConstValue::I(0)));
}

#[test]
fn integer_literal_truthiness_checks_the_value_before_any_narrowing() {
    for literal in [
        "0",
        "1",
        "2147483648",
        "4294967296",
        "-4294967296",
        "18446744073709551616",
        "170141183460469231731687303715884105727",
    ] {
        let nonzero = literal != "0";
        for expression in [
            literal.to_owned(),
            format!("({literal})"),
            format!("{literal} as bool"),
            format!("true && {literal}"),
            format!("false || {literal}"),
            format!("!{literal}"),
        ] {
            let ir = module(&format!(
                "fun probe() -> i32 {{ if ({expression}) {{ return 7; }} return 0; }}"
            ));
            let expected = if expression.starts_with('!') { !nonzero } else { nonzero };
            assert_eq!(
                run(&ir, &[]).unwrap().value,
                Some(ConstValue::I(if expected { 7 } else { 0 })),
                "{expression}"
            );
        }
    }
    let ir = module("fun probe() -> bool { return (4294967296 as i32) as bool; }");
    assert_eq!(run(&ir, &[]).unwrap().value, Some(ConstValue::Bool(false)));
}

#[test]
fn budgets_unsupported_operations_and_traps_have_distinct_outcomes() {
    let ir = module(
        "fun probe(n: i32) -> i32 { var x: i32 = n; while (x >= 0) { x = x + 1; } return x; }",
    );
    let limited = InterpreterOptions { max_steps: 40, ..options() };
    assert!(matches!(
        super::interpret_checked(&ir, ir.functions[0].id, &[ConstValue::I(0)], limited),
        Err(InterpreterError::StepLimit { limit: 40, .. })
    ));
    let ir = module("fun probe(n: i64) -> i64 { var x: i64 = n; return x; }");
    let limited = InterpreterOptions {
        memory_limits: MemoryLimits { max_bytes: 1, ..options().memory_limits },
        ..options()
    };
    assert!(matches!(
        super::interpret_checked(&ir, ir.functions[0].id, &[ConstValue::I(0)], limited),
        Err(InterpreterError::MemoryLimit { .. })
    ));
    let ir = module("fun probe(n: i32) -> i32 { var f: f64 = n as f64; return f as i32; }");
    assert_eq!(run(&ir, &[ConstValue::I(0)]).unwrap().value, Some(ConstValue::I(0)));
}

#[test]
fn nested_direct_calls_preserve_integer_widths_and_argument_order() {
    for bits in [8, 16, 32, 64, 128] {
        for signed in [false, true] {
            let ty = format!("{}{bits}", if signed { "i" } else { "u" });
            let ir = module(&format!(
                "fun probe(n: {ty}) -> {ty} {{ return outer(n, 3); }}
                 fun outer(a: {ty}, b: {ty}) -> {ty} {{ return leaf(a - b); }}
                 fun leaf(n: {ty}) -> {ty} {{ return n + 1; }}"
            ));
            for raw in [0, 1, mask(bits), 1 << (bits - 1)] {
                let expected = raw.wrapping_sub(3).wrapping_add(1);
                assert_eq!(
                    run(&ir, &[integer(raw, bits, signed)]).unwrap().value,
                    Some(integer(expected, bits, signed)),
                    "{ty}({raw})"
                );
            }
        }
    }
}

#[test]
fn recursive_calls_keep_each_invocations_locals_until_return() {
    let ir = module(
        r#"
fun probe(n: u32) -> u32 { return sum(n); }
fun sum(n: u32) -> u32 {
    var saved: u32 = n;
    if (n == 0) { return 0; }
    return saved + sum(n - 1);
}
"#,
    );
    for n in [0, 1, 2, 6, 12] {
        assert_eq!(
            run(&ir, &[ConstValue::U(n)]).unwrap().value,
            Some(ConstValue::U(n * (n + 1) / 2))
        );
    }
}

#[test]
fn grouped_and_returned_void_calls_execute_once_even_without_results() {
    let ir = module(
        r#"
fun probe(n: u64) {
    ((grouped(n)));
    return ((returned(n)));
}
fun grouped(n: u64) { ((leaf(n))); }
fun returned(n: u64) { return ((leaf(n))); }
fun leaf(n: u64) { var shifted: u8 = (1 as u8) << n; }
"#,
    );
    // Entry + two wrappers + two leaves. A skipped call needs fewer frames;
    // duplicated evaluation exceeds this budget even when its result is void.
    let exact = InterpreterOptions { max_call_depth: 3, max_frames: 5, ..options() };
    assert_eq!(
        super::interpret_checked(&ir, ir.functions[0].id, &[ConstValue::U(0)], exact)
            .unwrap()
            .value,
        None
    );
    let returned = ir.functions.iter().find(|f| f.name == "returned").unwrap().id;
    assert!(matches!(
        super::interpret_checked(
            &ir,
            ir.functions[0].id,
            &[ConstValue::U(0)],
            InterpreterOptions { max_frames: 4, ..exact }
        ),
        Err(InterpreterError::CallLimit { site, resource: CallResource::Frames, limit: 4 })
            if site.function == returned
    ));
    let leaf = ir.functions.iter().find(|f| f.name == "leaf").unwrap().id;
    assert!(matches!(
        run(&ir, &[ConstValue::U(8)]),
        Err(InterpreterError::Trap(InterpreterTrap { site, reason: TrapReason::Explicit(ref reason), .. }))
            if site.function == leaf && reason == "shift count out of range"
    ));
}

#[test]
fn short_circuiting_skips_calls_but_reached_unused_results_still_trap() {
    for (operator, skipped, entered) in [("&&", false, true), ("||", true, false)] {
        let ir = module(&format!(
            "fun probe(flag: bool, n: u64) -> bool {{ return flag {operator} helper(n); }}
             fun helper(n: u64) -> bool {{ return ((1 as u8) << n) != 0; }}"
        ));
        let only_entry = InterpreterOptions { max_call_depth: 1, max_frames: 1, ..options() };
        assert_eq!(
            super::interpret_checked(
                &ir,
                ir.functions[0].id,
                &[ConstValue::Bool(skipped), ConstValue::U(8)],
                only_entry
            )
            .unwrap()
            .value,
            Some(ConstValue::Bool(skipped))
        );
        assert!(matches!(
            run(&ir, &[ConstValue::Bool(entered), ConstValue::U(8)]),
            Err(InterpreterError::Trap(InterpreterTrap { reason: TrapReason::Explicit(ref reason), .. }))
                if reason == "shift count out of range"
        ));
        assert_eq!(
            run(&ir, &[ConstValue::Bool(entered), ConstValue::U(0)]).unwrap().value,
            Some(ConstValue::Bool(true))
        );
    }
    let ir = module(
        "fun probe(n: u64) -> i32 { helper(n); return 7; }
         fun helper(n: u64) -> u8 { return (1 as u8) << n; }",
    );
    assert_eq!(run(&ir, &[ConstValue::U(0)]).unwrap().value, Some(ConstValue::I(7)));
    assert!(matches!(run(&ir, &[ConstValue::U(8)]), Err(InterpreterError::Trap(_))));
}

#[test]
fn calls_share_fuel_memory_and_distinct_frame_and_live_value_limits() {
    let ir = module(
        "fun probe(n: u32) -> u32 { var first: u32 = leaf(n); return leaf(first); }
         fun leaf(n: u32) -> u32 { var copy: u32 = n; return copy + 1; }",
    );
    let entry = ir.functions[0].id;
    let arguments = [ConstValue::U(3)];
    let exact = InterpreterOptions { max_call_depth: 2, max_frames: 3, ..options() };
    let complete = super::interpret_checked(&ir, entry, &arguments, exact).unwrap();
    assert_eq!(complete.value, Some(ConstValue::U(5)));
    for (limited, resource, limit) in [
        (InterpreterOptions { max_call_depth: 1, ..exact }, CallResource::Depth, 1),
        (InterpreterOptions { max_frames: 2, ..exact }, CallResource::Frames, 2),
    ] {
        assert!(matches!(
            super::interpret_checked(&ir, entry, &arguments, limited),
            Err(InterpreterError::CallLimit { site, resource: actual, limit: actual_limit })
                if site.function == entry && actual == resource && actual_limit == limit
        ));
    }
    let required_values: usize = ir.functions.iter().map(|f| f.value_types.len()).sum();
    assert_eq!(
        super::interpret_checked(
            &ir,
            entry,
            &arguments,
            InterpreterOptions { max_live_values: required_values, ..exact }
        )
        .unwrap()
        .value,
        complete.value
    );
    assert!(matches!(
        super::interpret_checked(
            &ir,
            entry,
            &arguments,
            InterpreterOptions { max_live_values: required_values - 1, ..exact }
        ),
        Err(InterpreterError::CallLimit { site, resource: CallResource::Values, limit })
            if site.function == entry && limit == (required_values - 1) as u64
    ));
    let fuel = complete.steps - 1;
    assert!(matches!(
        super::interpret_checked(
            &ir,
            entry,
            &arguments,
            InterpreterOptions { max_steps: fuel, ..exact }
        ),
        Err(InterpreterError::StepLimit { limit, .. }) if limit == fuel
    ));
    // Each individual function fits, but their allocations share one arena
    // and retired invocation allocations remain charged to its total budget.
    let memory = InterpreterOptions {
        memory_limits: MemoryLimits { max_bytes: 16, ..exact.memory_limits },
        ..exact
    };
    let leaf = ir.functions.iter().find(|f| f.name == "leaf").unwrap().id;
    assert_eq!(
        super::interpret_checked(&ir, leaf, &arguments, memory).unwrap().value,
        Some(ConstValue::U(4))
    );
    assert!(matches!(
        super::interpret_checked(&ir, entry, &arguments, memory),
        Err(InterpreterError::MemoryLimit {
            site,
            resource: MemoryResource::Bytes,
            limit: 16,
        }) if site.function == leaf
    ));
}

#[test]
fn recursive_calls_exhaust_shared_depth_and_total_frame_budgets() {
    let ir = module(
        "fun probe(n: u32) -> u32 {
             if (n == 0) { return 0; }
             return n + probe(n - 1);
         }",
    );
    let entry = ir.functions[0].id;
    for (limited, resource) in [
        (InterpreterOptions { max_call_depth: 4, ..options() }, CallResource::Depth),
        (InterpreterOptions { max_frames: 4, ..options() }, CallResource::Frames),
    ] {
        assert!(matches!(
            super::interpret_checked(&ir, entry, &[ConstValue::U(5)], limited),
            Err(InterpreterError::CallLimit { site, resource: actual, limit: 4 })
                if site.function == entry && actual == resource
        ));
    }
    let exact = InterpreterOptions { max_call_depth: 6, max_frames: 6, ..options() };
    assert_eq!(
        super::interpret_checked(&ir, entry, &[ConstValue::U(5)], exact).unwrap().value,
        Some(ConstValue::U(15))
    );
}

#[test]
fn unsupported_callees_are_rejected_before_runtime_effects() {
    let ir = module(
        "fun probe(n: u64) -> i32 {
             var shifted: u8 = (1 as u8) << n;
             return helper();
         }
         fun helper() -> i32 { var f: f64 = 1.0; return f as i32; }",
    );
    assert!(matches!(run(&ir, &[ConstValue::U(8)]), Err(InterpreterError::Trap(_))));
    assert_eq!(run(&ir, &[ConstValue::U(0)]).unwrap().value, Some(ConstValue::I(1)));

    let mut ir = module(
        "fun probe(n: i32) -> i32 { return helper(n); }
         fun helper(n: i32) -> i32 { return n + 1; }",
    );
    let helper = ir.functions.iter().find(|f| f.name == "helper").unwrap().id;
    // The Wave adapter still rejects external definitions. Turn a generated
    // callee into a verifier-valid declaration to test the oracle boundary.
    ir.functions.retain(|f| f.id != helper);
    let declaration = ir.declarations.iter_mut().find(|d| d.id == helper).unwrap();
    declaration.linkage = Linkage::External;
    declaration.link_name = Some("external_helper".into());
    verify_module(&ir).unwrap();
    let text = print_module(&ir);
    let ir = parse_module(&text).unwrap();
    assert_eq!(print_module(&ir), text);
    assert!(matches!(
        run(&ir, &[ConstValue::I(0)]),
        Err(InterpreterError::UnsupportedInstruction { operation: "call", .. })
    ));
}
