// SPDX-License-Identifier: MPL-2.0
use super::module;
use whale_ir::*;

fn run(source: &str, input: i128) -> Option<ConstValue> {
    let ir = module(source);
    super::interpret_checked(
        &ir,
        ir.functions[0].id,
        &[ConstValue::I(input)],
        InterpreterOptions { max_steps: 4000, ..InterpreterOptions::default() },
    )
    .unwrap()
    .value
}

#[test]
fn for_continue_executes_increment_and_break_bypasses_it() {
    let source = r#"
fun probe(limit: i32) -> i32 {
    var ticks: i32 = 0;
    var sum: i32 = 0;
    for (var i: i32 = 0; i < limit; i = i + (ticks = ticks + 1)) {
        if (i == 0) { continue; }
        if (i == 3) { break; }
        sum = sum + i;
    }
    return ticks * 10 + sum;
}
"#;
    for (limit, expected) in [(0, 0), (1, 10), (3, 21), (4, 21), (9, 21)] {
        assert_eq!(run(source, limit), Some(ConstValue::I(expected)));
    }
}

#[test]
fn for_initializer_and_body_shadowing_keep_their_own_scopes() {
    let source = r#"
fun probe(limit: i32) -> i32 {
    var i: i32 = 7;
    var sum: i32 = 0;
    for (var i: i32 = i - 7; i < limit; i = i + 1) {
        if (true) {
            var i: i32 = i + 10;
            sum = sum + i;
        }
    }
    return sum + i;
}
"#;
    assert_eq!(run(source, 0), Some(ConstValue::I(7)));
    assert_eq!(run(source, 3), Some(ConstValue::I(40)));
}

#[test]
fn nested_for_and_while_controls_target_the_innermost_loop() {
    let source = r#"
fun probe(limit: i32) -> i32 {
    var result: i32 = 0;
    for (var i: i32 = 0; i < limit; i += 1) {
        for (var j: i32 = 0; j < 4; j = j + 1) {
            if (j == 1) { continue; }
            if (j == 3) { break; }
            result += 1;
        }
        var k: i32 = 0;
        while (k < 2) {
            k += 1;
            if (k == 1) { continue; }
            result += k;
        }
    }
    return result;
}
"#;
    assert_eq!(run(source, 3), Some(ConstValue::I(12)));
}

#[test]
fn for_increment_calls_run_once_and_return_skips_the_increment() {
    let source = r#"
fun probe(limit: i32) -> i32 {
    var count: i32 = 0;
    for (var i: i32 = 0; i < limit; i = next(i)) {
        count = count + 1;
        if (i == 2) { return count; }
    }
    return count;
}
fun next(n: i32) -> i32 { return n + 1; }
"#;
    assert_eq!(run(source, 0), Some(ConstValue::I(0)));
    assert_eq!(run(source, 9), Some(ConstValue::I(3)));
}
