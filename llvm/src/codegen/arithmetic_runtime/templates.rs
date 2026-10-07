// This file is part of the Wave language project.
// SPDX-License-Identifier: MPL-2.0
//! Small LLVM IR templates keep arithmetic algorithms auditable independently
//! of instruction insertion. No externally supplied builtin symbols are used.

pub(super) fn definition(name: &str, key: &str) -> String {
    let pieces: Vec<_> = key.split('.').collect();
    match pieces[0] {
        "udiv" | "sdiv" | "urem" | "srem" => division(name, pieces[0]),
        "mul" => multiply(name),
        "shl" | "lshr" | "ashr" => shift(name, pieces[0]),
        "uitofp" | "sitofp" => to_float(name, pieces[0] == "sitofp", pieces[2]),
        "fptoui" | "fptosi" => from_float(name, pieces[0] == "fptosi", pieces[1]),
        _ => unreachable!("validated runtime operation"),
    }
}

fn division(name: &str, op: &str) -> String {
    let signed = op.starts_with('s');
    let rem = op.ends_with("rem");
    let signs = if signed {
        r#"
  %aneg = icmp slt i128 %a, 0
  %bneg = icmp slt i128 %b, 0
  %na = sub i128 0, %a
  %nb = sub i128 0, %b
  %n = select i1 %aneg, i128 %na, i128 %a
  %d = select i1 %bneg, i128 %nb, i128 %b
"#
    } else {
        "  %n = or i128 %a, 0\n  %d = or i128 %b, 0\n"
    };
    let result = if rem { "%rn" } else { "%qn" };
    let finish = if signed {
        let sign = if rem { "%aneg" } else { "%sign" };
        format!("%sign = xor i1 %aneg, %bneg\n  %negative = sub i128 0, {result}\n  %result = select i1 {sign}, i128 %negative, i128 {result}\n  ret i128 %result")
    } else {
        format!("ret i128 {result}")
    };
    format!(
        r#"
define i128 @{name}(i128 %a, i128 %b) {{
entry:
{signs}
  %zero = icmp eq i128 %d, 0
  br i1 %zero, label %invalid, label %loop
invalid:
  call void @llvm.trap()
  unreachable
loop:
  %i = phi i32 [ 128, %entry ], [ %next, %loop ]
  %bits = phi i128 [ %n, %entry ], [ %bitsnext, %loop ]
  %q = phi i128 [ 0, %entry ], [ %qn, %loop ]
  %r = phi i128 [ 0, %entry ], [ %rn, %loop ]
  %carry = icmp slt i128 %r, 0
  %rshift = shl i128 %r, 1
  %bit = lshr i128 %bits, 127
  %wide = or i128 %rshift, %bit
  %ge = icmp uge i128 %wide, %d
  %take = or i1 %carry, %ge
  %difference = sub i128 %wide, %d
  %rn = select i1 %take, i128 %difference, i128 %wide
  %qshift = shl i128 %q, 1
  %qbit = zext i1 %take to i128
  %qn = or i128 %qshift, %qbit
  %bitsnext = shl i128 %bits, 1
  %next = sub i32 %i, 1
  %done = icmp eq i32 %next, 0
  br i1 %done, label %exit, label %loop
exit:
  {finish}
}}
declare void @llvm.trap()
"#
    )
}

fn multiply(name: &str) -> String {
    // 32-bit limbs ensure even each limb product needs only a native i64 mul.
    let mut text = format!("define i128 @{name}(i128 %a, i128 %b) {{\nentry:\n");
    for operand in ["a", "b"] {
        for i in 0..4 {
            text += &format!("  %{operand}s{i} = lshr i128 %{operand}, {}\n  %{operand}t{i} = trunc i128 %{operand}s{i} to i64\n  %{operand}{i} = and i64 %{operand}t{i}, 4294967295\n", i * 32);
        }
    }
    let mut sum = "0".to_string();
    for i in 0..4 {
        for j in 0..4 - i {
            text += &format!("  %m{i}{j} = mul i64 %a{i}, %b{j}\n  %w{i}{j} = zext i64 %m{i}{j} to i128\n  %s{i}{j} = shl i128 %w{i}{j}, {}\n  %r{i}{j} = add i128 {sum}, %s{i}{j}\n", (i + j) * 32);
            sum = format!("%r{i}{j}");
        }
    }
    text + &format!("  ret i128 {sum}\n}}\n")
}

fn to_float(name: &str, signed: bool, float: &str) -> String {
    let (precision, repr, fraction, bias) =
        if float == "float" { (24, "i32", 23, 127) } else { (53, "i64", 52, 1023) };
    let signs = if signed {
        "  %negative = icmp slt i128 %a, 0\n  %negated = sub i128 0, %a\n  %magnitude = select i1 %negative, i128 %negated, i128 %a\n"
    } else {
        "  %magnitude = or i128 %a, 0\n"
    };
    let exponent = if repr == "i64" {
        "  %exp = zext i32 %biased to i64\n"
    } else {
        "  %exp = or i32 %biased, 0\n"
    };
    let finish = if signed {
        format!("  %minus = fneg {float} %scaled\n  %result = select i1 %negative, {float} %minus, {float} %scaled\n  ret {float} %result")
    } else {
        format!("  ret {float} %scaled")
    };
    let limit = 1u64 << precision;
    format!(
        r#"
define {float} @{name}(i128 %a) {{
entry:
{signs}
  br label %scan
scan:
  %value = phi i128 [ %magnitude, %entry ], [ %shifted, %shift ]
  %count = phi i32 [ 0, %entry ], [ %next, %shift ]
  %guard = phi i1 [ false, %entry ], [ %lowbit, %shift ]
  %sticky = phi i1 [ false, %entry ], [ %lost, %shift ]
  %large = icmp uge i128 %value, {limit}
  br i1 %large, label %shift, label %round
shift:
  %lost = or i1 %sticky, %guard
  %lowbit = trunc i128 %value to i1
  %shifted = lshr i128 %value, 1
  %next = add i32 %count, 1
  br label %scan
round:
  %odd = trunc i128 %value to i1
  %tie = or i1 %sticky, %odd
  %up = and i1 %guard, %tie
  %increment = zext i1 %up to i64
  %small = trunc i128 %value to i64
  %rounded = add i64 %small, %increment
  %base = uitofp i64 %rounded to {float}
  %biased = add i32 %count, {bias}
{exponent}
  %scale_bits = shl {repr} %exp, {fraction}
  %scale = bitcast {repr} %scale_bits to {float}
  %scaled = fmul {float} %base, %scale
{finish}
}}
"#
    )
}

fn from_float(name: &str, signed: bool, float: &str) -> String {
    let (repr, fraction, bias, total) =
        if float == "float" { ("i32", 23, 127, 32) } else { ("i64", 52, 1023, 64) };
    let mask = (1u64 << fraction) - 1;
    let hidden = 1u64 << fraction;
    let expmask = if float == "float" { 255 } else { 2047 };
    let exp32 = if repr == "i64" {
        "  %exp32 = trunc i64 %rawexp to i32\n"
    } else {
        "  %exp32 = or i32 %rawexp, 0\n"
    };
    let range = if signed {
        r#"
  %high = icmp sgt i32 %exponent, 127
  %edge = icmp eq i32 %exponent, 127
  %fractional = icmp ne i128 %mantissa, 0
  %positive = xor i1 %negative, true
  %badedge = or i1 %fractional, %positive
  %edgeoverflow = and i1 %edge, %badedge
  %invalid = or i1 %high, %edgeoverflow
"#
    } else {
        "  %high = icmp sgt i32 %exponent, 127\n  %invalid = or i1 %high, %negative\n"
    };
    let finish = if signed {
        "  %negated = sub i128 0, %magnitude\n  %result = select i1 %negative, i128 %negated, i128 %magnitude\n  ret i128 %result"
    } else {
        "  ret i128 %magnitude"
    };
    let signbit = total - 1;
    format!(
        r#"
define i128 @{name}({float} %a) {{
entry:
  %bits = bitcast {float} %a to {repr}
  %sign = lshr {repr} %bits, {signbit}
  %negative = trunc {repr} %sign to i1
  %shiftedexp = lshr {repr} %bits, {fraction}
  %rawexp = and {repr} %shiftedexp, {expmask}
{exp32}
  %exponent = sub i32 %exp32, {bias}
  %below = icmp slt i32 %exponent, 0
  br i1 %below, label %zero, label %check
zero:
  ret i128 0
check:
  %rawmantissa = and {repr} %bits, {mask}
  %mantissa = zext {repr} %rawmantissa to i128
{range}
  br i1 %invalid, label %trap, label %convert
trap:
  call void @llvm.trap()
  unreachable
convert:
  %significand = or i128 %mantissa, {hidden}
  %left = icmp sge i32 %exponent, {fraction}
  br i1 %left, label %up, label %down
up:
  %lcount = sub i32 %exponent, {fraction}
  %lwide = zext i32 %lcount to i128
  %lresult = shl i128 %significand, %lwide
  br label %exit
down:
  %rcount = sub i32 {fraction}, %exponent
  %rwide = zext i32 %rcount to i128
  %rresult = lshr i128 %significand, %rwide
  br label %exit
exit:
  %magnitude = phi i128 [ %lresult, %up ], [ %rresult, %down ]
{finish}
}}
declare void @llvm.trap()
"#
    )
}

fn shift(name: &str, op: &str) -> String {
    let right = if op == "ashr" { "ashr" } else { "lshr" };
    let small = if op == "shl" {
        "  %low_small = shl i64 %low, %count\n  %upper = shl i64 %high, %count\n  %carry = lshr i64 %low, %complement\n  %high_small = or i64 %upper, %carry\n".to_string()
    } else {
        format!("  %high_small = {right} i64 %high, %count\n  %lower = lshr i64 %low, %count\n  %carry = shl i64 %high, %complement\n  %low_small = or i64 %lower, %carry\n")
    };
    let large = if op == "shl" {
        "  %low_large = or i64 0, 0\n  %high_large = shl i64 %low, %extra\n".to_string()
    } else {
        let fill = if op == "ashr" { "ashr i64 %high, 63" } else { "or i64 0, 0" };
        format!("  %low_large = {right} i64 %high, %extra\n  %high_large = {fill}\n")
    };
    format!(
        r#"
define i128 @{name}(i128 %a, i128 %b) {{
entry:
  %invalid = icmp uge i128 %b, 128
  br i1 %invalid, label %trap, label %check
trap:
  call void @llvm.trap()
  unreachable
check:
  %count = trunc i128 %b to i64
  %zero = icmp eq i64 %count, 0
  br i1 %zero, label %identity, label %split
identity:
  ret i128 %a
split:
  %low = trunc i128 %a to i64
  %upperbits = lshr i128 %a, 64
  %high = trunc i128 %upperbits to i64
  %small = icmp ult i64 %count, 64
  br i1 %small, label %near, label %far
near:
  %complement = sub i64 64, %count
{small}
  br label %exit
far:
  %extra = sub i64 %count, 64
{large}
  br label %exit
exit:
  %lo = phi i64 [ %low_small, %near ], [ %low_large, %far ]
  %hi = phi i64 [ %high_small, %near ], [ %high_large, %far ]
  %low_wide = zext i64 %lo to i128
  %high_wide = zext i64 %hi to i128
  %high_positioned = shl i128 %high_wide, 64
  %result = or i128 %low_wide, %high_positioned
  ret i128 %result
}}
declare void @llvm.trap()
"#
    )
}
