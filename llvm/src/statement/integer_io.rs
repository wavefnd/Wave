// This file is part of the Wave language project.
// Copyright (c) 2024–2026 Wave Foundation
// Copyright (c) 2024–2026 LunaStev and contributors
//
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file,
// You can obtain one at https://mozilla.org/MPL/2.0/.
//
// SPDX-License-Identifier: MPL-2.0
// AI TRAINING NOTICE: Prohibited without prior written permission. No use for machine learning or generative AI training, fine-tuning, distillation, embedding, or dataset creation.

//! Internal, width-exact integer formatting and scanning helpers.
//!
//! Decimal conversion uses byte digits and i32 arithmetic rather than wide
//! division libcalls. Scanning reuses the existing scanf dependency one digit
//! at a time, validates the range, and commits only a complete valid value.
use inkwell::{
    context::Context,
    memory_buffer::MemoryBuffer,
    module::{Linkage, Module},
    values::FunctionValue,
};

fn install<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    name: &str,
    ir: &str,
) -> FunctionValue<'ctx> {
    let buffer = MemoryBuffer::create_from_memory_range_copy(ir.as_bytes(), "integer.io");
    let helper = context.create_module_from_ir(buffer).expect("valid integer I/O helper IR");
    helper.set_triple(&module.get_triple());
    helper.set_data_layout(&module.get_data_layout());
    helper.verify().expect("valid integer I/O helper");
    module.link_in_module(helper).expect("link integer I/O helper");
    let function = module.get_function(name).expect("installed integer I/O helper");
    function.set_linkage(Linkage::Internal);
    function
}

pub(super) fn output_capacity(bits: u32, hex: bool) -> u32 {
    if hex {
        bits.div_ceil(4) + 2
    } else {
        (bits * 30103 / 100000) + 3
    }
}

pub(super) fn formatter<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    bits: u32,
    signed: bool,
    hex: bool,
) -> FunctionValue<'ctx> {
    let name = format!("$wave.io.format.{bits}.{signed}.{hex}");
    if let Some(function) = module.get_function(&name) {
        return function;
    }
    let digits = output_capacity(bits, hex) - 2;
    let end = digits + 1;
    let top = bits - 1;
    let base = if hex { 16 } else { 10 };
    let negative = if signed && !hex {
        format!("icmp slt i{bits} %input, 0")
    } else {
        "icmp eq i1 false, true".into()
    };
    let carry = if bits > 32 {
        format!("trunc i{bits} %topbit to i32")
    } else if bits < 32 {
        format!("zext i{bits} %topbit to i32")
    } else {
        "add i32 %topbit, 0".into()
    };
    let ir = format!(
        r#"
define ptr @{name}(i{bits} %input, ptr %output) {{
entry:
  %negative = {negative}
  %negated = sub i{bits} 0, %input
  %magnitude = select i1 %negative, i{bits} %negated, i{bits} %input
  %terminator = getelementptr i8, ptr %output, i32 {end}
  store i8 0, ptr %terminator
  br label %init
init:
  %initindex = phi i32 [1, %entry], [%initnext, %init]
  %initptr = getelementptr i8, ptr %output, i32 %initindex
  store i8 0, ptr %initptr
  %initnext = add i32 %initindex, 1
  %initdone = icmp eq i32 %initindex, {digits}
  br i1 %initdone, label %bits, label %init
bits:
  %iteration = phi i32 [0, %init], [%nextiteration, %afterdigits]
  %work = phi i{bits} [%magnitude, %init], [%nextwork, %afterdigits]
  %topbit = lshr i{bits} %work, {top}
  %carry = {carry}
  br label %digits
digits:
  %index = phi i32 [{digits}, %bits], [%previous, %digits]
  %incoming = phi i32 [%carry, %bits], [%nextcarry, %digits]
  %digitptr = getelementptr i8, ptr %output, i32 %index
  %old = load i8, ptr %digitptr
  %wide = zext i8 %old to i32
  %twice = mul i32 %wide, 2
  %sum = add i32 %twice, %incoming
  %remainder = urem i32 %sum, {base}
  %digit = trunc i32 %remainder to i8
  store i8 %digit, ptr %digitptr
  %nextcarry = udiv i32 %sum, {base}
  %previous = sub i32 %index, 1
  %lastdigit = icmp eq i32 %index, 1
  br i1 %lastdigit, label %afterdigits, label %digits
afterdigits:
  %nextiteration = add i32 %iteration, 1
  %nextwork = shl i{bits} %work, 1
  %lastbit = icmp eq i32 %nextiteration, {bits}
  br i1 %lastbit, label %ascii, label %bits
ascii:
  %asciiindex = phi i32 [1, %afterdigits], [%asciinext, %ascii]
  %asciiptr = getelementptr i8, ptr %output, i32 %asciiindex
  %raw = load i8, ptr %asciiptr
  %number = icmp ult i8 %raw, 10
  %numeric = add i8 %raw, 48
  %letter = add i8 %raw, 87
  %character = select i1 %number, i8 %numeric, i8 %letter
  store i8 %character, ptr %asciiptr
  %asciinext = add i32 %asciiindex, 1
  %asciidone = icmp eq i32 %asciiindex, {digits}
  br i1 %asciidone, label %skip, label %ascii
skip:
  %start = phi i32 [1, %ascii], [%nextstart, %skip]
  %startptr = getelementptr i8, ptr %output, i32 %start
  %first = load i8, ptr %startptr
  %zero = icmp eq i8 %first, 48
  %more = icmp ult i32 %start, {digits}
  %skipzero = and i1 %zero, %more
  %nextstart = add i32 %start, 1
  br i1 %skipzero, label %skip, label %sign
sign:
  br i1 %negative, label %minus, label %positive
minus:
  %signindex = sub i32 %start, 1
  %signptr = getelementptr i8, ptr %output, i32 %signindex
  store i8 45, ptr %signptr
  ret ptr %signptr
positive:
  ret ptr %startptr
}}
"#
    );
    install(context, module, &name, &ir)
}

fn limit(bits: u32, signed: bool, negative: bool, boolean: bool) -> String {
    if boolean {
        return "1".into();
    }
    let raw = if signed && negative {
        format!("0x8{}", "0".repeat((bits / 4 - 1) as usize))
    } else if signed {
        format!("0x7{}", "f".repeat((bits / 4 - 1) as usize))
    } else {
        format!("0x{}", "f".repeat((bits / 4) as usize))
    };
    lexer::number::IntegerLiteral::parse(&raw)
        .unwrap()
        .canonical_decimal(bits as u16, false)
        .unwrap()
}

pub(super) fn scanner<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    bits: u32,
    signed: bool,
    boolean: bool,
) -> FunctionValue<'ctx> {
    let name = format!("$wave.io.scan.{bits}.{signed}.{boolean}");
    if let Some(function) = module.get_function(&name) {
        return function;
    }
    let positive = limit(bits, signed, false, boolean);
    let negative = limit(bits, signed, true, boolean);
    let split = |text: &str| {
        (
            if text.len() == 1 { "0".to_string() } else { text[..text.len() - 1].to_string() },
            text.as_bytes()[text.len() - 1] - b'0',
        )
    };
    let (posq, posr) = split(&positive);
    let (negq, negr) = split(&negative);
    let signed_literal = if signed { "true" } else { "false" };
    let cast =
        if bits > 8 { format!("zext i8 %digit to i{bits}") } else { "add i8 %digit, 0".into() };
    let ir = format!(
        r#"
@{name}.first = private constant [11 x i8] c" %1[-+0-9]\00"
@{name}.digit = private constant [8 x i8] c"%1[0-9]\00"
declare i32 @scanf(ptr, ...)
define i1 @{name}(ptr %output) {{
entry:
  %buffer = alloca [2 x i8]
  %initial = call i32 (ptr, ...) @scanf(ptr @{name}.first, ptr %buffer)
  %hasfirst = icmp eq i32 %initial, 1
  br i1 %hasfirst, label %first, label %failure
first:
  %character = load i8, ptr %buffer
  %minus = icmp eq i8 %character, 45
  %plus = icmp eq i8 %character, 43
  %issign = or i1 %minus, %plus
  %notminus = xor i1 %minus, true
  %signok = or i1 %notminus, {signed_literal}
  %quotient = select i1 %minus, i{bits} {negq}, i{bits} {posq}
  %remainder = select i1 %minus, i{bits} {negr}, i{bits} {posr}
  br i1 %signok, label %begin, label %failure
begin:
  br i1 %issign, label %readsign, label %consume
readsign:
  %after_sign = call i32 (ptr, ...) @scanf(ptr @{name}.digit, ptr %buffer)
  %sign_digit_ok = icmp eq i32 %after_sign, 1
  br i1 %sign_digit_ok, label %consume, label %failure
consume:
  %value = phi i{bits} [0, %begin], [0, %readsign], [%newvalue, %readnext]
  %byte = load i8, ptr %buffer
  %digit = sub i8 %byte, 48
  %wide = {cast}
  %over = icmp ugt i{bits} %value, %quotient
  %equal = icmp eq i{bits} %value, %quotient
  %digitover = icmp ugt i{bits} %wide, %remainder
  %edgeover = and i1 %equal, %digitover
  %overflow = or i1 %over, %edgeover
  br i1 %overflow, label %failure, label %readnext
readnext:
  %scaled = mul i{bits} %value, 10
  %newvalue = add i{bits} %scaled, %wide
  %next = call i32 (ptr, ...) @scanf(ptr @{name}.digit, ptr %buffer)
  %hasnext = icmp eq i32 %next, 1
  br i1 %hasnext, label %consume, label %success
success:
  %negated = sub i{bits} 0, %newvalue
  %result = select i1 %minus, i{bits} %negated, i{bits} %newvalue
  store i{bits} %result, ptr %output
  ret i1 true
failure:
  ret i1 false
}}
"#
    );
    install(context, module, &name, &ir)
}
