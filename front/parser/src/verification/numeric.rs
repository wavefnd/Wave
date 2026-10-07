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

//! Pure numeric compatibility, literal ranges, and assignment/cast policy.
//! Reads the collected type environment; never mutates scopes or HIR facts.
use super::model::*;
use super::program::ProgramTypes;
use crate::ast::{Operator, WaveType};

pub(super) fn infer_binary_type(
    program: &ProgramTypes,
    operator: &Operator,
    left: ExpressionType,
    right: ExpressionType,
) -> Result<ExpressionType, String> {
    let has_generic_operand = [&left, &right].iter().any(|operand| {
        matches!(operand, ExpressionType::Known(ty) if program.is_generic_placeholder(ty))
    });
    if has_generic_operand {
        return if matches!(
            operator,
            Operator::GreaterEqual
                | Operator::LessEqual
                | Operator::Greater
                | Operator::Less
                | Operator::Equal
                | Operator::NotEqual
                | Operator::LogicalAnd
                | Operator::LogicalOr
        ) {
            Ok(ExpressionType::Known(WaveType::Bool))
        } else {
            Ok(ExpressionType::Unknown)
        };
    }
    let left_canonical = canonical_expression_type(program, &left);
    let right_canonical = canonical_expression_type(program, &right);
    let comparison = matches!(
        operator,
        Operator::GreaterEqual
            | Operator::LessEqual
            | Operator::Greater
            | Operator::Less
            | Operator::Equal
            | Operator::NotEqual
    );
    let logical = matches!(operator, Operator::LogicalAnd | Operator::LogicalOr);
    let arithmetic = matches!(
        operator,
        Operator::Add
            | Operator::Subtract
            | Operator::Multiply
            | Operator::Divide
            | Operator::Remainder
    );
    let integer_only = matches!(
        operator,
        Operator::ShiftLeft
            | Operator::ShiftRight
            | Operator::BitwiseAnd
            | Operator::BitwiseOr
            | Operator::BitwiseXor
    );

    if matches!(operator, Operator::ShiftLeft | Operator::ShiftRight) {
        let integer = |ty: &Option<WaveType>| {
            matches!(
                ty,
                Some(WaveType::Int(_) | WaveType::Uint(_) | WaveType::Byte | WaveType::Char)
            )
        };
        if !integer(&left_canonical) || !integer(&right_canonical) {
            return Err(binary_type_error(operator, &left, &right));
        }
        return Ok(left);
    }

    if arithmetic || comparison || integer_only {
        validate_contextual_integer_literal(program, &left, &right)?;
        validate_contextual_integer_literal(program, &right, &left)?;
    }

    let left_pointer = left_canonical.as_ref().is_some_and(is_pointer_like_type);
    let right_pointer = right_canonical.as_ref().is_some_and(is_pointer_like_type);
    let left_integer = left_canonical.as_ref().is_some_and(|ty| integer_bit_width(ty).is_some());
    let right_integer = right_canonical.as_ref().is_some_and(|ty| integer_bit_width(ty).is_some());
    let left_numeric = left_canonical.as_ref().is_some_and(is_numeric_type);
    let right_numeric = right_canonical.as_ref().is_some_and(is_numeric_type);

    if let (ExpressionType::Known(left_known), ExpressionType::Known(right_known)) = (&left, &right)
    {
        if let (WaveType::Float(left_bits), WaveType::Float(right_bits)) =
            (program.canonical_type(left_known), program.canonical_type(right_known))
        {
            if left_bits != right_bits {
                return Err(format!(
                    "mixed float widths require an explicit cast: found `f{}` and `f{}`",
                    left_bits, right_bits
                ));
            }
        }
    }

    if matches!(operator, Operator::Equal | Operator::NotEqual)
        && ((left_pointer && matches!(right, ExpressionType::Null))
            || (right_pointer && matches!(left, ExpressionType::Null)))
    {
        return Ok(ExpressionType::Known(WaveType::Bool));
    }

    if left_pointer || right_pointer {
        let valid = match (left_pointer, right_pointer) {
            (true, true) => {
                matches!(operator, Operator::Equal | Operator::NotEqual | Operator::Subtract)
            },
            (true, false) if right_integer => matches!(
                operator,
                Operator::Add | Operator::Subtract | Operator::Equal | Operator::NotEqual
            ),
            (false, true) if left_integer => {
                matches!(operator, Operator::Add | Operator::Equal | Operator::NotEqual)
            },
            _ => false,
        };
        if !valid {
            return Err(binary_type_error(operator, &left, &right));
        }
        if comparison {
            return Ok(ExpressionType::Known(WaveType::Bool));
        }
        if left_pointer {
            if right_pointer {
                return Ok(ExpressionType::Known(WaveType::Int(64)));
            }
            return Ok(left);
        }
        return Ok(right);
    }

    if logical {
        if left_integer && right_integer {
            return Ok(ExpressionType::Known(WaveType::Bool));
        }
        return Err(binary_type_error(operator, &left, &right));
    }

    if integer_only {
        if left_integer && right_integer {
            return Ok(contextual_integer_expression(program, left, right));
        }
        return Err(binary_type_error(operator, &left, &right));
    }

    if arithmetic || comparison {
        if !left_numeric || !right_numeric {
            return Err(binary_type_error(operator, &left, &right));
        }
        if comparison {
            return Ok(ExpressionType::Known(WaveType::Bool));
        }
        if matches!(left_canonical, Some(WaveType::Float(_))) {
            return Ok(left);
        }
        if matches!(right_canonical, Some(WaveType::Float(_))) {
            return Ok(right);
        }
        if matches!(left, ExpressionType::FloatLiteral) {
            return Ok(left);
        }
        if matches!(right, ExpressionType::FloatLiteral) {
            return Ok(right);
        }
        return Ok(contextual_integer_expression(program, left, right));
    }

    Err(binary_type_error(operator, &left, &right))
}

pub(super) fn canonical_expression_type(
    program: &ProgramTypes,
    ty: &ExpressionType,
) -> Option<WaveType> {
    match ty {
        ExpressionType::Known(ty) => Some(program.canonical_type(ty)),
        ExpressionType::IntLiteral(_) => Some(WaveType::Int(32)),
        ExpressionType::FloatLiteral => Some(WaveType::Float(32)),
        _ => None,
    }
}

pub(super) fn wider_integer_expression(
    program: &ProgramTypes,
    left: ExpressionType,
    right: ExpressionType,
) -> ExpressionType {
    let left_width = canonical_expression_type(program, &left)
        .as_ref()
        .and_then(integer_bit_width)
        .unwrap_or(32);
    let right_width = canonical_expression_type(program, &right)
        .as_ref()
        .and_then(integer_bit_width)
        .unwrap_or(32);
    if left_width >= right_width {
        left
    } else {
        right
    }
}

pub(super) fn validate_contextual_integer_literal(
    program: &ProgramTypes,
    literal: &ExpressionType,
    other: &ExpressionType,
) -> Result<(), String> {
    let (ExpressionType::IntLiteral(raw), ExpressionType::Known(other)) = (literal, other) else {
        return Ok(());
    };
    let other = program.canonical_type(other);
    if integer_bit_width(&other).is_none() || integer_literal_fits(raw, &other) {
        return Ok(());
    }
    Err(format!(
        "integer literal `{}` does not fit `{}`; use an explicit cast to select another type",
        raw,
        display_wave_type(&other)
    ))
}

pub(super) fn contextual_integer_expression(
    program: &ProgramTypes,
    left: ExpressionType,
    right: ExpressionType,
) -> ExpressionType {
    match (&left, &right) {
        (ExpressionType::IntLiteral(_), ExpressionType::Known(ty))
            if integer_bit_width(&program.canonical_type(ty)).is_some() =>
        {
            right
        },
        (ExpressionType::Known(ty), ExpressionType::IntLiteral(_))
            if integer_bit_width(&program.canonical_type(ty)).is_some() =>
        {
            left
        },
        _ => wider_integer_expression(program, left, right),
    }
}

pub(super) fn binary_type_error(
    operator: &Operator,
    left: &ExpressionType,
    right: &ExpressionType,
) -> String {
    format!(
        "binary operator `{:?}` is not supported for `{}` and `{}`",
        operator,
        display_expression_type(left),
        display_expression_type(right)
    )
}

pub(super) fn int_literal_is_zero(raw: &str) -> bool {
    lexer::number::IntegerLiteral::parse(raw).is_some_and(|n| n.is_zero())
}

pub(super) fn integer_literal_fits(raw: &str, ty: &WaveType) -> bool {
    let Some((negative, radix, digits)) = integer_literal_parts(raw) else {
        return false;
    };
    let Some(bit_len) = unsigned_literal_bit_len(radix, &digits) else {
        return false;
    };
    let is_zero = bit_len == 0;

    match ty {
        WaveType::Int(bits) if *bits > 0 => {
            let bits = usize::from(*bits);
            if negative {
                is_zero
                    || bit_len < bits
                    || (bit_len == bits && unsigned_is_power_of_two(radix, &digits))
            } else if radix == 10 {
                bit_len < bits
            } else {
                // Non-decimal literals may spell the full-width bit pattern.
                bit_len <= bits
            }
        },
        WaveType::Uint(bits) if *bits > 0 => !negative && bit_len <= usize::from(*bits),
        WaveType::Char | WaveType::Byte => !negative && bit_len <= 8,
        _ => false,
    }
}

pub(super) fn integer_literal_parts(raw: &str) -> Option<(bool, u32, String)> {
    let n = lexer::number::IntegerLiteral::parse(raw)?;
    Some((n.negative, n.radix, n.digits.trim_start_matches('0').to_string()))
}

pub(super) fn unsigned_literal_bit_len(radix: u32, digits: &str) -> Option<usize> {
    if digits.is_empty() {
        return Some(0);
    }
    match radix {
        2 => Some(digits.len()),
        8 | 16 => {
            let bits_per_digit = if radix == 8 { 3 } else { 4 };
            let first = digits.chars().next()?.to_digit(radix)?;
            let first_bits = (u32::BITS - first.leading_zeros()) as usize;
            Some((digits.len() - 1) * bits_per_digit + first_bits)
        },
        10 => {
            let mut decimal: Vec<u8> = digits.bytes().map(|byte| byte - b'0').collect();
            let mut bits = 0usize;
            while decimal.iter().any(|digit| *digit != 0) {
                let mut carry = 0u8;
                for digit in &mut decimal {
                    let value = carry * 10 + *digit;
                    *digit = value / 2;
                    carry = value % 2;
                }
                bits = bits.checked_add(1)?;
            }
            Some(bits)
        },
        _ => None,
    }
}

pub(super) fn unsigned_is_power_of_two(radix: u32, digits: &str) -> bool {
    if digits.is_empty() {
        return false;
    }
    if radix == 10 {
        let mut decimal: Vec<u8> = digits.bytes().map(|byte| byte - b'0').collect();
        loop {
            let first_nonzero = decimal.iter().position(|digit| *digit != 0);
            let Some(first_nonzero) = first_nonzero else {
                return false;
            };
            if decimal[first_nonzero..] == [1] {
                return true;
            }
            if decimal.last().is_none_or(|digit| digit % 2 != 0) {
                return false;
            }
            let mut carry = 0u8;
            for digit in &mut decimal {
                let value = carry * 10 + *digit;
                *digit = value / 2;
                carry = value % 2;
            }
        }
    }
    let mut seen_one = false;
    for ch in digits.chars() {
        let Some(mut value) = ch.to_digit(radix) else {
            return false;
        };
        while value != 0 {
            if value & 1 == 1 {
                if seen_one {
                    return false;
                }
                seen_one = true;
            }
            value >>= 1;
        }
    }
    seen_one
}

pub(super) fn is_integer_type(ty: &WaveType) -> bool {
    matches!(ty, WaveType::Int(_) | WaveType::Uint(_) | WaveType::Char | WaveType::Byte)
}

pub(super) fn integer_bit_width(ty: &WaveType) -> Option<u16> {
    match ty {
        WaveType::Int(bits) | WaveType::Uint(bits) => Some(*bits),
        WaveType::Bool => Some(1),
        WaveType::Char | WaveType::Byte => Some(8),
        _ => None,
    }
}

pub(super) fn is_numeric_type(ty: &WaveType) -> bool {
    is_integer_type(ty) || matches!(ty, WaveType::Float(_))
}

pub(super) fn is_pointer_like_type(ty: &WaveType) -> bool {
    matches!(ty, WaveType::Pointer(_) | WaveType::String)
}

pub(super) fn is_byte_like_type(ty: &WaveType) -> bool {
    matches!(ty, WaveType::Int(8) | WaveType::Uint(8) | WaveType::Char | WaveType::Byte)
}

pub(super) fn require_assignable(
    program: &ProgramTypes,
    actual: &ExpressionType,
    expected: &WaveType,
    context: &str,
) -> Result<(), String> {
    if program.is_generic_placeholder(expected) {
        return Ok(());
    }
    if let ExpressionType::ArrayLiteral(elements) = actual {
        let expected = program.canonical_type(expected);
        let WaveType::Array(element_type, expected_len) = expected else {
            return Err(format!(
                "type mismatch in {}: expected `{}`, found `array literal`",
                context,
                display_wave_type(&expected)
            ));
        };
        if elements.len() != expected_len as usize {
            return Err(format!(
                "array length mismatch in {}: expected {}, found {}",
                context,
                expected_len,
                elements.len()
            ));
        }
        for (index, element) in elements.iter().enumerate() {
            require_assignable(
                program,
                element,
                element_type.as_ref(),
                &format!("element {} of {}", index, context),
            )?;
        }
        return Ok(());
    }

    if let ExpressionType::AddressedArrayLiteral(elements) = actual {
        let expected = program.canonical_type(expected);
        let WaveType::Pointer(ref pointee) = expected else {
            return Err(format!(
                "type mismatch in {}: expected `{}`, found `addressed array literal`",
                context,
                display_wave_type(&expected)
            ));
        };
        let WaveType::Array(element_type, expected_len) = pointee.as_ref() else {
            return Err(format!(
                "addressed array literal in {} requires `ptr<array<T, N>>`, found `{}`",
                context,
                display_wave_type(&expected)
            ));
        };
        if elements.len() != *expected_len as usize {
            return Err(format!(
                "array length mismatch in {}: expected {}, found {}",
                context,
                expected_len,
                elements.len()
            ));
        }
        for (index, element) in elements.iter().enumerate() {
            require_assignable(
                program,
                element,
                element_type.as_ref(),
                &format!("element {} of {}", index, context),
            )?;
        }
        return Ok(());
    }

    if is_assignable(program, actual, expected) {
        return Ok(());
    }

    Err(format!(
        "type mismatch in {}: expected `{}`, found `{}`",
        context,
        display_wave_type(expected),
        display_expression_type(actual)
    ))
}

pub(super) fn is_assignable(
    program: &ProgramTypes,
    actual: &ExpressionType,
    expected: &WaveType,
) -> bool {
    let expected = program.canonical_type(expected);
    match actual {
        ExpressionType::Unknown => true,
        ExpressionType::Null => matches!(expected, WaveType::Pointer(_)),
        ExpressionType::ArrayLiteral(_) | ExpressionType::AddressedArrayLiteral(_) => false,
        ExpressionType::IntLiteral(raw) => {
            integer_literal_fits(raw, &expected)
                || (matches!(expected, WaveType::Pointer(_)) && int_literal_is_zero(raw))
        },
        ExpressionType::FloatLiteral => matches!(expected, WaveType::Float(_)),
        ExpressionType::Known(actual) => {
            if program.is_generic_placeholder(actual) {
                return true;
            }
            let actual = program.canonical_type(actual);
            if actual == expected {
                return true;
            }

            match (&actual, &expected) {
                (actual, expected)
                    if integer_bit_width(actual).is_some()
                        && integer_bit_width(expected).is_some() =>
                {
                    integer_bit_width(actual) <= integer_bit_width(expected)
                },
                (WaveType::Int(_) | WaveType::Uint(_), WaveType::Float(_))
                | (WaveType::Float(_), WaveType::Int(_) | WaveType::Uint(_)) => true,
                (WaveType::String, WaveType::Pointer(inner)) => {
                    is_byte_like_type(inner.as_ref()) || matches!(inner.as_ref(), WaveType::String)
                },
                (WaveType::Pointer(actual), WaveType::Pointer(expected)) => actual == expected,
                _ => false,
            }
        },
    }
}

pub(super) fn is_valid_cast(
    program: &ProgramTypes,
    source: &ExpressionType,
    target: &WaveType,
) -> bool {
    let target = program.canonical_type(target);
    if matches!(
        target,
        WaveType::Void | WaveType::Array(_, _) | WaveType::Struct(_) | WaveType::Variant(_)
    ) {
        return false;
    }

    if matches!(source, ExpressionType::Unknown) {
        return false;
    }
    if matches!(source, ExpressionType::Null) {
        return matches!(target, WaveType::Pointer(_));
    }
    if matches!(source, ExpressionType::ArrayLiteral(_) | ExpressionType::AddressedArrayLiteral(_))
    {
        return false;
    }

    let source = match source {
        ExpressionType::Known(ty) => program.canonical_type(ty),
        ExpressionType::IntLiteral(_) => WaveType::Int(32),
        ExpressionType::FloatLiteral => WaveType::Float(32),
        _ => return false,
    };
    if matches!(
        source,
        WaveType::Void | WaveType::Array(_, _) | WaveType::Struct(_) | WaveType::Variant(_)
    ) {
        return false;
    }

    if target == WaveType::Bool && is_pointer_like_type(&source) {
        return false;
    }
    let source_integer = integer_bit_width(&source).is_some();
    let target_integer = integer_bit_width(&target).is_some();
    let source_float = matches!(source, WaveType::Float(_));
    let target_float = matches!(target, WaveType::Float(_));
    let source_pointer = is_pointer_like_type(&source);
    let target_pointer = matches!(target, WaveType::Pointer(_) | WaveType::String);

    (source_integer && (target_integer || target_float || target_pointer))
        || (source_float && (target_integer || target_float))
        || (source_pointer && (target_integer || target_pointer))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn literal_ranges_preserve_signed_bit_patterns_and_decimal_limits() {
        for (raw, ty, fits) in [
            ("255", WaveType::Uint(8), true),
            ("256", WaveType::Uint(8), false),
            ("255", WaveType::Int(8), false),
            ("0xff", WaveType::Int(8), true),
            ("-128", WaveType::Int(8), true),
            ("-129", WaveType::Int(8), false),
            ("-1", WaveType::Uint(8), false),
            ("340282366920938463463374607431768211455", WaveType::Uint(128), true),
        ] {
            assert_eq!(integer_literal_fits(raw, &ty), fits, "{raw} {ty:?}");
        }
    }
    #[test]
    fn implicit_assignment_keeps_width_and_cast_policy_distinct() {
        let program = ProgramTypes::default();
        let small = ExpressionType::Known(WaveType::Uint(8));
        let wide = ExpressionType::Known(WaveType::Int(64));
        assert!(is_assignable(&program, &small, &WaveType::Int(64)));
        assert!(!is_assignable(&program, &wide, &WaveType::Uint(8)));
        assert!(is_valid_cast(&program, &wide, &WaveType::Uint(8)));
        assert!(is_assignable(&program, &wide, &WaveType::Float(64)));
    }
}
