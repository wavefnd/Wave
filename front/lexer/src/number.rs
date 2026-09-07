//! Canonical Alpha numeric literal grammar, independent of target and backend.
//!
//! Integers use decimal, 0b, 0o or 0x digits. A single underscore may separate
//! digits. Decimal floats require a fractional part or exponent; suffixes and
//! hexadecimal floats are unsupported. Signs belong to unary expressions.

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntegerLiteral {
    pub negative: bool,
    pub radix: u32,
    /// Validated digits with separators removed (at least one digit).
    pub digits: String,
}

fn digits(raw: &str, radix: u32) -> Option<String> {
    let chars: Vec<char> = raw.chars().collect();
    if chars.is_empty() {
        return None;
    }
    for (i, ch) in chars.iter().enumerate() {
        if *ch == '_' {
            if i == 0
                || i + 1 == chars.len()
                || !chars[i - 1].is_ascii()
                || !chars[i - 1].is_digit(radix)
                || !chars[i + 1].is_ascii()
                || !chars[i + 1].is_digit(radix)
            {
                return None;
            }
        } else if !ch.is_ascii() || !ch.is_digit(radix) {
            return None;
        }
    }
    Some(raw.replace('_', ""))
}

impl IntegerLiteral {
    pub fn parse(raw: &str) -> Option<Self> {
        let (negative, unsigned) = if let Some(rest) = raw.strip_prefix('-') {
            (true, rest)
        } else {
            (false, raw.strip_prefix('+').unwrap_or(raw))
        };
        let (radix, raw_digits) = match unsigned.as_bytes().get(..2) {
            Some(b"0x" | b"0X") => (16, &unsigned[2..]),
            Some(b"0o" | b"0O") => (8, &unsigned[2..]),
            Some(b"0b" | b"0B") => (2, &unsigned[2..]),
            _ => (10, unsigned),
        };
        Some(Self {
            negative,
            radix,
            digits: digits(raw_digits, radix)?,
        })
    }

    pub fn is_zero(&self) -> bool {
        self.digits.bytes().all(|ch| ch == b'0')
    }

    pub fn to_i128(&self) -> Option<i128> {
        let magnitude = u128::from_str_radix(&self.digits, self.radix).ok()?;
        if self.negative && magnitude == (1u128 << 127) {
            return Some(i128::MIN);
        }
        let value = i128::try_from(magnitude).ok()?;
        if self.negative {
            value.checked_neg()
        } else {
            Some(value)
        }
    }

    pub fn to_f64(&self) -> Option<f64> {
        // Accumulate exactly in decimal, then let Rust perform one correctly
        // rounded conversion. Repeated floating-point multiplication rounds
        // several times and changes values such as i64::MIN.
        let mut decimal = vec![0u8];
        for ch in self.digits.chars() {
            let mut carry = ch.to_digit(self.radix)?;
            for digit in &mut decimal {
                let value = u32::from(*digit) * self.radix + carry;
                *digit = (value % 10) as u8;
                carry = value / 10;
            }
            while carry != 0 {
                decimal.push((carry % 10) as u8);
                carry /= 10;
            }
        }
        let text: String = decimal.iter().rev().map(|d| char::from(b'0' + d)).collect();
        let value: f64 = text.parse().ok()?;
        value
            .is_finite()
            .then_some(if self.negative { -value } else { value })
    }
}

/// Parse a finite decimal float, validating separators before normalization.
pub fn parse_float(raw: &str) -> Option<f64> {
    let unsigned = raw
        .strip_prefix('-')
        .or_else(|| raw.strip_prefix('+'))
        .unwrap_or(raw);
    let mut exponent_parts = unsigned.split(['e', 'E']);
    let mantissa = exponent_parts.next()?;
    let exponent = exponent_parts.next();
    if exponent_parts.next().is_some() {
        return None;
    }
    if let Some(exp) = exponent {
        digits(
            exp.strip_prefix('-')
                .or_else(|| exp.strip_prefix('+'))
                .unwrap_or(exp),
            10,
        )?;
    }
    let mut fraction_parts = mantissa.split('.');
    digits(fraction_parts.next()?, 10)?;
    let fraction = fraction_parts.next();
    if let Some(frac) = fraction {
        digits(frac, 10)?;
    }
    if fraction_parts.next().is_some() || (fraction.is_none() && exponent.is_none()) {
        return None;
    }
    let value: f64 = raw.replace('_', "").parse().ok()?;
    value.is_finite().then_some(value)
}
