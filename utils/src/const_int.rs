// SPDX-License-Identifier: MPL-2.0
//! Bounded exact integer arithmetic for compiler constant evaluation.
//!
//! Values use signed magnitude with 2048 bits of storage: enough for the full
//! product of two Wave 1024-bit operands before width normalization. Operations
//! report capacity overflow; truncation occurs only through `normalize`.
//! This module has no frontend, backend, or external crate dependencies.
use std::cmp::Ordering;

const WORDS: usize = 32;
const CAPACITY: usize = WORDS * 64;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConstInt {
    negative: bool,
    words: Box<[u64; WORDS]>,
}

impl ConstInt {
    pub fn zero() -> Self {
        Self { negative: false, words: Box::new([0; WORDS]) }
    }

    pub fn from_u64(value: u64) -> Self {
        let mut result = Self::zero();
        result.words[0] = value;
        result
    }

    /// Parse unsigned digits. Apply a source-level minus with `negated`.
    /// Invalid digits, unsupported radices, and capacity overflow return None.
    pub fn from_digits(digits: &str, radix: u32) -> Option<Self> {
        if !matches!(radix, 2 | 8 | 10 | 16) || digits.is_empty() {
            return None;
        }
        let mut result = Self::zero();
        for ch in digits.chars() {
            if !ch.is_ascii() {
                return None;
            }
            let mut carry = u128::from(ch.to_digit(radix)?);
            for word in result.words.iter_mut() {
                let next = u128::from(*word) * u128::from(radix) + carry;
                *word = next as u64;
                carry = next >> 64;
            }
            if carry != 0 {
                return None;
            }
        }
        Some(result)
    }

    pub fn is_zero(&self) -> bool {
        self.words.iter().all(|&w| w == 0)
    }

    pub fn is_negative(&self) -> bool {
        self.negative
    }

    /// Little-endian two's-complement words at the requested storage width.
    pub fn to_le_words(&self, bits: u16) -> Vec<u64> {
        assert!((1..=1024).contains(&bits));
        let mut words = self.twos_complement()[..(bits as usize).div_ceil(64)].to_vec();
        if !bits.is_multiple_of(64) {
            *words.last_mut().unwrap() &= (1u64 << (bits % 64)) - 1;
        }
        words
    }

    pub fn bits(&self) -> usize {
        self.words
            .iter()
            .rposition(|&w| w != 0)
            .map_or(0, |i| i * 64 + 64 - self.words[i].leading_zeros() as usize)
    }

    fn bit(&self, index: usize) -> bool {
        index < CAPACITY && self.words[index / 64] & (1 << (index % 64)) != 0
    }

    fn canonical(mut self) -> Self {
        self.negative &= !self.is_zero();
        self
    }

    pub fn negated(mut self) -> Self {
        self.negative = !self.negative && !self.is_zero();
        self
    }

    fn magnitude_cmp(&self, other: &Self) -> Ordering {
        self.words.iter().rev().cmp(other.words.iter().rev())
    }

    fn magnitude_add(&self, other: &Self) -> Option<Self> {
        let mut result = Self::zero();
        let mut carry = 0u128;
        for i in 0..WORDS {
            let n = u128::from(self.words[i]) + u128::from(other.words[i]) + carry;
            result.words[i] = n as u64;
            carry = n >> 64;
        }
        (carry == 0).then_some(result)
    }

    fn magnitude_sub(&self, other: &Self) -> Self {
        debug_assert!(self.magnitude_cmp(other) != Ordering::Less);
        let mut result = Self::zero();
        let mut borrow = false;
        for i in 0..WORDS {
            let (n, a) = self.words[i].overflowing_sub(other.words[i]);
            let (n, b) = n.overflowing_sub(u64::from(borrow));
            result.words[i] = n;
            borrow = a || b;
        }
        debug_assert!(!borrow);
        result
    }

    pub fn checked_add(&self, other: &Self) -> Option<Self> {
        let (mut result, negative) = if self.negative == other.negative {
            (self.magnitude_add(other)?, self.negative)
        } else if self.magnitude_cmp(other) != Ordering::Less {
            (self.magnitude_sub(other), self.negative)
        } else {
            (other.magnitude_sub(self), other.negative)
        };
        result.negative = negative;
        Some(result.canonical())
    }

    pub fn checked_sub(&self, other: &Self) -> Option<Self> {
        self.checked_add(&other.clone().negated())
    }

    pub fn checked_mul(&self, other: &Self) -> Option<Self> {
        let mut result = Self::zero();
        let right_words = other.bits().div_ceil(64);
        for i in 0..self.bits().div_ceil(64) {
            let mut carry = 0u128;
            for j in 0..right_words {
                let product = u128::from(self.words[i]) * u128::from(other.words[j]);
                if i + j >= WORDS {
                    if product != 0 || carry != 0 {
                        return None;
                    }
                    continue;
                }
                let n = product + u128::from(result.words[i + j]) + carry;
                result.words[i + j] = n as u64;
                carry = n >> 64;
            }
            if carry != 0 {
                let index = i + right_words;
                if index >= WORDS {
                    return None;
                }
                // Earlier rows end before this word.
                debug_assert_eq!(result.words[index], 0);
                result.words[index] = carry as u64;
            }
        }
        result.negative = self.negative != other.negative;
        Some(result.canonical())
    }
    /// Truncating division; the remainder has the dividend's sign.
    pub fn div_rem(&self, other: &Self) -> Option<(Self, Self)> {
        if other.is_zero() {
            return None;
        }
        let mut quotient = Self::zero();
        let mut remainder = Self::zero();
        for bit in (0..self.bits()).rev() {
            // Each prefix of the dividend fits the original capacity.
            remainder = remainder.checked_shl(1)?;
            remainder.words[0] |= u64::from(self.bit(bit));
            if remainder.magnitude_cmp(other) != Ordering::Less {
                remainder = remainder.magnitude_sub(other);
                quotient.words[bit / 64] |= 1 << (bit % 64);
            }
        }
        quotient.negative = self.negative != other.negative;
        remainder.negative = self.negative;
        Some((quotient.canonical(), remainder.canonical()))
    }

    pub fn checked_shl(&self, shift: usize) -> Option<Self> {
        if self.is_zero() {
            return Some(Self::zero());
        }
        if shift > CAPACITY - self.bits() {
            return None;
        }
        let mut result = Self::zero();
        let (whole, part) = (shift / 64, shift % 64);
        for i in 0..self.bits().div_ceil(64) {
            result.words[i + whole] |= self.words[i] << part;
            if part != 0 && i + whole + 1 < WORDS {
                result.words[i + whole + 1] |= self.words[i] >> (64 - part);
            }
        }
        result.negative = self.negative;
        Some(result)
    }

    fn magnitude_shr(&self, shift: usize) -> Self {
        let mut result = Self::zero();
        if shift >= CAPACITY {
            return result;
        }
        let (whole, part) = (shift / 64, shift % 64);
        for i in 0..WORDS - whole {
            result.words[i] = self.words[i + whole] >> part;
            if part != 0 && i + whole + 1 < WORDS {
                result.words[i] |= self.words[i + whole + 1] << (64 - part);
            }
        }
        result
    }
    /// Arithmetic shift: negative values round toward negative infinity.
    pub fn shifted_right(&self, shift: usize) -> Self {
        let mut result = self.magnitude_shr(shift);
        if self.negative && (0..shift.min(CAPACITY)).any(|i| self.bit(i)) {
            result = result.magnitude_add(&Self::from_u64(1)).unwrap();
        }
        result.negative = self.negative;
        result.canonical()
    }

    fn twos_complement(&self) -> [u64; WORDS + 1] {
        let mut words = [0; WORDS + 1];
        words[..WORDS].copy_from_slice(self.words.as_ref());
        if self.negative {
            negate_words(&mut words);
        }
        words
    }

    fn from_twos_complement(mut words: [u64; WORDS + 1]) -> Option<Self> {
        let negative = words[WORDS] >> 63 != 0;
        if negative {
            negate_words(&mut words);
        }
        if words[WORDS] != 0 {
            return None;
        }
        let mut result = Self::zero();
        result.words.copy_from_slice(&words[..WORDS]);
        result.negative = negative;
        Some(result.canonical())
    }

    fn bitwise(&self, other: &Self, op: impl Fn(u64, u64) -> u64) -> Option<Self> {
        let mut words = self.twos_complement();
        for (a, b) in words.iter_mut().zip(other.twos_complement()) {
            *a = op(*a, b);
        }
        Self::from_twos_complement(words)
    }

    pub fn bitand(&self, other: &Self) -> Option<Self> {
        self.bitwise(other, |a, b| a & b)
    }

    pub fn bitor(&self, other: &Self) -> Option<Self> {
        self.bitwise(other, |a, b| a | b)
    }

    pub fn bitxor(&self, other: &Self) -> Option<Self> {
        self.bitwise(other, |a, b| a ^ b)
    }

    pub fn checked_not(&self) -> Option<Self> {
        Self::from_twos_complement(self.twos_complement().map(|w| !w))
    }
    /// Apply modulo 2^bits, then interpret the result with the requested sign.
    /// Wave widths 1..=1024 are supported, including bool and byte storage.
    pub fn normalize(&self, bits: u16, signed: bool) -> Self {
        assert!((1..=1024).contains(&bits), "unsupported Wave integer width");
        let bits = usize::from(bits);
        let mut words = self.twos_complement();
        let negative = signed && words[(bits - 1) / 64] & (1 << ((bits - 1) % 64)) != 0;
        let fill = if negative { u64::MAX } else { 0 };
        let (whole, part) = (bits / 64, bits % 64);
        if part != 0 {
            let mask = (1u64 << part) - 1;
            words[whole] = (words[whole] & mask) | (fill & !mask);
        }
        words[bits.div_ceil(64)..].fill(fill);
        Self::from_twos_complement(words).unwrap()
    }

    pub fn fits(&self, bits: u16, signed: bool) -> bool {
        self == &self.normalize(bits, signed)
    }

    pub fn to_usize(&self) -> Option<usize> {
        if self.negative || self.bits() > usize::BITS as usize {
            return None;
        }
        Some(self.words[0] as usize)
    }
    /// Decode IEEE binary64 exactly, discarding fractional bits toward zero.
    pub fn from_f64(value: f64) -> Option<Self> {
        if !value.is_finite() {
            return None;
        }
        let raw = value.to_bits();
        let exponent = ((raw >> 52) & 0x7ff) as i32 - 1023;
        if exponent < 0 {
            return Some(Self::zero());
        }
        let significand = (raw & ((1u64 << 52) - 1)) | (1u64 << 52);
        let value = Self::from_u64(significand);
        let mut result = if exponent >= 52 {
            value.checked_shl((exponent - 52) as usize)?
        } else {
            value.magnitude_shr((52 - exponent) as usize)
        };
        result.negative = raw >> 63 != 0;
        Some(result.canonical())
    }

    fn rounded_significand(&self, precision: usize) -> (u64, i32) {
        let shift = self.bits().saturating_sub(precision);
        let mut significand = self.magnitude_shr(shift).words[0];
        // Round once, to nearest with ties to even; do not pass f32 through f64.
        if shift != 0
            && self.bit(shift - 1)
            && (significand & 1 != 0 || (0..shift - 1).any(|i| self.bit(i)))
        {
            significand += 1;
        }
        (significand, shift as i32)
    }

    pub fn to_f64(&self) -> f64 {
        let (significand, shift) = self.rounded_significand(53);
        let value = significand as f64 * 2f64.powi(shift);
        if self.negative {
            -value
        } else {
            value
        }
    }

    pub fn to_f32(&self) -> f32 {
        let (significand, shift) = self.rounded_significand(24);
        let value = significand as f32 * 2f32.powi(shift);
        if self.negative {
            -value
        } else {
            value
        }
    }
}
fn negate_words(words: &mut [u64]) {
    let mut carry = true;
    for word in words {
        let (next, overflow) = (!*word).overflowing_add(u64::from(carry));
        *word = next;
        carry = overflow;
    }
}
impl Ord for ConstInt {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.negative, other.negative) {
            (true, false) => Ordering::Less,
            (false, true) => Ordering::Greater,
            (false, false) => self.magnitude_cmp(other),
            (true, true) => self.magnitude_cmp(other).reverse(),
        }
    }
}
impl PartialOrd for ConstInt {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn int(n: i128) -> ConstInt {
        let v = ConstInt::from_digits(&n.unsigned_abs().to_string(), 10).unwrap();
        if n < 0 {
            v.negated()
        } else {
            v
        }
    }
    #[test]
    fn storage_words_preserve_sign_and_truncate_only_at_the_requested_width() {
        assert_eq!(int(-1).to_le_words(1), vec![1]);
        assert_eq!(int(-2).to_le_words(8), vec![254]);
        assert_eq!(int(-1).to_le_words(128), vec![u64::MAX; 2]);
        assert_eq!(int(-1).to_le_words(1024), vec![u64::MAX; 16]);
        let wide = ConstInt::from_u64(1).checked_shl(1000).unwrap();
        let words = wide.to_le_words(1024);
        assert_eq!(words[15], 1 << 40);
        assert!(words[..15].iter().all(|w| *w == 0));
        assert_eq!(wide.to_le_words(64), vec![0]);
    }

    #[test]
    fn signed_arithmetic_matches_native_integers() {
        for a in -32i128..=32 {
            let x = int(a);
            assert_eq!(x.checked_not().unwrap(), int(!a));
            for shift in [0, 1, 7, 63, 64, 127, 1024, usize::MAX] {
                assert_eq!(
                    x.shifted_right(shift),
                    int(if shift < 128 { a >> shift } else { -((a < 0) as i128) })
                );
            }
            for b in -32i128..=32 {
                let y = int(b);
                assert_eq!(x.cmp(&y), a.cmp(&b));
                assert_eq!(x.checked_add(&y).unwrap(), int(a + b));
                assert_eq!(x.checked_sub(&y).unwrap(), int(a - b));
                assert_eq!(x.checked_mul(&y).unwrap(), int(a * b));
                assert_eq!(x.bitand(&y).unwrap(), int(a & b));
                assert_eq!(x.bitor(&y).unwrap(), int(a | b));
                assert_eq!(x.bitxor(&y).unwrap(), int(a ^ b));
                if b != 0 {
                    assert_eq!(x.div_rem(&y).unwrap(), (int(a / b), int(a % b)));
                } else {
                    assert!(x.div_rem(&y).is_none());
                }
            }
        }
    }
    #[test]
    fn wide_arithmetic_matches_independent_reference_vectors() {
        fn hex(raw: &str) -> ConstInt {
            let value = ConstInt::from_digits(raw.trim_start_matches('-'), 16).unwrap();
            if raw.starts_with('-') {
                value.negated()
            } else {
                value
            }
        }
        for (index, line) in include_str!("../tests/fixtures/const_int.tsv").lines().enumerate() {
            if line.starts_with('#') {
                continue;
            }
            let parts: Vec<_> = line.split_whitespace().collect();
            let (a, b, expected) = (hex(parts[1]), hex(parts[2]), hex(parts[3]));
            let result = match parts[0] {
                "add" => a.checked_add(&b),
                "sub" => a.checked_sub(&b),
                "mul" => a.checked_mul(&b),
                "div" => a.div_rem(&b).map(|(q, _)| q),
                "rem" => a.div_rem(&b).map(|(_, r)| r),
                "and" => a.bitand(&b),
                "or" => a.bitor(&b),
                "xor" => a.bitxor(&b),
                "shl" => a.checked_shl(b.to_usize().unwrap()),
                "shr" => Some(a.shifted_right(b.to_usize().unwrap())),
                _ => panic!("unknown operation"),
            };
            assert_eq!(result, Some(expected), "reference vector {}", index + 1);
        }
    }

    #[test]
    fn normalization_and_capacity_boundaries_are_explicit() {
        for value in [i128::MIN, i128::MAX, -129, -128, -1, 0, 127, 128, 255, 256] {
            let n = int(value);
            assert_eq!(n.normalize(8, true), int((value as i8) as i128));
            assert_eq!(n.normalize(8, false), int((value as u8) as i128));
            assert_eq!(n.normalize(128, true), n);
        }
        let max = ConstInt::from_digits(&"f".repeat(256), 16).unwrap();
        assert!(max.fits(1024, false));
        assert!(!max.fits(1024, true));
        assert_eq!(max.normalize(1024, true), int(-1));
        let square = max.checked_mul(&max).unwrap();
        assert_eq!(square.bits(), 2048);
        assert_eq!(square.div_rem(&max).unwrap(), (max.clone(), int(0)));
        assert_eq!(square.normalize(1024, false), int(1));
        assert!(square.checked_mul(&int(2)).is_none());
        assert!(max.checked_shl(1025).is_none());
        let limit = ConstInt::from_digits(&"f".repeat(512), 16).unwrap();
        assert!(limit.checked_add(&int(1)).is_none());
        assert!(limit.checked_not().is_none());
        assert!(ConstInt::from_digits(&"f".repeat(513), 16).is_none());
        assert_eq!(int(0).checked_shl(usize::MAX), Some(int(0)));
        for (digits, radix) in [("", 10), ("2", 2), ("1_0", 10), ("１２", 10), ("1", 3)] {
            assert!(ConstInt::from_digits(digits, radix).is_none());
        }
    }
    #[test]
    fn floating_conversions_round_once_and_decode_exactly() {
        for value in [i128::MIN, i128::MAX, -16777219, -1, 0, 1, 16777217, 16777219] {
            assert_eq!(int(value).to_f64(), value as f64);
            assert_eq!(int(value).to_f32(), value as f32);
        }
        // f64 intermediate rounding would erase the final +1 and choose the
        // wrong side of an f32 tie.
        for digits in [
            "1208925891672223212634113",
            "9007199254740993",
            "9007199254740995",
            "340282356779733661637539395458142568448",
        ] {
            let n = ConstInt::from_digits(digits, 10).unwrap();
            assert_eq!(n.to_f64().to_bits(), digits.parse::<f64>().unwrap().to_bits());
            assert_eq!(n.to_f32().to_bits(), digits.parse::<f32>().unwrap().to_bits());
            assert_eq!(n.clone().negated().to_f32(), -n.to_f32());
        }
        for value in [
            0.0,
            -0.0,
            0.99,
            -0.99,
            1.99,
            -1.99,
            f64::MIN_POSITIVE,
            f64::from_bits(1),
            2f64.powi(512),
            f64::MAX,
            -f64::MAX,
        ] {
            let n = ConstInt::from_f64(value).unwrap();
            assert_eq!(n.to_f64(), value.trunc());
        }
        let mut seed = 0x5741_5645_1024_2048u64;
        for _ in 0..4096 {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            let value = f64::from_bits(seed);
            if value.is_finite() {
                let integer = ConstInt::from_f64(value).unwrap();
                assert_eq!(integer.to_f64(), value.trunc(), "{value}");
                assert_eq!(integer.to_f32(), value.trunc() as f32, "{value}");
            }
        }
        for value in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
            assert!(ConstInt::from_f64(value).is_none());
        }
    }
}
