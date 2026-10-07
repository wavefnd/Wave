// SPDX-License-Identifier: MPL-2.0
//! Wave serialization shared by the compiler, diagnostics and metadata readers.
//!
//! Integrated from LunaStev/wson-rust, revision
//! 4b98c0b8b3820668fb6592a6f1784fc25f0fda73, with the author's permission.
//! The original value/parser/writer/error responsibilities are maintained here;
//! the byte parser incorporates Wave's existing JSON Unicode and depth guards.
//! Dates are validated locally, numbers use ConstInt, and no regex/date crates
//! are required. JSON is a strict mode, not an implicit WSON-to-JSON conversion.

mod parser;
#[cfg(test)]
mod tests;
mod writer;

use crate::const_int::ConstInt;
use std::fmt;
use std::io::Write;

pub const MAX_DEPTH: usize = 256;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Format {
    Wson,
    Json,
}

/// Exact spelling plus checked integer storage. Private fields prevent writers
/// accepting arbitrary tokens supplied through a numeric value.
#[derive(Debug, Clone, PartialEq)]
pub struct Number {
    raw: String,
    integer: Option<ConstInt>,
}

impl Number {
    pub fn as_str(&self) -> &str {
        &self.raw
    }

    pub fn as_integer(&self) -> Option<&ConstInt> {
        self.integer.as_ref()
    }

    pub fn as_u64(&self) -> Option<u64> {
        self.raw.parse().ok()
    }
    /// Explicit approximate conversion for consumers that require floating point.
    pub fn as_f64(&self) -> Option<f64> {
        self.raw.parse::<f64>().ok().filter(|v| v.is_finite())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Number(Number),
    String(String),
    Date(String),
    DateTime(String),
    Version(Vec<u32>),
    Array(Vec<Value>),
    /// Insertion order is preserved; duplicate keys are rejected on read/write.
    Object(Vec<(String, Value)>),
}

impl Value {
    /// Ordered fields; duplicate keys are rejected by the common writer.
    pub fn object<K: Into<String>>(fields: impl IntoIterator<Item = (K, Self)>) -> Self {
        Self::Object(fields.into_iter().map(|(k, v)| (k.into(), v)).collect())
    }

    pub fn string(value: impl AsRef<str>) -> Self {
        Self::String(value.as_ref().into())
    }

    pub fn strings<S: AsRef<str>>(values: impl IntoIterator<Item = S>) -> Self {
        Self::Array(values.into_iter().map(Self::string).collect())
    }

    pub fn optional_string(value: Option<impl AsRef<str>>) -> Self {
        value.map(Self::string).unwrap_or(Self::Null)
    }

    pub fn get(&self, key: &str) -> Option<&Self> {
        match self {
            Self::Object(fields) => fields.iter().find(|(k, _)| k == key).map(|(_, v)| v),
            _ => None,
        }
    }

    pub fn get_str(&self, key: &str) -> Option<&str> {
        match self.get(key) {
            Some(Self::String(s)) => Some(s),
            _ => None,
        }
    }

    pub fn get_num(&self, key: &str) -> Option<f64> {
        match self.get(key) {
            Some(Self::Number(n)) => n.as_f64(),
            _ => None,
        }
    }

    pub fn get_u64(&self, key: &str) -> Option<u64> {
        match self.get(key) {
            Some(Self::Number(n)) => n.as_u64(),
            _ => None,
        }
    }

    pub fn get_arr(&self, key: &str) -> Option<&[Self]> {
        match self.get(key) {
            Some(Self::Array(a)) => Some(a),
            _ => None,
        }
    }

    pub fn integer(value: u64) -> Self {
        Self::Number(Number { raw: value.to_string(), integer: Some(ConstInt::from_u64(value)) })
    }

    pub fn write_to(
        &self,
        mut output: impl Write,
        format: Format,
        pretty: bool,
    ) -> Result<(), Error> {
        // Validate completely before producing bytes on the caller's stream.
        let encoded = dumps(self, format, pretty)?;
        output.write_all(encoded.as_bytes()).map_err(|e| Error::at(&encoded, 0, e.to_string()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    pub message: String,
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}
impl Error {
    fn at(source: &str, offset: usize, message: impl Into<String>) -> Self {
        let mut line = 1;
        let mut column = 1;
        let mut after_cr = false;
        for ch in String::from_utf8_lossy(&source.as_bytes()[..offset.min(source.len())]).chars() {
            match ch {
                '\r' => {
                    line += 1;
                    column = 1;
                },
                '\n' if after_cr => {},
                '\n' => {
                    line += 1;
                    column = 1;
                },
                _ => column += 1,
            }
            after_cr = ch == '\r';
        }
        Self { message: message.into(), offset, line, column }
    }
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} (byte {}, line {}, column {})",
            self.message, self.offset, self.line, self.column
        )
    }
}
impl std::error::Error for Error {}

pub fn parse(input: &str, format: Format) -> Result<Value, Error> {
    parser::parse(input, format)
}

pub fn parse_json(input: &str) -> Result<Value, Error> {
    parse(input, Format::Json)
}

pub fn loads(input: &str) -> Result<Value, Error> {
    parse(input, Format::Wson)
}

pub fn validate(input: &str, format: Format) -> Result<(), Error> {
    parse(input, format).map(|_| ())
}

pub fn dumps(value: &Value, format: Format, pretty: bool) -> Result<String, Error> {
    writer::serialize(value, format, pretty, MAX_DEPTH)
}
/// Write an already constructed value with an explicit, bounded container depth.
/// Useful when a validated domain tree expands into several wire containers per node.
/// Parsing and the default writer retain their independent 256-container limit.
pub fn dumps_with_depth_limit(
    value: &Value,
    format: Format,
    pretty: bool,
    max_depth: usize,
) -> Result<String, Error> {
    if max_depth > 512 {
        return Err(Error::at("", 0, "writer depth limit cannot exceed 512"));
    }
    writer::serialize(value, format, pretty, max_depth)
}
pub use writer::quote;

fn number(raw: &str) -> Result<Value, String> {
    let integer = if !raw.contains(['.', 'e', 'E']) {
        let digits = raw.strip_prefix('-').unwrap_or(raw);
        let value = ConstInt::from_digits(digits, 10)
            .ok_or("integer exceeds supported 2048-bit capacity")?;
        Some(if raw.starts_with('-') { value.negated() } else { value })
    } else {
        let value = raw.parse::<f64>().map_err(|_| "invalid decimal")?;
        let nonzero_mantissa =
            raw.split(['e', 'E']).next().unwrap_or(raw).bytes().any(|b| matches!(b, b'1'..=b'9'));
        if !value.is_finite() || (value == 0.0 && nonzero_mantissa) {
            return Err("number is outside the supported finite f64 range".into());
        }
        None
    };
    Ok(Value::Number(Number { raw: raw.into(), integer }))
}

fn valid_date(raw: &str) -> bool {
    let b = raw.as_bytes();
    if b.len() != 10
        || b[4] != b'-'
        || b[7] != b'-'
        || !b.iter().enumerate().all(|(i, c)| i == 4 || i == 7 || c.is_ascii_digit())
    {
        return false;
    }
    let year: u32 = raw[..4].parse().unwrap();
    let month: usize = raw[5..7].parse().unwrap();
    let day: u32 = raw[8..].parse().unwrap();
    let leap = year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
    let days = [0, 31, 28 + u32::from(leap), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    (1..=12).contains(&month) && day > 0 && day <= days[month]
}
fn valid_datetime(raw: &str) -> bool {
    let b = raw.as_bytes();
    if b.len() != 19
        || b[10] != b' '
        || b[13] != b':'
        || b[16] != b':'
        || !b[11..].iter().enumerate().all(|(i, c)| i == 2 || i == 5 || c.is_ascii_digit())
        || !raw.is_char_boundary(10)
        || !valid_date(&raw[..10])
    {
        return false;
    }
    raw[11..13].parse::<u32>().unwrap() < 24
        && raw[14..16].parse::<u32>().unwrap() < 60
        && raw[17..].parse::<u32>().unwrap() < 60
}
