// SPDX-License-Identifier: MPL-2.0
//! WSON writer adapted from wson-rust's serializer, sharing strict JSON escaping.
use super::*;
use std::fmt::Write;

pub fn quote(value: &str) -> String {
    let mut out = String::from("\"");
    for ch in value.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\x08' => out.push_str("\\b"),
            '\x0c' => out.push_str("\\f"),
            c if c < '\x20' => {
                write!(out, "\\u{:04x}", c as u32).unwrap();
            },
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

pub(super) fn serialize(
    value: &Value,
    format: Format,
    pretty: bool,
    max_depth: usize,
) -> Result<String, Error> {
    let mut out = String::new();
    emit(&mut out, value, format, pretty, 0, max_depth)?;
    Ok(out)
}

fn fail(out: &str, message: &str) -> Error {
    Error::at(out, out.len(), message)
}

fn newline(out: &mut String, pretty: bool, depth: usize) {
    if pretty {
        out.push('\n');
        out.push_str(&"  ".repeat(depth));
    }
}

fn emit(
    out: &mut String,
    value: &Value,
    format: Format,
    pretty: bool,
    depth: usize,
    max_depth: usize,
) -> Result<(), Error> {
    if matches!(value, Value::Array(_) | Value::Object(_)) && depth >= max_depth {
        return Err(fail(out, "maximum nesting depth exceeded"));
    }
    match value {
        Value::Null => out.push_str("null"),
        Value::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        Value::Number(n) => out.push_str(n.as_str()),
        Value::String(s) => out.push_str(&quote(s)),
        Value::Date(s) | Value::DateTime(s) => {
            if format == Format::Json {
                return Err(fail(out, "WSON date requires explicit conversion for JSON"));
            }
            let valid =
                if matches!(value, Value::Date(_)) { valid_date(s) } else { valid_datetime(s) };
            if !valid {
                return Err(fail(out, "invalid date or datetime"));
            }
            out.push_str(s);
        },
        Value::Version(parts) => {
            if format == Format::Json {
                return Err(fail(out, "WSON version requires explicit conversion for JSON"));
            }
            if parts.len() < 3 {
                return Err(fail(out, "version requires at least three components"));
            }
            out.push_str(&parts.iter().map(u32::to_string).collect::<Vec<_>>().join("."));
        },
        Value::Array(values) => {
            out.push('[');
            for (i, v) in values.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                newline(out, pretty, depth + 1);
                emit(out, v, format, pretty, depth + 1, max_depth)?;
            }
            if !values.is_empty() {
                newline(out, pretty, depth);
            }
            out.push(']');
        },
        Value::Object(fields) => {
            let mut keys = std::collections::HashSet::new();
            out.push('{');
            for (i, (key, value)) in fields.iter().enumerate() {
                if !keys.insert(key) {
                    return Err(fail(out, "duplicate object key"));
                }
                if i > 0 {
                    out.push(',');
                }
                newline(out, pretty, depth + 1);
                out.push_str(&quote(key));
                out.push_str(match (format, pretty) {
                    (Format::Wson, _) => " = ",
                    (Format::Json, true) => ": ",
                    _ => ":",
                });
                emit(out, value, format, pretty, depth + 1, max_depth)?;
            }
            if !fields.is_empty() {
                newline(out, pretty, depth);
            }
            out.push('}');
        },
    }
    Ok(())
}
