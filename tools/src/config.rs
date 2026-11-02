// SPDX-License-Identifier: MPL-2.0
use crate::{error, Result};
use std::path::Path;
use utils::wson::{self, Format, Value};

pub fn read_json(path: &Path) -> Result<Value> {
    parse_json(&std::fs::read_to_string(path)?)
}

pub fn parse_json(text: &str) -> Result<Value> {
    Ok(wson::parse_json(text)?)
}

pub fn json(value: &Value) -> Result<String> {
    Ok(wson::dumps(value, Format::Json, true)? + "\n")
}

pub fn text<'a>(value: &'a Value, key: &str) -> Result<&'a str> {
    value.get_str(key).ok_or_else(|| error(format!("missing or non-string field {key}")))
}

pub fn array<'a>(value: &'a Value, key: &str) -> Result<&'a [Value]> {
    value.get_arr(key).ok_or_else(|| error(format!("missing or non-array field {key}")))
}

pub fn number(value: i64) -> Value {
    wson::parse_json(&value.to_string()).unwrap()
}

pub fn boolean(value: &Value, key: &str) -> Result<bool> {
    match value.get(key) {
        Some(Value::Bool(value)) => Ok(*value),
        _ => Err(error(format!("missing or non-boolean field {key}"))),
    }
}

pub fn field(value: &mut Value, key: &str, data: Value) -> Result<()> {
    let Value::Object(fields) = value else {
        return Err(error("expected object"));
    };
    if let Some((_, existing)) = fields.iter_mut().find(|(name, _)| name == key) {
        *existing = data;
    } else {
        fields.push((key.into(), data));
    }
    Ok(())
}

pub fn pin(name: &str) -> Result<String> {
    Ok(text(&read_json(&crate::root().join("tools/ci/toolchains.json"))?, name)?.into())
}

pub fn compiler_version() -> Result<String> {
    let manifest: toml::Value =
        std::fs::read_to_string(crate::root().join("Cargo.toml"))?.parse()?;
    manifest
        .get("package")
        .and_then(|v| v.get("version"))
        .and_then(toml::Value::as_str)
        .map(String::from)
        .ok_or_else(|| error("missing Cargo package version"))
}

pub fn env_name(name: &str) -> Result<()> {
    if name.is_empty()
        || !name.bytes().enumerate().all(|(index, b)| {
            b == b'_' || b.is_ascii_alphabetic() || index > 0 && b.is_ascii_digit()
        })
    {
        return Err(error("invalid environment variable name"));
    }
    Ok(())
}

pub fn encode_env(name: &str, value: &str) -> Result<Vec<u8>> {
    env_name(name)?;
    if value.contains('\0') {
        return Err(error("environment values cannot contain NUL"));
    }
    if !value.contains(['\r', '\n']) {
        return Ok(format!("{name}={value}\n").into_bytes());
    }
    let seed = crate::storage::bytes_digest(value.as_bytes());
    let mut index = 0;
    loop {
        let delimiter = format!("wave_{seed}_{index}");
        if !value.split('\n').any(|line| line.trim_end_matches('\r') == delimiter) {
            return Ok(format!("{name}<<{delimiter}\n{value}\n{delimiter}\n").into_bytes());
        }
        index += 1;
    }
}

pub fn decode_env(bytes: &[u8]) -> Result<std::collections::BTreeMap<String, String>> {
    let text = std::str::from_utf8(bytes)?.trim_start_matches('\u{feff}');
    if text.contains('\0') {
        return Err(error("environment values cannot contain NUL"));
    }
    let mut values = std::collections::BTreeMap::new();
    let lines: Vec<_> = text.split_inclusive('\n').collect();
    let mut index = 0;
    while index < lines.len() {
        let line = lines[index]
            .trim_end_matches('\n')
            .strip_suffix('\r')
            .unwrap_or(lines[index].trim_end_matches('\n'));
        index += 1;
        if line.is_empty() {
            continue;
        }
        let assignment = line.find('=');
        let multiline = line.find("<<");
        let (name, value) =
            if assignment.is_some() && (multiline.is_none() || assignment < multiline) {
                let (name, value) = line.split_once('=').unwrap();
                (name, value.to_string())
            } else if let Some((name, delimiter)) = line.split_once("<<") {
                if delimiter.is_empty() || delimiter.contains(['\r', '\n']) {
                    return Err(error("invalid environment delimiter"));
                }
                let start = index;
                while index < lines.len()
                    && lines[index].trim_end_matches('\n').trim_end_matches('\r') != delimiter
                {
                    index += 1;
                }
                if index == lines.len() {
                    return Err(error("unterminated environment record"));
                }
                let mut value = lines[start..index].concat();
                if value.ends_with('\n') {
                    value.pop();
                }
                index += 1;
                (name, value)
            } else {
                return Err(error("invalid environment record"));
            };
        env_name(name)?;
        values.insert(name.to_string(), value);
    }
    Ok(values)
}

pub fn append_env(path: &Path, name: &str, value: &str) -> Result<()> {
    use std::io::{Read, Seek, SeekFrom, Write};
    let mut record = encode_env(name, value)?;
    decode_env(&record)?;
    let mut file = std::fs::OpenOptions::new().create(true).read(true).append(true).open(path)?;
    // All writers through this codec serialize the rollback boundary as well.
    file.lock()?;
    let length = file.seek(SeekFrom::End(0))?;
    if length > 0 {
        file.seek(SeekFrom::End(-1))?;
        let mut last = [0];
        file.read_exact(&mut last)?;
        if last[0] != b'\n' {
            record.insert(0, b'\n');
        }
    }
    if let Err(error) = file.write_all(&record).and_then(|_| file.flush()) {
        file.set_len(length)?;
        return Err(error.into());
    }
    Ok(())
}

/// JSON object order is a serialization detail, unlike array element order.
pub fn equivalent(first: &Value, second: &Value) -> bool {
    match (first, second) {
        (Value::Object(a), Value::Object(b)) => {
            a.len() == b.len()
                && a.iter().all(|(key, value)| {
                    b.iter()
                        .find(|(other, _)| other == key)
                        .is_some_and(|(_, other)| equivalent(value, other))
                })
        },
        (Value::Array(a), Value::Array(b)) => {
            a.len() == b.len() && a.iter().zip(b).all(|(a, b)| equivalent(a, b))
        },
        _ => first == second,
    }
}

pub fn main(arguments: &[std::ffi::OsString]) -> Result<i32> {
    let args = crate::cli::Args::new(arguments);
    if args.values.len() != 1 {
        return Err(error("config expects one key"));
    }
    match args.values[0].to_str() {
        Some("version") => println!("{}", compiler_version()?),
        Some("rust-version") => println!("{}", pin("RUST_VERSION")?),
        Some("build-inputs") => {
            for name in [
                "Cargo.toml",
                "Cargo.lock",
                "tools/Cargo.toml",
                "utils/Cargo.toml",
                "tools/ci/toolchains.json",
            ] {
                println!("{}", crate::root().join(name).display());
            }
            for folder in ["tools/src", "tools/tests", "utils/src"] {
                for path in crate::storage::files(&crate::root().join(folder))? {
                    if path.extension().is_some_and(|v| v == "rs") {
                        println!("{}", path.display());
                    }
                }
            }
        },
        _ => return Err(error("unknown configuration key")),
    }
    Ok(0)
}
