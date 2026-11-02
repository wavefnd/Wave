// SPDX-License-Identifier: MPL-2.0
use crate::{config, error, Result};
use std::ffi::OsString;
use std::path::Path;
use utils::wson::Value;

struct Reader<'a> {
    bytes: &'a [u8],
    offset: usize,
}
impl<'a> Reader<'a> {
    fn byte(&mut self) -> Result<u8> {
        let value =
            *self.bytes.get(self.offset).ok_or_else(|| error("truncated WebAssembly field"))?;
        self.offset += 1;
        Ok(value)
    }

    fn uint(&mut self, bits: u32) -> Result<u64> {
        let mut result = 0u64;
        let mut shift = 0;
        loop {
            let byte = self.byte()?;
            let payload = (byte & 127) as u64;
            if shift >= bits
                || payload > (u64::MAX >> shift)
                || bits < 64 && payload > (((1u64 << bits) - 1) >> shift)
            {
                return Err(error("WebAssembly integer overflow"));
            }
            result |= payload << shift;
            if byte & 128 == 0 {
                return Ok(result);
            }
            shift += 7;
        }
    }

    fn take(&mut self, length: usize) -> Result<&'a [u8]> {
        let end =
            self.offset.checked_add(length).ok_or_else(|| error("WebAssembly range overflow"))?;
        let result = self
            .bytes
            .get(self.offset..end)
            .ok_or_else(|| error("truncated WebAssembly section"))?;
        self.offset = end;
        Ok(result)
    }

    fn name(&mut self) -> Result<&'a str> {
        let length = self.uint(32)? as usize;
        Ok(std::str::from_utf8(self.take(length)?)?)
    }

    fn limits(&mut self) -> Result<u32> {
        let flags = self.uint(32)?;
        if flags & !7 != 0 || flags & 2 != 0 && flags & 1 == 0 {
            return Err(error("invalid WebAssembly limits flags"));
        }
        let bits = if flags & 4 != 0 { 64 } else { 32 };
        let min = self.uint(bits)?;
        if flags & 1 != 0 && self.uint(bits)? < min {
            return Err(error("invalid WebAssembly max/min limits"));
        }
        Ok(bits)
    }

    fn end(&self) -> Result<()> {
        if self.offset == self.bytes.len() {
            Ok(())
        } else {
            Err(error("trailing WebAssembly section bytes"))
        }
    }
}

pub fn verify(path: &Path, expected: Option<u32>) -> Result<()> {
    let bytes = std::fs::read(path)?;
    if !bytes.starts_with(b"\0asm\x01\0\0\0") {
        return Err(error("invalid WebAssembly magic/version"));
    }
    let mut reader = Reader { bytes: &bytes[8..], offset: 0 };
    let mut seen = std::collections::BTreeSet::new();
    let mut widths = std::collections::BTreeSet::new();
    let mut memory64 = false;
    while reader.offset < reader.bytes.len() {
        let kind = reader.byte()?;
        if kind > 13 || kind != 0 && !seen.insert(kind) {
            return Err(error("invalid/duplicate WebAssembly section"));
        }
        let length = reader.uint(32)? as usize;
        let mut section = Reader { bytes: reader.take(length)?, offset: 0 };
        match kind {
            0 => {
                if section.name()? == "target_features" {
                    for _ in 0..section.uint(32)? {
                        let prefix = section.byte()?;
                        let feature = section.name()?;
                        if ![b'+', b'-', b'='].contains(&prefix) {
                            return Err(error("invalid WebAssembly feature prefix"));
                        }
                        if feature == "memory64" && prefix != b'-' {
                            memory64 = true;
                        }
                    }
                    section.end()?;
                }
            },
            2 => {
                for _ in 0..section.uint(32)? {
                    section.name()?;
                    section.name()?;
                    match section.byte()? {
                        0 => {
                            section.uint(32)?;
                        },
                        1 => {
                            section.byte()?;
                            section.limits()?;
                        },
                        2 => {
                            widths.insert(section.limits()?);
                        },
                        3 => {
                            section.byte()?;
                            section.byte()?;
                        },
                        4 => {
                            section.uint(32)?;
                            section.uint(32)?;
                        },
                        _ => return Err(error("invalid WebAssembly import kind")),
                    }
                }
                section.end()?;
            },
            5 => {
                for _ in 0..section.uint(32)? {
                    widths.insert(section.limits()?);
                }
                section.end()?;
            },
            _ => {},
        }
    }
    if widths.len() > 1 || memory64 && widths.contains(&32) || widths.is_empty() && !memory64 {
        return Err(error("missing or inconsistent WebAssembly address-width contract"));
    }
    let bits = if memory64 { 64 } else { *widths.first().unwrap() };
    if expected.is_some_and(|expected| expected != bits) {
        return Err(error("WebAssembly address-width mismatch"));
    }
    Ok(())
}

pub fn execution_command(value: &Value, artifact: &Path) -> Result<Vec<OsString>> {
    let link = value.get("link").ok_or_else(|| error("missing linked module plan"))?;
    if Path::new(config::text(link, "output")?) != artifact {
        return Err(error("planned module differs from built artifact"));
    }
    let execute = value.get("execute").ok_or_else(|| error("missing WebAssembly host command"))?;
    let program = config::text(execute, "program")?;
    if program.is_empty() {
        return Err(error("empty host command"));
    }
    let mut command = vec![OsString::from(program)];
    for value in config::array(execute, "args")? {
        let Value::String(value) = value else {
            return Err(error("host arguments must be strings"));
        };
        command.push(value.into());
    }
    if !command[1..].iter().any(|a| Path::new(a) == artifact) {
        return Err(error("host command does not reference built artifact"));
    }
    Ok(command)
}
