// SPDX-License-Identifier: MPL-2.0
//! Bounded inspection of COFF objects and Microsoft/GNU archives.
//! Layouts: Microsoft PE/COFF documentation and LLVM BinaryFormat/COFF.h.
use std::path::Path;

fn range(data: &[u8], offset: usize, size: usize) -> Result<&[u8], String> {
    offset
        .checked_add(size)
        .and_then(|end| data.get(offset..end))
        .ok_or_else(|| format!("truncated COFF input at offset {offset}, length {size}"))
}

fn u16_at(data: &[u8], offset: usize) -> Result<u16, String> {
    Ok(u16::from_le_bytes(range(data, offset, 2)?.try_into().unwrap()))
}

fn u32_at(data: &[u8], offset: usize) -> Result<usize, String> {
    Ok(u32::from_le_bytes(range(data, offset, 4)?.try_into().unwrap()) as usize)
}

fn table(data: &[u8], start: usize, count: usize, width: usize) -> Result<(), String> {
    range(data, start, count.checked_mul(width).ok_or("COFF table size overflow")?)?;
    Ok(())
}

fn machine(actual: u16, expected: u16) -> Result<(), String> {
    if actual == expected {
        Ok(())
    } else {
        Err(format!("COFF machine 0x{actual:04x} does not match target machine 0x{expected:04x}"))
    }
}

fn object(data: &[u8], expected: u16) -> Result<(), String> {
    if data.starts_with(b"BC\xc0\xde") || data.starts_with(&[0xde, 0xc0, 0x17, 0x0b]) {
        return Err("LLVM bitcode inputs are not supported by the MSVC input inspector; emit a target COFF object first".into());
    }
    if data.starts_with(b"MZ") {
        return Err("PE images are not relocatable objects; link the import library instead".into());
    }
    range(data, 0, 20)?;
    // VC SDK libraries include machine-neutral debug/weak-alias objects. They
    // contain no instructions or target relocations and can accompany either CPU.
    let neutral = u16_at(data, 0)? == 0 && !data.starts_with(&[0, 0, 255, 255]);
    let (header, sections, symbols, count, symbol_width) = if data.starts_with(&[0, 0, 255, 255]) {
        machine(u16_at(data, 6)?, expected)?;
        let version = u16_at(data, 4)?;
        if version == 0 {
            let payload = range(data, 20, u32_at(data, 12)?)?;
            let flags = u16_at(data, 18)?;
            if flags & !0x1f != 0 || flags & 3 > 2 || (flags >> 2) & 7 > 4 {
                return Err("invalid short import-object flags".into());
            }
            let required = if (flags >> 2) & 7 == 4 { 3 } else { 2 };
            if payload.iter().filter(|byte| **byte == 0).count() < required {
                return Err("truncated import-object names".into());
            }
            return Ok(());
        }
        const BIGOBJ: [u8; 16] = [
            0xc7, 0xa1, 0xba, 0xd1, 0xee, 0xba, 0xa9, 0x4b, 0xaf, 0x20, 0xfa, 0xf6, 0x6a, 0xa4,
            0xdc, 0xb8,
        ];
        range(data, 0, 56)?;
        if version < 2 || range(data, 12, 16)? != BIGOBJ {
            return Err("unsupported anonymous COFF object (expected bigobj)".into());
        }
        (56, u32_at(data, 44)?, u32_at(data, 48)?, u32_at(data, 52)?, 20)
    } else {
        if !neutral {
            machine(u16_at(data, 0)?, expected)?;
        }
        if u16_at(data, 16)? != 0 || u16_at(data, 18)? & 2 != 0 {
            return Err(
                "expected a relocatable COFF object without an optional image header".into()
            );
        }
        (20, u16_at(data, 2)? as usize, u32_at(data, 8)?, u32_at(data, 12)?, 18)
    };
    table(data, header, sections, 40)?;
    for index in 0..sections {
        let section = header + index * 40;
        if neutral {
            let name = range(data, section, 8)?;
            if !(name.starts_with(b".debug$") || name == b".drectve")
                || u16_at(data, section + 32)? != 0
                || u32_at(data, section + 36)? & (0x20 | 0x2000_0000) != 0
            {
                return Err(
                    "machine-neutral COFF object contains target-specific sections or relocations"
                        .into(),
                );
            }
        }
        let raw_size = u32_at(data, section + 16)?;
        let raw = u32_at(data, section + 20)?;
        // Uninitialized sections legitimately have a size and no file payload.
        if raw != 0 {
            range(data, raw, raw_size)?;
        }
        let reloc = u32_at(data, section + 24)?;
        let mut reloc_count = u16_at(data, section + 32)? as usize;
        if u32_at(data, section + 36)? & 0x0100_0000 != 0 {
            if reloc_count != 65535 {
                return Err("invalid relocation overflow count".into());
            }
            reloc_count = u32_at(data, reloc)?;
            if reloc_count == 0 {
                return Err("empty relocation overflow table".into());
            }
        }
        if reloc_count != 0 {
            if reloc == 0 {
                return Err("missing relocation table".into());
            }
            table(data, reloc, reloc_count, 10)?;
        }
        let lines = u16_at(data, section + 34)? as usize;
        if lines != 0 {
            table(data, u32_at(data, section + 28)?, lines, 6)?;
        }
    }
    if symbols != 0 {
        table(data, symbols, count, symbol_width)?;
        let strings = symbols
            .checked_add(count.checked_mul(symbol_width).ok_or("symbol size overflow")?)
            .ok_or("symbol offset overflow")?;
        let size = u32_at(data, strings)?;
        if size < 4 {
            return Err("invalid COFF string table size".into());
        }
        range(data, strings, size)?;
    } else if count != 0 {
        return Err("missing COFF symbol table".into());
    }
    Ok(())
}

pub fn inspect(data: &[u8], target: &str) -> Result<(), String> {
    inspect_with_policy(data, target, true)
}

fn inspect_with_policy(data: &[u8], target: &str, lazy_archive: bool) -> Result<(), String> {
    let expected = match target {
        "x86_64-pc-windows-msvc" => 0x8664,
        "aarch64-pc-windows-msvc" => 0xaa64,
        _ => return Err(format!("unsupported MSVC target {target}")),
    };
    if data.starts_with(b"!<thin>\n") {
        return Err("thin archives are not supported; use a regular COFF archive".into());
    }
    if !data.starts_with(b"!<arch>\n") {
        return object(data, expected);
    }
    let mut offset = 8;
    let mut names: &[u8] = &[];
    let mut member_offsets = std::collections::HashSet::new();
    let mut indexes = Vec::new();
    let mut ec_index = None;
    let mut objects = Vec::new();
    while offset < data.len() {
        let header = range(data, offset, 60)?;
        if &header[58..60] != b"`\n" {
            return Err(format!("invalid archive header at {offset}"));
        }
        let length: usize = std::str::from_utf8(&header[48..58])
            .map_err(|_| "invalid archive size")?
            .trim()
            .parse()
            .map_err(|_| "invalid archive size")?;
        let name = std::str::from_utf8(&header[..16]).map_err(|_| "invalid archive name")?.trim();
        let mut payload = range(data, offset + 60, length)?;
        let mut display = name.to_string();
        if name == "/" {
            indexes.push(payload);
        } else if name == "//" {
            names = payload;
        } else if name == "/<ECSYMBOLS>/" {
            if ec_index.replace(payload).is_some() {
                return Err("duplicate archive EC symbol index".into());
            }
        } else if name != "/SYM64/" {
            member_offsets.insert(offset);
            if let Some(size) = name.strip_prefix("#1/") {
                let size: usize = size.parse().map_err(|_| "invalid BSD archive name length")?;
                display = String::from_utf8_lossy(range(payload, 0, size)?)
                    .trim_end_matches('\0')
                    .to_string();
                payload = &payload[size..];
            } else if let Some(index) = name.strip_prefix('/') {
                let index: usize = index.parse().map_err(|_| "invalid archive long-name offset")?;
                let tail = names.get(index..).ok_or("archive long-name offset out of bounds")?;
                let end = tail
                    .iter()
                    .position(|b| *b == 0 || *b == b'\n')
                    .ok_or("unterminated archive long name")?;
                display = String::from_utf8_lossy(&tail[..end]).trim_end_matches('/').to_string();
            }
            objects.push((offset, display, payload));
        }
        offset = offset
            .checked_add(60)
            .and_then(|v| v.checked_add(length))
            .ok_or("archive offset overflow")?;
        if offset % 2 != 0 {
            // Microsoft librarians also use NUL for an odd member's padding.
            if !matches!(range(data, offset, 1)?, b"\n" | b"\0") {
                return Err("invalid archive alignment byte".into());
            }
            offset += 1;
        }
    }
    if indexes.len() > 2 {
        return Err("too many archive symbol indexes".into());
    }
    let mut native_members = std::collections::HashSet::new();
    let mut second_offsets = Vec::new();
    for (index, payload) in indexes.iter().enumerate() {
        let count = if index == 0 {
            u32::from_be_bytes(range(payload, 0, 4)?.try_into().unwrap()) as usize
        } else {
            u32_at(payload, 0)?
        };
        table(payload, 4, count, 4)?;
        for item in 0..count {
            let offset = if index == 0 {
                u32::from_be_bytes(range(payload, 4 + item * 4, 4)?.try_into().unwrap()) as usize
            } else {
                u32_at(payload, 4 + item * 4)?
            };
            if index == 0 {
                native_members.insert(offset);
            } else {
                second_offsets.push(offset);
            }
            if !member_offsets.contains(&offset) {
                return Err("archive symbol index references a nonexistent member".into());
            }
        }
        let end = 4 + count * 4;
        if index == 0 {
            if payload[end..].iter().filter(|b| **b == 0).count() < count {
                return Err("truncated archive symbol names".into());
            }
        } else {
            let symbols = u32_at(payload, end)?;
            table(payload, end + 4, symbols, 2)?;
            for item in 0..symbols {
                let member = u16_at(payload, end + 4 + item * 2)? as usize;
                if member == 0 || member > count {
                    return Err("invalid archive symbol member index".into());
                }
                native_members.insert(second_offsets[member - 1]);
            }
            if payload[end + 4 + symbols * 2..].iter().filter(|b| **b == 0).count() < symbols {
                return Err("truncated archive symbol names".into());
            }
        }
    }
    let mut ec_members = std::collections::HashSet::new();
    if let Some(ec) = ec_index {
        if indexes.len() != 2 {
            return Err("archive EC index requires a second linker member".into());
        }
        let symbols = u32_at(ec, 0)?;
        table(ec, 4, symbols, 2)?;
        for item in 0..symbols {
            let member = u16_at(ec, 4 + item * 2)? as usize;
            if member == 0 || member > second_offsets.len() {
                return Err("invalid EC symbol member index".into());
            }
            ec_members.insert(second_offsets[member - 1]);
        }
        if ec[4 + symbols * 2..].iter().filter(|b| **b == 0).count() < symbols {
            return Err("truncated EC symbol names".into());
        }
    }
    for (offset, display, payload) in objects {
        // Native ARM64 resolves the ordinary archive symbol index, not
        // /<ECSYMBOLS>/. SDK hybrid libraries may carry EC-only members. Still
        // validate their structure, and never exempt directly supplied objects,
        // foreign members in ordinary archives, or /WHOLEARCHIVE members.
        let actual = u16_at(payload, if payload.starts_with(&[0, 0, 255, 255]) { 6 } else { 0 })?;
        let ec_only = lazy_archive
            && expected == 0xaa64
            && matches!(actual, 0xa641 | 0x8664)
            && !ec_members.is_empty()
            && !native_members.is_empty()
            && !native_members.contains(&offset);
        object(payload, if ec_only { actual } else { expected })
            .map_err(|e| format!("archive member '{display}': {e}"))?;
    }
    // Empty archives are legal linker inputs, but do not prove any machine type.
    Ok(())
}

pub fn validate_file(path: &Path, target: &str) -> Result<(), String> {
    let bytes = std::fs::read(path).map_err(|e| format!("{}: {e}", path.display()))?;
    inspect(&bytes, target).map_err(|e| format!("{}: {e}", path.display()))
}

/// Whole-archive linking selects every member, including EC-only members.
pub fn validate_file_all_members(path: &Path, target: &str) -> Result<(), String> {
    let bytes = std::fs::read(path).map_err(|e| format!("{}: {e}", path.display()))?;
    inspect_with_policy(&bytes, target, false).map_err(|e| format!("{}: {e}", path.display()))
}
