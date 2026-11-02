// SPDX-License-Identifier: MPL-2.0
//! Inspect required binary bytes directly: no successful-empty fallback to `file`/`ldd`.
use crate::{error, Result};
use object::{Object, ObjectSection};
use std::collections::BTreeSet;
use std::path::Path;

#[derive(Debug, Default, Clone)]
pub struct Info {
    pub architectures: BTreeSet<String>,
    pub format: String,
    pub bits: u32,
    pub flags: u32,
    pub needed: BTreeSet<String>,
    pub rpaths: BTreeSet<String>,
    pub interpreter: Option<String>,
}

fn arch(architecture: object::Architecture) -> String {
    match architecture {
        object::Architecture::X86_64 => "x86_64",
        object::Architecture::Aarch64 => "aarch64",
        object::Architecture::Riscv64 => "riscv64",
        object::Architecture::LoongArch64 => "loongarch64",
        object::Architecture::I386 => "i686",
        object::Architecture::Wasm32 => "wasm32",
        object::Architecture::Wasm64 => "wasm64",
        _ => "unknown",
    }
    .into()
}

pub fn target_arch(target: &str) -> &str {
    match target.split('-').next().unwrap_or("") {
        "amd64" => "x86_64",
        "arm64" => "aarch64",
        "riscv64gc" => "riscv64",
        "loong64" => "loongarch64",
        value => value,
    }
}

fn range(bytes: &[u8], offset: u64, length: u64) -> Result<&[u8]> {
    let end = offset.checked_add(length).ok_or_else(|| error("binary range overflow"))?;
    let start = usize::try_from(offset)?;
    let end = usize::try_from(end)?;
    bytes.get(start..end).ok_or_else(|| error("truncated binary range"))
}

fn integer(bytes: &[u8], offset: u64, width: u64, little: bool) -> Result<u64> {
    let data = range(bytes, offset, width)?;
    let mut buffer = [0u8; 8];
    if little {
        buffer[..data.len()].copy_from_slice(data);
        Ok(u64::from_le_bytes(buffer))
    } else {
        buffer[8 - data.len()..].copy_from_slice(data);
        Ok(u64::from_be_bytes(buffer))
    }
}

fn string(bytes: &[u8], offset: u64) -> Result<String> {
    let tail = bytes
        .get(usize::try_from(offset)?..)
        .ok_or_else(|| error("string offset outside binary"))?;
    let end =
        tail.iter().position(|b| *b == 0).ok_or_else(|| error("unterminated binary string"))?;
    Ok(std::str::from_utf8(&tail[..end])?.to_owned())
}

fn elf(bytes: &[u8], info: &mut Info, little: bool) -> Result<()> {
    let wide = info.bits == 64;
    let word = if wide { 8 } else { 4 };
    let offset = integer(bytes, if wide { 32 } else { 28 }, word, little)?;
    let size = integer(bytes, if wide { 54 } else { 42 }, 2, little)?;
    let count = integer(bytes, if wide { 56 } else { 44 }, 2, little)?;
    if count == 0 {
        return Ok(());
    }
    if size < (if wide { 56 } else { 32 }) {
        return Err(error("invalid ELF program header size"));
    }
    range(bytes, offset, size.checked_mul(count).ok_or_else(|| error("ELF table overflow"))?)?;
    let mut loads = Vec::new();
    let mut dynamic = None;
    for index in 0..count {
        let start = offset + index * size;
        let kind = integer(bytes, start, 4, little)?;
        let file = integer(bytes, start + if wide { 8 } else { 4 }, word, little)?;
        let address = integer(bytes, start + if wide { 16 } else { 8 }, word, little)?;
        let length = integer(bytes, start + if wide { 32 } else { 16 }, word, little)?;
        range(bytes, file, length)?;
        if kind == 1 {
            loads.push((address, file, length));
        }
        if kind == 3 {
            if info.interpreter.is_some() {
                return Err(error("multiple ELF interpreters"));
            }
            let value = range(bytes, file, length)?;
            if value.last() != Some(&0) {
                return Err(error("unterminated ELF interpreter"));
            }
            info.interpreter = Some(std::str::from_utf8(&value[..value.len() - 1])?.into());
        }
        if kind == 2 && dynamic.replace((file, length)).is_some() {
            return Err(error("multiple ELF dynamic tables"));
        }
    }
    let Some((offset, length)) = dynamic else {
        return Ok(());
    };
    if length % (word * 2) != 0 {
        return Err(error("invalid ELF dynamic table length"));
    }
    let mut entries = Vec::new();
    let mut terminated = false;
    for index in 0..length / (word * 2) {
        let tag = integer(bytes, offset + index * word * 2, word, little)?;
        let value = integer(bytes, offset + index * word * 2 + word, word, little)?;
        if tag == 0 {
            terminated = true;
            break;
        }
        entries.push((tag, value));
    }
    if !terminated {
        return Err(error("unterminated ELF dynamic table"));
    }
    let needed = entries.iter().any(|(tag, _)| [1, 15, 29].contains(tag));
    if !needed {
        return Ok(());
    }
    let address = entries
        .iter()
        .find(|(tag, _)| *tag == 5)
        .ok_or_else(|| error("missing ELF dynamic string table"))?
        .1;
    let length = entries
        .iter()
        .find(|(tag, _)| *tag == 10)
        .ok_or_else(|| error("missing ELF dynamic string table size"))?
        .1;
    let file = loads
        .iter()
        .find_map(|(base, file, size)| {
            address
                .checked_sub(*base)
                .filter(|offset| *offset < *size && length <= size - offset)
                .map(|offset| file + offset)
        })
        .ok_or_else(|| error("ELF string table is not file-backed"))?;
    let strings = range(bytes, file, length)?;
    for (tag, value) in entries {
        if tag == 1 {
            info.needed.insert(string(strings, value)?);
        }
        if tag == 15 || tag == 29 {
            info.rpaths.extend(string(strings, value)?.split(':').map(String::from));
        }
    }
    Ok(())
}

fn pe(bytes: &[u8], info: &mut Info) -> Result<()> {
    let header = integer(bytes, 60, 4, true)?;
    if range(bytes, header, 4)? != b"PE\0\0" {
        return Err(error("invalid PE signature"));
    }
    let sections = integer(bytes, header + 6, 2, true)?;
    let optional_size = integer(bytes, header + 20, 2, true)?;
    let optional = header + 24;
    if optional_size < 112 || integer(bytes, optional, 2, true)? != 0x20b {
        return Err(error("expected complete PE32+ optional header"));
    }
    let directories = integer(bytes, optional + 108, 4, true)?;
    if directories > 16 || optional_size < 112 + directories * 8 {
        return Err(error("invalid PE data directory count"));
    }
    let table = optional + optional_size;
    range(bytes, table, sections * 40)?;
    let mut ranges = Vec::new();
    for index in 0..sections {
        let start = table + index * 40;
        let address = integer(bytes, start + 12, 4, true)?;
        let length = integer(bytes, start + 16, 4, true)?;
        let file = integer(bytes, start + 20, 4, true)?;
        if length > 0 {
            if file < table + sections * 40 {
                return Err(error("PE section overlaps headers"));
            }
            range(bytes, file, length)?;
            ranges.push((address, file, length));
        }
    }
    let mapped = |rva: u64, length: u64| -> Result<u64> {
        ranges
            .iter()
            .find_map(|(address, file, size)| {
                rva.checked_sub(*address)
                    .filter(|offset| *offset < *size && length <= size - offset)
                    .map(|offset| file + offset)
            })
            .ok_or_else(|| error("PE RVA outside file-backed sections"))
    };
    for index in [1u64, 13] {
        if directories <= index {
            continue;
        }
        let rva = integer(bytes, optional + 112 + index * 8, 4, true)?;
        let size = integer(bytes, optional + 116 + index * 8, 4, true)?;
        if rva == 0 && size == 0 {
            continue;
        }
        let entry = if index == 1 { 20 } else { 32 };
        let offset = mapped(rva, size)?;
        let mut terminated = false;
        for position in (0..size / entry).map(|n| offset + n * entry) {
            let raw = range(bytes, position, entry)?;
            if raw.iter().all(|b| *b == 0) {
                terminated = true;
                break;
            }
            let name = if index == 1 {
                integer(bytes, position + 12, 4, true)?
            } else {
                let flags = integer(bytes, position, 4, true)?;
                let name = integer(bytes, position + 4, 4, true)?;
                match flags {
                    1 => name,
                    0 => name
                        .checked_sub(integer(bytes, optional + 24, 8, true)?)
                        .ok_or_else(|| error("delay import VA precedes image base"))?,
                    _ => return Err(error("invalid PE delay import attributes")),
                }
            };
            let mapped = mapped(name, 1)?;
            let name = string(bytes, mapped)?;
            if name.is_empty() || name.contains(['/', '\\', ':']) {
                return Err(error("invalid imported DLL name"));
            }
            info.needed.insert(name);
        }
        if !terminated {
            return Err(error("missing PE import terminator"));
        }
    }
    Ok(())
}

fn macho(bytes: &[u8], info: &mut Info, little: bool) -> Result<()> {
    let header = if info.bits == 64 { 32 } else { 28 };
    let count = integer(bytes, 16, 4, little)?;
    let total = integer(bytes, 20, 4, little)?;
    range(bytes, header, total)?;
    let mut offset = header;
    for _ in 0..count {
        let command = integer(bytes, offset, 4, little)?;
        let size = integer(bytes, offset + 4, 4, little)?;
        if size < 8 || offset.checked_add(size).is_none_or(|end| end > header + total) {
            return Err(error("invalid Mach-O load command size"));
        }
        let data = range(bytes, offset, size)?;
        if [0xc, 0x80000018, 0x8000001f, 0x80000023, 0x20].contains(&command) {
            info.needed.insert(string(data, integer(data, 8, 4, little)?)?);
        }
        if command == 0x8000001c {
            info.rpaths.insert(string(data, integer(data, 8, 4, little)?)?);
        }
        offset += size;
    }
    if offset != header + total {
        return Err(error("Mach-O load command count/size mismatch"));
    }
    Ok(())
}

fn thin(bytes: &[u8]) -> Result<Info> {
    if object::FileKind::parse(bytes)? == object::FileKind::CoffImport {
        let file = object::read::coff::ImportFile::parse(bytes)?;
        let architecture = arch(file.architecture());
        return Ok(Info {
            format: "Coff".into(),
            bits: if ["x86_64", "aarch64"].contains(&architecture.as_str()) { 64 } else { 32 },
            architectures: BTreeSet::from([architecture]),
            ..Info::default()
        });
    }
    let file = object::File::parse(bytes)?;
    let architecture = arch(file.architecture());
    let mut info = Info {
        format: format!("{:?}", file.format()),
        bits: if file.is_64()
            || file.format() == object::BinaryFormat::Coff
                && ["x86_64", "aarch64"].contains(&architecture.as_str())
        {
            64
        } else {
            32
        },
        ..Info::default()
    };
    info.architectures.insert(architecture);
    for section in file.sections() {
        let _ = section.data()?;
    }
    if let object::FileFlags::Elf { e_flags, .. } = file.flags() {
        info.flags = e_flags;
    }
    match file.format() {
        object::BinaryFormat::Elf => elf(bytes, &mut info, file.is_little_endian())?,
        object::BinaryFormat::Pe => pe(bytes, &mut info)?,
        object::BinaryFormat::MachO => macho(bytes, &mut info, file.is_little_endian())?,
        object::BinaryFormat::Coff => {},
        format => return Err(error(format!("unsupported inspected binary format: {format:?}"))),
    }
    Ok(info)
}

fn validate_machine(bytes: &[u8], target: &str) -> Result<()> {
    if target.contains("windows") {
        let expected = match target_arch(target) {
            "x86_64" => 0x8664,
            "aarch64" => 0xaa64,
            _ => return Err(error("unsupported native MSVC architecture")),
        };
        let offset = match object::FileKind::parse(bytes)? {
            object::FileKind::Pe64 => integer(bytes, 60, 4, true)? + 4,
            object::FileKind::Coff => 0,
            object::FileKind::CoffBig | object::FileKind::CoffImport => 6,
            _ => return Err(error("expected native PE/COFF input")),
        };
        if integer(bytes, offset, 2, true)? != expected {
            return Err(error("PE/COFF machine does not match native MSVC target"));
        }
    }
    Ok(())
}

pub fn inspect(path: &Path, target: Option<&str>) -> Result<Info> {
    let bytes = std::fs::read(path)?;
    let kind = object::FileKind::parse(bytes.as_slice())?;
    if kind == object::FileKind::Archive {
        let archive = object::read::archive::ArchiveFile::parse(bytes.as_slice())?;
        if archive.is_thin() {
            return Err(error("external thin-archive members are not accepted"));
        }
        let mut info = Info { format: "Archive".into(), ..Info::default() };
        for member in archive.members() {
            let member = member?;
            let data = member.data(bytes.as_slice())?;
            let item = thin(data)?;
            if let Some(target) = target {
                validate_machine(data, target)?;
                let expected = if target.contains("windows") {
                    ["Coff"].as_slice()
                } else if target.contains("darwin") {
                    ["MachO"].as_slice()
                } else {
                    ["Elf"].as_slice()
                };
                if !item.architectures.contains(target_arch(target))
                    || !expected.contains(&item.format.as_str())
                {
                    return Err(error(
                        "archive contains a wrong-architecture or wrong-format member",
                    ));
                }
            }
            info.architectures.extend(item.architectures);
            info.bits = info.bits.max(item.bits);
        }
        if info.architectures.is_empty() {
            return Err(error("archive has no object members"));
        }
        return Ok(info);
    }
    let info = match kind {
        object::FileKind::MachOFat32 | object::FileKind::MachOFat64 => {
            use object::read::macho::FatArch;
            let slices = if kind == object::FileKind::MachOFat32 {
                object::read::macho::MachOFatFile32::parse(bytes.as_slice())?
                    .arches()
                    .iter()
                    .map(|a| a.data(bytes.as_slice()).map(|v| v.to_vec()))
                    .collect::<std::result::Result<Vec<_>, _>>()?
            } else {
                object::read::macho::MachOFatFile64::parse(bytes.as_slice())?
                    .arches()
                    .iter()
                    .map(|a| a.data(bytes.as_slice()).map(|v| v.to_vec()))
                    .collect::<std::result::Result<Vec<_>, _>>()?
            };
            let mut info = Info { format: "MachO".into(), ..Info::default() };
            for slice in slices {
                let item = thin(&slice)?;
                if item.architectures.iter().any(|a| info.architectures.contains(a)) {
                    return Err(error("duplicate universal Mach-O architecture"));
                }
                let selected = target.is_none_or(|t| item.architectures.contains(target_arch(t)));
                info.architectures.extend(item.architectures);
                if selected {
                    info.bits = item.bits;
                    info.needed.extend(item.needed);
                    info.rpaths.extend(item.rpaths);
                }
            }
            info
        },
        _ => thin(&bytes)?,
    };
    if let Some(target) = target {
        validate_machine(&bytes, target)?;
        if !info.architectures.contains(target_arch(target)) {
            return Err(error(format!(
                "{}: wrong architecture for {target}: {:?}",
                path.display(),
                info.architectures
            )));
        }
        let expected = if target.contains("windows") {
            ["Pe", "Coff"].as_slice()
        } else if target.contains("darwin") {
            ["MachO"].as_slice()
        } else {
            ["Elf"].as_slice()
        };
        if !expected.contains(&info.format.as_str()) {
            return Err(error(format!(
                "{}: wrong binary format {} for {target}",
                path.display(),
                info.format
            )));
        }
        if ["x86_64", "aarch64", "riscv64", "loongarch64"].contains(&target_arch(target))
            && info.bits != 64
        {
            return Err(error("binary address width differs from 64-bit target"));
        }
    }
    Ok(info)
}

pub fn system_library(name: &str, target: &str) -> bool {
    if target.contains("windows") {
        let lower = name.to_ascii_lowercase();
        let stem = lower.strip_suffix(".dll").unwrap_or(&lower);
        lower.starts_with("api-ms-win-")||lower.starts_with("ext-ms-win-")||"advapi32 bcrypt bcryptprimitives cabinet cfgmgr32 comdlg32 crypt32 cryptbase dbghelp dbgcore dnsapi dwmapi gdi32 imagehlp imm32 iphlpapi kernel32 ncrypt netapi32 ntdll ole32 oleaut32 powrprof profapi psapi rpcrt4 secur32 setupapi shell32 shlwapi user32 userenv ucrtbase uuid version winhttp winmm winspool wintrust ws2_32 wtsapi32 normaliz msvcrt".split_whitespace().any(|n|n==stem)
    } else if target.contains("darwin") {
        name.starts_with("/usr/lib/") || name.starts_with("/System/")
    } else {
        matches!(
            name,
            "libc.so.6"
                | "libdl.so.2"
                | "libm.so.6"
                | "libpthread.so.0"
                | "librt.so.1"
                | "libresolv.so.2"
                | "libutil.so.1"
        ) || name.starts_with("ld-linux")
    }
}
