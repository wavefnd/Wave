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

//! Minimal read-only ELF and Unix archive metadata inspection.
//!
//! Pre-link validation needs only machine and `e_flags`, so this module avoids a
//! full object-file parser while supporting direct ELF objects plus GNU and BSD
//! archive member naming. It never rewrites linker inputs.

use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

const ELF_MAGIC: &[u8; 4] = b"\x7fELF";
const THIN_MAGIC: &[u8; 8] = b"!<thin>\n";
const AR_MAGIC: &[u8; 8] = b"!<arch>\n";

#[derive(Debug)]
pub(super) struct ElfMetadata {
    pub input: String,
    pub machine: u16,
    pub flags: u32,
}

#[derive(Debug)]
pub enum LinkInputInspectionError {
    Read { input: PathBuf, source: std::io::Error },
    Malformed { input: String, reason: String },
}

impl fmt::Display for LinkInputInspectionError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Read { input, source } => write!(
                formatter,
                "failed to inspect linker input '{}': {}",
                input.display(),
                source
            ),
            Self::Malformed { input, reason } => {
                write!(formatter, "invalid ELF linker input '{}': {}", input, reason)
            },
        }
    }
}

impl std::error::Error for LinkInputInspectionError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Read { source, .. } => Some(source),
            Self::Malformed { .. } => None,
        }
    }
}

pub(super) fn inspect_link_inputs(
    inputs: &[String],
) -> Result<Vec<ElfMetadata>, LinkInputInspectionError> {
    let mut metadata = Vec::new();
    for input in inputs {
        let path = Path::new(input);
        let bytes = fs::read(path).map_err(|source| LinkInputInspectionError::Read {
            input: path.to_path_buf(),
            source,
        })?;
        inspect_input(path, &bytes, &mut metadata, 0)?;
    }
    Ok(metadata)
}

fn inspect_input(
    path: &Path,
    bytes: &[u8],
    metadata: &mut Vec<ElfMetadata>,
    depth: usize,
) -> Result<(), LinkInputInspectionError> {
    if depth > 64 {
        return Err(malformed(
            &path.display().to_string(),
            "archive nesting limit exceeded (possible cycle)",
        ));
    }
    if bytes.starts_with(ELF_MAGIC) {
        metadata.push(read_elf(&path.display().to_string(), bytes)?);
    } else if bytes.starts_with(AR_MAGIC) || bytes.starts_with(THIN_MAGIC) {
        inspect_archive(path, bytes, metadata, depth, None)?;
    }
    // LLVM bitcode and linker scripts do not carry ELF e_flags. They remain
    // valid linker inputs and are intentionally ignored by metadata validation.
    Ok(())
}

fn read_elf(display: &str, bytes: &[u8]) -> Result<ElfMetadata, LinkInputInspectionError> {
    if bytes.len() < 20 || &bytes[..4] != ELF_MAGIC {
        return Err(malformed(display, "truncated ELF header"));
    }
    let little_endian = match bytes[5] {
        1 => true,
        2 => false,
        _ => return Err(malformed(display, "invalid ELF data encoding")),
    };
    let machine = read_u16(&bytes[18..20], little_endian);
    let flags_offset = match bytes[4] {
        1 => 36,
        2 => 48,
        _ => return Err(malformed(display, "invalid ELF class")),
    };
    let Some(raw_flags) = bytes.get(flags_offset..flags_offset + 4) else {
        return Err(malformed(display, "truncated ELF e_flags"));
    };
    Ok(ElfMetadata {
        input: display.to_string(),
        machine,
        flags: read_u32(raw_flags, little_endian),
    })
}

fn inspect_archive(
    path: &Path,
    bytes: &[u8],
    metadata: &mut Vec<ElfMetadata>,
    depth: usize,
    selected: Option<usize>,
) -> Result<(), LinkInputInspectionError> {
    let display = path.display().to_string();
    let thin = bytes.starts_with(THIN_MAGIC);
    let mut offset = AR_MAGIC.len();
    let mut long_names: Option<&[u8]> = None;
    while offset < bytes.len() {
        let header_end = offset
            .checked_add(60)
            .ok_or_else(|| malformed(&display, "archive header offset overflow"))?;
        let header = bytes
            .get(offset..header_end)
            .ok_or_else(|| malformed(&display, "truncated archive header"))?;
        if &header[58..60] != b"`\n" {
            return Err(malformed(&display, "invalid archive member header"));
        }
        let size = archive_number(&display, &header[48..58], "invalid archive member size")?;
        let raw_name = std::str::from_utf8(&header[..16])
            .map_err(|_| malformed(&display, "invalid archive member name"))?
            .trim();
        let special = matches!(raw_name, "//" | "/" | "/SYM64/");
        // Thin members have a size describing the external object, but no
        // embedded bytes or padding. Only symbol/name tables remain embedded.
        let stored_size = if thin && !special { 0 } else { size };
        let data_end = header_end
            .checked_add(stored_size)
            .ok_or_else(|| malformed(&display, "archive member size overflow"))?;
        let member_data = bytes
            .get(header_end..data_end)
            .ok_or_else(|| malformed(&display, "truncated archive member"))?;
        if raw_name == "//" {
            long_names = Some(member_data);
        } else if !special && selected.is_none_or(|wanted| wanted == offset) {
            let (name, payload, origin) =
                archive_member(&display, raw_name, member_data, long_names, thin)?;
            let member_display = format!("{display}({name})");
            if thin {
                let member_path = path.parent().unwrap_or_else(|| Path::new(".")).join(&name);
                let external = fs::read(&member_path).map_err(|source| {
                    malformed(
                        &member_display,
                        &format!(
                            "failed to read thin archive member '{}': {source}",
                            member_path.display()
                        ),
                    )
                })?;
                if let Some(origin) = origin {
                    if !external.starts_with(AR_MAGIC) {
                        return Err(malformed(
                            &member_display,
                            "nested thin member does not reference a regular archive",
                        ));
                    }
                    // GNU ar may refer to one member of an existing regular
                    // archive as /name_offset:header_offset. Inspect only it.
                    inspect_archive(&member_path, &external, metadata, depth + 1, Some(origin))?;
                } else {
                    let before = metadata.len();
                    inspect_input(&member_path, &external, metadata, depth + 1)?;
                    for item in &mut metadata[before..] {
                        item.input = format!("{member_display}: {}", item.input);
                    }
                }
            } else if payload.starts_with(ELF_MAGIC) {
                metadata.push(read_elf(&member_display, payload)?);
            }
            if selected.is_some() {
                return Ok(());
            }
        }
        offset = data_end
            .checked_add(stored_size & 1)
            .ok_or_else(|| malformed(&display, "archive padding offset overflow"))?;
        if offset > bytes.len() {
            return Err(malformed(&display, "truncated archive padding"));
        }
    }
    if selected.is_some() {
        return Err(malformed(&display, "nested archive member offset is not a member header"));
    }
    Ok(())
}

fn archive_number(
    display: &str,
    text: &[u8],
    reason: &str,
) -> Result<usize, LinkInputInspectionError> {
    let text = std::str::from_utf8(text).map_err(|_| malformed(display, reason))?.trim();
    if text.is_empty() || !text.bytes().all(|c| c.is_ascii_digit()) {
        return Err(malformed(display, reason));
    }
    text.parse().map_err(|_| malformed(display, reason))
}

fn archive_member<'a>(
    display: &str,
    raw_name: &str,
    data: &'a [u8],
    long_names: Option<&[u8]>,
    thin: bool,
) -> Result<(String, &'a [u8], Option<usize>), LinkInputInspectionError> {
    if let Some(length) = raw_name.strip_prefix("#1/") {
        if thin {
            return Err(malformed(display, "BSD extended names are invalid in GNU thin archives"));
        }
        let length = archive_number(display, length.as_bytes(), "invalid BSD archive member name")?;
        let name = data
            .get(..length)
            .ok_or_else(|| malformed(display, "truncated BSD archive member name"))?;
        return Ok((
            String::from_utf8_lossy(name).trim_end_matches('\0').to_string(),
            &data[length..],
            None,
        ));
    }
    if let Some(reference) = raw_name.strip_prefix('/') {
        let (name_offset, origin) = match reference.split_once(':') {
            Some((name, origin)) if thin => (
                name,
                Some(archive_number(
                    display,
                    origin.as_bytes(),
                    "invalid nested archive member offset",
                )?),
            ),
            _ => (reference, None),
        };
        let offset =
            archive_number(display, name_offset.as_bytes(), "invalid GNU archive name offset")?;
        let table =
            long_names.ok_or_else(|| malformed(display, "archive long-name table is missing"))?;
        let tail = table
            .get(offset..)
            .ok_or_else(|| malformed(display, "archive long-name offset is out of range"))?;
        let end = tail
            .windows(2)
            .position(|w| w == b"/\n")
            .ok_or_else(|| malformed(display, "unterminated archive long name"))?;
        let name = String::from_utf8_lossy(&tail[..end]).into_owned();
        if name.is_empty() {
            return Err(malformed(display, "empty archive member name"));
        }
        return Ok((name, data, origin));
    }
    let name = raw_name.trim_end_matches('/');
    if name.is_empty() {
        return Err(malformed(display, "empty archive member name"));
    }
    Ok((name.to_string(), data, None))
}

fn malformed(input: &str, reason: &str) -> LinkInputInspectionError {
    LinkInputInspectionError::Malformed { input: input.to_string(), reason: reason.to_string() }
}

fn read_u16(bytes: &[u8], little_endian: bool) -> u16 {
    let bytes = [bytes[0], bytes[1]];
    if little_endian {
        u16::from_le_bytes(bytes)
    } else {
        u16::from_be_bytes(bytes)
    }
}

fn read_u32(bytes: &[u8], little_endian: bool) -> u32 {
    let bytes = [bytes[0], bytes[1], bytes[2], bytes[3]];
    if little_endian {
        u32::from_le_bytes(bytes)
    } else {
        u32::from_be_bytes(bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn archive_header(name: &str, size: usize) -> Vec<u8> {
        let header = format!("{name:<16}{:<12}{:<6}{:<6}{:<8}{size:<10}`\n", 0, 0, 0, 0);
        assert_eq!(header.len(), 60);
        header.into_bytes()
    }

    #[test]
    fn reads_nul_padded_bsd_extended_member_names() {
        let member_name = b"main.o\0\0\0\0\0\0";
        let mut elf = vec![0u8; 52];
        elf[..4].copy_from_slice(ELF_MAGIC);
        elf[4] = 1;
        elf[5] = 1;
        elf[18..20].copy_from_slice(&243u16.to_le_bytes());
        elf[36..40].copy_from_slice(&2u32.to_le_bytes());

        let size = member_name.len() + elf.len();
        let mut archive = AR_MAGIC.to_vec();
        archive.extend(archive_header("#1/12", size));
        archive.extend(member_name);
        archive.extend(elf);

        let mut metadata = Vec::new();
        inspect_archive(Path::new("libmixed.a"), &archive, &mut metadata, 0, None).unwrap();
        assert_eq!(metadata.len(), 1);
        assert_eq!(metadata[0].input, "libmixed.a(main.o)");
        assert_eq!(metadata[0].machine, 243);
        assert_eq!(metadata[0].flags, 2);
    }

    struct Fixture(PathBuf);
    impl Fixture {
        fn new() -> Self {
            use std::sync::atomic::{AtomicU64, Ordering};
            static NEXT: AtomicU64 = AtomicU64::new(0);
            let path = std::env::temp_dir().join(format!(
                "wave-thin-{}-{}",
                std::process::id(),
                NEXT.fetch_add(1, Ordering::Relaxed)
            ));
            fs::create_dir_all(path.join("objects")).unwrap();
            Self(path)
        }

        fn inspect(&self, bytes: &[u8]) -> Result<Vec<ElfMetadata>, LinkInputInspectionError> {
            let path = self.0.join("lib.a");
            fs::write(&path, bytes).unwrap();
            inspect_link_inputs(&[path.display().to_string()])
        }
    }
    impl Drop for Fixture {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    fn elf(flags: u32) -> Vec<u8> {
        let mut bytes = vec![0; 64];
        bytes[..4].copy_from_slice(ELF_MAGIC);
        bytes[4] = 2;
        bytes[5] = 1;
        bytes[18..20].copy_from_slice(&243u16.to_le_bytes());
        bytes[48..52].copy_from_slice(&flags.to_le_bytes());
        bytes
    }

    fn thin(names: &[u8], members: &[(&str, usize)]) -> Vec<u8> {
        let mut bytes = THIN_MAGIC.to_vec();
        bytes.extend(archive_header("/", 4));
        bytes.extend([0; 4]);
        bytes.extend(archive_header("//", names.len()));
        bytes.extend(names);
        if names.len() % 2 != 0 {
            bytes.push(b'\n');
        }
        for (name, size) in members {
            bytes.extend(archive_header(name, *size));
        }
        bytes
    }

    #[test]
    fn thin_paths_are_relative_to_the_archive_and_validate_each_member_abi() {
        let fixture = Fixture::new();
        fs::write(fixture.0.join("objects/long_member_name.o"), elf(4)).unwrap();
        fs::write(fixture.0.join("other.o"), elf(2)).unwrap();
        let names = b"objects/long_member_name.o/\nother.o/\n";
        let offset = names.windows(8).position(|w| w == b"other.o/").unwrap();
        let bytes = thin(names, &[("/0", 64), (&format!("/{offset}"), 64)]);
        let metadata = fixture.inspect(&bytes).unwrap();
        assert_eq!(metadata.iter().map(|m| m.flags).collect::<Vec<_>>(), [4, 2]);
        let error = super::super::riscv::validate_riscv_link_inputs(
            super::super::riscv::RiscvFloatAbi::Lp64d,
            &[fixture.0.join("lib.a").display().to_string()],
        )
        .unwrap_err()
        .to_string();
        assert!(error.contains("other.o") && error.contains("LP64F"), "{error}");
    }

    #[test]
    fn gnu_name_offsets_may_reference_a_shared_suffix() {
        let fixture = Fixture::new();
        fs::write(fixture.0.join("member.o"), elf(4)).unwrap();
        // GNU offsets identify bytes in the string table, not entry indices.
        let bytes = thin(b"prefix_member.o/\n", &[("/7", 64)]);
        let metadata = fixture.inspect(&bytes).unwrap();
        assert_eq!(metadata.len(), 1);
        assert_eq!(metadata[0].flags, 4);
    }

    #[test]
    fn thin_nested_regular_archive_selects_only_the_referenced_member() {
        let fixture = Fixture::new();
        let mut nested = AR_MAGIC.to_vec();
        nested.extend(archive_header("bad.o/", 64));
        nested.extend(elf(2));
        let origin = nested.len();
        nested.extend(archive_header("good.o/", 64));
        nested.extend(elf(4));
        fs::write(fixture.0.join("nested.a"), nested).unwrap();
        let bytes = thin(b"nested.a/\n", &[(&format!("/0:{origin}"), 64)]);
        let metadata = fixture.inspect(&bytes).unwrap();
        assert_eq!(metadata.len(), 1);
        assert_eq!(metadata[0].flags, 4);
        let bad = thin(b"nested.a/\n", &[("/0:9", 64)]);
        assert!(fixture.inspect(&bad).unwrap_err().to_string().contains("not a member header"));
    }

    #[test]
    fn thin_missing_members_and_malformed_headers_are_diagnostics() {
        let fixture = Fixture::new();
        let valid = thin(b"missing.o/\n", &[("/0", 64)]);
        assert!(fixture.inspect(&valid).unwrap_err().to_string().contains("missing.o"));
        for bytes in [
            thin(b"missing.o/\n", &[("/999", 64)]),
            thin(b"missing.o/\n", &[("/1", 64)]),
            thin(b"missing.o", &[("/0", 64)]),
            thin(b"missing.o/\n", &[("/0:no", 64)]),
            [THIN_MAGIC.as_slice(), b"truncated"].concat(),
            [AR_MAGIC.as_slice(), &archive_header("//", 9_999_999_999)].concat(),
        ] {
            assert!(fixture.inspect(&bytes).is_err());
        }
        let cycle = thin(b"lib.a/\n", &[("/0", 64)]);
        assert!(fixture.inspect(&cycle).unwrap_err().to_string().contains("nesting limit"));
    }
}
