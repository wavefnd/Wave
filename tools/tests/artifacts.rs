// SPDX-License-Identifier: MPL-2.0
use std::fs;
use std::path::Path;
use wave_tools::{binary, windows};

fn pe(path: &Path, machine: u16, imports: &[&str]) {
    fn u16_at(bytes: &mut [u8], offset: usize, value: u16) {
        bytes[offset..offset + 2].copy_from_slice(&value.to_le_bytes());
    }

    fn u32_at(bytes: &mut [u8], offset: usize, value: u32) {
        bytes[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
    }
    let mut data = vec![0; 4096];
    data[..2].copy_from_slice(b"MZ");
    u32_at(&mut data, 60, 64);
    data[64..68].copy_from_slice(b"PE\0\0");
    u16_at(&mut data, 68, machine);
    u16_at(&mut data, 70, 1);
    u16_at(&mut data, 84, 240);
    u16_at(&mut data, 86, 0x2022);
    let optional = 88;
    u16_at(&mut data, optional, 0x20b);
    u32_at(&mut data, optional + 32, 0x1000);
    u32_at(&mut data, optional + 36, 0x200);
    u32_at(&mut data, optional + 56, 0x2000);
    u32_at(&mut data, optional + 60, 512);
    u16_at(&mut data, optional + 68, 3);
    u32_at(&mut data, optional + 108, 16);
    let section = 328;
    data[section..section + 6].copy_from_slice(b".idata");
    u32_at(&mut data, section + 8, 3584);
    u32_at(&mut data, section + 12, 0x1000);
    u32_at(&mut data, section + 16, 3584);
    u32_at(&mut data, section + 20, 512);
    u32_at(&mut data, section + 36, 0x40000040);
    if !imports.is_empty() {
        u32_at(&mut data, optional + 120, 0x1000);
        u32_at(&mut data, optional + 124, ((imports.len() + 1) * 20) as u32);
    }
    let mut strings = 512 + (imports.len() + 1) * 20;
    for (index, name) in imports.iter().enumerate() {
        u32_at(&mut data, 512 + index * 20 + 12, (0x1000 + strings - 512) as u32);
        data[strings..strings + name.len()].copy_from_slice(name.as_bytes());
        strings += name.len() + 1;
    }
    fs::write(path, data).unwrap();
}

#[test]
fn real_pe_bytes_drive_transitive_cycles_case_insensitive_resolution_and_machine_checks() {
    let directory = tempfile::tempdir().unwrap();
    let root = directory.path();
    let exe = root.join("wavec.exe");
    let llvm = root.join("LLVM.dll");
    let zstd = root.join("zstd.dll");
    pe(&exe, 0x8664, &["LLVM.dll", "KERNEL32.dll", "api-ms-win-core-file-l1-1-0.dll"]);
    pe(&llvm, 0x8664, &["ZSTD.dll"]);
    pe(&zstd, 0x8664, &["LLVM.dll"]);
    let info = binary::inspect(&exe, Some("x86_64-pc-windows-msvc")).unwrap();
    assert!(info.needed.contains("LLVM.dll"));
    let (payload, required) = windows::dependency_closure(
        std::slice::from_ref(&exe),
        "x86_64-pc-windows-msvc",
        &[root.into()],
        &[],
    )
    .unwrap();
    assert_eq!(payload.len(), 2);
    assert!(required.is_empty());
    pe(&zstd, 0xaa64, &[]);
    assert!(windows::dependency_closure(
        std::slice::from_ref(&exe),
        "x86_64-pc-windows-msvc",
        &[root.into()],
        &[]
    )
    .is_err());
    pe(&zstd, 0x8664, &["missing.dll"]);
    assert!(windows::dependency_closure(
        std::slice::from_ref(&exe),
        "x86_64-pc-windows-msvc",
        &[root.into()],
        &[]
    )
    .unwrap_err()
    .to_string()
    .contains("missing.dll"));
    pe(&zstd, 0x8664, &["libwinpthread-1.dll"]);
    assert!(windows::dependency_closure(&[exe], "x86_64-pc-windows-msvc", &[root.into()], &[])
        .unwrap_err()
        .to_string()
        .contains("MinGW"));
}

#[test]
fn vc_runtime_is_an_installed_prerequisite_and_its_transitive_dependencies_are_verified() {
    let directory = tempfile::tempdir().unwrap();
    let root = directory.path();
    let runtime = root.join("System32");
    fs::create_dir(&runtime).unwrap();
    let exe = root.join("wavec.exe");
    pe(&exe, 0x8664, &["VCRUNTIME140.dll"]);
    pe(&root.join("vcruntime140.dll"), 0x8664, &[]);
    assert!(windows::dependency_closure(
        std::slice::from_ref(&exe),
        "x86_64-pc-windows-msvc",
        &[root.into()],
        std::slice::from_ref(&runtime)
    )
    .is_err());
    pe(&runtime.join("vcruntime140.dll"), 0x8664, &["MSVCP140.dll"]);
    pe(&runtime.join("msvcp140.dll"), 0x8664, &[]);
    let (payload, prerequisites) = windows::dependency_closure(
        std::slice::from_ref(&exe),
        "x86_64-pc-windows-msvc",
        &[root.into()],
        std::slice::from_ref(&runtime),
    )
    .unwrap();
    assert!(payload.is_empty());
    assert_eq!(prerequisites.len(), 2);
    fs::remove_file(runtime.join("msvcp140.dll")).unwrap();
    assert!(windows::dependency_closure(
        &[exe],
        "x86_64-pc-windows-msvc",
        &[root.into()],
        &[runtime]
    )
    .is_err());
}

#[test]
fn malformed_pe_offsets_sections_and_import_strings_are_rejected_without_executing_tools() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("wavec.exe");
    pe(&path, 0x8664, &["LLVM.dll"]);
    let good = fs::read(&path).unwrap();
    for length in [0, 2, 160, 400, 1000] {
        fs::write(&path, &good[..length]).unwrap();
        assert!(binary::inspect(&path, None).is_err());
    }
    let mut bad = good.clone();
    bad[60..64].copy_from_slice(&u32::MAX.to_le_bytes());
    fs::write(&path, bad).unwrap();
    assert!(binary::inspect(&path, None).is_err());
    pe(&path, 0x8664, &["../outside.dll"]);
    assert!(binary::inspect(&path, None).is_err());
}

#[test]
fn coff_address_width_and_every_archive_member_follow_the_native_machine() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("object.obj");
    let object = |machine: u16| {
        let mut bytes = vec![0; 20];
        bytes[..2].copy_from_slice(&machine.to_le_bytes());
        bytes
    };
    fs::write(&path, object(0x8664)).unwrap();
    assert_eq!(binary::inspect(&path, Some("x86_64-pc-windows-msvc")).unwrap().bits, 64);
    let archive = |members: Vec<Vec<u8>>| {
        let mut bytes = b"!<arch>\n".to_vec();
        for (index, member) in members.into_iter().enumerate() {
            bytes.extend(
                format!(
                    "{:<16}{:<12}{:<6}{:<6}{:<8}{:<10}`\n",
                    format!("{index}.obj/"),
                    0,
                    0,
                    0,
                    "100644",
                    member.len()
                )
                .as_bytes(),
            );
            bytes.extend(&member);
            if member.len() % 2 != 0 {
                bytes.push(b'\n');
            }
        }
        bytes
    };
    let library = directory.path().join("builtins.lib");
    fs::write(&library, archive(vec![object(0x8664), object(0x8664)])).unwrap();
    assert_eq!(binary::inspect(&library, Some("x86_64-pc-windows-msvc")).unwrap().bits, 64);
    fs::write(&library, archive(vec![object(0x8664), object(0xaa64)])).unwrap();
    assert!(binary::inspect(&library, Some("x86_64-pc-windows-msvc")).is_err());
    fs::write(&library, b"!<thin>\n").unwrap();
    assert!(binary::inspect(&library, Some("x86_64-pc-windows-msvc")).is_err());
}
