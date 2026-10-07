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

//! Discovery of CRT objects bundled with Wave distributions.
//!
//! Search order supports explicit overrides, installed compiler layouts, and the
//! repository build tree. This module only locates files; target-specific link
//! planning decides which CRT objects are required.

use std::env;
use std::path::{Path, PathBuf};

/// Finds the first existing Wave-bundled Linux CRT object for a target.
///
/// Candidate order is significant: explicit environment overrides take
/// precedence over paths relative to the running compiler and the build-time
/// fallback directory.
pub fn find_bundled_linux_crt(target: &str, abi: Option<&str>, name: &str) -> Option<PathBuf> {
    bundled_linux_crt_candidates(target, abi, name).into_iter().find(|path| path.is_file())
}

/// Returns the highest-priority path where a bundled CRT object is expected.
///
/// Unlike [`find_bundled_linux_crt`], this function does not require the file
/// to exist. Diagnostics use the result to report the path that was searched.
pub fn expected_bundled_linux_crt(target: &str, abi: Option<&str>, name: &str) -> PathBuf {
    bundled_linux_crt_candidates(target, abi, name)
        .into_iter()
        .next()
        .unwrap_or_else(|| PathBuf::from("crt").join(crt_relative_path(target, abi, name)))
}

fn bundled_linux_crt_candidates(target: &str, abi: Option<&str>, name: &str) -> Vec<PathBuf> {
    let mut paths = Vec::new();
    let relative = crt_relative_path(target, abi, name);

    // A file-specific override exists for compatibility with release and CI
    // environments that supply only the conventional process entry object.
    if name == "crt1.o" {
        if let Ok(path) = env::var("WAVE_LINUX_CRT1_OBJECT") {
            if !path.trim().is_empty() {
                paths.push(PathBuf::from(path));
            }
        }
    }

    if let Ok(path) = env::var("WAVE_LINUX_CRT_DIR") {
        if !path.trim().is_empty() {
            paths.push(PathBuf::from(path).join(&relative));
        }
    }

    // Installed archives place CRT files either beside wavec or below the
    // installation prefix. Keep these layouts ahead of the build-tree path.
    if let Ok(exe) = env::current_exe() {
        if let Some(dir) = exe.parent() {
            paths.push(dir.join("crt").join(&relative));
            if let Some(root) = dir.parent() {
                paths.push(root.join("lib").join("wave").join("crt").join(&relative));
            }
        }
    }

    paths.push(PathBuf::from(env!("WAVE_BUILD_CRT_DIR")).join(relative));
    paths
}

fn crt_relative_path(target: &str, abi: Option<&str>, name: &str) -> PathBuf {
    let mut path = PathBuf::from(target);
    if matches!(target, "riscv64-unknown-linux-gnu" | "loongarch64-unknown-linux-gnu") {
        path.push(abi.unwrap_or("lp64d"));
    }
    // Accept only the final component so a caller cannot escape the target CRT
    // directory by passing a path instead of an object-file name.
    path.push(Path::new(name).file_name().unwrap_or_default());
    path
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bundled_linux_crt_covers_every_hosted_linux_target() {
        #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-x86"))]
        for target in ["x86_64-unknown-linux-gnu"] {
            for name in ["crt1.o", "Scrt1.o", "rcrt1.o", "crti.o", "crtn.o"] {
                let path = find_bundled_linux_crt(target, None, name)
                    .unwrap_or_else(|| panic!("missing bundled {name} for {target}"));
                assert_elf_machine(&path, target);
            }
        }

        #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-aarch64"))]
        for target in ["aarch64-unknown-linux-gnu"] {
            for name in ["crt1.o", "Scrt1.o", "rcrt1.o", "crti.o", "crtn.o"] {
                let path = find_bundled_linux_crt(target, None, name)
                    .unwrap_or_else(|| panic!("missing bundled {name} for {target}"));
                assert_elf_machine(&path, target);
            }
        }

        #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
        for (abi, expected_float_flags) in [("lp64", 0), ("lp64f", 2), ("lp64d", 4)] {
            for name in ["crt1.o", "Scrt1.o", "rcrt1.o", "crti.o", "crtn.o"] {
                let path = find_bundled_linux_crt("riscv64-unknown-linux-gnu", Some(abi), name)
                    .unwrap_or_else(|| panic!("missing bundled RISC-V {abi} {name}"));
                let bytes = assert_elf_machine(&path, "riscv64-unknown-linux-gnu");
                let flags = u32::from_le_bytes(bytes[48..52].try_into().unwrap());
                assert_eq!(flags & 0x6, expected_float_flags, "{}", path.display());
            }
        }

        #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
        for (abi, expected_modifier) in [("lp64s", 0x1), ("lp64f", 0x2), ("lp64d", 0x3)] {
            for name in ["crt1.o", "Scrt1.o", "rcrt1.o", "crti.o", "crtn.o"] {
                let path = find_bundled_linux_crt("loongarch64-unknown-linux-gnu", Some(abi), name)
                    .unwrap_or_else(|| {
                        panic!("missing bundled LoongArch64 {} {name}", abi.to_uppercase())
                    });
                let bytes = assert_elf_machine(&path, "loongarch64-unknown-linux-gnu");
                let flags = u32::from_le_bytes(bytes[48..52].try_into().unwrap());
                assert_eq!(flags & 0x7, expected_modifier, "{}", path.display());
            }
        }
    }

    fn assert_elf_machine(path: &Path, target: &str) -> Vec<u8> {
        let bytes = std::fs::read(path).unwrap();
        assert!(bytes.len() >= 52, "{} is too short", path.display());
        assert_eq!(&bytes[..4], b"\x7fELF", "{}", path.display());
        let machine = u16::from_le_bytes([bytes[18], bytes[19]]);
        let expected = match target {
            "x86_64-unknown-linux-gnu" => 62,
            "aarch64-unknown-linux-gnu" => 183,
            "riscv64-unknown-linux-gnu" => 243,
            "loongarch64-unknown-linux-gnu" => 258,
            _ => unreachable!(),
        };
        assert_eq!(machine, expected, "{}", path.display());
        bytes
    }
}
