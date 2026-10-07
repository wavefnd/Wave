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

use std::env;
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const CRT_OBJECTS: &[(&str, &str)] = &[
    ("crt1.o", "crt1.s"),
    ("Scrt1.o", "crt1.s"),
    ("rcrt1.o", "crt1.s"),
    ("crti.o", "crti.s"),
    ("crtn.o", "crtn.s"),
];

struct CrtSpec {
    feature: &'static str,
    target: &'static str,
    source: &'static str,
    abi: Option<&'static str>,
    attributes: Option<&'static str>,
}

fn main() {
    println!("cargo:rerun-if-env-changed=WAVE_LLVM_MC");
    println!("cargo:rerun-if-env-changed=LLVM_CONFIG_PATH");
    println!("cargo:rerun-if-env-changed=LLVM_SYS_211_PREFIX");
    for architecture in ["x86_64", "aarch64", "riscv64", "loongarch64"] {
        for source in ["crt1.s", "crti.s", "crtn.s"] {
            println!("cargo:rerun-if-changed=crt/linux/{architecture}/{source}");
        }
    }

    let output_root =
        PathBuf::from(env::var_os("OUT_DIR").expect("OUT_DIR is set by Cargo")).join("crt");
    let llvm_mc = find_llvm_mc();
    let build_all = env::var_os("CARGO_FEATURE_LLVM_TARGET_ALL").is_some();

    let specs = [
        CrtSpec {
            feature: "CARGO_FEATURE_LLVM_TARGET_X86",
            target: "x86_64-unknown-linux-gnu",
            source: "crt/linux/x86_64/crt1.s",
            abi: None,
            attributes: None,
        },
        CrtSpec {
            feature: "CARGO_FEATURE_LLVM_TARGET_AARCH64",
            target: "aarch64-unknown-linux-gnu",
            source: "crt/linux/aarch64/crt1.s",
            abi: None,
            attributes: None,
        },
        CrtSpec {
            feature: "CARGO_FEATURE_LLVM_TARGET_RISCV",
            target: "riscv64-unknown-linux-gnu",
            source: "crt/linux/riscv64/crt1.s",
            abi: Some("lp64"),
            attributes: Some("+m,+a,+c,+zicsr,+zifencei"),
        },
        CrtSpec {
            feature: "CARGO_FEATURE_LLVM_TARGET_RISCV",
            target: "riscv64-unknown-linux-gnu",
            source: "crt/linux/riscv64/crt1.s",
            abi: Some("lp64f"),
            attributes: Some("+m,+a,+f,+c,+zicsr,+zifencei"),
        },
        CrtSpec {
            feature: "CARGO_FEATURE_LLVM_TARGET_RISCV",
            target: "riscv64-unknown-linux-gnu",
            source: "crt/linux/riscv64/crt1.s",
            abi: Some("lp64d"),
            attributes: Some("+m,+a,+f,+d,+c,+zicsr,+zifencei"),
        },
        CrtSpec {
            feature: "CARGO_FEATURE_LLVM_TARGET_LOONGARCH",
            target: "loongarch64-unknown-linux-gnu",
            source: "crt/linux/loongarch64/crt1.s",
            abi: Some("lp64s"),
            attributes: Some("-f,-d,+lsx,+ual"),
        },
        CrtSpec {
            feature: "CARGO_FEATURE_LLVM_TARGET_LOONGARCH",
            target: "loongarch64-unknown-linux-gnu",
            source: "crt/linux/loongarch64/crt1.s",
            abi: Some("lp64f"),
            attributes: Some("+f,-d,+lsx,+ual"),
        },
        CrtSpec {
            feature: "CARGO_FEATURE_LLVM_TARGET_LOONGARCH",
            target: "loongarch64-unknown-linux-gnu",
            source: "crt/linux/loongarch64/crt1.s",
            abi: Some("lp64d"),
            attributes: Some("+f,+d,+lsx,+ual"),
        },
    ];

    for spec in specs {
        if build_all || env::var_os(spec.feature).is_some() {
            build_crt(&llvm_mc, &output_root, &spec);
        }
    }

    println!("cargo:rustc-env=WAVE_BUILD_CRT_DIR={}", output_root.display());
}

fn build_crt(llvm_mc: &OsString, output_root: &Path, spec: &CrtSpec) {
    let source_dir =
        Path::new(spec.source).parent().expect("Linux CRT source has an architecture directory");
    let mut output_dir = output_root.join(spec.target);
    if let Some(abi) = spec.abi {
        output_dir.push(abi);
    }
    fs::create_dir_all(&output_dir).unwrap_or_else(|error| {
        panic!("failed to create Linux CRT output directory '{}': {}", output_dir.display(), error)
    });

    for (object_name, source_name) in CRT_OBJECTS {
        let source = source_dir.join(source_name);
        let output = output_dir.join(object_name);
        let mut command = Command::new(llvm_mc);
        command
            .arg(format!("-triple={}", llvm_triple_for_abi(spec.target, spec.abi)))
            .arg("-filetype=obj");
        if let Some(attributes) = spec.attributes {
            command.arg(format!("-mattr={attributes}"));
        }
        if spec.target != "loongarch64-unknown-linux-gnu" {
            if let Some(abi) = spec.abi {
                command.arg(format!("-target-abi={abi}"));
            }
        }
        let result = command.arg(&source).arg("-o").arg(&output).output();
        match result {
            Ok(result) if result.status.success() => {},
            Ok(result) => panic!(
                "failed to assemble Linux CRT '{}' for '{}': {}",
                source.display(),
                spec.target,
                String::from_utf8_lossy(&result.stderr).trim()
            ),
            Err(error) => panic!(
                "failed to execute llvm-mc while assembling Linux CRT for '{}': {}",
                spec.target, error
            ),
        }
    }
}

fn llvm_triple_for_abi(target: &str, abi: Option<&str>) -> String {
    if target == "loongarch64-unknown-linux-gnu" {
        return match abi.unwrap_or("lp64d") {
            "lp64s" => "loongarch64-unknown-linux-gnusf".to_string(),
            "lp64f" => "loongarch64-unknown-linux-gnuf32".to_string(),
            _ => target.to_string(),
        };
    }
    target.to_string()
}

fn find_llvm_mc() -> OsString {
    if let Some(path) = env::var_os("WAVE_LLVM_MC") {
        return path;
    }

    if let Some(prefix) = env::var_os("LLVM_SYS_211_PREFIX") {
        let candidate = llvm_tool_in(Path::new(&prefix).join("bin"), "llvm-mc");
        if candidate.is_file() {
            return candidate.into_os_string();
        }
    }

    if let Some(config) = env::var_os("LLVM_CONFIG_PATH") {
        let config = PathBuf::from(config);
        if let Some(bin_dir) = config.parent() {
            let candidate = llvm_tool_in(bin_dir, "llvm-mc");
            if candidate.is_file() {
                return candidate.into_os_string();
            }
        }
    }

    OsString::from(if cfg!(windows) { "llvm-mc.exe" } else { "llvm-mc" })
}

fn llvm_tool_in(directory: impl AsRef<Path>, name: &str) -> PathBuf {
    directory.as_ref().join(if cfg!(windows) { format!("{name}.exe") } else { name.to_string() })
}
