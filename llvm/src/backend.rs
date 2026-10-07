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

//! External LLVM tool and native-linker orchestration.
//!
//! In-process IR generation lives in `codegen`; this module translates resolved
//! backend options into `llc` and linker invocations, locates bundled tools, and
//! supplies platform startup/default-library arguments.

use crate::codegen::target::{llvm_triple_for_abi, target_spec_for_triple, CodegenTarget};
use crate::diagnostic::{CodegenError, CodegenPhase, PendingOutput};
use std::env;
use std::path::PathBuf;
use std::process::Command;

#[derive(Debug, Default, Clone)]
pub struct BackendOptions {
    pub target: Option<String>,
    pub cpu: Option<String>,
    pub features: Option<String>,
    pub abi: Option<String>,
    pub isa: Option<String>,
    pub code_model: Option<String>,
    pub relocation_model: Option<String>,
    pub sysroot: Option<String>,
    pub linker: Option<String>,
    pub link_args: Vec<String>,
    pub no_default_libs: bool,
    pub freestanding: bool,
}

pub fn is_windows_msvc_target(target: &str) -> bool {
    target_spec_for_triple(target).is_some_and(|spec| spec.os == "windows" && spec.env == "msvc")
}

/// Native COFF flags for an explicitly selected MSVC output target. SDK and
/// VC library discovery follows link.exe/lld-link's LIB environment or -L.
pub fn msvc_link_args(
    target: &str,
    objects: &[String],
    output: &str,
    libs: &[String],
    paths: &[String],
    no_default_libs: bool,
    static_crt: bool,
    shared: bool,
    entry: Option<&str>,
    extra: &[String],
) -> Vec<String> {
    let machine = if target.starts_with("aarch64-") { "ARM64" } else { "X64" };
    let mut args = vec!["/NOLOGO".into(), format!("/MACHINE:{machine}"), format!("/OUT:{output}")];
    if shared {
        args.push("/DLL".into());
    } else {
        args.push("/SUBSYSTEM:CONSOLE".into());
    }
    if let Some(entry) = entry {
        args.push(format!("/ENTRY:{entry}"));
    }
    args.extend(objects.iter().cloned());
    args.extend(paths.iter().map(|path| format!("/LIBPATH:{path}")));
    args.extend(libs.iter().map(|lib| {
        if lib.to_ascii_lowercase().ends_with(".lib") {
            lib.clone()
        } else {
            format!("{lib}.lib")
        }
    }));
    if no_default_libs {
        args.push("/NODEFAULTLIB".into());
    } else {
        let crt = if static_crt {
            ["libcmt", "libvcruntime", "libucrt"]
        } else {
            ["msvcrt", "vcruntime", "ucrt"]
        };
        args.extend(
            crt.into_iter()
                .chain([
                    "legacy_stdio_definitions",
                    "kernel32",
                    "user32",
                    "advapi32",
                    "shell32",
                    "ws2_32",
                    "bcrypt",
                ])
                .map(|lib| format!("/DEFAULTLIB:{lib}.lib")),
        );
    }
    args.extend(extra.iter().cloned());
    args
}

fn is_wasm_target(target: Option<&str>) -> bool {
    target.and_then(target_spec_for_triple).is_some_and(|spec| spec.object_format == "wasm")
}

fn normalize_llvm_opt_flag(opt_flag: &str) -> &str {
    match opt_flag {
        // LLVM pass pipeline currently has no dedicated Ofast preset, so keep
        // frontend/back-end optimization behavior aligned at O3.
        "-Ofast" => "-O3",
        other => other,
    }
}

pub fn compile_ir_to_object(
    ir: &str,
    file_stem: &str,
    opt_flag: &str,
    backend: &BackendOptions,
) -> Result<String, CodegenError> {
    let object_path = format!("{}.o", file_stem);
    let pending = PendingOutput::new(std::path::Path::new(&object_path))?;

    let normalized_opt = normalize_llvm_opt_flag(opt_flag);
    let llc = resolve_bundled_tool("llc");
    let mut cmd = Command::new(&llc);
    configure_bundled_llvm_tool_env(&mut cmd, &llc);

    if let Some(target) = &backend.target {
        cmd.arg(format!("--mtriple={}", llvm_triple_for_abi(target, backend.abi.as_deref())));
    }
    if let Some(cpu) = &backend.cpu {
        cmd.arg(format!("--mcpu={}", cpu));
    }
    if let Some(features) = &backend.features {
        cmd.arg(format!("--mattr={}", features));
    }
    if let Some(model) = &backend.code_model {
        cmd.arg(format!("--code-model={}", model));
    }
    if let Some(model) = &backend.relocation_model {
        cmd.arg(format!("--relocation-model={}", model));
    }
    if backend
        .target
        .as_deref()
        .and_then(target_spec_for_triple)
        .is_none_or(|spec| spec.codegen != CodegenTarget::LinuxLoongArch64)
    {
        if let Some(abi) = &backend.abi {
            cmd.arg(format!("--target-abi={}", abi));
        }
    }

    if !normalized_opt.is_empty() {
        cmd.arg(normalized_opt);
    }

    let mut child = cmd
        .arg("--filetype=obj")
        .arg("-")
        .arg("-o")
        .arg(pending.path())
        .stdin(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .map_err(|e| CodegenError::tool_launch(CodegenPhase::Tool, "llc", e))?;

    use std::io::Write;
    let mut stdin = child.stdin.take().expect("piped child stdin");
    let (written, output) = std::thread::scope(|scope| {
        let writer = scope.spawn(move || stdin.write_all(ir.as_bytes()));
        let output = child.wait_with_output();
        (writer.join(), output)
    });
    let output = output.map_err(|e| CodegenError::new(CodegenPhase::Tool, "wait for llc", e))?;
    if !output.status.success() {
        return Err(CodegenError::new(
            CodegenPhase::Tool,
            "llc",
            String::from_utf8_lossy(&output.stderr),
        ));
    }
    written
        .map_err(|_| {
            CodegenError::new(CodegenPhase::Tool, "write llc input", "input writer failed")
        })?
        .map_err(|e| CodegenError::new(CodegenPhase::Tool, "write llc input", e))?;
    pending.commit()?;
    Ok(object_path)
}

pub fn link_objects(
    objects: &[String],
    output: &str,
    libs: &[String],
    lib_paths: &[String],
    backend: &BackendOptions,
) -> Result<(), CodegenError> {
    let pending = PendingOutput::new(std::path::Path::new(output))?;
    let target = backend.target.as_deref().unwrap_or("");
    let linker_bin = backend.linker.clone().unwrap_or_else(|| default_lld_for_target(target));
    let mut cmd = Command::new(&linker_bin);
    configure_bundled_llvm_tool_env(&mut cmd, &linker_bin);

    if is_windows_msvc_target(target) {
        let mut args = msvc_link_args(
            target,
            objects,
            &pending.path().to_string_lossy(),
            libs,
            &crate::msvc::sdk::discovered_arguments(target, lib_paths),
            backend.no_default_libs,
            false,
            false,
            None,
            &backend.link_args,
        );
        crate::msvc::runtime::add_builtins(target, &mut args);
        crate::msvc::sdk::validate_link_inputs(target, objects, &args)
            .map_err(|e| CodegenError::new(CodegenPhase::Link, "validate MSVC inputs", e))?;
        let transport = crate::msvc::response::LinkArguments::prepare(
            &linker_bin,
            &args,
            std::path::Path::new(output),
        )?;
        cmd.args(&transport.arguments);
        let result = cmd
            .output()
            .map_err(|e| CodegenError::tool_launch(CodegenPhase::Link, "lld-link", e))?;
        if !result.status.success() {
            return Err(CodegenError::new(
                CodegenPhase::Link,
                "MSVC link (use matching SDK/UCRT/VC libraries via LIB or -L; arithmetic builtins require the matching LLVM 21 SDK via WAVE_LLVM_HOME)",
                String::from_utf8_lossy(&result.stderr),
            ));
        }
        return pending.commit();
    }
    if is_wasm_target(Some(target)) {
        cmd.arg("--no-entry")
            .arg("--allow-undefined")
            .arg("--export-if-defined=main")
            .arg("--export-memory");
    } else if backend.linker.is_none() {
        append_lld_target_args(&mut cmd, target, backend);
    }

    for obj in objects {
        cmd.arg(obj);
    }

    for path in lib_paths {
        cmd.arg(format!("-L{}", path));
    }

    for lib in libs {
        cmd.arg(format!("-l{}", lib));
    }

    for arg in expand_lld_link_args(&backend.link_args) {
        cmd.arg(arg);
    }

    cmd.arg("-o").arg(pending.path());

    if !backend.no_default_libs && !is_wasm_target(Some(target)) {
        if is_darwin_target(target) {
            cmd.arg("-lSystem");
        } else {
            cmd.arg("-lc").arg("-lm");
        }
    }

    let output =
        cmd.output().map_err(|e| CodegenError::tool_launch(CodegenPhase::Link, &linker_bin, e))?;
    if !output.status.success() {
        return Err(CodegenError::new(
            CodegenPhase::Link,
            linker_bin,
            String::from_utf8_lossy(&output.stderr),
        ));
    }
    pending.commit()
}

fn default_lld_for_target(target: &str) -> String {
    if is_windows_msvc_target(target) {
        return resolve_bundled_tool("lld-link");
    }
    if is_wasm_target(Some(target)) {
        resolve_bundled_tool("wasm-ld")
    } else if is_darwin_target(target) {
        resolve_bundled_tool("ld64.lld")
    } else {
        resolve_bundled_tool("ld.lld")
    }
}

fn append_lld_target_args(cmd: &mut Command, target: &str, backend: &BackendOptions) {
    if is_darwin_target(target) {
        let spec = target_spec_for_triple(target)
            .expect("Darwin linker configuration requires a registered target");
        cmd.arg("-arch")
            .arg(if spec.architecture.name() == "aarch64" {
                "arm64"
            } else {
                spec.architecture.name()
            })
            .arg("-platform_version")
            .arg("macos")
            .arg("11.0")
            .arg("11.0");

        if let Some(sysroot) = &backend.sysroot {
            cmd.arg("-syslibroot").arg(sysroot);
        }
        return;
    }

    if let Some(emulation) = elf_lld_emulation(target) {
        cmd.arg("-m").arg(emulation);
    }
    if let Some(sysroot) = &backend.sysroot {
        cmd.arg(format!("--sysroot={}", sysroot));
    }
}

fn expand_lld_link_args(link_args: &[String]) -> Vec<String> {
    let mut out = Vec::new();
    for arg in link_args {
        if arg == "-nostartfiles" {
            continue;
        }
        if let Some(rest) = arg.strip_prefix("-Wl,") {
            out.extend(
                rest.split(',').filter(|part| !part.is_empty()).map(|part| part.to_string()),
            );
        } else {
            out.push(arg.clone());
        }
    }
    out
}

fn is_darwin_target(target: &str) -> bool {
    target_spec_for_triple(target).is_some_and(|spec| spec.os == "macos")
}

fn elf_lld_emulation(target: &str) -> Option<&'static str> {
    match target_spec_for_triple(target)?.codegen {
        CodegenTarget::LinuxX86_64
        | CodegenTarget::FreeBsdX86_64
        | CodegenTarget::FreestandingX86_64 => Some("elf_x86_64"),
        CodegenTarget::LinuxArm64
        | CodegenTarget::FreeBsdArm64
        | CodegenTarget::FreestandingArm64 => Some("aarch64elf"),
        CodegenTarget::LinuxRISCV64
        | CodegenTarget::FreeBsdRISCV64
        | CodegenTarget::FreestandingRISCV64 => Some("elf64lriscv"),
        CodegenTarget::LinuxLoongArch64 => Some("elf64loongarch"),
        CodegenTarget::Wasm32Unknown
        | CodegenTarget::Wasm32WasiP1
        | CodegenTarget::Wasm64Unknown => None,
        _ => None,
    }
}

fn resolve_bundled_tool(tool: &str) -> String {
    if let Some(path) = resolve_bundled_tool_path(tool) {
        return path.to_string_lossy().to_string();
    }
    executable_tool_name(tool)
}

fn resolve_bundled_tool_path(tool: &str) -> Option<PathBuf> {
    for dir in llvm_tool_search_dirs() {
        let candidate = dir.join(executable_tool_name(tool));
        if candidate.is_file() {
            return Some(candidate);
        }
    }
    None
}

fn configure_bundled_llvm_tool_env(cmd: &mut Command, bin: &str) {
    let Some(bin_dir) = bundled_llvm_bin_dir(bin) else {
        return;
    };

    if cfg!(target_os = "linux") {
        if let Some(lib_dir) = bin_dir.parent().map(|llvm_dir| llvm_dir.join("lib")) {
            if lib_dir.is_dir() {
                prepend_env_path(cmd, "LD_LIBRARY_PATH", lib_dir);
            }
        }
    } else if cfg!(windows) {
        if let Some(root_dir) = bin_dir.parent().and_then(|llvm_dir| llvm_dir.parent()) {
            prepend_env_path(cmd, "PATH", root_dir.to_path_buf());
        }
        prepend_env_path(cmd, "PATH", bin_dir);
    }
}

fn bundled_llvm_bin_dir(bin: &str) -> Option<PathBuf> {
    let bin_path = std::path::Path::new(bin);
    let bin_dir = bin_path.parent()?;
    if bin_dir.file_name().and_then(|name| name.to_str()) != Some("bin") {
        return None;
    }

    let llvm_dir = bin_dir.parent()?;
    if llvm_dir.file_name().and_then(|name| name.to_str()) != Some("llvm") {
        return None;
    }

    Some(bin_dir.to_path_buf())
}

fn prepend_env_path(cmd: &mut Command, name: &str, first: PathBuf) {
    let mut paths = vec![first];
    if let Some(current) = env::var_os(name) {
        paths.extend(env::split_paths(&current));
    }
    if let Ok(joined) = env::join_paths(paths) {
        cmd.env(name, joined);
    }
}

fn executable_tool_name(tool: &str) -> String {
    if cfg!(windows) && !tool.to_ascii_lowercase().ends_with(".exe") {
        format!("{}.exe", tool)
    } else {
        tool.to_string()
    }
}

fn llvm_tool_search_dirs() -> Vec<PathBuf> {
    let mut dirs = Vec::new();

    if let Ok(path) = env::var("WAVE_LLVM_BIN") {
        if !path.trim().is_empty() {
            dirs.push(PathBuf::from(path));
        }
    }
    for env_name in ["WAVE_LLVM_HOME", "LLVM_SYS_211_PREFIX"] {
        if let Ok(path) = env::var(env_name) {
            if !path.trim().is_empty() {
                dirs.push(PathBuf::from(path).join("bin"));
            }
        }
    }
    if let Ok(exe) = env::current_exe() {
        if let Some(dir) = exe.parent() {
            dirs.push(dir.to_path_buf());
            dirs.push(dir.join("llvm").join("bin"));
            if let Some(root) = dir.parent() {
                dirs.push(root.join("llvm").join("bin"));
                dirs.push(root.join("lib").join("wave").join("llvm").join("bin"));
            }
        }
    }

    dirs
}
