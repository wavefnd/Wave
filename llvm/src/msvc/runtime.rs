// SPDX-License-Identifier: MPL-2.0
//! LLVM's arithmetic builtins supplement the MSVC CRT (notably for i128/u128).
use std::path::PathBuf;

pub fn find_builtins(target: &str, roots: &[PathBuf]) -> Option<PathBuf> {
    let arch = match target {
        "x86_64-pc-windows-msvc" => "x86_64",
        "aarch64-pc-windows-msvc" => "aarch64",
        _ => return None,
    };
    roots
        .iter()
        .map(|root| {
            root.join("lib/clang/21/lib/windows").join(format!("clang_rt.builtins-{arch}.lib"))
        })
        .find(|path| path.is_file())
}

pub fn add_builtins(target: &str, args: &mut Vec<String>) {
    if args.iter().any(|arg| arg.eq_ignore_ascii_case("/NODEFAULTLIB")) {
        return;
    }
    let mut roots = Vec::new();
    for name in ["WAVE_LLVM_HOME", "LLVM_SYS_211_PREFIX"] {
        if let Some(root) = std::env::var_os(name).filter(|v| !v.is_empty()) {
            roots.push(PathBuf::from(root));
        }
    }
    for name in ["WAVE_LLVM_BIN", "WAVE_WINDOWS_LLVM_BIN"] {
        if let Some(bin) = std::env::var_os(name).filter(|v| !v.is_empty()) {
            if let Some(root) = PathBuf::from(bin).parent() {
                roots.push(root.to_path_buf());
            }
        }
    }
    if let Ok(exe) = std::env::current_exe() {
        if let Some(bin) = exe.parent() {
            roots.push(bin.join("llvm"));
            if let Some(root) = bin.parent() {
                roots.push(root.join("llvm"));
                roots.push(root.join("lib/wave/llvm"));
            }
        }
    }
    if let Some(library) = find_builtins(target, &roots) {
        // Optional for programs using only CRT-provided helpers. Its members
        // are selected lazily and the regular MSVC input validator checks CPU.
        args.push(format!("/DEFAULTLIB:{}", library.display()));
    }
}
