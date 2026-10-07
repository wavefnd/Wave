// SPDX-License-Identifier: MPL-2.0
//! Target-specific SDK discovery. Explicit /LIBPATH (-L) precedes LIB, then
//! configured SDK/VC roots, then installed Windows SDKs/Visual Studio.
use super::coff;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

pub type Environment = BTreeMap<String, String>;
fn get<'a>(env: &'a Environment, key: &str) -> Option<&'a str> {
    env.iter()
        .find(|(k, _)| k.eq_ignore_ascii_case(key))
        .map(|(_, v)| v.as_str())
        .filter(|v| !v.is_empty())
}

fn versions(root: &Path) -> Vec<PathBuf> {
    let mut entries: Vec<_> = std::fs::read_dir(root)
        .into_iter()
        .flatten()
        .filter_map(Result::ok)
        .map(|e| e.path())
        .filter(|p| p.is_dir())
        .filter_map(|p| {
            let key = p
                .file_name()?
                .to_str()?
                .split('.')
                .map(str::parse::<u32>)
                .collect::<Result<Vec<_>, _>>()
                .ok()?;
            Some((key, p))
        })
        .collect();
    entries.sort_by(|a, b| b.0.cmp(&a.0));
    entries.into_iter().map(|(_, p)| p).collect()
}

fn add(paths: &mut Vec<PathBuf>, path: PathBuf) {
    if path.is_dir() && !paths.contains(&path) {
        paths.push(path);
    }
}

fn add_sdk(paths: &mut Vec<PathBuf>, root: &Path, version: Option<&str>, arch: &str) {
    let lib = root.join("Lib");
    let candidates = if let Some(v) = version {
        vec![lib.join(v.trim_end_matches(['/', '\\']))]
    } else {
        versions(&lib)
    };
    for candidate in candidates {
        let um = candidate.join("um").join(arch);
        let ucrt = candidate.join("ucrt").join(arch);
        if um.join("kernel32.lib").is_file() && ucrt.join("ucrt.lib").is_file() {
            add(paths, um);
            add(paths, ucrt);
            break;
        }
    }
}

fn add_vc(paths: &mut Vec<PathBuf>, root: &Path, arch: &str) {
    let lib = root.join("lib").join(arch);
    if lib.join("vcruntime.lib").is_file() {
        add(paths, lib);
    }
}

pub fn discover(target: &str, explicit: &[String], env: &Environment) -> Vec<PathBuf> {
    let arch = if target.starts_with("aarch64-") { "arm64" } else { "x64" };
    let mut paths = explicit.iter().map(PathBuf::from).collect::<Vec<_>>();
    if let Some(lib) = get(env, "LIB") {
        for path in lib.split(';').filter(|p| !p.is_empty()) {
            add(&mut paths, PathBuf::from(path));
        }
    }
    if let Some(root) = get(env, "WindowsSdkDir") {
        add_sdk(&mut paths, Path::new(root), get(env, "WindowsSDKVersion"), arch);
    } else if let Some(programs) = get(env, "ProgramFiles(x86)") {
        add_sdk(&mut paths, &Path::new(programs).join("Windows Kits/10"), None, arch);
    }
    if let Some(root) = get(env, "VCToolsInstallDir") {
        add_vc(&mut paths, Path::new(root), arch);
    } else if let Some(vs) = get(env, "VSINSTALLDIR") {
        for root in versions(&Path::new(vs).join("VC/Tools/MSVC")) {
            let before = paths.len();
            add_vc(&mut paths, &root, arch);
            if paths.len() != before {
                break;
            }
        }
    }
    paths
}

// Windows SDKs can live outside Program Files. Read the installer-owned key
// as UTF-16 so non-ASCII install paths do not depend on console code pages.
#[cfg(windows)]
fn installed_sdk_root() -> Option<String> {
    use std::ffi::c_void;
    #[link(name = "advapi32")]
    extern "system" {
        fn RegGetValueW(
            key: *mut c_void,
            subkey: *const u16,
            value: *const u16,
            flags: u32,
            kind: *mut u32,
            data: *mut c_void,
            bytes: *mut u32,
        ) -> i32;
    }
    let subkey: Vec<u16> =
        "SOFTWARE\\Microsoft\\Windows Kits\\Installed Roots\0".encode_utf16().collect();
    let value: Vec<u16> = "KitsRoot10\0".encode_utf16().collect();
    let mut buffer = vec![0u16; 32768];
    let mut bytes = (buffer.len() * 2) as u32;
    // SAFETY: predefined HKLM handle; all UTF-16 names are terminated, output
    // storage is writable for `bytes` bytes, and no pointer escapes the call.
    let status = unsafe {
        RegGetValueW(
            (-2147483646isize) as *mut c_void,
            subkey.as_ptr(),
            value.as_ptr(),
            0x0002 | 0x0002_0000,
            std::ptr::null_mut(),
            buffer.as_mut_ptr().cast(),
            &mut bytes,
        )
    };
    if status != 0 || bytes < 2 || bytes as usize > buffer.len() * 2 || bytes % 2 != 0 {
        return None;
    }
    String::from_utf16(&buffer[..bytes as usize / 2 - 1]).ok().filter(|s| !s.is_empty())
}

pub fn environment(target: &str) -> Environment {
    let mut env: Environment = std::env::vars_os()
        .filter_map(|(k, v)| Some((k.into_string().ok()?, v.into_string().ok()?)))
        .collect();
    #[cfg(windows)]
    if get(&env, "WindowsSdkDir").is_none() {
        if let Some(root) = installed_sdk_root() {
            env.insert("WindowsSdkDir".into(), root);
        }
    }
    // vswhere is installed with Visual Studio; no shell or PATH substitution.
    // It locates non-default installs while the selected target controls libs.
    if cfg!(windows)
        && get(&env, "VCToolsInstallDir").is_none()
        && get(&env, "VSINSTALLDIR").is_none()
    {
        if let Some(programs) = get(&env, "ProgramFiles(x86)") {
            let exe = Path::new(programs).join("Microsoft Visual Studio/Installer/vswhere.exe");
            if let Ok(out) = std::process::Command::new(exe)
                .args(["-all", "-sort", "-utf8", "-products", "*", "-property", "installationPath"])
                .output()
            {
                if out.status.success() {
                    let arch = if target.starts_with("aarch64-") { "arm64" } else { "x64" };
                    for root in String::from_utf8_lossy(&out.stdout).lines().map(str::trim) {
                        if !root.is_empty()
                            && versions(&Path::new(root).join("VC/Tools/MSVC"))
                                .iter()
                                .any(|p| p.join("lib").join(arch).join("vcruntime.lib").is_file())
                        {
                            env.insert("VSINSTALLDIR".into(), root.into());
                            break;
                        }
                    }
                }
            }
        }
    }
    env
}

pub fn discovered_arguments(target: &str, paths: &[String]) -> Vec<String> {
    discover(target, paths, &environment(target))
        .into_iter()
        .map(|p| p.to_string_lossy().into_owned())
        .collect()
}

fn value<'a>(arg: &'a str, prefix: &str) -> Option<&'a str> {
    arg.get(..prefix.len()).filter(|v| v.eq_ignore_ascii_case(prefix)).map(|_| &arg[prefix.len()..])
}

fn find_library(name: &str, paths: &[PathBuf]) -> Option<PathBuf> {
    let name = name.trim_matches('"');
    let file = if name.to_ascii_lowercase().ends_with(".lib") {
        name.to_string()
    } else {
        format!("{name}.lib")
    };
    let direct = PathBuf::from(&file);
    if direct.is_file() {
        return Some(direct);
    }
    paths.iter().map(|p| p.join(&file)).find(|p| p.is_file())
}

/// Inspect explicitly resolved inputs before either linker is launched. Object
/// directives are still interpreted by the linker; no claim of LTO validation.
pub fn validate_arguments(target: &str, args: &[String]) -> Result<(), String> {
    let paths: Vec<_> =
        args.iter().filter_map(|a| value(a, "/LIBPATH:")).map(PathBuf::from).collect();
    let no_defaults = args.iter().any(|a| a.eq_ignore_ascii_case("/NODEFAULTLIB"));
    let mut files = Vec::new();
    let mut required = Vec::new();
    for arg in args {
        if let Some(lib) = value(arg, "/DEFAULTLIB:") {
            if !no_defaults {
                required.push(lib);
            }
        } else if let Some(lib) = value(arg, "/WHOLEARCHIVE:") {
            required.push(lib);
        } else if arg.starts_with('@') {
            return Err(
                "MSVC response-file input cannot be inspected; supply inputs directly".into()
            );
        } else if arg.starts_with('/') && arg.contains(':') && !Path::new(arg).is_file() {
            continue;
        } else {
            match Path::new(arg)
                .extension()
                .and_then(|e| e.to_str())
                .map(str::to_ascii_lowercase)
                .as_deref()
            {
                Some("lib") => required.push(arg),
                Some("obj" | "o" | "a" | "bc") => files.push(PathBuf::from(arg)),
                _ => {},
            }
        }
    }
    for lib in required {
        let file = find_library(lib,&paths).ok_or_else(||format!(
            "MSVC library '{lib}' for {target} was not found; provide matching Windows SDK UM/UCRT and VC libraries with -L, LIB, or WindowsSdkDir/WindowsSDKVersion and VCToolsInstallDir. Searched: {}",
            paths.iter().map(|p|p.display().to_string()).collect::<Vec<_>>().join("; ")
        ))?;
        if !files.contains(&file) {
            files.push(file);
        }
    }
    for file in files {
        let whole = args.iter().any(|a| a.eq_ignore_ascii_case("/WHOLEARCHIVE"))
            || args
                .iter()
                .filter_map(|a| value(a, "/WHOLEARCHIVE:"))
                .filter_map(|lib| find_library(lib, &paths))
                .any(|p| p == file);
        if whole {
            coff::validate_file_all_members(&file, target)?;
        } else {
            coff::validate_file(&file, target)?;
        }
    }
    Ok(())
}

/// Object inputs are supplied separately so inspection never depends on their
/// filename extension. The argument pass also resolves named libraries.
pub fn validate_link_inputs(
    target: &str,
    objects: &[String],
    args: &[String],
) -> Result<(), String> {
    for object in objects {
        coff::validate_file(Path::new(object), target)?;
    }
    validate_arguments(target, args)
}
