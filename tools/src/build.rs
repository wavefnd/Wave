// SPDX-License-Identifier: MPL-2.0
//! Cargo owns compilation. This adapter only supplies platform link environment.
use crate::{binary, cli::Args, config, error, package, process::Spec, storage, Result};
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use utils::wson::Value;

pub fn compiler(arguments: &[OsString]) -> Result<i32> {
    let split = arguments
        .iter()
        .position(|a| a == "--")
        .ok_or_else(|| error("build-cargo requires -- followed by a Cargo command"))?;
    let mut args = Args::new(&arguments[..split]);
    let target = args.required("--target")?;
    let binary = args.path("--binary")?.ok_or_else(|| error("missing --binary"))?;
    let output = args.path("--output")?.ok_or_else(|| error("missing --output"))?;
    let release = args.flag("--release");
    args.finish()?;
    let command = &arguments[split + 1..];
    if command.is_empty() {
        return Err(error("missing Cargo command"));
    }
    let cross = std::env::var("WAVE_CROSS_LLVM_TARGET").ok().as_deref() == Some(target.as_str());
    if target.contains("linux") && !cross && binary::target_arch(&target) != std::env::consts::ARCH
    {
        return Err(error("LLVM compiler runtime target differs from native build host"));
    }
    if target.contains("windows")
        && (!cfg!(windows) || binary::target_arch(&target) != std::env::consts::ARCH)
    {
        return Err(error("Windows compiler builds require matching native host"));
    }
    if let Some(directory) = std::env::var_os("CARGO_TARGET_DIR") {
        let directory = PathBuf::from(directory);
        fs::create_dir_all(&directory)?;
        let marker = directory.join(".wave-cargo-owner");
        let owner = fs::canonicalize(crate::root())?;
        if marker.exists() && fs::read_to_string(&marker)?.trim() != owner.to_string_lossy() {
            return Err(error("Cargo output directory belongs to another checkout"));
        }
        storage::atomic_write(&marker, owner.to_string_lossy().as_bytes())?;
    }
    let mut spec = Spec::new(command.iter().cloned());
    if !spec.args.iter().any(|arg| arg.to_string_lossy().starts_with("--message-format")) {
        spec.args.push("--message-format=json-render-diagnostics".into());
    }
    spec.stream = true;
    if cross {
        spec.timeout = std::time::Duration::from_secs(3600);
    }
    if release && (target.contains("linux") || target.contains("freebsd")) {
        let existing = std::env::var("RUSTFLAGS").unwrap_or_default();
        spec.env.insert("RUSTFLAGS".into(),format!("{existing} -C link-arg=-Wl,--disable-new-dtags -C link-arg=-Wl,-z,origin -C link-arg=-Wl,-rpath,$ORIGIN/llvm/lib").into());
    }
    if target.contains("windows") {
        let prefix = package::prefix()?;
        let config = std::env::var_os("LLVM_CONFIG_PATH")
            .map(PathBuf::from)
            .unwrap_or_else(|| prefix.join("bin/llvm-config.exe"));
        if !config.is_file() {
            return Err(error("native LLVM_CONFIG_PATH is required"));
        }
        spec.env.insert("LLVM_SYS_211_PREFIX".into(), prefix.into_os_string());
        spec.env.insert("LLVM_CONFIG_PATH".into(), config.into_os_string());
    }
    if cross {
        let config = PathBuf::from(
            std::env::var_os("LLVM_CONFIG_PATH")
                .ok_or_else(|| error("cross LLVM config wrapper is required"))?,
        );
        let directory = std::env::var_os("CARGO_TARGET_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|| crate::root().join("target"))
            .join("cross-llvm-config")
            .join(&target)
            .join("bin");
        fs::create_dir_all(&directory)?;
        let wrapper = directory.join("llvm-config");
        let quoted = format!(
            "'{}'",
            config
                .to_str()
                .ok_or_else(|| error("cross SDK wrapper requires UTF-8 path"))?
                .replace('\'', "'\\''")
        );
        fs::write(&wrapper, format!("#!/bin/sh\nexec {quoted} \"$@\"\n"))?;
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            fs::set_permissions(&wrapper, fs::Permissions::from_mode(0o755))?;
        }
        let libs = package::run([
            wrapper.as_os_str(),
            std::ffi::OsStr::new("--libnames"),
            std::ffi::OsStr::new("--link-shared"),
        ])?;
        let libdir = package::run([wrapper.as_os_str(), std::ffi::OsStr::new("--libdir")])?;
        if libs.split_whitespace().next().is_none()
            || libs.split_whitespace().any(|name| {
                !name.starts_with("libLLVM") || !Path::new(libdir.trim()).join(name).is_file()
            })
        {
            return Err(error("cross llvm-config returned unavailable LLVM libraries"));
        }
        if package::run([
            wrapper.as_os_str(),
            std::ffi::OsStr::new("--system-libs"),
            std::ffi::OsStr::new("--link-shared"),
        ])?
        .split_whitespace()
        .any(|item| item == "-lffi")
        {
            return Err(error("cross LLVM must not link FFI"));
        }
        spec.env
            .insert("LLVM_SYS_211_PREFIX".into(), directory.parent().unwrap().as_os_str().into());
    }
    let result = spec.checked()?;
    let receipt = cargo_receipt(&result.strict_stdout()?, &binary)?;
    binary::inspect(&binary, Some(&target))?;
    activate_compiler_output(&binary, &output, &receipt)?;

    Ok(0)
}

/// Stage and activate the compiler and Cargo receipt as a recoverable pair.
/// The prior pair is restored by the shared replacement transaction if either
/// activation fails, and its backups are retained if restoration itself fails.
fn activate_compiler_output(binary: &Path, output: &Path, receipt: &Value) -> Result<()> {
    activate_compiler_output_with(binary, output, receipt, fs::rename)
}

fn activate_compiler_output_with<F>(
    binary: &Path,
    output: &Path,
    receipt: &Value,
    rename: F,
) -> Result<()>
where
    F: FnMut(&Path, &Path) -> std::io::Result<()>,
{
    let receipt_path = output.with_extension("build.json");
    let parent = output.parent().filter(|p| !p.as_os_str().is_empty()).unwrap_or(Path::new("."));
    fs::create_dir_all(parent)?;

    // Refuse directories and symlinks before moving the old compiler. A
    // conflicting build.json directory must not be hidden inside recovery
    // backups or replaced by a file.
    for destination in [output, receipt_path.as_path()] {
        match fs::symlink_metadata(destination) {
            Ok(metadata) if metadata.file_type().is_file() => {},
            Ok(_) => {
                return Err(error(format!(
                    "build output destination is not a regular file: {}",
                    destination.display()
                )));
            },
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => {},
            Err(err) => return Err(err.into()),
        }
    }

    // Serialization and all staging I/O must succeed before activation.
    let receipt_data = config::json(receipt)?;
    let binary_stage = tempfile::NamedTempFile::new_in(parent)?;
    fs::copy(binary, binary_stage.path())?;
    fs::set_permissions(binary_stage.path(), fs::metadata(binary)?.permissions())?;
    binary_stage.as_file().sync_all()?;

    let receipt_stage = tempfile::NamedTempFile::new_in(parent)?;
    fs::write(receipt_stage.path(), receipt_data)?;
    receipt_stage.as_file().sync_all()?;

    // Close staging handles before renaming (notably needed on Windows).
    // TempPath still cleans up unactivated candidates on every error path.
    let binary_stage = binary_stage.into_temp_path();
    let receipt_stage = receipt_stage.into_temp_path();
    storage::replace_set_with(
        &[
            (binary_stage.to_path_buf(), output.to_path_buf()),
            (receipt_stage.to_path_buf(), receipt_path),
        ],
        rename,
    )
}

pub fn clean(arguments: &[OsString]) -> Result<i32> {
    let mut args = Args::new(arguments);
    let root = crate::root();
    let selected = args.path("--cargo-target-dir")?.unwrap_or_else(|| root.join("target"));
    let dist = args.path("--dist-dir")?.unwrap_or_else(|| root.join("dist"));
    args.finish()?;
    clean_outputs(&root, &selected, &dist)?;
    Ok(0)
}

pub fn clean_outputs(root: &Path, selected: &Path, dist: &Path) -> Result<()> {
    let owner = fs::canonicalize(root)?;
    let default = storage::resolved(&root.join("target"))?;
    let location = |path: &Path| -> Result<PathBuf> {
        let name =
            path.file_name().ok_or_else(|| error("cleanup requires a named output directory"))?;
        Ok(storage::resolved(path.parent().unwrap_or(Path::new(".")))?.join(name))
    };
    let selected_location = location(selected)?;
    let selected_path = storage::resolved(selected)?;
    // Validate every destination before removing anything. Resolve parents but
    // not the final directory link: cleanup unlinks that link, never its target.
    if selected_location == owner || owner.starts_with(&selected_location) {
        return Err(error("refusing to delete the checkout or one of its ancestors"));
    }
    if location(dist)? != owner.join("dist") {
        return Err(error("refusing to delete an unowned custom distribution directory"));
    }
    if storage::exists(selected)
        && !fs::symlink_metadata(selected)?.is_symlink()
        && selected_path != default
    {
        let marker = selected.join(".wave-cargo-owner");
        if fs::read_to_string(marker).unwrap_or_default().trim() != owner.to_string_lossy() {
            return Err(error("refusing to delete unowned custom Cargo directory"));
        }
    }
    let manifest = crate::cases::Manifest::load(root, false)?;
    let targets = manifest
        .targets
        .iter()
        .filter(|t| t.enabled)
        .map(|t| t.archive_target())
        .collect::<Result<Vec<_>>>()?;
    storage::remove(selected)?;
    storage::remove(dist)?;
    for entry in fs::read_dir(root)? {
        let path = entry?.path();
        let name = path.file_name().unwrap().to_string_lossy();
        if name.starts_with("wave-v")
            && targets.iter().any(|target| {
                [".zip", ".tar.gz"].into_iter().any(|ext| {
                    ["", ".sha256", ".metadata.json"]
                        .into_iter()
                        .any(|suffix| name.ends_with(&format!("-{target}{ext}{suffix}")))
                })
            })
        {
            storage::remove(&path)?;
        }
    }
    Ok(())
}

// Override repository cross-linker defaults as well as inherited target flags.
// Clearing environment overrides alone exposes .cargo/config.toml again.
fn native_cargo(host: &str) -> std::process::Command {
    let mut cargo = std::process::Command::new("cargo");
    for (name, _) in std::env::vars_os() {
        let text = name.to_string_lossy();
        if text.starts_with("CARGO_TARGET_")
            || text.starts_with("RUSTFLAGS")
            || text == "CARGO_ENCODED_RUSTFLAGS"
            || text == "CARGO_BUILD_TARGET"
            || text == "RUSTC_WRAPPER"
            || text == "RUSTC_WORKSPACE_WRAPPER"
        {
            cargo.env_remove(name);
        }
    }
    if cfg!(unix) {
        cargo.env(format!("CARGO_TARGET_{}_LINKER", host.replace('-', "_").to_uppercase()), "cc");
    }
    cargo
}

/// Cargo tests can run directly without Meson. Their native helper is built in
/// a separate directory, so target-specific LLVM/link flags cannot leak into it.
pub fn test_command() -> Result<std::process::Command> {
    static BINARY: std::sync::OnceLock<std::result::Result<PathBuf, String>> =
        std::sync::OnceLock::new();
    let binary = BINARY.get_or_init(|| {
        (|| -> Result<PathBuf> {
            if let Some(path) = std::env::var_os("WAVE_TOOLS") {
                let path = PathBuf::from(path);
                if !path.is_file() {
                    return Err(error("WAVE_TOOLS binary is missing"));
                }
                return Ok(fs::canonicalize(path)?);
            }
            let pin = crate::config::pin("RUST_VERSION")?;
            let output =
                std::process::Command::new("rustc").arg(format!("+{pin}")).arg("-vV").output()?;
            if !output.status.success() {
                return Err(error("cannot query pinned native Rust toolchain"));
            }
            let text = String::from_utf8(output.stdout)?;
            let host = text
                .lines()
                .find_map(|v| v.strip_prefix("host: "))
                .ok_or_else(|| error("Rust host triple missing"))?;
            let directory = crate::root().join("target/wave-tools-native");
            let mut cargo = native_cargo(host);
            cargo
                .current_dir(crate::root())
                .args([
                    &format!("+{pin}"),
                    "build",
                    "--locked",
                    "--offline",
                    "--jobs",
                    "2",
                    "-p",
                    "wave-tools",
                    "--target",
                    host,
                    "--target-dir",
                ])
                .arg(&directory);
            // This is the pre-helper Cargo bootstrap; normal commands use Spec.
            let output = cargo.output()?;
            if !output.status.success() {
                return Err(error(format!(
                    "native tooling bootstrap failed: {}",
                    String::from_utf8_lossy(&output.stderr)
                )));
            }
            Ok(directory
                .join(host)
                .join("debug")
                .join(format!("wave-tools{}", std::env::consts::EXE_SUFFIX)))
        })()
        .map_err(|e| e.to_string())
    });
    Ok(std::process::Command::new(binary.as_ref().map_err(|e| error(e.clone()))?))
}

pub fn cargo_receipt(messages: &str, binary: &Path) -> Result<Value> {
    let mut artifact = false;
    let mut directories = std::collections::BTreeSet::new();
    for line in messages.lines().filter(|line| !line.trim().is_empty()) {
        let event = config::parse_json(line)?;
        if event.get_str("reason") == Some("compiler-artifact")
            && event.get("target").and_then(|target| target.get_str("name")) == Some("wavec")
            && event.get_str("executable").is_some_and(|path| {
                storage::resolved(Path::new(path)).is_ok_and(|path| {
                    storage::resolved(binary).is_ok_and(|expected| path == expected)
                })
            })
        {
            artifact = true;
        }
        if event.get_str("reason") == Some("build-script-executed")
            && event
                .get_str("package_id")
                .is_some_and(|id| id.starts_with("path+") && id.contains("/llvm#"))
        {
            directories.insert(config::text(&event, "out_dir")?.to_string());
        }
    }
    if !artifact || directories.len() != 1 {
        return Err(error(
            "Cargo did not identify exactly one wavec output and LLVM build directory",
        ));
    }
    Ok(Value::object([
        ("schema_version", Value::integer(1)),
        ("binary", Value::string(fs::canonicalize(binary)?.to_string_lossy())),
        ("binary_sha256", Value::string(storage::digest(binary)?)),
        ("llvm_out_dir", Value::string(directories.into_iter().next().unwrap())),
    ]))
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;

    #[test]
    fn native_bootstrap_overrides_repository_cross_linkers() {
        let directory = tempfile::tempdir().unwrap();
        let pin = crate::config::pin("RUST_VERSION").unwrap();
        let identity =
            std::process::Command::new("rustc").args([&format!("+{pin}"), "-vV"]).output().unwrap();
        assert!(identity.status.success());
        let identity = String::from_utf8(identity.stdout).unwrap();
        let host = identity.lines().find_map(|line| line.strip_prefix("host: ")).unwrap();
        fs::create_dir(directory.path().join(".cargo")).unwrap();
        fs::write(
            directory.path().join(".cargo/config.toml"),
            format!("[target.{host}]\nlinker = \"wave-missing-cross-linker\"\n"),
        )
        .unwrap();
        fs::write(
            directory.path().join("Cargo.toml"),
            "[package]\nname = \"native-bootstrap-probe\"\nversion = \"0.1.0\"\nedition = \"2021\"\n[workspace]\n",
        )
        .unwrap();
        fs::write(
            directory.path().join("Cargo.lock"),
            "version = 4\n[[package]]\nname = \"native-bootstrap-probe\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        fs::create_dir(directory.path().join("src")).unwrap();
        fs::write(directory.path().join("src/main.rs"), "fn main() { println!(\"native\"); }\n")
            .unwrap();
        let result = native_cargo(host)
            .current_dir(directory.path())
            .args([
                &format!("+{pin}"),
                "build",
                "--locked",
                "--offline",
                "--jobs",
                "2",
                "--target",
                host,
            ])
            .output()
            .unwrap();
        assert!(result.status.success(), "{}", String::from_utf8_lossy(&result.stderr));
        let result = std::process::Command::new(
            directory.path().join("target").join(host).join("debug/native-bootstrap-probe"),
        )
        .output()
        .unwrap();
        assert!(result.status.success());
        assert_eq!(result.stdout, b"native\n");
    }
}

#[cfg(test)]
mod build_output_tests {
    use super::*;

    fn receipt_for(binary: &Path) -> Value {
        Value::object([
            ("schema_version", Value::integer(1)),
            ("binary_sha256", Value::string(storage::digest(binary).unwrap())),
        ])
    }

    #[test]
    fn receipt_directory_conflict_preserves_prior_compiler() {
        let directory = tempfile::tempdir().unwrap();
        let root = directory.path();
        let candidate = root.join("cargo-wavec");
        let output = root.join("wavec");
        let receipt_path = output.with_extension("build.json");
        fs::write(&candidate, b"NEW-BINARY").unwrap();
        fs::write(&output, b"OLD-BINARY").unwrap();
        fs::create_dir(&receipt_path).unwrap();
        fs::write(receipt_path.join("sentinel"), b"KEEP").unwrap();
        let result = activate_compiler_output(&candidate, &output, &receipt_for(&candidate));
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("not a regular file"));
        assert_eq!(fs::read(&output).unwrap(), b"OLD-BINARY");
        assert_eq!(fs::read(receipt_path.join("sentinel")).unwrap(), b"KEEP");
    }

    #[test]
    fn successful_activation_installs_matching_binary_and_receipt() {
        let directory = tempfile::tempdir().unwrap();
        let root = directory.path();
        let candidate = root.join("cargo-wavec");
        let output = root.join("wavec");
        let receipt_path = output.with_extension("build.json");
        fs::write(&candidate, b"NEW-BINARY").unwrap();
        fs::write(&output, b"OLD-BINARY").unwrap();
        fs::write(&receipt_path, b"OLD-RECEIPT").unwrap();
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            fs::set_permissions(&candidate, fs::Permissions::from_mode(0o755)).unwrap();
        }
        let receipt = receipt_for(&candidate);
        activate_compiler_output(&candidate, &output, &receipt).unwrap();
        assert_eq!(fs::read(&output).unwrap(), b"NEW-BINARY");
        assert_eq!(fs::read_to_string(&receipt_path).unwrap(), config::json(&receipt).unwrap());
        assert_eq!(
            config::parse_json(&fs::read_to_string(&receipt_path).unwrap())
                .unwrap()
                .get_str("binary_sha256"),
            Some(storage::digest(&output).unwrap().as_str()),
        );
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            assert_eq!(fs::metadata(output).unwrap().permissions().mode() & 0o111, 0o111);
        }
    }

    #[test]
    fn receipt_activation_failure_restores_both_previous_outputs() {
        let directory = tempfile::tempdir().unwrap();
        let root = directory.path();
        let candidate = root.join("cargo-wavec");
        let output = root.join("wavec");
        let receipt_path = output.with_extension("build.json");
        fs::write(&candidate, b"NEW-BINARY").unwrap();
        fs::write(&output, b"OLD-BINARY").unwrap();
        fs::write(&receipt_path, b"OLD-RECEIPT").unwrap();
        let mut failed_receipt = false;
        let result = activate_compiler_output_with(
            &candidate,
            &output,
            &receipt_for(&candidate),
            |source, destination| {
                if destination == receipt_path && !failed_receipt {
                    failed_receipt = true;
                    return Err(std::io::Error::other("injected receipt activation failure"));
                }
                fs::rename(source, destination)
            },
        );
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("injected receipt activation failure"));
        assert_eq!(fs::read(output).unwrap(), b"OLD-BINARY");
        assert_eq!(fs::read(receipt_path).unwrap(), b"OLD-RECEIPT");
    }

    #[test]
    fn failed_restore_reports_retained_recovery_backup() {
        let directory = tempfile::tempdir().unwrap();
        let root = directory.path();
        let candidate = root.join("cargo-wavec");
        let output = root.join("wavec");
        let receipt_path = output.with_extension("build.json");
        fs::write(&candidate, b"NEW-BINARY").unwrap();
        fs::write(&output, b"OLD-BINARY").unwrap();
        fs::write(&receipt_path, b"OLD-RECEIPT").unwrap();
        let mut failed_receipt = false;
        let result = activate_compiler_output_with(
            &candidate,
            &output,
            &receipt_for(&candidate),
            |source, destination| {
                if destination == receipt_path && !failed_receipt {
                    failed_receipt = true;
                    return Err(std::io::Error::other("injected receipt activation failure"));
                }
                if destination == output
                    && source.file_name() == Some(std::ffi::OsStr::new("0"))
                {
                    return Err(std::io::Error::other("injected binary restoration failure"));
                }
                fs::rename(source, destination)
            },
        );
        let error = result.unwrap_err().to_string();
        assert!(error.contains("retained backups"), "{error}");
        assert!(storage::files(root).unwrap().iter().any(|path| {
            fs::read(path).unwrap() == b"OLD-BINARY"
        }));
        assert_eq!(fs::read(receipt_path).unwrap(), b"OLD-RECEIPT");
    }

    #[test]
    fn receipt_failure_restores_absence_of_previous_receipt() {
        let directory = tempfile::tempdir().unwrap();
        let root = directory.path();
        let candidate = root.join("cargo-wavec");
        let output = root.join("wavec");
        let receipt_path = output.with_extension("build.json");
        fs::write(&candidate, b"NEW-BINARY").unwrap();
        fs::write(&output, b"OLD-BINARY").unwrap();
        let result = activate_compiler_output_with(
            &candidate,
            &output,
            &receipt_for(&candidate),
            |source, destination| {
                if destination == receipt_path {
                    return Err(std::io::Error::other("injected receipt failure"));
                }
                fs::rename(source, destination)
            },
        );
        assert!(result.is_err());
        assert_eq!(fs::read(output).unwrap(), b"OLD-BINARY");
        assert!(!receipt_path.exists());
    }
}
