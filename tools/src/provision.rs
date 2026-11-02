// SPDX-License-Identifier: MPL-2.0
//! Explicit platform prerequisites. Actions/Meson own scheduling and conditions.
use crate::{cli::Args, config, error, package, process::Spec, sdk, storage, Result};
use std::collections::BTreeMap;
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;

pub fn export(values: &BTreeMap<String, String>, output: Option<&Path>) -> Result<()> {
    for (name, value) in values {
        config::env_name(name)?;
        if value.contains('\0') {
            return Err(error("environment value contains NUL"));
        }
    }
    if let Some(path) = output {
        for (name, value) in values {
            config::append_env(path, name, value)?;
        }
    } else {
        for (name, value) in values {
            print!("{}", String::from_utf8(config::encode_env(name, value)?)?);
        }
    }
    Ok(())
}

fn command(
    args: impl IntoIterator<Item = impl Into<OsString>>,
    env: &BTreeMap<String, String>,
) -> Result<String> {
    let mut spec = Spec::new(args);
    spec.env = env.iter().map(|(name, value)| (name.into(), value.into())).collect();
    spec.stream = true;
    spec.timeout = Duration::from_secs(1800);
    spec.checked()?.strict_stdout()
}

pub fn host(target: &str, provision: bool, work: &Path) -> Result<BTreeMap<String, String>> {
    let manifest = crate::cases::Manifest::load(&crate::root(), false)?;
    let selected = manifest.resolve(target)?;
    let mut env = BTreeMap::new();
    let major = config::pin("LLVM_VERSION")?;
    if cfg!(windows) {
        crate::windows::require_native(&selected.rust_target()?)?;
    }
    if selected.executor == "native"
        && crate::cases::normalize_arch(std::env::consts::ARCH) != selected.arch
    {
        return Err(error("native toolchain target must match the actual host"));
    }
    fs::create_dir_all(work)?;
    let prefix = if cfg!(target_os = "macos") {
        env.insert("HOMEBREW_NO_AUTO_UPDATE".into(), "1".into());
        env.insert("HOMEBREW_NO_INSTALL_UPGRADE".into(), "1".into());
        if provision {
            command(["brew", "install", &format!("llvm@{major}"), &format!("lld@{major}")], &env)?;
        }
        let prefix =
            PathBuf::from(command(["brew", "--prefix", &format!("llvm@{major}")], &env)?.trim());
        let lld =
            PathBuf::from(command(["brew", "--prefix", &format!("lld@{major}")], &env)?.trim());
        let original = std::env::var_os("PATH").unwrap_or_default();
        env.insert(
            "PATH".into(),
            std::env::join_paths(
                [lld.join("bin"), prefix.join("bin")]
                    .into_iter()
                    .chain(std::env::split_paths(&original)),
            )?
            .to_string_lossy()
            .into_owned(),
        );
        prefix
    } else if cfg!(windows) {
        package::prefix()?
    } else {
        if provision {
            command(["sudo", "apt-get", "update"], &env)?;
            let mut packages = vec![
                "sudo".into(),
                "apt-get".into(),
                "install".into(),
                "-y".into(),
                "wget".into(),
                "curl".into(),
                "software-properties-common".into(),
                "ca-certificates".into(),
                "gnupg".into(),
                "ripgrep".into(),
                "patchelf".into(),
                "xz-utils".into(),
            ];
            if ["linux-loong64", "linux-riscv64"].contains(&target) || selected.executor == "qemu" {
                packages.push("qemu-user".into());
            }
            if selected.id == "linux-riscv64" {
                packages.extend(
                    [
                        "gcc-riscv64-linux-gnu",
                        "g++-riscv64-linux-gnu",
                        "libc6-dev-riscv64-cross",
                        "binutils-riscv64-linux-gnu",
                    ]
                    .map(OsString::from),
                );
            }
            if selected.os == "freebsd" {
                packages.extend(
                    ["qemu-system-x86", "qemu-utils", "genisoimage", "openssh-client"]
                        .map(OsString::from),
                );
            }
            command(packages, &env)?;
            let installer = work.join("llvm.sh");
            command(
                [
                    OsString::from("curl"),
                    "--ipv4".into(),
                    "--fail".into(),
                    "--location".into(),
                    "--retry".into(),
                    "3".into(),
                    "--max-time".into(),
                    "90".into(),
                    "--proto".into(),
                    "=https".into(),
                    "--proto-redir".into(),
                    "=https".into(),
                    "https://apt.llvm.org/llvm.sh".into(),
                    "--output".into(),
                    installer.clone().into(),
                ],
                &env,
            )?;
            command(
                [OsString::from("sudo"), "bash".into(), installer.into(), major.clone().into()],
                &env,
            )?;
            command(
                [
                    "sudo",
                    "apt-get",
                    "install",
                    "-y",
                    &format!("clang-{major}"),
                    &format!("lld-{major}"),
                    &format!("llvm-{major}-dev"),
                    "libffi-dev",
                    "libzstd-dev",
                    "zlib1g-dev",
                ],
                &env,
            )?;
        }
        std::env::var_os("LLVM_SYS_211_PREFIX")
            .map(PathBuf::from)
            .unwrap_or(PathBuf::from(config::pin("LLVM_PREFIX_LINUX")?))
    };
    let llvm_config =
        prefix.join("bin").join(if cfg!(windows) { "llvm-config.exe" } else { "llvm-config" });
    let version = command([llvm_config.as_os_str(), "--version".as_ref()], &env)?;
    if !version.trim().starts_with(&format!("{major}.")) {
        return Err(error("LLVM major version mismatch"));
    }
    let rust = config::pin("RUST_VERSION")?;
    let identity = package::run(["rustc", &format!("+{rust}"), "--version"])?;
    if identity.split_whitespace().nth(1) != Some(&rust) {
        return Err(error("pinned Rust version mismatch"));
    }
    env.insert("LLVM_SYS_211_PREFIX".into(), prefix.to_string_lossy().into());
    env.insert("WAVE_LLVM_HOME".into(), prefix.to_string_lossy().into());
    env.insert("LLVM_CONFIG_PATH".into(), llvm_config.to_string_lossy().into());
    let inherited =
        env.get("PATH").cloned().unwrap_or_else(|| std::env::var("PATH").unwrap_or_default());
    env.insert(
        "PATH".into(),
        std::env::join_paths(
            std::iter::once(prefix.join("bin"))
                .chain(std::env::split_paths(&OsString::from(inherited))),
        )?
        .to_string_lossy()
        .into(),
    );
    let darwin = prefix.join("bin/ld64.lld");
    if darwin.is_file() {
        env.insert("WAVE_LD64_LLD".into(), darwin.to_string_lossy().into());
    }
    #[cfg(unix)]
    if !env.contains_key("WAVE_LD64_LLD") {
        if let Some(linker) = [prefix.join("bin/ld.lld"), prefix.join("bin/lld")]
            .into_iter()
            .find(|path| path.is_file())
        {
            let directory = work.join("llvm-bin");
            fs::create_dir_all(&directory)?;
            let alias = directory.join("ld64.lld");
            if storage::exists(&alias) {
                if !fs::symlink_metadata(&alias)?.is_symlink() {
                    return Err(error("LLVM alias path is not a symlink"));
                }
                fs::remove_file(&alias)?;
            }
            std::os::unix::fs::symlink(linker, &alias)?;
            env.insert("WAVE_LD64_LLD".into(), alias.to_string_lossy().into());
        }
    }

    Ok(env)
}

pub fn loongarch(work: &Path, provision: bool) -> Result<BTreeMap<String, String>> {
    let mut env = BTreeMap::new();
    if !provision {
        let root = PathBuf::from(
            std::env::var_os("WAVE_LOONGARCH64_SYSROOT")
                .ok_or_else(|| error("LoongArch sysroot required; use --provision"))?,
        );
        if !root.is_dir() || package::which("loongarch64-unknown-linux-gnu-gcc").is_none() {
            return Err(error("LoongArch compiler and sysroot required"));
        }
        return Ok(env);
    }
    fs::create_dir_all(work)?;
    let name = config::pin("LOONGARCH_TOOLCHAIN_ARCHIVE")?;
    let archive = work.join(&name);
    sdk::download(
        &format!(
            "https://github.com/loongson/build-tools/releases/download/{}/{name}",
            config::pin("LOONGARCH_TOOLCHAIN_VERSION")?
        ),
        &archive,
        &config::pin("LOONGARCH_TOOLCHAIN_SHA256")?,
    )?;
    let extracted = work.join("loongarch-toolchain");
    sdk::extract_toolchain(&archive, &extracted)?;
    let toolchain = extracted.join("cross-tools");
    if !toolchain.join("bin/loongarch64-unknown-linux-gnu-gcc").is_file() {
        return Err(error("LoongArch compiler missing from pinned archive"));
    }
    env.insert(
        "WAVE_LOONGARCH64_SYSROOT".into(),
        toolchain.join("target").to_string_lossy().into(),
    );
    env.insert(
        "PATH".into(),
        std::env::join_paths(
            std::iter::once(toolchain.join("bin"))
                .chain(std::env::split_paths(&std::env::var_os("PATH").unwrap_or_default())),
        )?
        .to_string_lossy()
        .into(),
    );
    Ok(env)
}

pub fn cross_environment(
    target: &str,
    work: &Path,
    sysroot: &Path,
    cross_prefix: &str,
    host_bin: &Path,
) -> Result<BTreeMap<String, String>> {
    let manifest = crate::cases::Manifest::load(&crate::root(), false)?;
    let selected = manifest.target(target)?;
    let triple = selected.rust_target()?;
    fs::create_dir_all(work)?;
    let pins = config::read_json(&crate::root().join("tools/ci/llvm_bundles.json"))?;
    let entry = pins
        .get("bundles")
        .and_then(|v| v.get(target))
        .ok_or_else(|| error("missing pinned SDK"))?;
    let sdk = work.join("llvm-sdk");
    sdk::fetch(entry, &sdk, target, sysroot)?;
    let qemu = if target == "linux-riscv64" { "qemu-riscv64" } else { "qemu-loongarch64" };
    if !Path::new("/proc/sys/fs/binfmt_misc").join(qemu).is_file() {
        return Err(error("QEMU binfmt registration required for extracted package validation"));
    }
    let wrapper = work.join("llvm-config-target");
    let quote = |path: &Path| -> Result<String> {
        Ok(format!(
            "'{}'",
            path.to_str()
                .ok_or_else(|| error("cross wrapper requires UTF-8 paths"))?
                .replace('\'', "'\\''")
        ))
    };
    fs::write(
        &wrapper,
        format!(
            "#!/bin/sh\nexec {qemu} -L {} {} \\\"$@\\\"\n",
            quote(sysroot)?,
            quote(&sdk.join("bin/llvm-config"))?
        )
        .replace("\\\"", "\""),
    )?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        fs::set_permissions(&wrapper, fs::Permissions::from_mode(0o755))?;
    }
    let mut env = BTreeMap::from([
        ("WAVE_CROSS_LLVM_TARGET".into(), triple.clone()),
        ("WAVE_LLVM_HOME".into(), sdk.to_string_lossy().into()),
        ("WAVE_LLVM_BIN".into(), sdk.join("bin").to_string_lossy().into()),
        ("WAVE_LLVM_LIB".into(), sdk.join("lib").to_string_lossy().into()),
        ("WAVE_HOST_LLVM_MC".into(), host_bin.join("llvm-mc").to_string_lossy().into()),
        ("WAVE_LLVM_MC".into(), host_bin.join("llvm-mc").to_string_lossy().into()),
        ("LLVM_SYS_211_PREFIX".into(), sdk.to_string_lossy().into()),
        ("LLVM_CONFIG_PATH".into(), wrapper.to_string_lossy().into()),
        (
            format!("CARGO_TARGET_{}_LINKER", triple.replace('-', "_").to_uppercase()),
            format!("{cross_prefix}gcc"),
        ),
        (format!("CC_{}", triple.replace('-', "_")), format!("{cross_prefix}gcc")),
        (format!("CXX_{}", triple.replace('-', "_")), format!("{cross_prefix}g++")),
        ("QEMU_LD_PREFIX".into(), sysroot.to_string_lossy().into()),
        ("WAVE_CROSS_SYSROOT".into(), sysroot.to_string_lossy().into()),
    ]);
    if target == "linux-loong64" {
        env.insert("WAVE_LOONGARCH64_SYSROOT".into(), sysroot.to_string_lossy().into());
    }
    Spec::new([
        OsString::from("rustup"),
        "target".into(),
        "add".into(),
        "--toolchain".into(),
        config::pin("RUST_VERSION")?.into(),
        triple.into(),
    ])
    .checked()?;
    Ok(env)
}

pub fn main(arguments: &[OsString]) -> Result<i32> {
    let (command_name, rest) =
        arguments.split_first().ok_or_else(|| error("expected prerequisite command"))?;
    let mut args = Args::new(rest);
    let output =
        args.path("--github-env")?.or_else(|| std::env::var_os("GITHUB_ENV").map(PathBuf::from));
    let work = args.path("--work-dir")?.unwrap_or_else(|| {
        std::env::var_os("RUNNER_TEMP")
            .map(PathBuf::from)
            .unwrap_or_else(std::env::temp_dir)
            .join("wave-prerequisites")
    });
    let values = if command_name == "host" {
        let target = args.required("--target")?;
        let provision = args.flag("--provision");
        args.finish()?;
        host(&target, provision, &work)?
    } else if command_name == "loongarch" {
        let provision = args.flag("--provision");
        args.finish()?;
        loongarch(&work, provision)?
    } else if command_name == "cross" {
        let target = args.required("--target")?;
        let sysroot = args.path("--sysroot")?.ok_or_else(|| error("missing --sysroot"))?;
        let prefix = args.required("--cross-prefix")?;
        let host = args.path("--host-llvm-bin")?.ok_or_else(|| error("missing --host-llvm-bin"))?;
        args.finish()?;
        cross_environment(&target, &work, &sysroot, &prefix, &host)?
    } else if command_name == "tooling" {
        args.finish()?;
        BTreeMap::from([(
            "WAVE_TOOLS".into(),
            fs::canonicalize(std::env::current_exe()?)?.to_string_lossy().into(),
        )])
    } else if command_name == "std" {
        args.finish()?;
        fs::create_dir_all(&work)?;
        let old_home = std::env::var_os("HOME")
            .or_else(|| std::env::var_os("USERPROFILE"))
            .map(PathBuf::from)
            .ok_or_else(|| error("home path is unavailable"))?;
        let home = work.join("home");
        let destination = home.join(".wave/lib/wave/std");
        if storage::exists(&destination) {
            storage::remove(&destination)?;
        }
        storage::copy_tree(&crate::root().join("std"), &destination)?;
        BTreeMap::from([
            ("HOME".into(), home.to_string_lossy().into()),
            ("USERPROFILE".into(), home.to_string_lossy().into()),
            (
                "CARGO_HOME".into(),
                std::env::var("CARGO_HOME")
                    .unwrap_or_else(|_| old_home.join(".cargo").to_string_lossy().into()),
            ),
            (
                "RUSTUP_HOME".into(),
                std::env::var("RUSTUP_HOME")
                    .unwrap_or_else(|_| old_home.join(".rustup").to_string_lossy().into()),
            ),
        ])
    } else if command_name == "pins" {
        args.finish()?;
        let Value::Object(fields) =
            config::read_json(&crate::root().join("tools/ci/toolchains.json"))?
        else {
            return Err(error("pins must be an object"));
        };
        fields
            .into_iter()
            .map(|(key, value)| match value {
                Value::String(value) => Ok((key, value)),
                _ => Err(error("pin must be string")),
            })
            .collect::<Result<_>>()?
    } else {
        return Err(error("unknown prerequisite command"));
    };
    if let Some(path) = &output {
        storage::validate_output(path, &crate::runner::protected_inputs(&[])?)?;
    }
    export(&values, output.as_deref())?;
    Ok(0)
}
use utils::wson::Value;
