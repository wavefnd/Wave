// SPDX-License-Identifier: MPL-2.0
//! Pinned SDKs are validated as data before any bundled tool is executed.
use crate::{binary, cli::Args, config, error, package, process::Spec, storage, Result};
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::ffi::OsString;
use std::fs::{self, File};
use std::io::{Read, Write};
use std::path::{Component, Path, PathBuf};
use std::time::Duration;
use utils::wson::Value;

const BACKENDS: &[&str] = &["AArch64", "LoongArch", "RISCV", "WebAssembly", "X86"];
const TOOLS: &[&str] = &["llvm-config", "llc", "llvm-as", "llvm-mc", "lld", "ld.lld", "wasm-ld"];
const TARGETS: &[&str] = &["linux-riscv64", "linux-loong64"];
const MAX_EXPANDED: u64 = 16 * 1024 * 1024 * 1024;

pub fn identity(target: &str) -> Result<Value> {
    if !TARGETS.contains(&target) {
        return Err(error("unsupported LLVM SDK target"));
    }
    Ok(Value::object(vec![
        ("schema_version", config::number(1)),
        ("target", Value::String(target.into())),
        ("llvm_version", Value::String(config::pin("LLVM_SOURCE_VERSION")?)),
        ("source_sha256", Value::String(config::pin("LLVM_SOURCE_SHA256")?)),
        ("backends", Value::Array(BACKENDS.iter().map(|v| Value::String((*v).into())).collect())),
    ]))
}

fn require_identity(value: &Value, target: &str) -> Result<()> {
    let Value::Object(fields) = identity(target)? else { unreachable!() };
    for (name, expected) in fields {
        if !value.get(&name).is_some_and(|actual| config::equivalent(actual, &expected)) {
            return Err(error(format!("LLVM SDK identity mismatch: {name}")));
        }
    }
    Ok(())
}

/// Every intermediate hop must stay inside the root, including links in parents.
pub fn check_tree(root: &Path) -> Result<()> {
    check_tree_policy(root, false)
}

fn check_tree_policy(root: &Path, directories: bool) -> Result<()> {
    let root = fs::canonicalize(root)?;
    for path in storage::files(&root)? {
        if !fs::symlink_metadata(&path)?.is_symlink() {
            continue;
        }
        let mut pending: VecDeque<_> = path
            .strip_prefix(&root)?
            .components()
            .map(|part| part.as_os_str().to_owned())
            .collect();
        let mut current = root.clone();
        let mut seen = BTreeSet::new();
        while let Some(part) = pending.pop_front() {
            if part == "." || part.is_empty() {
                continue;
            }
            if part == ".." {
                if current == root {
                    return Err(error("LLVM link escapes bundle"));
                }
                current.pop();
                continue;
            }
            current.push(part);
            let info = fs::symlink_metadata(&current)?;
            if info.is_symlink() {
                if !seen.insert(current.clone()) {
                    return Err(error("cyclic LLVM bundle link"));
                }
                let target = fs::read_link(&current)?;
                if target.is_absolute() {
                    return Err(error("absolute LLVM bundle link"));
                }
                let components: Vec<_> =
                    target.components().map(|c| c.as_os_str().to_owned()).collect();
                for component in components.into_iter().rev() {
                    pending.push_front(component);
                }
                current.pop();
            } else if !pending.is_empty() && !info.is_dir() {
                return Err(error("LLVM link traverses a non-directory"));
            }
        }
        let target = fs::symlink_metadata(current)?;
        if !(target.is_file() || directories && target.is_dir()) {
            return Err(error("LLVM link must target a regular file"));
        }
    }
    Ok(())
}

pub fn check_layout(prefix: &Path, target: &str) -> Result<Value> {
    check_tree(prefix)?;
    let metadata = config::read_json(&prefix.join("bundle.json"))?;
    require_identity(&metadata, target)?;
    for name in TOOLS {
        let path = prefix.join("bin").join(name);
        binary::inspect(
            &path,
            Some(if target == "linux-riscv64" {
                "riscv64-unknown-linux-gnu"
            } else {
                "loongarch64-unknown-linux-gnu"
            }),
        )?;
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            if fs::metadata(&path)?.permissions().mode() & 0o111 == 0 {
                return Err(error(format!("LLVM tool is not executable: {name}")));
            }
        }
    }
    for name in ["llvm/Config/llvm-config.h", "llvm-c/Core.h"] {
        if !prefix.join("include").join(name).is_file() {
            return Err(error(format!("missing LLVM header: {name}")));
        }
    }
    let mut libraries = 0;
    for path in storage::files(&prefix.join("lib"))? {
        let name = path.file_name().unwrap().to_string_lossy();
        if name.starts_with("libLLVM") && name.contains(".so") {
            binary::inspect(
                &path,
                Some(if target == "linux-riscv64" {
                    "riscv64-unknown-linux-gnu"
                } else {
                    "loongarch64-unknown-linux-gnu"
                }),
            )?;
            libraries += 1;
        }
    }
    if libraries == 0 {
        return Err(error("missing shared LLVM library"));
    }
    Ok(metadata)
}

pub fn validate(prefix: &Path, target: &str, sysroot: &Path) -> Result<()> {
    check_layout(prefix, target)?;
    let prefix = fs::canonicalize(prefix)?;
    let qemu = if target == "linux-riscv64" { "qemu-riscv64" } else { "qemu-loongarch64" };
    let query = |name: &str, flags: &[&str]| -> Result<String> {
        let mut command = vec![
            OsString::from(qemu),
            "-L".into(),
            sysroot.into(),
            prefix.join("bin").join(name).into(),
        ];
        command.extend(flags.iter().map(OsString::from));
        Ok(Spec::new(command).checked()?.strict_stdout()?.trim().into())
    };
    if query("llvm-config", &["--version"])? != config::pin("LLVM_SOURCE_VERSION")? {
        return Err(error("LLVM bundle version mismatch"));
    }
    let backends = query("llvm-config", &["--targets-built"])?;
    if backends.split_whitespace().collect::<BTreeSet<_>>() != BACKENDS.iter().copied().collect() {
        return Err(error("LLVM bundle backend set mismatch"));
    }
    for (flag, suffix) in [("--prefix", ""), ("--includedir", "include"), ("--libdir", "lib")] {
        if fs::canonicalize(query("llvm-config", &[flag])?)?
            != fs::canonicalize(prefix.join(suffix))?
        {
            return Err(error(format!("LLVM bundle is not relocatable: {flag}")));
        }
    }
    let libraries = query("llvm-config", &["--link-shared", "--libnames"])?;
    if libraries.is_empty()
        || libraries.split_whitespace().any(|name| {
            Path::new(name).file_name() != Some(name.as_ref())
                || !prefix.join("lib").join(name).is_file()
        })
    {
        return Err(error("LLVM shared library list is invalid"));
    }
    if query("llvm-config", &["--link-shared", "--system-libs"])?
        .split_whitespace()
        .any(|v| v.contains("ffi"))
    {
        return Err(error("LLVM bundle unexpectedly depends on libffi"));
    }
    for name in ["llc", "llvm-as", "llvm-mc", "ld.lld", "wasm-ld"] {
        query(name, &["--version"])?;
    }
    Ok(())
}

fn relative(path: &Path) -> Result<PathBuf> {
    let mut output = PathBuf::new();
    for component in path.components() {
        match component {
            Component::Normal(value) if !value.to_string_lossy().contains([':', '\\']) => {
                output.push(value)
            },
            Component::CurDir => (),
            _ => return Err(error(format!("unsafe archive path: {}", path.display()))),
        }
    }
    if output.as_os_str().is_empty() {
        return Err(error("empty archive entry"));
    }
    Ok(output)
}

#[cfg(unix)]
fn link(target: &Path, path: &Path) -> Result<()> {
    std::os::unix::fs::symlink(target, path)?;
    Ok(())
}

#[cfg(windows)]
fn link(target: &Path, path: &Path) -> Result<()> {
    std::os::windows::fs::symlink_file(target, path)?;
    Ok(())
}

/// Extract regular files before creating links. Never unpack through a link.
/// Callers choose whether the format contract permits links at all.
pub fn extract(archive: &Path, destination: &Path, allow_links: bool) -> Result<()> {
    extract_policy(archive, destination, allow_links, false)
}

/// Cross-toolchain/source archives retain their contained directory aliases;
/// installed LLVM SDKs use the stricter regular-file-only link contract.
pub fn extract_toolchain(archive: &Path, destination: &Path) -> Result<()> {
    extract_policy(archive, destination, true, true)
}

fn extract_policy(
    archive: &Path,
    destination: &Path,
    allow_links: bool,
    directories: bool,
) -> Result<()> {
    if storage::exists(destination) {
        return Err(error("archive destination already exists"));
    }
    let parent = destination.parent().unwrap_or(Path::new("."));
    fs::create_dir_all(parent)?;
    let work = tempfile::Builder::new().prefix(".wave-extract-").tempdir_in(parent)?;
    let stage = work.path().join("tree");
    fs::create_dir(&stage)?;
    let mut seen = BTreeSet::new();
    let mut links = Vec::new();
    let mut expanded = 0u64;
    let mut put = |name: &Path, size: u64, mode: u32, input: &mut dyn Read| -> Result<()> {
        let name = relative(name)?;
        if !seen.insert(name.clone()) {
            return Err(error("duplicate archive entry"));
        }
        expanded = expanded.checked_add(size).ok_or_else(|| error("archive size overflow"))?;
        if expanded > MAX_EXPANDED {
            return Err(error("expanded archive exceeds size limit"));
        }
        let path = stage.join(name);
        fs::create_dir_all(path.parent().unwrap())?;
        let mut file = fs::OpenOptions::new().write(true).create_new(true).open(&path)?;
        if std::io::copy(&mut input.take(size), &mut file)? != size {
            return Err(error("truncated archive entry"));
        }
        file.flush()?;
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            fs::set_permissions(path, fs::Permissions::from_mode(mode & 0o777))?;
        }
        #[cfg(not(unix))]
        {
            let _ = mode;
        }
        Ok(())
    };
    let name = archive.to_string_lossy();
    if name.ends_with(".zip") {
        let mut zip = zip::ZipArchive::new(File::open(archive)?)?;
        for index in 0..zip.len() {
            let mut entry = zip.by_index(index)?;
            if entry.is_dir()
                && Path::new(entry.name()).components().all(|c| c == Component::CurDir)
            {
                continue;
            }
            let path = relative(Path::new(entry.name()))?;
            let mode = entry.unix_mode().unwrap_or(0o100644);
            if entry.is_dir() {
                fs::create_dir_all(stage.join(path))?;
            } else if mode & 0o170000 == 0o120000 {
                if !allow_links || entry.size() > 4096 {
                    return Err(error("archive symlink is not allowed"));
                }
                let mut target = String::new();
                entry.read_to_string(&mut target)?;
                links.push((path, PathBuf::from(target), false));
            } else if mode & 0o170000 == 0 || mode & 0o170000 == 0o100000 {
                put(&path, entry.size(), mode, &mut entry)?;
            } else {
                return Err(error("unsupported archive entry type"));
            }
        }
    } else {
        let reader: Box<dyn Read> = if name.ends_with(".tar.gz") || name.ends_with(".tgz") {
            Box::new(flate2::read::GzDecoder::new(File::open(archive)?))
        } else if name.ends_with(".tar.xz") || name.ends_with(".txz") {
            let compressed = work.path().join("input.tar.xz");
            fs::copy(archive, &compressed)?;
            Spec::new([
                OsString::from("xz"),
                "--decompress".into(),
                "--memlimit-decompress=1GiB".into(),
                compressed.into(),
            ])
            .checked()?;
            let unpacked = work.path().join("input.tar");
            if fs::metadata(&unpacked)?.len() > MAX_EXPANDED {
                return Err(error("expanded archive exceeds size limit"));
            }
            Box::new(File::open(unpacked)?)
        } else if name.ends_with(".tar") {
            Box::new(File::open(archive)?)
        } else {
            return Err(error("unsupported archive extension"));
        };
        let mut tar = tar::Archive::new(reader);
        for entry in tar.entries()? {
            let mut entry = entry?;
            let name = entry.path()?.into_owned();
            let kind = entry.header().entry_type();
            if kind.is_dir() && name.components().all(|c| c == Component::CurDir) {
                continue;
            }
            let path = relative(&name)?;
            if kind.is_dir() {
                fs::create_dir_all(stage.join(path))?;
            } else if kind.is_file() {
                put(&path, entry.size(), entry.header().mode()?, &mut entry)?;
            } else if kind.is_symlink() || kind.is_hard_link() {
                if !allow_links {
                    return Err(error("archive link is not allowed"));
                }
                let target = entry
                    .link_name()?
                    .ok_or_else(|| error("missing archive link target"))?
                    .into_owned();
                links.push((path, target, kind.is_hard_link()));
            } else {
                return Err(error("unsupported archive entry type"));
            }
        }
    }
    // Creating a link never grants permission to write through it.
    let link_names: BTreeSet<_> = links.iter().map(|(name, _, _)| name.clone()).collect();
    for (name, _, _) in &links {
        if name.ancestors().skip(1).any(|parent| link_names.contains(parent)) {
            return Err(error("archive link traverses another link"));
        }
    }
    for (name, target, hard) in &links {
        if !seen.insert(name.clone()) || storage::exists(&stage.join(name)) {
            return Err(error("duplicate archive link"));
        }
        if target.is_absolute() {
            return Err(error("absolute archive link"));
        }
        let path = stage.join(name);
        fs::create_dir_all(path.parent().unwrap())?;
        if *hard {
            let target = stage.join(relative(target)?);
            if fs::symlink_metadata(&target)?.is_symlink()
                || !target.is_file()
                || !fs::canonicalize(&target)?.starts_with(&stage)
            {
                return Err(error("hard link must target a regular archive file"));
            }
            fs::hard_link(target, path)?;
        } else {
            link(target, &path)?;
        }
    }
    if allow_links {
        check_tree_policy(&stage, directories)?;
    }
    fs::rename(stage, destination)?;
    Ok(())
}

pub fn download(url: &str, destination: &Path, expected: &str) -> Result<()> {
    if !url.starts_with("https://")
        || url[8..].split('/').next().is_none_or(|v| v.is_empty())
        || expected.len() != 64
        || !expected.bytes().all(|v| v.is_ascii_hexdigit() && !v.is_ascii_uppercase())
    {
        return Err(error("download requires HTTPS and a pinned SHA-256"));
    }
    let parent = destination.parent().unwrap_or(Path::new("."));
    fs::create_dir_all(parent)?;
    let temporary = tempfile::NamedTempFile::new_in(parent)?;
    let mut spec = Spec::new([
        OsString::from("curl"),
        "--fail".into(),
        "--location".into(),
        "--proto".into(),
        "=https".into(),
        "--proto-redir".into(),
        "=https".into(),
        "--retry".into(),
        "3".into(),
        "--output".into(),
        temporary.path().into(),
        url.into(),
    ]);
    spec.timeout = Duration::from_secs(600);
    spec.checked()?;
    if storage::digest(temporary.path())? != expected {
        return Err(error("download SHA-256 mismatch"));
    }
    temporary.persist(destination).map_err(|e| e.error)?;
    Ok(())
}

pub fn fetch(entry: &Value, destination: &Path, target: &str, sysroot: &Path) -> Result<()> {
    require_identity(entry, target)?;
    if storage::exists(destination) {
        return Err(error("bundle destination already exists"));
    }
    let parent = destination.parent().unwrap_or(Path::new("."));
    fs::create_dir_all(parent)?;
    let work = tempfile::Builder::new().prefix(".wave-sdk-").tempdir_in(parent)?;
    let archive = work.path().join("sdk.tar.xz");
    download(config::text(entry, "url")?, &archive, config::text(entry, "sha256")?)?;
    extract(&archive, &work.path().join("unpacked"), true)?;
    let sdk = work.path().join("unpacked/llvm");
    validate(&sdk, target, sysroot)?;
    fs::rename(sdk, destination)?;
    Ok(())
}

pub fn main(arguments: &[OsString]) -> Result<i32> {
    let (command, rest) = arguments.split_first().ok_or_else(|| error("expected SDK command"))?;
    let mut args = Args::new(rest);
    if command == "build" {
        if !args.flag("--provision") {
            return Err(error("building an LLVM SDK requires --provision"));
        }
        let target = args.required("--target")?;
        let output = args.path("--output")?.ok_or_else(|| error("missing --output"))?;
        let rev = args.text("--revision")?.unwrap_or_else(|| "r1".into());
        let host = args.path("--host-llvm-bin")?.ok_or_else(|| error("missing --host-llvm-bin"))?;
        let sysroot = args.path("--sysroot")?.ok_or_else(|| error("missing --sysroot"))?;
        let prefix = args.required("--cross-prefix")?;
        args.finish()?;
        build_bundle(&target, &output, &rev, &host, &sysroot, &prefix)?;
    } else if command == "publish" {
        let input = args.path("--input")?.ok_or_else(|| error("missing --input"))?;
        let destination =
            args.path("--destination")?.ok_or_else(|| error("missing --destination"))?;
        let manifest = args.path("--manifest")?.ok_or_else(|| error("missing --manifest"))?;
        args.finish()?;
        let resolved = storage::resolved(&manifest)?;
        if resolved.starts_with(storage::resolved(&input)?)
            || resolved.starts_with(storage::resolved(&destination)?)
        {
            return Err(error("pin manifest must be outside artifact and website data"));
        }
        let value = prepare_publication(&input, &destination)?;
        storage::atomic_write(&manifest, config::json(&value)?.as_bytes())?;
    } else if command == "check-tree" {
        let prefix = args.path("--prefix")?.ok_or_else(|| error("missing --prefix"))?;
        args.finish()?;
        check_tree(&prefix)?;
    } else if command == "extract" {
        let archive = args.path("--archive")?.ok_or_else(|| error("missing --archive"))?;
        let output = args.path("--output")?.ok_or_else(|| error("missing --output"))?;
        let links = args.flag("--allow-internal-links");
        args.finish()?;
        extract(&archive, &output, links)?;
    } else if command == "fetch" || command == "validate" {
        let target = args.required("--target")?;
        let prefix = args.path("--prefix")?.ok_or_else(|| error("missing --prefix"))?;
        let sysroot = args.path("--sysroot")?.ok_or_else(|| error("missing --sysroot"))?;
        args.finish()?;
        if command == "fetch" {
            let pins = config::read_json(&crate::root().join("tools/ci/llvm_bundles.json"))?;
            let entry = pins
                .get("bundles")
                .and_then(|v| v.get(&target))
                .ok_or_else(|| error("missing published LLVM SDK pin"))?;
            fetch(entry, &prefix, &target, &sysroot)?;
        } else {
            validate(&prefix, &target, &sysroot)?;
        }
    } else {
        return Err(error("unknown SDK command"));
    }
    Ok(0)
}

fn revision(value: &str) -> Result<()> {
    if value.strip_prefix('r').is_none_or(|v| {
        v.is_empty() || v.starts_with('0') || !v.bytes().all(|c| c.is_ascii_digit())
    }) {
        return Err(error("SDK revision must be r followed by a positive integer"));
    }
    Ok(())
}

pub fn cmake_options(
    target: &str,
    host_bin: &Path,
    cross_prefix: &str,
) -> Result<BTreeMap<String, String>> {
    identity(target)?;
    let manifest = crate::cases::Manifest::load(&crate::root(), false)?;
    let triple = manifest.target(target)?.rust_target()?;
    let mut options = BTreeMap::new();
    for (key, tool) in [
        ("CMAKE_C_COMPILER", "gcc"),
        ("CMAKE_CXX_COMPILER", "g++"),
        ("CMAKE_AR", "ar"),
        ("CMAKE_RANLIB", "ranlib"),
        ("CMAKE_STRIP", "strip"),
    ] {
        let name = format!("{cross_prefix}{tool}");
        let path =
            package::which(&name).ok_or_else(|| error(format!("missing cross tool: {name}")))?;
        options.insert(
            key.into(),
            fs::canonicalize(path)?
                .to_str()
                .ok_or_else(|| error("CMake tool path must be UTF-8"))?
                .into(),
        );
    }
    for (key, value) in [
        ("CMAKE_BUILD_TYPE", "Release"),
        ("CMAKE_SYSTEM_NAME", "Linux"),
        ("CMAKE_SYSTEM_PROCESSOR", triple.split('-').next().unwrap()),
        ("CMAKE_EXE_LINKER_FLAGS", "-static-libgcc -static-libstdc++"),
        ("CMAKE_SHARED_LINKER_FLAGS", "-static-libgcc -static-libstdc++"),
        ("CMAKE_INSTALL_PREFIX", "/opt/wave-llvm"),
        ("CMAKE_INSTALL_LIBDIR", "lib"),
        ("LLVM_LIBDIR_SUFFIX", ""),
        (
            "LLVM_NATIVE_TOOL_DIR",
            host_bin.to_str().ok_or_else(|| error("host LLVM path must be UTF-8"))?,
        ),
        ("LLVM_HOST_TRIPLE", &triple),
        ("LLVM_DEFAULT_TARGET_TRIPLE", &triple),
        ("LLVM_ENABLE_PROJECTS", "lld"),
        ("LLVM_BUILD_LLVM_DYLIB", "ON"),
        ("LLVM_LINK_LLVM_DYLIB", "ON"),
        ("LLVM_BUILD_TOOLS", "ON"),
        ("LLVM_INSTALL_TOOLCHAIN_ONLY", "OFF"),
    ] {
        options.insert(key.into(), value.into());
    }
    options.insert("LLVM_TARGETS_TO_BUILD".into(), BACKENDS.join(";"));
    for feature in [
        "INCLUDE_TESTS",
        "INCLUDE_EXAMPLES",
        "INCLUDE_BENCHMARKS",
        "INCLUDE_DOCS",
        "ENABLE_BINDINGS",
        "ENABLE_FFI",
        "ENABLE_LIBEDIT",
        "ENABLE_LIBXML2",
        "ENABLE_ZLIB",
        "ENABLE_ZSTD",
    ] {
        options.insert(format!("LLVM_{feature}"), "OFF".into());
    }
    Ok(options)
}

fn build_bundle(
    target: &str,
    output: &Path,
    rev: &str,
    host_bin: &Path,
    sysroot: &Path,
    cross_prefix: &str,
) -> Result<()> {
    revision(rev)?;
    let options = cmake_options(target, host_bin, cross_prefix)?;
    fs::create_dir_all(output)?;
    let work = tempfile::Builder::new().prefix(".wave-llvm-build-").tempdir_in(output)?;
    let version = config::pin("LLVM_SOURCE_VERSION")?;
    let source_name = format!("llvm-project-{version}.src");
    let archive = work.path().join(format!("{source_name}.tar.xz"));
    download(&format!("https://github.com/llvm/llvm-project/releases/download/llvmorg-{version}/{source_name}.tar.xz"),&archive,&config::pin("LLVM_SOURCE_SHA256")?)?;
    let source = work.path().join("sources");
    extract_toolchain(&archive, &source)?;
    let source = source.join(&source_name);
    let build = work.path().join("build");
    let mut args = vec![
        OsString::from("cmake"),
        "-S".into(),
        source.join("llvm").into(),
        "-B".into(),
        build.clone().into(),
        "-G".into(),
        "Ninja".into(),
    ];
    args.extend(options.iter().map(|(name, value)| format!("-D{name}={value}").into()));
    Spec::new(args).checked()?;
    let mut compile = Spec::new([
        OsString::from("cmake"),
        "--build".into(),
        build.clone().into(),
        "--parallel".into(),
        "2".into(),
        "--target".into(),
        "llvm-config".into(),
        "llc".into(),
        "llvm-as".into(),
        "llvm-mc".into(),
        "lld".into(),
        "llvm-headers".into(),
    ]);
    compile.timeout = Duration::from_secs(14400);
    compile.stream = true;
    compile.checked()?;
    let sdk = work.path().join("llvm");
    for component in ["LLVM", "llvm-headers", "llvm-config", "llc", "llvm-as", "llvm-mc", "lld"] {
        Spec::new([
            OsString::from("cmake"),
            "--install".into(),
            build.clone().into(),
            "--prefix".into(),
            sdk.clone().into(),
            "--strip".into(),
            "--component".into(),
            component.into(),
        ])
        .checked()?;
    }
    for name in ["ld.lld", "wasm-ld"] {
        let path = sdk.join("bin").join(name);
        if !storage::exists(&path) {
            link(Path::new("lld"), &path)?;
        }
    }
    fs::copy(source.join("llvm/LICENSE.TXT"), sdk.join("LICENSE.TXT"))?;
    storage::atomic_write(&sdk.join("bundle.json"), config::json(&identity(target)?)?.as_bytes())?;
    validate(&sdk, target, sysroot)?;
    let name = format!("wave-llvm-{version}-{target}-{rev}");
    let tar = work.path().join(format!("{name}.tar"));
    let mut builder = tar::Builder::new(File::create(&tar)?);
    builder.follow_symlinks(false);
    builder.append_dir_all("llvm", &sdk)?;
    builder.finish()?;
    drop(builder);
    Spec::new([OsString::from("xz"), "--threads=2".into(), tar.into()]).checked()?;
    let archive = work.path().join(format!("{name}.tar.xz"));
    let extracted = work.path().join("relocated");
    extract(&archive, &extracted, true)?;
    validate(&extracted.join("llvm"), target, sysroot)?;
    let mut entry = identity(target)?;
    let digest = storage::digest(&archive)?;
    for (key, value) in [
        ("revision", Value::string(rev)),
        ("filename", Value::string(archive.file_name().unwrap().to_string_lossy())),
        (
            "url",
            Value::string(format!(
                "https://wave-lang.dev/downloads/toolchains/llvm/{version}/{rev}/{name}.tar.xz"
            )),
        ),
        ("sha256", Value::string(&digest)),
        ("size_bytes", Value::integer(fs::metadata(&archive)?.len())),
        (
            "build",
            Value::object([
                (
                    "compiler",
                    Value::string(
                        Spec::new([format!("{cross_prefix}g++"), "--version".into()])
                            .checked()?
                            .strict_stdout()?
                            .trim(),
                    ),
                ),
                (
                    "options",
                    Value::object(options.iter().map(|(k, v)| (k.clone(), Value::string(v)))),
                ),
                (
                    "source_commit",
                    Value::string(package::run(["git", "rev-parse", "HEAD"])?.trim()),
                ),
            ]),
        ),
    ] {
        config::field(&mut entry, key, value)?;
    }
    if let (Ok(repo), Ok(run)) =
        (std::env::var("GITHUB_REPOSITORY"), std::env::var("GITHUB_RUN_ID"))
    {
        let mut build = entry.get("build").unwrap().clone();
        config::field(
            &mut build,
            "ci_run_url",
            Value::string(format!("https://github.com/{repo}/actions/runs/{run}")),
        )?;
        config::field(&mut entry, "build", build)?;
    }
    let metadata = work.path().join(format!("{name}.json"));
    let checksum = work.path().join(format!("{name}.tar.xz.sha256"));
    fs::write(&metadata, config::json(&entry)?)?;
    fs::write(&checksum, format!("{digest}  {name}.tar.xz\n"))?;
    let entries = [archive, metadata, checksum]
        .into_iter()
        .map(|source| {
            let destination = output.join(source.file_name().unwrap());
            (source, destination)
        })
        .collect::<Vec<_>>();
    for (_, destination) in &entries {
        if storage::exists(destination) {
            return Err(error("SDK output already exists; use a new revision"));
        }
    }
    storage::replace_set(&entries)?;
    Ok(())
}

/// A generation becomes visible only once both pinned architectures verify.
pub fn prepare_publication(inputs: &Path, destination: &Path) -> Result<Value> {
    let mut records = BTreeMap::new();
    let mut generation = None;
    for path in storage::files(inputs)?.into_iter().filter(|p| {
        p.file_name().is_some_and(|v| v.to_string_lossy().starts_with("wave-llvm-"))
            && p.extension().is_some_and(|v| v == "json")
    }) {
        let entry = config::read_json(&path)?;
        let target = config::text(&entry, "target")?;
        require_identity(&entry, target)?;
        let rev = config::text(&entry, "revision")?;
        revision(rev)?;
        let version = config::text(&entry, "llvm_version")?;
        let name = format!("wave-llvm-{version}-{target}-{rev}.tar.xz");
        let relative = Path::new("llvm").join(version).join(rev);
        let url = format!(
            "https://wave-lang.dev/downloads/toolchains/{}/{}",
            relative.to_string_lossy().replace('\\', "/"),
            name
        );
        if entry.get_str("filename") != Some(&name) || entry.get_str("url") != Some(&url) {
            return Err(error("unexpected SDK filename or URL"));
        }
        if generation.as_ref().is_some_and(|v| v != &relative) {
            return Err(error("SDKs must have the same version and revision"));
        }
        generation = Some(relative);
        let archive = path.with_file_name(&name);
        let checksum = path.with_file_name(format!("{name}.sha256"));
        let digest = storage::digest(&archive)?;
        if entry.get_str("sha256") != Some(&digest)
            || entry.get_u64("size_bytes") != Some(fs::metadata(&archive)?.len())
            || fs::read_to_string(&checksum)?.trim() != format!("{digest}  {name}")
        {
            return Err(error("SDK checksum or size mismatch"));
        }
        if records.insert(target.to_string(), (entry, vec![archive, checksum, path])).is_some() {
            return Err(error("duplicate SDK architecture"));
        }
    }
    if records.keys().map(String::as_str).collect::<BTreeSet<_>>()
        != TARGETS.iter().copied().collect()
    {
        return Err(error("publication requires exactly both SDK architectures"));
    }
    fs::create_dir_all(destination)?;
    let lock = fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(destination.join(".publication.lock"))?;
    lock.try_lock().map_err(|_| error("another SDK publication owns this destination"))?;
    let catalog_path = destination.join("index.json");
    let mut catalog = if catalog_path.exists() {
        config::read_json(&catalog_path)?
    } else {
        Value::object([("schema_version", Value::integer(1)), ("bundles", Value::Array(vec![]))])
    };
    if catalog.get_u64("schema_version") != Some(1) {
        return Err(error("unsupported SDK catalog"));
    }
    let mut old = BTreeMap::new();
    for entry in config::array(&catalog, "bundles")? {
        if old.insert(config::text(entry, "url")?.to_string(), entry.clone()).is_some() {
            return Err(error("duplicate SDK catalog URL"));
        }
    }
    let generation = generation.unwrap();
    let final_path = destination.join(&generation);
    fs::create_dir_all(final_path.parent().unwrap())?;
    let temporary =
        tempfile::Builder::new().prefix(".toolchains-publish-").tempdir_in(destination)?;
    let stage = temporary.path().join("generation");
    fs::create_dir(&stage)?;
    let published =
        time::OffsetDateTime::now_utc().format(&time::format_description::well_known::Rfc3339)?;
    for (entry, files) in records.values() {
        let url = config::text(entry, "url")?;
        if let Some(previous) = old.get(url) {
            if previous.get_str("sha256") != entry.get_str("sha256")
                || previous.get_u64("size_bytes") != entry.get_u64("size_bytes")
            {
                return Err(error("published SDK identity is immutable; use a new revision"));
            }
        }
        for source in files {
            let name = source.file_name().unwrap();
            if storage::exists(&final_path)
                && (!final_path.join(name).is_file()
                    || storage::digest(&final_path.join(name))? != storage::digest(source)?)
            {
                return Err(error("published SDK files are immutable; use a new revision"));
            }
            fs::copy(source, stage.join(name))?;
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                fs::set_permissions(stage.join(name), fs::Permissions::from_mode(0o644))?;
            }
        }
        let mut entry = entry.clone();
        let timestamp =
            old.get(url).and_then(|e| e.get_str("published_at")).unwrap_or(&published).to_string();
        config::field(&mut entry, "published_at", Value::string(timestamp))?;
        old.insert(url.into(), entry);
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        for path in [
            destination,
            destination.join("llvm").as_path(),
            final_path.parent().unwrap(),
            stage.as_path(),
        ] {
            fs::set_permissions(path, fs::Permissions::from_mode(0o755))?;
        }
    }
    if !storage::exists(&final_path) {
        fs::rename(stage, &final_path)?;
    }
    let mut bundles = old.into_values().collect::<Vec<_>>();
    bundles.sort_by(|a, b| {
        (b.get_str("published_at"), b.get_str("target"))
            .cmp(&(a.get_str("published_at"), a.get_str("target")))
    });
    config::field(&mut catalog, "bundles", Value::Array(bundles))?;
    storage::atomic_write(&catalog_path, config::json(&catalog)?.as_bytes())?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        fs::set_permissions(&catalog_path, fs::Permissions::from_mode(0o644))?;
    }
    Ok(Value::object([
        ("schema_version", Value::integer(1)),
        ("bundles", Value::object(records.into_iter().map(|(target, (entry, _))| (target, entry)))),
    ]))
}
