// SPDX-License-Identifier: MPL-2.0
//! Committed std inputs, complete runtime staging and recoverable activation.
use crate::{binary, cases, cli::Args, config, error, process::Spec, storage, Result};
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::ffi::{OsStr, OsString};
use std::fs;
use std::path::{Path, PathBuf};
use utils::wson::Value;

pub fn which(name: &str) -> Option<PathBuf> {
    let raw = Path::new(name);
    if raw.is_absolute() || raw.components().count() > 1 {
        // For direct paths, ensure it's a file and meets executable requirements.
        if raw.is_file() {
            #[cfg(unix)]
            {
                use std::os::unix::fs::MetadataExt;
                if let Ok(metadata) = std::fs::metadata(raw) {
                    if metadata.mode() & 0o111 != 0 {
                        return Some(raw.into());
                    }
                }
                return None;
            }
            #[cfg(not(unix))]
            {
                return Some(raw.into());
            }
        }
        return None;
    }
    for directory in std::env::split_paths(&std::env::var_os("PATH").unwrap_or_default()) {
        for suffix in if cfg!(windows) { vec!["", ".exe", ".cmd", ".bat"] } else { vec![""] } {
            let path = directory.join(format!("{name}{suffix}"));
            if path.is_file() {
                #[cfg(unix)]
                {
                    use std::os::unix::fs::MetadataExt;
                    if let Ok(metadata) = std::fs::metadata(&path) {
                        // Checks if any execute bit is set (owner, group, or others)
                        if metadata.mode() & 0o111 != 0 {
                            return Some(path);
                        }
                    }
                }
                #[cfg(not(unix))]
                {
                    return Some(path);
                }
            }
        }
    }
    None
}

pub fn run(arguments: impl IntoIterator<Item = impl Into<OsString>>) -> Result<String> {
    Ok(Spec::new(arguments).checked()?.text_stdout())
}

pub fn prefix() -> Result<PathBuf> {
    for name in ["WAVE_LLVM_HOME", "LLVM_SYS_211_PREFIX"] {
        if let Some(path) = std::env::var_os(name) {
            let path = PathBuf::from(path);
            if path.is_dir() {
                return Ok(path);
            }
        }
    }
    let config = std::env::var_os("LLVM_CONFIG_PATH")
        .map(PathBuf::from)
        .or_else(|| which("llvm-config"))
        .ok_or_else(|| error("LLVM SDK is required"))?;
    let result = run([config.as_os_str(), OsStr::new("--prefix")])?;
    let path = PathBuf::from(result.trim());
    if !path.is_dir() {
        return Err(error("LLVM prefix is unavailable"));
    }
    Ok(path)
}

pub fn tool(name: &str, target: &str) -> Result<PathBuf> {
    let filename = format!("{name}{}", if target.contains("windows") { ".exe" } else { "" });
    let mut directories = Vec::new();
    for variable in ["WAVE_WINDOWS_LLVM_BIN", "WAVE_LLVM_BIN"] {
        if let Some(value) = std::env::var_os(variable) {
            directories.push(PathBuf::from(value));
        }
    }
    if let Ok(prefix) = prefix() {
        directories.push(prefix.join("bin"));
    }
    if let Some(path) = which(&filename) {
        directories.push(path.parent().unwrap().into());
    }
    for directory in directories {
        let path = directory.join(&filename);
        if path.is_file() {
            binary::inspect(&path, Some(target))?;
            return Ok(path);
        }
    }
    Err(error(format!("required {target} LLVM tool is missing: {filename}")))
}

pub fn contract(target: &str) -> Value {
    let windows = target.contains("windows");
    let linux = target.contains("linux");
    let freebsd = target.contains("freebsd");
    let mut paths = Value::object([
        ("compiler", Value::string(if windows { "wavec.exe" } else { "wavec" })),
        ("std", Value::string("std")),
        ("tools", Value::string("llvm/bin")),
    ]);
    if linux {
        config::field(&mut paths, "crt", Value::string("crt")).unwrap();
    }
    if windows {
        config::field(&mut paths, "builtins", Value::string("llvm/lib/clang/21/lib/windows"))
            .unwrap();
    }
    Value::object([
        (
            "abi",
            Value::string(if windows {
                "msvc"
            } else if linux {
                "gnu"
            } else if freebsd {
                "freebsd"
            } else {
                "darwin"
            }),
        ),
        ("paths", paths),
        (
            "external_prerequisites",
            Value::strings(if windows {
                vec!["Windows SDK", "MSVC/UCRT libraries", "Visual C++ runtime"]
            } else if linux {
                vec!["compatible target glibc and system libraries"]
            } else if freebsd {
                vec!["FreeBSD 14.4 or newer compatible base system"]
            } else {
                vec!["Apple SDK and system libraries"]
            }),
        ),
    ])
}

pub fn identity(archive: &Path, target: &str, version: &str) -> Result<Value> {
    let root = crate::root();
    let source = run(["git", "rev-parse", "HEAD"])?;
    let source = source.trim();
    if source.len() != 40 || !source.bytes().all(|b| b.is_ascii_hexdigit()) {
        return Err(error("invalid package source SHA"));
    }
    let revision = config::read_json(&root.join("std/manifest.json"))?
        .get_u64("compatibility_revision")
        .ok_or_else(|| error("invalid std compatibility revision"))?;
    let mut value = contract(target);
    for (key, data) in [
        ("schema_version", Value::integer(1)),
        ("compiler_version", Value::string(version)),
        ("source_sha", Value::string(source)),
        ("std_compatibility_revision", Value::integer(revision)),
        ("target", Value::string(target)),
        ("archive", Value::string(archive.file_name().unwrap().to_string_lossy())),
        ("sha256", Value::string(storage::digest(archive)?)),
    ] {
        config::field(&mut value, key, data)?;
    }
    Ok(value)
}

pub fn committed_std(root: &Path, destination: &Path) -> Result<()> {
    let mut listing = Spec::new(["git", "ls-tree", "-r", "-z", "HEAD", "--", "std"]);
    listing.cwd = root.into();
    let listing = listing.checked()?.stdout;
    let mut entries = Vec::new();
    for record in listing.split(|b| *b == 0).filter(|s| !s.is_empty()) {
        let (metadata, name) = record.split_at(
            record
                .iter()
                .position(|b| *b == b'\t')
                .ok_or_else(|| error("malformed Git std listing"))?,
        );
        let name = std::str::from_utf8(&name[1..])?;
        let fields: Vec<_> = std::str::from_utf8(metadata)?.split_whitespace().collect();
        if fields.len() != 3 || !["100644", "100755"].contains(&fields[0]) || fields[1] != "blob" {
            return Err(error(format!("std input is not a regular committed blob: {name}")));
        }
        let relative = cases::relative(name)?;
        if relative.components().next().is_none_or(|v| v.as_os_str() != "std") {
            return Err(error("std input path is outside std"));
        }
        let mut current = root.to_owned();
        for component in relative.components() {
            current.push(component.as_os_str());
            if fs::symlink_metadata(&current)?.is_symlink() {
                return Err(error(format!("std input symlink: {name}")));
            }
        }
        if !current.is_file() {
            return Err(error(format!("missing tracked std file: {name}")));
        }
        entries.push((relative, fields[0].to_owned(), fields[2].to_owned()));
    }
    if entries.is_empty() {
        return Err(error("no committed std inputs"));
    }
    let mut check = Spec::new(["git", "diff", "--quiet", "--no-ext-diff", "HEAD", "--", "std"]);
    check.cwd = root.into();
    if check.run()?.status != 0 {
        return Err(error("commit or restore std modifications before packaging"));
    }
    let mut query = Spec::new(["git", "cat-file", "--batch"]);
    query.cwd = root.into();
    query.input =
        Some(entries.iter().map(|(_, _, oid)| format!("{oid}\n")).collect::<String>().into_bytes());
    let data = query.checked()?.stdout;
    let mut offset = 0;
    for (relative, mode, oid) in entries {
        let end = data[offset..]
            .iter()
            .position(|b| *b == b'\n')
            .ok_or_else(|| error("truncated Git blob header"))?
            + offset;
        let header = std::str::from_utf8(&data[offset..end])?;
        let fields: Vec<_> = header.split_whitespace().collect();
        if fields.len() != 3 || fields[0] != oid || fields[1] != "blob" {
            return Err(error("unexpected Git object in std inputs"));
        }
        let length = fields[2].parse::<usize>()?;
        offset = end + 1;
        let end = offset.checked_add(length).ok_or_else(|| error("blob size overflow"))?;
        let bytes = data.get(offset..end).ok_or_else(|| error("truncated Git blob"))?;
        if data.get(end) != Some(&b'\n') {
            return Err(error("missing Git blob separator"));
        }
        let output = destination.join(relative.strip_prefix("std")?);
        fs::create_dir_all(output.parent().unwrap())?;
        fs::write(&output, bytes)?;
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            fs::set_permissions(
                &output,
                fs::Permissions::from_mode(if mode == "100755" { 0o755 } else { 0o644 }),
            )?;
        }
        #[cfg(not(unix))]
        let _ = mode;
        offset = end + 1;
    }
    if offset != data.len() {
        return Err(error("trailing Git std input data"));
    }
    Ok(())
}

fn executable(source: &Path, destination: &Path) -> Result<()> {
    if let Some(parent) = destination.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::copy(source, destination)?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mode = fs::metadata(destination)?.permissions().mode();
        fs::set_permissions(destination, fs::Permissions::from_mode(mode | 0o755))?;
    }
    Ok(())
}

pub fn linux_crt(stage: &Path, target: &str) -> Result<()> {
    if !target.contains("linux") {
        return Ok(());
    }
    let mc = std::env::var_os("WAVE_HOST_LLVM_MC")
        .map(PathBuf::from)
        .map(Ok)
        .unwrap_or_else(|| tool("llvm-mc", target))?;
    for (triple, architecture, abi, features) in [
        ("x86_64-unknown-linux-gnu", "x86_64", None, None),
        ("aarch64-unknown-linux-gnu", "aarch64", None, None),
        ("riscv64-unknown-linux-gnu", "riscv64", Some("lp64"), Some("+m,+a,+c,+zicsr,+zifencei")),
        (
            "riscv64-unknown-linux-gnu",
            "riscv64",
            Some("lp64f"),
            Some("+m,+a,+f,+c,+zicsr,+zifencei"),
        ),
        (
            "riscv64-unknown-linux-gnu",
            "riscv64",
            Some("lp64d"),
            Some("+m,+a,+f,+d,+c,+zicsr,+zifencei"),
        ),
        ("loongarch64-unknown-linux-gnu", "loongarch64", Some("lp64s"), Some("-f,-d,+lsx,+ual")),
        ("loongarch64-unknown-linux-gnu", "loongarch64", Some("lp64f"), Some("+f,-d,+lsx,+ual")),
        ("loongarch64-unknown-linux-gnu", "loongarch64", Some("lp64d"), Some("+f,+d,+lsx,+ual")),
    ] {
        let mut destination = stage.join("crt").join(triple);
        if let Some(abi) = abi {
            destination.push(abi);
        }
        fs::create_dir_all(&destination)?;
        let effective = if architecture == "loongarch64" {
            match abi {
                Some("lp64s") => "loongarch64-unknown-linux-gnusf",
                Some("lp64f") => "loongarch64-unknown-linux-gnuf32",
                _ => triple,
            }
        } else {
            triple
        };
        for (name, source) in [
            ("crt1.o", "crt1.s"),
            ("Scrt1.o", "crt1.s"),
            ("rcrt1.o", "crt1.s"),
            ("crti.o", "crti.s"),
            ("crtn.o", "crtn.s"),
        ] {
            let output = destination.join(name);
            let mut args = vec![
                mc.clone().into_os_string(),
                format!("-triple={effective}").into(),
                "-filetype=obj".into(),
            ];
            if let Some(features) = features {
                args.push(format!("-mattr={features}").into());
            }
            if architecture != "loongarch64" {
                if let Some(abi) = abi {
                    args.push(format!("-target-abi={abi}").into());
                }
            }
            args.extend([
                crate::root()
                    .join("llvm/crt/linux")
                    .join(architecture)
                    .join(source)
                    .into_os_string(),
                "-o".into(),
                output.clone().into_os_string(),
            ]);
            let mut spec = Spec::new(args);
            if let Some(lib) =
                mc.parent().and_then(Path::parent).map(|p| p.join("lib")).filter(|p| p.is_dir())
            {
                spec.env.insert("LD_LIBRARY_PATH".into(), lib.into_os_string());
            }
            spec.checked()?;
            binary::inspect(&output, Some(triple))?;
        }
    }
    Ok(())
}

fn library_directories(source: &Path, sdk: &Path) -> Vec<PathBuf> {
    let mut directories =
        vec![source.parent().unwrap().into(), sdk.join("lib"), sdk.join("lib64"), sdk.join("bin")];
    for name in ["WAVE_LLVM_LIB", "WAVE_WINDOWS_LLVM_BIN", "LD_LIBRARY_PATH", "DYLD_LIBRARY_PATH"] {
        if let Some(value) = std::env::var_os(name) {
            directories.extend(std::env::split_paths(&value));
        }
    }
    if let Some(sysroot) = std::env::var_os("WAVE_CROSS_SYSROOT") {
        let sysroot = PathBuf::from(sysroot);
        for directory in ["lib", "lib64", "usr/lib", "usr/lib64"] {
            directories.push(sysroot.join(directory));
        }
    } else {
        for directory in [
            "/lib",
            "/lib64",
            "/usr/lib",
            "/usr/lib64",
            "/usr/local/lib",
            "/lib/x86_64-linux-gnu",
            "/usr/lib/x86_64-linux-gnu",
            "/lib/aarch64-linux-gnu",
            "/usr/lib/aarch64-linux-gnu",
        ] {
            directories.push(PathBuf::from(directory));
        }
    }
    directories
}

fn resolve_library(
    name: &str,
    source: &Path,
    info: &binary::Info,
    sdk: &Path,
    target: &str,
) -> Result<PathBuf> {
    let mut candidates = Vec::new();
    if Path::new(name).is_absolute() {
        candidates.push(PathBuf::from(name));
    }
    for prefix in ["@loader_path/", "@executable_path/"] {
        if let Some(suffix) = name.strip_prefix(prefix) {
            candidates.push(source.parent().unwrap().join(suffix));
        }
    }
    if let Some(suffix) = name.strip_prefix("@rpath/") {
        for rpath in &info.rpaths {
            let rpath = rpath
                .replace("@loader_path", &source.parent().unwrap().to_string_lossy())
                .replace("@executable_path", &source.parent().unwrap().to_string_lossy());
            candidates.push(PathBuf::from(rpath).join(suffix));
        }
    }
    for directory in library_directories(source, sdk) {
        candidates
            .push(directory.join(
                Path::new(name).file_name().ok_or_else(|| error("invalid dependency name"))?,
            ));
    }
    for rpath in &info.rpaths {
        let directory = rpath
            .replace("${ORIGIN}", &source.parent().unwrap().to_string_lossy())
            .replace("$ORIGIN", &source.parent().unwrap().to_string_lossy());
        if !directory.contains('$') {
            candidates.push(PathBuf::from(directory).join(name));
        }
    }
    if cfg!(windows) {
        for directory in library_directories(source, sdk) {
            if directory.is_dir() {
                for entry in fs::read_dir(directory)? {
                    let entry = entry?;
                    if entry.file_name().to_string_lossy().eq_ignore_ascii_case(name) {
                        candidates.push(entry.path());
                    }
                }
            }
        }
    }
    let mut rejected = Vec::new();
    for path in candidates.into_iter().filter(|path| path.is_file()) {
        match binary::inspect(&path, Some(target)) {
            Ok(_) => return Ok(path),
            Err(e) => rejected.push(e.to_string()),
        }
    }
    Err(error(format!(
        "unresolved dependency {name} required by {}{}",
        source.display(),
        if rejected.is_empty() {
            String::new()
        } else {
            format!("; rejected candidates: {}", rejected.join("; "))
        }
    )))
}

pub fn runtime_closure(
    stage: &Path,
    target: &str,
    sdk: &Path,
    roots: &[(PathBuf, PathBuf)],
) -> Result<Vec<PathBuf>> {
    if target.contains("windows") {
        let binaries = roots.iter().map(|(source, _)| source.clone()).collect::<Vec<_>>();
        let mut search = library_directories(&binaries[0], sdk);
        search.extend(roots.iter().filter_map(|(source, _)| source.parent().map(Path::to_owned)));
        let system = PathBuf::from(
            std::env::var_os("SystemRoot").ok_or_else(|| error("missing Windows SystemRoot"))?,
        )
        .join("System32");
        let (payloads, prerequisites) =
            crate::windows::dependency_closure(&binaries, target, &search, &[system])?;
        let mut copied = Vec::new();
        for source in payloads.values() {
            for folder in [stage.to_path_buf(), stage.join("llvm/bin")] {
                fs::create_dir_all(&folder)?;
                let output = folder.join(source.file_name().unwrap());
                fs::copy(source, &output)?;
                copied.push(output);
            }
        }
        storage::atomic_write(
            &stage.join("windows-dependencies.json"),
            config::json(&Value::object([
                ("target", Value::string(target)),
                (
                    "payloads",
                    Value::object(
                        payloads.iter().map(|(name, path)| {
                            (name.clone(), Value::string(path.to_string_lossy()))
                        }),
                    ),
                ),
                ("vc_runtime_prerequisites", Value::strings(prerequisites.keys())),
            ]))?
            .as_bytes(),
        )?;
        return Ok(copied);
    }
    let mut pending: VecDeque<_> = roots.iter().cloned().collect();
    let mut visited = BTreeSet::new();
    let mut names: BTreeMap<String, PathBuf> = BTreeMap::new();
    let mut copied = Vec::new();
    let library_dir = stage.join(if target.contains("windows") { "llvm/bin" } else { "llvm/lib" });
    fs::create_dir_all(&library_dir)?;
    while let Some((source, staged)) = pending.pop_front() {
        if !visited.insert((fs::canonicalize(&source)?, staged.clone())) {
            continue;
        }
        let info = binary::inspect(&source, Some(target))?;
        for name in &info.needed {
            if binary::system_library(name, target) {
                continue;
            }
            let vc=target.contains("windows")&&regex_lite::Regex::new(r"(?i)^(?:vcruntime140(?:_1)?|msvcp140(?:_[0-9]+|_atomic_wait|_codecvt_ids)?|concrt140)\.dll$")?.is_match(name);
            if vc {
                let system = std::env::var_os("SystemRoot")
                    .ok_or_else(|| error("missing Windows SystemRoot"))?;
                let path = PathBuf::from(system).join("System32").join(name);
                binary::inspect(&path, Some(target))?;
                continue;
            }
            let dependency = resolve_library(name, &source, &info, sdk, target)?;
            binary::inspect(&dependency, Some(target))?;
            if target.contains("freebsd")
                && freebsd_base_library(
                    &dependency,
                    &std::env::var_os("WAVE_CROSS_SYSROOT")
                        .map(PathBuf::from)
                        .unwrap_or_else(|| PathBuf::from("/")),
                )
            {
                continue;
            }

            let basename = dependency.file_name().unwrap().to_string_lossy().to_string();
            let key =
                if target.contains("windows") { basename.to_lowercase() } else { basename.clone() };
            if let Some(previous) = names.get(&key) {
                if storage::digest(previous)? != storage::digest(&dependency)? {
                    return Err(error(format!("conflicting runtime library basename: {basename}")));
                }
            }
            let destination = library_dir.join(&basename);
            if !destination.exists() {
                fs::copy(&dependency, &destination)?;
                copied.push(destination.clone());
                names.insert(key, dependency.clone());
                pending.push_back((dependency.clone(), destination.clone()));
            }
            if target.contains("windows") {
                fs::copy(&dependency, stage.join(&basename))?;
            }
            if target.contains("darwin") {
                let prefix = if staged.parent() == Some(library_dir.as_path()) {
                    "@loader_path"
                } else if staged.parent() == Some(stage) {
                    "@executable_path/llvm/lib"
                } else {
                    "@executable_path/../lib"
                };
                run([
                    OsString::from("install_name_tool"),
                    "-change".into(),
                    name.into(),
                    format!("{prefix}/{basename}").into(),
                    staged.clone().into_os_string(),
                ])?;
            }
        }
    }
    Ok(copied)
}

fn freebsd_base_library(path: &Path, root: &Path) -> bool {
    [root.join("lib"), root.join("usr/lib")].iter().any(|base| path.starts_with(base))
}

pub fn require_rpath(path: &Path, expected: &str) -> Result<()> {
    let info = binary::inspect(path, None)?;
    if info.format != "Elf" || !info.rpaths.contains(expected) {
        return Err(error(format!(
            "{}: missing exact runtime search path {expected}",
            path.display()
        )));
    }
    Ok(())
}

pub fn patch_runtime(stage: &Path, target: &str, roots: &[(PathBuf, PathBuf)]) -> Result<()> {
    if target.contains("darwin") {
        for library in storage::files(&stage.join("llvm/lib"))? {
            if library.extension().is_some_and(|s| s == "dylib") {
                run([
                    OsString::from("install_name_tool"),
                    "-id".into(),
                    format!("@loader_path/{}", library.file_name().unwrap().to_string_lossy())
                        .into(),
                    library.clone().into_os_string(),
                ])?;
            }
        }
        for path in
            roots.iter().map(|(_, p)| p.clone()).chain(storage::files(&stage.join("llvm/lib"))?)
        {
            run([
                OsString::from("codesign"),
                "--force".into(),
                "--sign".into(),
                "-".into(),
                path.into_os_string(),
            ])?;
        }
    } else if target.contains("linux") || target.contains("freebsd") {
        for (index, (_, path)) in roots.iter().enumerate() {
            let expected = if index == 0 { "$ORIGIN/llvm/lib" } else { "$ORIGIN/../lib" };
            if let Some(patchelf) = which("patchelf") {
                run([
                    patchelf.into_os_string(),
                    "--force-rpath".into(),
                    "--set-rpath".into(),
                    expected.into(),
                    path.clone().into_os_string(),
                ])?;
            }
            require_rpath(path, expected)?;
        }
    }
    Ok(())
}

pub fn stage(binary: &Path, target: &str, directory: &Path) -> Result<()> {
    let root = crate::root();
    let sdk = prefix()?;
    binary::inspect(binary, Some(target))?;
    fs::create_dir_all(directory)?;
    committed_std(&root, &directory.join("std"))?;
    let compiler = directory.join(if target.contains("windows") { "wavec.exe" } else { "wavec" });
    executable(binary, &compiler)?;
    let mut roots = vec![(binary.into(), compiler)];
    let names = if target.contains("windows") {
        vec!["ld.lld", "lld-link", "llvm-readobj", "llc", "llvm-as", "llvm-mc"]
    } else if target.contains("darwin") {
        vec!["ld64.lld", "ld.lld", "llc", "llvm-as", "llvm-mc"]
    } else {
        vec!["ld.lld", "llc", "llvm-as", "llvm-mc"]
    };
    for name in names {
        let source = tool(name, target)?;
        let destination = directory
            .join("llvm/bin")
            .join(format!("{name}{}", if target.contains("windows") { ".exe" } else { "" }));
        executable(&source, &destination)?;
        roots.push((source, destination));
    }
    linux_crt(directory, target)?;
    let libraries = runtime_closure(directory, target, &sdk, &roots)?;
    if !target.contains("windows") && libraries.is_empty() {
        return Err(error("LLVM runtime dependency closure is empty"));
    }
    let licenses = directory.join("licenses");
    fs::create_dir_all(&licenses)?;
    fs::copy(root.join("LICENSE"), licenses.join("Wave.txt"))?;
    fs::copy(directory.join("std/LICENSE"), licenses.join("std.txt"))?;
    if target.contains("windows") {
        let builtins = PathBuf::from(format!(
            "lib/clang/21/lib/windows/clang_rt.builtins-{}.lib",
            binary::target_arch(target)
        ));
        let destination = directory.join("llvm").join(&builtins);
        fs::create_dir_all(destination.parent().unwrap())?;
        binary::inspect(&sdk.join(&builtins), Some(target))?;
        fs::copy(sdk.join(builtins), destination)?;
        for (component, name) in [("llvm", "LLVM"), ("compiler-rt", "compiler-rt")] {
            fs::copy(
                sdk.join("wave-notices").join(format!("{component}.txt")),
                licenses.join(format!("{name}.txt")),
            )?;
        }
        fs::copy(
            std::env::var_os("WAVE_LIBXML2_LICENSE")
                .ok_or_else(|| error("missing libxml2 license"))?,
            licenses.join("libxml2.txt"),
        )?;
        fs::copy(root.join("README.md"), directory.join("README.md"))?;
    }
    let command = if target.contains("windows") { r".\wavec.exe" } else { "./wavec" };
    let requirements = contract(target)
        .get_arr("external_prerequisites")
        .unwrap()
        .iter()
        .map(|v| match v {
            Value::String(v) => v.as_str(),
            _ => unreachable!(),
        })
        .collect::<Vec<_>>()
        .join(", ");
    fs::write(directory.join("INSTALL.txt"),format!("Wave manual installation ({target})\n\nExtract the entire archive into one directory. Keep the compiler, std/,\nllvm/, licenses/ and any crt/ directory together. Moving only the compiler\nbreaks bundled dependency discovery. Add this directory to PATH, or use\nthe relative compiler command below from the extracted directory.\n\nExternal prerequisites: {requirements}.\n\nVerify the compiler: {command} --version\nCreate hello.wave containing: fun main() {{ println(\"Hello, Wave!\"); }}\nCheck bundled std: {command} check hello.wave\nBuild: {command} build hello.wave -o hello\nThe default std lookup uses the extracted compiler's adjacent std.\nRun the resulting hello executable using your shell's local-file syntax.\n"))?;
    patch_runtime(directory, target, &roots)?;
    for path in roots.iter().map(|(_, p)| p.clone()).chain(libraries) {
        binary::inspect(&path, Some(target))?;
    }
    Ok(())
}

fn archive_epoch() -> Result<u64> {
    archive_epoch_value(std::env::var_os("SOURCE_DATE_EPOCH"), || {
        let mut spec = Spec::new(["git", "show", "-s", "--format=%ct", "HEAD"]);
        spec.cwd = crate::root();
        Ok(spec.checked()?.strict_stdout()?.trim().to_string())
    })
}

fn archive_epoch_value(
    explicit: Option<std::ffi::OsString>,
    source_timestamp: impl FnOnce() -> Result<String>,
) -> Result<u64> {
    let raw = match explicit {
        Some(value) => value.into_string().map_err(|_| error("SOURCE_DATE_EPOCH must be UTF-8"))?,
        None => source_timestamp()?,
    };
    if raw.is_empty() || !raw.bytes().all(|byte| byte.is_ascii_digit()) {
        return Err(error("archive epoch must be a nonnegative integer"));
    }
    raw.parse().map_err(|_| error("archive epoch is out of range"))
}

fn archive_mode(path: &Path, directory: bool) -> Result<u32> {
    if directory {
        return Ok(0o755);
    }
    #[cfg(unix)]
    let executable = {
        use std::os::unix::fs::PermissionsExt;
        fs::metadata(path)?.permissions().mode() & 0o111 != 0
    };
    #[cfg(not(unix))]
    let executable = path.extension().is_some_and(|ext| ext.eq_ignore_ascii_case("exe"));
    Ok(if executable { 0o755 } else { 0o644 })
}

fn archive_entries(stage: &Path) -> Result<BTreeMap<String, PathBuf>> {
    fn visit(stage: &Path, path: &Path, entries: &mut BTreeMap<String, PathBuf>) -> Result<()> {
        for item in fs::read_dir(path)? {
            let item = item?;
            let path = item.path();
            let relative = path
                .strip_prefix(stage)?
                .components()
                .map(|part| {
                    part.as_os_str().to_str().ok_or_else(|| error("archive paths must be UTF-8"))
                })
                .collect::<Result<Vec<_>>>()?
                .join("/");
            cases::relative(&relative)?;
            if entries.insert(relative, path.clone()).is_some() {
                return Err(error("duplicate archive path"));
            }
            if item.file_type()?.is_dir() {
                visit(stage, &path, entries)?;
            }
        }
        Ok(())
    }
    let mut entries = BTreeMap::new();
    visit(stage, stage, &mut entries)?;
    Ok(entries)
}

pub fn archive(stage: &Path, output: &Path, name: &str) -> Result<()> {
    archive_at_epoch(stage, output, name, archive_epoch()?)
}

fn archive_at_epoch(stage: &Path, output: &Path, name: &str, epoch: u64) -> Result<()> {
    cases::relative(name)?;
    crate::sdk::check_tree(stage)?;
    let entries = archive_entries(stage)?;
    if output.extension().is_some_and(|v| v == "zip") {
        if entries
            .values()
            .any(|path| fs::symlink_metadata(path).is_ok_and(|meta| meta.is_symlink()))
        {
            return Err(error("archive staging contains a symlink"));
        }
        let minimum =
            time::Date::from_calendar_date(1980, time::Month::January, 1)?.midnight().assume_utc();
        let maximum = time::Date::from_calendar_date(2107, time::Month::December, 31)?
            .with_hms(23, 59, 58)?
            .assume_utc();
        let date = time::OffsetDateTime::from_unix_timestamp(
            epoch.clamp(minimum.unix_timestamp() as u64, maximum.unix_timestamp() as u64) as i64,
        )?;
        let stamp = zip::DateTime::from_date_and_time(
            date.year() as u16,
            date.month() as u8,
            date.day(),
            date.hour(),
            date.minute(),
            date.second() & !1,
        )
        .map_err(|_| error("invalid ZIP archive epoch"))?;
        let mut zip = zip::ZipWriter::new(fs::File::create(output)?);
        zip.add_directory(
            format!("{name}/"),
            zip::write::SimpleFileOptions::default()
                .last_modified_time(stamp)
                .unix_permissions(0o755),
        )?;
        for (relative, path) in entries {
            let directory = path.is_dir();
            let options = zip::write::SimpleFileOptions::default()
                .compression_method(zip::CompressionMethod::Deflated)
                .last_modified_time(stamp)
                .unix_permissions(archive_mode(&path, directory)?);
            if directory {
                zip.add_directory(format!("{name}/{relative}/"), options)?;
            } else {
                zip.start_file(format!("{name}/{relative}"), options)?;
                std::io::copy(&mut fs::File::open(path)?, &mut zip)?;
            }
        }
        zip.finish()?;
    } else {
        let gzip = flate2::GzBuilder::new()
            .mtime(0)
            .write(fs::File::create(output)?, flate2::Compression::default());
        let mut archive = tar::Builder::new(gzip);
        fn header(epoch: u64, mode: u32, kind: tar::EntryType, size: u64) -> tar::Header {
            let mut header = tar::Header::new_gnu();
            header.set_uid(0);
            header.set_gid(0);
            header.set_mtime(epoch);
            header.set_mode(mode);
            header.set_entry_type(kind);
            header.set_size(size);
            header.set_cksum();
            header
        }
        archive.append_data(
            &mut header(epoch, 0o755, tar::EntryType::Directory, 0),
            name,
            std::io::empty(),
        )?;
        for (relative, path) in entries {
            let metadata = fs::symlink_metadata(&path)?;
            let name = format!("{name}/{relative}");
            if metadata.is_symlink() {
                archive.append_link(
                    &mut header(epoch, 0o777, tar::EntryType::Symlink, 0),
                    name,
                    fs::read_link(path)?,
                )?;
            } else if metadata.is_dir() {
                archive.append_data(
                    &mut header(epoch, 0o755, tar::EntryType::Directory, 0),
                    name,
                    std::io::empty(),
                )?;
            } else if metadata.is_file() {
                archive.append_data(
                    &mut header(
                        epoch,
                        archive_mode(&path, false)?,
                        tar::EntryType::Regular,
                        metadata.len(),
                    ),
                    name,
                    fs::File::open(path)?,
                )?;
            } else {
                return Err(error("unsupported archive input"));
            }
        }
        archive.into_inner()?.finish()?;
    }
    Ok(())
}

pub fn create(
    binary: &Path,
    target: &str,
    version: &str,
    dist: &Path,
    archives: &Path,
) -> Result<PathBuf> {
    if !regex_lite::Regex::new(r"^[0-9]+\.[0-9]+\.[0-9]+(?:-[0-9A-Za-z.-]+)?$")?.is_match(version) {
        return Err(error("invalid archive version"));
    }
    if !target.bytes().all(|c| c.is_ascii_alphanumeric() || [b'-', b'_', b'.'].contains(&c))
        || target.is_empty()
    {
        return Err(error("invalid distribution target"));
    }
    let target = target.replace("riscv64gc-", "riscv64-");
    let name = format!("wave-v{version}-{}", target.replace("-unknown-linux", "-linux"));
    if target.contains("windows") && !target.ends_with("-pc-windows-msvc") {
        return Err(error("only native MSVC Windows distributions are supported"));
    }
    fs::create_dir_all(dist)?;
    fs::create_dir_all(archives)?;
    let temporary = tempfile::Builder::new().prefix(".stage-").tempdir_in(dist)?;
    let candidate = temporary.path().join(&name);
    stage(binary, &target, &candidate)?;
    storage::replace_set(&[(candidate.clone(), dist.join(&name))])?;
    let archive_path = archives
        .join(format!("{name}{}", if target.contains("windows") { ".zip" } else { ".tar.gz" }));
    let pending = tempfile::Builder::new().prefix(".archive-").tempdir_in(archives)?;
    let archive_file = pending.path().join(archive_path.file_name().unwrap());
    archive(&dist.join(&name), &archive_file, &name)?;
    let metadata = identity(&archive_file, &target, version)?;
    let checksum = pending
        .path()
        .join(format!("{}.sha256", archive_path.file_name().unwrap().to_string_lossy()));
    fs::write(
        &checksum,
        format!(
            "{}  {}\n",
            config::text(&metadata, "sha256")?,
            archive_path.file_name().unwrap().to_string_lossy()
        ),
    )?;
    let descriptor = pending
        .path()
        .join(format!("{}.metadata.json", archive_path.file_name().unwrap().to_string_lossy()));
    fs::write(&descriptor, config::json(&metadata)?)?;
    storage::replace_set(&[
        (archive_file, archive_path.clone()),
        (checksum.clone(), archives.join(checksum.file_name().unwrap())),
        (descriptor.clone(), archives.join(descriptor.file_name().unwrap())),
    ])?;
    println!("Packaged {}", archive_path.display());
    Ok(archive_path)
}

pub fn install(binary: &Path, target: &str, prefix: &Path, bindir: &Path) -> Result<()> {
    if bindir.is_absolute()
        || bindir.components().any(|c| !matches!(c, std::path::Component::Normal(_)))
    {
        return Err(error("bindir must be inside installation prefix"));
    }
    let destination = prefix.join(bindir);
    if std::env::var("MESON_INSTALL_DRY_RUN").ok().as_deref() == Some("1") {
        println!("Would install complete runtime into {}", destination.display());
        return Ok(());
    }
    fs::create_dir_all(&destination)?;
    // Staging beside the destination also guarantees rename stays on one filesystem.
    let pending = tempfile::Builder::new().prefix(".wave-install-").tempdir_in(&destination)?;
    let candidate = pending.path().join("payload");
    stage(binary, target, &candidate)?;
    let entries = fs::read_dir(&candidate)?
        .map(|entry| {
            let entry = entry?;
            Ok((entry.path(), destination.join(entry.file_name())))
        })
        .collect::<std::io::Result<Vec<_>>>()?;
    storage::replace_set(&entries)?;
    println!("Installed complete runtime into {}", destination.display());
    Ok(())
}

pub fn main(arguments: &[OsString], installing: bool) -> Result<i32> {
    let mut args = Args::new(arguments);
    let target = args.required("--target")?;
    let binary = args.path("--binary")?.ok_or_else(|| error("missing --binary"))?;
    if let Some(prefix) = args.path("--llvm-prefix")? {
        if !prefix.as_os_str().is_empty() {
            std::env::set_var("WAVE_LLVM_HOME", &prefix);
            std::env::set_var("LLVM_SYS_211_PREFIX", &prefix);
        }
    }
    if installing {
        let bindir = args.path("--bindir")?.unwrap_or_else(|| "bin".into());
        let prefix = args
            .path("--prefix")?
            .or_else(|| std::env::var_os("MESON_INSTALL_DESTDIR_PREFIX").map(PathBuf::from))
            .ok_or_else(|| error("missing installation prefix"))?;
        args.finish()?;
        install(&binary, &target, &prefix, &bindir)?;
    } else {
        let version = args.text("--version")?.map(Ok).unwrap_or_else(config::compiler_version)?;
        let dist = args.path("--dist-dir")?.unwrap_or_else(|| crate::root().join("dist"));
        let archives = args.path("--archive-dir")?.unwrap_or_else(crate::root);
        args.finish()?;
        create(&binary, &target, &version, &dist, &archives)?;
    }
    Ok(0)
}

pub fn verify_crt(root: &Path) -> Result<()> {
    verify_crt_selection(root, &["x86_64", "aarch64", "riscv64", "loongarch64"])
}

pub fn verify_crt_selection(root: &Path, architectures: &[&str]) -> Result<()> {
    for (triple, abis) in [
        ("x86_64-unknown-linux-gnu", vec![("", None)]),
        ("aarch64-unknown-linux-gnu", vec![("", None)]),
        (
            "riscv64-unknown-linux-gnu",
            vec![("lp64", Some(1)), ("lp64f", Some(3)), ("lp64d", Some(5))],
        ),
        (
            "loongarch64-unknown-linux-gnu",
            vec![("lp64s", Some(0x41)), ("lp64f", Some(0x42)), ("lp64d", Some(0x43))],
        ),
    ] {
        if !architectures.contains(&binary::target_arch(triple)) {
            continue;
        }
        for (abi, flags) in abis {
            for name in ["crt1.o", "Scrt1.o", "rcrt1.o", "crti.o", "crtn.o"] {
                let path = root.join(triple).join(abi).join(name);
                let info = binary::inspect(&path, Some(triple))?;
                if flags.is_some_and(|flags| info.flags != flags) {
                    return Err(error(format!(
                        "{}: wrong CRT ABI flags {:#x}",
                        path.display(),
                        info.flags
                    )));
                }
            }
        }
    }
    Ok(())
}

pub fn controlled_environment(package: &Path, home: &Path) -> Result<BTreeMap<OsString, OsString>> {
    let mut env = BTreeMap::new();
    let temp = home.join("temp");
    fs::create_dir_all(&temp)?;
    if cfg!(windows) {
        for (key, value) in std::env::vars_os() {
            if [
                "systemroot",
                "windir",
                "comspec",
                "pathext",
                "programfiles",
                "programfiles(x86)",
                "programw6432",
                "systemdrive",
            ]
            .contains(&key.to_string_lossy().to_ascii_lowercase().as_str())
            {
                env.insert(key, value);
            }
        }
        let system = env
            .iter()
            .find(|(k, _)| k.to_string_lossy().eq_ignore_ascii_case("SystemRoot"))
            .map(|(_, v)| PathBuf::from(v))
            .ok_or_else(|| error("SystemRoot required"))?;
        env.insert(
            "PATH".into(),
            std::env::join_paths([
                package.to_owned(),
                package.join("llvm/bin"),
                system.join("System32"),
                system,
            ])?,
        );
    } else {
        env.insert("PATH".into(), "/usr/bin:/bin".into());
    }
    for name in ["HOME", "USERPROFILE"] {
        env.insert(name.into(), home.as_os_str().into());
    }
    for name in ["TEMP", "TMP"] {
        env.insert(name.into(), temp.as_os_str().into());
    }
    env.insert("NO_COLOR".into(), "1".into());
    for name in ["QEMU_LD_PREFIX", "WAVE_LOONGARCH64_SYSROOT"] {
        if let Some(value) = std::env::var_os(name) {
            env.insert(name.into(), value);
        }
    }
    Ok(env)
}

pub fn extracted_smoke(archive: &Path, target: &str, output: &Path) -> Result<()> {
    if target.contains("windows") {
        crate::windows::require_native(target)?;
    }
    if storage::exists(output) {
        return Err(error("package evidence directory already exists"));
    }
    fs::create_dir_all(output)?;
    let mut commands = Vec::new();
    let result = (|| -> Result<()> {
        let extracted = output.join("한글 package with spaces");
        crate::sdk::extract(archive, &extracted, false)?;
        let entries = fs::read_dir(&extracted)?.collect::<std::result::Result<Vec<_>, _>>()?;
        if entries.len() != 1 || !entries[0].file_type()?.is_dir() {
            return Err(error("expected one package root"));
        }
        let package = entries[0].path();
        let files = storage::files(&package)?;
        for path in &files {
            let name = path.file_name().unwrap().to_string_lossy().to_lowercase();
            if target.contains("windows")
                && ["mingw", "libgcc", "libstdc++", "winpthread"].iter().any(|v| name.contains(v))
            {
                return Err(error("retired GNU package payload"));
            }
        }
        let compiler = package.join(if target.contains("windows") { "wavec.exe" } else { "wavec" });
        for required in [
            &compiler,
            package.join("std/manifest.json").as_path(),
            package.join("licenses/Wave.txt").as_path(),
            package.join("licenses/std.txt").as_path(),
        ] {
            if !required.is_file() {
                return Err(error(format!("missing package component {}", required.display())));
            }
        }
        binary::inspect(&compiler, Some(target))?;
        let tools = if target.contains("windows") {
            vec!["ld.lld", "lld-link", "llvm-readobj", "llc", "llvm-as", "llvm-mc"]
        } else if target.contains("darwin") {
            vec!["ld64.lld", "ld.lld", "llc", "llvm-as", "llvm-mc"]
        } else {
            vec!["ld.lld", "llc", "llvm-as", "llvm-mc"]
        };
        let roots = std::iter::once(compiler.clone())
            .chain(tools.iter().map(|name| {
                package
                    .join("llvm/bin")
                    .join(format!("{name}{}", if target.contains("windows") { ".exe" } else { "" }))
            }))
            .collect::<Vec<_>>();
        for path in &roots {
            binary::inspect(path, Some(target))?;
        }
        if target.contains("windows") {
            for name in ["LLVM", "compiler-rt", "libxml2"] {
                if !package.join(format!("licenses/{name}.txt")).is_file() {
                    return Err(error(format!("missing {name} notice")));
                }
            }
            if !package
                .join(format!(
                    "llvm/lib/clang/21/lib/windows/clang_rt.builtins-{}.lib",
                    binary::target_arch(target)
                ))
                .is_file()
            {
                return Err(error("missing arithmetic builtins"));
            }
            let runtime = PathBuf::from(
                std::env::var_os("SystemRoot").ok_or_else(|| error("SystemRoot required"))?,
            )
            .join("System32");
            let binaries = files
                .iter()
                .filter(|p| {
                    p.extension().is_some_and(|e| {
                        e.eq_ignore_ascii_case("exe") || e.eq_ignore_ascii_case("dll")
                    })
                })
                .cloned()
                .collect::<Vec<_>>();
            crate::windows::dependency_closure(
                &binaries,
                target,
                &[package.clone(), package.join("llvm/bin")],
                &[runtime],
            )?;
        }
        if target.contains("linux") {
            verify_crt(&package.join("crt"))?;
        }
        if target.contains("linux") || target.contains("freebsd") {
            for (index, path) in roots.iter().enumerate() {
                require_rpath(
                    path,
                    if index == 0 { "$ORIGIN/llvm/lib" } else { "$ORIGIN/../lib" },
                )?;
            }
            for path in roots.iter().chain(
                files.iter().filter(|p| p.parent() == Some(package.join("llvm/lib").as_path())),
            ) {
                let info = binary::inspect(path, Some(target))?;
                for name in info.needed {
                    if binary::system_library(&name, target) {
                        continue;
                    }
                    let bundled = package.join("llvm/lib").join(&name).is_file();
                    let base_system = target.contains("freebsd")
                        && [Path::new("/lib"), Path::new("/usr/lib")]
                            .iter()
                            .any(|base| base.join(&name).is_file());
                    if !bundled && !base_system {
                        return Err(error(format!(
                            "unbundled dependency {} required by {}",
                            name,
                            path.display()
                        )));
                    }
                }
            }
        }
        let home = output.join("isolated home");
        fs::create_dir(&home)?;
        let work = output.join("detached working directory");
        fs::create_dir(&work)?;
        let env = controlled_environment(&package, &home)?;
        let mut run =
            |args: Vec<OsString>, expected: Option<&str>| -> Result<crate::process::Output> {
                let mut spec = Spec::new(args);
                spec.cwd = work.clone();
                spec.clean_env = true;
                spec.env = env.clone();
                spec.timeout = std::time::Duration::from_secs(120);
                let result = spec.run();
                let mut row = Value::object([
                    ("command", Value::strings(spec.args.iter().map(|a| a.to_string_lossy()))),
                    ("cwd", Value::string(work.to_string_lossy())),
                ]);
                match &result {
                    Ok(result) => {
                        for (key, value) in [
                            ("exit", config::number(result.status as i64)),
                            ("stdout", Value::string(result.text_stdout())),
                            ("stderr", Value::string(result.text_stderr())),
                        ] {
                            config::field(&mut row, key, value)?;
                        }
                    },
                    Err(e) => config::field(&mut row, "error", Value::string(e.to_string()))?,
                };
                commands.push(row);
                let output = result?;
                if output.failure.is_some() || output.status != 0 {
                    return Err(error(format!(
                        "extracted package command failed: {:?}, {}, {}",
                        output.failure,
                        output.status,
                        output.text_stderr()
                    )));
                }
                if expected.is_some_and(|expected| output.text_stdout() != expected) {
                    return Err(error(format!(
                        "unexpected extracted package stdout: {:?}",
                        output.text_stdout()
                    )));
                }
                Ok(output)
            };
        run(vec![compiler.clone().into(), "-V".into()], None)?;
        if target.contains("windows") || target.contains("freebsd") {
            for property in ["host-target", "default-target"] {
                run(
                    vec![compiler.clone().into(), "print".into(), property.into()],
                    Some(&format!("{target}\n")),
                )?;
            }
        }
        if target.contains("freebsd") {
            for name in &tools {
                run(vec![package.join("llvm/bin").join(name).into(), "--version".into()], None)?;
            }
            let dependency = regex_lite::Regex::new(r"=> (/[^\n]+?) \(0x")?;
            for path in &roots {
                let mut spec = Spec::new([OsString::from("/usr/bin/ldd"), path.as_os_str().into()]);
                spec.cwd = work.clone();
                spec.clean_env = true;
                spec.env = env.clone();
                let result = spec.checked()?.strict_stdout()?;
                if result.contains("not found") {
                    return Err(error("FreeBSD runtime dependency not found"));
                }
                for found in dependency.captures_iter(&result) {
                    let path = Path::new(&found[1]);
                    if !path.starts_with(&package)
                        && ![Some(Path::new("/lib")), Some(Path::new("/usr/lib"))]
                            .contains(&path.parent())
                    {
                        return Err(error(format!(
                            "FreeBSD package uses a host dependency: {}",
                            path.display()
                        )));
                    }
                }
            }
        }
        if target.contains("windows") {
            for name in &tools {
                run(
                    vec![
                        package.join("llvm/bin").join(format!("{name}.exe")).into(),
                        "--version".into(),
                    ],
                    None,
                )?;
            }
            for (name, expected) in [
                ("implicit", "native implicit main\n"),
                ("wide_numeric", "native wide numeric checked\n"),
                ("package_std", "packaged std checked\n"),
            ] {
                let source = work.join(format!("{name}.wave"));
                fs::copy(
                    crate::root().join("tests/fixtures/msvc_native").join(format!("{name}.wave")),
                    &source,
                )?;
                for static_link in [false, true] {
                    let exe = work.join(format!("{name}-{static_link}.exe"));
                    let mut args = vec![
                        compiler.clone().into(),
                        "build".into(),
                        source.clone().into(),
                        "-o".into(),
                        exe.clone().into(),
                    ];
                    if static_link {
                        args.push("--static".into());
                    }
                    run(args, None)?;
                    binary::inspect(&exe, Some(target))?;
                    let runtime =
                        PathBuf::from(std::env::var_os("SystemRoot").unwrap()).join("System32");
                    crate::windows::dependency_closure(
                        std::slice::from_ref(&exe),
                        target,
                        &[package.clone(), package.join("llvm/bin")],
                        &[runtime],
                    )?;
                    run(vec![exe.into()], Some(expected))?;
                }
                run(vec![compiler.clone().into(), "run".into(), source.into()], Some(expected))?;
            }
            run(
                vec![
                    compiler.into(),
                    "build".into(),
                    work.join("package_std.wave").into(),
                    "--std-root".into(),
                    package.join("std").into(),
                    "--dry-run".into(),
                    "-o".into(),
                    work.join("sdk-plan.exe").into(),
                ],
                None,
            )?;
        } else {
            let source = work.join("std smoke.wave");
            fs::write(&source,"import(\"std::mem::layout\")::{size_of}; fun main() -> i32 { if (size_of<i64>() != 8) { return 7; } println(\"release smoke\"); return 0; }\n")?;
            let mut args = vec![compiler.clone().into(), "run".into(), source.clone().into()];
            if let Some(sysroot) = std::env::var_os("WAVE_CROSS_SYSROOT") {
                args.extend(["--sysroot".into(), sysroot]);
            }
            run(args, Some("release smoke\n"))?;
            if target.contains("freebsd") {
                let object = work.join("smoke.o");
                run(
                    vec![
                        compiler.clone().into(),
                        "build".into(),
                        source.clone().into(),
                        "--emit=obj".into(),
                        "-o".into(),
                        object.clone().into(),
                        "--std-root".into(),
                        package.join("std").into(),
                    ],
                    None,
                )?;
                binary::inspect(&object, Some(target))?;
                let missing = "wave_missing_release_dependency";
                let failed = work.join("must-not-exist");
                let mut spec = Spec::new([
                    compiler.as_os_str(),
                    "build".as_ref(),
                    source.as_os_str(),
                    "--std-root".as_ref(),
                    package.join("std").as_os_str(),
                    format!("-l{missing}").as_ref(),
                    "-o".as_ref(),
                    failed.as_os_str(),
                ]);
                spec.cwd = work.clone();
                spec.env = env.clone();
                spec.clean_env = true;
                let output = spec.run()?;
                if output.status == 0
                    || output.failure.is_some()
                    || !format!("{}{}", output.text_stdout(), output.text_stderr())
                        .contains(missing)
                    || failed.exists()
                {
                    return Err(error("missing library did not fail clearly without output"));
                }
                let peer = work.join("provider.c");
                fs::write(&peer, "int answer(void) { return 42; }\n")?;
                let object = work.join("provider.o");
                run(
                    vec![
                        "/usr/bin/cc".into(),
                        "-c".into(),
                        peer.into(),
                        "-o".into(),
                        object.clone().into(),
                    ],
                    None,
                )?;
                fs::write(
                    &source,
                    "extern(c) fun answer() -> i32; fun main() -> i32 { return answer() - 42; }\n",
                )?;
                run(
                    vec![
                        compiler.into(),
                        "build".into(),
                        source.into(),
                        object.into(),
                        "--run".into(),
                        "--std-root".into(),
                        package.join("std").into(),
                    ],
                    None,
                )?;
            }
        }
        Ok(())
    })();
    let mut report = Value::object([
        ("target", Value::string(target)),
        ("commands", Value::Array(commands)),
        ("passed", Value::Bool(result.is_ok())),
    ]);
    if let Err(e) = &result {
        config::field(&mut report, "error", Value::string(e.to_string()))?;
    }
    storage::atomic_write(&output.join("report.json"), config::json(&report)?.as_bytes())?;
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn freebsd_base_libraries_are_resolved_under_the_selected_sysroot() {
        let root = Path::new("/reviewed-freebsd-base");
        assert!(freebsd_base_library(&root.join("lib/libc.so.7"), root));
        assert!(freebsd_base_library(&root.join("usr/lib/libc++.so.1"), root));
        assert!(!freebsd_base_library(Path::new("/lib/libc.so.7"), root));
        assert!(!freebsd_base_library(&root.join("usr/local/lib/libzstd.so.1"), root));
        assert!(!freebsd_base_library(
            Path::new("/reviewed-freebsd-base-other/lib/libc.so.7"),
            root
        ));
    }

    #[test]
    fn archive_bytes_ignore_ambient_times_order_and_nonsemantic_permissions() {
        let temp = tempfile::tempdir().unwrap();
        let stage = temp.path().join("stage");
        fs::create_dir_all(stage.join("empty")).unwrap();
        fs::create_dir_all(stage.join("bin")).unwrap();
        let payload = stage.join("payload");
        let executable = stage.join("bin/wavec.exe");
        fs::write(&payload, b"unchanged bytes").unwrap();
        fs::write(&executable, b"executable bytes").unwrap();
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            fs::set_permissions(&payload, fs::Permissions::from_mode(0o600)).unwrap();
            fs::set_permissions(&executable, fs::Permissions::from_mode(0o700)).unwrap();
        }
        for extension in ["tar.gz", "zip"] {
            // Windows requires write-attributes access for set_times; a
            // read-only handle is insufficient even for a writable file.
            fs::OpenOptions::new()
                .write(true)
                .open(&payload)
                .unwrap()
                .set_times(fs::FileTimes::new().set_modified(
                    std::time::UNIX_EPOCH + std::time::Duration::from_secs(1500000000),
                ))
                .unwrap();
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                fs::set_permissions(&payload, fs::Permissions::from_mode(0o600)).unwrap();
                fs::set_permissions(&executable, fs::Permissions::from_mode(0o700)).unwrap();
            }
            let first = temp.path().join(format!("first.{extension}"));
            let second = temp.path().join(format!("second.{extension}"));
            archive_at_epoch(&stage, &first, "wave", 1600000001).unwrap();
            fs::OpenOptions::new()
                .write(true)
                .open(&payload)
                .unwrap()
                .set_times(fs::FileTimes::new().set_modified(
                    std::time::UNIX_EPOCH + std::time::Duration::from_secs(1700000000),
                ))
                .unwrap();
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                fs::set_permissions(&payload, fs::Permissions::from_mode(0o644)).unwrap();
                fs::set_permissions(&executable, fs::Permissions::from_mode(0o755)).unwrap();
            }
            // Recreate a directory to change ambient directory timestamps/order.
            fs::remove_dir(stage.join("empty")).unwrap();
            fs::create_dir(stage.join("empty")).unwrap();
            archive_at_epoch(&stage, &second, "wave", 1600000001).unwrap();
            assert_eq!(fs::read(&first).unwrap(), fs::read(&second).unwrap(), "{extension}");
            if extension == "tar.gz" {
                let mut archive =
                    tar::Archive::new(flate2::read::GzDecoder::new(fs::File::open(first).unwrap()));
                let mut names = Vec::new();
                for entry in archive.entries().unwrap() {
                    let entry = entry.unwrap();
                    let header = entry.header();
                    assert_eq!(header.uid().unwrap(), 0);
                    assert_eq!(header.gid().unwrap(), 0);
                    assert_eq!(header.mtime().unwrap(), 1600000001);
                    let path = entry.path().unwrap().into_owned();
                    let expected = if path == Path::new("wave/payload") { 0o644 } else { 0o755 };
                    assert_eq!(header.mode().unwrap(), expected, "{}", path.display());
                    names.push(path);
                }
                assert_eq!(
                    names,
                    ["wave", "wave/bin", "wave/bin/wavec.exe", "wave/empty", "wave/payload"]
                        .map(PathBuf::from)
                );
            } else {
                let mut archive = zip::ZipArchive::new(fs::File::open(first).unwrap()).unwrap();
                assert_eq!(archive.len(), 5);
                assert_eq!(
                    archive.by_name("wave/bin/wavec.exe").unwrap().unix_mode().unwrap() & 0o777,
                    0o755
                );
                assert_eq!(
                    archive.by_name("wave/payload").unwrap().unix_mode().unwrap() & 0o777,
                    0o644
                );
            }
        }
    }

    #[test]
    fn archive_epoch_selects_explicit_values_without_consulting_the_repository() {
        for text in ["0", "1600000001"] {
            assert_eq!(
                archive_epoch_value(Some(text.into()), || panic!("unexpected Git lookup"))
                    .unwrap()
                    .to_string(),
                text
            );
        }
        for text in ["", "-1", "+1", " 1", "1\n", "18446744073709551616"] {
            assert!(archive_epoch_value(Some(text.into()), || panic!(
                "invalid explicit value must fail"
            ))
            .is_err());
        }
        assert_eq!(archive_epoch_value(None, || Ok("1600000001".into())).unwrap(), 1600000001);
        assert!(archive_epoch_value(None, || Err(error("Git unavailable"))).is_err());
        #[cfg(unix)]
        {
            use std::os::unix::ffi::OsStringExt;
            assert!(archive_epoch_value(Some(std::ffi::OsString::from_vec(vec![255])), || panic!(
                "invalid UTF-8"
            ))
            .is_err());
        }
    }

    #[cfg(unix)]
    #[test]
    fn normalized_tar_keeps_only_safe_relative_links() {
        let temp = tempfile::tempdir().unwrap();
        let stage = temp.path().join("stage");
        fs::create_dir(&stage).unwrap();
        fs::write(stage.join("real"), b"payload").unwrap();
        std::os::unix::fs::symlink("real", stage.join("alias")).unwrap();
        let output = temp.path().join("test.tar.gz");
        archive_at_epoch(&stage, &output, "wave", 1).unwrap();
        let mut archive =
            tar::Archive::new(flate2::read::GzDecoder::new(fs::File::open(&output).unwrap()));
        assert!(archive.entries().unwrap().any(|entry| entry
            .unwrap()
            .header()
            .entry_type()
            .is_symlink()));
        fs::remove_file(stage.join("alias")).unwrap();
        std::os::unix::fs::symlink("../outside", stage.join("alias")).unwrap();
        assert!(archive_at_epoch(&stage, &output, "wave", 1).is_err());
    }

    #[test]
    fn which_skips_non_executable_files_and_preserves_executable_matches() {
        let temp = tempfile::tempdir().unwrap();
        let first = temp.path().join("first");
        let second = temp.path().join("second");
        fs::create_dir_all(&first).unwrap();
        fs::create_dir_all(&second).unwrap();

        let tool_name = if cfg!(windows) { "audit-tool.exe" } else { "audit-tool" };
        let non_exec = first.join(tool_name);
        let exec = second.join(tool_name);

        fs::write(&non_exec, b"dummy content").unwrap();
        fs::write(&exec, b"#!/bin/sh\necho ok").unwrap();

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            fs::set_permissions(&non_exec, fs::Permissions::from_mode(0o644)).unwrap();
            fs::set_permissions(&exec, fs::Permissions::from_mode(0o755)).unwrap();
        }

        let path_os = std::env::join_paths([&first, &second]).unwrap();
        let old_path = std::env::var_os("PATH");
        std::env::set_var("PATH", &path_os);

        // Search PATH: should skip `first` (0644) and hit `second` (0755)
        let found = which("audit-tool");

        // Restore PATH early so we don't pollute other test runs
        if let Some(old) = old_path {
            std::env::set_var("PATH", old);
        } else {
            std::env::remove_var("PATH");
        }

        #[cfg(unix)]
        {
            assert_eq!(found, Some(exec.clone()));
            assert_eq!(which(non_exec.to_str().unwrap()), None);
            assert_eq!(which(exec.to_str().unwrap()), Some(exec));
        }

        #[cfg(not(unix))]
        {
            assert!(found.is_some());
        }
    }
}
