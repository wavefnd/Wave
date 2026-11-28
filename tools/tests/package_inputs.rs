// SPDX-License-Identifier: MPL-2.0
use std::fs;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Output, Stdio};
use wave_tools::{config, storage};

fn git(root: &Path, args: &[&str], input: Option<&[u8]>) -> Output {
    let mut child = Command::new("git")
        .args([
            "-c",
            "core.autocrlf=false",
            "-c",
            "user.name=Fixture",
            "-c",
            "user.email=fixture@example.invalid",
            "-c",
            "commit.gpgsign=false",
        ])
        .args(args)
        .current_dir(root)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    if let Some(input) = input {
        child.stdin.take().unwrap().write_all(input).unwrap();
    }
    let result = child.wait_with_output().unwrap();
    assert!(result.status.success(), "{}", String::from_utf8_lossy(&result.stderr));
    result
}

#[test]
fn package_membership_uses_committed_blobs_excludes_local_files_and_rejects_dirty_or_link_inputs() {
    let work = tempfile::tempdir().unwrap();
    let root = work.path();
    git(root, &["init", "--quiet"], None);
    fs::create_dir(root.join("std")).unwrap();
    fs::write(root.join("std/core.wave"), b"COMMITTED\n").unwrap();
    fs::write(root.join(".gitignore"), "std/ignored\n").unwrap();
    git(root, &["add", "std", ".gitignore"], None);
    git(root, &["commit", "--quiet", "-m", "fixture"], None);
    for name in ["ignored", "untracked"] {
        fs::write(root.join("std").join(name), "LOCAL").unwrap();
    }
    let check = || {
        Command::new(env!("CARGO_BIN_EXE_wave-tools"))
            .args(["check", "std-inputs"])
            .env("WAVE_REPOSITORY", root)
            .output()
            .unwrap()
    };
    let result = check();
    assert!(result.status.success(), "{}", String::from_utf8_lossy(&result.stderr));
    let rows = config::parse_json(std::str::from_utf8(&result.stdout).unwrap()).unwrap();
    let utils::wson::Value::Array(rows) = rows else { panic!() };
    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0].get_str("path"), Some("core.wave"));
    assert_eq!(rows[0].get_str("sha256"), Some(storage::bytes_digest(b"COMMITTED\n").as_str()));
    fs::write(root.join("std/core.wave"), "DIRTY").unwrap();
    let dirty = check();
    assert!(!dirty.status.success());
    assert!(String::from_utf8_lossy(&dirty.stderr).contains("commit or restore"));
    git(root, &["checkout", "--", "std/core.wave"], None);
    // Git records a symlink independently of the host's symlink privilege.
    let blob = git(root, &["hash-object", "-w", "--stdin"], Some(b"core.wave"));
    let oid = std::str::from_utf8(&blob.stdout).unwrap().trim();
    git(
        root,
        &["update-index", "--add", "--cacheinfo", &format!("120000,{oid},std/link.wave")],
        None,
    );
    git(root, &["commit", "--quiet", "-m", "link"], None);
    let link = check();
    assert!(!link.status.success());
    assert!(String::from_utf8_lossy(&link.stderr).contains("not a regular committed blob"));
    assert_eq!(fs::read(root.join("std/core.wave")).unwrap(), b"COMMITTED\n");
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn linux_package_preserves_loader_path_order_and_rpath_inheritance() {
    let work = tempfile::tempdir().unwrap();
    let root = work.path();
    for dir in ["z-first", "a-second", "sdk/lib"] {
        fs::create_dir_all(root.join(dir)).unwrap();
    }
    let cc = |args: &[&str]| {
        let output = Command::new("cc").current_dir(root).args(args).output().unwrap();
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    };
    for (dir, value) in [("z-first", 22), ("a-second", 33), ("sdk/lib", 11)] {
        fs::write(root.join("value.c"), format!("int value(void) {{ return {value}; }}")).unwrap();
        cc(&[
            "-shared",
            "-fPIC",
            "value.c",
            "-Wl,-soname,libwave_value.so",
            "-o",
            &format!("{dir}/libwave_value.so"),
        ]);
    }
    fs::write(root.join("call.c"), "extern int value(void); int call(void) { return value(); }")
        .unwrap();
    cc(&[
        "-shared",
        "-fPIC",
        "call.c",
        "-Lz-first",
        "-lwave_value",
        "-Wl,-soname,libwave_call.so",
        "-o",
        "z-first/libwave_call.so",
    ]);
    fs::write(root.join("main.c"), "extern int call(void); int main(void) { return call(); }")
        .unwrap();
    let target = "x86_64-unknown-linux-gnu";
    for (name, flag, provider) in [
        ("rpath", "-Wl,--disable-new-dtags", "z-first"),
        ("runpath", "-Wl,--enable-new-dtags", "sdk/lib"),
    ] {
        cc(&[
            "main.c",
            "-Lz-first",
            "-lwave_call",
            "-Wl,-rpath-link,z-first",
            flag,
            "-Wl,-rpath,$ORIGIN/z-first:${ORIGIN}/a-second",
            "-o",
            "program",
        ]);
        let program = root.join("program");
        let info = wave_tools::binary::inspect(&program, Some(target)).unwrap();
        assert_eq!(info.runtime_paths(), ["$ORIGIN/z-first", "${ORIGIN}/a-second"]);
        assert_eq!(info.runpaths.is_empty(), name == "rpath");
        let stage = root.join(name);
        fs::create_dir(&stage).unwrap();
        let staged = stage.join("program");
        fs::copy(&program, &staged).unwrap();
        wave_tools::package::runtime_closure(
            &stage,
            target,
            &root.join("sdk"),
            &[(program, staged.clone())],
        )
        .unwrap();
        let libs = stage.join("llvm/lib");
        assert_eq!(
            storage::digest(&libs.join("libwave_value.so")).unwrap(),
            storage::digest(&root.join(provider).join("libwave_value.so")).unwrap(),
            "{name} provider"
        );
        let status = Command::new(staged).env("LD_LIBRARY_PATH", libs).status().unwrap();
        assert_eq!(status.code(), Some(if name == "rpath" { 22 } else { 11 }));
    }
}

#[cfg(target_os = "macos")]
#[test]
fn macos_package_uses_the_main_executable_for_transitive_dylibs() {
    let work = tempfile::tempdir().unwrap();
    let root = work.path();
    for path in ["app/lib", "app/plugins", "app/providers", "stage"] {
        fs::create_dir_all(root.join(path)).unwrap();
    }
    let target =
        if cfg!(target_arch = "aarch64") { "aarch64-apple-darwin" } else { "x86_64-apple-darwin" };
    let clang = |args: &[&str]| {
        let output = Command::new("clang")
            .args(args)
            .arg("-Wl,-headerpad_max_install_names")
            .current_dir(root)
            .output()
            .unwrap();
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    };
    fs::write(root.join("value.c"), "int value(void) { return 22; }").unwrap();
    fs::write(
        root.join("plugin.c"),
        "extern int value(void); int plugin(void) { return value(); }",
    )
    .unwrap();
    fs::write(
        root.join("lib.c"),
        "extern int plugin(void); int library(void) { return plugin(); }",
    )
    .unwrap();
    fs::write(
        root.join("main.c"),
        "extern int library(void); int main(void) { return library() != 22; }",
    )
    .unwrap();
    clang(&[
        "-dynamiclib",
        "value.c",
        "-Wl,-install_name,@rpath/value.dylib",
        "-o",
        "app/providers/value.dylib",
    ]);
    clang(&[
        "-dynamiclib",
        "plugin.c",
        "app/providers/value.dylib",
        "-Wl,-install_name,@executable_path/plugins/plugin.dylib",
        "-o",
        "app/plugins/plugin.dylib",
    ]);
    clang(&[
        "-dynamiclib",
        "lib.c",
        "app/plugins/plugin.dylib",
        "-Wl,-install_name,@loader_path/lib/library.dylib",
        "-o",
        "app/lib/library.dylib",
    ]);
    clang(&[
        "main.c",
        "app/lib/library.dylib",
        "-Wl,-rpath,@executable_path/providers",
        "-o",
        "app/program",
    ]);
    fs::create_dir_all(root.join("sdk/bin")).unwrap();
    fs::write(root.join("tool.c"), "int main(void) { return 0; }").unwrap();
    clang(&["tool.c", "-o", "tool"]);
    for name in ["ld64.lld", "ld.lld", "llc", "llvm-as", "llvm-mc"] {
        fs::copy(root.join("tool"), root.join("sdk/bin").join(name)).unwrap();
    }
    let stage = root.join("stage");
    let installed = Command::new(env!("CARGO_BIN_EXE_wave-tools"))
        .args(["install", "--target", target, "--binary"])
        .arg(root.join("app/program"))
        .arg("--llvm-prefix")
        .arg(root.join("sdk"))
        .arg("--prefix")
        .arg(&stage)
        .env("WAVE_LLVM_BIN", root.join("sdk/bin"))
        .env_remove("WAVE_WINDOWS_LLVM_BIN")
        .env_remove("MESON_INSTALL_DRY_RUN")
        .output()
        .unwrap();
    assert!(installed.status.success(), "{}", String::from_utf8_lossy(&installed.stderr));
    let payload = stage.join("bin");
    for name in ["library.dylib", "plugin.dylib", "value.dylib"] {
        assert!(payload.join("llvm/lib").join(name).is_file());
    }
    fs::remove_dir_all(root.join("app")).unwrap();
    assert!(Command::new(payload.join("wavec")).current_dir(&stage).status().unwrap().success());
}
