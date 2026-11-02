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
