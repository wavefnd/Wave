// SPDX-License-Identifier: MPL-2.0
use std::{env, path::Path, process::Command};

fn git(args: &[&str]) -> Option<String> {
    let result = Command::new("git").args(args).output().ok()?;
    result.status.success().then(|| String::from_utf8_lossy(&result.stdout).trim().to_owned())
}

fn main() {
    println!("cargo:rerun-if-env-changed=WAVE_STD_REVISION");
    // Track both detached HEAD and branch ref changes, including worktrees.
    for path in [
        git(&["rev-parse", "--git-path", "HEAD"]),
        git(&["rev-parse", "--git-path", "packed-refs"]),
    ]
    .into_iter()
    .flatten()
    {
        println!("cargo:rerun-if-changed={path}");
    }
    if let Some(reference) = git(&["symbolic-ref", "-q", "HEAD"]) {
        if let Some(path) = git(&["rev-parse", "--git-path", &reference]) {
            println!("cargo:rerun-if-changed={path}");
        }
    }
    // Source archives can supply the original immutable commit explicitly.
    let revision = env::var("WAVE_STD_REVISION")
        .ok()
        .or_else(|| {
            // Do not accidentally record an enclosing, unrelated repository.
            let root = git(&["rev-parse", "--show-toplevel"])?;
            if Path::new(&root).canonicalize().ok()?
                != Path::new(&env::var("CARGO_MANIFEST_DIR").ok()?).canonicalize().ok()?
            {
                return None;
            }
            git(&["rev-parse", "--verify", "HEAD"])
        })
        .unwrap_or_default();
    assert!(
        revision.is_empty()
            || ((revision.len() == 40 || revision.len() == 64)
                && revision.bytes().all(|c| c.is_ascii_hexdigit())),
        "WAVE_STD_REVISION must be a full Git commit ID"
    );
    if revision.is_empty() {
        println!("cargo:warning=No std revision recorded; set WAVE_STD_REVISION when building source archives or use install std --ref");
    }
    println!("cargo:rustc-env=WAVE_BUNDLED_STD_REVISION={revision}");
}
