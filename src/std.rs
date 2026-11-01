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

//! Installation and update commands for the separately licensed Wave standard library.
//!
//! Only the repository's `std` subtree is fetched. Its manifest is validated
//! before files are copied into the per-user Wave library directory so an
//! unexpected repository layout is not installed as the standard library.

use crate::errors::CliError;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, SystemTime, UNIX_EPOCH};
mod paths;
mod process;
use std::{env, fs};

const STD_REPOSITORY: &str = "https://github.com/wavefnd/Wave.git";
pub const BUNDLED_STD_REVISION: &str = env!("WAVE_BUNDLED_STD_REVISION");

pub fn std_install() -> Result<(), CliError> {
    install_or_update_std(false, None)
}

pub fn std_update() -> Result<(), CliError> {
    install_or_update_std(true, None)
}

pub fn std_install_with_reference(reference: Option<&str>) -> Result<(), CliError> {
    install_or_update_std(false, reference)
}

pub fn std_update_with_reference(reference: Option<&str>) -> Result<(), CliError> {
    install_or_update_std(true, reference)
}

fn resolve_std_reference(reference: Option<&str>) -> Result<&str, CliError> {
    select_std_reference(reference, BUNDLED_STD_REVISION)
}

fn select_std_reference<'a>(
    reference: Option<&'a str>,
    bundled_revision: &'a str,
) -> Result<&'a str, CliError> {
    let value = reference.unwrap_or(bundled_revision);
    if value.is_empty() {
        return Err(CliError::usage(
            "this compiler has no pinned std revision; pass --ref <commit-or-ref>",
        ));
    }
    if value.starts_with('-') || value.contains(':') || value.chars().any(char::is_whitespace) {
        return Err(CliError::usage("invalid std reference: expected a Git commit, branch or tag"));
    }
    Ok(value)
}

fn install_or_update_std(is_update: bool, reference: Option<&str>) -> Result<(), CliError> {
    let install_dir = resolve_std_install_dir()?;

    if install_dir.exists() && !is_update {
        return Err(CliError::StdAlreadyInstalled { path: install_dir });
    }

    let reference = resolve_std_reference(reference)?;
    install_from_repository_mode(
        STD_REPOSITORY,
        reference,
        &install_dir,
        is_update,
        validate_staged_std,
    )?;

    if is_update {
        println!("✅ std updated: {}", install_dir.display());
    } else {
        println!("✅ std installed: {}", install_dir.display());
    }

    Ok(())
}

#[cfg(test)]
fn install_from_repository(
    repository: &str,
    reference: &str,
    install_dir: &Path,
    validate: impl FnOnce(&Path, &Path) -> Result<(), CliError>,
) -> Result<(), CliError> {
    install_from_repository_mode(repository, reference, install_dir, true, validate)
}

struct InstallationLock(fs::File);

impl Drop for InstallationLock {
    fn drop(&mut self) {
        // Closing only this handle is insufficient if a concurrent fork still
        // holds a duplicate open-file description before exec. Release the
        // transaction's lock explicitly before the owned file is closed.
        let _ = self.0.unlock();
    }
}

struct Cleanup {
    path: PathBuf,
    identity: (u64, u64),
}
impl Cleanup {
    fn new(path: PathBuf) -> Result<Self, CliError> {
        let identity = paths::identity(&path)?;
        Ok(Self { path, identity })
    }
}
impl Drop for Cleanup {
    fn drop(&mut self) {
        // Never delete a replacement directory owned by another transaction.
        if paths::identity(&self.path).is_ok_and(|identity| identity == self.identity) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }
}

fn install_from_repository_mode(
    repository: &str,
    reference: &str,
    install_dir: &Path,
    allow_existing: bool,
    validate: impl FnOnce(&Path, &Path) -> Result<(), CliError>,
) -> Result<(), CliError> {
    let install_parent = install_dir.parent().ok_or_else(|| {
        CliError::CommandFailed(format!(
            "std installation path '{}' has no parent",
            install_dir.display()
        ))
    })?;
    fs::create_dir_all(install_parent)?;

    let install_parent = fs::canonicalize(install_parent)?;
    let install_dir = install_parent
        .join(install_dir.file_name().ok_or_else(|| CliError::usage("invalid std destination"))?);
    let install_dir = install_dir.as_path();
    let lock_path = install_parent
        .join(format!(".{}-install.lock", install_dir.file_name().unwrap().to_string_lossy()));
    if fs::symlink_metadata(&lock_path).is_ok_and(|m| !m.is_file() || m.file_type().is_symlink()) {
        return Err(CliError::CommandFailed("std lock path is a symbolic link".into()));
    }
    let parent_identity = paths::identity(&install_parent)?;
    let mut options = fs::OpenOptions::new();
    options.read(true).write(true).create(true).truncate(false);
    #[cfg(target_os = "linux")]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options.custom_flags(0x20000);
    }
    #[cfg(any(
        target_os = "macos",
        target_os = "freebsd",
        target_os = "openbsd",
        target_os = "netbsd",
        target_os = "dragonfly"
    ))]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options.custom_flags(0x100);
    }
    #[cfg(windows)]
    {
        use std::os::windows::fs::OpenOptionsExt;
        options.custom_flags(0x00200000);
    }
    let lock = options.open(&lock_path)?;
    if !lock.metadata()?.is_file() {
        return Err(CliError::CommandFailed("std lock is not a regular file".into()));
    }
    match lock.try_lock() {
        Ok(()) => {},
        Err(fs::TryLockError::WouldBlock) => {
            return Err(CliError::InstallationBusy { path: install_dir.to_path_buf() })
        },
        Err(fs::TryLockError::Error(error)) => return Err(error.into()),
    }
    let _lock = InstallationLock(lock);
    if install_dir.exists() && !allow_existing {
        return Err(CliError::StdAlreadyInstalled { path: install_dir.to_path_buf() });
    }
    if paths::identity(&install_parent)? != parent_identity {
        return Err(CliError::CommandFailed(
            "std installation parent changed while acquiring the lock".into(),
        ));
    }
    let _budget = process::Budget::begin();
    reject_links_if_present(install_dir)?;

    // Keep staging beside the final directory so the final rename stays on one
    // filesystem. The installed tree is untouched until every check passes.
    let checkout = make_tmp_dir("wave-std-checkout")?;
    let _checkout_cleanup = Cleanup::new(checkout.clone())?;
    let stage_home = make_tmp_dir_in(&install_parent, ".std-stage")?;
    let _stage_cleanup = Cleanup::new(stage_home.clone())?;
    let stage_std = stage_home.join(".wave/lib/wave/std");

    let result = (|| {
        let (src_std, source_revision) =
            fetch_std_from_wave_repo_sparse(&checkout, repository, reference)?;
        reject_links(&src_std)?;
        validate_std_manifest(&src_std)?;

        copy_dir_all(&src_std, &stage_std)?;
        fs::write(
            stage_std.join("INSTALL_META"),
            format!(
                "repo={}\nref={}\nrevision={}\ncompatibility_revision={}\n",
                repository,
                reference,
                source_revision,
                parser::import::STD_COMPATIBILITY_REVISION
            ),
        )?;

        validate(&stage_home, &stage_std)?;
        process::check_deadline()?;
        if fs::canonicalize(&install_parent)? != install_parent
            || paths::identity(&install_parent)? != parent_identity
        {
            return Err(CliError::CommandFailed(
                "std installation parent changed during staging".into(),
            ));
        }
        reject_links(&stage_std)?;
        reject_links(&src_std)?;
        reject_links_if_present(install_dir)?;
        replace_std_tree(&stage_std, install_dir)
    })();

    result
}

fn fetch_std_from_wave_repo_sparse(
    checkout: &Path,
    repository: &str,
    reference: &str,
) -> Result<(PathBuf, String), CliError> {
    let version = process::output(Command::new("git").arg("--version"), Duration::from_secs(300))
        .map_err(|error| match error {
        CliError::Io(error) if error.kind() == std::io::ErrorKind::NotFound => {
            CliError::ExternalToolMissing("git".into())
        },
        error => error,
    })?;
    if !version.status.success() {
        return Err(CliError::CommandFailed("git --version exited unsuccessfully".into()));
    }

    // Fetch the requested ref/commit directly; --branch cannot name a pinned
    // commit. Never fetch a moving master as a fallback for a missing pin.
    run_cmd(Command::new("git").arg("init").arg(checkout), "git init")?;
    run_cmd(
        Command::new("git").arg("-C").arg(checkout).args(["remote", "add", "origin", repository]),
        "git remote add",
    )?;
    run_cmd(
        Command::new("git").arg("-C").arg(checkout).args(["sparse-checkout", "set", "std"]),
        "git sparse-checkout set std",
    )?;
    run_cmd(
        Command::new("git").arg("-C").arg(checkout).args([
            "fetch",
            "--depth=1",
            "--filter=blob:none",
            "origin",
            reference,
        ]),
        "git fetch std reference",
    )?;
    run_cmd(
        Command::new("git").arg("-C").arg(checkout).args(["checkout", "--detach", "FETCH_HEAD"]),
        "git checkout std revision",
    )?;

    let source_revision = run_cmd_stdout(
        Command::new("git").arg("-C").arg(checkout).arg("rev-parse").arg("HEAD"),
        "git rev-parse HEAD",
    )?;

    Ok((checkout.join("std"), source_revision))
}

fn validate_std_manifest(std_root: &Path) -> Result<(), CliError> {
    let revision =
        parser::import::std_compatibility_revision(std_root).map_err(CliError::CommandFailed)?;
    let required = parser::import::STD_COMPATIBILITY_REVISION;
    if revision != required {
        return Err(CliError::CommandFailed(format!(
            "downloaded std compatibility revision {}, but this compiler requires {}",
            revision, required
        )));
    }
    Ok(())
}

fn validate_staged_std(stage_home: &Path, stage_std: &Path) -> Result<(), CliError> {
    validate_std_manifest(stage_std)?;

    let mut sources = Vec::new();
    collect_wave_sources(stage_std, &mut sources)?;
    sources.sort();
    if sources.is_empty() {
        return Err(CliError::CommandFailed(
            "downloaded std contains no Wave source files".to_string(),
        ));
    }

    let compiler = env::current_exe()?;
    for source in sources {
        let output = process::output(
            Command::new(&compiler)
                .env("HOME", stage_home)
                .arg("--std-root")
                .arg(stage_std)
                .arg("check")
                .arg(&source),
            Duration::from_secs(30),
        )?;
        if !output.status.success() {
            return Err(CliError::CommandFailed(format!(
                "staged std validation failed for '{}'\nstdout: {}\nstderr: {}",
                source.display(),
                String::from_utf8_lossy(&output.stdout).trim(),
                String::from_utf8_lossy(&output.stderr).trim()
            )));
        }
    }
    Ok(())
}

fn collect_wave_sources(dir: &Path, out: &mut Vec<PathBuf>) -> Result<(), CliError> {
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        let ty = entry.file_type()?;
        if ty.is_dir() {
            collect_wave_sources(&path, out)?;
        } else if ty.is_file() && path.extension().is_some_and(|ext| ext == "wave") {
            out.push(path);
        }
    }
    Ok(())
}

fn replace_std_tree(staged: &Path, install_dir: &Path) -> Result<(), CliError> {
    if !install_dir.exists() {
        fs::rename(staged, install_dir)?;
        return Ok(());
    }

    let parent = install_dir.parent().ok_or_else(|| {
        CliError::CommandFailed(format!(
            "std installation path '{}' has no parent",
            install_dir.display()
        ))
    })?;
    let backup = unique_path_in(parent, ".std-backup");
    fs::rename(install_dir, &backup)?;

    if let Err(install_error) = fs::rename(staged, install_dir) {
        return match fs::rename(&backup, install_dir) {
            Ok(()) => Err(CliError::CommandFailed(format!(
                "failed to activate staged std; previous installation was restored: {}",
                install_error
            ))),
            Err(restore_error) => Err(CliError::CommandFailed(format!(
                "failed to activate staged std ({}) and failed to restore '{}' ({})",
                install_error,
                backup.display(),
                restore_error
            ))),
        };
    }

    let _ = fs::remove_dir_all(backup);
    Ok(())
}

fn resolve_std_install_dir() -> Result<PathBuf, CliError> {
    utils::paths::std_root_dir().ok_or(CliError::HomeNotSet)
}

fn reject_links_if_present(path: &Path) -> Result<(), CliError> {
    match fs::symlink_metadata(path) {
        Ok(_) => reject_links(path),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error.into()),
    }
}

fn reject_links(root: &Path) -> Result<(), CliError> {
    let mut pending = vec![root.to_path_buf()];
    while let Some(path) = pending.pop() {
        let metadata = fs::symlink_metadata(&path)?;
        if metadata.file_type().is_symlink() || (!metadata.is_dir() && !metadata.is_file()) {
            return Err(CliError::CommandFailed(format!(
                "unsupported std input entry: '{}'",
                path.display()
            )));
        }
        if metadata.is_dir() {
            for entry in fs::read_dir(&path)? {
                pending.push(entry?.path());
            }
        }
    }
    Ok(())
}

fn copy_dir_all(src: &Path, dst: &Path) -> Result<(), CliError> {
    fs::create_dir_all(dst)?;
    for entry in fs::read_dir(src)? {
        process::check_deadline()?;
        let entry = entry?;
        let ty = entry.file_type()?;
        let from = entry.path();
        let to = dst.join(entry.file_name());

        if ty.is_symlink() {
            return Err(CliError::CommandFailed(format!(
                "std input contains a symbolic link: '{}'",
                from.display()
            )));
        } else if ty.is_dir() {
            copy_dir_all(&from, &to)?;
        } else if ty.is_file() {
            if let Some(parent) = to.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::copy(&from, &to)?;
        }
    }
    Ok(())
}

fn run_cmd(cmd: &mut Command, label: &str) -> Result<(), CliError> {
    let out = process::output(cmd, Duration::from_secs(300))?;
    if out.status.success() {
        Ok(())
    } else {
        let stderr = String::from_utf8_lossy(&out.stderr).trim().to_string();
        let stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        Err(CliError::CommandFailed(format!(
            "{} (status={})\nstdout: {}\nstderr: {}",
            label, out.status, stdout, stderr
        )))
    }
}

fn run_cmd_stdout(cmd: &mut Command, label: &str) -> Result<String, CliError> {
    let out = process::output(cmd, Duration::from_secs(300))?;
    if !out.status.success() {
        let stderr = String::from_utf8_lossy(&out.stderr).trim().to_string();
        let stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        return Err(CliError::CommandFailed(format!(
            "{} (status={})\nstdout: {}\nstderr: {}",
            label, out.status, stdout, stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
    if stdout.is_empty() {
        return Err(CliError::CommandFailed(format!("{} returned empty output", label)));
    }
    Ok(stdout)
}

fn make_tmp_dir(prefix: &str) -> Result<PathBuf, CliError> {
    make_tmp_dir_in(&env::temp_dir(), prefix)
}

fn unique_path_in(parent: &Path, prefix: &str) -> PathBuf {
    let timestamp = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
    parent.join(format!("{}-{}-{}", prefix, std::process::id(), timestamp))
}

fn make_tmp_dir_in(parent: &Path, prefix: &str) -> Result<PathBuf, CliError> {
    let path = unique_path_in(parent, prefix);
    let builder = fs::DirBuilder::new();
    #[cfg(unix)]
    let builder = {
        use std::os::unix::fs::DirBuilderExt;
        let mut builder = builder;
        builder.mode(0o700);
        builder
    };
    builder.create(&path)?;
    Ok(path)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pinned_fetch_and_failed_updates_preserve_the_installed_tree() {
        let root = make_tmp_dir("wave-std-pinned").unwrap();
        let repository = root.join("repo");
        fs::create_dir_all(repository.join("std")).unwrap();
        let git = |args: &[&str]| {
            let out = Command::new("git")
                .current_dir(&repository)
                .args([
                    "-c",
                    "user.name=Wave tests",
                    "-c",
                    "user.email=tests@example.invalid",
                    "-c",
                    "commit.gpgsign=false",
                ])
                .args(args)
                .output()
                .unwrap();
            assert!(out.status.success(), "{out:?}");
            String::from_utf8(out.stdout).unwrap().trim().to_owned()
        };
        git(&["init"]);
        let manifest = repository.join("std/manifest.json");
        fs::write(
            &manifest,
            format!(
                r#"{{"name":"std","format":1,"compatibility_revision":{}}}"#,
                parser::import::STD_COMPATIBILITY_REVISION
            ),
        )
        .unwrap();
        fs::write(repository.join("std/version.wave"), "old").unwrap();
        git(&["add", "."]);
        git(&["commit", "-m", "compatible"]);
        let pinned = git(&["rev-parse", "HEAD"]);
        git(&["tag", "compatible"]);
        fs::write(&manifest, r#"{"name":"std","format":1,"compatibility_revision":999999}"#)
            .unwrap();
        fs::write(repository.join("std/version.wave"), "new").unwrap();
        git(&["add", "."]);
        git(&["commit", "-m", "incompatible"]);
        let installed = root.join("install/std");
        let repo = repository.to_str().unwrap();
        install_from_repository(repo, &pinned, &installed, |_, _| Ok(())).unwrap();
        assert_eq!(fs::read_to_string(installed.join("version.wave")).unwrap(), "old");
        assert!(fs::read_to_string(installed.join("INSTALL_META"))
            .unwrap()
            .contains(&format!("revision={pinned}")));
        for reference in ["HEAD", "missing-reference"] {
            assert!(install_from_repository(repo, reference, &installed, |_, _| Ok(())).is_err());
            assert_eq!(fs::read_to_string(installed.join("version.wave")).unwrap(), "old");
        }
        assert!(install_from_repository(repo, "compatible", &installed, |_, _| Err(
            CliError::CommandFailed("validation failed".into())
        ))
        .is_err());
        assert!(install_from_repository(
            root.join("missing-repository").to_str().unwrap(),
            &pinned,
            &installed,
            |_, _| Ok(())
        )
        .is_err());
        assert_eq!(fs::read_to_string(installed.join("version.wave")).unwrap(), "old");
        assert_eq!(
            fs::read_dir(installed.parent().unwrap())
                .unwrap()
                .filter(|entry| entry.as_ref().unwrap().file_name() != ".std-install.lock")
                .count(),
            1
        );
        install_from_repository(repo, "compatible", &installed, |_, stage| {
            assert_eq!(fs::read_to_string(stage.join("version.wave"))?, "old");
            Ok(())
        })
        .unwrap();
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn std_reference_defaults_to_the_recorded_immutable_revision() {
        // Exercise both Git checkouts and source archives in every test run,
        // rather than branching on a build-time constant.
        let revision = "0123456789abcdef0123456789abcdef01234567";
        assert_eq!(select_std_reference(None, revision).unwrap(), revision);
        assert!(select_std_reference(None, "").is_err());
        assert_eq!(select_std_reference(Some("release-tag"), "").unwrap(), "release-tag");
        assert_eq!(resolve_std_reference(Some("release-tag")).unwrap(), "release-tag");
        for value in ["", "--all", "head:local", "two refs"] {
            assert!(resolve_std_reference(Some(value)).is_err());
        }
    }

    #[test]
    fn successful_replacement_exposes_only_the_staged_tree() {
        let root = make_tmp_dir("wave-std-replace-success").unwrap();
        let installed = root.join("std");
        let staged = root.join("staged");
        fs::create_dir_all(&installed).unwrap();
        fs::create_dir_all(&staged).unwrap();
        fs::write(installed.join("old.wave"), "old").unwrap();
        fs::write(staged.join("new.wave"), "new").unwrap();

        replace_std_tree(&staged, &installed).unwrap();

        assert!(!installed.join("old.wave").exists());
        assert_eq!(fs::read_to_string(installed.join("new.wave")).unwrap(), "new");
        assert!(!staged.exists());
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn failed_activation_restores_the_previous_installation() {
        let root = make_tmp_dir("wave-std-replace-rollback").unwrap();
        let installed = root.join("std");
        let missing_stage = root.join("missing-stage");
        fs::create_dir_all(&installed).unwrap();
        fs::write(installed.join("old.wave"), "old").unwrap();

        let error = replace_std_tree(&missing_stage, &installed).unwrap_err();

        assert!(error.message().contains("previous installation was restored"));
        assert_eq!(fs::read_to_string(installed.join("old.wave")).unwrap(), "old");
        let _ = fs::remove_dir_all(root);
    }
}

#[cfg(test)]
mod transaction_tests {
    use super::*;
    #[test]
    fn same_root_is_busy_before_git_or_staging_and_different_roots_are_independent() {
        let root = make_tmp_dir("wave-std-lock").unwrap();
        let installed = root.join("std");
        let lock_path = root.join(".std-install.lock");
        let lock = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create_new(true)
            .open(&lock_path)
            .unwrap();
        lock.try_lock().unwrap();
        let lock = InstallationLock(lock);
        let result = install_from_repository("missing", "master", &installed, |_, _| {
            panic!("validation cannot start")
        });
        assert!(matches!(result, Err(CliError::InstallationBusy { .. })));
        assert_eq!(fs::read_dir(&root).unwrap().count(), 1);
        let another = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create_new(true)
            .open(root.join(".another-install.lock"))
            .unwrap();
        another.try_lock().unwrap();
        let another = InstallationLock(another);
        drop(lock);
        let next = fs::OpenOptions::new().read(true).write(true).open(lock_path).unwrap();
        next.try_lock().unwrap();
        let next = InstallationLock(next);
        drop(next);
        drop(another);
        fs::remove_dir_all(root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn aliased_parent_uses_the_same_lock_and_all_module_links_are_rejected() {
        use std::os::unix::fs::symlink;
        let root = make_tmp_dir("wave-std-links").unwrap();
        let physical = root.join("physical");
        fs::create_dir(&physical).unwrap();
        let alias = root.join("alias");
        symlink(&physical, &alias).unwrap();
        let lock = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create_new(true)
            .open(physical.join(".std-install.lock"))
            .unwrap();
        lock.try_lock().unwrap();
        let lock = InstallationLock(lock);
        assert!(matches!(
            install_from_repository("unused", "master", &alias.join("std"), |_, _| Ok(())),
            Err(CliError::InstallationBusy { .. })
        ));
        for target in ["real", "missing", "link", "/etc/passwd", "."] {
            let tree = physical.join("input");
            fs::create_dir(&tree).unwrap();
            fs::write(tree.join("real"), "source").unwrap();
            symlink(target, tree.join("link")).unwrap();
            assert!(reject_links(&tree).is_err());
            fs::remove_dir_all(&tree).unwrap();
        }
        drop(lock);
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn lock_process_helper() {
        let Some(path) = env::var_os("WAVE_TEST_INSTALL_LOCK_HELPER") else { return };
        let path = PathBuf::from(path);
        let lock =
            fs::OpenOptions::new().read(true).write(true).create_new(true).open(&path).unwrap();
        lock.try_lock().unwrap();
        fs::write(path.with_extension("ready"), "ready").unwrap();
        loop {
            std::thread::sleep(Duration::from_secs(1));
        }
    }

    #[test]
    fn abnormal_process_exit_releases_the_os_lock() {
        let root = make_tmp_dir("wave-std-abnormal-lock").unwrap();
        let path = root.join(".std-install.lock");
        let mut child = Command::new(env::current_exe().unwrap())
            .args(["--exact", "std::transaction_tests::lock_process_helper"])
            .env("WAVE_TEST_INSTALL_LOCK_HELPER", &path)
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .spawn()
            .unwrap();
        let ready = path.with_extension("ready");
        let deadline = std::time::Instant::now() + Duration::from_secs(10);
        while !ready.exists() && std::time::Instant::now() < deadline {
            if child.try_wait().unwrap().is_some() {
                break;
            }
            std::thread::sleep(Duration::from_millis(5));
        }
        let acquired = ready.exists();
        let _ = child.kill();
        child.wait().unwrap();
        assert!(acquired, "child must hold the lock before abnormal termination");
        let lock = fs::OpenOptions::new().read(true).write(true).open(path).unwrap();
        lock.try_lock().unwrap();
        drop(lock);
        fs::remove_dir_all(root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn parent_replacement_and_late_links_preserve_installed_and_foreign_files() {
        let root = make_tmp_dir("wave-std-parent-replace").unwrap();
        let repository = root.join("repo");
        fs::create_dir_all(repository.join("std")).unwrap();
        fs::write(
            repository.join("std/manifest.json"),
            format!(
                r#"{{"name":"std","format":1,"compatibility_revision":{}}}"#,
                parser::import::STD_COMPATIBILITY_REVISION,
            ),
        )
        .unwrap();
        fs::write(repository.join("std/module.wave"), "new").unwrap();
        for args in [vec!["init"], vec!["add", "."], vec!["commit", "-m", "fixture"]] {
            assert!(Command::new("git")
                .current_dir(&repository)
                .args([
                    "-c",
                    "user.name=Wave tests",
                    "-c",
                    "user.email=tests@example.invalid",
                    "-c",
                    "commit.gpgsign=false"
                ])
                .args(args)
                .output()
                .unwrap()
                .status
                .success());
        }
        let parent = root.join("install");
        let installed = parent.join("std");
        fs::create_dir_all(&installed).unwrap();
        fs::write(installed.join("old.wave"), "old").unwrap();
        let repo = repository.to_str().unwrap();
        #[cfg(target_os = "linux")]
        let mut inherited_lock = None;
        let error = install_from_repository(repo, "HEAD", &installed, |_, stage| {
            #[cfg(target_os = "linux")]
            {
                // Model a concurrent fork retaining the lock's open-file
                // description before exec closes its CLOEXEC descriptor.
                let lock_path = fs::canonicalize(parent.join(".std-install.lock"))?;
                let descriptor = fs::read_dir("/proc/self/fd")?
                    .filter_map(Result::ok)
                    .find(|entry| fs::read_link(entry.path()).is_ok_and(|path| path == lock_path))
                    .expect("the active transaction owns its lock descriptor")
                    .file_name()
                    .to_string_lossy()
                    .parse::<i32>()
                    .unwrap();
                // SAFETY: this is the active transaction's descriptor. Its
                // File stays alive until this validation callback returns.
                let borrowed = unsafe { std::os::fd::BorrowedFd::borrow_raw(descriptor) };
                inherited_lock = Some(borrowed.try_clone_to_owned()?);
            }
            std::os::unix::fs::symlink("module.wave", stage.join("late-link"))?;
            Ok(())
        })
        .unwrap_err();
        assert!(error.message().contains("unsupported std input entry"), "{}", error.message());
        assert_eq!(fs::read_to_string(installed.join("old.wave")).unwrap(), "old");
        let moved = root.join("original-install");
        let mut foreign_stage = PathBuf::new();
        let error = install_from_repository(repo, "HEAD", &installed, |home, _| {
            let name = home.file_name().unwrap();
            fs::rename(&parent, &moved)?;
            fs::create_dir(&parent)?;
            foreign_stage = parent.join(name);
            fs::create_dir(&foreign_stage)?;
            fs::write(foreign_stage.join("foreign"), "preserve")?;
            Ok(())
        })
        .unwrap_err();
        assert!(error.message().contains("parent changed"), "{}", error.message());
        assert_eq!(fs::read_to_string(moved.join("std/old.wave")).unwrap(), "old");
        assert_eq!(fs::read_to_string(foreign_stage.join("foreign")).unwrap(), "preserve");
        assert!(!installed.exists());
        #[cfg(target_os = "linux")]
        drop(inherited_lock);
        fs::remove_dir_all(root).unwrap();
    }
}
