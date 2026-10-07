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
use std::time::{SystemTime, UNIX_EPOCH};
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
    install_from_repository(STD_REPOSITORY, reference, &install_dir, validate_staged_std)?;

    if is_update {
        println!("✅ std updated: {}", install_dir.display());
    } else {
        println!("✅ std installed: {}", install_dir.display());
    }

    Ok(())
}

fn install_from_repository(
    repository: &str,
    reference: &str,
    install_dir: &Path,
    validate: impl FnOnce(&Path, &Path) -> Result<(), CliError>,
) -> Result<(), CliError> {
    let install_parent = install_dir.parent().ok_or_else(|| {
        CliError::CommandFailed(format!(
            "std installation path '{}' has no parent",
            install_dir.display()
        ))
    })?;
    fs::create_dir_all(install_parent)?;

    // Keep staging beside the final directory so the final rename stays on one
    // filesystem. The installed tree is untouched until every check passes.
    let checkout = make_tmp_dir("wave-std-checkout")?;
    let stage_home = make_tmp_dir_in(install_parent, ".std-stage")?;
    let stage_std = stage_home.join(".wave/lib/wave/std");

    let result = (|| {
        let (src_std, source_revision) =
            fetch_std_from_wave_repo_sparse(&checkout, repository, reference)?;
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
        replace_std_tree(&stage_std, install_dir)
    })();

    let _ = fs::remove_dir_all(&checkout);
    let _ = fs::remove_dir_all(&stage_home);
    result
}

fn fetch_std_from_wave_repo_sparse(
    checkout: &Path,
    repository: &str,
    reference: &str,
) -> Result<(PathBuf, String), CliError> {
    if !tool_exists("git") {
        return Err(CliError::ExternalToolMissing("git".to_string()));
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
        let output = Command::new(&compiler)
            .env("HOME", stage_home)
            .arg("--std-root")
            .arg(stage_std)
            .arg("check")
            .arg(&source)
            .output()?;
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

fn copy_dir_all(src: &Path, dst: &Path) -> Result<(), CliError> {
    fs::create_dir_all(dst)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let ty = entry.file_type()?;
        let from = entry.path();
        let to = dst.join(entry.file_name());

        if ty.is_dir() {
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

fn tool_exists(name: &str) -> bool {
    Command::new(name).arg("--version").output().is_ok()
}

fn run_cmd(cmd: &mut Command, label: &str) -> Result<(), CliError> {
    let out = cmd.output()?;
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
    let out = cmd.output()?;
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
    fs::create_dir_all(&path)?;
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
        assert_eq!(fs::read_dir(installed.parent().unwrap()).unwrap().count(), 1);
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
