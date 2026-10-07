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

//! Publish an MSVC image and its linker-created companions as one output set.
//!
//! Staging keeps each final basename: import libraries embed the DLL basename,
//! so renaming only a randomly named temporary DLL after linking is insufficient.

use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

struct Output {
    staged: PathBuf,
    destination: PathBuf,
    backup: PathBuf,
    backed_up: bool,
    published: bool,
}

pub(crate) struct MsvcOutputs {
    outputs: Vec<Output>,
    directories: Vec<(PathBuf, PathBuf)>,
    preserve_backups: bool,
}

impl MsvcOutputs {
    pub(crate) fn prepare(output: &Path, args: &mut Vec<String>) -> io::Result<Self> {
        let mut set =
            Self { outputs: Vec::new(), directories: Vec::new(), preserve_backups: false };
        let image = set.stage(output)?;
        for arg in args.iter_mut() {
            if option_value(arg, "OUT").is_some() {
                *arg = format!("/OUT:{}", image.display());
            }
        }
        // LINK derives the .exp basename from the import library, when it emits
        // that companion. Both share their final directory and basename here.
        let library = option_path(args, "IMPLIB").unwrap_or_else(|| output.with_extension("lib"));
        set.rewrite_path(args, "IMPLIB", &library)?;
        set.stage(&library.with_extension("exp"))?;
        let pdb = option_path(args, "PDB").unwrap_or_else(|| output.with_extension("pdb"));
        if !pdb.to_str().is_some_and(|name| name.eq_ignore_ascii_case("NONE")) {
            set.rewrite_path(args, "PDB", &pdb)?;
            if !args.iter().any(|arg| option_value(arg, "PDBALTPATH").is_some()) {
                let final_pdb = set.outputs.last().unwrap().destination.to_string_lossy();
                args.push(format!("/PDBALTPATH:{final_pdb}"));
            }
        }
        for (option, default) in [
            ("ILK", output.with_extension("ilk")),
            ("MAP", output.with_extension("map")),
            ("MANIFESTFILE", PathBuf::from(format!("{}.manifest", output.display()))),
        ] {
            if args.iter().any(|arg| {
                option_value(arg, option).is_some()
                    || arg.eq_ignore_ascii_case(&format!("/{option}"))
            }) {
                let destination = option_path(args, option).unwrap_or(default);
                set.rewrite_path(args, option, &destination)?;
            } else {
                // Implicit companions remain beside the staged image.
                set.stage(&default)?;
            }
        }
        Ok(set)
    }

    fn rewrite_path(
        &mut self,
        args: &mut Vec<String>,
        option: &str,
        destination: &Path,
    ) -> io::Result<()> {
        let staged = self.stage(destination)?;
        args.retain(|arg| {
            option_value(arg, option).is_none() && !arg.eq_ignore_ascii_case(&format!("/{option}"))
        });
        args.push(format!("/{option}:{}", staged.display()));
        Ok(())
    }

    fn stage(&mut self, destination: &Path) -> io::Result<PathBuf> {
        let filename = destination.file_name().ok_or_else(|| {
            io::Error::new(io::ErrorKind::InvalidInput, "link output must name a file")
        })?;
        let parent =
            destination.parent().filter(|p| !p.as_os_str().is_empty()).unwrap_or(Path::new("."));
        fs::create_dir_all(parent)?;
        let parent = fs::canonicalize(parent)?;
        let destination = parent.join(filename);
        if self.outputs.iter().any(|out| out.destination == destination) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("link outputs share the same destination: {}", destination.display()),
            ));
        }
        let directory =
            if let Some((_, staged)) = self.directories.iter().find(|(path, _)| path == &parent) {
                staged.clone()
            } else {
                static NEXT: AtomicU64 = AtomicU64::new(0);
                let mut selected = None;
                for _ in 0..128 {
                    let path = parent.join(format!(
                        ".wave-link-{}-{}",
                        std::process::id(),
                        NEXT.fetch_add(1, Ordering::Relaxed)
                    ));
                    match fs::create_dir(&path) {
                        Ok(()) => {
                            selected = Some(path);
                            break;
                        },
                        Err(e) if e.kind() == io::ErrorKind::AlreadyExists => continue,
                        Err(e) => return Err(e),
                    }
                }
                let staged = selected.ok_or_else(|| {
                    io::Error::new(io::ErrorKind::AlreadyExists, "link staging directory collision")
                })?;
                self.directories.push((parent, staged.clone()));
                staged
            };
        let staged = directory.join(filename);
        let backup_dir = directory.join(".backups");
        fs::create_dir_all(&backup_dir)?;
        let backup = backup_dir.join(self.outputs.len().to_string());
        self.outputs.push(Output {
            staged: staged.clone(),
            destination,
            backup,
            backed_up: false,
            published: false,
        });
        Ok(staged)
    }

    pub(crate) fn commit(mut self) -> io::Result<()> {
        if fs::metadata(&self.outputs[0].staged)?.len() == 0 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "linker emitted an empty image",
            ));
        }
        for output in &self.outputs {
            if !output.staged.exists() {
                continue;
            }
            if !output.staged.is_file() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "link output is not a file",
                ));
            }
            if output.destination.exists() && !output.destination.is_file() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("link destination is not a file: {}", output.destination.display()),
                ));
            }
        }
        // Validate the entire set first, and retain replaced files until every
        // rename has succeeded so publication errors can restore the old set.
        let result: io::Result<()> = (|| {
            for output in &mut self.outputs {
                if !output.staged.exists() {
                    continue;
                }
                if output.destination.exists() {
                    fs::rename(&output.destination, &output.backup)?;
                    output.backed_up = true;
                }
                fs::rename(&output.staged, &output.destination)?;
                output.published = true;
            }
            Ok(())
        })();
        if let Err(error) = result {
            for output in self.outputs.iter().rev() {
                if output.published {
                    if let Err(rollback) = fs::remove_file(&output.destination) {
                        self.preserve_backups = true;
                        return Err(io::Error::other(format!(
                            "{error}; rollback failed: {rollback}; backups retained at {}",
                            output.backup.display()
                        )));
                    }
                }
                if output.backed_up {
                    if let Err(rollback) = fs::rename(&output.backup, &output.destination) {
                        self.preserve_backups = true;
                        return Err(io::Error::other(format!(
                            "{error}; rollback failed: {rollback}; backups retained at {}",
                            output.backup.display()
                        )));
                    }
                }
            }
            return Err(error);
        }
        Ok(())
    }
}

fn option_value<'a>(arg: &'a str, option: &str) -> Option<&'a str> {
    let (name, value) = arg.strip_prefix('/').or_else(|| arg.strip_prefix('-'))?.split_once(':')?;
    name.eq_ignore_ascii_case(option).then_some(value)
}

fn option_path(args: &[String], option: &str) -> Option<PathBuf> {
    args.iter().rev().find_map(|arg| option_value(arg, option).map(PathBuf::from))
}

impl Drop for MsvcOutputs {
    fn drop(&mut self) {
        if !self.preserve_backups {
            for (_, directory) in &self.directories {
                let _ = fs::remove_dir_all(directory);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn directory(name: &str) -> PathBuf {
        static NEXT: AtomicU64 = AtomicU64::new(0);
        let path = std::env::temp_dir().join(format!(
            "wave-link-output-test-{name}-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir(&path).unwrap();
        path
    }

    #[test]
    fn publication_error_restores_previously_replaced_companions() {
        let root = directory("rollback");
        let dll = root.join("answer.dll");
        let library = root.join("answer.lib");
        fs::write(&dll, b"old image").unwrap();
        fs::write(&library, b"old import library").unwrap();
        let mut args = vec![format!("/OUT:{}", dll.display())];
        let pending = MsvcOutputs::prepare(&dll, &mut args).unwrap();
        fs::write(&pending.outputs[0].staged, b"new image").unwrap();
        fs::write(&pending.outputs[1].staged, b"new import library").unwrap();
        // Force a filesystem failure after the first artifact was published.
        fs::create_dir(&pending.outputs[1].backup).unwrap();
        fs::write(pending.outputs[1].backup.join("occupied"), b"occupied").unwrap();
        assert!(pending.commit().is_err());
        assert_eq!(fs::read(&dll).unwrap(), b"old image");
        assert_eq!(fs::read(&library).unwrap(), b"old import library");
        assert_eq!(fs::read_dir(&root).unwrap().count(), 2);
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn aliasing_output_paths_are_rejected_before_linking() {
        let root = directory("alias");
        let dll = root.join("answer.dll");
        fs::write(&dll, b"old image").unwrap();
        let mut args =
            vec![format!("/OUT:{}", dll.display()), format!("/IMPLIB:{}", dll.display())];
        assert!(MsvcOutputs::prepare(&dll, &mut args).is_err());
        assert_eq!(fs::read(&dll).unwrap(), b"old image");
        assert_eq!(fs::read_dir(&root).unwrap().count(), 1);
        fs::remove_dir_all(root).unwrap();
    }
}
