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

//! Standard-library locations used by installation, imports and CLI output.

use std::ffi::OsString;
use std::io::ErrorKind;
use std::path::PathBuf;

/// The writable per-user installation destination. Installing or updating std
/// must not modify the library shipped beside a compiler executable.
pub fn std_root_dir() -> Option<PathBuf> {
    home_dir_from(cfg!(windows), |name| std::env::var_os(name))
        .map(|home| home.join(".wave/lib/wave/std"))
}

/// Prefer the std shipped beside the running compiler, then the legacy per-user
/// installation. An existing but invalid bundled tree is still selected so its
/// manifest or compatibility error cannot be hidden by another installation.
pub fn default_std_root_dir() -> Option<PathBuf> {
    let bundled = std::env::current_exe()
        .ok()
        .and_then(|executable| executable.canonicalize().ok())
        .and_then(|executable| executable.parent().map(|parent| parent.join("std")));
    bundled
        .filter(|path| match std::fs::symlink_metadata(path) {
            Ok(_) => true,
            Err(error) => error.kind() != ErrorKind::NotFound,
        })
        .or_else(std_root_dir)
}

pub fn missing_home_message() -> &'static str {
    if cfg!(windows) {
        "HOME or USERPROFILE environment variable not set"
    } else {
        "HOME environment variable not set"
    }
}

fn home_dir_from(windows: bool, mut get: impl FnMut(&str) -> Option<OsString>) -> Option<PathBuf> {
    let mut nonempty = |name| get(name).filter(|value| !value.is_empty());
    // HOME also provides the explicit override used by staged std installation.
    nonempty("HOME")
        .or_else(|| if windows { nonempty("USERPROFILE") } else { None })
        .map(PathBuf::from)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn windows_uses_the_profile_when_home_is_unset_or_empty() {
        for home in [None, Some(OsString::new())] {
            let path = home_dir_from(true, |key| match key {
                "HOME" => home.clone(),
                "USERPROFILE" => Some("/users/wave user".into()),
                _ => None,
            });
            assert_eq!(path, Some(PathBuf::from("/users/wave user")));
        }
    }

    #[test]
    fn explicit_home_wins_for_staged_installation() {
        for windows in [false, true] {
            let path = home_dir_from(windows, |key| match key {
                "HOME" => Some("/staging".into()),
                "USERPROFILE" => Some("/users/wave".into()),
                _ => None,
            });
            assert_eq!(path, Some(PathBuf::from("/staging")));
        }
    }

    #[test]
    fn unix_does_not_fall_back_to_a_windows_profile() {
        let path = home_dir_from(false, |key| (key == "USERPROFILE").then(|| "/users/wave".into()));
        assert_eq!(path, None);
        assert_eq!(home_dir_from(true, |_| None), None);
        assert_eq!(home_dir_from(true, |_| Some(OsString::new())), None);
    }
}
