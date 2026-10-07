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

//! Shared standard-library location used by installation, imports and CLI output.

use std::ffi::OsString;
use std::path::PathBuf;

pub fn std_root_dir() -> Option<PathBuf> {
    home_dir_from(cfg!(windows), |name| std::env::var_os(name))
        .map(|home| home.join(".wave/lib/wave/std"))
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
