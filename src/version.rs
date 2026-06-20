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

use std::process::Command;

const VERSION: &str = env!("CARGO_PKG_VERSION");

pub(crate) fn version() -> &'static str {
    VERSION
}

pub fn get_os_pretty_name() -> String {
    #[cfg(target_os = "linux")]
    {
        use std::fs;

        if let Ok(content) = fs::read_to_string("/etc/os-release") {
            for line in content.lines() {
                if line.starts_with("PRETTY_NAME=") {
                    return line
                        .trim_start_matches("PRETTY_NAME=")
                        .trim_matches('"')
                        .to_string();
                }
            }
        }

        // fallback to uname
        if let Ok(output) = Command::new("uname").arg("-sr").output() {
            let text = String::from_utf8_lossy(&output.stdout);
            return format!("Linux ({})", text.trim());
        }

        "Linux (unknown version)".to_string()
    }

    #[cfg(target_os = "windows")]
    {
        if let Ok(output) = Command::new("cmd").args(["/C", "ver"]).output() {
            let text = String::from_utf8_lossy(&output.stdout);
            return format!("Windows {}", text.trim());
        }

        return "Windows (unknown version)".to_string();
    }

    #[cfg(target_os = "macos")]
    {
        if let Ok(output) = Command::new("sw_vers").arg("-productVersion").output() {
            let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
            return format!("macOS {}", version);
        }

        "macOS (unknown version)".to_string()
    }

    #[cfg(not(any(target_os = "linux", target_os = "windows", target_os = "macos")))]
    {
        return std::env::consts::OS.to_string();
    }
}
