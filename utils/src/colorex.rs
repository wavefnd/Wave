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

//! Minimal ANSI styling used by command-line diagnostics.
//!
//! All formatting honors the conventional `NO_COLOR`, `CLICOLOR`, and
//! `CLICOLOR_FORCE` environment variables. Styling methods therefore return
//! plain text when color is disabled and never make rendering a requirement.

pub struct Color(u8, u8, u8);

fn colors_enabled() -> bool {
    if std::env::var_os("NO_COLOR").is_some() {
        return false;
    }

    if std::env::var("CLICOLOR").as_deref() == Ok("0") {
        return false;
    }

    if std::env::var("CLICOLOR_FORCE").is_ok_and(|v| v != "0") {
        return true;
    }

    windows_ansi_supported()
}

#[cfg(windows)]
fn windows_ansi_supported() -> bool {
    if std::env::var_os("WT_SESSION").is_some()
        || std::env::var_os("ANSICON").is_some()
        || std::env::var("ConEmuANSI").is_ok_and(|v| v.eq_ignore_ascii_case("ON"))
    {
        return true;
    }

    std::env::var("TERM").is_ok_and(|term| {
        let term = term.to_ascii_lowercase();
        term.contains("xterm")
            || term.contains("ansi")
            || term.contains("cygwin")
            || term.contains("msys")
    })
}

#[cfg(not(windows))]
fn windows_ansi_supported() -> bool {
    true
}

impl Color {
    pub fn from_rgb(rgb: &str) -> Result<Color, &'static str> {
        let parts: Vec<&str> = rgb.split(',').collect();
        if parts.len() == 3 {
            let r = parts[0].parse::<u8>().map_err(|_| "Invalid RGB format")?;
            let g = parts[1].parse::<u8>().map_err(|_| "Invalid RGB format")?;
            let b = parts[2].parse::<u8>().map_err(|_| "Invalid RGB format")?;
            Ok(Color(r, g, b))
        } else {
            Err("Invalid RGB format")
        }
    }

    pub fn from_hex(hex: &str) -> Result<Color, &'static str> {
        if hex.len() != 7 || !hex.starts_with('#') {
            return Err("Invalid HEX format");
        }

        let r = u8::from_str_radix(&hex[1..3], 16).map_err(|_| "Invalid HEX value")?;
        let g = u8::from_str_radix(&hex[3..5], 16).map_err(|_| "Invalid HEX value")?;
        let b = u8::from_str_radix(&hex[5..7], 16).map_err(|_| "Invalid HEX value")?;

        Ok(Color(r, g, b))
    }
}

pub trait Colorize {
    fn color(self, color: &str) -> String;
    fn bg_color(self, color: &str) -> String;
    fn bold(self) -> String;
    fn italic(self) -> String;
    fn underline(self) -> String;
    fn strikethrough(self) -> String;
    fn dim(self) -> String;
    fn invert(self) -> String;
}

impl Colorize for &str {
    fn color(self, color: &str) -> String {
        if !colors_enabled() {
            return self.to_string();
        }

        let color =
            if color.starts_with('#') { Color::from_hex(color) } else { Color::from_rgb(color) };

        match color {
            Ok(c) => format!("\x1b[38;2;{};{};{}m{}\x1b[0m", c.0, c.1, c.2, self),
            Err(_) => self.to_string(),
        }
    }

    fn bg_color(self, color: &str) -> String {
        if !colors_enabled() {
            return self.to_string();
        }

        let color =
            if color.starts_with('#') { Color::from_hex(color) } else { Color::from_rgb(color) };

        match color {
            Ok(c) => format!("\x1b[48;2;{};{};{}m{}\x1b[0m", c.0, c.1, c.2, self),
            Err(_) => self.to_string(),
        }
    }

    fn bold(self) -> String {
        if !colors_enabled() {
            return self.to_string();
        }
        format!("\x1b[1m{}\x1b[0m", self)
    }

    fn italic(self) -> String {
        if !colors_enabled() {
            return self.to_string();
        }
        format!("\x1b[3m{}\x1b[0m", self)
    }

    fn underline(self) -> String {
        if !colors_enabled() {
            return self.to_string();
        }
        format!("\x1b[4m{}\x1b[0m", self)
    }

    fn strikethrough(self) -> String {
        if !colors_enabled() {
            return self.to_string();
        }
        format!("\x1b[9m{}\x1b[0m", self)
    }

    fn dim(self) -> String {
        if !colors_enabled() {
            return self.to_string();
        }
        format!("\x1b[2m{}\x1b[0m", self)
    }

    fn invert(self) -> String {
        if !colors_enabled() {
            return self.to_string();
        }
        format!("\x1b[7m{}\x1b[0m", self)
    }
}
