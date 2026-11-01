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

//! Errors produced while parsing or executing the `wavec` command line.
//!
//! CLI failures are separate from source diagnostics because they have no Wave
//! source span. Stable `kind` and exit-code mappings are shared by human and JSON
//! output.

use utils::wson::{self, Value};

use std::fmt;
use std::path::PathBuf;

#[derive(Debug)]
pub enum CliError {
    Usage(String),
    Backend(llvm::diagnostic::CodegenError),

    // std
    StdAlreadyInstalled { path: PathBuf },
    InstallationBusy { path: PathBuf },
    InstallationTimeout(String),
    InstallationOutputLimit(String),
    ExternalToolMissing(String),
    CommandFailed(String),
    HomeNotSet,

    // io
    Io(std::io::Error),
}

impl CliError {
    pub fn usage(msg: impl Into<String>) -> Self {
        CliError::Usage(msg.into())
    }

    pub fn kind(&self) -> &'static str {
        match self {
            CliError::Usage(_) => "usage",
            CliError::Backend(error) => {
                if error.kind == llvm::diagnostic::CodegenErrorKind::MissingTool {
                    "external-tool-missing"
                } else {
                    "command-failed"
                }
            },
            CliError::StdAlreadyInstalled { .. } => "std-already-installed",
            CliError::InstallationBusy { .. } => "installation-busy",
            CliError::InstallationTimeout(_) => "installation-timeout",
            CliError::InstallationOutputLimit(_) => "installation-output-limit",
            CliError::ExternalToolMissing(_) => "external-tool-missing",
            CliError::CommandFailed(_) => "command-failed",
            CliError::HomeNotSet => "home-not-set",
            CliError::Io(_) => "io",
        }
    }

    pub fn message(&self) -> String {
        match self {
            CliError::Usage(msg) => msg.clone(),
            CliError::Backend(error) => error.to_string(),
            CliError::StdAlreadyInstalled { path } => {
                format!("std already installed at '{}'", path.display())
            },
            CliError::InstallationBusy { path } => {
                format!("std installation already in progress at '{}'", path.display())
            },
            CliError::InstallationTimeout(message) => {
                format!("std installation timed out: {message}")
            },
            CliError::InstallationOutputLimit(message) => {
                format!("std installation output limit exceeded: {message}")
            },
            CliError::ExternalToolMissing(t) => format!("required tool not found: {}", t),
            CliError::CommandFailed(msg) => format!("command failed: {}", msg),
            CliError::HomeNotSet => utils::paths::missing_home_message().to_string(),
            CliError::Io(e) => e.to_string(),
        }
    }

    pub fn to_json(&self) -> String {
        let mut fields = vec![
            ("kind", Value::string(self.kind())),
            ("message", Value::string(self.message())),
            ("exit_code", Value::integer(self.exit_code() as u64)),
        ];
        if let Self::Backend(error) = self {
            fields.push(("phase", Value::string(error.phase.to_string())));
            fields.push(("operation", Value::string(&error.operation)));
        }
        wson::dumps(&Value::object([("error", Value::object(fields))]), wson::Format::Json, false)
            .expect("CLI diagnostic schema contains only shallow JSON values")
    }

    pub fn exit_code(&self) -> i32 {
        match self {
            CliError::Usage(_) => 2,
            CliError::Backend(error) => {
                if error.kind == llvm::diagnostic::CodegenErrorKind::MissingTool {
                    3
                } else {
                    1
                }
            },
            CliError::ExternalToolMissing(_) | CliError::HomeNotSet | CliError::Io(_) => 3,
            CliError::StdAlreadyInstalled { .. }
            | CliError::CommandFailed(_)
            | CliError::InstallationBusy { .. }
            | CliError::InstallationTimeout(_)
            | CliError::InstallationOutputLimit(_) => 1,
        }
    }
}

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let prefix = if matches!(self, CliError::Io(_)) { "IO Error" } else { "Error" };
        write!(f, "{prefix}: {}", utils::display_width::terminal_text(&self.message()))
    }
}

impl From<std::io::Error> for CliError {
    fn from(e: std::io::Error) -> Self {
        CliError::Io(e)
    }
}

impl From<llvm::diagnostic::CodegenError> for CliError {
    fn from(error: llvm::diagnostic::CodegenError) -> Self {
        Self::Backend(error)
    }
}
