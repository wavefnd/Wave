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

//! Process entry point for `wavec`.
//!
//! Argument handling lives in the library crate. This binary selects the error
//! renderer, prints usage for usage failures, and preserves the CLI error's
//! stable exit code.

use std::process;

fn main() {
    let json_errors = wavec::cli::args_request_json_errors(
        std::env::args_os().skip(1).map(|arg| arg.to_string_lossy().into_owned()),
    );

    if let Err(e) = wavec::cli::run() {
        if json_errors {
            eprintln!("{}", e.to_json());
        } else {
            eprintln!("{}", e);
        }
        if !json_errors && matches!(e, wavec::errors::CliError::Usage(_)) {
            wavec::cli::print_usage();
        }
        process::exit(e.exit_code());
    }
}
