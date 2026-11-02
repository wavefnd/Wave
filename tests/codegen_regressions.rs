// SPDX-License-Identifier: MPL-2.0
// Keep the Cargo integration test target stable: CI and release workflows
// select tests with `--test codegen_regressions <name filter>`.
include!("codegen/support.rs");

#[path = "codegen/language.rs"]
mod language;
#[path = "codegen/abi.rs"]
mod abi;
#[path = "codegen/platform.rs"]
mod platform;
#[path = "codegen/diagnostics.rs"]
mod diagnostics;
