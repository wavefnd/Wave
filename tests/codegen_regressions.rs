// SPDX-License-Identifier: MPL-2.0
// Keep the Cargo integration test target stable: CI and release workflows
// select tests with `--test codegen_regressions <name filter>`.
include!("codegen/support.rs");

#[path = "codegen/abi.rs"]
mod abi;
#[path = "codegen/asm_results.rs"]
mod asm_results;
#[path = "codegen/diagnostics.rs"]
mod diagnostics;
#[path = "codegen/language.rs"]
mod language;
#[path = "codegen/platform.rs"]
mod platform;
