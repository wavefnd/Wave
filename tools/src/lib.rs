// SPDX-License-Identifier: MPL-2.0
//! Native maintenance, validation and distribution contracts. No LLVM dependency.
pub mod binary;
pub mod build;
pub mod cases;
pub mod checks;
pub mod ci;
pub mod cli;
pub mod config;
pub mod freebsd;
pub mod maintenance;
pub mod native;
pub mod package;
pub mod process;
pub mod provision;
pub mod publication;
pub mod runner;
pub mod sdk;
pub mod source;
pub mod storage;
pub mod wasm;
pub mod windows;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

pub fn error(message: impl Into<String>) -> Box<dyn std::error::Error + Send + Sync> {
    message.into().into()
}

pub fn root() -> std::path::PathBuf {
    std::env::var_os("WAVE_REPOSITORY").map(std::path::PathBuf::from).unwrap_or_else(|| {
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap().into()
    })
}
