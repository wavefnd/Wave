//! Structured diagnostics shared by the Wave frontend and compiler driver.
//!
//! Diagnostics retain source locations, labels, notes, and machine-readable
//! error codes so human and JSON renderers report the same underlying failure.

pub mod error;

pub use error::*;

pub mod span;
pub use span::SourceSpan;
