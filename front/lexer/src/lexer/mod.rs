#![allow(
    dead_code,
    unused_variables,
    clippy::module_inception,
    clippy::type_complexity,
    clippy::unnecessary_map_or,
    clippy::question_mark,
    clippy::result_large_err,
    clippy::ptr_arg,
    clippy::while_let_on_iterator,
    clippy::needless_borrow,
    clippy::useless_format,
    clippy::manual_strip,
    clippy::explicit_auto_deref,
    clippy::collapsible_if,
    clippy::collapsible_else_if,
    clippy::new_without_default
)]

mod core;
mod cursor;
mod trivia;
mod literals;
mod ident;
mod scan;

pub use core::{Lexer, Token};

