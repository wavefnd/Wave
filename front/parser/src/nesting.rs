// SPDX-License-Identifier: MPL-2.0
//! Independent parser budgets, restored on every return path.
use crate::parser::ParseError;
use lexer::Token;
use std::cell::Cell;

thread_local! {
    static BLOCK: Cell<usize> = const { Cell::new(0) };
    static TYPE: Cell<usize> = const { Cell::new(0) };
    static TYPE_EXCEEDED: Cell<bool> = const { Cell::new(false) };
}

pub(crate) struct Block;
impl Block {
    pub(crate) fn enter(token: Option<&Token>) -> Result<Self, ParseError> {
        BLOCK.with(|depth| {
            if depth.get() >= 128 {
                Err(ParseError::syntax_at(token, "block nesting exceeds the maximum of 128 levels"))
            } else {
                depth.set(depth.get() + 1);
                Ok(Self)
            }
        })
    }
}
impl Drop for Block {
    fn drop(&mut self) {
        BLOCK.with(|depth| depth.set(depth.get() - 1));
    }
}

pub(crate) struct Type;
impl Type {
    pub(crate) fn enter() -> Option<Self> {
        TYPE.with(|depth| {
            if depth.get() >= 128 {
                TYPE_EXCEEDED.with(|exceeded| exceeded.set(true));
                None
            } else {
                if depth.get() == 0 {
                    TYPE_EXCEEDED.with(|exceeded| exceeded.set(false));
                }
                depth.set(depth.get() + 1);
                Some(Self)
            }
        })
    }
}
impl Drop for Type {
    fn drop(&mut self) {
        TYPE.with(|depth| depth.set(depth.get() - 1));
    }
}

pub(crate) fn type_exceeded() -> bool {
    TYPE_EXCEEDED.with(|exceeded| exceeded.replace(false))
}

#[cfg(test)]
mod tests {
    #[test]
    fn rejected_parses_restore_counters_on_the_same_thread() {
        for _ in 0..3 {
            let source =
                format!("fun main() {{ {}{} }}", "if (true) {".repeat(128), "}".repeat(128));
            let tokens = lexer::Lexer::new_with_file(&source, "nested.wave").tokenize().unwrap();
            assert!(crate::parse_syntax_with_spans(&tokens)
                .unwrap_err()
                .message()
                .contains("block nesting"));
            assert!(crate::types::parse_type(&format!(
                "{}i32{}",
                "ptr<".repeat(128),
                ">".repeat(128)
            ))
            .is_none());
            let tokens =
                lexer::Lexer::new_with_file("fun main() { var p: ptr<i32>; }", "simple.wave")
                    .tokenize()
                    .unwrap();
            assert!(crate::parse_syntax_with_spans(&tokens).is_ok());
        }
    }
}
