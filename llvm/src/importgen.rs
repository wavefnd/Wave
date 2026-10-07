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

//! Compatibility entry point for recursively expanding local imports before codegen.
//!
//! The compiler driver uses the richer parser import expansion that retains
//! source origins and dependency configuration. This helper remains for callers
//! that already provide a parsed entry AST and need local imports only.

use std::collections::HashSet;
use std::path::Path;

use error::error::WaveError;
use parser::ast::{ASTNode, StatementNode};
use parser::import::local_import_unit;

fn expand_imports_recursive(
    ast: Vec<ASTNode>,
    current_file_dir: &Path,
    already: &mut HashSet<String>,
) -> Result<Vec<ASTNode>, WaveError> {
    let mut out = Vec::new();

    for node in ast {
        match node {
            ASTNode::Statement(StatementNode::Import(import)) => {
                let imported = local_import_unit(&import.path, already, current_file_dir)?;

                let next_dir = imported.abs_path.parent().unwrap_or(current_file_dir);

                let expanded = expand_imports_recursive(imported.ast, next_dir, already)?;
                out.extend(expanded);
            },
            other => out.push(other),
        }
    }

    Ok(out)
}

pub fn build_codegen_ast(
    entry_path: &Path,
    entry_ast: Vec<ASTNode>,
) -> Result<Vec<ASTNode>, WaveError> {
    let mut already = HashSet::new();

    if let Ok(abs) = entry_path.canonicalize() {
        if let Some(s) = abs.to_str() {
            already.insert(s.to_string());
        }
    }

    let entry_dir = entry_path.parent().unwrap_or(Path::new("../.."));
    expand_imports_recursive(entry_ast, entry_dir, &mut already)
}
