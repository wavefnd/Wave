use std::collections::HashSet;
use std::path::Path;

use error::error::WaveError;
use parser::ast::{ASTNode, StatementNode};
use parser::parser::import::local_import_unit;

fn expand_imports_recursive(
    ast: Vec<ASTNode>,
    current_file_dir: &Path,
    already: &mut HashSet<String>,
) -> Result<Vec<ASTNode>, WaveError> {
    let mut out = Vec::new();

    for node in ast {
        match node {
            ASTNode::Statement(StatementNode::Import(path)) => {
                let imported = local_import_unit(&path, already, current_file_dir)?;

                let next_dir = imported.abs_path.parent().unwrap_or(current_file_dir);

                let expanded = expand_imports_recursive(imported.ast, next_dir, already)?;
                out.extend(expanded);

            }
            other => out.push(other),
        }
    }

    Ok(out)
}

pub fn build_codegen_ast(entry_path: &Path, entry_ast: Vec<ASTNode>) -> Result<Vec<ASTNode>, WaveError> {
    let mut already = HashSet::new();

    if let Ok(abs) = entry_path.canonicalize() {
        if let Some(s) = abs.to_str() {
            already.insert(s.to_string());
        }
    }

    let entry_dir = entry_path.parent().unwrap_or(Path::new("."));
    expand_imports_recursive(entry_ast, entry_dir, &mut already)
}
