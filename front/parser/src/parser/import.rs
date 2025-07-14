use std::collections::HashSet;
use std::path::{Path, PathBuf};
use error::error::{WaveError, WaveErrorKind};
use crate::ast::ASTNode;
use crate::parse;
use lexer::Lexer;

pub fn local_import(
    path: &str,
    already_imported: &mut HashSet<String>,
    base_dir: &Path,
) -> Result<Vec<ASTNode>, WaveError> {
    if path.trim().is_empty() {
        return Err(WaveError::new(
            WaveErrorKind::SyntaxError("Empty import path".to_string()),
            "import path cannot be empty",
            "<main>",
            0,
            0,
        ));
    }

    let target_file_name = if path.ends_with(".wave") {
        path.to_string()
    } else {
        format!("{}.wave", path)
    };

    let found_path = base_dir.join(&target_file_name);
    if !found_path.exists() || !found_path.is_file() {
        return Err(WaveError::new(
            WaveErrorKind::SyntaxError("File not found".to_string()),
            format!("Could not find import target '{}'", target_file_name),
            target_file_name.clone(),
            0,
            0,
        ));
    }

    let abs_path = found_path.canonicalize().map_err(|e| {
        WaveError::new(
            WaveErrorKind::SyntaxError("Canonicalization failed".to_string()),
            format!("Failed to canonicalize path: {}", e),
            target_file_name.clone(),
            0,
            0,
        )
    })?;
    let abs_path_str = abs_path.to_str().ok_or_else(|| {
        WaveError::new(
            WaveErrorKind::UnexpectedChar('?'),
            "Invalid path encoding",
            target_file_name.clone(),
            0,
            0,
        )
    })?.to_string();

    if already_imported.contains(&abs_path_str) {
        return Ok(vec![]);
    }
    already_imported.insert(abs_path_str);

    let content = std::fs::read_to_string(&found_path).map_err(|e| {
        WaveError::new(
            WaveErrorKind::SyntaxError("Read error".to_string()),
            format!("Failed to read '{}': {}", target_file_name, e),
            target_file_name.clone(),
            0,
            0,
        )
    })?;

    let mut lexer = Lexer::new(&content);
    let tokens = lexer.tokenize();

    let ast = parse(&tokens).ok_or_else(|| {
        WaveError::new(
            WaveErrorKind::SyntaxError("Parse failed".to_string()),
            format!("Failed to parse '{}'", target_file_name),
            target_file_name.clone(),
            1,
            1,
        ).with_source(content.lines().nth(0).unwrap_or("").to_string())
            .with_label("here".to_string())
    })?;

    Ok(ast)
}

fn find_wave_file_recursive(dir: &Path, target_file_name: &str) -> Option<PathBuf> {
    for entry in std::fs::read_dir(dir).ok()? {
        let entry = entry.ok()?;
        let path = entry.path();

        if path.is_dir() {
            if let Some(found) = find_wave_file_recursive(&path, target_file_name) {
                return Some(found);
            }
        } else if path.is_file() {
            if path.file_name()?.to_str()? == target_file_name {
                return Some(path);
            }
        }
    }
    None
}