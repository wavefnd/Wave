use crate::ast::ASTNode;
use crate::parse;
use error::error::{WaveError, WaveErrorKind};
use lexer::Lexer;
use std::collections::HashSet;
use std::path::{Path, PathBuf};

pub struct ImportedUnit {
    pub abs_path: PathBuf,
    pub ast: Vec<ASTNode>,
}

pub fn local_import_unit(
    path: &str,
    already_imported: &mut HashSet<String>,
    base_dir: &Path,
) -> Result<ImportedUnit, WaveError> {
    if path.trim().is_empty() {
        return Err(WaveError::new(
            WaveErrorKind::SyntaxError("Empty import path".to_string()),
            "import path cannot be empty",
            "<main>",
            0,
            0,
        ));
    }

    if path.starts_with("std::") {
        already_imported.insert(path.to_string());
        return Ok(ImportedUnit {
            abs_path: base_dir.to_path_buf(),
            ast: vec![],
        });
    }

    if path.contains("::") {
        return Err(WaveError::new(
            WaveErrorKind::SyntaxError("External import is not supported".to_string()),
            "External imports are not supported by the Wave compiler (standalone).",
            path,
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

    let abs_path_str = abs_path
        .to_str()
        .ok_or_else(|| {
            WaveError::new(
                WaveErrorKind::UnexpectedChar('?'),
                "Invalid path encoding",
                target_file_name.clone(),
                0,
                0,
            )
        })?
        .to_string();

    if already_imported.contains(&abs_path_str) {
        return Ok(ImportedUnit { abs_path, ast: vec![] });
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
        )
            .with_source(content.lines().next().unwrap_or("").to_string())
            .with_label("here".to_string())
    })?;

    Ok(ImportedUnit { abs_path, ast })
}

pub fn local_import(
    path: &str,
    already_imported: &mut HashSet<String>,
    base_dir: &Path,
) -> Result<Vec<ASTNode>, WaveError> {
    Ok(local_import_unit(path, already_imported, base_dir)?.ast)
}
