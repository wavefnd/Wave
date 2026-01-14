use crate::ast::{ASTNode, StatementNode};
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
        return std_import_unit(path, already_imported);
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

    parse_wave_file(&found_path, &target_file_name, already_imported)
}

pub fn local_import(
    path: &str,
    already_imported: &mut HashSet<String>,
    base_dir: &Path,
) -> Result<Vec<ASTNode>, WaveError> {
    Ok(local_import_unit(path, already_imported, base_dir)?.ast)
}

fn std_import_unit(path: &str, already_imported: &mut HashSet<String>) -> Result<ImportedUnit, WaveError> {
    let rel = path.strip_prefix("std::").unwrap();
    if rel.trim().is_empty() {
        return Err(WaveError::new(
            WaveErrorKind::SyntaxError("Empty std import".to_string()),
            "std import path cannot be empty (example: import(\"std::io::format\"))",
            path,
            0,
            0,
        ));
    }

    let std_root = std_root_dir(path)?;

    // std::io::format -> ~/.wave/lib/wave/std/io/format.wave
    let rel_path = rel.replace("::", "/");
    let found_path = std_root.join(format!("{}.wave", rel_path));

    if !found_path.exists() || !found_path.is_file() {
        return Err(WaveError::new(
            WaveErrorKind::SyntaxError("File not found".to_string()),
            format!("Could not find std import target '{}'", found_path.display()),
            path,
            0,
            0,
        ));
    }

    parse_wave_file(&found_path, path, already_imported)
}

fn std_root_dir(import_path: &str) -> Result<PathBuf, WaveError> {
    let home = std::env::var("HOME").map_err(|_| {
        WaveError::new(
            WaveErrorKind::SyntaxError("std not installed".to_string()),
            "HOME env not set; cannot locate std at ~/.wave/lib/wave/std",
            import_path,
            0,
            0,
        )
    })?;

    Ok(PathBuf::from(home).join(".wave/lib/wave/std"))
}

fn parse_wave_file(
    found_path: &Path,
    display_name: &str,
    already_imported: &mut HashSet<String>,
) -> Result<ImportedUnit, WaveError> {
    let abs_path = found_path.canonicalize().map_err(|e| {
        WaveError::new(
            WaveErrorKind::SyntaxError("Canonicalization failed".to_string()),
            format!("Failed to canonicalize path: {}", e),
            display_name,
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
                display_name,
                0,
                0,
            )
        })?
        .to_string();

    if already_imported.contains(&abs_path_str) {
        return Ok(ImportedUnit { abs_path, ast: vec![] });
    }
    already_imported.insert(abs_path_str);

    let content = std::fs::read_to_string(&abs_path).map_err(|e| {
        WaveError::new(
            WaveErrorKind::SyntaxError("Read error".to_string()),
            format!("Failed to read '{}': {}", abs_path.display(), e),
            display_name,
            0,
            0,
        )
    })?;

    let mut lexer = Lexer::new(&content);
    let tokens = lexer.tokenize();

    let ast = parse(&tokens).ok_or_else(|| {
        WaveError::new(
            WaveErrorKind::SyntaxError("Parse failed".to_string()),
            format!("Failed to parse '{}'", abs_path.display()),
            display_name,
            1,
            1,
        )
            .with_source(content.lines().next().unwrap_or("").to_string())
            .with_label("here".to_string())
    })?;

    Ok(ImportedUnit { abs_path, ast })
}
