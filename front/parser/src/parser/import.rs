use std::collections::HashSet;
use std::path::{Path, PathBuf};
use error::error::{WaveError, WaveErrorKind};
use crate::ast::ASTNode;
use crate::parse;
use crate::parser::stdlib::StdlibManager;
use lexer::Lexer;

pub fn local_import(
    path: &str,
    already_imported: &mut HashSet<String>,
    base_dir: &Path,
    stdlib_manager: Option<&StdlibManager>,
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

    // Handle standard library imports (std::*)
    if path.starts_with("std::") {
        return handle_std_import(path, already_imported, stdlib_manager);
    }

    // Handle external library imports (contain :: but not std::)
    if path.contains("::") && !path.starts_with("std::") {
        return external_import(path, already_imported);
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

fn handle_std_import(
    path: &str,
    already_imported: &mut HashSet<String>,
    stdlib_manager: Option<&StdlibManager>,
) -> Result<Vec<ASTNode>, WaveError> {
    if already_imported.contains(path) {
        return Ok(vec![]);
    }
    already_imported.insert(path.to_string());

    // Extract the module name after "std::"
    let module_name = path.strip_prefix("std::").unwrap_or(path);
    
    // Check if stdlib manager is available (Vex integration)
    if let Some(_manager) = stdlib_manager {
        // TODO: Implement actual stdlib import validation with Vex communication
        // In Vex integration mode, only import is allowed and actual validation is handled by Vex
        // Wave compiler only recognizes std:: import syntax
        println!("Standard library import '{}' will be handled by Vex", path);
        Ok(vec![])
    } else {
        // Wave compiler used standalone - no standard library access
        Err(WaveError::stdlib_requires_vex(module_name, path, 0, 0))
    }
}

pub fn external_import(
    path: &str,
    already_imported: &mut HashSet<String>,
) -> Result<Vec<ASTNode>, WaveError> {
    if already_imported.contains(path) {
        return Ok(vec![]);
    }
    already_imported.insert(path.to_string());

    // For external libraries, we would typically:
    // 1. Check if the library exists in a known path
    // 2. Load the library's interface/header information
    // 3. Return the appropriate AST nodes
    
    // For now, return empty AST as external library handling 
    // would be implemented by the package manager (Vex)
    Ok(vec![])
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