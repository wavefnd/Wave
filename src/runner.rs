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

use crate::{DebugFlags, DepFlags, LinkFlags, LlvmFlags};
use ::error::*;
use ::parser::ast::*;
use ::parser::generics::monomorphize_generics;
use ::parser::import::*;
use ::parser::verification::validate_program;
use ::parser::*;
use lexer::Lexer;
use llvm::backend::*;
use llvm::codegen::*;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::sync::{Arc, Mutex};
use std::{fs, process, process::Command};

enum TargetAttr<'a> {
    Supported(&'a str),
    Unsupported,
}

fn parse_target_os_attr(line: &str) -> Option<TargetAttr<'_>> {
    let trimmed = line.trim();
    if !trimmed.starts_with("#[target(os=\"") || !trimmed.ends_with("\")]") {
        return None;
    }

    let start = "#[target(os=\"".len();
    let end = trimmed.len() - 3; // ")]"
    let os = &trimmed[start..end];
    if os == "linux" || os == "macos" || os == "windows" {
        Some(TargetAttr::Supported(os))
    } else {
        Some(TargetAttr::Unsupported)
    }
}

fn is_supported_target_item_start(line: &str) -> bool {
    fn has_ident_boundary(rest: &str) -> bool {
        match rest.chars().next() {
            None => true,
            Some(c) => !(c.is_ascii_alphanumeric() || c == '_'),
        }
    }

    let trimmed = line.trim_start();
    for kw in ["fun", "struct", "enum", "const", "static", "type", "proto"] {
        if let Some(rest) = trimmed.strip_prefix(kw) {
            if has_ident_boundary(rest) {
                return true;
            }
        }
    }

    false
}

fn scan_target_item_line(
    line: &str,
    in_block_comment: &mut bool,
    depth: &mut i32,
    seen_open: &mut bool,
    saw_semicolon: &mut bool,
) {
    let mut chars = line.chars().peekable();
    let mut in_string = false;
    let mut in_char = false;
    let mut escape = false;

    while let Some(ch) = chars.next() {
        if *in_block_comment {
            if ch == '*' && chars.peek() == Some(&'/') {
                chars.next();
                *in_block_comment = false;
            }
            continue;
        }

        if in_string {
            if escape {
                escape = false;
                continue;
            }
            if ch == '\\' {
                escape = true;
                continue;
            }
            if ch == '"' {
                in_string = false;
            }
            continue;
        }

        if in_char {
            if escape {
                escape = false;
                continue;
            }
            if ch == '\\' {
                escape = true;
                continue;
            }
            if ch == '\'' {
                in_char = false;
            }
            continue;
        }

        if ch == '/' {
            if chars.peek() == Some(&'/') {
                break;
            }
            if chars.peek() == Some(&'*') {
                chars.next();
                *in_block_comment = true;
                continue;
            }
        }

        if ch == '"' {
            in_string = true;
            continue;
        }
        if ch == '\'' {
            in_char = true;
            continue;
        }

        if ch == '{' {
            *depth += 1;
            *seen_open = true;
            continue;
        }
        if ch == '}' {
            if *depth > 0 {
                *depth -= 1;
            }
            continue;
        }
        if ch == ';' {
            *saw_semicolon = true;
        }
    }
}

fn consume_target_item(lines: &[&str], mut idx: usize, keep: bool, out: &mut Vec<String>) -> usize {
    let mut depth: i32 = 0;
    let mut seen_open = false;
    let mut in_block_comment = false;

    while idx < lines.len() {
        let line = lines[idx];
        if keep {
            out.push(line.to_string());
        } else {
            out.push(String::new());
        }

        let mut saw_semicolon = false;
        scan_target_item_line(
            line,
            &mut in_block_comment,
            &mut depth,
            &mut seen_open,
            &mut saw_semicolon,
        );

        idx += 1;

        if seen_open {
            if depth == 0 {
                break;
            }
        } else if saw_semicolon {
            break;
        }
    }

    idx
}

fn target_os_from_triple(triple: &str) -> Option<&'static str> {
    let lower = triple.to_ascii_lowercase();
    if lower.contains("windows") {
        Some("windows")
    } else if lower.contains("darwin") || lower.contains("apple") {
        Some("macos")
    } else if lower.contains("linux") {
        Some("linux")
    } else {
        None
    }
}

fn target_os_for_llvm(llvm: Option<&LlvmFlags>) -> Option<String> {
    llvm.and_then(|opts| opts.target.as_deref())
        .and_then(target_os_from_triple)
        .map(str::to_string)
}

fn preprocess_target_attrs(source: &str, target_os: Option<&str>) -> String {
    let host = target_os.unwrap_or(std::env::consts::OS);
    let lines: Vec<&str> = source.lines().collect();
    let mut out: Vec<String> = Vec::with_capacity(lines.len());
    let mut idx: usize = 0;

    while idx < lines.len() {
        let line = lines[idx];
        if let Some(target_attr) = parse_target_os_attr(line) {
            // Attribute line is removed for parser compatibility,
            // but we keep its line slot to preserve diagnostics.
            out.push(String::new());
            idx += 1;

            let keep_item = match target_attr {
                TargetAttr::Supported(target_os) => target_os == host,
                // Ignore unsupported target values.
                TargetAttr::Unsupported => true,
            };

            // Attribute applies to the next top-level item.
            // Preserve line count for any leading blanks/comments.
            while idx < lines.len() {
                let item_line = lines[idx];
                let trimmed = item_line.trim_start();

                let is_leading_comment = trimmed.starts_with("//")
                    || trimmed.starts_with("/*")
                    || trimmed.starts_with('*')
                    || trimmed.starts_with("*/");

                if trimmed.is_empty() || is_leading_comment {
                    if keep_item {
                        out.push(item_line.to_string());
                    } else {
                        out.push(String::new());
                    }
                    idx += 1;
                    continue;
                }

                if is_supported_target_item_start(trimmed) {
                    idx = consume_target_item(&lines, idx, keep_item, &mut out);
                } else if keep_item {
                    out.push(item_line.to_string());
                    idx += 1;
                } else {
                    out.push(String::new());
                    idx += 1;
                }
                break;
            }
            continue;
        }

        out.push(line.to_string());
        idx += 1;
    }

    let mut processed = out.join("\n");
    if source.ends_with('\n') {
        processed.push('\n');
    }
    processed
}

fn parse_wave_tokens_or_exit(
    file_path: &Path,
    source: &str,
    tokens: &[lexer::Token],
) -> Vec<ASTNode> {
    parse_syntax_only(tokens).unwrap_or_else(|err| {
        let (kind, title, code) = match &err {
            ParseError::Syntax(_) => (
                WaveErrorKind::SyntaxError(err.message().to_string()),
                "failed to parse Wave source",
                "E2001",
            ),
            ParseError::Semantic(_) => (
                WaveErrorKind::InvalidStatement(err.message().to_string()),
                "semantic validation failed",
                "E3001",
            ),
        };

        let mut wave_err = WaveError::new(
            kind,
            format!("{}: {}", title, err.message()),
            file_path.display().to_string(),
            err.line().max(1),
            err.column().max(1),
        )
        .with_code(code)
        .with_source_code(source.to_string());

        if let Some(ctx) = err.context() {
            wave_err = wave_err.with_context(ctx.to_string());
        }
        if !err.expected().is_empty() {
            wave_err = wave_err.with_expected_many(err.expected().iter().cloned());
        }
        if let Some(found) = err.found() {
            wave_err = wave_err.with_found(found.to_string());
        }
        if let Some(note) = err.note() {
            wave_err = wave_err.with_note(note.to_string());
        }
        if let Some(help) = err.help() {
            wave_err = wave_err.with_help(help.to_string());
        } else {
            wave_err = wave_err.with_help("fix the diagnostic details above and try again");
        }

        wave_err.display();

        process::exit(1);
    })
}

fn validate_wave_ast_or_exit(file_path: &Path, source: &str, ast: &Vec<ASTNode>) {
    if let Err(msg) = validate_program(ast) {
        WaveError::new(
            WaveErrorKind::InvalidStatement(msg.clone()),
            format!("semantic validation failed: {}", msg),
            file_path.display().to_string(),
            1,
            1,
        )
        .with_code("E3001")
        .with_source_code(source.to_string())
        .with_context("semantic validation")
        .with_help("fix mutability, scope, and expression validity issues")
        .display();

        process::exit(1);
    }
}

fn panic_payload_to_string(payload: &(dyn std::any::Any + Send)) -> String {
    if let Some(s) = payload.downcast_ref::<String>() {
        return s.clone();
    }
    if let Some(s) = payload.downcast_ref::<&str>() {
        return (*s).to_string();
    }
    "non-string panic payload".to_string()
}

fn run_panic_guarded<T, F>(f: F) -> Result<T, (String, Option<String>)>
where
    F: FnOnce() -> T,
{
    let captured: Arc<Mutex<Option<(String, Option<String>)>>> = Arc::new(Mutex::new(None));
    let hook_state = Arc::clone(&captured);

    let old_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let payload = if let Some(s) = info.payload().downcast_ref::<String>() {
            s.clone()
        } else if let Some(s) = info.payload().downcast_ref::<&str>() {
            (*s).to_string()
        } else {
            "non-string panic payload".to_string()
        };

        let loc = info
            .location()
            .map(|l| format!("{}:{}:{}", l.file(), l.line(), l.column()));

        if let Ok(mut guard) = hook_state.lock() {
            *guard = Some((payload, loc));
        }
    }));

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(f));
    std::panic::set_hook(old_hook);

    match result {
        Ok(v) => Ok(v),
        Err(payload) => {
            let fallback = panic_payload_to_string(&*payload);
            let captured = captured
                .lock()
                .ok()
                .and_then(|g| g.clone())
                .unwrap_or((fallback.clone(), None));

            let msg = if captured.0.trim().is_empty() {
                fallback
            } else {
                captured.0
            };

            Err((msg, captured.1))
        }
    }
}

fn classify_codegen_panic(panic_message: &str) -> (&'static str, &'static str, &'static str) {
    if panic_message.contains("null literal can only be assigned to ptr<T>") {
        return (
            "E3102",
            "invalid null assignment",
            "use `null` only where the target type is `ptr<T>`",
        );
    }

    if panic_message.contains("integer literals cannot initialize pointers") {
        return (
            "E3103",
            "invalid pointer literal",
            "use `null` or an explicit cast when initializing a pointer",
        );
    }

    if panic_message.contains("implicit integer narrowing is forbidden") {
        return (
            "E3201",
            "implicit integer narrowing is forbidden",
            "insert an explicit cast or widen the destination type",
        );
    }

    if panic_message.contains("missing a return statement") {
        return (
            "E3004",
            "non-void function is missing return",
            "ensure every control-flow path returns a value",
        );
    }

    if panic_message.contains("unsupported extern ABI") {
        return (
            "E3006",
            "unsupported extern ABI",
            "Wave currently supports `extern(c)` only",
        );
    }

    if panic_message.contains("match value must be integer/enum type")
        || panic_message.contains("match case identifier")
    {
        return (
            "E3010",
            "invalid match operand",
            "use `match` only with integer/enum values and integer/enum case labels",
        );
    }

    if panic_message.contains("duplicate match case value")
        || panic_message.contains("duplicate wildcard match arm")
    {
        return (
            "E3011",
            "duplicate match case",
            "remove duplicate case labels so every match case value is unique",
        );
    }

    (
        "E9001",
        "compiler internal error during code generation",
        "this looks like a compiler bug; please report the panic details below",
    )
}

#[derive(Debug, Clone)]
struct InferredSourceLoc {
    line: usize,
    column: usize,
    span_len: usize,
    label: String,
    note: String,
}

fn extract_between(s: &str, start: &str, end: &str) -> Option<String> {
    let st = s.find(start)? + start.len();
    let rest = &s[st..];
    let en = rest.find(end)?;
    Some(rest[..en].to_string())
}

fn byte_index_to_line_col(source: &str, byte_index: usize) -> (usize, usize) {
    let idx = byte_index.min(source.len());
    let mut line = 1usize;
    let mut line_start = 0usize;

    for (i, ch) in source.char_indices() {
        if i >= idx {
            break;
        }
        if ch == '\n' {
            line += 1;
            line_start = i + 1;
        }
    }

    let col = source[line_start..idx].chars().count() + 1;
    (line, col)
}

fn find_function_call_site(source: &str, fn_name: &str) -> Option<usize> {
    let needle = format!("{}(", fn_name);
    let mut pos = 0usize;

    while pos < source.len() {
        let rel = source[pos..].find(&needle)?;
        let idx = pos + rel;

        let prefix_start = idx.saturating_sub(6);
        let prefix = &source[prefix_start..idx];
        if !prefix.ends_with("fun ") {
            return Some(idx);
        }

        pos = idx + needle.len();
    }

    None
}

fn find_function_decl(source: &str, fn_name: &str) -> Option<usize> {
    let pattern = format!("fun {}", fn_name);
    let idx = source.find(&pattern)?;
    Some(idx + "fun ".len())
}

fn find_variable_decl(source: &str, var_name: &str) -> Option<usize> {
    let patterns = [
        format!("let mut {}", var_name),
        format!("let {}", var_name),
        format!("var {}", var_name),
        format!("const {}", var_name),
    ];

    for p in patterns {
        if let Some(idx) = source.find(&p) {
            if let Some(off) = p.find(var_name) {
                return Some(idx + off);
            }
        }
    }

    None
}

fn is_ident_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

fn find_identifier_occurrence(source: &str, ident: &str) -> Option<usize> {
    if ident.is_empty() {
        return None;
    }

    let mut pos = 0usize;
    while pos < source.len() {
        let rel = source[pos..].find(ident)?;
        let idx = pos + rel;
        let end = idx + ident.len();

        let before_ok = source[..idx]
            .chars()
            .next_back()
            .map(|ch| !is_ident_char(ch))
            .unwrap_or(true);

        let after_ok = source[end..]
            .chars()
            .next()
            .map(|ch| !is_ident_char(ch))
            .unwrap_or(true);

        if before_ok && after_ok {
            return Some(idx);
        }

        pos = end;
    }

    None
}

fn extract_single_quoted_identifiers(message: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut seen = HashSet::new();
    let mut rest = message;

    while let Some(start) = rest.find('\'') {
        let tail = &rest[start + 1..];
        let Some(end) = tail.find('\'') else {
            break;
        };

        let candidate = &tail[..end];
        let is_symbol = !candidate.is_empty()
            && candidate.chars().any(is_ident_char)
            && candidate.chars().all(|ch| is_ident_char(ch) || ch == '.');

        if is_symbol && seen.insert(candidate.to_string()) {
            out.push(candidate.to_string());
        }

        rest = &tail[end + 1..];
    }

    out
}

fn find_best_symbol_site(source: &str, symbol: &str) -> Option<usize> {
    if symbol.is_empty() {
        return None;
    }

    if let Some(idx) = find_function_call_site(source, symbol) {
        return Some(idx);
    }
    if let Some(idx) = find_variable_decl(source, symbol) {
        return Some(idx);
    }
    if let Some(idx) = find_function_decl(source, symbol) {
        return Some(idx);
    }
    if let Some(idx) = find_identifier_occurrence(source, symbol) {
        return Some(idx);
    }

    if let Some(base) = symbol.split('.').next() {
        if base != symbol {
            return find_best_symbol_site(source, base);
        }
    }

    None
}

fn infer_codegen_source_location(source: &str, panic_message: &str) -> Option<InferredSourceLoc> {
    if let Some(fn_name) = extract_between(panic_message, "Function '", "' not found") {
        if let Some(idx) = find_function_call_site(source, &fn_name) {
            let (line, column) = byte_index_to_line_col(source, idx);
            return Some(InferredSourceLoc {
                line,
                column,
                span_len: fn_name.chars().count().max(1),
                label: format!("unresolved function `{}` is called here", fn_name),
                note: "source position inferred from unresolved function name in backend panic"
                    .to_string(),
            });
        }
    }

    if let Some(fn_name) = extract_between(
        panic_message,
        "Non-void function '",
        "' is missing a return statement",
    ) {
        if let Some(idx) = find_function_decl(source, &fn_name) {
            let (line, column) = byte_index_to_line_col(source, idx);
            return Some(InferredSourceLoc {
                line,
                column,
                span_len: fn_name.chars().count().max(1),
                label: format!("function `{}` declaration", fn_name),
                note: "source position inferred from function name in backend panic".to_string(),
            });
        }
    }

    if let Some(var_name) = extract_between(panic_message, "(variable '", "':") {
        if let Some(idx) = find_variable_decl(source, &var_name) {
            let (line, column) = byte_index_to_line_col(source, idx);
            return Some(InferredSourceLoc {
                line,
                column,
                span_len: var_name.chars().count().max(1),
                label: format!("variable `{}` declaration", var_name),
                note: "source position inferred from variable name in backend panic".to_string(),
            });
        }
    }

    for symbol in extract_single_quoted_identifiers(panic_message) {
        if let Some(idx) = find_best_symbol_site(source, &symbol) {
            let (line, column) = byte_index_to_line_col(source, idx);
            return Some(InferredSourceLoc {
                line,
                column,
                span_len: symbol
                    .split('.')
                    .next_back()
                    .unwrap_or(&symbol)
                    .chars()
                    .count()
                    .max(1),
                label: format!("related symbol `{}` appears here", symbol),
                note: "source position inferred from backend panic symbol".to_string(),
            });
        }
    }

    None
}

fn emit_codegen_panic_and_exit(
    file_path: &Path,
    source: &str,
    phase: &str,
    panic_message: String,
    panic_location: Option<String>,
) -> ! {
    let (code, headline, help) = classify_codegen_panic(&panic_message);
    let inferred = infer_codegen_source_location(source, &panic_message);

    let (line, column) = inferred
        .as_ref()
        .map(|x| (x.line, x.column))
        .unwrap_or((1, 1));

    let mut err = WaveError::new(
        WaveErrorKind::CompilationFailed(panic_message.clone()),
        format!("{} ({})", headline, phase),
        file_path.display().to_string(),
        line,
        column,
    )
    .with_code(code)
    .with_source_code(source.to_string())
    .with_context(format!("compiler phase: {}", phase))
    .with_found(panic_message)
    .with_help(help);

    if let Some(loc) = inferred {
        err = err
            .with_span_len(loc.span_len)
            .with_label(loc.label)
            .with_note(loc.note);
    } else {
        err = err.with_note("no precise source span was available for this backend diagnostic");
    }

    if let Some(loc) = panic_location {
        err = err.with_suggestion(format!("compiler panic location: {}", loc));
    }

    err.display();
    process::exit(1);
}

fn build_import_config(dep: &DepFlags, target_os: Option<String>) -> ImportConfig {
    let mut config = ImportConfig {
        target_os,
        ..ImportConfig::default()
    };

    for root in &dep.roots {
        config.dep_roots.push(PathBuf::from(root));
    }

    for package in &dep.packages {
        config
            .dep_packages
            .insert(package.name.clone(), PathBuf::from(&package.path));
    }

    config
}

fn expand_imports_for_codegen(
    entry_path: &Path,
    ast: Vec<ASTNode>,
    import_config: &ImportConfig,
) -> Result<Vec<ASTNode>, WaveError> {
    fn expand_from_dir(
        base_dir: &Path,
        ast: Vec<ASTNode>,
        already: &mut HashSet<String>,
        import_config: &ImportConfig,
    ) -> Result<Vec<ASTNode>, WaveError> {
        let mut out = Vec::new();

        for node in ast {
            match node {
                ASTNode::Statement(StatementNode::Import(module)) => {
                    let unit =
                        local_import_unit_with_config(&module, already, base_dir, import_config)?;

                    if unit.ast.is_empty() {
                        continue;
                    }

                    let next_dir = unit.abs_path.parent().unwrap_or(base_dir);

                    let expanded = expand_from_dir(next_dir, unit.ast, already, import_config)?;
                    out.extend(expanded);
                }

                other => out.push(other),
            }
        }

        Ok(out)
    }

    let mut already = HashSet::new();

    if let Ok(abs) = entry_path.canonicalize() {
        if let Some(s) = abs.to_str() {
            already.insert(s.to_string());
        }
    }

    let base_dir = entry_path.parent().unwrap_or(Path::new("."));
    expand_from_dir(base_dir, ast, &mut already, import_config)
}

#[allow(dead_code)]
fn resolve_output_target(
    default_output: &str,
    output: Option<&Path>,
    file_path: &Path,
    source: &str,
    stage: &str,
) -> String {
    let Some(output) = output else {
        return default_output.to_string();
    };

    if output.as_os_str().is_empty() {
        WaveError::new(
            WaveErrorKind::FileWriteError(file_path.display().to_string()),
            "output path must not be empty",
            file_path.display().to_string(),
            0,
            0,
        )
        .with_code("E1005")
        .with_source_code(source.to_string())
        .with_context(stage)
        .with_help("pass a valid path to -o <file>")
        .display();
        process::exit(1);
    }

    if let Some(parent) = output.parent() {
        if !parent.as_os_str().is_empty() && !parent.exists() {
            if let Err(err) = fs::create_dir_all(parent) {
                WaveError::new(
                    WaveErrorKind::FileWriteError(output.display().to_string()),
                    format!(
                        "failed to create output directory `{}`: {}",
                        parent.display(),
                        err
                    ),
                    file_path.display().to_string(),
                    0,
                    0,
                )
                .with_code("E1005")
                .with_source_code(source.to_string())
                .with_context(stage)
                .with_help("check path permissions for the output directory")
                .display();
                process::exit(1);
            }
        }
    }

    output.display().to_string()
}

fn build_backend_options(llvm: &LlvmFlags) -> BackendOptions {
    BackendOptions {
        target: llvm.target.clone(),
        cpu: llvm.cpu.clone(),
        features: llvm.features.clone(),
        abi: llvm.abi.clone(),
        code_model: llvm.code_model.clone(),
        relocation_model: llvm.relocation_model.clone(),
        sysroot: llvm.sysroot.clone(),
        linker: llvm.linker.clone(),
        link_args: llvm.link_args.clone(),
        no_default_libs: llvm.no_default_libs,
        freestanding: llvm.freestanding,
    }
}

fn frontend_prepare_wave_ast(
    file_path: &Path,
    debug: &DebugFlags,
    dep: &DepFlags,
    llvm: Option<&LlvmFlags>,
) -> (String, Vec<ASTNode>) {
    let raw_code = match fs::read_to_string(file_path) {
        Ok(c) => c,
        Err(_) => {
            WaveError::new(
                WaveErrorKind::FileReadError(file_path.display().to_string()),
                format!("failed to read file `{}`", file_path.display()),
                file_path.display().to_string(),
                0,
                0,
            )
            .with_help("check if the file exists and you have permission to read it")
            .display();
            process::exit(1);
        }
    };
    let target_os = target_os_for_llvm(llvm);
    let code = preprocess_target_attrs(&raw_code, target_os.as_deref());

    let mut lexer = Lexer::new_with_file(&code, file_path.display().to_string());
    let tokens = lexer.tokenize().unwrap_or_else(|e| {
        e.display();
        process::exit(1);
    });

    let parsed_ast = parse_wave_tokens_or_exit(file_path, &code, &tokens);

    if debug.tokens {
        println!("\n===== Tokens =====");
        for token in &tokens {
            println!("{:?}", token);
        }
    }

    if debug.ast {
        println!("\n===== AST =====\n{:#?}", parsed_ast);
    }

    let import_config = build_import_config(dep, target_os);
    let ast = match expand_imports_for_codegen(file_path, parsed_ast, &import_config) {
        Ok(a) => a,
        Err(e) => {
            e.display();
            process::exit(1);
        }
    };
    let ast = match monomorphize_generics(ast) {
        Ok(a) => a,
        Err(msg) => {
            WaveError::new(
                WaveErrorKind::InvalidStatement(msg.clone()),
                format!("generic monomorphization failed: {}", msg),
                file_path.display().to_string(),
                1,
                1,
            )
            .with_code("E3001")
            .with_source_code(code.to_string())
            .with_context("generic instantiation")
            .with_help(
                "check generic type arguments, generic function calls, and generic struct usages",
            )
            .display();
            process::exit(1);
        }
    };

    validate_wave_ast_or_exit(file_path, &code, &ast);
    (code, ast)
}

pub(crate) unsafe fn check_wave_file(
    file_path: &Path,
    debug: &DebugFlags,
    dep: &DepFlags,
    llvm: &LlvmFlags,
) {
    let _ = frontend_prepare_wave_ast(file_path, debug, dep, Some(llvm));
}

pub(crate) unsafe fn emit_wave_ast_text(
    file_path: &Path,
    debug: &DebugFlags,
    dep: &DepFlags,
    llvm: &LlvmFlags,
) -> String {
    let (_, ast) = frontend_prepare_wave_ast(file_path, debug, dep, Some(llvm));
    format!("{:#?}\n", ast)
}

pub(crate) unsafe fn emit_wave_ir_text(
    file_path: &Path,
    opt_flag: &str,
    debug: &DebugFlags,
    dep: &DepFlags,
    llvm: &LlvmFlags,
) -> String {
    let (code, ast) = frontend_prepare_wave_ast(file_path, debug, dep, Some(llvm));
    let backend_opts = build_backend_options(llvm);

    let ir = match run_panic_guarded(|| unsafe { generate_ir(&ast, opt_flag, &backend_opts) }) {
        Ok(ir) => ir,
        Err((msg, loc)) => {
            emit_codegen_panic_and_exit(file_path, &code, "llvm-ir-generation", msg, loc)
        }
    };

    if debug.ir {
        println!("\n===== LLVM IR =====\n{}", ir);
    }

    ir
}

fn codegen_file_phase(kind: CodegenFileKind) -> &'static str {
    match kind {
        CodegenFileKind::Bitcode => "bitcode-emission",
        CodegenFileKind::Assembly => "assembly-emission",
        CodegenFileKind::Object => "object-emission",
    }
}

unsafe fn emit_wave_codegen_file(
    file_path: &Path,
    opt_flag: &str,
    debug: &DebugFlags,
    dep: &DepFlags,
    llvm: &LlvmFlags,
    output: &Path,
    kind: CodegenFileKind,
) {
    let (code, ast) = frontend_prepare_wave_ast(file_path, debug, dep, Some(llvm));
    emit_wave_codegen_file_from_ast(file_path, &code, &ast, opt_flag, debug, llvm, output, kind);
}

unsafe fn emit_wave_codegen_file_from_ast(
    file_path: &Path,
    code: &str,
    ast: &[ASTNode],
    opt_flag: &str,
    debug: &DebugFlags,
    llvm: &LlvmFlags,
    output: &Path,
    kind: CodegenFileKind,
) {
    let backend_opts = build_backend_options(llvm);

    if debug.ir {
        let ir = match run_panic_guarded(|| unsafe { generate_ir(ast, opt_flag, &backend_opts) }) {
            Ok(ir) => ir,
            Err((msg, loc)) => {
                emit_codegen_panic_and_exit(file_path, code, "llvm-ir-generation", msg, loc)
            }
        };
        println!("\n===== LLVM IR =====\n{}", ir);
    }

    if let Some(parent) = output.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent).unwrap_or_else(|e| {
                emit_codegen_panic_and_exit(
                    file_path,
                    code,
                    codegen_file_phase(kind),
                    format!(
                        "failed to create output directory '{}': {}",
                        parent.display(),
                        e
                    ),
                    None,
                )
            });
        }
    }

    if let Err((msg, loc)) = run_panic_guarded(|| unsafe {
        emit_codegen_file(ast, opt_flag, &backend_opts, output, kind);
    }) {
        emit_codegen_panic_and_exit(file_path, code, codegen_file_phase(kind), msg, loc);
    }
}

pub(crate) unsafe fn emit_wave_bitcode_file(
    file_path: &Path,
    opt_flag: &str,
    debug: &DebugFlags,
    dep: &DepFlags,
    llvm: &LlvmFlags,
    output: &Path,
) {
    emit_wave_codegen_file(
        file_path,
        opt_flag,
        debug,
        dep,
        llvm,
        output,
        CodegenFileKind::Bitcode,
    );
}

pub(crate) unsafe fn emit_wave_assembly_file(
    file_path: &Path,
    opt_flag: &str,
    debug: &DebugFlags,
    dep: &DepFlags,
    llvm: &LlvmFlags,
    output: &Path,
) {
    emit_wave_codegen_file(
        file_path,
        opt_flag,
        debug,
        dep,
        llvm,
        output,
        CodegenFileKind::Assembly,
    );
}

#[allow(dead_code)]
pub(crate) unsafe fn run_wave_file(
    file_path: &Path,
    opt_flag: &str,
    debug: &DebugFlags,
    link: &LinkFlags,
    dep: &DepFlags,
    llvm: &LlvmFlags,
) {
    let raw_code = match fs::read_to_string(file_path) {
        Ok(c) => c,
        Err(_) => {
            WaveError::new(
                WaveErrorKind::FileReadError(file_path.display().to_string()),
                format!("failed to read file `{}`", file_path.display()),
                file_path.display().to_string(),
                0,
                0,
            )
            .with_help("check if the file exists and you have permission to read it")
            .display();
            process::exit(1);
        }
    };
    let target_os = target_os_for_llvm(Some(llvm));
    let code = preprocess_target_attrs(&raw_code, target_os.as_deref());

    let mut lexer = Lexer::new_with_file(&code, file_path.display().to_string());
    let tokens = lexer.tokenize().unwrap_or_else(|e| {
        e.display();
        process::exit(1);
    });

    let ast = parse_wave_tokens_or_exit(file_path, &code, &tokens);

    if debug.tokens {
        println!("\n===== Tokens =====");
        for token in &tokens {
            println!("{:?}", token);
        }
    }

    if debug.ast {
        println!("\n===== AST =====\n{:#?}", ast);
    }

    let import_config = build_import_config(dep, target_os);

    let ast = match expand_imports_for_codegen(file_path, ast, &import_config) {
        Ok(a) => a,
        Err(e) => {
            e.display();
            process::exit(1);
        }
    };
    let ast = match monomorphize_generics(ast) {
        Ok(a) => a,
        Err(msg) => {
            WaveError::new(
                WaveErrorKind::InvalidStatement(msg.clone()),
                format!("generic monomorphization failed: {}", msg),
                file_path.display().to_string(),
                1,
                1,
            )
            .with_code("E3001")
            .with_source_code(code.to_string())
            .with_context("generic instantiation")
            .with_help(
                "check generic type arguments, generic function calls, and generic struct usages",
            )
            .display();
            process::exit(1);
        }
    };

    validate_wave_ast_or_exit(file_path, &code, &ast);

    let file_stem = file_path.file_stem().unwrap().to_str().unwrap();
    let object_patch = format!("{}.o", file_stem);
    emit_wave_codegen_file_from_ast(
        file_path,
        &code,
        &ast,
        opt_flag,
        debug,
        llvm,
        Path::new(&object_patch),
        CodegenFileKind::Object,
    );

    if debug.mc {
        println!("\n===== MACHINE CODE PATH =====");
        println!("{}", object_patch);
    }

    if debug.hex {
        println!("\n===== HEX DUMP =====");
        let data = fs::read(&object_patch).unwrap();
        for (i, b) in data.iter().enumerate() {
            if i % 16 == 0 {
                print!("\n{:04x}: ", i);
            }
            print!("{:02x} ", b);
        }
        println!();
    }

    let exe_patch = format!("target/{}", file_stem);
    let backend_opts = build_backend_options(llvm);

    if let Err((msg, loc)) = run_panic_guarded(|| {
        link_objects(
            std::slice::from_ref(&object_patch),
            &exe_patch,
            &link.libs,
            &link.paths,
            &backend_opts,
        );
    }) {
        emit_codegen_panic_and_exit(file_path, &code, "native-link", msg, loc);
    }

    let status = Command::new(&exe_patch)
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .unwrap_or_else(|e| {
            eprintln!("Failed to run `{}`: {}", exe_patch, e);
            process::exit(1);
        });

    if !status.success() {
        process::exit(status.code().unwrap_or(1));
    }
}

pub(crate) unsafe fn object_build_wave_file(
    file_path: &Path,
    opt_flag: &str,
    debug: &DebugFlags,
    dep: &DepFlags,
    llvm: &LlvmFlags,
    output: Option<&Path>,
) -> String {
    let raw_code = fs::read_to_string(file_path).unwrap_or_else(|_| {
        WaveError::new(
            WaveErrorKind::FileReadError(file_path.display().to_string()),
            format!("failed to read file `{}`", file_path.display()),
            file_path.display().to_string(),
            0,
            0,
        )
        .display();
        process::exit(1);
    });
    let target_os = target_os_for_llvm(Some(llvm));
    let code = preprocess_target_attrs(&raw_code, target_os.as_deref());

    let mut lexer = Lexer::new_with_file(&code, file_path.display().to_string());
    let tokens = lexer.tokenize().unwrap_or_else(|e| {
        e.display();
        process::exit(1);
    });

    let ast = parse_wave_tokens_or_exit(file_path, &code, &tokens);

    if debug.tokens {
        println!("\n===== Tokens =====");
        for token in &tokens {
            println!("{:?}", token);
        }
    }

    if debug.ast {
        println!("\n===== AST =====\n{:#?}", ast);
    }

    let import_config = build_import_config(dep, target_os);

    let ast = expand_imports_for_codegen(file_path, ast, &import_config).unwrap_or_else(|e| {
        e.display();
        process::exit(1);
    });
    let ast = monomorphize_generics(ast).unwrap_or_else(|msg| {
        WaveError::new(
            WaveErrorKind::InvalidStatement(msg.clone()),
            format!("generic monomorphization failed: {}", msg),
            file_path.display().to_string(),
            1,
            1,
        )
        .with_code("E3001")
        .with_source_code(code.to_string())
        .with_context("generic instantiation")
        .with_help(
            "check generic type arguments, generic function calls, and generic struct usages",
        )
        .display();
        process::exit(1);
    });

    validate_wave_ast_or_exit(file_path, &code, &ast);

    let file_stem = file_path.file_stem().unwrap().to_str().unwrap();
    let default_object_path = PathBuf::from(format!("{}.o", file_stem));
    let output_path = output.unwrap_or(default_object_path.as_path());
    emit_wave_codegen_file_from_ast(
        file_path,
        &code,
        &ast,
        opt_flag,
        debug,
        llvm,
        output_path,
        CodegenFileKind::Object,
    );
    let object_path = output_path.to_string_lossy().to_string();

    if debug.mc {
        println!("\n===== MACHINE CODE PATH =====");
        println!("{}", object_path);
    }

    if debug.hex {
        println!("\n===== HEX DUMP =====");
        let data = fs::read(&object_path).unwrap();
        for (i, b) in data.iter().enumerate() {
            if i % 16 == 0 {
                print!("\n{:04x}: ", i);
            }
            print!("{:02x} ", b);
        }
        println!();
    }

    object_path
}

#[allow(dead_code)]
pub(crate) unsafe fn build_wave_file(
    file_path: &Path,
    opt_flag: &str,
    debug: &DebugFlags,
    link: &LinkFlags,
    dep: &DepFlags,
    llvm: &LlvmFlags,
    output: Option<&Path>,
) {
    let object_path = object_build_wave_file(file_path, opt_flag, debug, dep, llvm, None);

    let file_stem = file_path.file_stem().unwrap().to_str().unwrap();
    let default_exe_path = format!("target/{}", file_stem);
    let source = fs::read_to_string(file_path).unwrap_or_default();
    let exe_path =
        resolve_output_target(&default_exe_path, output, file_path, &source, "native-link");
    let backend_opts = build_backend_options(llvm);

    if let Err((msg, loc)) = run_panic_guarded(|| {
        link_objects(
            &[object_path],
            &exe_path,
            &link.libs,
            &link.paths,
            &backend_opts,
        );
    }) {
        emit_codegen_panic_and_exit(file_path, &source, "native-link", msg, loc);
    }

    if debug.mc {
        println!("\n===== OUTPUT BINARY =====");
        println!("{}", exe_path);
    }
}
