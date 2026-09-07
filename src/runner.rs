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

//! Compiler-driver orchestration from source loading through native execution.
//!
//! This module owns user-facing phase boundaries and diagnostics. Frontend work
//! must finish in the order target preprocessing, parsing, import expansion,
//! template validation, monomorphization, and typed HIR construction. The
//! legacy LLVM path currently receives the HIR's syntax view. Backend panics
//! are caught here and translated into Wave diagnostics; lower layers should
//! not duplicate that presentation policy.

use crate::module_resolver::{demangle_module_names, resolve_import_graph};
use crate::{DebugFlags, DepFlags, LinkFlags, LlvmFlags};
use ::error::*;
use ::parser::ast::*;
use ::parser::generics::monomorphize_generics;
use ::parser::hir::TypedProgram;
use ::parser::import::*;
use ::parser::verification::validate_program_detailed;
use ::parser::*;
use lexer::Lexer;
use llvm::backend::*;
use llvm::codegen::target::target_spec_for_triple;
use llvm::codegen::*;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::sync::{Arc, Mutex};
use std::{fs, process, process::Command};

fn target_condition_context_for_llvm(llvm: Option<&LlvmFlags>) -> TargetConditionContext {
    let mut target = TargetConditionContext::default();

    if let Some(opts) = llvm {
        if let Some(triple) = opts.target.as_deref() {
            if let Some(spec) = target_spec_for_triple(triple) {
                target.arch = Some(spec.architecture.name().to_string());
                target.os = Some(spec.os.to_string());
                target.env = Some(spec.env.to_string());
            }
        }
        if opts.abi.is_some() {
            target.abi = opts.abi.clone();
        }
    }

    target
}
fn parse_wave_tokens_or_exit(
    file_path: &Path,
    source: &str,
    tokens: &[lexer::Token],
) -> Vec<ASTNode> {
    parse_syntax_with_spans(tokens).unwrap_or_else(|err| {
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

        wave_err = wave_err.with_span(err.span());
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

        wave_err.display_auto();

        process::exit(1);
    })
}

fn lower_wave_hir_or_exit(file_path: &Path, source: &str, ast: Vec<ASTNode>) -> TypedProgram {
    match TypedProgram::lower(ast) {
        Ok(program) => program,
        Err(error) => {
            let (_ast, diagnostic) = error.into_parts();
            let span = diagnostic.span.clone();
            let diagnostic_source = span
                .as_ref()
                .and_then(|s| fs::read_to_string(&s.file).ok())
                .unwrap_or_else(|| source.to_string());
            let mut error = WaveError::new(
                WaveErrorKind::InvalidStatement(diagnostic.message.clone()),
                format!("semantic validation failed: {}", diagnostic.message),
                file_path.display().to_string(),
                0,
                0,
            )
            .with_code(diagnostic.code)
            .with_source_code(diagnostic_source)
            .with_span(span.as_ref())
            .with_context("semantic validation")
            .with_label(diagnostic.label)
            .with_help(diagnostic.help);
            if let Some(note) = diagnostic.note {
                error = error.with_note(note);
            }
            error.display_auto();

            process::exit(1);
        }
    }
}

fn validate_expanded_ast_or_exit(expanded: &ExpandedWaveAst) {
    let Err(mut diagnostic) = validate_program_detailed(&expanded.ast) else {
        return;
    };
    diagnostic.message = demangle_module_names(&diagnostic.message);
    diagnostic.label = demangle_module_names(&diagnostic.label);
    diagnostic.help = demangle_module_names(&diagnostic.help);
    diagnostic.note = diagnostic.note.map(|note| demangle_module_names(&note));
    if let Some(primary) = &mut diagnostic.primary {
        primary.text = demangle_module_names(&primary.text);
    }
    let origin = expanded
        .origins
        .get(diagnostic.top_level_index)
        .copied()
        .unwrap_or(0);
    let source_unit = expanded.sources.get(origin).unwrap_or(&expanded.sources[0]);
    let span = diagnostic.span.clone();
    let diagnostic_source = span
        .as_ref()
        .and_then(|span| {
            expanded
                .sources
                .iter()
                .find(|unit| unit.path.to_string_lossy() == span.file)
        })
        .map(|unit| unit.source.clone())
        .unwrap_or_else(|| source_unit.source.clone());

    let mut error = WaveError::new(
        WaveErrorKind::InvalidStatement(diagnostic.message.clone()),
        format!("semantic validation failed: {}", diagnostic.message),
        source_unit.path.display().to_string(),
        0,
        0,
    )
    .with_code(diagnostic.code)
    .with_source_code(diagnostic_source)
    .with_span(span.as_ref())
    .with_context("semantic validation")
    .with_label(diagnostic.label)
    .with_help(diagnostic.help);
    if let Some(note) = diagnostic.note {
        error = error.with_note(note);
    }
    error.display_auto();

    process::exit(1);
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

    if panic_message.contains("asm input register/constraint")
        || panic_message.contains("asm output register/constraint")
        || panic_message.contains("Invalid clobber token")
        || panic_message.contains("asm touches the stack")
        || panic_message.contains("asm contains a non-returning branch")
        || panic_message.contains("asm stack delta is not balanced")
        || panic_message.contains("asm writes the stack pointer")
        || panic_message.contains("asm cannot declare both")
        || panic_message.contains("conflicts with an input/output operand register")
        || panic_message.contains("asm expression cannot declare")
    {
        return (
            "E3401",
            "invalid inline assembly contract",
            "use registers valid for the selected target and declare stack, clobber, and control-flow effects explicitly",
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
    let patterns = [format!("var {}", var_name), format!("const {}", var_name)];

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

    if code == "E9001" {
        if let Some(loc) = panic_location {
            err = err.with_suggestion(format!("compiler panic location: {}", loc));
        }
    }

    err.display_auto();
    process::exit(1);
}

fn build_import_config(dep: &DepFlags, target: TargetConditionContext) -> ImportConfig {
    let mut config = ImportConfig {
        target,
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

struct SemanticSourceUnit {
    path: PathBuf,
    source: String,
}

struct ExpandedWaveAst {
    ast: Vec<ASTNode>,
    origins: Vec<usize>,
    sources: Vec<SemanticSourceUnit>,
}

fn expand_imports_for_codegen(
    entry_path: &Path,
    entry_source: &str,
    ast: Vec<ASTNode>,
    import_config: &ImportConfig,
) -> Result<ExpandedWaveAst, WaveError> {
    let mut graph = resolve_import_graph(entry_path, entry_source, ast, import_config)?;
    let pointer_bits = if import_config.target.arch.as_deref() == Some("wasm32") {
        32
    } else {
        64
    };
    ::parser::hir::resolve_target_types(&mut graph.ast, pointer_bits).map_err(|message| {
        WaveError::new(
            WaveErrorKind::InvalidStatement(message.clone()),
            message,
            entry_path.display().to_string(),
            0,
            0,
        )
    })?;
    Ok(ExpandedWaveAst {
        ast: graph.ast,
        origins: graph.origins,
        sources: graph
            .sources
            .into_iter()
            .map(|source| SemanticSourceUnit {
                path: source.path,
                source: source.source,
            })
            .collect(),
    })
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
        .display_auto();
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
                .display_auto();
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
        isa: llvm.isa.clone(),
        code_model: llvm.code_model.clone(),
        relocation_model: llvm.relocation_model.clone(),
        sysroot: llvm.sysroot.clone(),
        linker: llvm.linker.clone(),
        link_args: llvm.link_args.clone(),
        no_default_libs: llvm.no_default_libs,
        freestanding: llvm.freestanding,
    }
}

fn frontend_prepare_wave_hir(
    file_path: &Path,
    debug: &DebugFlags,
    dep: &DepFlags,
    llvm: Option<&LlvmFlags>,
) -> (String, TypedProgram) {
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
            .display_auto();
            process::exit(1);
        }
    };
    let target = target_condition_context_for_llvm(llvm);
    let code = preprocess_target_attrs(&raw_code, &target);

    let mut lexer = Lexer::new_with_file(&code, file_path.display().to_string());
    let tokens = lexer.tokenize().unwrap_or_else(|e| {
        e.display_auto();
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

    // Imports are expanded before monomorphization so generic references may
    // resolve across source files. The expanded AST retains source ownership
    // long enough to report semantic errors against the originating file.
    let import_config = build_import_config(dep, target);
    let expanded = match expand_imports_for_codegen(file_path, &code, parsed_ast, &import_config) {
        Ok(a) => a,
        Err(e) => {
            e.display_auto();
            process::exit(1);
        }
    };
    // Validate both sides of monomorphization: templates must be semantically
    // sound, and generated concrete nodes must satisfy the same language rules.
    validate_expanded_ast_or_exit(&expanded);
    let ast = match monomorphize_generics(expanded.ast) {
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
            .display_auto();
            process::exit(1);
        }
    };

    let hir = lower_wave_hir_or_exit(file_path, &code, ast);
    (code, hir)
}

pub(crate) unsafe fn check_wave_file(
    file_path: &Path,
    debug: &DebugFlags,
    dep: &DepFlags,
    llvm: &LlvmFlags,
) {
    let _ = frontend_prepare_wave_hir(file_path, debug, dep, Some(llvm));
}

pub(crate) unsafe fn emit_wave_ast_text(
    file_path: &Path,
    debug: &DebugFlags,
    dep: &DepFlags,
    llvm: &LlvmFlags,
) -> String {
    let (_, hir) = frontend_prepare_wave_hir(file_path, debug, dep, Some(llvm));
    format!("{:#?}\n", hir.syntax())
}

pub(crate) unsafe fn emit_wave_ir_text(
    file_path: &Path,
    opt_flag: &str,
    debug: &DebugFlags,
    dep: &DepFlags,
    llvm: &LlvmFlags,
) -> String {
    let (code, hir) = frontend_prepare_wave_hir(file_path, debug, dep, Some(llvm));
    let backend_opts = build_backend_options(llvm);

    let ir = match run_panic_guarded(|| unsafe { generate_ir(&hir, opt_flag, &backend_opts) }) {
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
    let (code, hir) = frontend_prepare_wave_hir(file_path, debug, dep, Some(llvm));
    emit_wave_codegen_file_from_hir(file_path, &code, &hir, opt_flag, debug, llvm, output, kind);
}

unsafe fn emit_wave_codegen_file_from_hir(
    file_path: &Path,
    code: &str,
    hir: &TypedProgram,
    opt_flag: &str,
    debug: &DebugFlags,
    llvm: &LlvmFlags,
    output: &Path,
    kind: CodegenFileKind,
) {
    let backend_opts = build_backend_options(llvm);

    if debug.ir {
        let ir = match run_panic_guarded(|| unsafe { generate_ir(hir, opt_flag, &backend_opts) }) {
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
        emit_codegen_file(hir, opt_flag, &backend_opts, output, kind);
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
            .display_auto();
            process::exit(1);
        }
    };
    let target = target_condition_context_for_llvm(Some(llvm));
    let code = preprocess_target_attrs(&raw_code, &target);

    let mut lexer = Lexer::new_with_file(&code, file_path.display().to_string());
    let tokens = lexer.tokenize().unwrap_or_else(|e| {
        e.display_auto();
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

    let import_config = build_import_config(dep, target);

    let expanded = match expand_imports_for_codegen(file_path, &code, ast, &import_config) {
        Ok(a) => a,
        Err(e) => {
            e.display_auto();
            process::exit(1);
        }
    };
    validate_expanded_ast_or_exit(&expanded);
    let ast = match monomorphize_generics(expanded.ast) {
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
            .display_auto();
            process::exit(1);
        }
    };

    let hir = lower_wave_hir_or_exit(file_path, &code, ast);

    let file_stem = file_path.file_stem().unwrap().to_str().unwrap();
    let object_patch = format!("{}.o", file_stem);
    emit_wave_codegen_file_from_hir(
        file_path,
        &code,
        &hir,
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
        .display_auto();
        process::exit(1);
    });
    let target = target_condition_context_for_llvm(Some(llvm));
    let code = preprocess_target_attrs(&raw_code, &target);

    let mut lexer = Lexer::new_with_file(&code, file_path.display().to_string());
    let tokens = lexer.tokenize().unwrap_or_else(|e| {
        e.display_auto();
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

    let import_config = build_import_config(dep, target);

    let expanded = expand_imports_for_codegen(file_path, &code, ast, &import_config)
        .unwrap_or_else(|e| {
            e.display_auto();
            process::exit(1);
        });
    validate_expanded_ast_or_exit(&expanded);
    let ast = monomorphize_generics(expanded.ast).unwrap_or_else(|msg| {
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
        .display_auto();
        process::exit(1);
    });

    let hir = lower_wave_hir_or_exit(file_path, &code, ast);

    let file_stem = file_path.file_stem().unwrap().to_str().unwrap();
    let default_object_path = PathBuf::from(format!("{}.o", file_stem));
    let output_path = output.unwrap_or(default_object_path.as_path());
    emit_wave_codegen_file_from_hir(
        file_path,
        &code,
        &hir,
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
