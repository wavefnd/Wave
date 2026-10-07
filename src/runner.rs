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
use ::hir::TypedProgram;
use ::parser::ast::*;
use ::parser::generics::monomorphize_generics;
use ::parser::import::*;
use ::parser::verification::validate_program_detailed;
use ::parser::*;
use lexer::Lexer;
use llvm::backend::*;
use llvm::codegen::target::target_spec_for_triple;
use llvm::codegen::*;
use std::path::{Path, PathBuf};
use std::process::Stdio;

pub(crate) fn child_exit_code(status: std::process::ExitStatus) -> i32 {
    if let Some(code) = status.code() {
        return code;
    }
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if let Some(signal) = status.signal() {
            return 128 + signal;
        }
    }
    1
}
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

        wave_err = wave_err.with_span(err.span()).with_related(err.related().iter().cloned());
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
        },
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
    let origin = expanded.origins.get(diagnostic.top_level_index).copied().unwrap_or(0);
    let source_unit = expanded.sources.get(origin).unwrap_or(&expanded.sources[0]);
    let span = diagnostic.span.clone();
    let diagnostic_source = span
        .as_ref()
        .and_then(|span| {
            expanded.sources.iter().find(|unit| unit.path.to_string_lossy() == span.file)
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

        let loc = info.location().map(|l| format!("{}:{}:{}", l.file(), l.line(), l.column()));

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
            let captured =
                captured.lock().ok().and_then(|g| g.clone()).unwrap_or((fallback.clone(), None));

            let msg = if captured.0.trim().is_empty() { fallback } else { captured.0 };

            Err((msg, captured.1))
        },
    }
}

fn emit_backend_error_and_exit(
    file_path: &Path,
    source: &str,
    error: llvm::diagnostic::CodegenError,
) -> ! {
    let mut diagnostic = WaveError::new(
        WaveErrorKind::CompilationFailed(error.message.clone()),
        format!("{}: {}", error.operation, error.message),
        file_path.display().to_string(),
        0,
        0,
    )
    .with_code(if error.kind == llvm::diagnostic::CodegenErrorKind::InvalidAssembly {
        "E3401"
    } else {
        "E9002"
    })
    .with_context(format!("compiler phase: {}", error.phase));
    if let Some(span) = error.span {
        let contents = if Path::new(&span.file) == file_path {
            Some(source.to_string())
        } else {
            fs::read_to_string(&span.file).ok()
        };
        diagnostic = diagnostic.with_span(Some(&span));
        if let Some(contents) = contents {
            diagnostic = diagnostic.with_source_code(contents);
        }
    }
    diagnostic.display_auto();
    process::exit(1);
}

fn emit_codegen_panic_and_exit(
    file_path: &Path,
    _source: &str,
    phase: &str,
    message: String,
    location: Option<String>,
) -> ! {
    let mut diagnostic = WaveError::new(
        WaveErrorKind::CompilationFailed(message.clone()),
        "compiler internal error during code generation",
        file_path.display().to_string(),
        0,
        0,
    )
    .with_code("E9001")
    .with_context(format!("compiler phase: {phase}"))
    .with_found(message)
    .with_help("report this compiler invariant failure with the source and target");
    if let Some(location) = location {
        diagnostic = diagnostic.with_note(format!("compiler panic location: {location}"));
    }
    diagnostic.display_auto();
    process::exit(1);
}

fn build_import_config(dep: &DepFlags, target: TargetConditionContext) -> ImportConfig {
    let mut config = ImportConfig {
        target,
        dep_roots: Vec::new(),
        dep_packages: std::collections::HashMap::new(),
        std_root: dep
            .resolved_std_root
            .clone()
            .expect("compilation environment must resolve std before frontend preparation"),
    };

    for root in &dep.roots {
        config.dep_roots.push(PathBuf::from(root));
    }

    for package in &dep.packages {
        config.dep_packages.insert(package.name.clone(), PathBuf::from(&package.path));
    }

    config
}

/// Discover inputs for the driver's no-clobber check, before any emit writes.
pub fn wave_input_paths(path: &Path, dep: &DepFlags, llvm: &LlvmFlags) -> Vec<PathBuf> {
    let raw = std::fs::read_to_string(path).unwrap_or_else(|error| {
        eprintln!("cannot read '{}': {error}", path.display());
        process::exit(1);
    });
    let target = target_condition_context_for_llvm(Some(llvm));
    let source = preprocess_target_attrs(&raw, &target);
    let mut lexer = Lexer::new_with_file(&source, path.display().to_string());
    let tokens = lexer.tokenize().unwrap_or_else(|error| {
        error.display_auto();
        process::exit(1);
    });
    let ast = parse_wave_tokens_or_exit(path, &source, &tokens);
    let config = build_import_config(dep, target);
    resolve_import_graph(path, &source, ast, &config)
        .unwrap_or_else(|error| {
            error.display_auto();
            process::exit(1);
        })
        .sources
        .into_iter()
        .map(|source| source.path)
        .collect()
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
    let pointer_bits = if import_config.target.arch.as_deref() == Some("wasm32") { 32 } else { 64 };
    ::hir::resolve_target_types(&mut graph.ast, pointer_bits).map_err(|message| {
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
            .map(|source| SemanticSourceUnit { path: source.path, source: source.source })
            .collect(),
    })
}

#[allow(dead_code)]
fn default_output_path(file_path: &Path, directory: &Path, extension: Option<&str>) -> PathBuf {
    let Some(stem) = file_path.file_stem().filter(|stem| !stem.is_empty()) else {
        WaveError::new(
            WaveErrorKind::FileWriteError(file_path.display().to_string()),
            "cannot derive an output name from the source path",
            file_path.display().to_string(),
            0,
            0,
        )
        .with_code("E1005")
        .with_help("pass an explicit output path with -o <file>")
        .display_auto();
        process::exit(1);
    };
    let mut name = stem.to_os_string();
    if let Some(extension) = extension {
        name.push(".");
        name.push(extension);
    }
    directory.join(name)
}

fn utf8_output_path(path: &Path, file_path: &Path) -> String {
    match path.to_str() {
        Some(value) => value.to_owned(),
        None => {
            WaveError::new(
                WaveErrorKind::FileWriteError(path.display().to_string()),
                "the backend requires a UTF-8 output path",
                file_path.display().to_string(),
                0,
                0,
            )
            .with_code("E1005")
            .with_help("pass a UTF-8 output path with -o <file>")
            .display_auto();
            process::exit(1);
        },
    }
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
                    format!("failed to create output directory `{}`: {}", parent.display(), err),
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

    utf8_output_path(output, file_path)
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
        },
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
        print_ast_dump(&parsed_ast, &code, file_path);
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
        },
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
        },
    };

    let hir = lower_wave_hir_or_exit(file_path, &code, ast);
    (code, hir)
}

pub(crate) fn emit_whale_ir_text(
    file_path: &Path,
    debug: &DebugFlags,
    dep: &DepFlags,
    target: &LlvmFlags,
) -> String {
    let (source, program) = frontend_prepare_wave_hir(file_path, debug, dep, Some(target));
    match whale::emit_ir(&program) {
        Ok(text) => text,
        Err(diagnostic) => {
            let diagnostic_source = diagnostic
                .span
                .as_ref()
                .and_then(|s| fs::read_to_string(&s.file).ok())
                .unwrap_or(source);
            WaveError::new(
                WaveErrorKind::InvalidStatement(diagnostic.message.clone()),
                diagnostic.message,
                file_path.display().to_string(),
                0,
                0,
            )
            .with_code("E4001")
            .with_source_code(diagnostic_source)
            .with_span(diagnostic.span.as_ref())
            .with_context("Whale IR lowering")
            .display_auto();
            process::exit(1);
        },
    }
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
    _debug: &DebugFlags,
    _dep: &DepFlags,
    _llvm: &LlvmFlags,
    format: ::parser::ast_output::AstFormat,
) -> Result<String, String> {
    let code = fs::read_to_string(file_path).map_err(|e| e.to_string())?;
    let mut lexer = Lexer::new_with_file(&code, file_path.display().to_string());
    let tokens = lexer.tokenize().unwrap_or_else(|e| {
        e.display_auto();
        process::exit(1);
    });
    let ast = parse_wave_tokens_or_exit(file_path, &code, &tokens);
    ::parser::ast_output::dump(&ast, &code, &file_path.to_string_lossy(), format)
        .map_err(|e| e.to_string())
}

fn print_ast_dump(ast: &[::parser::ast::ASTNode], code: &str, file: &Path) {
    match ::parser::ast_output::dump(
        ast,
        code,
        &file.to_string_lossy(),
        ::parser::ast_output::AstFormat::Sexpr,
    ) {
        Ok(text) => println!("\n===== AST =====\n{text}"),
        Err(error) => {
            eprintln!("AST dump failed: {error}");
            process::exit(1);
        },
    }
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
        Ok(Ok(ir)) => ir,
        Ok(Err(error)) => emit_backend_error_and_exit(file_path, &code, error),
        Err((msg, loc)) => {
            emit_codegen_panic_and_exit(file_path, &code, "llvm-ir-generation", msg, loc)
        },
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
            Ok(Ok(ir)) => ir,
            Ok(Err(error)) => emit_backend_error_and_exit(file_path, code, error),
            Err((msg, loc)) => {
                emit_codegen_panic_and_exit(file_path, code, "llvm-ir-generation", msg, loc)
            },
        };
        println!("\n===== LLVM IR =====\n{}", ir);
    }

    if let Some(parent) = output.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent).unwrap_or_else(|e| {
                emit_backend_error_and_exit(
                    file_path,
                    code,
                    llvm::diagnostic::CodegenError::new(
                        llvm::diagnostic::CodegenPhase::Emission,
                        format!("create output directory {}", parent.display()),
                        e,
                    ),
                )
            });
        }
    }

    match run_panic_guarded(|| unsafe {
        emit_codegen_file(hir, opt_flag, &backend_opts, output, kind)
    }) {
        Ok(Ok(())) => {},
        Ok(Err(error)) => emit_backend_error_and_exit(file_path, code, error),
        Err((msg, loc)) => {
            emit_codegen_panic_and_exit(file_path, code, codegen_file_phase(kind), msg, loc)
        },
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
    emit_wave_codegen_file(file_path, opt_flag, debug, dep, llvm, output, CodegenFileKind::Bitcode);
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
        },
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
        print_ast_dump(&ast, &code, file_path);
    }

    let import_config = build_import_config(dep, target);

    let expanded = match expand_imports_for_codegen(file_path, &code, ast, &import_config) {
        Ok(a) => a,
        Err(e) => {
            e.display_auto();
            process::exit(1);
        },
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
        },
    };

    let hir = lower_wave_hir_or_exit(file_path, &code, ast);

    let object_patch =
        utf8_output_path(&default_output_path(file_path, Path::new(""), Some("o")), file_path);
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

    let exe_patch =
        utf8_output_path(&default_output_path(file_path, Path::new("target"), None), file_path);
    let backend_opts = build_backend_options(llvm);

    match run_panic_guarded(|| {
        link_objects(
            std::slice::from_ref(&object_patch),
            &exe_patch,
            &link.libs,
            &link.paths,
            &backend_opts,
        )
    }) {
        Ok(Ok(())) => {},
        Ok(Err(error)) => emit_backend_error_and_exit(file_path, &code, error),
        Err((msg, loc)) => emit_codegen_panic_and_exit(file_path, &code, "native-link", msg, loc),
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
        process::exit(child_exit_code(status));
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
        print_ast_dump(&ast, &code, file_path);
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

    let output_path = output
        .map(Path::to_path_buf)
        .unwrap_or_else(|| default_output_path(file_path, Path::new(""), Some("o")));
    let object_path = utf8_output_path(&output_path, file_path);
    emit_wave_codegen_file_from_hir(
        file_path,
        &code,
        &hir,
        opt_flag,
        debug,
        llvm,
        &output_path,
        CodegenFileKind::Object,
    );

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

    let exe_path = output
        .map(Path::to_path_buf)
        .unwrap_or_else(|| default_output_path(file_path, Path::new("target"), None));
    let default_exe_path = utf8_output_path(&exe_path, file_path);
    let source = fs::read_to_string(file_path).unwrap_or_default();
    let exe_path =
        resolve_output_target(&default_exe_path, output, file_path, &source, "native-link");
    let backend_opts = build_backend_options(llvm);

    match run_panic_guarded(|| {
        link_objects(&[object_path], &exe_path, &link.libs, &link.paths, &backend_opts)
    }) {
        Ok(Ok(())) => {},
        Ok(Err(error)) => emit_backend_error_and_exit(file_path, &source, error),
        Err((msg, loc)) => emit_codegen_panic_and_exit(file_path, &source, "native-link", msg, loc),
    }

    if debug.mc {
        println!("\n===== OUTPUT BINARY =====");
        println!("{}", exe_path);
    }
}
