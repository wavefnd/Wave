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

//! `wavec` command-line parsing, validation, and execution planning.
//!
//! Raw arguments are converted into a `BuildRequest` before compilation
//! starts. Target and output contracts are validated here so the runner receives
//! a coherent request and dry-run output describes the same plan that execution
//! would follow.

use utils::wson::{self, quote as json_string, Value};

use crate::errors::CliError;
use crate::flags::{
    validate_opt_flag, DebugFlags, DepFlags, DepPackage, LinkFlags, LlvmFlags, WhaleFlags,
};
use crate::link_validation::{
    validate_loongarch64_link_inputs, validate_riscv_link_inputs, LoongArchFloatAbi, RiscvFloatAbi,
};
use crate::{runner, std as wave_std, version};
use llvm::diagnostic::{CodegenError, CodegenPhase, PendingOutput};

use crate::version::get_os_pretty_name;
use llvm::codegen::target::{
    resolve_target_options, supported_target_specs, target_spec_for_triple, CodegenTarget,
    EffectiveTargetOptions, TargetSpec,
};
use std::collections::BTreeSet;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::{self, Command as ProcessCommand, Stdio};
use std::{env, fs};
use utils::colorex::*;

#[derive(Debug)]
enum CliCommand {
    Build(BuildRequest),
    Print { item: String, target: Option<String>, format: PrintFormat },
    StdInstall(Option<String>),
    StdUpdate(Option<String>),
    Help,
    Version,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum ErrorFormat {
    #[default]
    Human,
    Json,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum PrintFormat {
    #[default]
    Human,
    Json,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum EmitKind {
    Ast,
    Ir,
    Bc,
    Asm,
    Obj,
    Bin,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum EmitSpec {
    Check,
    Set(BTreeSet<EmitKind>),
}

impl EmitSpec {
    fn default_bin() -> Self {
        let mut set = BTreeSet::new();
        set.insert(EmitKind::Bin);
        EmitSpec::Set(set)
    }

    fn is_check(&self) -> bool {
        matches!(self, EmitSpec::Check)
    }

    fn as_set(&self) -> Option<&BTreeSet<EmitKind>> {
        match self {
            EmitSpec::Set(set) => Some(set),
            EmitSpec::Check => None,
        }
    }

    fn contains(&self, kind: EmitKind) -> bool {
        match self {
            EmitSpec::Check => false,
            EmitSpec::Set(set) => set.contains(&kind),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InputKind {
    Wave,
    Ir,
    Bc,
    Asm,
    Obj,
    Archive,
}

impl InputKind {
    fn as_str(self) -> &'static str {
        match self {
            InputKind::Wave => "wave",
            InputKind::Ir => "ir",
            InputKind::Bc => "bc",
            InputKind::Asm => "asm",
            InputKind::Obj => "obj",
            InputKind::Archive => "archive",
        }
    }

    fn is_link_input(self) -> bool {
        matches!(self, InputKind::Obj | InputKind::Archive)
    }
}

#[derive(Debug, Clone)]
struct BuildRequest {
    ast_format: parser::ast_output::AstFormat,
    inputs: Vec<PathBuf>,
    output: Option<PathBuf>,
    out_dir: Option<PathBuf>,
    target_dir: Option<PathBuf>,
    emit: EmitSpec,
    input_type: Option<InputKind>,
    link_only: bool,
    run: bool,
    dry_run: bool,
    run_args: Vec<String>,
    freestanding: bool,
    entry: Option<String>,
    linker_script: Option<PathBuf>,
    no_start_files: bool,
    shared: bool,
    static_link: bool,
    pie: Option<bool>,
    error_format: ErrorFormat,
}

impl Default for BuildRequest {
    fn default() -> Self {
        Self {
            ast_format: parser::ast_output::AstFormat::default(),
            inputs: Vec::new(),
            output: None,
            out_dir: None,
            target_dir: None,
            emit: EmitSpec::default_bin(),
            input_type: None,
            link_only: false,
            run: false,
            dry_run: false,
            run_args: Vec::new(),
            freestanding: false,
            entry: None,
            linker_script: None,
            no_start_files: false,
            shared: false,
            static_link: false,
            pie: None,
            error_format: ErrorFormat::Human,
        }
    }
}

#[derive(Default, Clone)]
struct Global {
    opt: String,
    debug: DebugFlags,
    link: LinkFlags,
    dep: DepFlags,
    std_root: Option<PathBuf>,
    llvm: LlvmFlags,
    whale: WhaleFlags,
    error_format: ErrorFormat,
}

#[derive(Debug, Clone)]
struct ClassifiedInput {
    path: PathBuf,
    kind: InputKind,
}

#[derive(Debug, Clone)]
struct CompileJob {
    input: PathBuf,
    kind: InputKind,
    output: PathBuf,
}

#[derive(Debug, Clone, Default)]
struct BuildPlan {
    compile_jobs: Vec<CompileJob>,
    link_inputs: Vec<String>,
    link_output: Option<PathBuf>,
}

pub fn run() -> Result<(), CliError> {
    // The CLI grammar currently uses UTF-8 strings. Reject unrepresentable
    // arguments before planning any outputs; args() panics and lossy decoding
    // can alias distinct filesystem names.
    let args: Vec<String> = env::args_os()
        .skip(1)
        .map(|arg| {
            arg.into_string().map_err(|arg| {
                CliError::usage(format!("command-line argument is not valid UTF-8: {arg:?}"))
            })
        })
        .collect::<Result<_, _>>()?;
    if args.is_empty() {
        return Err(CliError::usage("not enough arguments"));
    }

    let (global, rest) = parse_global(args)?;
    let cmd = parse_command(rest)?;

    dispatch(global, cmd)
}

pub fn args_request_json_errors<I, S>(args: I) -> bool
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let mut expect_value = false;
    let mut wants_json = false;
    for arg in args {
        let arg = arg.as_ref();
        if arg == "--" {
            break;
        }
        if expect_value {
            if arg == "json" {
                wants_json = true;
            }
            expect_value = false;
            continue;
        }
        if arg == "--error-format" {
            expect_value = true;
            continue;
        }
        if let Some(value) = arg.strip_prefix("--error-format=") {
            if value == "json" {
                wants_json = true;
            }
        }
    }
    wants_json
}

fn dispatch(mut global: Global, cmd: CliCommand) -> Result<(), CliError> {
    if global.std_root.is_some() && matches!(&cmd, CliCommand::Build(_) | CliCommand::Print { .. })
    {
        let root = parser::import::ResolvedStdRoot::resolve(global.std_root.as_deref())
            .map_err(|error| CliError::usage(error.message))?;
        global.dep.resolved_std_root = Some(Ok(root));
    }

    match cmd {
        CliCommand::Version => {
            print_version_for_backend(global.whale.enabled);
            Ok(())
        },
        CliCommand::Help => {
            print_help();
            Ok(())
        },
        CliCommand::Build(build) => dispatch_build(&global, &build),
        CliCommand::Print { item, target, format } => {
            dispatch_print(&global, &item, target.as_deref(), format)
        },
        CliCommand::StdInstall(reference) => {
            wave_std::std_install_with_reference(reference.as_deref())
        },
        CliCommand::StdUpdate(reference) => {
            wave_std::std_update_with_reference(reference.as_deref())
        },
    }
}

/// Whale currently produces verified IR only. Keep its capability checks out of
/// LLVM target resolution: an LLVM installation must not decide Whale support.
fn dispatch_whale_build(global: &Global, build: &BuildRequest) -> Result<(), CliError> {
    let mut global = global.clone();
    let target = global.llvm.target.get_or_insert_with(|| "x86_64-unknown-linux-gnu".into());
    if target != "x86_64-unknown-linux-gnu" {
        return Err(CliError::usage(
            "Whale currently supports only --target x86_64-unknown-linux-gnu",
        ));
    }
    if global.opt != "-O0" {
        return Err(CliError::usage("Whale currently supports only -O0"));
    }
    if !build.emit.is_check()
        && !matches!(build.emit.as_set(), Some(set) if set.len() == 1 && set.contains(&EmitKind::Ir))
    {
        return Err(CliError::usage("Whale currently supports check and --emit=ir (.wir) only; executable, object, assembly and bitcode output are not available"));
    }
    let llvm = &global.llvm;
    if build.run
        || build.link_only
        || build.shared
        || build.static_link
        || build.pie.is_some()
        || build.freestanding
        || build.entry.is_some()
        || build.linker_script.is_some()
        || build.no_start_files
        || !global.link.libs.is_empty()
        || !global.link.paths.is_empty()
        || llvm.cpu.is_some()
        || llvm.features.is_some()
        || llvm.abi.is_some()
        || llvm.isa.is_some()
        || llvm.code_model.is_some()
        || llvm.relocation_model.is_some()
        || llvm.sysroot.is_some()
        || llvm.linker.is_some()
        || !llvm.link_args.is_empty()
        || llvm.no_default_libs
        || global.debug.mc
        || global.debug.hex
    {
        return Err(CliError::usage("Whale IR does not support execution, linking, LLVM tuning, or machine-code debug options"));
    }
    let classified = classify_inputs(build)?;
    if classified.is_empty() || classified.iter().any(|i| i.kind != InputKind::Wave) {
        return Err(CliError::usage("Whale requires Wave source inputs"));
    }
    if build.emit.is_check() && (build.output.is_some() || build.out_dir.is_some()) {
        return Err(CliError::usage(
            "--emit=check does not produce artifacts; remove -o/--out-dir",
        ));
    }
    if build.output.is_some() && (classified.len() != 1 || build.out_dir.is_some()) {
        return Err(CliError::usage(
            "Whale -o requires one input and cannot be combined with --out-dir",
        ));
    }
    if global.dep.resolved_std_root.is_none() {
        global.dep.resolved_std_root = Some(parser::import::ResolvedStdRoot::resolve(None));
    }
    let outputs: Vec<_> = if build.emit.is_check() {
        vec![]
    } else {
        classified
            .iter()
            .enumerate()
            .map(|(i, input)| {
                let output = build.output.clone().unwrap_or_else(|| {
                    resolve_extra_emit_output_path(build, input, EmitKind::Ir, i, classified.len())
                        .with_extension("wir")
                });
                (output, None)
            })
            .collect()
    };
    let mut inputs = build.inputs.clone();
    for input in &classified {
        inputs.extend(runner::wave_input_paths(&input.path, &global.dep, &global.llvm));
    }
    crate::output_guard::validate(&inputs, &outputs)?;
    if build.dry_run {
        for (i, input) in classified.iter().enumerate() {
            println!(
                "Whale / Linux amd64 / O0: {} -> {}",
                input.path.display(),
                outputs
                    .get(i)
                    .map(|(p, _)| p.display().to_string())
                    .unwrap_or_else(|| "verify only".into())
            );
        }
        return Ok(());
    }
    // Lower every source before publishing any output, including multi-input requests.
    let texts: Vec<_> = classified
        .iter()
        .map(|input| {
            runner::emit_whale_ir_text(&input.path, &global.debug, &global.dep, &global.llvm)
        })
        .collect();
    if global.debug.ir {
        for text in &texts {
            println!("{text}");
        }
    }
    for ((output, _), text) in outputs.iter().zip(texts) {
        ensure_parent_dir(output)?;
        let pending = PendingOutput::new(output)?;
        fs::write(pending.path(), text)?;
        pending.commit()?;
    }
    Ok(())
}

fn dispatch_build(global: &Global, build: &BuildRequest) -> Result<(), CliError> {
    let mut build = build.clone();
    if global.error_format == ErrorFormat::Json {
        build.error_format = ErrorFormat::Json;
    }
    configure_wave_error_format(build.error_format);

    if global.whale.enabled {
        return dispatch_whale_build(global, &build);
    }

    let mut effective_global = effective_global_for_build(global, &build);
    resolve_target_configuration(&mut effective_global.llvm)?;
    resolve_build_sysroot(&mut effective_global.llvm, &build);
    if effective_global.dep.resolved_std_root.is_none() {
        effective_global.dep.resolved_std_root =
            Some(parser::import::ResolvedStdRoot::resolve(None));
    }
    let classified = classify_inputs(&build)?;
    validate_build_request(&effective_global, &build, &classified)?;

    let plan = create_build_plan(&effective_global, &build, &classified)?;
    validate_output_paths(&effective_global, &build, &classified, &plan)?;

    if build.dry_run {
        print_dry_run(&effective_global, &build, &classified, &plan);
        return Ok(());
    }

    if build.emit.is_check() {
        for input in &classified {
            unsafe {
                runner::check_wave_file(
                    &input.path,
                    &effective_global.debug,
                    &effective_global.dep,
                    &effective_global.llvm,
                );
            }
        }
        return Ok(());
    }

    let Some(emit_set) = build.emit.as_set() else {
        return Err(CliError::usage("invalid emit mode"));
    };

    execute_explicit_emit_artifacts(&effective_global, &build, &classified, emit_set)?;

    for job in &plan.compile_jobs {
        match job.kind {
            InputKind::Wave => unsafe {
                runner::object_build_wave_file(
                    &job.input,
                    &effective_global.opt,
                    &effective_global.debug,
                    &effective_global.dep,
                    &effective_global.llvm,
                    Some(job.output.as_path()),
                );
            },
            InputKind::Ir | InputKind::Bc | InputKind::Asm => {
                compile_non_wave_to_object(&effective_global, job)?;
            },
            InputKind::Obj | InputKind::Archive => {},
        }
    }

    if let Some(link_output) = &plan.link_output {
        if plan.link_inputs.is_empty() {
            return Err(CliError::CommandFailed(
                "no object inputs available for link stage".to_string(),
            ));
        }

        link_objects(&effective_global, &build, &plan.link_inputs, link_output)?;

        if build.run {
            let (program, args) = build_execute_command(&effective_global, &build, link_output);
            let status = ProcessCommand::new(&program)
                .args(&args)
                .stdin(Stdio::inherit())
                .stdout(Stdio::inherit())
                .stderr(Stdio::inherit())
                .status()
                .map_err(|e| {
                    CliError::CommandFailed(format!("failed to run `{}`: {}", program, e))
                })?;

            if !status.success() {
                process::exit(crate::runner::child_exit_code(status));
            }
        }
    }

    Ok(())
}

fn effective_global_for_build(global: &Global, build: &BuildRequest) -> Global {
    let mut out = global.clone();

    if out.llvm.target.is_none() {
        out.llvm.target = Some(host_target_triple());
    }

    if build.freestanding {
        out.llvm.no_default_libs = true;
        out.llvm.freestanding = true;
    }
    if build.no_start_files {
        out.llvm.link_args.push("-nostartfiles".to_string());
    }
    if let Some(script) = &build.linker_script {
        out.llvm.link_args.push(format!("-Wl,-T,{}", script.to_string_lossy()));
    }

    out
}

fn configure_wave_error_format(format: ErrorFormat) {
    match format {
        ErrorFormat::Human => env::remove_var("WAVE_ERROR_FORMAT"),
        ErrorFormat::Json => env::set_var("WAVE_ERROR_FORMAT", "json"),
    }
}

fn dispatch_print(
    global: &Global,
    item: &str,
    target_arg: Option<&str>,
    format: PrintFormat,
) -> Result<(), CliError> {
    if global.whale.enabled {
        return Err(CliError::usage("Whale capability queries are not yet available; supported target: x86_64-unknown-linux-gnu, optimization: O0, output: IR"));
    }
    let target = target_arg
        .map(|s| s.to_string())
        .or_else(|| global.llvm.target.clone())
        .unwrap_or_else(host_target_triple);

    match format {
        PrintFormat::Human => dispatch_print_human(global, item, &target),
        PrintFormat::Json => dispatch_print_json(global, item, &target),
    }
}

fn dispatch_print_human(global: &Global, item: &str, target: &str) -> Result<(), CliError> {
    match item {
        "host-target" | "default-target" => {
            println!("{}", host_target_triple());
            Ok(())
        },
        "host" => {
            validate_target_options_for(&host_target_triple(), &global.llvm)?;
            print_target_spec_human(global, &host_target_triple());
            Ok(())
        },
        "target-spec" => {
            validate_target_options_for(target, &global.llvm)?;
            print_target_spec_human(global, target);
            Ok(())
        },
        "target-list" | "supported-targets" => {
            for t in supported_targets() {
                println!("{}", t);
            }
            Ok(())
        },
        "sysroot" => {
            let selection = effective_sysroot_selection(global, target)?;
            if let Some(selection) = selection {
                println!("{}", selection.path);
            } else {
                println!();
            }
            Ok(())
        },
        "std-path" => {
            if let Some(path) = default_std_path(global) {
                println!("{}", path);
            } else {
                println!();
            }
            Ok(())
        },
        "dep-search-paths" => {
            if let Some(path) = default_std_path(global) {
                println!("{}", path);
            }
            Ok(())
        },
        "default-linker" => {
            ensure_supported_target(target)?;
            let target_global = global_with_target(global, target);
            println!("{}", default_linker_name(&target_global));
            Ok(())
        },
        "supported-input-types" => {
            for t in supported_input_types() {
                println!("{}", t);
            }
            Ok(())
        },
        "supported-emit-kinds" => {
            println!("check (control-mode)");
            for e in supported_artifact_emit_kinds() {
                println!("{}", e);
            }
            Ok(())
        },
        "supported-print-items" => {
            for item in supported_print_items() {
                println!("{}", item);
            }
            Ok(())
        },
        "cpu-list" => {
            let spec = ensure_supported_target(target)?;
            for cpu in spec.cpus {
                println!("{}", cpu);
            }
            Ok(())
        },
        "target-features" => {
            let spec = ensure_supported_target(target)?;
            for feat in spec.features {
                println!("{}", feat);
            }
            Ok(())
        },
        _ => Err(CliError::usage(format!("unknown print item: {}", item))),
    }
}

fn dispatch_print_json(global: &Global, item: &str, target: &str) -> Result<(), CliError> {
    match item {
        "host-target" | "default-target" => {
            println!("{}", json_string(&host_target_triple()));
            Ok(())
        },
        "host" => {
            validate_target_options_for(&host_target_triple(), &global.llvm)?;
            println!("{}", target_spec_json(global, &host_target_triple()));
            Ok(())
        },
        "target-spec" => {
            validate_target_options_for(target, &global.llvm)?;
            println!("{}", target_spec_json(global, target));
            Ok(())
        },
        "target-list" | "supported-targets" => {
            println!("{}", json_string_array(supported_targets()));
            Ok(())
        },
        "sysroot" => {
            let selection = effective_sysroot_selection(global, target)?;
            println!(
                "{}",
                schema_json(&Value::optional_string(
                    selection.as_ref().map(|value| value.path.as_str())
                ))
            );
            Ok(())
        },
        "std-path" => {
            println!(
                "{}",
                schema_json(&Value::optional_string(default_std_path(global).as_deref()))
            );
            Ok(())
        },
        "dep-search-paths" => {
            let paths = default_std_path(global).into_iter().collect::<Vec<_>>();
            println!("{}", schema_json(&Value::strings(&paths)));
            Ok(())
        },
        "default-linker" => {
            ensure_supported_target(target)?;
            let target_global = global_with_target(global, target);
            println!("{}", json_string(&default_linker_name(&target_global)));
            Ok(())
        },
        "supported-input-types" => {
            println!("{}", json_string_array(supported_input_types()));
            Ok(())
        },
        "supported-emit-kinds" => {
            let mut kinds = vec!["check"];
            kinds.extend(supported_artifact_emit_kinds());
            println!("{}", json_string_array(kinds));
            Ok(())
        },
        "supported-print-items" => {
            println!("{}", json_string_array(supported_print_items()));
            Ok(())
        },
        "cpu-list" => {
            let spec = ensure_supported_target(target)?;
            println!("{}", json_string_array(spec.cpus.to_vec()));
            Ok(())
        },
        "target-features" => {
            let spec = ensure_supported_target(target)?;
            println!("{}", json_string_array(spec.features.to_vec()));
            Ok(())
        },
        _ => Err(CliError::usage(format!("unknown print item: {}", item))),
    }
}

fn parse_global(args: Vec<String>) -> Result<(Global, Vec<String>), CliError> {
    let mut g = Global {
        opt: "-O0".to_string(),
        debug: DebugFlags::default(),
        link: LinkFlags::default(),
        dep: DepFlags::default(),
        std_root: None,
        llvm: LlvmFlags::default(),
        whale: WhaleFlags::default(),
        error_format: ErrorFormat::Human,
    };

    let mut rest: Vec<String> = Vec::new();
    let mut i = 0usize;

    while i < args.len() {
        let a = &args[i];

        if a == "--" {
            rest.push("--".to_string());
            rest.extend_from_slice(&args[i + 1..]);
            break;
        }

        if a == "--whale" {
            g.whale.enabled = true;
            i += 1;
            continue;
        }

        if a == "--llvm" {
            i += 1;
            continue;
        }

        if let Some(v) = a.strip_prefix("--error-format=") {
            g.error_format = parse_error_format(v)?;
            i += 1;
            continue;
        }

        if a == "--error-format" {
            let v = args
                .get(i + 1)
                .ok_or_else(|| CliError::usage("missing value: --error-format <human,json>"))?;
            g.error_format = parse_error_format(v)?;
            i += 2;
            continue;
        }

        if parse_llvm_backend_option(&args, &mut i, &mut g.llvm)? {
            continue;
        }

        if a.starts_with("-O") {
            if !validate_opt_flag(a) {
                return Err(CliError::usage(format!("invalid optimization flag: {}", a)));
            }
            g.opt = a.clone();
            i += 1;
            continue;
        }

        if let Some(mode) = a.strip_prefix("--debug-wave=") {
            for value in mode.split(',').map(str::trim).filter(|v| !v.is_empty()) {
                if !matches!(value, "tokens" | "ast" | "ir" | "mc" | "hex" | "all") {
                    return Err(CliError::usage(format!("unknown --debug-wave mode: {value}")));
                }
            }
            g.debug.apply(mode);
            i += 1;
            continue;
        }

        if a == "--debug-wave" {
            let mode = args.get(i + 1).ok_or_else(|| {
                CliError::usage("missing value: --debug-wave <tokens,ast,ir,mc,hex,all,...>")
            })?;
            for value in mode.split(',').map(str::trim).filter(|v| !v.is_empty()) {
                if !matches!(value, "tokens" | "ast" | "ir" | "mc" | "hex" | "all") {
                    return Err(CliError::usage(format!("unknown --debug-wave mode: {value}")));
                }
            }
            g.debug.apply(mode);
            i += 2;
            continue;
        }

        if let Some(lib) = a.strip_prefix("--link=") {
            g.link.libs.push(lib.to_string());
            i += 1;
            continue;
        }

        if a == "--link" {
            let lib =
                args.get(i + 1).ok_or_else(|| CliError::usage("missing value: --link <lib>"))?;
            g.link.libs.push(lib.to_string());
            i += 2;
            continue;
        }

        if let Some(p) = a.strip_prefix("-L") {
            if p.is_empty() {
                let path =
                    args.get(i + 1).ok_or_else(|| CliError::usage("missing value: -L <path>"))?;
                g.link.paths.push(path.to_string());
                i += 2;
            } else if let Some(native) = p.strip_prefix("native=") {
                g.link.paths.push(native.to_string());
                i += 1;
            } else {
                g.link.paths.push(p.to_string());
                i += 1;
            }
            continue;
        }

        if a == "--std-root" || a.starts_with("--std-root=") {
            let (path, consumed) = if let Some(path) = a.strip_prefix("--std-root=") {
                (path, 1)
            } else {
                (
                    args.get(i + 1)
                        .map(String::as_str)
                        .ok_or_else(|| CliError::usage("missing value: --std-root <path>"))?,
                    2,
                )
            };
            if path.is_empty() || path.starts_with("--") {
                return Err(CliError::usage("missing value: --std-root <path>"));
            }
            if g.std_root.is_some() {
                return Err(CliError::usage("pass --std-root only once"));
            }
            g.std_root = Some(PathBuf::from(path));
            i += consumed;
            continue;
        }

        if let Some(path) = a.strip_prefix("--dep-root=") {
            if path.trim().is_empty() {
                return Err(CliError::usage("missing value: --dep-root <path>"));
            }
            g.dep.roots.push(path.to_string());
            i += 1;
            continue;
        }

        if a == "--dep-root" {
            let path = args
                .get(i + 1)
                .ok_or_else(|| CliError::usage("missing value: --dep-root <path>"))?;
            if path.trim().is_empty() {
                return Err(CliError::usage("missing value: --dep-root <path>"));
            }
            g.dep.roots.push(path.to_string());
            i += 2;
            continue;
        }

        if let Some(spec) = a.strip_prefix("--dep=") {
            let dep = parse_dep_spec(spec)?;
            if g.dep.packages.iter().any(|p| p.name == dep.name) {
                return Err(CliError::usage(format!(
                    "duplicate dependency mapping for '{}': pass --dep once per package",
                    dep.name
                )));
            }
            g.dep.packages.push(dep);
            i += 1;
            continue;
        }

        if a == "--dep" {
            let spec = args
                .get(i + 1)
                .ok_or_else(|| CliError::usage("missing value: --dep <name>=<path>"))?;
            let dep = parse_dep_spec(spec)?;
            if g.dep.packages.iter().any(|p| p.name == dep.name) {
                return Err(CliError::usage(format!(
                    "duplicate dependency mapping for '{}': pass --dep once per package",
                    dep.name
                )));
            }
            g.dep.packages.push(dep);
            i += 2;
            continue;
        }

        rest.push(a.clone());
        i += 1;
    }

    Ok((g, rest))
}

fn parse_dep_spec(spec: &str) -> Result<DepPackage, CliError> {
    let trimmed = spec.trim();
    if trimmed.is_empty() {
        return Err(CliError::usage("invalid --dep value: expected <name>=<path>"));
    }

    let Some((name, path)) = trimmed.split_once('=') else {
        return Err(CliError::usage("invalid --dep value: expected <name>=<path>"));
    };

    let name = name.trim();
    let path = path.trim();

    if name.is_empty() || path.is_empty() {
        return Err(CliError::usage("invalid --dep value: expected <name>=<path>"));
    }

    let mut chars = name.chars();
    let valid = if let Some(first) = chars.next() {
        (first.is_ascii_alphabetic() || first == '_')
            && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
    } else {
        false
    };

    if !valid {
        return Err(CliError::usage("invalid --dep package name: use [A-Za-z_][A-Za-z0-9_]*"));
    }

    Ok(DepPackage { name: name.to_string(), path: path.to_string() })
}

fn parse_llvm_backend_option(
    args: &[String],
    i: &mut usize,
    llvm: &mut LlvmFlags,
) -> Result<bool, CliError> {
    let a = &args[*i];

    if let Some(v) = a.strip_prefix("--target=") {
        if v.trim().is_empty() {
            return Err(CliError::usage("missing value: --target=<triple>"));
        }
        llvm.target = Some(v.to_string());
        llvm.target_explicit = true;
        *i += 1;
        return Ok(true);
    }
    if a == "--target" {
        let v =
            args.get(*i + 1).ok_or_else(|| CliError::usage("missing value: --target <triple>"))?;
        if v.trim().is_empty() {
            return Err(CliError::usage("missing value: --target <triple>"));
        }
        llvm.target = Some(v.to_string());
        llvm.target_explicit = true;
        *i += 2;
        return Ok(true);
    }

    if let Some(v) = a.strip_prefix("--cpu=") {
        if v.trim().is_empty() {
            return Err(CliError::usage("missing value: --cpu=<name>"));
        }
        llvm.cpu = Some(v.to_string());
        *i += 1;
        return Ok(true);
    }
    if a == "--cpu" {
        let v = args.get(*i + 1).ok_or_else(|| CliError::usage("missing value: --cpu <name>"))?;
        if v.trim().is_empty() {
            return Err(CliError::usage("missing value: --cpu <name>"));
        }
        llvm.cpu = Some(v.to_string());
        *i += 2;
        return Ok(true);
    }

    if let Some(v) = a.strip_prefix("--features=") {
        if v.trim().is_empty() {
            return Err(CliError::usage("missing value: --features=<csv>"));
        }
        llvm.features = Some(v.to_string());
        *i += 1;
        return Ok(true);
    }
    if a == "--features" {
        let v =
            args.get(*i + 1).ok_or_else(|| CliError::usage("missing value: --features <csv>"))?;
        if v.trim().is_empty() {
            return Err(CliError::usage("missing value: --features <csv>"));
        }
        llvm.features = Some(v.to_string());
        *i += 2;
        return Ok(true);
    }

    if let Some(v) = a.strip_prefix("--abi=") {
        if v.trim().is_empty() {
            return Err(CliError::usage("missing value: --abi=<name>"));
        }
        llvm.abi = Some(v.to_string());
        *i += 1;
        return Ok(true);
    }
    if a == "--abi" {
        let v = args.get(*i + 1).ok_or_else(|| CliError::usage("missing value: --abi <name>"))?;
        if v.trim().is_empty() {
            return Err(CliError::usage("missing value: --abi <name>"));
        }
        llvm.abi = Some(v.to_string());
        *i += 2;
        return Ok(true);
    }

    if let Some(v) = a.strip_prefix("--sysroot=") {
        if v.trim().is_empty() {
            return Err(CliError::usage("missing value: --sysroot=<path>"));
        }
        llvm.sysroot = Some(v.to_string());
        llvm.sysroot_source = Some("explicit".to_string());
        *i += 1;
        return Ok(true);
    }
    if a == "--sysroot" {
        let v =
            args.get(*i + 1).ok_or_else(|| CliError::usage("missing value: --sysroot <path>"))?;
        if v.trim().is_empty() {
            return Err(CliError::usage("missing value: --sysroot <path>"));
        }
        llvm.sysroot = Some(v.to_string());
        llvm.sysroot_source = Some("explicit".to_string());
        *i += 2;
        return Ok(true);
    }

    if a == "-C" {
        let spec =
            args.get(*i + 1).ok_or_else(|| CliError::usage("missing value: -C <key>[=<value>]"))?;
        parse_llvm_codegen_spec(spec, llvm)?;
        *i += 2;
        return Ok(true);
    }

    if let Some(spec) = a.strip_prefix("-C") {
        if spec.is_empty() {
            return Err(CliError::usage("missing value: -C <key>[=<value>]"));
        }
        parse_llvm_codegen_spec(spec, llvm)?;
        *i += 1;
        return Ok(true);
    }

    Ok(false)
}

fn parse_llvm_codegen_spec(spec: &str, llvm: &mut LlvmFlags) -> Result<(), CliError> {
    let spec = spec.trim();
    if spec.is_empty() {
        return Err(CliError::usage("missing value: -C <key>[=<value>]"));
    }

    if spec == "no-default-libs" {
        llvm.no_default_libs = true;
        return Ok(());
    }

    let Some((key, value)) = spec.split_once('=') else {
        return Err(CliError::usage(format!(
            "invalid -C option '{}': expected key=value or no-default-libs",
            spec
        )));
    };

    let key = key.trim();
    let value = value.trim();
    if value.is_empty() {
        return Err(CliError::usage(format!("missing value for -C {}", key)));
    }

    match key {
        "linker" => llvm.linker = Some(value.to_string()),
        "link-arg" => llvm.link_args.push(value.to_string()),
        "link-sysroot" => set_link_sysroot_arg(&mut llvm.link_args, value),
        "code-model" => llvm.code_model = Some(value.to_string()),
        "relocation-model" => llvm.relocation_model = Some(value.to_string()),
        _ => {
            return Err(CliError::usage(format!(
                "unsupported -C option '{}': supported keys are linker, link-arg, link-sysroot, no-default-libs, code-model, relocation-model",
                key
            )));
        },
    }

    Ok(())
}

fn parse_command(rest: Vec<String>) -> Result<CliCommand, CliError> {
    if rest.is_empty() {
        return Err(CliError::usage("not enough arguments"));
    }

    let cmd = rest[0].as_str();
    let args = &rest[1..];

    match cmd {
        "--help" | "-h" | "help" => Ok(CliCommand::Help),
        "--version" | "-V" | "version" => Ok(CliCommand::Version),

        "build" => parse_build(args),
        "run" => parse_run_alias(args),
        "check" => parse_check_alias(args),
        "print" => parse_print(args),

        "install" => parse_install(args),
        "update" => parse_update(args),

        other => Err(CliError::usage(format!("unknown command: {}", other))),
    }
}

fn parse_run_alias(args: &[String]) -> Result<CliCommand, CliError> {
    let mut file: Option<PathBuf> = None;
    let mut run_args: Vec<String> = Vec::new();
    let mut after_double_dash = false;

    for a in args {
        if after_double_dash {
            run_args.push(a.clone());
            continue;
        }

        if a == "--" {
            after_double_dash = true;
            continue;
        }

        if a.starts_with('-') {
            return Err(CliError::usage(format!("unknown option for run: {}", a)));
        }

        if file.is_none() {
            file = Some(PathBuf::from(a));
        } else {
            return Err(CliError::usage(format!("unexpected extra argument: {}", a)));
        }
    }

    let file = file.ok_or_else(|| CliError::usage("usage: wavec run <file>"))?;

    let mut build = BuildRequest::default();
    build.inputs.push(file);
    build.run = true;
    build.run_args = run_args;

    Ok(CliCommand::Build(build))
}

fn parse_check_alias(args: &[String]) -> Result<CliCommand, CliError> {
    let mut file: Option<PathBuf> = None;

    for a in args {
        if a.starts_with('-') {
            return Err(CliError::usage(format!("unknown option for check: {}", a)));
        }

        if file.is_none() {
            file = Some(PathBuf::from(a));
        } else {
            return Err(CliError::usage(format!("unexpected extra argument: {}", a)));
        }
    }

    let file = file.ok_or_else(|| CliError::usage("usage: wavec check <file>"))?;

    let mut build = BuildRequest::default();
    build.inputs.push(file);
    build.emit = EmitSpec::Check;

    Ok(CliCommand::Build(build))
}

fn parse_build(args: &[String]) -> Result<CliCommand, CliError> {
    let mut build = BuildRequest::default();
    let mut emit_explicit = false;
    let mut compile_only = false;
    let mut after_double_dash = false;
    let mut i = 0usize;

    while i < args.len() {
        let a = &args[i];

        if after_double_dash {
            build.run_args.push(a.clone());
            i += 1;
            continue;
        }

        if a == "--ast-format" || a.starts_with("--ast-format=") {
            let value = if let Some(value) = a.strip_prefix("--ast-format=") {
                value
            } else {
                i += 1;
                args.get(i).ok_or_else(|| CliError::usage("missing --ast-format value"))?
            };
            build.ast_format = match value {
                "wson" => parser::ast_output::AstFormat::Wson,
                "json" => parser::ast_output::AstFormat::Json,
                "sexpr" => parser::ast_output::AstFormat::Sexpr,
                _ => return Err(CliError::usage("--ast-format must be wson, json, or sexpr")),
            };
            i += 1;
            continue;
        }
        match a.as_str() {
            "--" => {
                after_double_dash = true;
                i += 1;
            },
            "-c" => {
                compile_only = true;
                i += 1;
            },
            "-o" | "--output" => {
                let Some(v) = args.get(i + 1) else {
                    return Err(CliError::usage(format!("missing value: {} <file>", a)));
                };
                if v.trim().is_empty() {
                    return Err(CliError::usage(format!("invalid output file: {}", v)));
                }
                build.output = Some(PathBuf::from(v));
                i += 2;
            },
            _ if a.starts_with("--output=") => {
                let v = a.trim_start_matches("--output=");
                if v.trim().is_empty() {
                    return Err(CliError::usage("missing value: --output=<file>"));
                }
                build.output = Some(PathBuf::from(v));
                i += 1;
            },
            "--out-dir" => {
                let Some(v) = args.get(i + 1) else {
                    return Err(CliError::usage("missing value: --out-dir <dir>"));
                };
                if v.trim().is_empty() {
                    return Err(CliError::usage("missing value: --out-dir <dir>"));
                }
                build.out_dir = Some(PathBuf::from(v));
                i += 2;
            },
            _ if a.starts_with("--out-dir=") => {
                let v = a.trim_start_matches("--out-dir=");
                if v.trim().is_empty() {
                    return Err(CliError::usage("missing value: --out-dir=<dir>"));
                }
                build.out_dir = Some(PathBuf::from(v));
                i += 1;
            },
            "--target-dir" => {
                let Some(v) = args.get(i + 1) else {
                    return Err(CliError::usage("missing value: --target-dir <dir>"));
                };
                if v.trim().is_empty() {
                    return Err(CliError::usage("missing value: --target-dir <dir>"));
                }
                build.target_dir = Some(PathBuf::from(v));
                i += 2;
            },
            _ if a.starts_with("--target-dir=") => {
                let v = a.trim_start_matches("--target-dir=");
                if v.trim().is_empty() {
                    return Err(CliError::usage("missing value: --target-dir=<dir>"));
                }
                build.target_dir = Some(PathBuf::from(v));
                i += 1;
            },
            "--emit" => {
                let Some(v) = args.get(i + 1) else {
                    return Err(CliError::usage("missing value: --emit <kinds>"));
                };
                apply_emit_spec(&mut build, &mut emit_explicit, v)?;
                i += 2;
            },
            _ if a.starts_with("--emit=") => {
                let v = a.trim_start_matches("--emit=");
                apply_emit_spec(&mut build, &mut emit_explicit, v)?;
                i += 1;
            },
            "--input-type" => {
                let Some(v) = args.get(i + 1) else {
                    return Err(CliError::usage("missing value: --input-type <kind>"));
                };
                build.input_type = Some(parse_input_kind(v)?);
                i += 2;
            },
            _ if a.starts_with("--input-type=") => {
                let v = a.trim_start_matches("--input-type=");
                build.input_type = Some(parse_input_kind(v)?);
                i += 1;
            },
            "--link-only" => {
                build.link_only = true;
                i += 1;
            },
            "--run" => {
                build.run = true;
                i += 1;
            },
            "--dry-run" => {
                build.dry_run = true;
                i += 1;
            },
            "--freestanding" => {
                build.freestanding = true;
                i += 1;
            },
            "--entry" => {
                let Some(v) = args.get(i + 1) else {
                    return Err(CliError::usage("missing value: --entry <symbol>"));
                };
                if v.trim().is_empty() {
                    return Err(CliError::usage("missing value: --entry <symbol>"));
                }
                build.entry = Some(v.clone());
                i += 2;
            },
            _ if a.starts_with("--entry=") => {
                let v = a.trim_start_matches("--entry=");
                if v.trim().is_empty() {
                    return Err(CliError::usage("missing value: --entry=<symbol>"));
                }
                build.entry = Some(v.to_string());
                i += 1;
            },
            "--linker-script" => {
                let Some(v) = args.get(i + 1) else {
                    return Err(CliError::usage("missing value: --linker-script <path>"));
                };
                if v.trim().is_empty() {
                    return Err(CliError::usage("missing value: --linker-script <path>"));
                }
                build.linker_script = Some(PathBuf::from(v));
                i += 2;
            },
            _ if a.starts_with("--linker-script=") => {
                let v = a.trim_start_matches("--linker-script=");
                if v.trim().is_empty() {
                    return Err(CliError::usage("missing value: --linker-script=<path>"));
                }
                build.linker_script = Some(PathBuf::from(v));
                i += 1;
            },
            "--no-start-files" => {
                build.no_start_files = true;
                i += 1;
            },
            "--shared" => {
                build.shared = true;
                i += 1;
            },
            "--static" => {
                build.static_link = true;
                i += 1;
            },
            "--pie" => {
                if build.pie == Some(false) {
                    return Err(CliError::usage("cannot combine --pie and --no-pie"));
                }
                build.pie = Some(true);
                i += 1;
            },
            "--no-pie" => {
                if build.pie == Some(true) {
                    return Err(CliError::usage("cannot combine --pie and --no-pie"));
                }
                build.pie = Some(false);
                i += 1;
            },
            "--error-format" => {
                let Some(v) = args.get(i + 1) else {
                    return Err(CliError::usage("missing value: --error-format <human,json>"));
                };
                build.error_format = parse_error_format(v)?;
                i += 2;
            },
            _ if a.starts_with("--error-format=") => {
                let v = a.trim_start_matches("--error-format=");
                build.error_format = parse_error_format(v)?;
                i += 1;
            },
            _ if a.starts_with('-') => {
                return Err(CliError::usage(format!("unknown option for build: {}", a)));
            },
            _ => {
                build.inputs.push(PathBuf::from(a));
                i += 1;
            },
        }
    }

    if build.inputs.is_empty() {
        return Err(CliError::usage("usage: wavec build <input...> [options]"));
    }

    if !build.run_args.is_empty() && !build.run {
        return Err(CliError::usage(
            "run arguments after `--` require --run (or use `wavec run <file> -- <args...>`)",
        ));
    }

    if compile_only {
        match &build.emit {
            EmitSpec::Check => {
                return Err(CliError::usage("-c cannot be combined with --emit=check"));
            },
            EmitSpec::Set(set) => {
                if emit_explicit {
                    if !(set.len() == 1 && set.contains(&EmitKind::Obj)) {
                        return Err(CliError::usage(
                            "-c is equivalent to --emit=obj and cannot be combined with other emit kinds",
                        ));
                    }
                } else {
                    let mut obj_only = BTreeSet::new();
                    obj_only.insert(EmitKind::Obj);
                    build.emit = EmitSpec::Set(obj_only);
                }
            },
        }
    }

    Ok(CliCommand::Build(build))
}

fn parse_print(args: &[String]) -> Result<CliCommand, CliError> {
    let item = args
        .first()
        .ok_or_else(|| {
            CliError::usage("usage: wavec print <item> [--target <triple>] [--format human|json]")
        })?
        .clone();

    let mut target: Option<String> = None;
    let mut format = PrintFormat::Human;
    let mut i = 1usize;
    while i < args.len() {
        let a = &args[i];
        if a == "--target" {
            let v = args
                .get(i + 1)
                .ok_or_else(|| CliError::usage("missing value: --target <triple>"))?;
            if v.trim().is_empty() {
                return Err(CliError::usage("missing value: --target <triple>"));
            }
            target = Some(v.clone());
            i += 2;
            continue;
        }
        if let Some(v) = a.strip_prefix("--target=") {
            if v.trim().is_empty() {
                return Err(CliError::usage("missing value: --target=<triple>"));
            }
            target = Some(v.to_string());
            i += 1;
            continue;
        }
        if a == "--format" {
            let v = args
                .get(i + 1)
                .ok_or_else(|| CliError::usage("missing value: --format <human,json>"))?;
            format = parse_print_format(v)?;
            i += 2;
            continue;
        }
        if let Some(v) = a.strip_prefix("--format=") {
            format = parse_print_format(v)?;
            i += 1;
            continue;
        }
        if a == "--json" {
            format = PrintFormat::Json;
            i += 1;
            continue;
        }

        return Err(CliError::usage(format!("unknown option for print: {}", a)));
    }

    Ok(CliCommand::Print { item, target, format })
}

fn parse_std_reference(args: &[String]) -> Result<Option<String>, CliError> {
    if args.first().map(String::as_str) != Some("std") {
        return Err(CliError::usage("usage: wavec install|update std [--ref <commit-or-ref>]"));
    }
    let reference = match &args[1..] {
        [] => return Ok(None),
        [flag, value] if flag == "--ref" => value.as_str(),
        [value] if value.starts_with("--ref=") => &value[6..],
        _ => {
            return Err(CliError::usage("usage: wavec install|update std [--ref <commit-or-ref>]"))
        },
    };
    if reference.is_empty()
        || reference.starts_with('-')
        || reference.contains(':')
        || reference.chars().any(char::is_whitespace)
    {
        return Err(CliError::usage("invalid std reference: expected a Git commit, branch or tag"));
    }
    Ok(Some(reference.to_owned()))
}

fn parse_install(args: &[String]) -> Result<CliCommand, CliError> {
    Ok(CliCommand::StdInstall(parse_std_reference(args)?))
}

fn parse_update(args: &[String]) -> Result<CliCommand, CliError> {
    Ok(CliCommand::StdUpdate(parse_std_reference(args)?))
}

fn parse_input_kind(v: &str) -> Result<InputKind, CliError> {
    match v.trim().to_ascii_lowercase().as_str() {
        "wave" => Ok(InputKind::Wave),
        "ir" => Ok(InputKind::Ir),
        "bc" => Ok(InputKind::Bc),
        "asm" => Ok(InputKind::Asm),
        "obj" => Ok(InputKind::Obj),
        "archive" | "lib" => Ok(InputKind::Archive),
        _ => Err(CliError::usage(format!(
            "invalid --input-type '{}': expected wave, ir, bc, asm, obj, archive",
            v
        ))),
    }
}

fn parse_error_format(v: &str) -> Result<ErrorFormat, CliError> {
    match v.trim() {
        "human" => Ok(ErrorFormat::Human),
        "json" => Ok(ErrorFormat::Json),
        _ => Err(CliError::usage(format!("invalid --error-format '{}': expected human, json", v))),
    }
}

fn parse_print_format(v: &str) -> Result<PrintFormat, CliError> {
    match v.trim() {
        "human" => Ok(PrintFormat::Human),
        "json" => Ok(PrintFormat::Json),
        _ => Err(CliError::usage(format!("invalid --format '{}': expected human, json", v))),
    }
}

fn parse_emit_kind(item: &str) -> Result<EmitKind, CliError> {
    match item.trim() {
        "ast" => Ok(EmitKind::Ast),
        "ir" => Ok(EmitKind::Ir),
        "bc" => Ok(EmitKind::Bc),
        "asm" => Ok(EmitKind::Asm),
        "obj" => Ok(EmitKind::Obj),
        "bin" => Ok(EmitKind::Bin),
        _ => Err(CliError::usage(format!(
            "unknown --emit kind '{}': expected check, ast, ir, bc, asm, obj, bin",
            item
        ))),
    }
}

fn apply_emit_spec(
    build: &mut BuildRequest,
    emit_explicit: &mut bool,
    spec: &str,
) -> Result<(), CliError> {
    if spec.trim().is_empty() {
        return Err(CliError::usage("missing value: --emit=<kinds>"));
    }

    if !*emit_explicit {
        build.emit = EmitSpec::Set(BTreeSet::new());
        *emit_explicit = true;
    }

    let mut saw_check = false;
    let mut set = BTreeSet::new();

    for raw in spec.split(',') {
        let item = raw.trim();
        if item.is_empty() {
            continue;
        }

        if item == "check" {
            saw_check = true;
        } else {
            set.insert(parse_emit_kind(item)?);
        }
    }

    if saw_check && !set.is_empty() {
        return Err(CliError::usage("--emit=check must be used alone (check is a control mode)"));
    }

    if saw_check {
        match build.emit {
            EmitSpec::Check => return Ok(()),
            EmitSpec::Set(ref existing) if existing.is_empty() => {
                build.emit = EmitSpec::Check;
                return Ok(());
            },
            EmitSpec::Set(_) => {
                return Err(CliError::usage(
                    "--emit=check cannot be combined with other emit kinds",
                ));
            },
        }
    }

    if set.is_empty() {
        return Err(CliError::usage("--emit requires at least one emit kind"));
    }

    match &mut build.emit {
        EmitSpec::Check => {
            Err(CliError::usage("--emit=check cannot be combined with other emit kinds"))
        },
        EmitSpec::Set(existing) => {
            existing.extend(set);
            Ok(())
        },
    }
}

fn classify_inputs(build: &BuildRequest) -> Result<Vec<ClassifiedInput>, CliError> {
    let mut out = Vec::with_capacity(build.inputs.len());
    for input in &build.inputs {
        let kind = resolve_input_kind(input, build.input_type)?;
        out.push(ClassifiedInput { path: input.clone(), kind });
    }
    Ok(out)
}

fn resolve_input_kind(path: &Path, forced: Option<InputKind>) -> Result<InputKind, CliError> {
    let inferred = infer_input_kind(path);

    if let Some(forced_kind) = forced {
        if let Some(inferred_kind) = inferred {
            if inferred_kind != forced_kind {
                return Err(CliError::usage(format!(
                    "--input-type={} conflicts with input '{}'(inferred {})",
                    forced_kind.as_str(),
                    path.display(),
                    inferred_kind.as_str()
                )));
            }
        }
        return Ok(forced_kind);
    }

    inferred.ok_or_else(|| {
        CliError::usage(format!(
            "cannot infer input type for '{}': use --input-type=<wave,ir,bc,asm,obj,archive>",
            path.display()
        ))
    })
}

fn infer_input_kind(path: &Path) -> Option<InputKind> {
    let ext = path.extension()?.to_str()?.to_ascii_lowercase();
    match ext.as_str() {
        "wave" => Some(InputKind::Wave),
        "ll" | "ir" => Some(InputKind::Ir),
        "bc" => Some(InputKind::Bc),
        "s" | "asm" => Some(InputKind::Asm),
        "o" | "obj" => Some(InputKind::Obj),
        "a" | "lib" => Some(InputKind::Archive),
        _ => None,
    }
}

fn validate_build_request(
    global: &Global,
    build: &BuildRequest,
    classified: &[ClassifiedInput],
) -> Result<(), CliError> {
    let target = target_triple_for_global(global);
    if is_wasm_target(&target) {
        if build.run
            && !build.run_args.is_empty()
            && matches!(
                target_spec_for_triple(&target).map(|spec| spec.codegen),
                Some(CodegenTarget::Wasm32Unknown | CodegenTarget::Wasm64Unknown)
            )
        {
            return Err(CliError::usage(
                "run-time arguments require wasm32-wasip1; bare WebAssembly targets have no process argument ABI",
            ));
        }
        if build.shared || build.static_link || build.pie.is_some() || build.linker_script.is_some()
        {
            return Err(CliError::usage(
                "--shared, --static, --pie/--no-pie, and --linker-script are not supported for WebAssembly targets",
            ));
        }
    }

    if build.shared && build.static_link {
        return Err(CliError::usage("cannot combine --shared and --static"));
    }
    if build.shared && build.pie.is_some() {
        return Err(CliError::usage("cannot combine --shared with --pie/--no-pie in v1"));
    }

    if let Some(reloc) = global.llvm.relocation_model.as_deref() {
        if build.pie == Some(true) && reloc != "pie" {
            return Err(CliError::usage(
                "--pie requires -C relocation-model=pie when relocation-model is set",
            ));
        }
        if build.pie == Some(false) && reloc == "pie" {
            return Err(CliError::usage(
                "--no-pie cannot be combined with -C relocation-model=pie",
            ));
        }
        if build.shared && reloc != "pic" && reloc != "dynamic-no-pic" {
            return Err(CliError::usage(
                "--shared requires -C relocation-model=pic or dynamic-no-pic",
            ));
        }
    }

    if build.emit.is_check() {
        if build.link_only {
            return Err(CliError::usage("--emit=check cannot be combined with --link-only"));
        }
        if build.run {
            return Err(CliError::usage("--emit=check cannot be combined with --run"));
        }
        if build.output.is_some() || build.out_dir.is_some() {
            return Err(CliError::usage(
                "--emit=check does not produce artifacts; remove -o/--out-dir",
            ));
        }
        if classified.iter().any(|i| i.kind != InputKind::Wave) {
            return Err(CliError::usage("--emit=check currently supports only Wave source inputs"));
        }
        return Ok(());
    }

    let emit_set = build.emit.as_set().expect("non-check emit set expected");

    for kind in [EmitKind::Ast, EmitKind::Ir, EmitKind::Bc, EmitKind::Asm] {
        if emit_set.contains(&kind)
            && !classified.iter().any(|input| supports_emit_for_input(kind, input.kind))
        {
            return Err(CliError::usage(format!(
                "--emit={} has no compatible inputs in this build request",
                emit_kind_name(kind)
            )));
        }
    }

    if build.link_only {
        if !(emit_set.len() == 1 && emit_set.contains(&EmitKind::Bin)) {
            return Err(CliError::usage("--link-only supports only --emit=bin in v1"));
        }
        if classified.iter().any(|i| !i.kind.is_link_input()) {
            return Err(CliError::usage(
                "--link-only requires link-ready inputs only (.o/.obj/.a)",
            ));
        }
    }

    if emit_set.len() == 1
        && emit_set.contains(&EmitKind::Obj)
        && classified.iter().all(|i| i.kind.is_link_input())
    {
        return Err(CliError::usage(
            "--emit=obj requires at least one compilable input (wave, ir, bc, asm)",
        ));
    }

    if build.run {
        if !emit_set.contains(&EmitKind::Bin) {
            return Err(CliError::usage("--run requires a binary output (emit includes bin)"));
        }
        if build.shared {
            return Err(CliError::usage("--run is not allowed when --shared is specified"));
        }
    }

    let need_link = emit_set.contains(&EmitKind::Bin) || build.run;
    if need_link && llvm::backend::is_windows_msvc_target(&target_triple_for_global(global)) {
        if build.linker_script.is_some() {
            return Err(CliError::usage(
                "MSVC linking does not support GNU linker scripts; use COFF linker options",
            ));
        }
        if build.no_start_files && build.entry.is_none() {
            return Err(CliError::usage(
                "MSVC --no-start-files requires --entry and -Cno-default-libs",
            ));
        }
        if build.no_start_files && !global.llvm.no_default_libs {
            return Err(CliError::usage("MSVC --no-start-files requires -Cno-default-libs"));
        }
    }

    if (build.entry.is_some() || build.linker_script.is_some() || build.no_start_files)
        && !need_link
    {
        return Err(CliError::usage(
            "--entry/--linker-script/--no-start-files require a link stage (emit includes bin)",
        ));
    }

    if need_link
        && global.llvm.linker.is_some()
        && global.llvm.sysroot.is_some()
        && !has_explicit_link_sysroot_arg(&global.llvm.link_args)
    {
        return Err(CliError::usage(
            "when using -C linker=..., --sysroot=<path> is compile-stage only; \
             pass linker sysroot explicitly with -C link-sysroot=<path> \
             (or -C link-arg=--sysroot=<path>)",
        ));
    }

    if build.output.is_some() {
        let compile_count = classified.iter().filter(|i| !i.kind.is_link_input()).count();
        let has_bin = emit_set.contains(&EmitKind::Bin) || build.run;

        if !has_bin {
            let obj_only = emit_set.len() == 1 && emit_set.contains(&EmitKind::Obj);
            if !(obj_only && compile_count == 1) {
                return Err(CliError::usage(
                    "-o is only allowed for final binary output, or single-input --emit=obj",
                ));
            }
        }
    }

    Ok(())
}

fn validate_output_paths(
    global: &Global,
    build: &BuildRequest,
    inputs: &[ClassifiedInput],
    plan: &BuildPlan,
) -> Result<(), CliError> {
    let Some(emits) = build.emit.as_set() else {
        return Ok(());
    };
    let mut outputs = Vec::new();
    for job in &plan.compile_jobs {
        outputs.push((job.output.clone(), None));
    }
    if let Some(output) = &plan.link_output {
        outputs.push((output.clone(), None));
    }
    for (index, input) in inputs.iter().enumerate() {
        for kind in [EmitKind::Ast, EmitKind::Ir, EmitKind::Bc, EmitKind::Asm] {
            if emits.contains(&kind) && supports_emit_for_input(kind, input.kind) {
                let passthrough = matches!(
                    (kind, input.kind),
                    (EmitKind::Ir, InputKind::Ir)
                        | (EmitKind::Bc, InputKind::Bc)
                        | (EmitKind::Asm, InputKind::Asm)
                );
                outputs.push((
                    resolve_extra_emit_output_path(build, input, kind, index, inputs.len()),
                    passthrough.then_some(input.path.as_path()),
                ));
            }
        }
    }
    let mut sources: Vec<PathBuf> = inputs.iter().map(|input| input.path.clone()).collect();
    crate::output_guard::validate(&sources, &outputs)?;
    if emits.len() == 1 && emits.contains(&EmitKind::Ast) {
        return Ok(());
    }
    // Imported source files are compiler inputs too. Resolve them before any
    // emit or object job writes an artifact, including the final link output.
    if !build.dry_run {
        for input in inputs.iter().filter(|input| input.kind == InputKind::Wave) {
            sources.extend(runner::wave_input_paths(&input.path, &global.dep, &global.llvm));
        }
        crate::output_guard::validate(&sources, &outputs)?;
    }
    Ok(())
}

fn is_link_sysroot_arg(arg: &str) -> bool {
    arg == "--sysroot" || arg.starts_with("--sysroot=") || arg.contains("--sysroot=")
}

fn has_explicit_link_sysroot_arg(args: &[String]) -> bool {
    args.iter().any(|arg| is_link_sysroot_arg(arg))
}

fn set_link_sysroot_arg(link_args: &mut Vec<String>, value: &str) {
    link_args.retain(|arg| !is_link_sysroot_arg(arg));
    link_args.push(format!("--sysroot={}", value));
}

fn create_build_plan(
    global: &Global,
    build: &BuildRequest,
    classified: &[ClassifiedInput],
) -> Result<BuildPlan, CliError> {
    if build.emit.is_check() {
        return Ok(BuildPlan::default());
    }

    let emit_set = build.emit.as_set().expect("non-check emit set expected");
    let need_objects =
        emit_set.contains(&EmitKind::Obj) || emit_set.contains(&EmitKind::Bin) || build.run;
    let need_link = emit_set.contains(&EmitKind::Bin) || build.run;

    if !need_objects && !need_link {
        return Ok(BuildPlan::default());
    }

    let compile_total = classified.iter().filter(|i| !i.kind.is_link_input()).count();
    let mut compile_index = 0usize;

    let mut plan = BuildPlan::default();

    for input in classified {
        if input.kind.is_link_input() {
            plan.link_inputs.push(input.path.to_string_lossy().to_string());
            continue;
        }

        if !need_objects {
            continue;
        }

        let output = resolve_object_output_path(
            build,
            input,
            compile_index,
            compile_total,
            emit_set.contains(&EmitKind::Obj),
            need_link,
        );

        plan.link_inputs.push(output.to_string_lossy().to_string());
        plan.compile_jobs.push(CompileJob { input: input.path.clone(), kind: input.kind, output });
        compile_index += 1;
    }

    if need_link {
        let primary = classified
            .first()
            .ok_or_else(|| CliError::usage("build requires at least one input"))?;
        plan.link_output = Some(resolve_binary_output_path(global, build, primary));
    }

    Ok(plan)
}

fn resolve_object_output_path(
    build: &BuildRequest,
    input: &ClassifiedInput,
    compile_index: usize,
    compile_total: usize,
    emit_obj: bool,
    need_link: bool,
) -> PathBuf {
    if emit_obj && !need_link && compile_total == 1 {
        if let Some(path) = &build.output {
            return path.clone();
        }
    }

    let file_name = object_file_name(&input.path, compile_index, compile_total);

    if emit_obj {
        if let Some(out_dir) = &build.out_dir {
            return out_dir.join(&file_name);
        }
        if let Some(target_dir) = &build.target_dir {
            return target_dir.join(&file_name);
        }
        return PathBuf::from(file_name);
    }

    if let Some(target_dir) = &build.target_dir {
        return target_dir.join(file_name);
    }

    PathBuf::from("target").join(file_name)
}

fn resolve_binary_output_path(
    global: &Global,
    build: &BuildRequest,
    primary: &ClassifiedInput,
) -> PathBuf {
    if let Some(path) = &build.output {
        return path.clone();
    }

    let stem = primary
        .path
        .file_stem()
        .and_then(|s| s.to_str())
        .filter(|s| !s.is_empty())
        .unwrap_or("a.out");
    let stem = if llvm::backend::is_windows_msvc_target(&target_triple_for_global(global)) {
        format!("{}.{}", stem, if build.shared { "dll" } else { "exe" })
    } else if global.llvm.target.as_deref().is_some_and(is_wasm_target) {
        format!("{}.wasm", stem)
    } else {
        stem.to_string()
    };

    if let Some(out_dir) = &build.out_dir {
        return out_dir.join(&stem);
    }

    if let Some(target_dir) = &build.target_dir {
        return target_dir.join(&stem);
    }

    PathBuf::from("target").join(&stem)
}

fn object_file_name(path: &Path, compile_index: usize, compile_total: usize) -> String {
    let stem =
        path.file_stem().and_then(|s| s.to_str()).filter(|s| !s.is_empty()).unwrap_or("input");

    if compile_total > 1 {
        format!("{}_{}.o", stem, compile_index + 1)
    } else {
        format!("{}.o", stem)
    }
}

fn emit_kind_name(kind: EmitKind) -> &'static str {
    match kind {
        EmitKind::Ast => "ast",
        EmitKind::Ir => "ir",
        EmitKind::Bc => "bc",
        EmitKind::Asm => "asm",
        EmitKind::Obj => "obj",
        EmitKind::Bin => "bin",
    }
}

fn supports_emit_for_input(kind: EmitKind, input: InputKind) -> bool {
    match kind {
        EmitKind::Ast => input == InputKind::Wave,
        EmitKind::Ir => input == InputKind::Wave || input == InputKind::Ir,
        EmitKind::Bc => matches!(input, InputKind::Wave | InputKind::Ir | InputKind::Bc),
        EmitKind::Asm => {
            matches!(input, InputKind::Wave | InputKind::Ir | InputKind::Bc | InputKind::Asm)
        },
        EmitKind::Obj => matches!(
            input,
            InputKind::Wave | InputKind::Ir | InputKind::Bc | InputKind::Asm | InputKind::Obj
        ),
        EmitKind::Bin => matches!(
            input,
            InputKind::Wave
                | InputKind::Ir
                | InputKind::Bc
                | InputKind::Asm
                | InputKind::Obj
                | InputKind::Archive
        ),
    }
}

fn emit_artifact_extension(kind: EmitKind) -> &'static str {
    match kind {
        EmitKind::Ast => "ast",
        EmitKind::Ir => "ll",
        EmitKind::Bc => "bc",
        EmitKind::Asm => "s",
        EmitKind::Obj => "o",
        EmitKind::Bin => "",
    }
}

fn emit_artifact_file_name(
    path: &Path,
    input_index: usize,
    input_total: usize,
    kind: EmitKind,
) -> String {
    let stem =
        path.file_stem().and_then(|s| s.to_str()).filter(|s| !s.is_empty()).unwrap_or("input");

    let base =
        if input_total > 1 { format!("{}_{}", stem, input_index + 1) } else { stem.to_string() };

    let ext = emit_artifact_extension(kind);
    if ext.is_empty() {
        base
    } else {
        format!("{}.{}", base, ext)
    }
}

fn resolve_extra_emit_output_path(
    build: &BuildRequest,
    input: &ClassifiedInput,
    kind: EmitKind,
    input_index: usize,
    input_total: usize,
) -> PathBuf {
    let mut file_name = emit_artifact_file_name(&input.path, input_index, input_total, kind);
    if kind == EmitKind::Ast {
        file_name =
            format!("{}.{}", file_name.strip_suffix(".ast").unwrap(), build.ast_format.extension());
    }
    if let Some(out_dir) = &build.out_dir {
        return out_dir.join(&file_name);
    }
    if let Some(target_dir) = &build.target_dir {
        return target_dir.join(&file_name);
    }
    PathBuf::from(file_name)
}

fn copy_if_different(src: &Path, dst: &Path) -> Result<(), CliError> {
    if crate::output_guard::same_file(src, dst)? {
        return Ok(());
    }
    ensure_parent_dir(dst)?;
    fs::copy(src, dst)?;
    Ok(())
}

fn execute_explicit_emit_artifacts(
    global: &Global,
    build: &BuildRequest,
    classified: &[ClassifiedInput],
    emit_set: &BTreeSet<EmitKind>,
) -> Result<(), CliError> {
    let kinds = [EmitKind::Ast, EmitKind::Ir, EmitKind::Bc, EmitKind::Asm];
    let total_inputs = classified.len();

    for (input_index, input) in classified.iter().enumerate() {
        for kind in kinds {
            if !emit_set.contains(&kind) || !supports_emit_for_input(kind, input.kind) {
                continue;
            }

            let output =
                resolve_extra_emit_output_path(build, input, kind, input_index, total_inputs);
            ensure_parent_dir(&output)?;

            match kind {
                EmitKind::Ast => {
                    let text = unsafe {
                        runner::emit_wave_ast_text(
                            &input.path,
                            &global.debug,
                            &global.dep,
                            &global.llvm,
                            build.ast_format,
                        )
                    };
                    fs::write(output, text.map_err(|e| CliError::usage(e.to_string()))?)?;
                },
                EmitKind::Ir => match input.kind {
                    InputKind::Wave => {
                        let text = unsafe {
                            runner::emit_wave_ir_text(
                                &input.path,
                                &global.opt,
                                &global.debug,
                                &global.dep,
                                &global.llvm,
                            )
                        };
                        fs::write(output, text)?;
                    },
                    InputKind::Ir => copy_if_different(&input.path, &output)?,
                    _ => {},
                },
                EmitKind::Bc => match input.kind {
                    InputKind::Wave => unsafe {
                        runner::emit_wave_bitcode_file(
                            &input.path,
                            &global.opt,
                            &global.debug,
                            &global.dep,
                            &global.llvm,
                            &output,
                        );
                    },
                    InputKind::Ir => {
                        compile_lowering_with_llvm_tools(
                            global,
                            &input.path,
                            InputKind::Ir,
                            &output,
                            EmitKind::Bc,
                        )?;
                    },
                    InputKind::Bc => copy_if_different(&input.path, &output)?,
                    _ => {},
                },
                EmitKind::Asm => match input.kind {
                    InputKind::Wave => unsafe {
                        runner::emit_wave_assembly_file(
                            &input.path,
                            &global.opt,
                            &global.debug,
                            &global.dep,
                            &global.llvm,
                            &output,
                        );
                    },
                    InputKind::Ir | InputKind::Bc => {
                        compile_lowering_with_llvm_tools(
                            global,
                            &input.path,
                            input.kind,
                            &output,
                            EmitKind::Asm,
                        )?;
                    },
                    InputKind::Asm => copy_if_different(&input.path, &output)?,
                    _ => {},
                },
                _ => {},
            }
        }
    }

    Ok(())
}

fn compile_non_wave_to_object(global: &Global, job: &CompileJob) -> Result<(), CliError> {
    ensure_parent_dir(&job.output)?;
    compile_lowering_with_llvm_tools(global, &job.input, job.kind, &job.output, EmitKind::Obj)
}

fn compile_lowering_with_llvm_tools(
    global: &Global,
    input: &Path,
    input_kind: InputKind,
    output: &Path,
    emit_kind: EmitKind,
) -> Result<(), CliError> {
    let pending = PendingOutput::new(output)?;
    let (bin, args) =
        build_llvm_lowering_args(global, input, input_kind, pending.path(), emit_kind);
    let mut command = ProcessCommand::new(&bin);
    configure_bundled_llvm_tool_env(&mut command, &bin);

    let process_output = command
        .args(&args)
        .output()
        .map_err(|error| CodegenError::tool_launch(CodegenPhase::Tool, &bin, error))?;

    if process_output.status.success() {
        return pending.commit().map_err(CliError::from);
    }

    let stderr = String::from_utf8_lossy(&process_output.stderr).trim().to_string();
    let stdout = String::from_utf8_lossy(&process_output.stdout).trim().to_string();

    Err(CodegenError::new(
        CodegenPhase::Tool,
        emit_kind_name(emit_kind),
        format!(
            "{} failed (status={})\nstdout: {}\nstderr: {}",
            emit_kind_name(emit_kind),
            process_output.status,
            stdout,
            stderr
        ),
    )
    .into())
}

fn build_llvm_lowering_args(
    global: &Global,
    input: &Path,
    input_kind: InputKind,
    output: &Path,
    emit_kind: EmitKind,
) -> (String, Vec<String>) {
    match (input_kind, emit_kind) {
        (InputKind::Ir, EmitKind::Bc) => {
            let args = vec![
                input.to_string_lossy().to_string(),
                "-o".to_string(),
                output.to_string_lossy().to_string(),
            ];
            (resolve_bundled_tool("llvm-as"), args)
        },
        (InputKind::Ir | InputKind::Bc, EmitKind::Obj | EmitKind::Asm) => {
            build_llc_lowering_args(global, input, output, emit_kind)
        },
        (InputKind::Asm, EmitKind::Obj) => build_llvm_mc_lowering_args(global, input, output),
        _ => (resolve_bundled_tool("llvm-as"), vec!["--version".to_string()]),
    }
}

fn build_llc_lowering_args(
    global: &Global,
    input: &Path,
    output: &Path,
    emit_kind: EmitKind,
) -> (String, Vec<String>) {
    let mut args = Vec::new();

    args.push(format!(
        "--filetype={}",
        match emit_kind {
            EmitKind::Asm => "asm",
            _ => "obj",
        }
    ));

    // IR and bitcode carry their own target contract. Only override it when
    // the user explicitly supplied --target; the implicit host target must
    // not silently rewrite a cross-target artifact.
    if global.llvm.target_explicit {
        if let Some(target) = &global.llvm.target {
            args.push(format!("--mtriple={}", target));
        }
        if let Some(cpu) = &global.llvm.cpu {
            args.push(format!("--mcpu={}", cpu));
        }
        if let Some(features) = &global.llvm.features {
            args.push(format!("--mattr={}", features));
        }
        if let Some(abi) = &global.llvm.abi {
            args.push(format!("--target-abi={}", abi));
        }
    }
    if let Some(model) = &global.llvm.code_model {
        args.push(format!("--code-model={}", model));
    }
    if let Some(model) = &global.llvm.relocation_model {
        args.push(format!("--relocation-model={}", model));
    }
    if !global.opt.is_empty() {
        args.push(normalize_opt_for_llvm_tool(&global.opt).to_string());
    }

    args.push(input.to_string_lossy().to_string());
    args.push("-o".to_string());
    args.push(output.to_string_lossy().to_string());

    (resolve_bundled_tool("llc"), args)
}

fn build_llvm_mc_lowering_args(
    global: &Global,
    input: &Path,
    output: &Path,
) -> (String, Vec<String>) {
    let mut args = Vec::new();

    if let Some(target) = &global.llvm.target {
        let llvm_target =
            llvm::codegen::target::llvm_triple_for_abi(target, global.llvm.abi.as_deref());
        args.push(format!("--triple={}", llvm_target));
    }
    if let Some(cpu) = &global.llvm.cpu {
        args.push(format!("--mcpu={}", cpu));
    }
    if let Some(features) = &global.llvm.features {
        args.push(format!("--mattr={}", features));
    }
    args.push("--filetype=obj".to_string());
    args.push(input.to_string_lossy().to_string());
    args.push("-o".to_string());
    args.push(output.to_string_lossy().to_string());

    (resolve_bundled_tool("llvm-mc"), args)
}

fn link_objects(
    global: &Global,
    build: &BuildRequest,
    objects: &[String],
    output: &Path,
) -> Result<(), CliError> {
    ensure_parent_dir(output)?;

    let target = target_triple_for_global(global);

    if matches!(
        target_spec_for_triple(&target).map(|spec| spec.codegen),
        Some(CodegenTarget::LinuxLoongArch64)
    ) && global.llvm.abi.as_deref() == Some("lp64f")
        && !global.llvm.no_default_libs
    {
        return Err(CliError::CommandFailed(
            "hosted Linux LoongArch64 LP64F linking is unavailable because glibc does not provide an LP64F runtime; use --emit=obj for LP64F toolchain work, select --abi=lp64s/lp64d, or provide a freestanding runtime with -Cno-default-libs"
                .to_string(),
        ));
    }

    if global.llvm.linker.is_none() {
        validate_default_elf_runtime(global, build)?;
    }

    let (_, args) = build_linker_args(global, build, objects, output);
    if matches!(
        target_spec_for_triple(&target).map(|spec| spec.codegen),
        Some(CodegenTarget::LinuxRISCV64 | CodegenTarget::FreestandingRISCV64)
    ) {
        let abi = global.llvm.abi.as_deref().unwrap_or("lp64d");
        let target_abi = RiscvFloatAbi::from_target_abi(abi).ok_or_else(|| {
            CliError::CommandFailed(format!("unsupported RISC-V target ABI '{}'", abi))
        })?;
        let validation_inputs = collect_linker_input_paths(objects, &args, build.static_link);
        validate_riscv_link_inputs(target_abi, &validation_inputs)
            .map_err(|error| CliError::CommandFailed(error.to_string()))?;
    }
    if matches!(
        target_spec_for_triple(&target).map(|spec| spec.codegen),
        Some(CodegenTarget::LinuxLoongArch64)
    ) {
        let target_abi = global.llvm.abi.as_deref().unwrap_or("lp64d");
        let target_abi = LoongArchFloatAbi::from_target_abi(target_abi)
            .expect("LoongArch ABI is validated before linking");
        let validation_inputs = collect_linker_input_paths(objects, &args, build.static_link);
        validate_loongarch64_link_inputs(target_abi, &validation_inputs)
            .map_err(|error| CliError::CommandFailed(error.to_string()))?;
    }
    enum LinkOutput {
        Single(PendingOutput),
        Msvc(crate::link_outputs::MsvcOutputs),
    }
    let (pending, bin, args) = if llvm::backend::is_windows_msvc_target(&target) {
        let (bin, mut args) = build_linker_args(global, build, objects, output);
        llvm::msvc::sdk::validate_link_inputs(&target, objects, &args)
            .map_err(CliError::CommandFailed)?;
        let pending = crate::link_outputs::MsvcOutputs::prepare(output, &mut args)?;
        (LinkOutput::Msvc(pending), bin, args)
    } else {
        let pending = PendingOutput::new(output)?;
        let (bin, args) = build_linker_args(global, build, objects, pending.path());
        (LinkOutput::Single(pending), bin, args)
    };
    let mut command = ProcessCommand::new(&bin);
    configure_bundled_llvm_tool_env(&mut command, &bin);

    let transport = if llvm::backend::is_windows_msvc_target(&target) {
        Some(llvm::msvc::response::LinkArguments::prepare(&bin, &args, output)?)
    } else {
        None
    };
    let args = transport.as_ref().map_or(&args, |transport| &transport.arguments);
    let out = command.args(args).output().map_err(|error| {
        CodegenError::tool_launch(
            CodegenPhase::Link,
            &missing_linker_tool_name(global, &bin),
            error,
        )
    })?;

    if out.status.success() {
        return match pending {
            LinkOutput::Single(output) => output.commit().map_err(CliError::from),
            LinkOutput::Msvc(outputs) => outputs.commit().map_err(CliError::from),
        };
    }

    let stderr = String::from_utf8_lossy(&out.stderr).trim().to_string();
    let stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();

    let hint = if llvm::backend::is_windows_msvc_target(&target) {
        "\nMSVC output requires matching Windows SDK (UM/UCRT) and VC runtime libraries; run a Developer Command Prompt for the target architecture or supply their directories with -L."
    } else {
        ""
    };
    Err(CodegenError::new(
        CodegenPhase::Link,
        "link objects",
        format!(
            "link failed (status={})\nstdout: {}\nstderr: {}{}",
            out.status, stdout, stderr, hint
        ),
    )
    .into())
}

fn validate_default_elf_runtime(global: &Global, build: &BuildRequest) -> Result<(), CliError> {
    let target = target_triple_for_global(global);
    if !is_hosted_elf_target(&target)
        || target == host_target_triple()
        || global.llvm.no_default_libs
    {
        return Ok(());
    }

    let mut missing = Vec::new();
    if !build.shared && !build.no_start_files {
        let start_name = elf_start_file_name(build);
        let has_crt = if is_linux_target(&target) {
            [start_name, "crti.o", "crtn.o"].into_iter().all(|name| {
                llvm::toolchain::find_bundled_linux_crt(&target, global.llvm.abi.as_deref(), name)
                    .is_some()
            })
        } else {
            [start_name, "crti.o", "crtn.o"]
                .into_iter()
                .all(|name| find_elf_runtime_file(&target, global, name).is_some())
        };
        if !has_crt {
            missing.push(format!("target CRT ({}, crti.o, crtn.o)", start_name));
        }
    }
    let libc_names: &[&str] =
        if build.static_link { &["libc.a"] } else { &["libc.so", "libc.a", "libc.so.6"] };
    let libm_names: &[&str] =
        if build.static_link { &["libm.a"] } else { &["libm.so", "libm.a", "libm.so.6"] };
    if find_elf_runtime_file_any(&target, global, libc_names).is_none() {
        missing.push("libc".to_string());
    }
    if find_elf_runtime_file_any(&target, global, libm_names).is_none() {
        missing.push("libm".to_string());
    }
    if !build.static_link && !build.shared {
        match elf_dynamic_linker(&target, global.llvm.abi.as_deref()) {
            Some(loader) => {
                let loader_name =
                    Path::new(loader).file_name().and_then(|name| name.to_str()).unwrap_or(loader);
                if find_elf_runtime_file(&target, global, loader_name).is_none() {
                    missing.push(format!("dynamic loader ({loader})"));
                }
            },
            None => missing.push(format!(
                "dynamic loader for ABI {}",
                global.llvm.abi.as_deref().unwrap_or("default")
            )),
        }
    }

    if missing.is_empty() {
        return Ok(());
    }

    let location = global
        .llvm
        .sysroot
        .as_deref()
        .map(|sysroot| format!("sysroot '{}'", sysroot))
        .unwrap_or_else(|| "the target-specific system paths".to_string());
    Err(CliError::CommandFailed(format!(
        "target runtime for '{}' is incomplete in {} (missing: {}); install the target C runtime or pass --sysroot=<path> to a complete target sysroot; for a freestanding binary use --freestanding with an explicit entry point",
        target,
        location,
        missing.join(", ")
    )))
}

fn build_linker_args(
    global: &Global,
    build: &BuildRequest,
    objects: &[String],
    output: &Path,
) -> (String, Vec<String>) {
    let target = target_triple_for_global(global);
    if llvm::backend::is_windows_msvc_target(&target) {
        let mut args = llvm::backend::msvc_link_args(
            &target,
            objects,
            &output.to_string_lossy(),
            &global.link.libs,
            &llvm::msvc::sdk::discovered_arguments(&target, &global.link.paths),
            global.llvm.no_default_libs,
            build.static_link,
            build.shared,
            build.entry.as_deref(),
            &global.llvm.link_args,
        );
        llvm::msvc::runtime::add_builtins(&target, &mut args);
        return (
            global.llvm.linker.clone().unwrap_or_else(|| resolve_bundled_tool("lld-link")),
            args,
        );
    }
    if let Some(linker) = &global.llvm.linker {
        return build_user_linker_args(linker, global, build, objects, output);
    }

    let target = target_triple_for_global(global);
    if is_wasm_target(&target) {
        build_wasm_lld_args(global, build, objects, output)
    } else if is_darwin_target(&target) {
        build_darwin_lld_args(global, build, objects, output, &target)
    } else {
        build_elf_lld_args(global, build, objects, output, &target)
    }
}

fn build_wasm_lld_args(
    global: &Global,
    build: &BuildRequest,
    objects: &[String],
    output: &Path,
) -> (String, Vec<String>) {
    let mut args = Vec::new();
    let target = target_triple_for_global(global);
    if matches!(
        target_spec_for_triple(&target).map(|spec| spec.codegen),
        Some(CodegenTarget::Wasm64Unknown)
    ) {
        args.push("-mwasm64".to_string());
    }
    for object in objects {
        args.push(object.clone());
    }
    append_link_search_and_libs(&mut args, global);
    append_lld_link_args(&mut args, &global.llvm.link_args);

    if let Some(entry) = &build.entry {
        args.push(format!("--entry={entry}"));
    } else {
        args.push("--no-entry".to_string());
    }
    args.push("--allow-undefined".to_string());
    args.push("--export-if-defined=main".to_string());
    args.push("--export-memory".to_string());
    args.push("-o".to_string());
    args.push(output.to_string_lossy().to_string());

    (resolve_bundled_tool("wasm-ld"), args)
}

fn wasm_runner(memory64: bool, wasi: bool, format: ErrorFormat) -> String {
    let json = format == ErrorFormat::Json;
    format!(
        "{}\nawait runWaveModule({{ memory64: {memory64}, wasi: {wasi}, jsonErrors: {json} }});\n",
        include_str!("runtime/wasm_host.mjs")
    )
}

fn build_execute_command(
    global: &Global,
    build: &BuildRequest,
    output: &Path,
) -> (String, Vec<String>) {
    let target = target_triple_for_global(global);
    let codegen = target_spec_for_triple(&target).map(|spec| spec.codegen);
    match codegen {
        Some(target @ (CodegenTarget::Wasm32Unknown | CodegenTarget::Wasm64Unknown)) => {
            let mut args = vec!["--no-warnings".to_string()];
            let runner =
                wasm_runner(target == CodegenTarget::Wasm64Unknown, false, global.error_format);
            args.extend([
                "--input-type=module".to_string(),
                "--eval".to_string(),
                runner,
                output.to_string_lossy().to_string(),
            ]);
            args.extend(build.run_args.iter().cloned());
            ("node".to_string(), args)
        },
        Some(CodegenTarget::Wasm32WasiP1) => {
            let mut args = vec![
                "--no-warnings".to_string(),
                "--input-type=module".to_string(),
                "--eval".to_string(),
                wasm_runner(false, true, global.error_format),
                output.to_string_lossy().to_string(),
            ];
            args.extend(build.run_args.iter().cloned());
            ("node".to_string(), args)
        },
        Some(CodegenTarget::LinuxLoongArch64) if std::env::consts::ARCH != "loongarch64" => {
            let mut args = Vec::new();
            if let Some(sysroot) = &global.llvm.sysroot {
                args.push("-L".to_string());
                args.push(sysroot.clone());
            }
            args.push(output.to_string_lossy().to_string());
            args.extend(build.run_args.iter().cloned());
            ("qemu-loongarch64".to_string(), args)
        },
        _ => (output.to_string_lossy().to_string(), build.run_args.clone()),
    }
}

fn build_user_linker_args(
    linker: &str,
    global: &Global,
    build: &BuildRequest,
    objects: &[String],
    output: &Path,
) -> (String, Vec<String>) {
    let mut args = Vec::new();

    for obj in objects {
        args.push(obj.clone());
    }
    append_link_search_and_libs(&mut args, global);
    let name = linker.rsplit(['/', '\\']).next().unwrap_or(linker);
    let name = name.strip_suffix(".exe").unwrap_or(name);
    let direct_linker = matches!(name, "ld" | "ld.lld" | "ld64.lld" | "wasm-ld")
        || name.ends_with("-ld")
        || name.ends_with("-ld.lld");
    if direct_linker {
        append_lld_link_args(&mut args, &global.llvm.link_args);
    } else {
        args.extend(global.llvm.link_args.iter().cloned());
    }
    append_entry_args(&mut args, build, !direct_linker);
    append_common_link_mode_args(&mut args, build, LinkerDialect::Gnu);

    args.push("-o".to_string());
    args.push(output.to_string_lossy().to_string());

    if !global.llvm.no_default_libs && !is_wasm_target(&target_triple_for_global(global)) {
        args.push("-lc".to_string());
        args.push("-lm".to_string());
    }

    (linker.to_string(), args)
}

fn build_darwin_lld_args(
    global: &Global,
    build: &BuildRequest,
    objects: &[String],
    output: &Path,
    target: &str,
) -> (String, Vec<String>) {
    let mut args = Vec::new();
    args.push("-arch".to_string());
    args.push(darwin_arch(target).to_string());

    let macos_version = macos_deployment_version();
    args.push("-platform_version".to_string());
    args.push("macos".to_string());
    args.push(macos_version.clone());
    args.push(macos_version);

    let detected_sysroot = detect_macos_sysroot_owned();
    if let Some(sysroot) = global.llvm.sysroot.as_deref().or(detected_sysroot.as_deref()) {
        args.push("-syslibroot".to_string());
        args.push(sysroot.to_string());
    }

    for obj in objects {
        args.push(obj.clone());
    }
    append_link_search_and_libs(&mut args, global);
    append_lld_link_args(&mut args, &global.llvm.link_args);
    append_entry_args(&mut args, build, false);
    append_common_link_mode_args(&mut args, build, LinkerDialect::Darwin);

    args.push("-o".to_string());
    args.push(output.to_string_lossy().to_string());

    if !global.llvm.no_default_libs {
        args.push("-lSystem".to_string());
    }

    (resolve_bundled_tool("ld64.lld"), args)
}

fn build_elf_lld_args(
    global: &Global,
    build: &BuildRequest,
    objects: &[String],
    output: &Path,
    target: &str,
) -> (String, Vec<String>) {
    let mut args = Vec::new();

    if let Some(emulation) = elf_lld_emulation(target) {
        args.push("-m".to_string());
        args.push(emulation.to_string());
    }
    if let Some(sysroot) = elf_lld_sysroot(target, global) {
        args.push(format!("--sysroot={}", sysroot));
    }
    let mut uses_elf_end_files = false;
    if !global.llvm.no_default_libs && is_hosted_elf_target(target) && !build.shared {
        if !build.static_link {
            if let Some(dynamic_linker) = elf_dynamic_linker(target, global.llvm.abi.as_deref()) {
                args.push(format!("--dynamic-linker={}", dynamic_linker));
            }
        }
        uses_elf_end_files = append_elf_start_files(&mut args, target, global, build);
    }

    for obj in objects {
        args.push(obj.clone());
    }
    if !global.llvm.no_default_libs && is_hosted_elf_target(target) {
        append_elf_search_paths(&mut args, target, global);
    }
    append_link_search_and_libs(&mut args, global);
    append_lld_link_args(&mut args, &global.llvm.link_args);
    append_entry_args(&mut args, build, false);
    append_common_link_mode_args(&mut args, build, LinkerDialect::Gnu);

    if !global.llvm.no_default_libs && is_hosted_elf_target(target) {
        append_elf_default_libs(&mut args, target, global);
        if uses_elf_end_files {
            append_elf_end_files(&mut args, target, global);
        }
    }

    args.push("-o".to_string());
    args.push(output.to_string_lossy().to_string());

    (resolve_bundled_tool("ld.lld"), args)
}

fn append_entry_args(args: &mut Vec<String>, build: &BuildRequest, compiler_driver: bool) {
    if let Some(entry) = &build.entry {
        if compiler_driver {
            args.push(format!("-Wl,-e,{entry}"));
        } else {
            args.extend(["-e".to_string(), entry.clone()]);
        }
    }
}

#[derive(Clone, Copy)]
enum LinkerDialect {
    Gnu,
    Darwin,
}

fn append_common_link_mode_args(
    args: &mut Vec<String>,
    build: &BuildRequest,
    dialect: LinkerDialect,
) {
    if build.shared {
        args.push(
            match dialect {
                LinkerDialect::Gnu => "-shared",
                LinkerDialect::Darwin => "-dylib",
            }
            .to_string(),
        );
    }
    if build.static_link {
        args.push("-static".to_string());
    }
    if build.pie == Some(true) {
        args.push("-pie".to_string());
    }
    if build.pie == Some(false) {
        args.push(
            match dialect {
                LinkerDialect::Gnu => "-no-pie",
                LinkerDialect::Darwin => "-no_pie",
            }
            .to_string(),
        );
    }
}

fn append_link_search_and_libs(args: &mut Vec<String>, global: &Global) {
    for path in &global.link.paths {
        args.push(format!("-L{}", path));
    }
    for lib in &global.link.libs {
        args.push(format!("-l{}", lib));
    }
}

fn collect_linker_input_paths(
    objects: &[String],
    args: &[String],
    static_link: bool,
) -> Vec<String> {
    let mut inputs = objects.to_vec();
    let mut search_paths = Vec::new();
    let mut libraries = Vec::new();
    let mut index = 0;
    while index < args.len() {
        let argument = &args[index];
        if matches!(argument.as_str(), "-o" | "--output") {
            index += 2;
            continue;
        }
        if argument == "-L" {
            if let Some(path) = args.get(index + 1) {
                search_paths.push(PathBuf::from(path));
            }
            index += 2;
            continue;
        }
        if let Some(path) = argument.strip_prefix("-L") {
            if !path.is_empty() {
                search_paths.push(PathBuf::from(path));
            }
            index += 1;
            continue;
        }
        if let Some(library) = argument.strip_prefix("-l") {
            if !library.is_empty() {
                libraries.push(library.to_string());
            }
            index += 1;
            continue;
        }
        if argument.starts_with('-') {
            index += 1;
            continue;
        }
        if Path::new(argument).is_file() {
            inputs.push(argument.clone());
        }
        index += 1;
    }

    for library in libraries {
        let candidates = if let Some(exact) = library.strip_prefix(':') {
            vec![exact.to_string()]
        } else if static_link {
            vec![format!("lib{library}.a")]
        } else {
            vec![format!("lib{library}.so"), format!("lib{library}.a")]
        };
        if let Some(path) = search_paths
            .iter()
            .flat_map(|directory| candidates.iter().map(move |name| directory.join(name)))
            .find(|path| path.is_file())
        {
            inputs.push(path.to_string_lossy().to_string());
        }
    }

    inputs.sort();
    inputs.dedup();
    inputs
}

fn append_lld_link_args(args: &mut Vec<String>, link_args: &[String]) {
    for arg in link_args {
        if arg == "-nostartfiles" {
            continue;
        }
        if let Some(rest) = arg.strip_prefix("-Wl,") {
            args.extend(
                rest.split(',').filter(|part| !part.is_empty()).map(|part| part.to_string()),
            );
        } else {
            args.push(arg.clone());
        }
    }
}

fn target_triple_for_global(global: &Global) -> String {
    global.llvm.target.clone().unwrap_or_else(host_target_triple)
}

fn is_darwin_target(target: &str) -> bool {
    target_spec_for_triple(target).is_some_and(|spec| spec.os == "macos")
}

fn is_linux_target(target: &str) -> bool {
    target_spec_for_triple(target).is_some_and(|spec| spec.os == "linux")
}

fn is_wasm_target(target: &str) -> bool {
    target_spec_for_triple(target).is_some_and(|spec| spec.object_format == "wasm")
}

fn is_freebsd_target(target: &str) -> bool {
    target_spec_for_triple(target).is_some_and(|spec| spec.os == "freebsd")
}

fn is_hosted_elf_target(target: &str) -> bool {
    is_linux_target(target) || is_freebsd_target(target)
}

fn darwin_arch(target: &str) -> &'static str {
    match target_spec_for_triple(target).map(|spec| spec.codegen) {
        Some(CodegenTarget::DarwinArm64) => "arm64",
        Some(CodegenTarget::DarwinX86_64) => "x86_64",
        _ => unreachable!("Darwin linker requires a registered Darwin target"),
    }
}

fn elf_lld_emulation(target: &str) -> Option<&'static str> {
    match target_spec_for_triple(target)?.codegen {
        CodegenTarget::LinuxX86_64
        | CodegenTarget::FreeBsdX86_64
        | CodegenTarget::FreestandingX86_64 => Some("elf_x86_64"),
        CodegenTarget::LinuxArm64
        | CodegenTarget::FreeBsdArm64
        | CodegenTarget::FreestandingArm64 => Some("aarch64elf"),
        CodegenTarget::LinuxRISCV64
        | CodegenTarget::FreeBsdRISCV64
        | CodegenTarget::FreestandingRISCV64 => Some("elf64lriscv"),
        CodegenTarget::LinuxLoongArch64 => Some("elf64loongarch"),
        _ => None,
    }
}

fn elf_dynamic_linker(target: &str, abi: Option<&str>) -> Option<&'static str> {
    match target_spec_for_triple(target)?.codegen {
        CodegenTarget::LinuxX86_64 => Some("/lib64/ld-linux-x86-64.so.2"),
        CodegenTarget::LinuxArm64 => Some("/lib/ld-linux-aarch64.so.1"),
        CodegenTarget::LinuxRISCV64 => match abi {
            Some("lp64") => Some("/lib/ld-linux-riscv64-lp64.so.1"),
            Some("lp64f") => Some("/lib/ld-linux-riscv64-lp64f.so.1"),
            Some("lp64d") | None => Some("/lib/ld-linux-riscv64-lp64d.so.1"),
            Some(_) => None,
        },
        CodegenTarget::LinuxLoongArch64 => match abi {
            Some("lp64s") => Some("/lib64/ld-linux-loongarch-lp64s.so.1"),
            Some("lp64d") | None => Some("/lib64/ld-linux-loongarch-lp64d.so.1"),
            // glibc does not currently provide an LP64F configuration.
            Some("lp64f") | Some(_) => None,
        },
        CodegenTarget::FreeBsdX86_64
        | CodegenTarget::FreeBsdArm64
        | CodegenTarget::FreeBsdRISCV64 => Some("/libexec/ld-elf.so.1"),
        _ => None,
    }
}

fn linux_multiarch(target: &str) -> Option<&'static str> {
    match target_spec_for_triple(target)?.codegen {
        CodegenTarget::LinuxX86_64 => Some("x86_64-linux-gnu"),
        CodegenTarget::LinuxArm64 => Some("aarch64-linux-gnu"),
        CodegenTarget::LinuxRISCV64 => Some("riscv64-linux-gnu"),
        CodegenTarget::LinuxLoongArch64 => Some("loongarch64-linux-gnu"),
        _ => None,
    }
}

fn append_elf_start_files(
    args: &mut Vec<String>,
    target: &str,
    global: &Global,
    build: &BuildRequest,
) -> bool {
    if build.no_start_files {
        return false;
    }

    let start_name = elf_start_file_name(build);

    if is_freebsd_target(target) {
        args.push(
            find_elf_runtime_file(target, global, start_name)
                .unwrap_or_else(|| start_name.to_string()),
        );
        args.push(
            find_elf_runtime_file(target, global, "crti.o").unwrap_or_else(|| "crti.o".to_string()),
        );
        return true;
    }

    append_bundled_linux_crt(
        args,
        bundled_linux_crt_path(target, global.llvm.abi.as_deref(), start_name),
    );
    args.push(
        bundled_linux_crt_path(target, global.llvm.abi.as_deref(), "crti.o")
            .to_string_lossy()
            .to_string(),
    );
    true
}

fn elf_start_file_name(build: &BuildRequest) -> &'static str {
    match (build.static_link, build.pie) {
        (true, Some(true)) => "rcrt1.o",
        (false, Some(true)) => "Scrt1.o",
        _ => "crt1.o",
    }
}

fn append_elf_end_files(args: &mut Vec<String>, target: &str, global: &Global) {
    if is_freebsd_target(target) {
        args.push(
            find_elf_runtime_file(target, global, "crtn.o").unwrap_or_else(|| "crtn.o".to_string()),
        );
        return;
    }
    args.push(
        bundled_linux_crt_path(target, global.llvm.abi.as_deref(), "crtn.o")
            .to_string_lossy()
            .to_string(),
    );
}

fn append_elf_default_libs(args: &mut Vec<String>, target: &str, global: &Global) {
    append_elf_default_lib(args, target, global, "c", &["libc.so", "libc.a"], &["libc.so.6"]);
    append_elf_default_lib(args, target, global, "m", &["libm.so", "libm.a"], &["libm.so.6"]);
}

fn append_elf_default_lib(
    args: &mut Vec<String>,
    target: &str,
    global: &Global,
    link_name: &str,
    development_names: &[&str],
    runtime_names: &[&str],
) {
    if find_elf_runtime_file_any(target, global, development_names).is_some() {
        args.push(format!("-l{}", link_name));
        return;
    }

    if let Some(path) = find_elf_runtime_file_any(target, global, runtime_names) {
        args.push(path);
        return;
    }

    args.push(format!("-l{}", link_name));
}

fn append_bundled_linux_crt(args: &mut Vec<String>, path: PathBuf) {
    args.push("-e".to_string());
    args.push("_start".to_string());
    args.push(path.to_string_lossy().to_string());
}

fn bundled_linux_crt_path(target: &str, abi: Option<&str>, name: &str) -> PathBuf {
    llvm::toolchain::find_bundled_linux_crt(target, abi, name)
        .unwrap_or_else(|| llvm::toolchain::expected_bundled_linux_crt(target, abi, name))
}

fn append_elf_search_paths(args: &mut Vec<String>, target: &str, global: &Global) {
    for path in elf_runtime_dirs(target, global) {
        if path.exists() {
            args.push(format!("-L{}", path.display()));
        }
    }
}

fn find_elf_runtime_file(target: &str, global: &Global, name: &str) -> Option<String> {
    elf_runtime_dirs(target, global)
        .into_iter()
        .map(|dir| dir.join(name))
        .find(|path| path.is_file())
        .map(|path| path.to_string_lossy().to_string())
}

fn find_elf_runtime_file_any(target: &str, global: &Global, names: &[&str]) -> Option<String> {
    for dir in elf_runtime_dirs(target, global) {
        for name in names {
            let path = dir.join(name);
            if path.exists() {
                return Some(path.to_string_lossy().to_string());
            }
        }
    }
    None
}

fn elf_lld_sysroot(target: &str, global: &Global) -> Option<String> {
    let sysroot = global.llvm.sysroot.as_deref()?;

    // Debian-style cross runtime prefixes (for example
    // /usr/riscv64-linux-gnu) are accepted as Wave sysroots so target CRT and
    // libraries can be discovered inside them. Their libc/libm linker
    // scripts, however, refer back to that prefix with absolute paths. Passing
    // the same prefix to ld.lld as --sysroot would prepend it a second time.
    // Root the linker at the host filesystem only for this detected layout;
    // all search paths and CRT objects remain the target-owned paths selected
    // by elf_runtime_dirs.
    if linker_script_references_absolute_sysroot(target, global, sysroot) {
        Some("/".to_string())
    } else {
        Some(sysroot.to_string())
    }
}

fn linker_script_references_absolute_sysroot(target: &str, global: &Global, sysroot: &str) -> bool {
    if sysroot.is_empty() || !Path::new(sysroot).is_absolute() {
        return false;
    }
    let normalized_sysroot = sysroot.trim_end_matches(['/', '\\']).replace('\\', "/");
    if normalized_sysroot.is_empty() {
        return false;
    }
    let absolute_prefix = format!("{}/", normalized_sysroot);

    ["libc.so", "libm.so"].into_iter().any(|name| {
        let Some(path) = find_elf_runtime_file(target, global, name) else {
            return false;
        };
        fs::read_to_string(path)
            .is_ok_and(|script| script.replace('\\', "/").contains(&absolute_prefix))
    })
}

fn elf_runtime_dirs(target: &str, global: &Global) -> Vec<PathBuf> {
    let sysroot = global.llvm.sysroot.as_deref().unwrap_or("");
    let mut dirs = Vec::new();

    if let Some(multiarch) = linux_multiarch(target) {
        dirs.push(sysroot_path(sysroot, &format!("usr/lib/{}", multiarch)));
        dirs.push(sysroot_path(sysroot, &format!("lib/{}", multiarch)));
        dirs.push(sysroot_path(sysroot, &format!("usr/{}/lib", multiarch)));
        dirs.push(sysroot_path(sysroot, &format!("{}/lib", multiarch)));
    }
    if let Some(abi_dir) = riscv64_abi_lib_dir(target, global.llvm.abi.as_deref()) {
        dirs.push(sysroot_path(sysroot, &format!("usr/{}", abi_dir)));
        dirs.push(sysroot_path(sysroot, abi_dir));
    }

    // Generic lib directories are only target-owned when they are inside an
    // explicit sysroot or when the target is the host. A cross target must
    // never consume the host's crt objects or libc by accident.
    if !sysroot.is_empty() || target == host_target_triple() {
        dirs.push(sysroot_path(sysroot, "usr/lib64"));
        dirs.push(sysroot_path(sysroot, "lib64"));
        dirs.push(sysroot_path(sysroot, "usr/lib"));
        dirs.push(sysroot_path(sysroot, "lib"));
    }
    dirs.dedup();
    dirs
}

fn riscv64_abi_lib_dir(target: &str, abi: Option<&str>) -> Option<&'static str> {
    match target_spec_for_triple(target)?.codegen {
        CodegenTarget::LinuxRISCV64 => match abi {
            Some("lp64") => Some("lib64/lp64"),
            Some("lp64f") => Some("lib64/lp64f"),
            Some("lp64d") | None => Some("lib64/lp64d"),
            Some(_) => None,
        },
        _ => None,
    }
}

fn sysroot_path(sysroot: &str, suffix: &str) -> PathBuf {
    if sysroot.is_empty() {
        PathBuf::from("/").join(suffix)
    } else {
        Path::new(sysroot).join(suffix)
    }
}

fn macos_deployment_version() -> String {
    if let Ok(value) = env::var("MACOSX_DEPLOYMENT_TARGET") {
        let value = value.trim();
        if !value.is_empty() {
            return value.to_string();
        }
    }

    ProcessCommand::new("sw_vers")
        .arg("-productVersion")
        .output()
        .ok()
        .filter(|out| out.status.success())
        .map(|out| String::from_utf8_lossy(&out.stdout).trim().to_string())
        .filter(|value| !value.is_empty())
        .unwrap_or_else(|| "11.0".to_string())
}

fn detect_macos_sysroot_owned() -> Option<String> {
    if let Ok(value) = env::var("SDKROOT") {
        let value = value.trim().to_string();
        if !value.is_empty() {
            return Some(value);
        }
    }

    ProcessCommand::new("xcrun")
        .args(["--sdk", "macosx", "--show-sdk-path"])
        .output()
        .ok()
        .filter(|out| out.status.success())
        .map(|out| String::from_utf8_lossy(&out.stdout).trim().to_string())
        .filter(|value| !value.is_empty())
}

fn resolve_bundled_tool(tool: &str) -> String {
    if let Some(path) = resolve_bundled_tool_path(tool) {
        return path.to_string_lossy().to_string();
    }
    executable_tool_name(tool)
}

fn resolve_bundled_tool_path(tool: &str) -> Option<PathBuf> {
    for dir in llvm_tool_search_dirs() {
        let candidate = dir.join(executable_tool_name(tool));
        if candidate.is_file() {
            return Some(candidate);
        }
    }
    None
}

fn configure_bundled_llvm_tool_env(cmd: &mut ProcessCommand, bin: &str) {
    let Some(bin_dir) = bundled_llvm_bin_dir(bin) else {
        return;
    };

    if cfg!(target_os = "linux") {
        if let Some(lib_dir) = bin_dir.parent().map(|llvm_dir| llvm_dir.join("lib")) {
            if lib_dir.is_dir() {
                prepend_env_path(cmd, "LD_LIBRARY_PATH", lib_dir);
            }
        }
    } else if cfg!(windows) {
        if let Some(root_dir) = bin_dir.parent().and_then(|llvm_dir| llvm_dir.parent()) {
            prepend_env_path(cmd, "PATH", root_dir.to_path_buf());
        }
        prepend_env_path(cmd, "PATH", bin_dir);
    }
}

fn bundled_llvm_bin_dir(bin: &str) -> Option<PathBuf> {
    let bin_path = Path::new(bin);
    let bin_dir = bin_path.parent()?;
    if bin_dir.file_name().and_then(|name| name.to_str()) != Some("bin") {
        return None;
    }

    let llvm_dir = bin_dir.parent()?;
    if llvm_dir.file_name().and_then(|name| name.to_str()) != Some("llvm") {
        return None;
    }

    Some(bin_dir.to_path_buf())
}

fn prepend_env_path(cmd: &mut ProcessCommand, name: &str, first: PathBuf) {
    let mut paths = vec![first];
    if let Some(current) = env::var_os(name) {
        paths.extend(env::split_paths(&current));
    }
    if let Ok(joined) = env::join_paths(paths) {
        cmd.env(name, joined);
    }
}

fn executable_tool_name(tool: &str) -> String {
    if cfg!(windows) && !tool.to_ascii_lowercase().ends_with(".exe") {
        format!("{}.exe", tool)
    } else {
        tool.to_string()
    }
}

fn llvm_tool_search_dirs() -> Vec<PathBuf> {
    let mut dirs = Vec::new();

    if let Ok(path) = env::var("WAVE_LLVM_BIN") {
        if !path.trim().is_empty() {
            dirs.push(PathBuf::from(path));
        }
    }
    for env_name in ["WAVE_LLVM_HOME", "LLVM_SYS_211_PREFIX"] {
        if let Ok(path) = env::var(env_name) {
            if !path.trim().is_empty() {
                dirs.push(PathBuf::from(path).join("bin"));
            }
        }
    }
    if let Ok(exe) = env::current_exe() {
        if let Some(dir) = exe.parent() {
            dirs.push(dir.to_path_buf());
            dirs.push(dir.join("llvm").join("bin"));
            if let Some(root) = dir.parent() {
                dirs.push(root.join("llvm").join("bin"));
                dirs.push(root.join("lib").join("wave").join("llvm").join("bin"));
            }
        }
    }

    dirs
}

fn linker_tool_name(bin: &str) -> String {
    Path::new(bin).file_name().and_then(|name| name.to_str()).unwrap_or(bin).to_string()
}

fn missing_linker_tool_name(_global: &Global, bin: &str) -> String {
    linker_tool_name(bin)
}

fn default_linker_name(global: &Global) -> String {
    if let Some(linker) = &global.llvm.linker {
        return linker.clone();
    }

    let target = target_triple_for_global(global);
    if llvm::backend::is_windows_msvc_target(&target) {
        return resolve_bundled_tool("lld-link");
    }
    if is_wasm_target(&target) {
        resolve_bundled_tool("wasm-ld")
    } else if is_darwin_target(&target) {
        resolve_bundled_tool("ld64.lld")
    } else {
        resolve_bundled_tool("ld.lld")
    }
}

fn normalize_opt_for_llvm_tool(flag: &str) -> &str {
    match flag {
        "-Ofast" => "-O3",
        other => other,
    }
}

fn ensure_parent_dir(path: &Path) -> Result<(), CliError> {
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent)?;
        }
    }
    Ok(())
}

fn dry_run_explicit_emit_steps(
    global: &Global,
    build: &BuildRequest,
    classified: &[ClassifiedInput],
) -> Vec<String> {
    let Some(emit_set) = build.emit.as_set() else {
        return Vec::new();
    };

    let total_inputs = classified.len();
    let mut steps = Vec::new();
    let kinds = [EmitKind::Ast, EmitKind::Ir, EmitKind::Bc, EmitKind::Asm];

    for (input_index, input) in classified.iter().enumerate() {
        for kind in kinds {
            if !emit_set.contains(&kind) || !supports_emit_for_input(kind, input.kind) {
                continue;
            }

            let output =
                resolve_extra_emit_output_path(build, input, kind, input_index, total_inputs);
            let step = match (kind, input.kind) {
                (EmitKind::Ast, InputKind::Wave) => {
                    format!(
                        "[wave frontend] {} -> {} (ast)",
                        input.path.display(),
                        output.display()
                    )
                },
                (EmitKind::Ir, InputKind::Wave) => {
                    format!("[wave frontend] {} -> {} (ir)", input.path.display(), output.display())
                },
                (EmitKind::Ir, InputKind::Ir) => {
                    format!("cp {} {}", input.path.display(), output.display())
                },
                (EmitKind::Bc, InputKind::Wave) | (EmitKind::Asm, InputKind::Wave) => {
                    format!(
                        "[wave frontend + LLVM] {} -> {} ({})",
                        input.path.display(),
                        output.display(),
                        emit_kind_name(kind)
                    )
                },
                (EmitKind::Bc, InputKind::Ir)
                | (EmitKind::Asm, InputKind::Ir)
                | (EmitKind::Asm, InputKind::Bc) => {
                    let (bin, args) =
                        build_llvm_lowering_args(global, &input.path, input.kind, &output, kind);
                    shell_join(&bin, &args)
                },
                (EmitKind::Bc, InputKind::Bc) | (EmitKind::Asm, InputKind::Asm) => {
                    format!("cp {} {}", input.path.display(), output.display())
                },
                _ => continue,
            };
            steps.push(step);
        }
    }

    steps
}

fn print_dry_run(
    global: &Global,
    build: &BuildRequest,
    classified: &[ClassifiedInput],
    plan: &BuildPlan,
) {
    match build.error_format {
        ErrorFormat::Human => print_dry_run_human(global, build, classified, plan),
        ErrorFormat::Json => print_dry_run_json(global, build, classified, plan),
    }
}

fn print_dry_run_human(
    global: &Global,
    build: &BuildRequest,
    classified: &[ClassifiedInput],
    plan: &BuildPlan,
) {
    let target = target_triple_for_global(global);
    let target_options = target_options_for(&target, &global.llvm)
        .expect("dry-run requires validated target options");
    println!("DRY-RUN PLAN");
    println!("  mode: {}", build_mode_label(build));
    println!("  target: {}", target);
    println!("  cpu: {}", target_options.cpu);
    println!("  features: {}", target_options.features);
    println!("  abi: {}", target_options.abi.as_deref().unwrap_or(""));
    println!("  isa: {}", target_options.isa.as_deref().unwrap_or(""));
    println!("  sysroot: {}", global.llvm.sysroot.as_deref().unwrap_or(""));
    println!("  sysroot-source: {}", global.llvm.sysroot_source.as_deref().unwrap_or(""));
    println!("  emit: {}", render_emit_spec(&build.emit));
    println!("  link-only: {}", build.link_only);
    println!("  run: {}", build.run);
    println!("  freestanding: {}", build.freestanding);
    if let Some(entry) = &build.entry {
        println!("  entry: {}", entry);
    }
    if let Some(script) = &build.linker_script {
        println!("  linker-script: {}", script.display());
    }
    println!("  no-start-files: {}", build.no_start_files);
    if !build.run_args.is_empty() {
        println!("  run-args: {}", build.run_args.join(" "));
    }

    println!("  inputs:");
    for i in classified {
        println!("    - {} ({})", i.path.display(), i.kind.as_str());
    }

    if build.emit.is_check() {
        println!("  steps:");
        println!("    - frontend check only (parse/import/semantic)");
        return;
    }

    let emit_jobs = dry_run_explicit_emit_steps(global, build, classified);
    if !emit_jobs.is_empty() {
        println!("  emit jobs:");
        for step in emit_jobs {
            println!("    - {}", step);
        }
    }

    if !plan.compile_jobs.is_empty() {
        println!("  compile jobs:");
        for job in &plan.compile_jobs {
            if job.kind == InputKind::Wave {
                println!(
                    "    - [wave frontend + LLVM] {} -> {}",
                    job.input.display(),
                    job.output.display()
                );
            } else {
                let (bin, args) = build_llvm_lowering_args(
                    global,
                    &job.input,
                    job.kind,
                    &job.output,
                    EmitKind::Obj,
                );
                println!("    - {}", shell_join(&bin, &args));
            }
        }
    }

    if let Some(link_output) = &plan.link_output {
        println!("  link:");
        println!("    - {}", render_link_command(global, build, &plan.link_inputs, link_output));
    }

    if build.run {
        if let Some(link_output) = &plan.link_output {
            let (program, args) = build_execute_command(global, build, link_output);
            println!("  run:");
            println!("    - {}", shell_join(&program, &args));
        }
    }
}

fn print_dry_run_json(
    global: &Global,
    build: &BuildRequest,
    classified: &[ClassifiedInput],
    plan: &BuildPlan,
) {
    let target = target_triple_for_global(global);
    let target_options = target_options_for(&target, &global.llvm)
        .expect("dry-run requires validated target options");
    let compile = Value::Array(
        plan.compile_jobs
            .iter()
            .map(|job| {
                let command = if job.kind == InputKind::Wave {
                    format!(
                        "wavec <internal-wave-compile> {} -o {}",
                        job.input.display(),
                        job.output.display()
                    )
                } else {
                    let (bin, args) = build_llvm_lowering_args(
                        global,
                        &job.input,
                        job.kind,
                        &job.output,
                        EmitKind::Obj,
                    );
                    shell_join(&bin, &args)
                };
                Value::object([
                    ("input", Value::string(job.input.to_string_lossy())),
                    ("kind", Value::string(job.kind.as_str())),
                    ("output", Value::string(job.output.to_string_lossy())),
                    ("command", Value::string(command)),
                ])
            })
            .collect(),
    );
    let link = plan
        .link_output
        .as_ref()
        .map(|output| {
            let (program, args) = build_linker_args(global, build, &plan.link_inputs, output);
            Value::object([
                ("output", Value::string(output.to_string_lossy())),
                ("inputs", Value::strings(&plan.link_inputs)),
                ("program", Value::string(&program)),
                ("args", Value::strings(&args)),
                ("command", Value::string(shell_join(&program, &args))),
            ])
        })
        .unwrap_or(Value::Null);
    let execute = if build.run {
        plan.link_output
            .as_ref()
            .map(|output| {
                let (program, args) = build_execute_command(global, build, output);
                Value::object([
                    ("program", Value::string(&program)),
                    ("args", Value::strings(&args)),
                    ("command", Value::string(shell_join(&program, &args))),
                ])
            })
            .unwrap_or(Value::Null)
    } else {
        Value::Null
    };
    let emit_kinds = match &build.emit {
        EmitSpec::Check => Value::strings(["check"]),
        EmitSpec::Set(set) => Value::strings(set.iter().map(|kind| emit_kind_name(*kind))),
    };
    let value = Value::object([
        ("schema_version", Value::integer(1)),
        ("mode", Value::string(build_mode_label(build))),
        ("target", Value::string(target)),
        ("cpu", Value::string(&target_options.cpu)),
        ("features", Value::string(&target_options.features)),
        ("abi", Value::optional_string(target_options.abi.as_deref())),
        ("isa", Value::optional_string(target_options.isa.as_deref())),
        ("sysroot", Value::optional_string(global.llvm.sysroot.as_deref())),
        ("sysroot_source", Value::optional_string(global.llvm.sysroot_source.as_deref())),
        ("emit", Value::string(render_emit_spec(&build.emit))),
        ("emit_kinds", emit_kinds),
        ("control_mode", Value::optional_string(build.emit.is_check().then_some("check"))),
        ("forced_input_type", Value::optional_string(build.input_type.map(|k| k.as_str()))),
        ("link_only", Value::Bool(build.link_only)),
        ("run", Value::Bool(build.run)),
        ("freestanding", Value::Bool(build.freestanding)),
        ("no_start_files", Value::Bool(build.no_start_files)),
        ("entry", Value::optional_string(build.entry.as_deref())),
        (
            "linker_script",
            Value::optional_string(build.linker_script.as_ref().map(|p| p.to_string_lossy())),
        ),
        ("out_dir", Value::optional_string(build.out_dir.as_ref().map(|p| p.to_string_lossy()))),
        (
            "target_dir",
            Value::optional_string(build.target_dir.as_ref().map(|p| p.to_string_lossy())),
        ),
        ("run_args", Value::strings(&build.run_args)),
        (
            "inputs",
            Value::Array(
                classified
                    .iter()
                    .map(|i| {
                        Value::object([
                            ("path", Value::string(i.path.to_string_lossy())),
                            ("kind", Value::string(i.kind.as_str())),
                        ])
                    })
                    .collect(),
            ),
        ),
        ("emit_jobs", Value::strings(dry_run_explicit_emit_steps(global, build, classified))),
        ("compile", compile),
        ("link", link),
        ("execute", execute),
    ]);
    println!("{}", schema_json(&value));
}

fn schema_json(value: &Value) -> String {
    wson::dumps(value, wson::Format::Json, false)
        .expect("CLI schema contains only shallow JSON values")
}

fn json_string_array(values: Vec<&str>) -> String {
    schema_json(&Value::strings(values))
}

fn shell_join(bin: &str, args: &[String]) -> String {
    let mut parts = Vec::with_capacity(args.len() + 1);
    parts.push(shell_quote(bin));
    for arg in args {
        parts.push(shell_quote(arg));
    }
    parts.join(" ")
}

fn shell_quote(s: &str) -> String {
    if s.is_empty() {
        return "''".to_string();
    }

    if s.chars().all(|c| {
        c.is_ascii_alphanumeric() || matches!(c, '_' | '-' | '.' | '/' | ':' | '=' | '+' | ',')
    }) {
        return s.to_string();
    }

    let mut out = String::from("'");
    for ch in s.chars() {
        if ch == '\'' {
            out.push_str("'\\''");
        } else {
            out.push(ch);
        }
    }
    out.push('\'');
    out
}

fn build_mode_label(build: &BuildRequest) -> &'static str {
    if build.emit.is_check() {
        return "check";
    }
    if build.link_only {
        return "link-only";
    }
    if build.run {
        return "build+run";
    }
    if build.emit.contains(EmitKind::Bin) {
        return "build";
    }
    "compile-only"
}

fn render_link_command(
    global: &Global,
    build: &BuildRequest,
    objects: &[String],
    output: &Path,
) -> String {
    let (bin, args) = build_linker_args(global, build, objects, output);
    shell_join(&bin, &args)
}

fn render_emit_spec(spec: &EmitSpec) -> String {
    match spec {
        EmitSpec::Check => "check".to_string(),
        EmitSpec::Set(set) => set
            .iter()
            .map(|k| match k {
                EmitKind::Ast => "ast",
                EmitKind::Ir => "ir",
                EmitKind::Bc => "bc",
                EmitKind::Asm => "asm",
                EmitKind::Obj => "obj",
                EmitKind::Bin => "bin",
            })
            .collect::<Vec<_>>()
            .join(","),
    }
}

fn host_target_triple() -> String {
    let arch = env::consts::ARCH;
    let os_part = match env::consts::OS {
        "linux" => "unknown-linux-gnu".to_string(),
        "macos" => "apple-darwin".to_string(),
        "windows" => "pc-windows-msvc".to_string(),
        other => format!("unknown-{}", other),
    };
    format!("{}-{}", arch, os_part)
}

fn supported_targets() -> Vec<&'static str> {
    supported_target_specs().into_iter().map(|spec| spec.triple).collect()
}

fn supported_input_types() -> Vec<&'static str> {
    vec!["wave", "ir", "bc", "asm", "obj", "archive"]
}

fn supported_artifact_emit_kinds() -> Vec<&'static str> {
    vec!["ast", "ir", "bc", "asm", "obj", "bin"]
}

fn supported_print_items() -> Vec<&'static str> {
    vec![
        "host",
        "host-target",
        "default-target",
        "target-spec",
        "target-list",
        "supported-targets",
        "supported-input-types",
        "supported-emit-kinds",
        "supported-print-items",
        "cpu-list",
        "target-features",
        "default-linker",
        "sysroot",
        "std-path",
        "dep-search-paths",
    ]
}

#[derive(Debug, Clone)]
struct SysrootSelection {
    path: String,
    source: String,
}

#[derive(Debug, Clone)]
struct TargetSpecInfo {
    triple: String,
    arch: String,
    vendor: Option<String>,
    os: Option<String>,
    env: Option<String>,
    cpu: String,
    features: String,
    abi: Option<String>,
    isa: Option<String>,
    object_format: &'static str,
    hosted: bool,
    supported: bool,
}

fn target_spec_info(global: &Global, target: &str) -> TargetSpecInfo {
    let spec = target_spec_for_triple(target)
        .expect("target spec rendering requires a validated target triple");
    let effective = resolve_target_options(
        spec,
        global.llvm.cpu.as_deref(),
        global.llvm.features.as_deref(),
        global.llvm.abi.as_deref(),
    )
    .expect("target spec rendering requires validated target options");

    TargetSpecInfo {
        triple: target.to_string(),
        arch: spec.architecture.name().to_string(),
        vendor: Some(spec.vendor.to_string()),
        os: Some(spec.os.to_string()),
        env: Some(spec.env.to_string()),
        cpu: effective.cpu,
        features: effective.features,
        abi: effective.abi,
        isa: effective.isa,
        object_format: spec.object_format,
        hosted: spec.hosted,
        supported: true,
    }
}

fn print_target_spec_human(global: &Global, target: &str) {
    let spec = target_spec_info(global, target);
    let target_global = global_with_target(global, target);
    let sysroot = effective_sysroot_selection(global, target)
        .expect("target spec rendering requires validated target options");
    println!("triple: {}", spec.triple);
    println!("arch: {}", spec.arch);
    println!("vendor: {}", spec.vendor.as_deref().unwrap_or(""));
    println!("os: {}", spec.os.as_deref().unwrap_or(""));
    println!("env: {}", spec.env.as_deref().unwrap_or(""));
    println!("cpu: {}", spec.cpu);
    println!("features: {}", spec.features);
    println!("abi: {}", spec.abi.as_deref().unwrap_or(""));
    println!("isa: {}", spec.isa.as_deref().unwrap_or(""));
    println!("object-format: {}", spec.object_format);
    println!("hosted: {}", spec.hosted);
    println!("freestanding: {}", !spec.hosted);
    println!("supported: {}", spec.supported);
    println!("default-linker: {}", default_linker_name(&target_global));
    println!("sysroot: {}", sysroot.as_ref().map(|value| value.path.as_str()).unwrap_or(""));
    println!(
        "sysroot-source: {}",
        sysroot.as_ref().map(|value| value.source.as_str()).unwrap_or("")
    );
}

fn target_spec_json(global: &Global, target: &str) -> String {
    let spec = target_spec_info(global, target);
    let target_global = global_with_target(global, target);
    let sysroot = effective_sysroot_selection(global, target)
        .expect("target spec rendering requires validated target options");
    schema_json(&Value::object([
        ("triple", Value::string(&spec.triple)),
        ("arch", Value::string(&spec.arch)),
        ("vendor", Value::optional_string(spec.vendor.as_deref())),
        ("os", Value::optional_string(spec.os.as_deref())),
        ("env", Value::optional_string(spec.env.as_deref())),
        ("cpu", Value::string(&spec.cpu)),
        ("features", Value::string(&spec.features)),
        ("abi", Value::optional_string(spec.abi.as_deref())),
        ("isa", Value::optional_string(spec.isa.as_deref())),
        ("object_format", Value::string(spec.object_format)),
        ("hosted", Value::Bool(spec.hosted)),
        ("freestanding", Value::Bool(!spec.hosted)),
        ("supported", Value::Bool(spec.supported)),
        ("default_linker", Value::string(default_linker_name(&target_global))),
        ("sysroot", Value::optional_string(sysroot.as_ref().map(|v| v.path.as_str()))),
        ("sysroot_source", Value::optional_string(sysroot.as_ref().map(|v| v.source.as_str()))),
    ]))
}

fn global_with_target(global: &Global, target: &str) -> Global {
    let mut out = global.clone();
    out.llvm.target = Some(target.to_string());
    out
}

fn ensure_supported_target(target: &str) -> Result<&'static TargetSpec, CliError> {
    if target.contains("windows-gnu") {
        let replacement = if target.starts_with("aarch64-") {
            "aarch64-pc-windows-msvc"
        } else {
            "x86_64-pc-windows-msvc"
        };
        return Err(CliError::usage(format!(
            "Windows GNU/MinGW target '{target}' has been retired; use --target={replacement} with matching Windows SDK and MSVC Build Tools"
        )));
    }
    target_spec_for_triple(target).ok_or_else(|| {
        CliError::usage(format!(
            "unsupported target '{}'; supported targets: {}; see `wavec print target-list`",
            target,
            supported_targets().join(", ")
        ))
    })
}

fn target_options_for(target: &str, llvm: &LlvmFlags) -> Result<EffectiveTargetOptions, CliError> {
    let spec = ensure_supported_target(target)?;
    resolve_target_options(spec, llvm.cpu.as_deref(), llvm.features.as_deref(), llvm.abi.as_deref())
        .map_err(CliError::usage)
}

fn resolve_target_configuration(llvm: &mut LlvmFlags) -> Result<(), CliError> {
    let target = llvm
        .target
        .clone()
        .ok_or_else(|| CliError::usage("target resolution did not produce a target triple"))?;
    let effective = target_options_for(&target, llvm)?;
    llvm.cpu = Some(effective.cpu);
    llvm.features = if effective.features.is_empty() { None } else { Some(effective.features) };
    llvm.abi = effective.abi;
    llvm.isa = effective.isa;
    Ok(())
}

fn resolve_build_sysroot(llvm: &mut LlvmFlags, build: &BuildRequest) {
    let needs_link = build.emit.contains(EmitKind::Bin) || build.run;
    if llvm.sysroot.is_some()
        || llvm.freestanding
        || llvm.no_default_libs
        || llvm.linker.is_some()
        || !needs_link
    {
        return;
    }

    let Some(target) = llvm.target.as_deref() else {
        return;
    };
    let Some(selection) = detect_default_sysroot(target, llvm.abi.as_deref()) else {
        return;
    };
    llvm.sysroot = Some(selection.path);
    llvm.sysroot_source = Some(selection.source);
}

fn validate_target_options_for(target: &str, llvm: &LlvmFlags) -> Result<(), CliError> {
    target_options_for(target, llvm).map(|_| ())
}

fn effective_sysroot_selection(
    global: &Global,
    target: &str,
) -> Result<Option<SysrootSelection>, CliError> {
    let effective = target_options_for(target, &global.llvm)?;
    if let Some(path) = global.llvm.sysroot.as_ref() {
        return Ok(Some(SysrootSelection {
            path: path.clone(),
            source: global.llvm.sysroot_source.clone().unwrap_or_else(|| "explicit".to_string()),
        }));
    }
    Ok(detect_default_sysroot(target, effective.abi.as_deref()))
}

fn detect_default_sysroot(target: &str, abi: Option<&str>) -> Option<SysrootSelection> {
    if is_darwin_target(target) {
        return detect_macos_sysroot_owned()
            .map(|path| SysrootSelection { path, source: "xcrun".to_string() });
    }

    if matches!(
        target_spec_for_triple(target).map(|spec| spec.codegen),
        Some(CodegenTarget::LinuxRISCV64)
    ) {
        return detect_riscv64_linux_sysroot(target, abi);
    }
    if matches!(
        target_spec_for_triple(target).map(|spec| spec.codegen),
        Some(CodegenTarget::LinuxLoongArch64)
    ) {
        return detect_loongarch64_linux_sysroot(target, abi);
    }

    None
}

fn detect_riscv64_linux_sysroot(target: &str, abi: Option<&str>) -> Option<SysrootSelection> {
    let mut candidates = linux_cross_gcc_sysroot_candidates(target);
    candidates.extend(
        [
            "/usr/riscv64-linux-gnu",
            "/usr/riscv64-linux-gnu/sys-root",
            "/usr/riscv64-linux-gnu/sysroot",
            "/opt/riscv/sysroot",
        ]
        .into_iter()
        .map(|path| (PathBuf::from(path), "standard-prefix".to_string())),
    );
    select_riscv64_linux_sysroot(target, abi, candidates)
}

fn linux_cross_gcc_sysroot_candidates(target: &str) -> Vec<(PathBuf, String)> {
    let Some(tool_prefix) = linux_multiarch(target) else {
        return Vec::new();
    };
    let mut candidates = Vec::new();
    let mut tool_prefixes = vec![tool_prefix];
    if matches!(
        target_spec_for_triple(target).map(|spec| spec.codegen),
        Some(CodegenTarget::LinuxLoongArch64)
    ) {
        // Loongson's official CLFS toolchain includes the vendor component in
        // executable names while Debian-style installations omit it.
        tool_prefixes.push("loongarch64-unknown-linux-gnu");
    }

    for tool_prefix in tool_prefixes {
        let tool = format!("{}-gcc", tool_prefix);
        if let Some(path) = command_stdout_path(&tool, "-print-sysroot") {
            candidates.push((path, tool.clone()));
        }

        if let Some(libc) = command_stdout_path(&tool, "-print-file-name=libc.so") {
            let canonical = fs::canonicalize(&libc).unwrap_or(libc);
            if canonical.is_file() {
                for ancestor in canonical.parent().into_iter().flat_map(Path::ancestors) {
                    if ancestor.parent().is_none() {
                        break;
                    }
                    candidates.push((ancestor.to_path_buf(), tool.clone()));
                }
            }
        }
    }

    candidates
}

fn detect_loongarch64_linux_sysroot(target: &str, abi: Option<&str>) -> Option<SysrootSelection> {
    let mut candidates = linux_cross_gcc_sysroot_candidates(target);
    candidates.extend(
        [
            "/usr/loongarch64-linux-gnu",
            "/usr/loongarch64-linux-gnu/sys-root",
            "/usr/loongarch64-linux-gnu/sysroot",
            "/opt/loongarch64-linux-gnu/sysroot",
            "/opt/loongarch/sysroot",
        ]
        .into_iter()
        .map(|path| (PathBuf::from(path), "standard-prefix".to_string())),
    );
    select_loongarch64_linux_sysroot(target, abi, candidates)
}

fn select_loongarch64_linux_sysroot<I>(
    target: &str,
    abi: Option<&str>,
    candidates: I,
) -> Option<SysrootSelection>
where
    I: IntoIterator<Item = (PathBuf, String)>,
{
    let mut seen = BTreeSet::new();
    for (candidate, source) in candidates {
        let Ok(candidate) = fs::canonicalize(candidate) else {
            continue;
        };
        if candidate.parent().is_none() || !seen.insert(candidate.clone()) {
            continue;
        }
        if loongarch64_linux_sysroot_is_complete(target, abi, &candidate) {
            return Some(SysrootSelection {
                path: candidate.to_string_lossy().to_string(),
                source,
            });
        }
    }
    None
}

fn loongarch64_linux_sysroot_is_complete(target: &str, abi: Option<&str>, root: &Path) -> bool {
    let Some(expected_abi_flags) = loongarch64_abi_elf_flags(abi) else {
        return false;
    };
    let mut global = Global::default();
    global.llvm.target = Some(target.to_string());
    global.llvm.abi = abi.map(str::to_string);
    global.llvm.sysroot = Some(root.to_string_lossy().to_string());

    if find_elf_runtime_file_any(target, &global, &["libc.so", "libc.a"]).is_none()
        || find_elf_runtime_file_any(target, &global, &["libm.so", "libm.a"]).is_none()
    {
        return false;
    }

    let Some(loader_name) = elf_dynamic_linker(target, abi)
        .and_then(|path| Path::new(path).file_name())
        .and_then(|name| name.to_str())
    else {
        return false;
    };
    ["libc.so.6", "libm.so.6", loader_name].into_iter().all(|name| {
        find_elf_runtime_file(target, &global, name).is_some_and(|path| {
            loongarch64_elf_header_matches(Path::new(&path), expected_abi_flags)
        })
    })
}

fn command_stdout_path(tool: &str, argument: &str) -> Option<PathBuf> {
    let output = ProcessCommand::new(tool).arg(argument).output().ok()?;
    if !output.status.success() {
        return None;
    }
    let value = String::from_utf8(output.stdout).ok()?;
    let value = value.trim();
    if value.is_empty() || value == argument.trim_start_matches("-print-file-name=") {
        return None;
    }
    Some(PathBuf::from(value))
}

fn select_riscv64_linux_sysroot<I>(
    target: &str,
    abi: Option<&str>,
    candidates: I,
) -> Option<SysrootSelection>
where
    I: IntoIterator<Item = (PathBuf, String)>,
{
    let mut seen = BTreeSet::new();
    for (candidate, source) in candidates {
        let Ok(candidate) = fs::canonicalize(candidate) else {
            continue;
        };
        if candidate.parent().is_none() || !seen.insert(candidate.clone()) {
            continue;
        }
        if riscv64_linux_sysroot_is_complete(target, abi, &candidate) {
            return Some(SysrootSelection {
                path: candidate.to_string_lossy().to_string(),
                source,
            });
        }
    }
    None
}

fn riscv64_linux_sysroot_is_complete(target: &str, abi: Option<&str>, root: &Path) -> bool {
    let Some(expected_abi_flags) = riscv64_abi_elf_flags(abi) else {
        return false;
    };
    let mut global = Global::default();
    global.llvm.target = Some(target.to_string());
    global.llvm.abi = abi.map(str::to_string);
    global.llvm.sysroot = Some(root.to_string_lossy().to_string());

    if find_elf_runtime_file_any(target, &global, &["libc.so", "libc.a"]).is_none()
        || find_elf_runtime_file_any(target, &global, &["libm.so", "libm.a"]).is_none()
    {
        return false;
    }

    let Some(loader_name) = elf_dynamic_linker(target, abi)
        .and_then(|path| Path::new(path).file_name())
        .and_then(|name| name.to_str())
    else {
        return false;
    };
    ["libc.so.6", "libm.so.6", loader_name].into_iter().all(|name| {
        find_elf_runtime_file(target, &global, name)
            .is_some_and(|path| riscv64_elf_header_matches(Path::new(&path), expected_abi_flags))
    })
}

fn riscv64_abi_elf_flags(abi: Option<&str>) -> Option<u32> {
    match abi.unwrap_or("lp64d") {
        "lp64" => Some(0x0),
        "lp64f" => Some(0x2),
        "lp64d" => Some(0x4),
        _ => None,
    }
}

fn riscv64_elf_header_matches(path: &Path, expected_abi_flags: u32) -> bool {
    let Ok(mut file) = fs::File::open(path) else {
        return false;
    };
    let mut header = [0_u8; 52];
    if file.read_exact(&mut header).is_err() || &header[..4] != b"\x7fELF" || header[4] != 2 {
        return false;
    }
    let (machine, flags) = match header[5] {
        1 => (
            u16::from_le_bytes([header[18], header[19]]),
            u32::from_le_bytes([header[48], header[49], header[50], header[51]]),
        ),
        2 => (
            u16::from_be_bytes([header[18], header[19]]),
            u32::from_be_bytes([header[48], header[49], header[50], header[51]]),
        ),
        _ => return false,
    };
    machine == 243 && flags & 0x6 == expected_abi_flags
}

fn loongarch64_abi_elf_flags(abi: Option<&str>) -> Option<u32> {
    match abi.unwrap_or("lp64d") {
        "lp64s" => Some(0x1),
        "lp64f" => Some(0x2),
        "lp64d" => Some(0x3),
        _ => None,
    }
}

fn loongarch64_elf_header_matches(path: &Path, expected_abi_flags: u32) -> bool {
    let Ok(mut file) = fs::File::open(path) else {
        return false;
    };
    let mut header = [0_u8; 52];
    if file.read_exact(&mut header).is_err()
        || &header[..4] != b"\x7fELF"
        || header[4] != 2
        || header[5] != 1
    {
        return false;
    }
    let machine = u16::from_le_bytes([header[18], header[19]]);
    let flags = u32::from_le_bytes([header[48], header[49], header[50], header[51]]);
    machine == 258 && flags & 0x7 == expected_abi_flags
}

fn default_std_path(global: &Global) -> Option<String> {
    if let Some(Ok(root)) = &global.dep.resolved_std_root {
        return Some(root.path().to_string_lossy().into_owned());
    }
    utils::paths::std_root_dir().map(|path| path.to_string_lossy().into_owned())
}

pub fn print_usage() {
    println!(
        "\n{} {}",
        "Usage:".color("255,71,71"),
        "wavec [global-options] <command> [command-options]"
    );
}

pub fn print_version() {
    print_version_for_backend(false);
}

fn print_version_for_backend(whale: bool) {
    let os = format!("({})", get_os_pretty_name()).color("117,117,117");

    println!("{} {} {}", "wavec".color("2,161,47"), version::version().color("2,161,47"), os);

    if whale {
        println!("  backend: Whale IR (experimental, Linux amd64, O0)");
    } else if let Some(backend) = llvm::backend() {
        println!("  backend: {}", backend.color("117,117,117"));
    } else {
        println!("{}", "  backend: unknown backend".color("117,117,117"));
    }
}

pub fn print_help() {
    println!("{}", "Wave Compiler".color("145,161,2"));
    print_usage();

    println!("\nCommands:");
    println!(
        "  {:<22} {}",
        "build <input...>".color("38,139,235"),
        "Build/check/link/run pipeline (flag-driven)"
    );
    println!("  {:<22} {}", "check <file>".color("38,139,235"), "Alias: build <file> --emit=check");
    println!(
        "  {:<22} {}",
        "run <file>".color("38,139,235"),
        "Alias: build <file> --run (supports `-- <args...>`)"
    );
    println!(
        "  {:<22} {}",
        "print <item>".color("38,139,235"),
        "Print compiler/toolchain capability item"
    );
    println!(
        "  {:<22} {}",
        "install std".color("38,139,235"),
        "Install compiler-pinned std; --ref <commit-or-ref> overrides with compatibility validation"
    );
    println!(
        "  {:<22} {}",
        "update std".color("38,139,235"),
        "Restore compiler-pinned std; --ref <commit-or-ref> selects a compatible revision"
    );
    println!("  {:<22} {}", "--version".color("38,139,235"), "Show version");
    println!("  {:<22} {}", "--help".color("38,139,235"), "Show help");

    println!("\nBackend selection:");
    println!(
        "  --whale                  Experimental Whale IR (Linux amd64, O0); LLVM is the default"
    );
    println!("\nBuild options:");
    println!(
        "  {:<24} {}",
        "--ast-format=<format>".color("38,139,235"),
        "wson (default), json, sexpr for --emit=ast"
    );
    println!(
        "  {:<24} {}",
        "--emit=<kinds>".color("38,139,235"),
        "check, ast, ir, bc, asm, obj, bin (check must be alone)"
    );
    println!(
        "  {:<24} {}",
        "--input-type=<kind>".color("38,139,235"),
        "wave, ir, bc, asm, obj, archive (forced type for all inputs)"
    );
    println!(
        "  {:<24} {}",
        "--link-only".color("38,139,235"),
        "Link object inputs only (requires emit=bin)"
    );
    println!(
        "  {:<24} {}",
        "--run".color("38,139,235"),
        "Run linked binary (requires emit includes bin)"
    );
    println!(
        "  {:<24} {}",
        "-- <args...>".color("38,139,235"),
        "Forward run-time arguments to executable (with --run)"
    );
    println!(
        "  {:<24} {}",
        "--freestanding".color("38,139,235"),
        "Kernel/OS-style link defaults (no default libc/libm)"
    );
    println!(
        "  {:<24} {}",
        "--entry <symbol>".color("38,139,235"),
        "Set linker entry symbol (link stage only)"
    );
    println!(
        "  {:<24} {}",
        "--linker-script <path>".color("38,139,235"),
        "Pass linker script path via -Wl,-T,<path>"
    );
    println!(
        "  {:<24} {}",
        "--no-start-files".color("38,139,235"),
        "Pass -nostartfiles to linker (link stage only)"
    );
    println!("  {:<24} {}", "-o <file>".color("38,139,235"), "Output file");
    println!(
        "  {:<24} {}",
        "--out-dir <dir>".color("38,139,235"),
        "Output directory for emitted artifacts"
    );
    println!(
        "  {:<24} {}",
        "--target-dir <dir>".color("38,139,235"),
        "Intermediate/default artifact root"
    );
    println!("  {:<24} {}", "--dry-run".color("38,139,235"), "Plan only, no compile/link/exec");
    println!("  {:<24} {}", "--error-format=...".color("38,139,235"), "human, json");

    println!("\nLink mode options:");
    println!(
        "  {:<24} {}",
        "--shared".color("38,139,235"),
        "Build shared output (conflicts with --run)"
    );
    println!("  {:<24} {}", "--static".color("38,139,235"), "Request static link mode");
    println!("  {:<24} {}", "--pie".color("38,139,235"), "Enable PIE mode");
    println!("  {:<24} {}", "--no-pie".color("38,139,235"), "Disable PIE mode");

    println!("\nGlobal options:");
    println!("  {:<24} {}", "-O0..-O3/-Os/-Oz/-Ofast".color("38,139,235"), "Optimization level");
    println!("  {:<24} {}", "--debug-wave=...".color("38,139,235"), "tokens,ast,ir,mc,hex,all");
    println!("  {:<24} {}", "--link=<lib>".color("38,139,235"), "Link library");
    println!("  {:<24} {}", "-L <path>".color("38,139,235"), "Library search path");
    println!(
        "  {:<24} {}",
        "--std-root=<path>".color("38,139,235"),
        "Explicit standard-library root (no fallback)"
    );
    println!("  {:<24} {}", "--dep-root=<path>".color("38,139,235"), "Dependency root directory");
    println!(
        "  {:<24} {}",
        "--dep=<name>=<path>".color("38,139,235"),
        "Explicit dependency mapping"
    );

    println!("\nLLVM/backend options:");
    println!("  {:<24} {}", "--target=<triple>".color("38,139,235"), "Target triple");
    println!("  {:<24} {}", "--cpu=<name>".color("38,139,235"), "Target CPU");
    println!("  {:<24} {}", "--features=<csv>".color("38,139,235"), "Target features");
    println!("  {:<24} {}", "--abi=<name>".color("38,139,235"), "Target ABI");
    println!(
        "  {:<24} {}",
        "--sysroot=<path>".color("38,139,235"),
        "Override the detected target sysroot"
    );
    println!(
        "  {:<24} {}",
        "-C linker=<path>".color("38,139,235"),
        "Override linker executable (default: bundled LLD)"
    );
    println!("  {:<24} {}", "-C link-arg=<arg>".color("38,139,235"), "Append raw linker argument");
    println!(
        "  {:<24} {}",
        "-C link-sysroot=<path>".color("38,139,235"),
        "Set linker sysroot as --sysroot=<path>"
    );
    println!(
        "  {:<24} {}",
        "-C relocation-model=<m>".color("38,139,235"),
        "relocation model for compatibility checks"
    );
    println!("  {:<24} {}", "-C no-default-libs".color("38,139,235"), "Disable automatic -lc -lm");

    println!("\nPrint items:");
    println!("  {:<24} {}", "target-spec".color("38,139,235"), "Target metadata for build tools");
    println!("  {:<24} {}", "supported-targets".color("38,139,235"), "Supported target triples");
    println!(
        "  {:<24} {}",
        "supported-input-types".color("38,139,235"),
        "Input kinds accepted by build"
    );
    println!(
        "  {:<24} {}",
        "supported-emit-kinds".color("38,139,235"),
        "Emit kinds accepted by build"
    );
    println!(
        "  {:<24} {}",
        "--format=json".color("38,139,235"),
        "Machine-readable print output for Vex/tooling"
    );
}

#[cfg(all(test, any(feature = "llvm-target-riscv", feature = "llvm-target-loongarch")))]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU64, Ordering};

    static NEXT_SYSROOT_CASE: AtomicU64 = AtomicU64::new(0);

    fn temp_sysroot(name: &str) -> PathBuf {
        let sequence = NEXT_SYSROOT_CASE.fetch_add(1, Ordering::Relaxed);
        let root =
            env::temp_dir().join(format!("wavec-sysroot-{}-{}-{}", name, process::id(), sequence));
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(root.join("lib")).unwrap();
        root
    }

    fn write_elf64(path: &Path, machine: u16, flags: u32) {
        let mut header = [0_u8; 64];
        header[..4].copy_from_slice(b"\x7fELF");
        header[4] = 2;
        header[5] = 1;
        header[6] = 1;
        header[16..18].copy_from_slice(&3_u16.to_le_bytes());
        header[18..20].copy_from_slice(&machine.to_le_bytes());
        header[48..52].copy_from_slice(&flags.to_le_bytes());
        fs::write(path, header).unwrap();
    }

    fn populate_lp64d_runtime(root: &Path, loader: &str, machine: u16, flags: u32) {
        let lib = root.join("lib");
        fs::write(lib.join("libc.so"), "GROUP ( libc.so.6 )\n").unwrap();
        fs::write(lib.join("libm.so"), "GROUP ( libm.so.6 )\n").unwrap();
        write_elf64(&lib.join("libc.so.6"), machine, flags);
        write_elf64(&lib.join("libm.so.6"), machine, flags);
        write_elf64(&lib.join(loader), machine, flags);
    }

    #[cfg(feature = "llvm-target-riscv")]
    #[test]
    fn std_commands_accept_only_one_explicit_reference() {
        let args = |items: &[&str]| items.iter().map(|s| s.to_string()).collect::<Vec<_>>();
        assert_eq!(parse_std_reference(&args(&["std"])).unwrap(), None);
        for input in [vec!["std", "--ref", "release"], vec!["std", "--ref=release"]] {
            assert_eq!(parse_std_reference(&args(&input)).unwrap().as_deref(), Some("release"));
        }
        for input in [
            vec![],
            vec!["std", "--ref"],
            vec!["std", "--ref="],
            vec!["std", "--ref", "--all"],
            vec!["std", "--ref=a", "--ref=b"],
            vec!["other"],
        ] {
            assert!(parse_std_reference(&args(&input)).is_err());
        }
    }

    #[test]
    fn riscv64_sysroot_selection_skips_incomplete_and_foreign_runtimes() {
        let incomplete = temp_sysroot("incomplete");
        let foreign = temp_sysroot("foreign");
        let complete = temp_sysroot("complete");
        populate_lp64d_runtime(&foreign, "ld-linux-riscv64-lp64d.so.1", 62, 0x4);
        populate_lp64d_runtime(&complete, "ld-linux-riscv64-lp64d.so.1", 243, 0x4);

        let selected = select_riscv64_linux_sysroot(
            "riscv64-unknown-linux-gnu",
            Some("lp64d"),
            [
                (incomplete.clone(), "incomplete".to_string()),
                (foreign.clone(), "foreign".to_string()),
                (complete.clone(), "cross-gcc".to_string()),
            ],
        )
        .unwrap();

        assert_eq!(Path::new(&selected.path), fs::canonicalize(&complete).unwrap());
        assert_eq!(selected.source, "cross-gcc");
        let _ = fs::remove_dir_all(incomplete);
        let _ = fs::remove_dir_all(foreign);
        let _ = fs::remove_dir_all(complete);
    }

    #[cfg(feature = "llvm-target-riscv")]
    #[test]
    fn riscv64_sysroot_selection_requires_the_effective_float_abi() {
        let root = temp_sysroot("abi-mismatch");
        populate_lp64d_runtime(&root, "ld-linux-riscv64-lp64d.so.1", 243, 0x4);

        assert!(select_riscv64_linux_sysroot(
            "riscv64-unknown-linux-gnu",
            Some("lp64"),
            [(root.clone(), "candidate".to_string())],
        )
        .is_none());
        let _ = fs::remove_dir_all(root);
    }

    #[cfg(feature = "llvm-target-loongarch")]
    #[test]
    fn loongarch64_sysroot_selection_requires_a_complete_lp64d_runtime() {
        let foreign = temp_sysroot("loong-foreign");
        let incompatible = temp_sysroot("loong-incompatible");
        let complete = temp_sysroot("loong-complete");
        let soft = temp_sysroot("loong-soft");
        populate_lp64d_runtime(&foreign, "ld-linux-loongarch-lp64d.so.1", 62, 0x3);
        populate_lp64d_runtime(&incompatible, "ld-linux-loongarch-lp64d.so.1", 258, 0x1);
        populate_lp64d_runtime(&complete, "ld-linux-loongarch-lp64d.so.1", 258, 0x43);
        populate_lp64d_runtime(&soft, "ld-linux-loongarch-lp64s.so.1", 258, 0x41);

        let selected = select_loongarch64_linux_sysroot(
            "loongarch64-unknown-linux-gnu",
            Some("lp64d"),
            [
                (foreign.clone(), "foreign".to_string()),
                (incompatible.clone(), "incompatible".to_string()),
                (complete.clone(), "cross-gcc".to_string()),
            ],
        )
        .unwrap();
        assert_eq!(Path::new(&selected.path), fs::canonicalize(&complete).unwrap());
        assert_eq!(selected.source, "cross-gcc");

        let selected_soft = select_loongarch64_linux_sysroot(
            "loongarch64-unknown-linux-gnu",
            Some("lp64s"),
            [(soft.clone(), "soft-runtime".to_string())],
        )
        .unwrap();
        assert_eq!(selected_soft.source, "soft-runtime");

        // glibc has no standardized LP64F loader/runtime configuration yet.
        assert!(select_loongarch64_linux_sysroot(
            "loongarch64-unknown-linux-gnu",
            Some("lp64f"),
            [(soft.clone(), "single-runtime".to_string())],
        )
        .is_none());

        let _ = fs::remove_dir_all(foreign);
        let _ = fs::remove_dir_all(incompatible);
        let _ = fs::remove_dir_all(complete);
        let _ = fs::remove_dir_all(soft);
    }
}
