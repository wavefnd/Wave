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

//! Construction and emission of a complete LLVM module.
//!
//! This is the backend assembly point: it consumes the frontend's typed HIR,
//! lowers C ABI boundaries, emits functions, and applies the selected optimization pipeline. Target
//! initialization is process-wide, while each compilation receives its own LLVM
//! context and module.

use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::context::Context;
use inkwell::module::{FlagBehavior, Linkage, Module};
use inkwell::passes::PassBuilderOptions;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue, ValueKind,
};
use inkwell::OptimizationLevel;

use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetData, TargetMachine,
    TargetMachineOptions, TargetTriple,
};
use parser::ast::{
    ASTNode, EnumNode, ExternFunctionNode, FunctionNode, Mutability, VariableNode, WaveType,
};
use parser::hir::TypedProgram;
use std::collections::HashMap;
use std::sync::Once;

use crate::backend::BackendOptions;
use crate::codegen::target::{
    llvm_triple_for_abi, require_supported_target_from_triple, CodegenTarget,
};
use crate::statement::generate_statement_ir;

use super::consts::{create_llvm_const_value, ConstEvalError};
use super::types::{wave_type_to_llvm_type, TypeFlavor, VariableInfo};
use super::variants::{declare_variant_types, define_variant_types};

use crate::codegen::abi_c::{
    apply_extern_c_attrs, lower_extern_c, AbiPart, ExternCInfo, ParamLowering, RetLowering,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CodegenFileKind {
    Bitcode,
    Assembly,
    Object,
}

struct GeneratedModule {
    module: &'static Module<'static>,
    target_machine: TargetMachine,
}

struct FunctionCodegenEntry<'a> {
    symbol: String,
    node: &'a FunctionNode,
}

struct ExportCWrapper<'ctx> {
    wrapper: FunctionValue<'ctx>,
    implementation: FunctionValue<'ctx>,
    info: ExternCInfo<'ctx>,
    wave_param_types: Vec<BasicTypeEnum<'ctx>>,
    wave_ret_type: Option<BasicTypeEnum<'ctx>>,
}

fn reinterpret_abi_value<'ctx>(
    context: &'ctx Context,
    builder: &inkwell::builder::Builder<'ctx>,
    td: &TargetData,
    value: BasicValueEnum<'ctx>,
    target: BasicTypeEnum<'ctx>,
    tag: &str,
) -> BasicValueEnum<'ctx> {
    if value.get_type() == target {
        return value;
    }

    let source = value.get_type();
    let source_size = td.get_store_size(&source);
    let target_size = td.get_store_size(&target);
    let source_is_aggregate = matches!(
        source,
        BasicTypeEnum::ArrayType(_) | BasicTypeEnum::StructType(_)
    );
    let target_is_aggregate = matches!(
        target,
        BasicTypeEnum::ArrayType(_) | BasicTypeEnum::StructType(_)
    );
    if source_size != target_size && !(source_is_aggregate || target_is_aggregate) {
        panic!(
            "cannot reinterpret C ABI value '{}' from {} bytes to {} bytes",
            tag, source_size, target_size
        );
    }

    let source_ptr = builder
        .build_alloca(source, &format!("{}_source", tag))
        .unwrap();
    builder.build_store(source_ptr, value).unwrap();
    let target_ptr = builder
        .build_alloca(target, &format!("{}_target", tag))
        .unwrap();
    builder
        .build_store(target_ptr, target.const_zero())
        .unwrap();
    let size = context
        .i64_type()
        .const_int(source_size.min(target_size), false);
    builder
        .build_memcpy(
            target_ptr,
            td.get_abi_alignment(&target),
            source_ptr,
            td.get_abi_alignment(&source),
            size,
        )
        .unwrap();
    builder
        .build_load(target, target_ptr, &format!("{}_load", tag))
        .unwrap()
        .as_basic_value_enum()
}

fn rebuild_split_abi_value<'ctx>(
    context: &'ctx Context,
    builder: &inkwell::builder::Builder<'ctx>,
    td: &TargetData,
    parts: &[BasicValueEnum<'ctx>],
    target: BasicTypeEnum<'ctx>,
    tag: &str,
) -> BasicValueEnum<'ctx> {
    let target_ptr = builder
        .build_alloca(target, &format!("{}_target", tag))
        .unwrap();
    let target_size = td.get_store_size(&target);
    builder
        .build_store(target_ptr, target.const_zero())
        .unwrap();
    let mut offset = 0u64;

    for (index, part) in parts.iter().enumerate() {
        let part_type = part.get_type();
        let part_size = td.get_store_size(&part_type);
        let part_ptr = builder
            .build_alloca(part_type, &format!("{}_part_{}", tag, index))
            .unwrap();
        builder.build_store(part_ptr, *part).unwrap();
        let offset_value = context.i64_type().const_int(offset, false);
        // SAFETY: `target_ptr` is an opaque pointer to a live stack allocation;
        // using `i8` makes this a byte offset. When the offset reaches or passes
        // the allocation size, `copy_size` is zero and the pointer is not used
        // for a memory access.
        let destination = unsafe {
            builder
                .build_gep(
                    context.i8_type(),
                    target_ptr,
                    &[offset_value],
                    &format!("{}_offset_{}", tag, index),
                )
                .unwrap()
        };
        let copy_size = part_size.min(target_size.saturating_sub(offset));
        if copy_size > 0 {
            builder
                .build_memcpy(
                    destination,
                    1,
                    part_ptr,
                    td.get_abi_alignment(&part_type),
                    context.i64_type().const_int(copy_size, false),
                )
                .unwrap();
        }
        offset += part_size;
    }
    builder
        .build_load(target, target_ptr, &format!("{}_load", tag))
        .unwrap()
        .as_basic_value_enum()
}

fn rebuild_expanded_abi_value<'ctx>(
    context: &'ctx Context,
    builder: &inkwell::builder::Builder<'ctx>,
    td: &TargetData,
    parts: &[AbiPart<'ctx>],
    values: &[BasicValueEnum<'ctx>],
    target: BasicTypeEnum<'ctx>,
    tag: &str,
) -> BasicValueEnum<'ctx> {
    assert_eq!(parts.len(), values.len());
    let target_size = td.get_store_size(&target);
    let target_ptr = builder
        .build_alloca(target, &format!("{tag}_target"))
        .unwrap();
    builder
        .build_store(target_ptr, target.const_zero())
        .unwrap();

    for (index, (part, value)) in parts.iter().zip(values).enumerate() {
        let part_size = td.get_store_size(&part.ty);
        assert!(
            part.offset + part_size <= target_size,
            "LoongArch ABI part extends past its aggregate"
        );
        let part_ptr = builder
            .build_alloca(part.ty, &format!("{tag}_part_{index}"))
            .unwrap();
        builder.build_store(part_ptr, *value).unwrap();
        let offset = context.i64_type().const_int(part.offset, false);
        // SAFETY: the classifier obtained this byte offset from TargetData and
        // the bounds assertion above keeps the copy inside the allocation.
        let destination = unsafe {
            builder
                .build_gep(
                    context.i8_type(),
                    target_ptr,
                    &[offset],
                    &format!("{tag}_offset_{index}"),
                )
                .unwrap()
        };
        builder
            .build_memcpy(
                destination,
                1,
                part_ptr,
                td.get_abi_alignment(&part.ty),
                context.i64_type().const_int(part_size, false),
            )
            .unwrap();
    }

    builder
        .build_load(target, target_ptr, &format!("{tag}_load"))
        .unwrap()
        .as_basic_value_enum()
}

fn build_export_c_wrapper<'ctx>(
    context: &'ctx Context,
    builder: &inkwell::builder::Builder<'ctx>,
    td: &TargetData,
    export: &ExportCWrapper<'ctx>,
) {
    let entry = context.append_basic_block(export.wrapper, "entry");
    builder.position_at_end(entry);

    let mut llvm_index = 0u32;
    let sret_ptr: Option<PointerValue<'ctx>> =
        if matches!(export.info.ret, RetLowering::SRet { .. }) {
            let pointer = export
                .wrapper
                .get_nth_param(0)
                .expect("C ABI sret wrapper requires a hidden pointer")
                .into_pointer_value();
            llvm_index += 1;
            Some(pointer)
        } else {
            None
        };

    let mut implementation_args = Vec::<BasicMetadataValueEnum<'ctx>>::new();
    for (wave_index, (lowering, wave_type)) in export
        .info
        .params
        .iter()
        .zip(export.wave_param_types.iter())
        .enumerate()
    {
        let value = match lowering {
            ParamLowering::Ignore => wave_type.const_zero(),
            ParamLowering::Direct(_) => {
                let incoming = export
                    .wrapper
                    .get_nth_param(llvm_index)
                    .expect("missing direct C ABI wrapper argument");
                llvm_index += 1;
                reinterpret_abi_value(
                    context,
                    builder,
                    td,
                    incoming,
                    *wave_type,
                    &format!("export_arg_{}", wave_index),
                )
            }
            ParamLowering::Indirect { .. } | ParamLowering::ByVal { .. } => {
                let pointer = export
                    .wrapper
                    .get_nth_param(llvm_index)
                    .expect("missing indirect C ABI wrapper argument")
                    .into_pointer_value();
                llvm_index += 1;
                builder
                    .build_load(*wave_type, pointer, &format!("export_byval_{}", wave_index))
                    .unwrap()
                    .as_basic_value_enum()
            }
            ParamLowering::Split(parts) => {
                let mut incoming = Vec::with_capacity(parts.len());
                for _ in parts {
                    incoming.push(
                        export
                            .wrapper
                            .get_nth_param(llvm_index)
                            .expect("missing split C ABI wrapper argument"),
                    );
                    llvm_index += 1;
                }
                rebuild_split_abi_value(
                    context,
                    builder,
                    td,
                    &incoming,
                    *wave_type,
                    &format!("export_split_{}", wave_index),
                )
            }
            ParamLowering::CoerceAndExpand(parts) => {
                let mut incoming = Vec::with_capacity(parts.len());
                for _ in parts {
                    incoming.push(
                        export
                            .wrapper
                            .get_nth_param(llvm_index)
                            .expect("missing expanded C ABI wrapper argument"),
                    );
                    llvm_index += 1;
                }
                rebuild_expanded_abi_value(
                    context,
                    builder,
                    td,
                    parts,
                    &incoming,
                    *wave_type,
                    &format!("export_loong_expand_{}", wave_index),
                )
            }
        };
        implementation_args.push(value.into());
    }

    let call = builder
        .build_call(
            export.implementation,
            &implementation_args,
            "export_implementation",
        )
        .unwrap();

    match &export.info.ret {
        RetLowering::Void => {
            builder.build_return(None).unwrap();
        }
        RetLowering::SRet { .. } => {
            let value = match call.try_as_basic_value() {
                ValueKind::Basic(value) => value,
                ValueKind::Instruction(_) => {
                    panic!("C ABI sret wrapper implementation returned void")
                }
            };
            builder
                .build_store(sret_ptr.expect("missing C ABI sret pointer"), value)
                .unwrap();
            builder.build_return(None).unwrap();
        }
        RetLowering::Direct(lowered_type) => {
            let value = match call.try_as_basic_value() {
                ValueKind::Basic(value) => value,
                ValueKind::Instruction(_) => {
                    panic!("C ABI direct wrapper implementation returned void")
                }
            };
            let wave_type = export
                .wave_ret_type
                .expect("direct C ABI wrapper requires a Wave return type");
            if value.get_type() != wave_type {
                panic!("C ABI wrapper implementation return type changed unexpectedly");
            }
            let lowered =
                reinterpret_abi_value(context, builder, td, value, *lowered_type, "export_return");
            builder.build_return(Some(&lowered)).unwrap();
        }
    }
}

fn is_implicit_i32_main(name: &str, return_type: &Option<WaveType>) -> bool {
    name == "main" && matches!(return_type, None | Some(WaveType::Void))
}

fn is_supported_extern_abi(abi: &str, target: CodegenTarget) -> bool {
    match target {
        CodegenTarget::WindowsX86_64Gnu | CodegenTarget::WindowsArm64Gnu => {
            abi.eq_ignore_ascii_case("c") || abi.eq_ignore_ascii_case("system")
        }
        _ => abi.eq_ignore_ascii_case("c"),
    }
}

fn supported_extern_abi_description(target: CodegenTarget) -> &'static str {
    match target {
        CodegenTarget::WindowsX86_64Gnu | CodegenTarget::WindowsArm64Gnu => "'c' and 'system'",
        _ => "'c'; Windows 'system' is accepted only on Windows GNU targets",
    }
}

fn normalize_opt_flag_for_passes(opt_flag: &str) -> &str {
    match opt_flag {
        // LLVM's pass pipeline has no dedicated Ofast preset; keep it aligned with codegen tools.
        "-Ofast" => "-O3",
        other => other,
    }
}

fn target_opt_level_from_flag(opt_flag: &str) -> OptimizationLevel {
    match normalize_opt_flag_for_passes(opt_flag) {
        "" | "-O0" => OptimizationLevel::None,
        "-O1" => OptimizationLevel::Less,
        "-O2" | "-Os" | "-Oz" => OptimizationLevel::Default,
        "-O3" => OptimizationLevel::Aggressive,
        other => panic!("unknown opt flag for target machine: {}", other),
    }
}

fn code_model_from_backend(backend: &BackendOptions, target: CodegenTarget) -> CodeModel {
    if let Some(model) = backend.code_model.as_deref() {
        return match model {
            "default" => CodeModel::Default,
            "jitdefault" | "jit-default" => CodeModel::JITDefault,
            "small" => CodeModel::Small,
            "kernel" => CodeModel::Kernel,
            "medium" => CodeModel::Medium,
            "large" => CodeModel::Large,
            other => panic!("unsupported -C code-model={}", other),
        };
    }

    match target {
        CodegenTarget::FreestandingX86_64 => CodeModel::Kernel,
        _ => CodeModel::Default,
    }
}

fn reloc_mode_from_backend(backend: &BackendOptions, target: CodegenTarget) -> RelocMode {
    if let Some(model) = backend.relocation_model.as_deref() {
        return match model {
            "default" => RelocMode::Default,
            "static" => RelocMode::Static,
            "pic" | "pie" => RelocMode::PIC,
            "dynamic-no-pic" | "dynamic_no_pic" => RelocMode::DynamicNoPic,
            other => panic!("unsupported -C relocation-model={}", other),
        };
    }

    if backend.freestanding
        || matches!(
            target,
            CodegenTarget::FreestandingX86_64
                | CodegenTarget::FreestandingArm64
                | CodegenTarget::FreestandingRISCV64
        )
    {
        RelocMode::Static
    } else {
        RelocMode::Default
    }
}

static INIT_LLVM_TARGETS: Once = Once::new();

fn codegen_trace(step: &str) {
    if std::env::var_os("WAVE_CODEGEN_TRACE").is_some() {
        eprintln!("[wavec-codegen] {}", step);
    }
}

fn initialize_llvm_targets() {
    INIT_LLVM_TARGETS.call_once(|| {
        let config = InitializationConfig::default();

        #[cfg(feature = "llvm-target-all")]
        {
            Target::initialize_all(&config);
        }

        #[cfg(all(not(feature = "llvm-target-all"), feature = "llvm-target-x86"))]
        {
            Target::initialize_x86(&config);
        }

        #[cfg(all(not(feature = "llvm-target-all"), feature = "llvm-target-aarch64"))]
        {
            Target::initialize_aarch64(&config);
        }

        #[cfg(all(not(feature = "llvm-target-all"), feature = "llvm-target-riscv"))]
        {
            Target::initialize_riscv(&config);
        }

        #[cfg(all(not(feature = "llvm-target-all"), feature = "llvm-target-loongarch"))]
        {
            Target::initialize_loongarch(&config);
        }

        #[cfg(all(not(feature = "llvm-target-all"), feature = "llvm-target-wasm"))]
        {
            Target::initialize_webassembly(&config);
        }
    });
}

fn should_run_llvm_pass_pipeline() -> bool {
    // LLVM 21's C pass pipeline can jump through a null callback in the
    // MinGW-built Windows package. Code generation still uses the target
    // machine's optimization level, so keep Windows codegen usable by skipping
    // the in-process IR pass pipeline there.
    !cfg!(target_os = "windows")
}

fn should_disable_red_zone(backend: &BackendOptions, target: CodegenTarget) -> bool {
    backend.freestanding
        || matches!(
            target,
            CodegenTarget::FreestandingX86_64
                | CodegenTarget::FreestandingArm64
                | CodegenTarget::FreestandingRISCV64
        )
}

fn apply_function_codegen_attrs<'ctx>(
    context: &'ctx Context,
    function: FunctionValue<'ctx>,
    disable_red_zone: bool,
    cpu: &str,
    features: &str,
) {
    if !cpu.is_empty() {
        function.add_attribute(
            AttributeLoc::Function,
            context.create_string_attribute("target-cpu", cpu),
        );
    }
    if !features.is_empty() {
        function.add_attribute(
            AttributeLoc::Function,
            context.create_string_attribute("target-features", features),
        );
    }
    if disable_red_zone {
        let no_red_zone = Attribute::get_named_enum_kind_id("noredzone");
        let attr = context.create_enum_attribute(no_red_zone, 0);
        function.add_attribute(AttributeLoc::Function, attr);

        let no_unwind = Attribute::get_named_enum_kind_id("nounwind");
        let attr = context.create_enum_attribute(no_unwind, 0);
        function.add_attribute(AttributeLoc::Function, attr);
    }
}

fn apply_wasm_import_attrs<'ctx>(
    context: &'ctx Context,
    function: FunctionValue<'ctx>,
    target: CodegenTarget,
    import_name: &str,
) {
    if import_name.starts_with("llvm.wasm.") {
        return;
    }
    let module = match target {
        CodegenTarget::Wasm32Unknown | CodegenTarget::Wasm64Unknown => "env",
        CodegenTarget::Wasm32WasiP1 => "wasi_snapshot_preview1",
        _ => return,
    };
    function.add_attribute(
        AttributeLoc::Function,
        context.create_string_attribute("wasm-import-module", module),
    );
    function.add_attribute(
        AttributeLoc::Function,
        context.create_string_attribute("wasm-import-name", import_name),
    );
}

fn apply_wasm_export_attr<'ctx>(
    context: &'ctx Context,
    function: FunctionValue<'ctx>,
    target: CodegenTarget,
    export_name: &str,
) {
    if !matches!(
        target,
        CodegenTarget::Wasm32Unknown | CodegenTarget::Wasm32WasiP1 | CodegenTarget::Wasm64Unknown
    ) {
        return;
    }
    function.add_attribute(
        AttributeLoc::Function,
        context.create_string_attribute("wasm-export-name", export_name),
    );
}

fn build_wasi_start_wrapper<'ctx>(
    context: &'ctx Context,
    builder: &inkwell::builder::Builder<'ctx>,
    module: &Module<'ctx>,
    target: CodegenTarget,
) {
    if target != CodegenTarget::Wasm32WasiP1 {
        return;
    }
    let Some(main) = module.get_function("main") else {
        return;
    };
    if main.count_params() != 0
        || main.get_type().get_return_type() != Some(context.i32_type().into())
    {
        panic!("wasm32-wasip1 requires 'main' to take no parameters and return i32 or omit its return type");
    }
    if module.get_function("_start").is_some() {
        panic!("wasm32-wasip1 reserves '_start' for its command entry point");
    }

    let exit_type = context
        .void_type()
        .fn_type(&[context.i32_type().into()], false);
    let proc_exit = module.add_function("__wasi_proc_exit", exit_type, None);
    apply_wasm_import_attrs(context, proc_exit, target, "proc_exit");

    let start = module.add_function("_start", context.void_type().fn_type(&[], false), None);
    apply_wasm_export_attr(context, start, target, "_start");
    let block = context.append_basic_block(start, "entry");
    builder.position_at_end(block);
    let call = builder.build_call(main, &[], "main_status").unwrap();
    let status = match call.try_as_basic_value() {
        ValueKind::Basic(value) => value.into_int_value(),
        ValueKind::Instruction(_) => unreachable!("validated WASI main returned void"),
    };
    builder.build_call(proc_exit, &[status.into()], "").unwrap();
    builder.build_unreachable().unwrap();
}

/// Builds an LLVM module and returns its textual representation.
///
/// # Safety
///
/// This function retains an unsafe signature for compatibility with the
/// compiler driver's LLVM boundary. It imposes no additional caller-side
/// memory-safety requirements.
pub unsafe fn generate_ir(
    program: &TypedProgram,
    opt_flag: &str,
    backend: &BackendOptions,
) -> String {
    let generated = build_module(program, opt_flag, backend);
    generated.module.print_to_string().to_string()
}

/// Builds a module and emits one target-machine output file.
///
/// # Safety
///
/// This function retains an unsafe signature for compatibility with the
/// compiler driver's LLVM boundary. It imposes no additional caller-side
/// memory-safety requirements.
pub unsafe fn emit_codegen_file(
    program: &TypedProgram,
    opt_flag: &str,
    backend: &BackendOptions,
    output: &std::path::Path,
    kind: CodegenFileKind,
) {
    let generated = build_module(program, opt_flag, backend);

    match kind {
        CodegenFileKind::Bitcode => {
            if !generated.module.write_bitcode_to_path(output) {
                panic!("failed to write LLVM bitcode to '{}'", output.display());
            }
        }
        CodegenFileKind::Assembly => generated
            .target_machine
            .write_to_file(generated.module, FileType::Assembly, output)
            .unwrap_or_else(|e| {
                panic!(
                    "failed to emit LLVM assembly to '{}': {}",
                    output.display(),
                    e.to_string()
                )
            }),
        CodegenFileKind::Object => generated
            .target_machine
            .write_to_file(generated.module, FileType::Object, output)
            .unwrap_or_else(|e| {
                panic!(
                    "failed to emit object file to '{}': {}",
                    output.display(),
                    e.to_string()
                )
            }),
    }
}

fn build_module(
    program: &TypedProgram,
    opt_flag: &str,
    backend: &BackendOptions,
) -> GeneratedModule {
    let ast_nodes = program.syntax();
    codegen_trace("initialize targets");
    initialize_llvm_targets();

    // Inkwell ties every module and builder to its context through lifetimes.
    // GeneratedModule crosses this function boundary, so these allocations live
    // for the compiler process. The CLI is short-lived and never calls LLVM
    // shutdown; a long-lived embedding API should replace this with an owned
    // compilation-session object rather than copying this pattern.
    codegen_trace("create context");
    let context: &'static Context = Box::leak(Box::new(Context::create()));
    codegen_trace("create module");
    let module: &'static _ = Box::leak(Box::new(context.create_module("main")));
    codegen_trace("create builder");
    let builder: &'static _ = Box::leak(Box::new(context.create_builder()));

    codegen_trace("resolve target triple");
    let requested_triple = if let Some(raw) = &backend.target {
        TargetTriple::create(raw)
    } else {
        TargetMachine::get_default_triple()
    };
    let abi_target = require_supported_target_from_triple(&requested_triple);
    let triple = TargetTriple::create(&llvm_triple_for_abi(
        requested_triple.as_str().to_str().unwrap_or_default(),
        backend.abi.as_deref(),
    ));
    let disable_red_zone = should_disable_red_zone(backend, abi_target);
    codegen_trace("lookup target");
    let target = Target::from_triple(&triple).unwrap();
    let cpu = backend.cpu.as_deref().unwrap_or("generic");
    let features = backend.features.as_deref().unwrap_or("");
    let reloc_mode = reloc_mode_from_backend(backend, abi_target);
    let code_model = code_model_from_backend(backend, abi_target);

    codegen_trace("create target machine");
    let mut target_options = TargetMachineOptions::new()
        .set_cpu(cpu)
        .set_features(features)
        .set_level(target_opt_level_from_flag(opt_flag))
        .set_reloc_mode(reloc_mode)
        .set_code_model(code_model);
    if abi_target.architecture() != super::arch::Architecture::LoongArch64 {
        if let Some(abi) = backend.abi.as_deref() {
            target_options = target_options.set_abi(abi);
        }
    }
    let tm = target
        .create_target_machine_from_options(&triple, target_options)
        .unwrap();

    codegen_trace("set target metadata");
    module.set_triple(&triple);

    let td_val: TargetData = tm.get_target_data();
    module.set_data_layout(&td_val.get_data_layout());
    if abi_target.architecture() == super::arch::Architecture::Riscv64 {
        if let Some(abi) = backend.abi.as_deref() {
            module.add_metadata_flag(
                "target-abi",
                FlagBehavior::Error,
                context.metadata_string(abi),
            );
        }
        if let Some(isa) = backend.isa.as_deref() {
            let isa_node = context.metadata_node(&[context.metadata_string(isa).into()]);
            module.add_metadata_flag("riscv-isa", FlagBehavior::AppendUnique, isa_node);
        }
    }
    let td: &'static TargetData = Box::leak(Box::new(td_val));

    let mut extern_c_info: HashMap<String, ExternCInfo<'static>> = HashMap::new();

    let mut global_consts: HashMap<String, BasicValueEnum<'static>> = HashMap::new();
    let mut global_statics: HashMap<String, VariableInfo<'static>> = HashMap::new();

    let mut struct_types: HashMap<String, inkwell::types::StructType> = HashMap::new();
    let mut struct_field_indices: HashMap<String, HashMap<String, u32>> = HashMap::new();
    let mut struct_field_types: HashMap<String, HashMap<String, WaveType>> = HashMap::new();
    // (1) struct opaque + field index map
    for ast in ast_nodes {
        if let ASTNode::Struct(struct_node) = ast {
            if !struct_node.generic_params.is_empty() {
                continue;
            }
            let st = context.opaque_struct_type(&struct_node.name);
            struct_types.insert(struct_node.name.clone(), st);

            let mut index_map = HashMap::new();
            let mut type_map = HashMap::new();
            for (i, (field_name, field_ty)) in struct_node.fields.iter().enumerate() {
                index_map.insert(field_name.clone(), i as u32);
                type_map.insert(field_name.clone(), field_ty.clone());
            }
            struct_field_indices.insert(struct_node.name.clone(), index_map);
            struct_field_types.insert(struct_node.name.clone(), type_map);
        }
    }

    let variant_definitions = declare_variant_types(context, program, &mut struct_types);

    for ast in ast_nodes {
        if let ASTNode::Struct(struct_node) = ast {
            if !struct_node.generic_params.is_empty() {
                continue;
            }
            let st = *struct_types
                .get(&struct_node.name)
                .unwrap_or_else(|| panic!("Opaque struct missing: {}", struct_node.name));

            let field_types: Vec<BasicTypeEnum> = struct_node
                .fields
                .iter()
                .map(|(_, ty)| wave_type_to_llvm_type(context, ty, &struct_types, TypeFlavor::AbiC))
                .collect();

            st.set_body(&field_types, false);
        }
    }

    define_variant_types(context, &variant_definitions, &struct_types);

    for ast in ast_nodes {
        if let ASTNode::Enum(e) = ast {
            add_enum_consts_to_globals(context, e, &mut global_consts);
        }
    }

    let mut pending: Vec<&VariableNode> = ast_nodes
        .iter()
        .filter_map(|ast| match ast {
            ASTNode::Variable(v) if v.mutability == Mutability::Const => Some(v),
            _ => None,
        })
        .collect();

    let mut round = 0;
    while !pending.is_empty() {
        round += 1;

        let mut progressed = false;
        let mut next_pending: Vec<&VariableNode> = Vec::new();

        for v in pending {
            let init = v
                .initial_value
                .as_ref()
                .unwrap_or_else(|| panic!("Constant must be initialized: {}", v.name));

            match create_llvm_const_value(
                context,
                &v.type_name,
                init,
                &struct_types,
                &struct_field_indices,
                &global_consts,
                Some(program),
            ) {
                Ok(val) => {
                    global_consts.insert(v.name.clone(), val);
                    progressed = true;
                }
                Err(ConstEvalError::UnknownIdentifier(_)) => {
                    next_pending.push(v);
                }
                Err(e) => {
                    panic!("const '{}' evaluation failed: {}", v.name, e);
                }
            }
        }

        if next_pending.is_empty() {
            break;
        }
        if !progressed {
            let names: Vec<String> = next_pending.iter().map(|v| v.name.clone()).collect();
            panic!(
                "unresolved const cycle or missing symbols after {} rounds: {:?}",
                round, names
            );
        }

        pending = next_pending;
    }

    for ast in ast_nodes {
        let ASTNode::Variable(v) = ast else {
            continue;
        };
        if v.mutability != Mutability::Static {
            continue;
        }

        let llvm_ty =
            wave_type_to_llvm_type(context, &v.type_name, &struct_types, TypeFlavor::AbiC);
        let g = module.add_global(llvm_ty, None, &v.name);

        let init = if let Some(expr) = &v.initial_value {
            create_llvm_const_value(
                context,
                &v.type_name,
                expr,
                &struct_types,
                &struct_field_indices,
                &global_consts,
                Some(program),
            )
            .unwrap_or_else(|e| panic!("static '{}' initialization failed: {}", v.name, e))
        } else {
            llvm_ty.const_zero().as_basic_value_enum()
        };

        g.set_initializer(&init);
        g.set_constant(false);

        global_statics.insert(
            v.name.clone(),
            VariableInfo {
                ptr: g.as_pointer_value(),
                mutability: Mutability::Static,
                ty: v.type_name.clone(),
            },
        );
    }

    let mut functions: HashMap<String, FunctionValue> = HashMap::new();
    let mut export_wrappers: Vec<ExportCWrapper> = Vec::new();

    let mut function_nodes = Vec::new();
    for ast in ast_nodes {
        match ast {
            ASTNode::Function(function) => function_nodes.push(FunctionCodegenEntry {
                symbol: function.name.clone(),
                node: function,
            }),
            ASTNode::ProtoImpl(implementation) => {
                for method in &implementation.methods {
                    function_nodes.push(FunctionCodegenEntry {
                        symbol: format!("{}_{}", implementation.target, method.name),
                        node: method,
                    });
                }
            }
            _ => {}
        }
    }

    let extern_functions: Vec<&ExternFunctionNode> = ast_nodes
        .iter()
        .filter_map(|ast| {
            if let ASTNode::ExternFunction(ext) = ast {
                Some(ext)
            } else {
                None
            }
        })
        .collect();

    for entry in &function_nodes {
        let FunctionNode {
            name,
            parameters,
            return_type,
            export,
            ..
        } = entry.node;
        let symbol = &entry.symbol;
        if let Some(export) = export {
            if !is_supported_extern_abi(&export.abi, abi_target) {
                panic!(
                    "unsupported export ABI '{}' for function '{}' on {}: supported ABIs are {}",
                    export.abi,
                    name,
                    abi_target.desc(),
                    supported_extern_abi_description(abi_target)
                );
            }
        }

        let param_types: Vec<BasicMetadataTypeEnum> = parameters
            .iter()
            .map(|p| {
                wave_type_to_llvm_type(context, &p.param_type, &struct_types, TypeFlavor::AbiC)
                    .into()
            })
            .collect();

        let fn_type = if is_implicit_i32_main(name, return_type) {
            context.i32_type().fn_type(&param_types, false)
        } else {
            match return_type {
                None | Some(WaveType::Void | WaveType::Never) => {
                    context.void_type().fn_type(&param_types, false)
                }
                Some(wave_ret_ty) => {
                    let llvm_ret_type = wave_type_to_llvm_type(
                        context,
                        wave_ret_ty,
                        &struct_types,
                        TypeFlavor::AbiC,
                    );
                    llvm_ret_type.fn_type(&param_types, false)
                }
            }
        };

        if let Some(export) = export {
            let wave_param_types = parameters
                .iter()
                .map(|parameter| {
                    wave_type_to_llvm_type(
                        context,
                        &parameter.param_type,
                        &struct_types,
                        TypeFlavor::AbiC,
                    )
                })
                .collect::<Vec<_>>();
            let wave_ret_type = return_type.as_ref().and_then(|return_type| {
                if matches!(return_type, WaveType::Void | WaveType::Never) {
                    None
                } else {
                    Some(wave_type_to_llvm_type(
                        context,
                        return_type,
                        &struct_types,
                        TypeFlavor::AbiC,
                    ))
                }
            });
            let export_decl = ExternFunctionNode {
                name: name.clone(),
                abi: export.abi.clone(),
                symbol: export.symbol.clone(),
                params: parameters
                    .iter()
                    .map(|parameter| (parameter.name.clone(), parameter.param_type.clone()))
                    .collect(),
                variadic: false,
                return_type: return_type.clone().unwrap_or(WaveType::Void),
            };
            let lowered = lower_extern_c(
                context,
                td,
                abi_target,
                backend.abi.as_deref(),
                &export_decl,
                &struct_types,
            );
            let wrapper = module.add_function(&lowered.llvm_name, lowered.fn_type, None);
            apply_extern_c_attrs(context, wrapper, &lowered.info);
            apply_function_codegen_attrs(context, wrapper, disable_red_zone, cpu, features);
            if matches!(return_type, Some(WaveType::Never)) {
                wrapper.add_attribute(
                    AttributeLoc::Function,
                    context.create_enum_attribute(Attribute::get_named_enum_kind_id("noreturn"), 0),
                );
            }
            apply_wasm_export_attr(context, wrapper, abi_target, &lowered.llvm_name);

            let implementation_name = format!("__wave_export_impl_{}", symbol);
            let implementation =
                module.add_function(&implementation_name, fn_type, Some(Linkage::Internal));
            apply_function_codegen_attrs(context, implementation, disable_red_zone, cpu, features);
            if matches!(return_type, Some(WaveType::Never)) {
                implementation.add_attribute(
                    AttributeLoc::Function,
                    context.create_enum_attribute(Attribute::get_named_enum_kind_id("noreturn"), 0),
                );
            }

            functions.insert(symbol.clone(), implementation);
            extern_c_info.insert(symbol.clone(), lowered.info.clone());
            export_wrappers.push(ExportCWrapper {
                wrapper,
                implementation,
                info: lowered.info,
                wave_param_types,
                wave_ret_type,
            });
        } else {
            let function = module.add_function(symbol, fn_type, None);
            apply_function_codegen_attrs(context, function, disable_red_zone, cpu, features);
            if matches!(return_type, Some(WaveType::Never)) {
                function.add_attribute(
                    AttributeLoc::Function,
                    context.create_enum_attribute(Attribute::get_named_enum_kind_id("noreturn"), 0),
                );
            }
            functions.insert(symbol.clone(), function);
        }
    }

    for ext in &extern_functions {
        if !is_supported_extern_abi(&ext.abi, abi_target) {
            panic!(
                "unsupported extern ABI '{}' for function '{}' on {}: supported ABIs are {}",
                ext.abi,
                ext.name,
                abi_target.desc(),
                supported_extern_abi_description(abi_target)
            );
        }

        let lowered = lower_extern_c(
            context,
            td,
            abi_target,
            backend.abi.as_deref(),
            ext,
            &struct_types,
        );

        let f = module.add_function(&lowered.llvm_name, lowered.fn_type, None);
        apply_extern_c_attrs(context, f, &lowered.info);
        apply_function_codegen_attrs(context, f, disable_red_zone, cpu, features);
        apply_wasm_import_attrs(context, f, abi_target, &lowered.llvm_name);

        functions.insert(ext.name.clone(), f);

        extern_c_info.insert(ext.name.clone(), lowered.info);
    }

    for entry in &function_nodes {
        let func_node = entry.node;
        let function = *functions.get(&entry.symbol).unwrap();
        let entry_block = context.append_basic_block(function, "entry");
        builder.position_at_end(entry_block);

        let mut variables: HashMap<String, VariableInfo> = global_statics.clone();
        let mut string_counter = 0;
        let mut loop_exit_stack = vec![];
        let mut loop_continue_stack = vec![];

        for (i, param) in func_node.parameters.iter().enumerate() {
            let llvm_type =
                wave_type_to_llvm_type(context, &param.param_type, &struct_types, TypeFlavor::AbiC);
            let alloca = builder.build_alloca(llvm_type, &param.name).unwrap();
            let param_val = function.get_nth_param(i as u32).unwrap();
            builder.build_store(alloca, param_val).unwrap();

            variables.insert(
                param.name.clone(),
                VariableInfo {
                    ptr: alloca,
                    mutability: Mutability::Var,
                    ty: param.param_type.clone(),
                },
            );
        }

        for stmt in &func_node.body {
            if builder
                .get_insert_block()
                .is_some_and(|block| block.get_terminator().is_some())
            {
                break;
            }
            if let ASTNode::Statement(_) | ASTNode::Variable(_) = stmt {
                generate_statement_ir(
                    context,
                    builder,
                    module,
                    &mut string_counter,
                    stmt,
                    &mut variables,
                    &mut loop_exit_stack,
                    &mut loop_continue_stack,
                    function,
                    &global_consts,
                    &struct_types,
                    &struct_field_indices,
                    &struct_field_types,
                    td,
                    &extern_c_info,
                    program,
                );
            } else {
                panic!("Unsupported node inside function '{}'", func_node.name);
            }
        }

        let current_block = builder.get_insert_block().unwrap();
        if current_block.get_terminator().is_none() {
            let implicit_i32_main = is_implicit_i32_main(&func_node.name, &func_node.return_type);
            let is_void_like = match &func_node.return_type {
                None => true,
                Some(WaveType::Void) => true,
                _ => false,
            };

            if implicit_i32_main {
                let zero = context.i32_type().const_zero();
                builder.build_return(Some(&zero)).unwrap();
            } else if func_node.return_type == Some(WaveType::Never) {
                builder.build_unreachable().unwrap();
            } else if is_void_like {
                builder.build_return(None).unwrap();
            } else {
                panic!(
                    "Non-void function '{}' is missing a return statement",
                    func_node.name
                );
            }
        }
    }

    for export in &export_wrappers {
        build_export_c_wrapper(context, builder, td, export);
    }

    build_wasi_start_wrapper(context, builder, module, abi_target);

    if should_run_llvm_pass_pipeline() {
        let pbo = PassBuilderOptions::create();
        let pipeline = pipeline_from_opt_flag(opt_flag);

        codegen_trace("run optimization passes");
        module
            .run_passes(pipeline, &tm, pbo)
            .expect("failed to run optimization passes");
    } else {
        codegen_trace("skip optimization passes");
    }

    codegen_trace("finish module");
    GeneratedModule {
        module,
        target_machine: tm,
    }
}

fn pipeline_from_opt_flag(opt_flag: &str) -> &'static str {
    match normalize_opt_flag_for_passes(opt_flag) {
        "" | "-O0" => "default<O0>",
        "-O1" => "default<O1>",
        "-O2" => "default<O2>",
        "-O3" => "default<O3>",
        "-Os" => "default<Os>",
        "-Oz" => "default<Oz>",
        other => panic!("unknown opt flag for LLVM passes: {}", other),
    }
}

fn parse_int_literal(raw: &str) -> Option<i128> {
    lexer::number::IntegerLiteral::parse(raw)?.to_i128()
}

fn repr_bits_signed(ty: &WaveType) -> Option<(u32, bool)> {
    match ty {
        WaveType::Int(b) => Some((*b as u32, true)),
        WaveType::Uint(b) => Some((*b as u32, false)),
        WaveType::Bool => Some((1, false)),
        WaveType::Byte => Some((8, false)),
        WaveType::Char => Some((8, false)),
        _ => None,
    }
}

fn fits_in_int(v: i128, bits: u32, signed: bool) -> bool {
    if bits == 0 || bits > 64 {
        return false;
    }

    if signed {
        if bits == 64 {
            return v >= i64::MIN as i128 && v <= i64::MAX as i128;
        }
        let min = -(1i128 << (bits - 1));
        let max = (1i128 << (bits - 1)) - 1;
        v >= min && v <= max
    } else {
        if v < 0 {
            return false;
        }
        if bits == 64 {
            return (v as u128) <= u64::MAX as u128;
        }
        let max = (1u128 << bits) - 1;
        (v as u128) <= max
    }
}

fn add_enum_consts_to_globals(
    context: &'static Context,
    e: &EnumNode,
    global_consts: &mut HashMap<String, BasicValueEnum<'static>>,
) {
    let (bits, signed) = repr_bits_signed(&e.repr_type).unwrap_or_else(|| {
        panic!(
            "enum '{}' repr type must be an integer type, got {:?}",
            e.name, e.repr_type
        )
    });

    if bits > 64 || bits == 0 {
        panic!("enum '{}' repr bit-width unsupported: {}", e.name, bits);
    }

    let int_ty = context.custom_width_int_type(bits);

    let mut next: i128 = 0;

    for v in &e.variants {
        if let Some(raw) = &v.explicit_value {
            next = parse_int_literal(raw).unwrap_or_else(|| {
                panic!(
                    "enum '{}' variant '{}' has invalid integer literal: {}",
                    e.name, v.name, raw
                )
            });
        }

        if !fits_in_int(next, bits, signed) {
            panic!(
                "enum '{}' variant '{}' value {} does not fit in {}{}",
                e.name,
                v.name,
                next,
                if signed { "i" } else { "u" },
                bits
            );
        }

        let c = if signed {
            int_ty.const_int(next as u64, true)
        } else {
            int_ty.const_int(next as u64, false)
        };

        global_consts.insert(v.name.clone(), c.into());

        next += 1;
    }
}
