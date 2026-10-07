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

//! Internal tagged-variant layout construction.
//!
//! Variants cannot cross the C ABI directly, so their representation is an
//! internal compiler contract: i32 tag, a zero-length alignment witness, and
//! one byte array as large as the largest payload. Field 2 starts at the aligned
//! payload offset. Constructors zero the complete value before storing the
//! selected case; projections reinterpret field 2 using that case's tuple type.

use super::types::{wave_type_to_llvm_type, TypeFlavor};
use hir::{HirExpressionType, TypedProgram};
use inkwell::context::Context;
use inkwell::targets::TargetData;
use inkwell::types::{BasicType, StructType};
use parser::ast::{ASTNode, StatementNode, VariantNode, WaveType};
use parser::types::{parse_type, split_top_level_generic_args, token_type_to_wave_type};
use std::collections::{BTreeMap, BTreeSet, HashMap};

#[derive(Debug)]
pub(crate) struct VariantDefinition {
    pub name: String,
    pub cases: Vec<Vec<WaveType>>,
}

pub(crate) fn declare_variant_types<'ctx>(
    context: &'ctx Context,
    program: &TypedProgram,
    struct_types: &mut HashMap<String, StructType<'ctx>>,
) -> Vec<VariantDefinition> {
    let templates = program
        .syntax()
        .iter()
        .filter_map(|node| match node {
            ASTNode::Variant(variant) => Some((variant.name.clone(), variant)),
            _ => None,
        })
        .collect::<HashMap<_, _>>();

    let mut names = BTreeSet::new();
    collect_node_variant_types(program.syntax(), &mut names);
    for expression_type in program.expression_types() {
        if let HirExpressionType::Resolved(ty) = expression_type {
            collect_type_variants(ty, &mut names);
        }
    }
    for construction in program.variant_constructions() {
        collect_type_variants(&construction.variant_type, &mut names);
        for payload in &construction.payload_types {
            collect_type_variants(payload, &mut names);
        }
    }
    for pattern in program.variant_patterns() {
        collect_type_variants(&pattern.variant_type, &mut names);
        for payload in &pattern.payload_types {
            collect_type_variants(payload, &mut names);
        }
    }
    for variant in templates.values().filter(|variant| variant.generic_params.is_empty()) {
        names.insert(variant.name.clone());
    }

    let mut definitions = BTreeMap::<String, Vec<Vec<WaveType>>>::new();
    while let Some(name) = names.iter().find(|name| !definitions.contains_key(*name)).cloned() {
        let cases = specialize_variant(&name, &templates);
        for case in &cases {
            for payload in case {
                collect_type_variants(payload, &mut names);
            }
        }
        definitions.insert(name, cases);
    }

    let definitions = definitions
        .into_iter()
        .map(|(name, cases)| {
            let llvm_name = format!("variant.{name}");
            let ty = context.opaque_struct_type(&llvm_name);
            if struct_types.insert(name.clone(), ty).is_some() {
                panic!("variant type '{}' conflicts with another LLVM aggregate", name);
            }
            VariantDefinition { name, cases }
        })
        .collect::<Vec<_>>();
    definitions
}

pub(crate) fn payload_type<'ctx>(
    context: &'ctx Context,
    payloads: &[WaveType],
    struct_types: &HashMap<String, StructType<'ctx>>,
) -> StructType<'ctx> {
    let fields = payloads
        .iter()
        .map(|ty| wave_type_to_llvm_type(context, ty, struct_types, TypeFlavor::AbiC))
        .collect::<Vec<_>>();
    context.struct_type(&fields, false)
}

pub(crate) fn define_variant_types<'ctx>(
    context: &'ctx Context,
    definitions: &[VariantDefinition],
    struct_types: &HashMap<String, StructType<'ctx>>,
    target_data: &TargetData,
) {
    let mut pending = definitions.iter().collect::<Vec<_>>();
    while !pending.is_empty() {
        let before = pending.len();
        pending.retain(|definition| {
            let payloads = definition
                .cases
                .iter()
                .map(|case| payload_type(context, case, struct_types))
                .collect::<Vec<_>>();
            if payloads.iter().any(|ty| ty.size_of().is_none()) {
                return true;
            }
            let mut alignment_witness = context.i8_type().as_basic_type_enum();
            let mut alignment = 1;
            let mut size = 0;
            for payload in payloads {
                size = size.max(target_data.get_abi_size(&payload));
                let payload_alignment = target_data.get_abi_alignment(&payload);
                if payload_alignment > alignment {
                    alignment = payload_alignment;
                    alignment_witness = payload.as_basic_type_enum();
                }
            }
            let storage_size =
                u32::try_from(size).expect("variant payload exceeds LLVM array capacity");
            struct_types[&definition.name].set_body(
                &[
                    context.i32_type().into(),
                    alignment_witness.array_type(0).into(),
                    context.i8_type().array_type(storage_size).into(),
                ],
                false,
            );
            false
        });
        assert!(pending.len() < before, "validated variants contain an unsized value cycle");
    }
}

fn specialize_variant(
    concrete_name: &str,
    templates: &HashMap<String, &VariantNode>,
) -> Vec<Vec<WaveType>> {
    if let Some(concrete) =
        templates.get(concrete_name).filter(|variant| variant.generic_params.is_empty())
    {
        return concrete.cases.iter().map(|case| case.payload_types.clone()).collect();
    }
    let (base, arguments) = split_variant_application(concrete_name)
        .unwrap_or_else(|| (concrete_name.to_string(), Vec::new()));
    let template =
        templates.get(&base).unwrap_or_else(|| panic!("variant template '{}' not found", base));
    if template.generic_params.len() != arguments.len() {
        panic!(
            "variant '{}' expects {} type arguments, found {}",
            base,
            template.generic_params.len(),
            arguments.len()
        );
    }
    let substitutions =
        template.generic_params.iter().cloned().zip(arguments).collect::<HashMap<_, _>>();
    template
        .cases
        .iter()
        .map(|case| {
            case.payload_types
                .iter()
                .map(|payload| resolve_payload_type(payload, &substitutions, templates))
                .collect()
        })
        .collect()
}

fn resolve_payload_type(
    ty: &WaveType,
    substitutions: &HashMap<String, WaveType>,
    templates: &HashMap<String, &VariantNode>,
) -> WaveType {
    match ty {
        WaveType::Future(inner) => {
            WaveType::Future(Box::new(resolve_payload_type(inner, substitutions, templates)))
        },
        WaveType::Pointer(inner) => {
            WaveType::Pointer(Box::new(resolve_payload_type(inner, substitutions, templates)))
        },
        WaveType::Array(inner, length) => WaveType::Array(
            Box::new(resolve_payload_type(inner, substitutions, templates)),
            *length,
        ),
        WaveType::Struct(name) | WaveType::Variant(name) => {
            if let Some(substitution) = substitutions.get(name) {
                return substitution.clone();
            }
            let Some((base, arguments)) = split_variant_application(name) else {
                return if templates.contains_key(name) {
                    WaveType::Variant(name.clone())
                } else {
                    WaveType::Struct(name.clone())
                };
            };
            let arguments = arguments
                .iter()
                .map(|argument| resolve_payload_type(argument, substitutions, templates))
                .collect::<Vec<_>>();
            let concrete = format!(
                "{}<{}>",
                base,
                arguments.iter().map(display_wave_type).collect::<Vec<_>>().join(",")
            );
            if templates.contains_key(&base) {
                WaveType::Variant(concrete)
            } else {
                WaveType::Struct(concrete)
            }
        },
        _ => ty.clone(),
    }
}

fn split_variant_application(name: &str) -> Option<(String, Vec<WaveType>)> {
    let (base, tail) = name.split_once('<')?;
    let inner = tail.strip_suffix('>')?;
    let arguments = split_top_level_generic_args(inner)?
        .into_iter()
        .map(|argument| token_type_to_wave_type(&parse_type(&argument)?))
        .collect::<Option<Vec<_>>>()?;
    Some((base.trim().to_string(), arguments))
}

fn display_wave_type(ty: &WaveType) -> String {
    match ty {
        WaveType::Isz => "isz".to_string(),
        WaveType::Usz => "usz".to_string(),
        WaveType::Int(bits) => format!("i{bits}"),
        WaveType::Uint(bits) => format!("u{bits}"),
        WaveType::Float(bits) => format!("f{bits}"),
        WaveType::Bool => "bool".to_string(),
        WaveType::Char => "char".to_string(),
        WaveType::Byte => "byte".to_string(),
        WaveType::String => "str".to_string(),
        WaveType::Future(inner) => format!("Future<{}>", display_wave_type(inner)),
        WaveType::Pointer(inner) => format!("ptr<{}>", display_wave_type(inner)),
        WaveType::Array(inner, length) => format!("array<{},{}>", display_wave_type(inner), length),
        WaveType::Void => "void".to_string(),
        WaveType::Never => "!".to_string(),
        WaveType::Struct(name) | WaveType::Variant(name) => name.clone(),
    }
}

fn collect_type_variants(ty: &WaveType, names: &mut BTreeSet<String>) {
    match ty {
        WaveType::Variant(name) => {
            names.insert(name.clone());
        },
        WaveType::Future(inner) | WaveType::Pointer(inner) | WaveType::Array(inner, _) => {
            collect_type_variants(inner, names)
        },
        _ => {},
    }
}

fn collect_node_variant_types(nodes: &[ASTNode], names: &mut BTreeSet<String>) {
    for node in nodes {
        match node {
            ASTNode::Located { .. } => unreachable!("typed HIR detaches source wrappers"),
            ASTNode::Function(function) => {
                for parameter in &function.parameters {
                    collect_type_variants(&parameter.param_type, names);
                }
                if let Some(return_type) = &function.return_type {
                    collect_type_variants(return_type, names);
                }
                collect_node_variant_types(&function.body, names);
            },
            ASTNode::ExternFunction(function) => {
                for (_, ty) in &function.params {
                    collect_type_variants(ty, names);
                }
                collect_type_variants(&function.return_type, names);
            },
            ASTNode::Program(parameter) => collect_type_variants(&parameter.param_type, names),
            ASTNode::Variable(variable) => collect_type_variants(&variable.type_name, names),
            ASTNode::Statement(statement) => collect_statement_variant_types(statement, names),
            ASTNode::Struct(structure) => {
                for (_, ty) in &structure.fields {
                    collect_type_variants(ty, names);
                }
                for method in &structure.methods {
                    if let Some(return_type) = &method.return_type {
                        collect_type_variants(return_type, names);
                    }
                    for parameter in &method.parameters {
                        collect_type_variants(&parameter.param_type, names);
                    }
                    collect_node_variant_types(&method.body, names);
                }
            },
            ASTNode::ProtoImpl(implementation) => {
                for method in &implementation.methods {
                    if let Some(return_type) = &method.return_type {
                        collect_type_variants(return_type, names);
                    }
                    for parameter in &method.parameters {
                        collect_type_variants(&parameter.param_type, names);
                    }
                    collect_node_variant_types(&method.body, names);
                }
            },
            ASTNode::TypeAlias(alias) => collect_type_variants(&alias.target, names),
            ASTNode::Enum(enumeration) => collect_type_variants(&enumeration.repr_type, names),
            ASTNode::Variant(_) | ASTNode::Expression(_) => {},
        }
    }
}

fn collect_statement_variant_types(statement: &StatementNode, names: &mut BTreeSet<String>) {
    match statement {
        StatementNode::If { body, else_if_blocks, else_block, .. } => {
            collect_node_variant_types(body, names);
            if let Some(blocks) = else_if_blocks {
                for (_, body) in blocks.iter() {
                    collect_node_variant_types(body, names);
                }
            }
            if let Some(body) = else_block {
                collect_node_variant_types(body, names);
            }
        },
        StatementNode::While { body, .. } => collect_node_variant_types(body, names),
        StatementNode::For { initialization, body, .. } => {
            collect_node_variant_types(std::slice::from_ref(initialization.as_ref()), names);
            collect_node_variant_types(body, names);
        },
        StatementNode::Match { arms, .. } => {
            for arm in arms {
                collect_node_variant_types(&arm.body, names);
            }
        },
        _ => {},
    }
}

/// Serialize already evaluated scalar constants into target storage bytes.
/// Constant pointers currently accepted by the frontend are null or integer
/// addresses, so this never splits a relocatable symbol into byte relocations.
pub(crate) fn constant_storage_bytes<'ctx>(
    context: &'ctx Context,
    td: &TargetData,
    value: inkwell::values::BasicValueEnum<'ctx>,
    destination: &mut [u8],
) -> Result<(), super::consts::ConstEvalError> {
    use super::consts::ConstEvalError;
    use inkwell::types::AsTypeRef;
    use inkwell::values::{AnyValue, AsValueRef, BasicValueEnum, IntValue};
    match value {
        BasicValueEnum::StructValue(structure) => {
            let ty = structure.get_type();
            for index in 0..ty.count_fields() {
                let offset = td.offset_of_element(&ty, index).unwrap() as usize;
                constant_storage_bytes(
                    context,
                    td,
                    structure.get_field_at_index(index).unwrap(),
                    &mut destination[offset..],
                )?;
            }
            return Ok(());
        },
        BasicValueEnum::ArrayValue(array) => {
            let ty = array.get_type();
            let stride = td.get_abi_size(&ty.get_element_type()) as usize;
            for index in 0..ty.len() {
                // SAFETY: the constant array index is bounded by its LLVM type.
                let element = unsafe {
                    BasicValueEnum::new(llvm_sys::core::LLVMGetAggregateElement(
                        array.as_value_ref(),
                        index,
                    ))
                };
                constant_storage_bytes(
                    context,
                    td,
                    element,
                    &mut destination[index as usize * stride..],
                )?;
            }
            return Ok(());
        },
        _ => {},
    }
    let size = td.get_store_size(&value.get_type()) as usize;
    let integer = match value {
        BasicValueEnum::IntValue(integer) => integer,
        BasicValueEnum::FloatValue(float) => {
            let ty = context.custom_width_int_type(float.get_type().get_bit_width());
            // SAFETY: scalar constant and integer type have identical bit width.
            unsafe {
                IntValue::new(llvm_sys::core::LLVMConstBitCast(
                    float.as_value_ref(),
                    ty.as_type_ref(),
                ))
            }
        },
        BasicValueEnum::PointerValue(pointer) => {
            let ty = context.custom_width_int_type(size as u32 * 8);
            // LLVM does not fold ptrtoint(inttoptr(i64)) to i32 without
            // target information. Explicitly apply the target pointer width.
            let raw = pointer.as_value_ref();
            // SAFETY: the opcode and operand are inspected only for a constant
            // expression; inttoptr always has one integer operand.
            unsafe {
                if !llvm_sys::core::LLVMIsAConstantExpr(raw).is_null()
                    && llvm_sys::core::LLVMGetConstOpcode(raw) == llvm_sys::LLVMOpcode::LLVMIntToPtr
                {
                    IntValue::new(llvm_sys::core::LLVMGetOperand(raw, 0))
                } else {
                    pointer.const_to_int(ty)
                }
            }
        },
        _ => {
            return Err(ConstEvalError::Unsupported(
                "variant constant contains unsupported storage".into(),
            ))
        },
    };
    let printed = integer.print_to_string().to_string();
    let raw = printed.split_once(' ').map(|(_, value)| value).unwrap_or("");
    let raw = match raw {
        "true" => "1",
        "false" => "0",
        other => other,
    };
    let (negative, digits) = raw.strip_prefix('-').map_or((false, raw), |digits| (true, digits));
    if digits.is_empty() || !digits.bytes().all(|byte| byte.is_ascii_digit()) {
        return Err(ConstEvalError::Unsupported(format!(
            "variant constant requires numeric storage: {printed}"
        )));
    }
    let bytes = &mut destination[..size];
    bytes.fill(0);
    for digit in digits.bytes() {
        let mut carry = u16::from(digit - b'0');
        for byte in bytes.iter_mut() {
            let next = u16::from(*byte) * 10 + carry;
            *byte = next as u8;
            carry = next >> 8;
        }
    }
    if negative {
        let mut carry = 1u16;
        for byte in bytes.iter_mut() {
            let next = u16::from(!*byte) + carry;
            *byte = next as u8;
            carry = next >> 8;
        }
    }
    if td.get_byte_ordering() == inkwell::targets::ByteOrdering::BigEndian {
        bytes.reverse();
    }
    Ok(())
}
