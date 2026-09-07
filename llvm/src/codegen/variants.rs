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
//! internal compiler contract. Each value stores an `i32` discriminant followed
//! by one naturally aligned payload tuple per case. Only the selected case is
//! initialized by a constructor; the complete value is zeroed first so copying
//! and inspecting nested patterns never reads uninitialized storage.

use super::types::{wave_type_to_llvm_type, TypeFlavor};
use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum, StructType};
use parser::ast::{ASTNode, StatementNode, VariantNode, WaveType};
use parser::hir::{HirExpressionType, TypedProgram};
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
    for variant in templates
        .values()
        .filter(|variant| variant.generic_params.is_empty())
    {
        names.insert(variant.name.clone());
    }

    let mut definitions = BTreeMap::<String, Vec<Vec<WaveType>>>::new();
    while let Some(name) = names
        .iter()
        .find(|name| !definitions.contains_key(*name))
        .cloned()
    {
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
                panic!(
                    "variant type '{}' conflicts with another LLVM aggregate",
                    name
                );
            }
            VariantDefinition { name, cases }
        })
        .collect::<Vec<_>>();
    definitions
}

pub(crate) fn define_variant_types<'ctx>(
    context: &'ctx Context,
    definitions: &[VariantDefinition],
    struct_types: &HashMap<String, StructType<'ctx>>,
) {
    for definition in definitions {
        let variant_ty = *struct_types
            .get(&definition.name)
            .unwrap_or_else(|| panic!("variant type '{}' was not declared", definition.name));
        let mut fields = Vec::<BasicTypeEnum<'ctx>>::with_capacity(definition.cases.len() + 1);
        fields.push(context.i32_type().as_basic_type_enum());
        for payloads in &definition.cases {
            let payload_types = payloads
                .iter()
                .map(|payload| {
                    wave_type_to_llvm_type(context, payload, struct_types, TypeFlavor::AbiC)
                })
                .collect::<Vec<_>>();
            fields.push(
                context
                    .struct_type(&payload_types, false)
                    .as_basic_type_enum(),
            );
        }
        variant_ty.set_body(&fields, false);
    }
}

fn specialize_variant(
    concrete_name: &str,
    templates: &HashMap<String, &VariantNode>,
) -> Vec<Vec<WaveType>> {
    if let Some(concrete) = templates
        .get(concrete_name)
        .filter(|variant| variant.generic_params.is_empty())
    {
        return concrete
            .cases
            .iter()
            .map(|case| case.payload_types.clone())
            .collect();
    }
    let (base, arguments) = split_variant_application(concrete_name)
        .unwrap_or_else(|| (concrete_name.to_string(), Vec::new()));
    let template = templates
        .get(&base)
        .unwrap_or_else(|| panic!("variant template '{}' not found", base));
    if template.generic_params.len() != arguments.len() {
        panic!(
            "variant '{}' expects {} type arguments, found {}",
            base,
            template.generic_params.len(),
            arguments.len()
        );
    }
    let substitutions = template
        .generic_params
        .iter()
        .cloned()
        .zip(arguments)
        .collect::<HashMap<_, _>>();
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
        WaveType::Pointer(inner) => WaveType::Pointer(Box::new(resolve_payload_type(
            inner,
            substitutions,
            templates,
        ))),
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
                arguments
                    .iter()
                    .map(display_wave_type)
                    .collect::<Vec<_>>()
                    .join(",")
            );
            if templates.contains_key(&base) {
                WaveType::Variant(concrete)
            } else {
                WaveType::Struct(concrete)
            }
        }
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
        }
        WaveType::Pointer(inner) | WaveType::Array(inner, _) => collect_type_variants(inner, names),
        _ => {}
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
            }
            ASTNode::ExternFunction(function) => {
                for (_, ty) in &function.params {
                    collect_type_variants(ty, names);
                }
                collect_type_variants(&function.return_type, names);
            }
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
            }
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
            }
            ASTNode::TypeAlias(alias) => collect_type_variants(&alias.target, names),
            ASTNode::Enum(enumeration) => collect_type_variants(&enumeration.repr_type, names),
            ASTNode::Variant(_) | ASTNode::Expression(_) => {}
        }
    }
}

fn collect_statement_variant_types(statement: &StatementNode, names: &mut BTreeSet<String>) {
    match statement {
        StatementNode::If {
            body,
            else_if_blocks,
            else_block,
            ..
        } => {
            collect_node_variant_types(body, names);
            if let Some(blocks) = else_if_blocks {
                for (_, body) in blocks.iter() {
                    collect_node_variant_types(body, names);
                }
            }
            if let Some(body) = else_block {
                collect_node_variant_types(body, names);
            }
        }
        StatementNode::While { body, .. } => collect_node_variant_types(body, names),
        StatementNode::For {
            initialization,
            body,
            ..
        } => {
            collect_node_variant_types(std::slice::from_ref(initialization.as_ref()), names);
            collect_node_variant_types(body, names);
        }
        StatementNode::Match { arms, .. } => {
            for arm in arms {
                collect_node_variant_types(&arm.body, names);
            }
        }
        _ => {}
    }
}
