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
//! Target-sized type resolution and backend-boundary type canonicalization.

use parser::ast::{ASTNode, Expression, StatementNode, WaveType};
use parser::types::{parse_type, split_top_level_generic_args, token_type_to_wave_type};
use std::collections::{HashMap, HashSet};

/// Resolve target-sized integers before semantic analysis and monomorphization.
/// This pass uses only the selected pointer width, never LLVM or the host width.
pub fn resolve_target_types(nodes: &mut [ASTNode], pointer_bits: u16) -> Result<(), String> {
    if !matches!(pointer_bits, 32 | 64) {
        return Err(format!("unsupported target pointer width {pointer_bits}"));
    }
    let named = HashMap::from([
        ("isz".to_string(), WaveType::Int(pointer_bits)),
        ("usz".to_string(), WaveType::Uint(pointer_bits)),
    ]);
    for node in nodes {
        canonicalize_node_types(node, &named);
    }
    Ok(())
}

pub(super) fn canonicalize_syntax_types(nodes: &mut [ASTNode]) {
    let named = collect_named_types(nodes);
    for node in nodes {
        canonicalize_node_types(node, &named);
    }
}

fn collect_named_types(nodes: &[ASTNode]) -> HashMap<String, WaveType> {
    let mut named = HashMap::new();
    for node in nodes {
        match node.unspanned() {
            ASTNode::TypeAlias(alias) => {
                named.insert(alias.name.clone(), alias.target.clone());
            },
            ASTNode::Enum(enumeration) => {
                named.insert(enumeration.name.clone(), enumeration.repr_type.clone());
            },
            ASTNode::Variant(variant) => {
                named.insert(variant.name.clone(), WaveType::Variant(variant.name.clone()));
            },
            _ => {},
        }
    }
    named
}

fn canonical_type(
    ty: &WaveType,
    named: &HashMap<String, WaveType>,
    visiting: &mut HashSet<String>,
) -> WaveType {
    match ty {
        WaveType::Isz => named.get("isz").cloned().unwrap_or(WaveType::Isz),
        WaveType::Usz => named.get("usz").cloned().unwrap_or(WaveType::Usz),
        WaveType::Future(inner) => {
            WaveType::Future(Box::new(canonical_type(inner, named, visiting)))
        },
        WaveType::Pointer(inner) => {
            WaveType::Pointer(Box::new(canonical_type(inner, named, visiting)))
        },
        WaveType::Array(inner, length) => {
            WaveType::Array(Box::new(canonical_type(inner, named, visiting)), *length)
        },
        WaveType::Struct(name) => canonical_named_type(name, named, visiting)
            .unwrap_or_else(|| WaveType::Struct(name.clone())),
        WaveType::Variant(name) => canonical_variant_application(name, named, visiting)
            .unwrap_or_else(|| WaveType::Variant(name.clone())),
        _ => ty.clone(),
    }
}

fn canonical_named_type(
    name: &str,
    named: &HashMap<String, WaveType>,
    visiting: &mut HashSet<String>,
) -> Option<WaveType> {
    if let Some(target) = named.get(name) {
        assert!(
            visiting.insert(name.to_string()),
            "semantic validation allowed a named type cycle at `{name}`"
        );
        let resolved = canonical_type(target, named, visiting);
        visiting.remove(name);
        return Some(resolved);
    }
    canonical_variant_application(name, named, visiting)
}

fn canonical_variant_application(
    name: &str,
    named: &HashMap<String, WaveType>,
    visiting: &mut HashSet<String>,
) -> Option<WaveType> {
    let (base, arguments) = split_named_application(name)?;
    let arguments = arguments
        .into_iter()
        .map(|argument| canonical_type(&argument, named, visiting))
        .map(|argument| display_wave_type(&argument))
        .collect::<Vec<_>>()
        .join(",");
    let name = format!("{base}<{arguments}>");
    Some(if matches!(named.get(base), Some(WaveType::Variant(_))) {
        WaveType::Variant(name)
    } else {
        WaveType::Struct(name)
    })
}

fn split_named_application(name: &str) -> Option<(&str, Vec<WaveType>)> {
    let (base, tail) = name.split_once('<')?;
    let inner = tail.strip_suffix('>')?;
    let arguments = split_top_level_generic_args(inner)?
        .into_iter()
        .map(|argument| token_type_to_wave_type(&parse_type(&argument)?))
        .collect::<Option<Vec<_>>>()?;
    Some((base.trim(), arguments))
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
        WaveType::Array(inner, length) => {
            format!("array<{},{}>", display_wave_type(inner), length)
        },
        WaveType::Void => "void".to_string(),
        WaveType::Never => "!".to_string(),
        WaveType::Struct(name) | WaveType::Variant(name) => name.clone(),
    }
}

fn canonicalize_type(ty: &mut WaveType, named: &HashMap<String, WaveType>) {
    *ty = canonical_type(ty, named, &mut HashSet::new());
}

fn canonicalize_function_types(
    function: &mut parser::ast::FunctionNode,
    named: &HashMap<String, WaveType>,
) {
    for parameter in &mut function.parameters {
        canonicalize_type(&mut parameter.param_type, named);
        if let Some(default) = &mut parameter.initial_value {
            canonicalize_expression_types(default, named);
        }
    }
    if let Some(return_type) = &mut function.return_type {
        canonicalize_type(return_type, named);
    }
    for node in &mut function.body {
        canonicalize_node_types(node, named);
    }
}

fn canonicalize_node_types(node: &mut ASTNode, named: &HashMap<String, WaveType>) {
    match node {
        ASTNode::Located { value, .. } => canonicalize_node_types(value, named),
        ASTNode::Function(function) => canonicalize_function_types(function, named),
        ASTNode::ExternFunction(function) => {
            for (_, parameter_type) in &mut function.params {
                canonicalize_type(parameter_type, named);
            }
            canonicalize_type(&mut function.return_type, named);
        },
        ASTNode::Program(parameter) => canonicalize_type(&mut parameter.param_type, named),
        ASTNode::Statement(statement) => canonicalize_statement_types(statement, named),
        ASTNode::Variable(variable) => {
            canonicalize_type(&mut variable.type_name, named);
            if let Some(initializer) = &mut variable.initial_value {
                canonicalize_expression_types(initializer, named);
            }
        },
        ASTNode::Expression(expression) => canonicalize_expression_types(expression, named),
        ASTNode::Struct(structure) => {
            for (_, field_type) in &mut structure.fields {
                canonicalize_type(field_type, named);
            }
            for method in &mut structure.methods {
                canonicalize_function_types(method, named);
            }
        },
        ASTNode::ProtoImpl(implementation) => {
            for method in &mut implementation.methods {
                canonicalize_function_types(method, named);
            }
        },
        ASTNode::TypeAlias(alias) => canonicalize_type(&mut alias.target, named),
        ASTNode::Enum(enumeration) => canonicalize_type(&mut enumeration.repr_type, named),
        ASTNode::Variant(variant) => {
            for case in &mut variant.cases {
                for payload_type in &mut case.payload_types {
                    canonicalize_type(payload_type, named);
                }
            }
        },
    }
}

fn canonicalize_statement_types(statement: &mut StatementNode, named: &HashMap<String, WaveType>) {
    match statement {
        StatementNode::PrintFormat { args, .. }
        | StatementNode::PrintlnFormat { args, .. }
        | StatementNode::Input { args, .. } => {
            for argument in args {
                canonicalize_expression_types(argument, named);
            }
        },
        StatementNode::If { condition, body, else_if_blocks, else_block } => {
            canonicalize_expression_types(condition, named);
            for node in body {
                canonicalize_node_types(node, named);
            }
            if let Some(blocks) = else_if_blocks {
                for (condition, body) in blocks.iter_mut() {
                    canonicalize_expression_types(condition, named);
                    for node in body {
                        canonicalize_node_types(node, named);
                    }
                }
            }
            if let Some(body) = else_block {
                for node in body.iter_mut() {
                    canonicalize_node_types(node, named);
                }
            }
        },
        StatementNode::For { initialization, condition, increment, body } => {
            canonicalize_node_types(initialization, named);
            canonicalize_expression_types(condition, named);
            canonicalize_expression_types(increment, named);
            for node in body {
                canonicalize_node_types(node, named);
            }
        },
        StatementNode::While { condition, body } => {
            canonicalize_expression_types(condition, named);
            for node in body {
                canonicalize_node_types(node, named);
            }
        },
        StatementNode::Match { value, arms } => {
            canonicalize_expression_types(value, named);
            for arm in arms {
                for node in &mut arm.body {
                    canonicalize_node_types(node, named);
                }
            }
        },
        StatementNode::Assign { value, .. } => canonicalize_expression_types(value, named),
        StatementNode::AsmBlock { inputs, outputs, .. } => {
            for (_, expression) in inputs.iter_mut().chain(outputs.iter_mut()) {
                canonicalize_expression_types(expression, named);
            }
        },
        StatementNode::Return(Some(expression)) | StatementNode::Expression(expression) => {
            canonicalize_expression_types(expression, named);
        },
        StatementNode::Print(_)
        | StatementNode::Println(_)
        | StatementNode::Variable(_)
        | StatementNode::Import(_)
        | StatementNode::Break
        | StatementNode::Continue
        | StatementNode::Return(None) => {},
    }
}

fn canonicalize_expression_types(expression: &mut Expression, named: &HashMap<String, WaveType>) {
    match expression {
        Expression::Located { value, .. } => canonicalize_expression_types(value, named),
        Expression::StructLiteral { name, fields } => {
            if let Some(ty) = canonical_variant_application(name, named, &mut HashSet::new()) {
                *name = display_wave_type(&ty);
            }
            for (_, value) in fields {
                canonicalize_expression_types(value, named);
            }
        },
        Expression::FunctionCall { type_args, args, .. } => {
            for type_argument in type_args {
                canonicalize_type(type_argument, named);
            }
            for argument in args {
                canonicalize_expression_types(argument, named);
            }
        },
        Expression::MethodCall { object, args, type_args, .. } => {
            for type_argument in type_args {
                canonicalize_type(type_argument, named);
            }
            canonicalize_expression_types(object, named);
            for argument in args {
                canonicalize_expression_types(argument, named);
            }
        },
        Expression::Deref(inner)
        | Expression::AddressOf(inner)
        | Expression::Await(inner)
        | Expression::Grouped(inner)
        | Expression::Unary { expr: inner, .. }
        | Expression::FieldAccess { object: inner, .. }
        | Expression::IncDec { target: inner, .. } => {
            canonicalize_expression_types(inner, named);
        },
        Expression::Cast { expr, target_type } => {
            canonicalize_expression_types(expr, named);
            canonicalize_type(target_type, named);
        },
        Expression::BinaryExpression { left, right, .. }
        | Expression::IndexAccess { target: left, index: right }
        | Expression::AssignOperation { target: left, value: right, .. }
        | Expression::Assignment { target: left, value: right } => {
            canonicalize_expression_types(left, named);
            canonicalize_expression_types(right, named);
        },
        Expression::ArrayLiteral(values) => {
            for value in values {
                canonicalize_expression_types(value, named);
            }
        },
        Expression::AsmBlock { inputs, outputs, .. } => {
            for (_, expression) in inputs.iter_mut().chain(outputs.iter_mut()) {
                canonicalize_expression_types(expression, named);
            }
        },
        Expression::Null | Expression::Literal(_) | Expression::Variable(_) => {},
    }
}
