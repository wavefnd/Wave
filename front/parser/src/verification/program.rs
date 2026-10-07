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

//! Immutable declaration collection, aliases, and generic type lookup.
use super::diagnostics::{top_level_span_hint, SemanticSpanHint, SemanticSpanKind};
use super::model::*;
use crate::ast::{ASTNode, Expression, FunctionNode, Literal, Mutability, WaveType};
use crate::types::{parse_type, split_top_level_generic_args, token_type_to_wave_type};
use std::collections::{HashMap, HashSet};

#[derive(Default)]
pub(super) struct ProgramTypes {
    pub(super) functions: HashMap<String, FunctionType>,
    pub(super) methods: HashMap<(String, String), FunctionType>,
    pub(super) structs: HashMap<String, HashMap<String, WaveType>>,
    pub(super) aliases: HashMap<String, WaveType>,
    pub(super) enum_reprs: HashMap<String, WaveType>,
    pub(super) globals: HashMap<String, Binding>,
    pub(super) constant_values: HashMap<String, String>,
    pub(super) type_names: HashSet<String>,
    pub(super) generic_type_params: HashSet<String>,
    pub(super) struct_generic_params: HashMap<String, Vec<String>>,
    pub(super) variants: HashMap<String, VariantType>,
    pub(super) variant_generic_params: HashMap<String, Vec<String>>,
}

impl ProgramTypes {
    pub(super) fn collect(
        nodes: &[ASTNode],
    ) -> Result<Self, (usize, String, Option<SemanticSpanHint>)> {
        let mut out = Self::default();

        // Reserve all type names and generic parameters first. The second pass
        // can then resolve forward references without depending on source order.
        for (index, node) in nodes.iter().enumerate() {
            let type_name = match node {
                ASTNode::Struct(structure) => Some(structure.name.as_str()),
                ASTNode::TypeAlias(alias) => Some(alias.name.as_str()),
                ASTNode::Enum(enumeration) => Some(enumeration.name.as_str()),
                ASTNode::Variant(variant) => Some(variant.name.as_str()),
                _ => None,
            };
            if let Some(name) = type_name {
                if !out.type_names.insert(name.to_string()) {
                    return Err((
                        index,
                        format!("duplicate type declaration `{}`", name),
                        Some(top_level_span_hint(node)),
                    ));
                }
            }
            match node {
                ASTNode::Function(function) => {
                    out.generic_type_params.extend(function.generic_params.iter().cloned());
                },
                ASTNode::Struct(structure) => {
                    out.generic_type_params.extend(structure.generic_params.iter().cloned());
                    for method in &structure.methods {
                        out.generic_type_params.extend(method.generic_params.iter().cloned());
                    }
                },
                ASTNode::Variant(variant) => {
                    out.generic_type_params.extend(variant.generic_params.iter().cloned());
                },
                ASTNode::ProtoImpl(implementation) => {
                    for method in &implementation.methods {
                        out.generic_type_params.extend(method.generic_params.iter().cloned());
                    }
                },
                _ => {},
            }
        }

        let mut value_names = HashSet::new();

        // Values, fields, methods, aliases, and constants need their complete
        // signatures before any function body is checked.
        for (index, node) in nodes.iter().enumerate() {
            let failure =
                |message: String, primary: Option<SemanticSpanHint>| (index, message, primary);
            match node {
                ASTNode::Function(function) => {
                    insert_unique_value_name(&mut value_names, &function.name)
                        .map_err(|message| failure(message, Some(top_level_span_hint(node))))?;
                    insert_unique_function(
                        &mut out.functions,
                        &function.name,
                        function_type(function),
                    )
                    .map_err(|message| failure(message, Some(top_level_span_hint(node))))?;
                },
                ASTNode::ExternFunction(function) => {
                    insert_unique_value_name(&mut value_names, &function.name)
                        .map_err(|message| failure(message, Some(top_level_span_hint(node))))?;
                    insert_unique_function(
                        &mut out.functions,
                        &function.name,
                        FunctionType {
                            defaults: Vec::new(),
                            params: function.params.iter().map(|(_, ty)| ty.clone()).collect(),
                            required_params: function.params.len(),
                            return_type: function.return_type.clone(),
                            generic_params: Vec::new(),
                            variadic: function.variadic,
                        },
                    )
                    .map_err(|message| failure(message, Some(top_level_span_hint(node))))?;
                },
                ASTNode::Struct(structure) => {
                    let mut fields = HashMap::new();
                    for (name, ty) in &structure.fields {
                        if fields.insert(name.clone(), ty.clone()).is_some() {
                            return Err(failure(
                                format!(
                                    "duplicate field `{}` in struct `{}`",
                                    name, structure.name
                                ),
                                Some(SemanticSpanHint {
                                    kind: SemanticSpanKind::Declaration,
                                    text: name.clone(),
                                    occurrence: 2,
                                }),
                            ));
                        }
                    }
                    out.structs.insert(structure.name.clone(), fields);
                    out.struct_generic_params
                        .insert(structure.name.clone(), structure.generic_params.clone());
                    for method in &structure.methods {
                        insert_unique_method(
                            &mut out.methods,
                            &structure.name,
                            &method.name,
                            function_type(method),
                        )
                        .map_err(|message| {
                            failure(
                                message,
                                Some(SemanticSpanHint {
                                    kind: SemanticSpanKind::Declaration,
                                    text: method.name.clone(),
                                    occurrence: 2,
                                }),
                            )
                        })?;
                    }
                },
                ASTNode::ProtoImpl(implementation) => {
                    for method in &implementation.methods {
                        let signature = function_type(method);
                        insert_unique_method(
                            &mut out.methods,
                            &implementation.target,
                            &method.name,
                            signature.clone(),
                        )
                        .map_err(|message| {
                            failure(
                                message,
                                Some(SemanticSpanHint {
                                    kind: SemanticSpanKind::Declaration,
                                    text: method.name.clone(),
                                    occurrence: 2,
                                }),
                            )
                        })?;
                        let lowered =
                            crate::ast::method_symbol(&implementation.target, &method.name);
                        insert_unique_value_name(&mut value_names, &lowered)
                            .map_err(|message| failure(message, Some(top_level_span_hint(node))))?;
                        insert_unique_function(&mut out.functions, &lowered, signature)
                            .map_err(|message| failure(message, Some(top_level_span_hint(node))))?;
                    }
                },
                ASTNode::TypeAlias(alias) => {
                    out.aliases.insert(alias.name.clone(), alias.target.clone());
                },
                ASTNode::Variable(variable)
                    if matches!(variable.mutability, Mutability::Const | Mutability::Static) =>
                {
                    insert_unique_value_name(&mut value_names, &variable.name)
                        .map_err(|message| failure(message, Some(top_level_span_hint(node))))?;
                    out.globals.insert(
                        variable.name.clone(),
                        Binding { mutability: variable.mutability, ty: variable.type_name.clone() },
                    );
                    if matches!(variable.mutability, Mutability::Const) {
                        if let Some(Expression::Literal(Literal::Int(raw))) =
                            &variable.initial_value
                        {
                            if lexer::number::IntegerLiteral::parse(raw).is_some() {
                                out.constant_values.insert(variable.name.clone(), raw.clone());
                            }
                        }
                    }
                },
                ASTNode::Enum(enumeration) => {
                    out.enum_reprs.insert(enumeration.name.clone(), enumeration.repr_type.clone());
                    let mut variants = HashSet::new();
                    let mut next = 0i128;
                    for variant in &enumeration.variants {
                        if !variants.insert(variant.name.clone()) {
                            return Err(failure(
                                format!(
                                    "duplicate variant `{}` in enum `{}`",
                                    variant.name, enumeration.name
                                ),
                                Some(SemanticSpanHint {
                                    kind: SemanticSpanKind::Declaration,
                                    text: variant.name.clone(),
                                    occurrence: 2,
                                }),
                            ));
                        }
                        insert_unique_value_name(&mut value_names, &variant.name)
                            .map_err(|message| failure(message, Some(top_level_span_hint(node))))?;
                        out.globals.insert(
                            variant.name.clone(),
                            Binding {
                                mutability: Mutability::Const,
                                ty: enumeration.repr_type.clone(),
                            },
                        );
                        if let Some(raw) = &variant.explicit_value {
                            next = parse_integer_value(raw).ok_or_else(|| {
                                failure(
                                    format!(
                                        "enum `{}.{}` has invalid integer value `{}`",
                                        enumeration.name, variant.name, raw
                                    ),
                                    Some(SemanticSpanHint {
                                        kind: SemanticSpanKind::Declaration,
                                        text: variant.name.clone(),
                                        occurrence: 1,
                                    }),
                                )
                            })?;
                        }
                        out.constant_values.insert(variant.name.clone(), next.to_string());
                        next = next.checked_add(1).ok_or_else(|| {
                            failure(
                                format!("enum `{}` value overflow", enumeration.name),
                                Some(top_level_span_hint(node)),
                            )
                        })?;
                    }
                },
                ASTNode::Variant(variant) => {
                    let mut names = HashSet::new();
                    let mut cases = Vec::with_capacity(variant.cases.len());
                    for case in &variant.cases {
                        if !names.insert(case.name.clone()) {
                            return Err(failure(
                                format!(
                                    "duplicate case `{}` in variant `{}`",
                                    case.name, variant.name
                                ),
                                Some(SemanticSpanHint {
                                    kind: SemanticSpanKind::Declaration,
                                    text: case.name.clone(),
                                    occurrence: 2,
                                }),
                            ));
                        }
                        cases.push((case.name.clone(), case.payload_types.clone()));
                    }
                    out.variant_generic_params
                        .insert(variant.name.clone(), variant.generic_params.clone());
                    out.variants.insert(
                        variant.name.clone(),
                        VariantType { generic_params: variant.generic_params.clone(), cases },
                    );
                },
                _ => {},
            }
        }

        Ok(out)
    }

    pub(super) fn is_known_named_type(&self, name: &str) -> bool {
        self.type_names.contains(name)
            || name.split_once('<').is_some_and(|(base, _)| self.type_names.contains(base.trim()))
    }

    pub(super) fn named_type_base<'a>(&self, name: &'a str) -> &'a str {
        name.split_once('<').map_or(name, |(base, _)| base.trim())
    }

    pub(super) fn struct_fields(&self, name: &str) -> Option<&HashMap<String, WaveType>> {
        self.structs.get(name).or_else(|| self.structs.get(self.named_type_base(name)))
    }

    pub(super) fn generic_substitution(&self, name: &str) -> HashMap<String, WaveType> {
        let Some((base, arguments)) = parse_named_type_application(name) else {
            return HashMap::new();
        };
        let Some(parameters) = self
            .struct_generic_params
            .get(&base)
            .or_else(|| self.variant_generic_params.get(&base))
        else {
            return HashMap::new();
        };
        parameters.iter().cloned().zip(arguments).collect()
    }

    pub(super) fn struct_field_type(&self, owner: &str, field: &str) -> Option<WaveType> {
        let ty = self.struct_fields(owner)?.get(field)?;
        Some(substitute_wave_type(ty, &self.generic_substitution(owner)))
    }

    pub(super) fn method_type(&self, owner: &str, name: &str) -> Option<FunctionType> {
        let signature = self.methods.get(&(owner.to_string(), name.to_string())).or_else(|| {
            self.methods.get(&(self.named_type_base(owner).to_string(), name.to_string()))
        })?;
        Some(substitute_function_type(signature, &self.generic_substitution(owner)))
    }

    pub(super) fn is_generic_placeholder(&self, ty: &WaveType) -> bool {
        matches!(ty, WaveType::Struct(name) if self.generic_type_params.contains(name))
    }

    pub(super) fn variant_type(&self, name: &str) -> Option<&VariantType> {
        self.variants.get(name).or_else(|| self.variants.get(self.named_type_base(name)))
    }

    pub(super) fn variant_constructor<'b>(&self, name: &'b str) -> Option<(&'b str, &'b str)> {
        let (owner, case) = name.rsplit_once("::")?;
        self.variant_type(owner)?;
        Some((owner, case))
    }

    pub(super) fn variant_case(&self, owner: &str, case: &str) -> Option<(u32, Vec<WaveType>)> {
        let definition = self.variant_type(owner)?;
        let substitution = self.generic_substitution(owner);
        definition.cases.iter().enumerate().find(|(_, (name, _))| name == case).map(
            |(index, (_, payloads))| {
                (
                    index as u32,
                    payloads
                        .iter()
                        .map(|ty| self.canonical_type(&substitute_wave_type(ty, &substitution)))
                        .collect(),
                )
            },
        )
    }

    pub(super) fn validate_type(
        &self,
        ty: &WaveType,
        generic_params: &HashSet<String>,
        allow_void: bool,
        context: &str,
    ) -> Result<(), String> {
        match ty {
            WaveType::Isz | WaveType::Usz => Err(format!("{context}: target-sized integer requires target resolution before semantic analysis")),
            WaveType::Never if !allow_void => Err(format!("{context} cannot use the return-only `!` type")),
            WaveType::Void if !allow_void => Err(format!("{} cannot use the `void` type", context)),
            WaveType::Future(inner) => self.validate_type(inner, generic_params, true, context),
            WaveType::Pointer(inner) | WaveType::Array(inner, _) => {
                self.validate_type(inner, generic_params, false, context)
            }
            WaveType::Struct(name) | WaveType::Variant(name) => {
                if matches!(ty, WaveType::Struct(_)) && generic_params.contains(name) {
                    return Ok(());
                }
                let base = self.named_type_base(name);
                if !self.is_known_named_type(name) {
                    return Err(format!("unknown type `{}` in {}", name, context));
                }
                let expected_arity = self
                    .struct_generic_params
                    .get(base)
                    .or_else(|| self.variant_generic_params.get(base))
                    .map_or(0, Vec::len);
                let arguments = parse_named_type_application(name)
                    .map(|(_, arguments)| arguments)
                    .unwrap_or_default();
                if arguments.len() != expected_arity {
                    return Err(format!(
                        "type `{}` expects {} generic argument(s), found {} in {}",
                        base,
                        expected_arity,
                        arguments.len(),
                        context
                    ));
                }
                for argument in &arguments {
                    self.validate_type(argument, generic_params, false, context)?;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    pub(super) fn canonical_type(&self, ty: &WaveType) -> WaveType {
        self.canonical_type_inner(ty, &mut HashSet::new())
    }

    pub(super) fn canonical_type_inner(
        &self,
        ty: &WaveType,
        seen: &mut HashSet<String>,
    ) -> WaveType {
        match ty {
            WaveType::Struct(name) => {
                if !seen.insert(name.clone()) {
                    return ty.clone();
                }
                let resolved = if let Some(target) =
                    self.aliases.get(name).or_else(|| self.enum_reprs.get(name))
                {
                    self.canonical_type_inner(target, seen)
                } else if let Some((base, arguments)) = parse_named_type_application(name) {
                    let arguments = arguments
                        .iter()
                        .map(|argument| {
                            display_wave_type(&self.canonical_type_inner(argument, seen))
                        })
                        .collect::<Vec<_>>()
                        .join(",");
                    let name = format!("{}<{}>", base, arguments);
                    if self.variants.contains_key(&base) {
                        WaveType::Variant(name)
                    } else {
                        WaveType::Struct(name)
                    }
                } else if self.variants.contains_key(name) {
                    WaveType::Variant(name.clone())
                } else {
                    ty.clone()
                };
                seen.remove(name);
                resolved
            },
            WaveType::Future(inner) => {
                WaveType::Future(Box::new(self.canonical_type_inner(inner, seen)))
            },
            WaveType::Pointer(inner) => {
                WaveType::Pointer(Box::new(self.canonical_type_inner(inner, seen)))
            },
            WaveType::Array(inner, size) => {
                WaveType::Array(Box::new(self.canonical_type_inner(inner, seen)), *size)
            },
            WaveType::Variant(name) => WaveType::Variant(name.clone()),
            _ => ty.clone(),
        }
    }
}

fn insert_unique_value_name(names: &mut HashSet<String>, name: &str) -> Result<(), String> {
    if names.insert(name.to_string()) {
        Ok(())
    } else {
        Err(format!("duplicate value declaration `{}`", name))
    }
}

fn insert_unique_function(
    functions: &mut HashMap<String, FunctionType>,
    name: &str,
    signature: FunctionType,
) -> Result<(), String> {
    if functions.insert(name.to_string(), signature).is_none() {
        Ok(())
    } else {
        Err(format!("duplicate function declaration `{}`", name))
    }
}

fn insert_unique_method(
    methods: &mut HashMap<(String, String), FunctionType>,
    owner: &str,
    name: &str,
    signature: FunctionType,
) -> Result<(), String> {
    if methods.insert((owner.to_string(), name.to_string()), signature).is_none() {
        Ok(())
    } else {
        Err(format!("duplicate method `{}.{}`", owner, name))
    }
}

pub(super) fn parse_integer_value(raw: &str) -> Option<i128> {
    lexer::number::IntegerLiteral::parse(raw)?.to_i128()
}

pub(super) fn function_type(function: &FunctionNode) -> FunctionType {
    FunctionType {
        defaults: function.parameters.iter().map(|p| p.initial_value.clone()).collect(),
        params: function.parameters.iter().map(|parameter| parameter.param_type.clone()).collect(),
        required_params: function
            .parameters
            .iter()
            .filter(|parameter| parameter.initial_value.is_none())
            .count(),
        return_type: if function.is_async {
            WaveType::Future(Box::new(function.return_type.clone().unwrap_or(WaveType::Void)))
        } else {
            function.return_type.clone().unwrap_or(WaveType::Void)
        },
        generic_params: function.generic_params.clone(),
        variadic: false,
    }
}

pub(super) fn parse_named_type_application(name: &str) -> Option<(String, Vec<WaveType>)> {
    let (base, tail) = name.split_once('<')?;
    let inner = tail.strip_suffix('>')?;
    let arguments = split_top_level_generic_args(inner)?
        .into_iter()
        .map(|argument| {
            let token = parse_type(&argument)?;
            token_type_to_wave_type(&token)
        })
        .collect::<Option<Vec<_>>>()?;
    Some((base.trim().to_string(), arguments))
}

pub(super) fn substitute_wave_type(
    ty: &WaveType,
    substitutions: &HashMap<String, WaveType>,
) -> WaveType {
    match ty {
        WaveType::Struct(name) => {
            if let Some(substitution) = substitutions.get(name) {
                return substitution.clone();
            }
            if let Some((base, arguments)) = parse_named_type_application(name) {
                let arguments = arguments
                    .iter()
                    .map(|argument| {
                        display_wave_type(&substitute_wave_type(argument, substitutions))
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                WaveType::Struct(format!("{}<{}>", base, arguments))
            } else {
                ty.clone()
            }
        },
        WaveType::Future(inner) => {
            WaveType::Future(Box::new(substitute_wave_type(inner, substitutions)))
        },
        WaveType::Pointer(inner) => {
            WaveType::Pointer(Box::new(substitute_wave_type(inner, substitutions)))
        },
        WaveType::Array(inner, size) => {
            WaveType::Array(Box::new(substitute_wave_type(inner, substitutions)), *size)
        },
        WaveType::Variant(name) => {
            if let Some((base, arguments)) = parse_named_type_application(name) {
                let arguments = arguments
                    .iter()
                    .map(|argument| {
                        display_wave_type(&substitute_wave_type(argument, substitutions))
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                WaveType::Variant(format!("{}<{}>", base, arguments))
            } else {
                ty.clone()
            }
        },
        _ => ty.clone(),
    }
}

pub(super) fn substitute_function_type(
    signature: &FunctionType,
    substitutions: &HashMap<String, WaveType>,
) -> FunctionType {
    FunctionType {
        defaults: signature.defaults.clone(),
        params: signature
            .params
            .iter()
            .map(|parameter| substitute_wave_type(parameter, substitutions))
            .collect(),
        required_params: signature.required_params,
        return_type: substitute_wave_type(&signature.return_type, substitutions),
        generic_params: signature.generic_params.clone(),
        variadic: signature.variadic,
    }
}
