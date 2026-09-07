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

//! Whole-program semantic validation and expression type analysis.
//!
//! The verifier runs after imports and generics have been expanded. It first
//! collects declarations into a program-wide type environment, then validates
//! bodies with lexical scopes and expected types. Physical diagnostics use the
//! detached AST source map; legacy unlocated callers retain descriptive hints.

use crate::ast::{
    ASTNode, AssignOperator, Expression, FunctionNode, IncDecKind, Literal, MatchPattern,
    Mutability, Operator, StatementNode, WaveType,
};
use crate::hir::{HirExpressionType, HirVariantConstruction, HirVariantPattern};
use crate::types::{parse_type, split_top_level_generic_args, token_type_to_wave_type};
use std::collections::{HashMap, HashSet};
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SemanticSpanKind {
    Declaration,
    Keyword,
    Identifier,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SemanticSpanHint {
    pub kind: SemanticSpanKind,
    pub text: String,
    pub occurrence: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SemanticDiagnostic {
    pub code: String,
    pub message: String,
    pub top_level_index: usize,
    pub primary: Option<SemanticSpanHint>,
    pub span: Option<error::SourceSpan>,
    pub label: String,
    pub note: Option<String>,
    pub help: String,
}

impl fmt::Display for SemanticDiagnostic {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.message)
    }
}

impl std::error::Error for SemanticDiagnostic {}

#[derive(Clone, Debug)]
struct Binding {
    mutability: Mutability,
    ty: WaveType,
}

#[derive(Clone, Debug)]
struct FunctionType {
    params: Vec<WaveType>,
    required_params: usize,
    return_type: WaveType,
    generic_params: Vec<String>,
    variadic: bool,
}

#[derive(Clone, Debug)]
struct VariantType {
    generic_params: Vec<String>,
    cases: Vec<(String, Vec<WaveType>)>,
}

#[derive(Clone, Debug)]
enum ExpressionType {
    // Literal and null states stay distinct until an expected type supplies the
    // width, signedness, element type, or pointer pointee required to commit.
    Known(WaveType),
    IntLiteral(String),
    FloatLiteral,
    Null,
    ArrayLiteral(Vec<ExpressionType>),
    AddressedArrayLiteral(Vec<ExpressionType>),
    Unknown,
}

#[derive(Default)]
struct ProgramTypes {
    functions: HashMap<String, FunctionType>,
    methods: HashMap<(String, String), FunctionType>,
    structs: HashMap<String, HashMap<String, WaveType>>,
    aliases: HashMap<String, WaveType>,
    enum_reprs: HashMap<String, WaveType>,
    globals: HashMap<String, Binding>,
    constant_values: HashMap<String, i128>,
    type_names: HashSet<String>,
    generic_type_params: HashSet<String>,
    struct_generic_params: HashMap<String, Vec<String>>,
    variants: HashMap<String, VariantType>,
    variant_generic_params: HashMap<String, Vec<String>>,
}

impl ProgramTypes {
    fn collect(nodes: &[ASTNode]) -> Result<Self, (usize, String, Option<SemanticSpanHint>)> {
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
                    out.generic_type_params
                        .extend(function.generic_params.iter().cloned());
                }
                ASTNode::Struct(structure) => {
                    out.generic_type_params
                        .extend(structure.generic_params.iter().cloned());
                    for method in &structure.methods {
                        out.generic_type_params
                            .extend(method.generic_params.iter().cloned());
                    }
                }
                ASTNode::Variant(variant) => {
                    out.generic_type_params
                        .extend(variant.generic_params.iter().cloned());
                }
                ASTNode::ProtoImpl(implementation) => {
                    for method in &implementation.methods {
                        out.generic_type_params
                            .extend(method.generic_params.iter().cloned());
                    }
                }
                _ => {}
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
                }
                ASTNode::ExternFunction(function) => {
                    insert_unique_value_name(&mut value_names, &function.name)
                        .map_err(|message| failure(message, Some(top_level_span_hint(node))))?;
                    insert_unique_function(
                        &mut out.functions,
                        &function.name,
                        FunctionType {
                            params: function.params.iter().map(|(_, ty)| ty.clone()).collect(),
                            required_params: function.params.len(),
                            return_type: function.return_type.clone(),
                            generic_params: Vec::new(),
                            variadic: function.variadic,
                        },
                    )
                    .map_err(|message| failure(message, Some(top_level_span_hint(node))))?;
                }
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
                }
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
                        let lowered = format!("{}_{}", implementation.target, method.name);
                        insert_unique_value_name(&mut value_names, &lowered)
                            .map_err(|message| failure(message, Some(top_level_span_hint(node))))?;
                        insert_unique_function(&mut out.functions, &lowered, signature)
                            .map_err(|message| failure(message, Some(top_level_span_hint(node))))?;
                    }
                }
                ASTNode::TypeAlias(alias) => {
                    out.aliases.insert(alias.name.clone(), alias.target.clone());
                }
                ASTNode::Variable(variable)
                    if matches!(variable.mutability, Mutability::Const | Mutability::Static) =>
                {
                    insert_unique_value_name(&mut value_names, &variable.name)
                        .map_err(|message| failure(message, Some(top_level_span_hint(node))))?;
                    out.globals.insert(
                        variable.name.clone(),
                        Binding {
                            mutability: variable.mutability,
                            ty: variable.type_name.clone(),
                        },
                    );
                    if matches!(variable.mutability, Mutability::Const) {
                        if let Some(Expression::Literal(Literal::Int(raw))) =
                            &variable.initial_value
                        {
                            if let Some(value) = parse_integer_value(raw) {
                                out.constant_values.insert(variable.name.clone(), value);
                            }
                        }
                    }
                }
                ASTNode::Enum(enumeration) => {
                    out.enum_reprs
                        .insert(enumeration.name.clone(), enumeration.repr_type.clone());
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
                        out.constant_values.insert(variant.name.clone(), next);
                        next = next.checked_add(1).ok_or_else(|| {
                            failure(
                                format!("enum `{}` value overflow", enumeration.name),
                                Some(top_level_span_hint(node)),
                            )
                        })?;
                    }
                }
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
                        VariantType {
                            generic_params: variant.generic_params.clone(),
                            cases,
                        },
                    );
                }
                _ => {}
            }
        }

        Ok(out)
    }

    fn is_known_named_type(&self, name: &str) -> bool {
        self.type_names.contains(name)
            || name
                .split_once('<')
                .is_some_and(|(base, _)| self.type_names.contains(base.trim()))
    }

    fn named_type_base<'a>(&self, name: &'a str) -> &'a str {
        name.split_once('<').map_or(name, |(base, _)| base.trim())
    }

    fn struct_fields(&self, name: &str) -> Option<&HashMap<String, WaveType>> {
        self.structs
            .get(name)
            .or_else(|| self.structs.get(self.named_type_base(name)))
    }

    fn generic_substitution(&self, name: &str) -> HashMap<String, WaveType> {
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

    fn struct_field_type(&self, owner: &str, field: &str) -> Option<WaveType> {
        let ty = self.struct_fields(owner)?.get(field)?;
        Some(substitute_wave_type(ty, &self.generic_substitution(owner)))
    }

    fn method_type(&self, owner: &str, name: &str) -> Option<FunctionType> {
        let signature = self
            .methods
            .get(&(owner.to_string(), name.to_string()))
            .or_else(|| {
                self.methods
                    .get(&(self.named_type_base(owner).to_string(), name.to_string()))
            })?;
        Some(substitute_function_type(
            signature,
            &self.generic_substitution(owner),
        ))
    }

    fn is_generic_placeholder(&self, ty: &WaveType) -> bool {
        matches!(ty, WaveType::Struct(name) if self.generic_type_params.contains(name))
    }

    fn variant_type(&self, name: &str) -> Option<&VariantType> {
        self.variants
            .get(name)
            .or_else(|| self.variants.get(self.named_type_base(name)))
    }

    fn variant_constructor<'b>(&self, name: &'b str) -> Option<(&'b str, &'b str)> {
        let (owner, case) = name.rsplit_once("::")?;
        self.variant_type(owner)?;
        Some((owner, case))
    }

    fn variant_case(&self, owner: &str, case: &str) -> Option<(u32, Vec<WaveType>)> {
        let definition = self.variant_type(owner)?;
        let substitution = self.generic_substitution(owner);
        definition
            .cases
            .iter()
            .enumerate()
            .find(|(_, (name, _))| name == case)
            .map(|(index, (_, payloads))| {
                (
                    index as u32,
                    payloads
                        .iter()
                        .map(|ty| self.canonical_type(&substitute_wave_type(ty, &substitution)))
                        .collect(),
                )
            })
    }

    fn validate_type(
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

    fn canonical_type(&self, ty: &WaveType) -> WaveType {
        self.canonical_type_inner(ty, &mut HashSet::new())
    }

    fn canonical_type_inner(&self, ty: &WaveType, seen: &mut HashSet<String>) -> WaveType {
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
            }
            WaveType::Pointer(inner) => {
                WaveType::Pointer(Box::new(self.canonical_type_inner(inner, seen)))
            }
            WaveType::Array(inner, size) => {
                WaveType::Array(Box::new(self.canonical_type_inner(inner, seen)), *size)
            }
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
    if methods
        .insert((owner.to_string(), name.to_string()), signature)
        .is_none()
    {
        Ok(())
    } else {
        Err(format!("duplicate method `{}.{}`", owner, name))
    }
}

fn parse_integer_value(raw: &str) -> Option<i128> {
    lexer::number::IntegerLiteral::parse(raw)?.to_i128()
}

fn function_type(function: &FunctionNode) -> FunctionType {
    FunctionType {
        params: function
            .parameters
            .iter()
            .map(|parameter| parameter.param_type.clone())
            .collect(),
        required_params: function
            .parameters
            .iter()
            .filter(|parameter| parameter.initial_value.is_none())
            .count(),
        return_type: function.return_type.clone().unwrap_or(WaveType::Void),
        generic_params: function.generic_params.clone(),
        variadic: false,
    }
}

fn parse_named_type_application(name: &str) -> Option<(String, Vec<WaveType>)> {
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

fn substitute_wave_type(ty: &WaveType, substitutions: &HashMap<String, WaveType>) -> WaveType {
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
        }
        WaveType::Pointer(inner) => {
            WaveType::Pointer(Box::new(substitute_wave_type(inner, substitutions)))
        }
        WaveType::Array(inner, size) => {
            WaveType::Array(Box::new(substitute_wave_type(inner, substitutions)), *size)
        }
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
        }
        _ => ty.clone(),
    }
}

fn substitute_function_type(
    signature: &FunctionType,
    substitutions: &HashMap<String, WaveType>,
) -> FunctionType {
    FunctionType {
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

fn infer_variant_substitution(
    program: &ProgramTypes,
    template: &WaveType,
    actual: &ExpressionType,
    generic_params: &[String],
    substitutions: &mut HashMap<String, WaveType>,
) -> Result<(), String> {
    let actual = match actual {
        ExpressionType::Known(ty) => program.canonical_type(ty),
        ExpressionType::IntLiteral(_) => WaveType::Int(32),
        ExpressionType::FloatLiteral => WaveType::Float(32),
        _ => return Ok(()),
    };
    infer_variant_type_pair(program, template, &actual, generic_params, substitutions)
}

fn infer_variant_type_pair(
    program: &ProgramTypes,
    template: &WaveType,
    actual: &WaveType,
    generic_params: &[String],
    substitutions: &mut HashMap<String, WaveType>,
) -> Result<(), String> {
    if let WaveType::Struct(name) = template {
        if generic_params.contains(name) {
            if let Some(previous) = substitutions.get(name) {
                if program.canonical_type(previous) != program.canonical_type(actual) {
                    return Err(format!(
                        "conflicting inferred types `{}` and `{}` for variant generic `{}`",
                        display_wave_type(previous),
                        display_wave_type(actual),
                        name
                    ));
                }
            } else {
                substitutions.insert(name.clone(), actual.clone());
            }
            return Ok(());
        }
    }

    match (template, actual) {
        (WaveType::Pointer(template), WaveType::Pointer(actual))
        | (WaveType::Array(template, _), WaveType::Array(actual, _)) => {
            infer_variant_type_pair(program, template, actual, generic_params, substitutions)
        }
        (WaveType::Struct(template_name), WaveType::Struct(actual_name))
        | (WaveType::Struct(template_name), WaveType::Variant(actual_name))
        | (WaveType::Variant(template_name), WaveType::Struct(actual_name))
        | (WaveType::Variant(template_name), WaveType::Variant(actual_name)) => {
            let Some((template_base, template_args)) = parse_named_type_application(template_name)
            else {
                return Ok(());
            };
            let Some((actual_base, actual_args)) = parse_named_type_application(actual_name) else {
                return Ok(());
            };
            if template_base != actual_base || template_args.len() != actual_args.len() {
                return Ok(());
            }
            for (template_arg, actual_arg) in template_args.iter().zip(&actual_args) {
                infer_variant_type_pair(
                    program,
                    template_arg,
                    actual_arg,
                    generic_params,
                    substitutions,
                )?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

struct Validator<'a> {
    program: &'a ProgramTypes,
    scopes: Vec<HashMap<String, Binding>>,
    current_function: Option<String>,
    current_return_type: Option<WaveType>,
    current_type_params: HashSet<String>,
    loop_depth: usize,
    top_level_index: usize,
    span_counts: HashMap<(SemanticSpanKind, String), usize>,
    primary_span: Option<SemanticSpanHint>,
    source_map: &'a crate::source::SourceMap,
    source_span: Option<error::SourceSpan>,
    diagnostic_help: Option<String>,
    expression_types: HashMap<usize, WaveType>,
    hir_expression_types: HashMap<usize, HirExpressionType>,
    hir_variant_constructions: HashMap<usize, HirVariantConstruction>,
    hir_variant_patterns: HashMap<usize, HirVariantPattern>,
}

impl<'a> Validator<'a> {
    fn new(program: &'a ProgramTypes, source_map: &'a crate::source::SourceMap) -> Self {
        Self {
            program,
            scopes: vec![HashMap::new()],
            current_function: None,
            current_return_type: None,
            current_type_params: HashSet::new(),
            loop_depth: 0,
            top_level_index: 0,
            span_counts: HashMap::new(),
            primary_span: None,
            source_map,
            source_span: None,
            diagnostic_help: None,
            expression_types: HashMap::new(),
            hir_expression_types: HashMap::new(),
            hir_variant_constructions: HashMap::new(),
            hir_variant_patterns: HashMap::new(),
        }
    }

    fn begin_top_level(&mut self, index: usize, hint: SemanticSpanHint) {
        self.top_level_index = index;
        self.span_counts.clear();
        self.primary_span = Some(hint);
        self.diagnostic_help = None;
    }

    fn mark_span(&mut self, kind: SemanticSpanKind, text: impl Into<String>) {
        let text = text.into();
        let occurrence = self
            .span_counts
            .entry((kind.clone(), text.clone()))
            .and_modify(|count| *count += 1)
            .or_insert(1);
        self.primary_span = Some(SemanticSpanHint {
            kind,
            text,
            occurrence: *occurrence,
        });
    }

    fn diagnostic(&self, message: String) -> SemanticDiagnostic {
        SemanticDiagnostic {
            code: "E3001".to_string(),
            label: message.clone(),
            message,
            top_level_index: self.top_level_index,
            primary: self.primary_span.clone(),
            span: self.source_span.clone(),
            note: None,
            help: self.diagnostic_help.clone().unwrap_or_else(|| {
                "fix type, mutability, scope, and control-flow errors".to_string()
            }),
        }
    }

    fn lookup_binding(&self, name: &str) -> Option<Binding> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
            .or_else(|| self.program.globals.get(name).cloned())
    }

    fn with_scope<T>(
        &mut self,
        validate: impl FnOnce(&mut Self) -> Result<T, String>,
    ) -> Result<T, String> {
        self.scopes.push(HashMap::new());
        let result = validate(self);
        self.scopes.pop();
        result
    }

    fn validate_function(
        &mut self,
        function: &FunctionNode,
        display_name: &str,
        inherited_type_params: &[String],
    ) -> Result<(), String> {
        let mut declared_type_params: HashSet<&str> =
            inherited_type_params.iter().map(String::as_str).collect();
        for param in &function.generic_params {
            if !declared_type_params.insert(param) {
                return Err(format!(
                    "duplicate generic parameter `{}` in function `{}`",
                    param, display_name
                ));
            }
        }
        let previous_function = self.current_function.replace(display_name.to_string());
        let previous_return = self
            .current_return_type
            .replace(function.return_type.clone().unwrap_or(WaveType::Void));
        let previous_loop_depth = std::mem::replace(&mut self.loop_depth, 0);
        let previous_type_params = std::mem::take(&mut self.current_type_params);
        self.current_type_params
            .extend(inherited_type_params.iter().cloned());
        self.current_type_params
            .extend(function.generic_params.iter().cloned());

        for parameter in &function.parameters {
            if let Some(span) = &parameter.span {
                self.source_span = Some(span.clone());
            }
            self.program.validate_type(
                &parameter.param_type,
                &self.current_type_params,
                false,
                &format!(
                    "parameter `{}` of function `{}`",
                    parameter.name, display_name
                ),
            )?;
        }
        let mut saw_default = false;
        for parameter in &function.parameters {
            if let Some(value) = &parameter.initial_value {
                saw_default = true;
                let actual = self.validate_expr_expected(value, Some(&parameter.param_type))?;
                self.require_assignable(
                    &actual,
                    &parameter.param_type,
                    &format!("default for `{}`", parameter.name),
                )?;
            } else if saw_default {
                return Err(format!(
                    "required parameter `{}` follows a default parameter",
                    parameter.name
                ));
            }
        }
        self.source_span = function
            .return_type_span
            .clone()
            .or_else(|| function.span.clone());
        let return_type = function.return_type.clone().unwrap_or(WaveType::Void);
        self.program.validate_type(
            &return_type,
            &self.current_type_params,
            true,
            &format!("return type of function `{}`", display_name),
        )?;

        let result = self.with_scope(|validator| {
            for parameter in &function.parameters {
                validator.insert_current_binding(
                    parameter.name.clone(),
                    Binding {
                        mutability: Mutability::Var,
                        ty: parameter.param_type.clone(),
                    },
                    "parameter",
                )?;
            }

            let falls_through = validator.validate_block(&function.body)?;
            let return_type = function.return_type.clone().unwrap_or(WaveType::Void);
            if return_type != WaveType::Void && falls_through {
                validator.source_span = function.span.clone();
                return Err(format!(
                    "non-void function `{}` may exit without returning `{}`",
                    display_name,
                    display_wave_type(&return_type)
                ));
            }

            Ok(())
        });

        self.current_function = previous_function;
        self.current_return_type = previous_return;
        self.loop_depth = previous_loop_depth;
        self.current_type_params = previous_type_params;
        result
    }

    fn insert_current_binding(
        &mut self,
        name: String,
        binding: Binding,
        kind: &str,
    ) -> Result<(), String> {
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains_key(&name) {
            return Err(format!(
                "duplicate {} declaration `{}` in the same scope",
                kind, name
            ));
        }
        scope.insert(name, binding);
        Ok(())
    }

    fn validate_block(&mut self, nodes: &[ASTNode]) -> Result<bool, String> {
        let mut falls_through = true;
        for node in nodes {
            let node_falls_through = self.validate_node(node)?;
            if falls_through {
                falls_through = node_falls_through;
            }
        }
        Ok(falls_through)
    }

    fn validate_scoped_block(&mut self, nodes: &[ASTNode]) -> Result<bool, String> {
        self.with_scope(|validator| validator.validate_block(nodes))
    }

    fn validate_node(&mut self, node: &ASTNode) -> Result<bool, String> {
        if let Some(span) = self.source_map.nodes.get(&(node as *const _ as usize)) {
            self.source_span = Some(span.clone());
        }
        match node {
            ASTNode::Variable(variable) => {
                self.mark_span(SemanticSpanKind::Declaration, variable.name.clone());
                self.program.validate_type(
                    &variable.type_name,
                    &self.current_type_params,
                    false,
                    &format!("variable `{}`", variable.name),
                )?;
                if self.scopes.last().unwrap().contains_key(&variable.name) {
                    return Err(format!(
                        "duplicate variable declaration `{}` in the same scope",
                        variable.name
                    ));
                }
                let asm_initializer =
                    matches!(variable.initial_value, Some(Expression::AsmBlock { .. }));
                if asm_initializer {
                    self.insert_current_binding(
                        variable.name.clone(),
                        Binding {
                            mutability: variable.mutability,
                            ty: variable.type_name.clone(),
                        },
                        "variable",
                    )?;
                }

                if let Some(initial_value) = &variable.initial_value {
                    let actual =
                        self.validate_expr_expected(initial_value, Some(&variable.type_name))?;
                    self.require_assignable(
                        &actual,
                        &variable.type_name,
                        &format!("initializer for `{}`", variable.name),
                    )?;
                }

                if !asm_initializer {
                    self.insert_current_binding(
                        variable.name.clone(),
                        Binding {
                            mutability: variable.mutability,
                            ty: variable.type_name.clone(),
                        },
                        "variable",
                    )?;
                }
                Ok(true)
            }
            ASTNode::Statement(statement) => self.validate_statement(statement),
            ASTNode::Expression(expression) => {
                let ty = self.validate_expr(expression)?;
                Ok(!matches!(ty, ExpressionType::Known(WaveType::Never)))
            }
            _ => Ok(true),
        }
    }

    fn validate_statement(&mut self, statement: &StatementNode) -> Result<bool, String> {
        match statement {
            StatementNode::Expression(expression) => {
                let ty = self.validate_expr(expression)?;
                Ok(!matches!(ty, ExpressionType::Known(WaveType::Never)))
            }
            StatementNode::Assign { variable, value } => {
                self.mark_span(SemanticSpanKind::Identifier, variable.clone());
                let binding = self
                    .lookup_binding(variable)
                    .ok_or_else(|| format!("use of undeclared identifier `{}`", variable))?;
                self.ensure_mutable_binding(variable, &binding, "assign")?;
                let actual = self.validate_expr_expected(value, Some(&binding.ty))?;
                self.require_assignable(
                    &actual,
                    &binding.ty,
                    &format!("assignment to `{}`", variable),
                )?;
                Ok(true)
            }
            StatementNode::PrintFormat { args, .. } | StatementNode::PrintlnFormat { args, .. } => {
                self.mark_span(SemanticSpanKind::Keyword, "println|print");
                for argument in args {
                    let ty = self.validate_expr(argument)?;
                    self.validate_format_argument(&ty)?;
                }
                Ok(true)
            }
            StatementNode::Input { args, .. } => {
                self.mark_span(SemanticSpanKind::Keyword, "input");
                for argument in args {
                    if !is_lvalue_expression(argument) {
                        return Err("input argument must be a mutable lvalue".to_string());
                    }
                    self.ensure_mutable_write_target(argument, "write input into")?;
                    let ty = self.validate_expr(argument)?;
                    let supported = match &ty {
                        ExpressionType::Known(ty) => matches!(
                            self.program.canonical_type(ty),
                            WaveType::Bool
                                | WaveType::Int(_)
                                | WaveType::Uint(_)
                                | WaveType::Float(_)
                                | WaveType::Char
                                | WaveType::Byte
                        ),
                        _ => false,
                    };
                    if !supported {
                        return Err(format!(
                            "input argument must be a mutable scalar lvalue, found `{}`",
                            display_expression_type(&ty)
                        ));
                    }
                }
                Ok(true)
            }
            StatementNode::If {
                condition,
                body,
                else_if_blocks,
                else_block,
            } => {
                self.mark_span(SemanticSpanKind::Keyword, "if");
                self.validate_condition(condition, "if condition")?;
                let mut any_branch_falls_through = self.validate_scoped_block(body)?;

                if let Some(blocks) = else_if_blocks {
                    for (condition, block) in blocks.iter() {
                        self.mark_span(SemanticSpanKind::Keyword, "if");
                        self.validate_condition(condition, "else-if condition")?;
                        any_branch_falls_through |= self.validate_scoped_block(block)?;
                    }
                }

                if let Some(block) = else_block {
                    any_branch_falls_through |= self.validate_scoped_block(block)?;
                } else {
                    any_branch_falls_through = true;
                }

                Ok(any_branch_falls_through)
            }
            StatementNode::While { condition, body } => {
                self.mark_span(SemanticSpanKind::Keyword, "while");
                self.validate_condition(condition, "while condition")?;
                self.loop_depth += 1;
                let body_result = self.validate_scoped_block(body);
                self.loop_depth -= 1;
                body_result?;

                Ok(!expression_is_true(condition) || block_breaks_current_loop(body))
            }
            StatementNode::For {
                initialization,
                condition,
                increment,
                body,
            } => self.with_scope(|validator| {
                validator.mark_span(SemanticSpanKind::Keyword, "for");
                validator.validate_node(initialization)?;
                validator.validate_condition(condition, "for condition")?;
                validator.validate_expr(increment)?;
                validator.loop_depth += 1;
                let body_result = validator.validate_block(body);
                validator.loop_depth -= 1;
                body_result?;

                Ok(!expression_is_true(condition) || block_breaks_current_loop(body))
            }),
            StatementNode::Match { value, arms } => {
                self.mark_span(SemanticSpanKind::Keyword, "match");
                let value_type = self.validate_expr(value)?;
                match &value_type {
                    ExpressionType::Known(ty) => match self.program.canonical_type(ty) {
                        WaveType::Variant(name) => self.validate_variant_match(&name, arms),
                        _ if self.is_integer_expression(&value_type) => {
                            self.validate_integer_match(arms)
                        }
                        _ => Err(format!(
                            "match value must be an integer, enum, or variant, found `{}`",
                            display_expression_type(&value_type)
                        )),
                    },
                    _ if self.is_integer_expression(&value_type) => {
                        self.validate_integer_match(arms)
                    }
                    _ => Err(format!(
                        "match value must be an integer, enum, or variant, found `{}`",
                        display_expression_type(&value_type)
                    )),
                }
            }
            StatementNode::Break => {
                self.mark_span(SemanticSpanKind::Keyword, "break");
                if self.loop_depth == 0 {
                    return Err("`break` can only be used inside a loop".to_string());
                }
                Ok(false)
            }
            StatementNode::Continue => {
                self.mark_span(SemanticSpanKind::Keyword, "continue");
                if self.loop_depth == 0 {
                    return Err("`continue` can only be used inside a loop".to_string());
                }
                Ok(false)
            }
            StatementNode::Return(value) => {
                self.mark_span(SemanticSpanKind::Keyword, "return");
                self.validate_return(value.as_ref())?;
                Ok(false)
            }
            StatementNode::AsmBlock {
                inputs,
                outputs,
                clobbers,
                ..
            } => {
                for (_, expression) in inputs.iter().chain(outputs.iter()) {
                    self.validate_expr(expression)?;
                }
                Ok(!clobbers.iter().any(|clobber| clobber == "noreturn"))
            }
            _ => Ok(true),
        }
    }

    fn validate_integer_match(&mut self, arms: &[crate::ast::MatchArm]) -> Result<bool, String> {
        let mut seen = HashSet::new();
        let mut has_wildcard = false;
        let mut all_arms_terminate = !arms.is_empty();

        for arm in arms {
            if let Some(span) = self
                .source_map
                .patterns
                .get(&(&arm.pattern as *const _ as usize))
            {
                self.source_span = Some(span.clone());
            }
            let key = match &arm.pattern {
                MatchPattern::Located { .. } => {
                    unreachable!("source wrappers detached before analysis")
                }
                MatchPattern::Int(raw) => {
                    self.mark_span(SemanticSpanKind::Keyword, raw.clone());
                    let value = parse_integer_value(raw)
                        .ok_or_else(|| format!("invalid integer match case `{}`", raw))?;
                    format!("value:{}", value)
                }
                MatchPattern::Ident(name) => {
                    self.mark_span(SemanticSpanKind::Identifier, name.clone());
                    let binding = self
                        .lookup_binding(name)
                        .ok_or_else(|| format!("unknown match case constant `{}`", name))?;
                    if !matches!(binding.mutability, Mutability::Const)
                        || !is_integer_type(&self.program.canonical_type(&binding.ty))
                    {
                        return Err(format!(
                            "match case `{}` must name an integer or enum constant",
                            name
                        ));
                    }
                    let value = self.program.constant_values.get(name).ok_or_else(|| {
                        format!(
                            "match case `{}` does not have a compile-time integer value",
                            name
                        )
                    })?;
                    format!("value:{}", value)
                }
                MatchPattern::Wildcard => {
                    self.mark_span(SemanticSpanKind::Keyword, "_");
                    has_wildcard = true;
                    "wildcard:_".to_string()
                }
                MatchPattern::Binding(_) | MatchPattern::Variant { .. } => {
                    return Err("variant patterns require a variant match value".to_string())
                }
            };
            if !seen.insert(key.clone()) {
                return Err(format!("duplicate match case pattern `{}`", key));
            }
            all_arms_terminate &= !self.validate_scoped_block(&arm.body)?;
        }

        Ok(!(has_wildcard && all_arms_terminate))
    }

    fn validate_variant_match(
        &mut self,
        variant_name: &str,
        arms: &[crate::ast::MatchArm],
    ) -> Result<bool, String> {
        let definition = self.program.variant_type(variant_name).cloned().unwrap();
        let all_cases = definition
            .cases
            .iter()
            .map(|(name, _)| name.clone())
            .collect::<HashSet<_>>();
        let expected_type = WaveType::Variant(variant_name.to_string());
        let mut seen_patterns = HashSet::new();
        let mut covered_patterns: Vec<&MatchPattern> = Vec::new();
        let mut all_arms_terminate = !arms.is_empty();

        for arm in arms {
            if let Some(span) = self
                .source_map
                .patterns
                .get(&(&arm.pattern as *const _ as usize))
            {
                self.source_span = Some(span.clone());
            }
            if self.variant_patterns_cover(&covered_patterns, &expected_type) {
                return Err(
                    if covered_patterns
                        .iter()
                        .any(|pattern| matches!(pattern, MatchPattern::Wildcard))
                    {
                        "match pattern after wildcard is unreachable".to_string()
                    } else {
                        "match pattern is unreachable because previous variant patterns are exhaustive"
                        .to_string()
                    },
                );
            }
            let mut bindings = HashMap::new();
            match &arm.pattern {
                MatchPattern::Located { .. } => {
                    unreachable!("source wrappers detached before analysis")
                }
                MatchPattern::Wildcard => {}
                MatchPattern::Variant {
                    variant_type,
                    case_name,
                    ..
                } => {
                    if self.program.named_type_base(variant_type)
                        != self.program.named_type_base(variant_name)
                    {
                        return Err(format!(
                            "case `{}::{}` does not belong to variant `{}`",
                            variant_type, case_name, variant_name
                        ));
                    }
                    let key = variant_pattern_key(&arm.pattern);
                    if !seen_patterns.insert(key) {
                        return Err(format!(
                            "duplicate variant case `{}::{}` in match",
                            variant_type, case_name
                        ));
                    }
                    self.collect_variant_pattern_bindings(
                        &arm.pattern,
                        &expected_type,
                        &mut bindings,
                    )?;
                }
                MatchPattern::Int(_) | MatchPattern::Ident(_) | MatchPattern::Binding(_) => {
                    return Err(format!(
                        "match on variant `{}` requires a qualified case pattern or `_`",
                        variant_name
                    ))
                }
            }

            let arm_falls_through = self.with_scope(|validator| {
                for (name, ty) in bindings {
                    validator.insert_current_binding(
                        name,
                        Binding {
                            mutability: Mutability::Var,
                            ty,
                        },
                        "pattern binding",
                    )?;
                }
                validator.validate_block(&arm.body)
            })?;
            all_arms_terminate &= !arm_falls_through;
            covered_patterns.push(&arm.pattern);
        }

        let exhaustive = self.variant_patterns_cover(&covered_patterns, &expected_type);
        if !exhaustive {
            let mut missing = all_cases
                .iter()
                .filter(|case| {
                    !covered_patterns.iter().any(|pattern| {
                        matches!(
                            pattern,
                            MatchPattern::Variant { case_name, .. } if case_name == *case
                        )
                    })
                })
                .cloned()
                .collect::<Vec<_>>();
            missing.sort();
            if missing.is_empty() {
                return Err(format!(
                    "non-exhaustive match on variant `{}`; patterns do not cover all payload shapes",
                    variant_name
                ));
            }
            return Err(format!(
                "non-exhaustive match on variant `{}`; missing case(s): {}",
                variant_name,
                missing.join(", ")
            ));
        }

        Ok(!(exhaustive && all_arms_terminate))
    }

    fn collect_variant_pattern_bindings(
        &mut self,
        pattern: &MatchPattern,
        expected: &WaveType,
        bindings: &mut HashMap<String, WaveType>,
    ) -> Result<(), String> {
        match pattern {
            MatchPattern::Located { .. } => {
                unreachable!("source wrappers detached before analysis")
            }
            MatchPattern::Binding(name) => {
                if bindings.insert(name.clone(), expected.clone()).is_some() {
                    return Err(format!("duplicate pattern binding `{}`", name));
                }
                Ok(())
            }
            MatchPattern::Wildcard => Ok(()),
            MatchPattern::Variant {
                variant_type,
                case_name,
                payloads,
            } => {
                let WaveType::Variant(expected_name) = self.program.canonical_type(expected) else {
                    return Err(format!(
                        "nested variant pattern `{}::{}` cannot match payload type `{}`",
                        variant_type,
                        case_name,
                        display_wave_type(expected)
                    ));
                };
                if self.program.named_type_base(variant_type)
                    != self.program.named_type_base(&expected_name)
                {
                    return Err(format!(
                        "case `{}::{}` does not belong to payload variant `{}`",
                        variant_type, case_name, expected_name
                    ));
                }
                let (discriminant, payload_types) = self
                    .program
                    .variant_case(&expected_name, case_name)
                    .ok_or_else(|| {
                        format!(
                            "unknown case `{}` in variant `{}`",
                            case_name, expected_name
                        )
                    })?;
                if payloads.len() != payload_types.len() {
                    return Err(format!(
                        "variant pattern `{}::{}` expects {} payload pattern(s), found {}",
                        variant_type,
                        case_name,
                        payload_types.len(),
                        payloads.len()
                    ));
                }
                self.hir_variant_patterns.insert(
                    pattern as *const MatchPattern as usize,
                    HirVariantPattern {
                        variant_type: WaveType::Variant(expected_name.clone()),
                        case_name: case_name.clone(),
                        discriminant,
                        payload_types: payload_types.clone(),
                    },
                );
                for (payload, ty) in payloads.iter().zip(&payload_types) {
                    self.collect_variant_pattern_bindings(payload, ty, bindings)?;
                }
                Ok(())
            }
            MatchPattern::Int(_) | MatchPattern::Ident(_) => {
                Err("variant payload patterns support bindings, `_`, or nested cases".to_string())
            }
        }
    }

    fn variant_pattern_is_irrefutable(&self, pattern: &MatchPattern, expected: &WaveType) -> bool {
        match pattern {
            MatchPattern::Located { .. } => {
                unreachable!("source wrappers detached before analysis")
            }
            MatchPattern::Binding(_) | MatchPattern::Wildcard => true,
            MatchPattern::Variant {
                variant_type,
                case_name,
                payloads,
            } => {
                let WaveType::Variant(expected_name) = self.program.canonical_type(expected) else {
                    return false;
                };
                if self.program.named_type_base(variant_type)
                    != self.program.named_type_base(&expected_name)
                {
                    return false;
                }
                let Some(definition) = self.program.variant_type(&expected_name) else {
                    return false;
                };
                if definition.cases.len() != 1 || definition.cases[0].0 != *case_name {
                    return false;
                }
                let Some((_, payload_types)) = self.program.variant_case(&expected_name, case_name)
                else {
                    return false;
                };
                payloads.len() == payload_types.len()
                    && payloads
                        .iter()
                        .zip(&payload_types)
                        .all(|(payload, ty)| self.variant_pattern_is_irrefutable(payload, ty))
            }
            MatchPattern::Int(_) | MatchPattern::Ident(_) => false,
        }
    }

    fn variant_patterns_cover(&self, patterns: &[&MatchPattern], expected: &WaveType) -> bool {
        if patterns
            .iter()
            .any(|pattern| matches!(pattern, MatchPattern::Binding(_) | MatchPattern::Wildcard))
        {
            return true;
        }
        let WaveType::Variant(expected_name) = self.program.canonical_type(expected) else {
            return false;
        };
        let Some(definition) = self.program.variant_type(&expected_name) else {
            return false;
        };
        definition.cases.iter().all(|(case_name, _)| {
            let case_patterns = patterns
                .iter()
                .filter_map(|pattern| match pattern {
                    MatchPattern::Variant {
                        variant_type,
                        case_name: pattern_case,
                        payloads,
                    } if self.program.named_type_base(variant_type)
                        == self.program.named_type_base(&expected_name)
                        && pattern_case == case_name =>
                    {
                        Some(payloads)
                    }
                    _ => None,
                })
                .collect::<Vec<_>>();
            if case_patterns.is_empty() {
                return false;
            }
            let Some((_, payload_types)) = self.program.variant_case(&expected_name, case_name)
            else {
                return false;
            };
            if payload_types.is_empty() {
                return true;
            }
            if payload_types.len() == 1 {
                let nested = case_patterns
                    .iter()
                    .filter_map(|payloads| payloads.first())
                    .collect::<Vec<_>>();
                return self.variant_patterns_cover(&nested, &payload_types[0]);
            }
            case_patterns.iter().any(|payloads| {
                payloads.len() == payload_types.len()
                    && payloads
                        .iter()
                        .zip(&payload_types)
                        .all(|(pattern, ty)| self.variant_pattern_is_irrefutable(pattern, ty))
            })
        })
    }

    fn validate_return(&mut self, value: Option<&Expression>) -> Result<(), String> {
        let function = self
            .current_function
            .clone()
            .unwrap_or_else(|| "<unknown function>".to_string());
        let expected = self.current_return_type.clone().unwrap_or(WaveType::Void);

        match (expected, value) {
            (WaveType::Never, _) => Err(format!(
                "never-returning function `{function}` cannot contain a return statement"
            )),
            (WaveType::Void, None) => Ok(()),
            (WaveType::Void, Some(_)) => Err(format!(
                "void function `{}` cannot return a value",
                function
            )),
            (expected, None) => Err(format!(
                "non-void function `{}` must return `{}`",
                function,
                display_wave_type(&expected)
            )),
            (expected, Some(expression)) => {
                let actual = self.validate_expr_expected(expression, Some(&expected))?;
                self.require_assignable(
                    &actual,
                    &expected,
                    &format!("return value of function `{}`", function),
                )
            }
        }
    }

    fn validate_condition(&mut self, expression: &Expression, context: &str) -> Result<(), String> {
        if let Some(mutation) = condition_mutation(expression) {
            self.diagnostic_help = Some(mutation.help().to_string());
            return Err(format!(
                "{} `{}` is not allowed in {}",
                mutation.description(),
                mutation.symbol(),
                context
            ));
        }

        let ty = self.validate_expr(expression)?;
        if self.is_truthy_expression(&ty) {
            return Ok(());
        }

        Err(format!(
            "{} must be bool, numeric, pointer, or string, found `{}`",
            context,
            display_expression_type(&ty)
        ))
    }

    fn validate_format_argument(&self, ty: &ExpressionType) -> Result<(), String> {
        let supported = match ty {
            ExpressionType::IntLiteral(_)
            | ExpressionType::FloatLiteral
            | ExpressionType::Null
            | ExpressionType::Unknown => true,
            ExpressionType::Known(ty) => {
                self.program.is_generic_placeholder(ty)
                    || matches!(
                        self.program.canonical_type(ty),
                        WaveType::Bool
                            | WaveType::Int(_)
                            | WaveType::Uint(_)
                            | WaveType::Float(_)
                            | WaveType::Char
                            | WaveType::Byte
                            | WaveType::String
                            | WaveType::Pointer(_)
                    )
            }
            ExpressionType::ArrayLiteral(_) | ExpressionType::AddressedArrayLiteral(_) => false,
        };
        if supported {
            Ok(())
        } else {
            Err(format!(
                "format argument must be a scalar, string, pointer, or null, found `{}`",
                display_expression_type(ty)
            ))
        }
    }

    fn is_truthy_expression(&self, ty: &ExpressionType) -> bool {
        match ty {
            ExpressionType::IntLiteral(_) | ExpressionType::FloatLiteral => true,
            ExpressionType::Known(ty) => matches!(
                self.program.canonical_type(ty),
                WaveType::Bool
                    | WaveType::Int(_)
                    | WaveType::Uint(_)
                    | WaveType::Float(_)
                    | WaveType::Char
                    | WaveType::Byte
                    | WaveType::String
                    | WaveType::Pointer(_)
            ),
            ExpressionType::Null
            | ExpressionType::ArrayLiteral(_)
            | ExpressionType::AddressedArrayLiteral(_)
            | ExpressionType::Unknown => false,
        }
    }

    fn is_integer_expression(&self, ty: &ExpressionType) -> bool {
        match ty {
            ExpressionType::IntLiteral(_) => true,
            ExpressionType::Known(ty) => is_integer_type(&self.program.canonical_type(ty)),
            _ => false,
        }
    }

    fn validate_expr(&mut self, expression: &Expression) -> Result<ExpressionType, String> {
        self.validate_expr_expected(expression, None)
    }

    fn validate_expr_expected(
        &mut self,
        expression: &Expression,
        expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        if let Some(span) = self
            .source_map
            .expressions
            .get(&(expression as *const _ as usize))
        {
            self.source_span = Some(span.clone());
        }

        let result = self.validate_expr_inner(expression, expected);
        if let Ok(expression_type) = &result {
            self.hir_expression_types.insert(
                expression as *const Expression as usize,
                hir_expression_type(self.program, expression_type),
            );
            if let Some(ty) = canonical_expression_type(self.program, expression_type) {
                self.expression_types
                    .insert(expression as *const Expression as usize, ty);
            }
        }
        result
    }

    fn validate_expr_inner(
        &mut self,
        expression: &Expression,
        expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        match expression {
            Expression::Located { .. } => unreachable!("source wrappers detached before analysis"),
            Expression::Literal(literal) => Ok(match literal {
                Literal::Int(raw) => {
                    if let Some(WaveType::Float(bits)) =
                        expected.map(|ty| self.program.canonical_type(ty))
                    {
                        let value = lexer::number::IntegerLiteral::parse(raw)
                            .and_then(|value| value.to_f64())
                            .ok_or_else(|| {
                                "integer literal is out of range for floating-point conversion"
                                    .to_string()
                            })?;
                        if bits == 32 && !(value as f32).is_finite() {
                            return Err(
                                "integer literal is out of range for f32 conversion".to_string()
                            );
                        }
                    }
                    ExpressionType::IntLiteral(raw.clone())
                }
                Literal::Float(value) => {
                    if !value.is_finite()
                        || (matches!(
                            expected.map(|ty| self.program.canonical_type(ty)),
                            Some(WaveType::Float(32))
                        ) && !(*value as f32).is_finite())
                    {
                        return Err(
                            "floating-point literal is out of range for its expected type"
                                .to_string(),
                        );
                    }
                    ExpressionType::FloatLiteral
                }
                Literal::String(_) => ExpressionType::Known(WaveType::String),
                Literal::Bool(_) => ExpressionType::Known(WaveType::Bool),
                Literal::Char(_) => ExpressionType::Known(WaveType::Char),
                Literal::Byte(_) => ExpressionType::Known(WaveType::Byte),
            }),
            Expression::Null => Ok(ExpressionType::Null),
            Expression::Variable(name) => {
                if self.program.variant_constructor(name).is_some() {
                    return self.validate_variant_constructor(expression, name, &[], expected);
                }
                if let Some(binding) = self.lookup_binding(name) {
                    Ok(ExpressionType::Known(binding.ty))
                } else {
                    self.mark_span(SemanticSpanKind::Identifier, name.clone());
                    Err(format!("use of undeclared identifier `{}`", name))
                }
            }
            Expression::Grouped(inner) => self.validate_expr_expected(inner, expected),
            Expression::Cast { expr, target_type } => {
                self.mark_span(SemanticSpanKind::Keyword, "as");
                self.program.validate_type(
                    target_type,
                    &self.current_type_params,
                    false,
                    "cast target",
                )?;
                let source = self.validate_expr_expected(expr, Some(target_type))?;
                if !self.is_valid_cast(&source, target_type) {
                    return Err(format!(
                        "invalid cast from `{}` to `{}`",
                        display_expression_type(&source),
                        display_wave_type(target_type)
                    ));
                }
                Ok(ExpressionType::Known(target_type.clone()))
            }
            Expression::AddressOf(inner) => {
                self.mark_span(SemanticSpanKind::Keyword, "&");
                if !is_lvalue_expression(inner)
                    && !matches!(inner.as_ref(), Expression::ArrayLiteral(_))
                {
                    return Err("cannot take the address of a non-lvalue expression".to_string());
                }
                if !matches!(inner.as_ref(), Expression::ArrayLiteral(_)) {
                    self.ensure_mutable_write_target(inner, "take the address of")?;
                }
                let inner_type = self.validate_expr(inner)?;
                Ok(match inner_type {
                    ExpressionType::Known(ty) => {
                        ExpressionType::Known(WaveType::Pointer(Box::new(ty)))
                    }
                    ExpressionType::ArrayLiteral(elements) => {
                        ExpressionType::AddressedArrayLiteral(elements)
                    }
                    _ => ExpressionType::Unknown,
                })
            }
            Expression::Deref(inner) => {
                self.mark_span(SemanticSpanKind::Keyword, "deref");
                if matches!(inner.as_ref(), Expression::FieldAccess { .. }) {
                    return self.validate_expr(inner);
                }
                if matches!(inner.as_ref(), Expression::IndexAccess { .. }) {
                    let indexed_type = self.validate_expr(inner)?;
                    return Ok(match indexed_type {
                        ExpressionType::Known(WaveType::Pointer(ty)) => ExpressionType::Known(*ty),
                        other => other,
                    });
                }
                let inner_type = self.validate_expr(inner)?;
                match inner_type {
                    ExpressionType::Known(WaveType::Pointer(ty)) => Ok(ExpressionType::Known(*ty)),
                    other => Err(format!(
                        "deref expects a pointer, found `{}`",
                        display_expression_type(&other)
                    )),
                }
            }
            Expression::BinaryExpression {
                left,
                operator,
                right,
            } => {
                self.mark_span(
                    SemanticSpanKind::Keyword,
                    operator_source_symbol(operator).unwrap_or("binary operator"),
                );
                let left_type = self.validate_expr(left)?;
                let right_type = self.validate_expr(right)?;
                infer_binary_type(self.program, operator, left_type, right_type)
            }
            Expression::Unary { operator, expr } => {
                let ty = self.validate_expr(expr)?;
                self.validate_unary(operator, ty)
            }
            Expression::FunctionCall {
                name,
                type_args,
                args,
            } => {
                self.mark_span(SemanticSpanKind::Identifier, name.clone());
                if self.program.variant_constructor(name).is_some() {
                    if !type_args.is_empty() {
                        return Err(format!(
                            "variant constructor `{}` does not accept function type arguments",
                            name
                        ));
                    }
                    self.validate_variant_constructor(expression, name, args, expected)
                } else {
                    self.validate_function_call(name, type_args, args)
                }
            }
            Expression::MethodCall { object, name, args } => {
                self.mark_span(SemanticSpanKind::Identifier, name.clone());
                self.validate_method_call(object, name, args)
            }
            Expression::StructLiteral { name, fields } => {
                self.mark_span(SemanticSpanKind::Identifier, name.clone());
                let known_fields = self
                    .program
                    .struct_fields(name)
                    .cloned()
                    .ok_or_else(|| format!("unknown struct `{}`", name))?;
                let mut provided = HashSet::new();
                for (field_name, value) in fields {
                    self.mark_span(SemanticSpanKind::Declaration, field_name.clone());
                    if !provided.insert(field_name.as_str()) {
                        return Err(format!(
                            "struct literal `{}` initializes field `{}` more than once",
                            name, field_name
                        ));
                    }
                    let expected = self
                        .program
                        .struct_field_type(name, field_name)
                        .ok_or_else(|| {
                            format!("struct `{}` has no field `{}`", name, field_name)
                        })?;
                    let actual = self.validate_expr_expected(value, Some(&expected))?;
                    self.require_assignable(
                        &actual,
                        &expected,
                        &format!("field `{}.{}`", name, field_name),
                    )?;
                }
                let mut missing: Vec<&str> = known_fields
                    .keys()
                    .map(String::as_str)
                    .filter(|field| !provided.contains(field))
                    .collect();
                missing.sort_unstable();
                if !missing.is_empty() {
                    return Err(format!(
                        "struct literal `{}` is missing field(s): {}",
                        name,
                        missing.join(", ")
                    ));
                }
                Ok(ExpressionType::Known(WaveType::Struct(name.clone())))
            }
            Expression::FieldAccess { object, field } => {
                self.mark_span(SemanticSpanKind::Identifier, field.clone());
                let object_type = self.validate_expr(object)?;
                if let Some(span) = self
                    .source_map
                    .expressions
                    .get(&(expression as *const _ as usize))
                {
                    self.source_span = Some(span.clone());
                }
                let structure = match &object_type {
                    ExpressionType::Known(WaveType::Struct(name)) => Some(name.clone()),
                    ExpressionType::Known(WaveType::Pointer(inner)) => match inner.as_ref() {
                        WaveType::Struct(name) => Some(name.clone()),
                        _ => None,
                    },
                    _ => None,
                };

                let structure = structure.ok_or_else(|| {
                    format!(
                        "field access requires a struct or pointer-to-struct, found `{}`",
                        display_expression_type(&object_type)
                    )
                })?;
                let field_type = self
                    .program
                    .struct_field_type(&structure, field)
                    .ok_or_else(|| format!("struct `{}` has no field `{}`", structure, field))?;
                Ok(ExpressionType::Known(field_type))
            }
            Expression::IndexAccess { target, index } => {
                self.mark_span(SemanticSpanKind::Keyword, "[");
                let target_type = self.validate_expr(target)?;
                let index_type = self.validate_expr(index)?;
                if !self.is_integer_expression(&index_type) {
                    return Err(format!(
                        "index expression must be an integer, found `{}`",
                        display_expression_type(&index_type)
                    ));
                }
                if !is_codegen_supported_index(index) {
                    return Err(
                        "index expression must currently be an integer literal or integer lvalue"
                            .to_string(),
                    );
                }
                match target_type {
                    ExpressionType::Known(WaveType::String) => {
                        Ok(ExpressionType::Known(WaveType::Int(8)))
                    }
                    ExpressionType::Known(WaveType::Array(element, _)) => {
                        Ok(ExpressionType::Known(*element))
                    }
                    ExpressionType::Known(WaveType::Pointer(element)) => match *element {
                        WaveType::Array(array_element, _) => {
                            Ok(ExpressionType::Known(*array_element))
                        }
                        other => Ok(ExpressionType::Known(other)),
                    },
                    other => Err(format!(
                        "index access requires an array or pointer, found `{}`",
                        display_expression_type(&other)
                    )),
                }
            }
            Expression::ArrayLiteral(values) => {
                self.mark_span(SemanticSpanKind::Keyword, "[");
                let expected_element =
                    expected
                        .map(|ty| self.program.canonical_type(ty))
                        .and_then(|ty| match ty {
                            WaveType::Array(element, _) => Some(*element),
                            _ => None,
                        });
                let mut element_types = Vec::with_capacity(values.len());
                for value in values {
                    element_types
                        .push(self.validate_expr_expected(value, expected_element.as_ref())?);
                }
                Ok(ExpressionType::ArrayLiteral(element_types))
            }
            Expression::Assignment { target, value } => {
                self.mark_span(SemanticSpanKind::Keyword, "=");
                if !is_lvalue_expression(target) {
                    return Err("assignment target is not an lvalue".to_string());
                }
                self.ensure_mutable_write_target(target, "assign")?;
                let target_type = self.validate_expr(target)?;
                let value_type = if let ExpressionType::Known(expected) = &target_type {
                    self.validate_expr_expected(value, Some(expected))?
                } else {
                    self.validate_expr(value)?
                };
                if let ExpressionType::Known(expected) = &target_type {
                    let context = find_base_var(target, false)
                        .map(|(name, _)| format!("assignment to `{}`", name))
                        .unwrap_or_else(|| "assignment expression".to_string());
                    self.require_assignable(&value_type, expected, &context)?;
                }
                Ok(target_type)
            }
            Expression::AssignOperation {
                target,
                operator,
                value,
            } => {
                self.mark_span(
                    SemanticSpanKind::Keyword,
                    assign_operator_source_symbol(operator),
                );
                if !is_lvalue_expression(target) {
                    return Err("compound assignment target is not an lvalue".to_string());
                }
                self.ensure_mutable_write_target(target, "modify with compound assignment")?;
                let target_type = self.validate_expr(target)?;
                let value_type = if let ExpressionType::Known(expected) = &target_type {
                    self.validate_expr_expected(value, Some(expected))?
                } else {
                    self.validate_expr(value)?
                };
                if matches!(operator, AssignOperator::Assign) {
                    if let ExpressionType::Known(expected) = &target_type {
                        let context = find_base_var(target, false)
                            .map(|(name, _)| format!("assignment to `{}`", name))
                            .unwrap_or_else(|| "assignment expression".to_string());
                        self.require_assignable(&value_type, expected, &context)?;
                    }
                    return Ok(target_type);
                }
                let target_is_numeric = match &target_type {
                    ExpressionType::Known(ty) => is_numeric_type(&self.program.canonical_type(ty)),
                    _ => false,
                };
                let value_is_numeric = match &value_type {
                    ExpressionType::IntLiteral(_) | ExpressionType::FloatLiteral => true,
                    ExpressionType::Known(ty) => is_numeric_type(&self.program.canonical_type(ty)),
                    _ => false,
                };
                if !target_is_numeric || !value_is_numeric {
                    return Err(format!(
                        "compound assignment `{:?}` requires numeric operands, found `{}` and `{}`",
                        operator,
                        display_expression_type(&target_type),
                        display_expression_type(&value_type)
                    ));
                }
                if let ExpressionType::Known(expected) = &target_type {
                    self.require_assignable(
                        &value_type,
                        expected,
                        "right operand of compound assignment",
                    )?;
                }
                Ok(target_type)
            }
            Expression::IncDec { target, .. } => {
                self.mark_span(SemanticSpanKind::Keyword, "++|--");
                if !is_lvalue_expression(target) {
                    return Err("++/-- target is not an lvalue".to_string());
                }
                self.ensure_mutable_write_target(target, "modify with ++/--")?;
                let ty = self.validate_expr(target)?;
                let supported = match &ty {
                    ExpressionType::Known(ty) => matches!(
                        self.program.canonical_type(ty),
                        WaveType::Int(_)
                            | WaveType::Uint(_)
                            | WaveType::Float(_)
                            | WaveType::Char
                            | WaveType::Byte
                            | WaveType::Pointer(_)
                    ),
                    _ => false,
                };
                if !supported {
                    return Err(format!(
                        "++/-- requires a numeric or pointer lvalue, found `{}`",
                        display_expression_type(&ty)
                    ));
                }
                Ok(ty)
            }
            Expression::AsmBlock {
                inputs, outputs, ..
            } => {
                for (_, expression) in inputs.iter().chain(outputs.iter()) {
                    self.validate_expr(expression)?;
                }
                Ok(ExpressionType::Unknown)
            }
        }
    }

    fn validate_function_call(
        &mut self,
        name: &str,
        type_args: &[WaveType],
        args: &[Expression],
    ) -> Result<ExpressionType, String> {
        let signature = self.program.functions.get(name).map(|signature| {
            let substitutions: HashMap<String, WaveType> = signature
                .generic_params
                .iter()
                .cloned()
                .zip(type_args.iter().cloned())
                .collect();
            substitute_function_type(signature, &substitutions)
        });
        let Some(signature) = signature else {
            for argument in args {
                self.validate_expr(argument)?;
            }
            return Err(format!("call to unknown function `{}`", name));
        };

        let declared_generic_count = self
            .program
            .functions
            .get(name)
            .map_or(0, |function| function.generic_params.len());
        if type_args.len() != declared_generic_count {
            return Err(format!(
                "function `{}` expects {} generic argument(s), found {}",
                name,
                declared_generic_count,
                type_args.len()
            ));
        }

        self.validate_call_arguments(
            "function",
            name,
            args,
            &signature.params,
            signature.required_params,
            signature.variadic,
        )?;
        Ok(ExpressionType::Known(signature.return_type))
    }

    fn validate_variant_constructor(
        &mut self,
        expression: &Expression,
        name: &str,
        args: &[Expression],
        expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let (owner, case_name) = self
            .program
            .variant_constructor(name)
            .ok_or_else(|| format!("unknown variant constructor `{}`", name))?;
        let definition = self.program.variant_type(owner).cloned().unwrap();
        let (_, (_, template_payloads)) = definition
            .cases
            .iter()
            .enumerate()
            .find(|(_, (case, _))| case == case_name)
            .ok_or_else(|| format!("unknown case `{}` in variant `{}`", case_name, owner))?;

        if args.len() != template_payloads.len() {
            return Err(format!(
                "variant constructor `{}` expects {} payload value(s), found {}",
                name,
                template_payloads.len(),
                args.len()
            ));
        }

        let expected_variant = expected
            .map(|ty| self.program.canonical_type(ty))
            .and_then(|ty| match ty {
                WaveType::Variant(name)
                    if self.program.named_type_base(&name)
                        == self.program.named_type_base(owner) =>
                {
                    Some(name)
                }
                _ => None,
            });

        let concrete_name = if let Some(name) = expected_variant {
            name
        } else if definition.generic_params.is_empty() {
            owner.to_string()
        } else {
            let mut substitutions = HashMap::new();
            for (argument, template) in args.iter().zip(template_payloads) {
                let actual = self.validate_expr(argument)?;
                infer_variant_substitution(
                    self.program,
                    template,
                    &actual,
                    &definition.generic_params,
                    &mut substitutions,
                )?;
            }
            let unresolved = definition
                .generic_params
                .iter()
                .filter(|param| !substitutions.contains_key(*param))
                .cloned()
                .collect::<Vec<_>>();
            if !unresolved.is_empty() {
                return Err(format!(
                    "cannot resolve generic argument(s) {} for variant constructor `{}`; add an explicit destination type",
                    unresolved.join(", "),
                    name
                ));
            }
            let arguments = definition
                .generic_params
                .iter()
                .map(|param| display_wave_type(&substitutions[param]))
                .collect::<Vec<_>>()
                .join(",");
            format!("{}<{}>", owner, arguments)
        };

        let (discriminant, payload_types) = self
            .program
            .variant_case(&concrete_name, case_name)
            .expect("validated variant case remains available");
        for (index, (argument, payload_type)) in args.iter().zip(&payload_types).enumerate() {
            let actual = self.validate_expr_expected(argument, Some(payload_type))?;
            self.require_assignable(
                &actual,
                payload_type,
                &format!("payload {} of variant constructor `{}`", index + 1, name),
            )?;
        }

        let variant_type = WaveType::Variant(concrete_name);
        self.hir_variant_constructions.insert(
            expression as *const Expression as usize,
            HirVariantConstruction {
                variant_type: variant_type.clone(),
                case_name: case_name.to_string(),
                discriminant,
                payload_types,
            },
        );
        Ok(ExpressionType::Known(variant_type))
    }

    fn validate_method_call(
        &mut self,
        object: &Expression,
        name: &str,
        args: &[Expression],
    ) -> Result<ExpressionType, String> {
        let object_type = self.validate_expr(object)?;
        let structure = match &object_type {
            ExpressionType::Known(WaveType::Struct(name)) => Some(name.clone()),
            ExpressionType::Known(WaveType::Pointer(inner)) => match inner.as_ref() {
                WaveType::Struct(name) => Some(name.clone()),
                _ => None,
            },
            _ => None,
        };

        if let Some(ref structure) = structure {
            if let Some(signature) = self.program.method_type(structure, name) {
                if let Some(expected_self) = signature.params.first() {
                    self.require_assignable(
                        &object_type,
                        expected_self,
                        &format!("receiver of method `{}.{}`", structure, name),
                    )?;
                }
                let params = signature.params.get(1..).unwrap_or(&[]);
                self.validate_call_arguments(
                    "method",
                    name,
                    args,
                    params,
                    signature.required_params.saturating_sub(1),
                    false,
                )?;
                return Ok(ExpressionType::Known(signature.return_type));
            }
        }

        if let Some(signature) = self.program.functions.get(name).cloned() {
            if let Some(expected_self) = signature.params.first() {
                self.require_assignable(
                    &object_type,
                    expected_self,
                    &format!("receiver of method-style call `{}`", name),
                )?;
                self.validate_call_arguments(
                    "method",
                    name,
                    args,
                    &signature.params[1..],
                    signature.required_params.saturating_sub(1),
                    false,
                )?;
                return Ok(ExpressionType::Known(signature.return_type));
            }
        }

        for argument in args {
            self.validate_expr(argument)?;
        }
        match structure {
            Some(structure) => Err(format!("struct `{}` has no method `{}`", structure, name)),
            None => Err(format!(
                "method call `{}` requires a struct receiver, found `{}`",
                name,
                display_expression_type(&object_type)
            )),
        }
    }

    fn validate_unary(
        &self,
        operator: &Operator,
        ty: ExpressionType,
    ) -> Result<ExpressionType, String> {
        if matches!(&ty, ExpressionType::Known(inner) if self.program.is_generic_placeholder(inner))
        {
            return if matches!(operator, Operator::Not | Operator::LogicalNot) {
                Ok(ExpressionType::Known(WaveType::Bool))
            } else {
                Ok(ty)
            };
        }
        let canonical = match &ty {
            ExpressionType::Known(ty) => Some(self.program.canonical_type(ty)),
            ExpressionType::IntLiteral(_) => Some(WaveType::Int(32)),
            ExpressionType::FloatLiteral => Some(WaveType::Float(32)),
            _ => None,
        };

        let supported = match operator {
            Operator::Neg => canonical.as_ref().is_some_and(is_numeric_type),
            Operator::Not | Operator::LogicalNot => canonical.as_ref().is_some_and(|ty| {
                matches!(
                    ty,
                    WaveType::Bool
                        | WaveType::Int(_)
                        | WaveType::Uint(_)
                        | WaveType::Char
                        | WaveType::Byte
                )
            }),
            Operator::BitwiseNot => canonical.as_ref().is_some_and(is_integer_type),
            _ => false,
        };

        if !supported {
            return Err(format!(
                "unary operator `{:?}` is not supported for `{}`",
                operator,
                display_expression_type(&ty)
            ));
        }

        if matches!(operator, Operator::Not | Operator::LogicalNot) {
            Ok(ExpressionType::Known(WaveType::Bool))
        } else if matches!(operator, Operator::Neg) {
            match ty {
                ExpressionType::IntLiteral(raw) => {
                    let value = parse_integer_value(&raw)
                        .and_then(i128::checked_neg)
                        .ok_or_else(|| format!("integer literal `{}` overflows", raw))?;
                    Ok(ExpressionType::IntLiteral(value.to_string()))
                }
                other => Ok(other),
            }
        } else {
            Ok(ty)
        }
    }

    fn validate_call_arguments(
        &mut self,
        kind: &str,
        name: &str,
        args: &[Expression],
        params: &[WaveType],
        required_params: usize,
        variadic: bool,
    ) -> Result<(), String> {
        if args.len() < required_params || (!variadic && args.len() > params.len()) {
            let expectation = if required_params == params.len() {
                params.len().to_string()
            } else {
                format!("between {} and {}", required_params, params.len())
            };
            return Err(format!(
                "{} `{}` expects {} argument(s), found {}",
                kind,
                name,
                expectation,
                args.len()
            ));
        }

        for (index, (argument, expected)) in args.iter().zip(params).enumerate() {
            let actual = self.validate_expr_expected(argument, Some(expected))?;
            self.require_assignable(
                &actual,
                expected,
                &format!("argument {} of {} `{}`", index + 1, kind, name),
            )?;
        }

        if variadic {
            for (index, argument) in args.iter().enumerate().skip(params.len()) {
                let actual = self.validate_expr(argument)?;
                let actual = canonical_expression_type(self.program, &actual).ok_or_else(|| {
                    format!(
                        "variadic argument {} of function `{}` has no scalar type",
                        index + 1,
                        name
                    )
                })?;
                if !matches!(
                    actual,
                    WaveType::Int(_)
                        | WaveType::Uint(_)
                        | WaveType::Float(_)
                        | WaveType::Bool
                        | WaveType::Char
                        | WaveType::Byte
                        | WaveType::String
                        | WaveType::Pointer(_)
                ) {
                    return Err(format!(
                        "variadic argument {} of function `{}` must be a scalar value",
                        index + 1,
                        name
                    ));
                }
            }
        }
        Ok(())
    }

    fn require_assignable(
        &self,
        actual: &ExpressionType,
        expected: &WaveType,
        context: &str,
    ) -> Result<(), String> {
        if self.program.is_generic_placeholder(expected) {
            return Ok(());
        }
        if let ExpressionType::ArrayLiteral(elements) = actual {
            let expected = self.program.canonical_type(expected);
            let WaveType::Array(element_type, expected_len) = expected else {
                return Err(format!(
                    "type mismatch in {}: expected `{}`, found `array literal`",
                    context,
                    display_wave_type(&expected)
                ));
            };
            if elements.len() != expected_len as usize {
                return Err(format!(
                    "array length mismatch in {}: expected {}, found {}",
                    context,
                    expected_len,
                    elements.len()
                ));
            }
            for (index, element) in elements.iter().enumerate() {
                self.require_assignable(
                    element,
                    element_type.as_ref(),
                    &format!("element {} of {}", index, context),
                )?;
            }
            return Ok(());
        }

        if let ExpressionType::AddressedArrayLiteral(elements) = actual {
            let expected = self.program.canonical_type(expected);
            let WaveType::Pointer(ref pointee) = expected else {
                return Err(format!(
                    "type mismatch in {}: expected `{}`, found `addressed array literal`",
                    context,
                    display_wave_type(&expected)
                ));
            };
            let WaveType::Array(element_type, expected_len) = pointee.as_ref() else {
                return Err(format!(
                    "addressed array literal in {} requires `ptr<array<T, N>>`, found `{}`",
                    context,
                    display_wave_type(&expected)
                ));
            };
            if elements.len() != *expected_len as usize {
                return Err(format!(
                    "array length mismatch in {}: expected {}, found {}",
                    context,
                    expected_len,
                    elements.len()
                ));
            }
            for (index, element) in elements.iter().enumerate() {
                self.require_assignable(
                    element,
                    element_type.as_ref(),
                    &format!("element {} of {}", index, context),
                )?;
            }
            return Ok(());
        }

        if self.is_assignable(actual, expected) {
            return Ok(());
        }

        Err(format!(
            "type mismatch in {}: expected `{}`, found `{}`",
            context,
            display_wave_type(expected),
            display_expression_type(actual)
        ))
    }

    fn is_assignable(&self, actual: &ExpressionType, expected: &WaveType) -> bool {
        let expected = self.program.canonical_type(expected);
        match actual {
            ExpressionType::Unknown => true,
            ExpressionType::Null => matches!(expected, WaveType::Pointer(_)),
            ExpressionType::ArrayLiteral(_) | ExpressionType::AddressedArrayLiteral(_) => false,
            ExpressionType::IntLiteral(raw) => {
                integer_literal_fits(raw, &expected)
                    || (matches!(expected, WaveType::Pointer(_)) && int_literal_is_zero(raw))
            }
            ExpressionType::FloatLiteral => matches!(expected, WaveType::Float(_)),
            ExpressionType::Known(actual) => {
                if self.program.is_generic_placeholder(actual) {
                    return true;
                }
                let actual = self.program.canonical_type(actual);
                if actual == expected {
                    return true;
                }

                match (&actual, &expected) {
                    (actual, expected)
                        if integer_bit_width(actual).is_some()
                            && integer_bit_width(expected).is_some() =>
                    {
                        integer_bit_width(actual) <= integer_bit_width(expected)
                    }
                    (WaveType::Int(_) | WaveType::Uint(_), WaveType::Float(_))
                    | (WaveType::Float(_), WaveType::Int(_) | WaveType::Uint(_)) => true,
                    (WaveType::String, WaveType::Pointer(inner)) => {
                        is_byte_like_type(inner.as_ref())
                            || matches!(inner.as_ref(), WaveType::String)
                    }
                    (WaveType::Pointer(actual), WaveType::Pointer(expected)) => actual == expected,
                    _ => false,
                }
            }
        }
    }

    fn is_valid_cast(&self, source: &ExpressionType, target: &WaveType) -> bool {
        let target = self.program.canonical_type(target);
        if matches!(
            target,
            WaveType::Void | WaveType::Array(_, _) | WaveType::Struct(_) | WaveType::Variant(_)
        ) {
            return false;
        }

        if matches!(source, ExpressionType::Unknown) {
            return false;
        }
        if matches!(source, ExpressionType::Null) {
            return matches!(target, WaveType::Pointer(_));
        }
        if matches!(
            source,
            ExpressionType::ArrayLiteral(_) | ExpressionType::AddressedArrayLiteral(_)
        ) {
            return false;
        }

        let source = match source {
            ExpressionType::Known(ty) => self.program.canonical_type(ty),
            ExpressionType::IntLiteral(_) => WaveType::Int(32),
            ExpressionType::FloatLiteral => WaveType::Float(32),
            _ => return false,
        };
        if matches!(
            source,
            WaveType::Void | WaveType::Array(_, _) | WaveType::Struct(_) | WaveType::Variant(_)
        ) {
            return false;
        }

        let source_integer = integer_bit_width(&source).is_some();
        let target_integer = integer_bit_width(&target).is_some();
        let source_float = matches!(source, WaveType::Float(_));
        let target_float = matches!(target, WaveType::Float(_));
        let source_pointer = is_pointer_like_type(&source);
        let target_pointer = matches!(target, WaveType::Pointer(_) | WaveType::String);

        (source_integer && (target_integer || target_float || target_pointer))
            || (source_float && (target_integer || target_float))
            || (source_pointer && (target_integer || target_pointer))
    }

    fn ensure_mutable_write_target(
        &self,
        target: &Expression,
        operation: &str,
    ) -> Result<(), String> {
        let Some((base, saw_deref)) = find_base_var(target, false) else {
            return Ok(());
        };
        if saw_deref {
            return Ok(());
        }

        if let Some(binding) = self.lookup_binding(&base) {
            self.ensure_mutable_binding(&base, &binding, operation)?;
        }
        Ok(())
    }

    fn ensure_mutable_binding(
        &self,
        name: &str,
        binding: &Binding,
        operation: &str,
    ) -> Result<(), String> {
        if matches!(binding.mutability, Mutability::Const) {
            return Err(format!(
                "cannot {} immutable binding `{}` ({:?})",
                operation, name, binding.mutability
            ));
        }
        Ok(())
    }
}

fn infer_binary_type(
    program: &ProgramTypes,
    operator: &Operator,
    left: ExpressionType,
    right: ExpressionType,
) -> Result<ExpressionType, String> {
    let has_generic_operand = [&left, &right].iter().any(|operand| {
        matches!(operand, ExpressionType::Known(ty) if program.is_generic_placeholder(ty))
    });
    if has_generic_operand {
        return if matches!(
            operator,
            Operator::GreaterEqual
                | Operator::LessEqual
                | Operator::Greater
                | Operator::Less
                | Operator::Equal
                | Operator::NotEqual
                | Operator::LogicalAnd
                | Operator::LogicalOr
        ) {
            Ok(ExpressionType::Known(WaveType::Bool))
        } else {
            Ok(ExpressionType::Unknown)
        };
    }
    let left_canonical = canonical_expression_type(program, &left);
    let right_canonical = canonical_expression_type(program, &right);
    let comparison = matches!(
        operator,
        Operator::GreaterEqual
            | Operator::LessEqual
            | Operator::Greater
            | Operator::Less
            | Operator::Equal
            | Operator::NotEqual
    );
    let logical = matches!(operator, Operator::LogicalAnd | Operator::LogicalOr);
    let arithmetic = matches!(
        operator,
        Operator::Add
            | Operator::Subtract
            | Operator::Multiply
            | Operator::Divide
            | Operator::Remainder
    );
    let integer_only = matches!(
        operator,
        Operator::ShiftLeft
            | Operator::ShiftRight
            | Operator::BitwiseAnd
            | Operator::BitwiseOr
            | Operator::BitwiseXor
    );

    if arithmetic || comparison || integer_only {
        validate_contextual_integer_literal(program, &left, &right)?;
        validate_contextual_integer_literal(program, &right, &left)?;
    }

    let left_pointer = left_canonical.as_ref().is_some_and(is_pointer_like_type);
    let right_pointer = right_canonical.as_ref().is_some_and(is_pointer_like_type);
    let left_integer = left_canonical
        .as_ref()
        .is_some_and(|ty| integer_bit_width(ty).is_some());
    let right_integer = right_canonical
        .as_ref()
        .is_some_and(|ty| integer_bit_width(ty).is_some());
    let left_numeric = left_canonical.as_ref().is_some_and(is_numeric_type);
    let right_numeric = right_canonical.as_ref().is_some_and(is_numeric_type);

    if let (ExpressionType::Known(left_known), ExpressionType::Known(right_known)) = (&left, &right)
    {
        if let (WaveType::Float(left_bits), WaveType::Float(right_bits)) = (
            program.canonical_type(left_known),
            program.canonical_type(right_known),
        ) {
            if left_bits != right_bits {
                return Err(format!(
                    "mixed float widths require an explicit cast: found `f{}` and `f{}`",
                    left_bits, right_bits
                ));
            }
        }
    }

    if matches!(operator, Operator::Equal | Operator::NotEqual)
        && ((left_pointer && matches!(right, ExpressionType::Null))
            || (right_pointer && matches!(left, ExpressionType::Null)))
    {
        return Ok(ExpressionType::Known(WaveType::Bool));
    }

    if left_pointer || right_pointer {
        let valid = match (left_pointer, right_pointer) {
            (true, true) => matches!(
                operator,
                Operator::Equal | Operator::NotEqual | Operator::Subtract
            ),
            (true, false) if right_integer => matches!(
                operator,
                Operator::Add | Operator::Subtract | Operator::Equal | Operator::NotEqual
            ),
            (false, true) if left_integer => {
                matches!(
                    operator,
                    Operator::Add | Operator::Equal | Operator::NotEqual
                )
            }
            _ => false,
        };
        if !valid {
            return Err(binary_type_error(operator, &left, &right));
        }
        if comparison {
            return Ok(ExpressionType::Known(WaveType::Bool));
        }
        if left_pointer {
            if right_pointer {
                return Ok(ExpressionType::Known(WaveType::Int(64)));
            }
            return Ok(left);
        }
        return Ok(right);
    }

    if logical {
        if left_integer && right_integer {
            return Ok(ExpressionType::Known(WaveType::Bool));
        }
        return Err(binary_type_error(operator, &left, &right));
    }

    if integer_only {
        if left_integer && right_integer {
            return Ok(contextual_integer_expression(program, left, right));
        }
        return Err(binary_type_error(operator, &left, &right));
    }

    if arithmetic || comparison {
        if !left_numeric || !right_numeric {
            return Err(binary_type_error(operator, &left, &right));
        }
        if comparison {
            return Ok(ExpressionType::Known(WaveType::Bool));
        }
        if matches!(left_canonical, Some(WaveType::Float(_))) {
            return Ok(left);
        }
        if matches!(right_canonical, Some(WaveType::Float(_))) {
            return Ok(right);
        }
        if matches!(left, ExpressionType::FloatLiteral) {
            return Ok(left);
        }
        if matches!(right, ExpressionType::FloatLiteral) {
            return Ok(right);
        }
        return Ok(contextual_integer_expression(program, left, right));
    }

    Err(binary_type_error(operator, &left, &right))
}

fn canonical_expression_type(program: &ProgramTypes, ty: &ExpressionType) -> Option<WaveType> {
    match ty {
        ExpressionType::Known(ty) => Some(program.canonical_type(ty)),
        ExpressionType::IntLiteral(_) => Some(WaveType::Int(32)),
        ExpressionType::FloatLiteral => Some(WaveType::Float(32)),
        _ => None,
    }
}

fn hir_expression_type(program: &ProgramTypes, ty: &ExpressionType) -> HirExpressionType {
    match ty {
        ExpressionType::Known(ty) => HirExpressionType::Resolved(program.canonical_type(ty)),
        ExpressionType::IntLiteral(_) => HirExpressionType::IntegerLiteral,
        ExpressionType::FloatLiteral => HirExpressionType::FloatLiteral,
        ExpressionType::Null => HirExpressionType::Null,
        ExpressionType::ArrayLiteral(_) => HirExpressionType::ArrayLiteral,
        ExpressionType::AddressedArrayLiteral(_) => HirExpressionType::AddressedArrayLiteral,
        ExpressionType::Unknown => HirExpressionType::Unknown,
    }
}

fn wider_integer_expression(
    program: &ProgramTypes,
    left: ExpressionType,
    right: ExpressionType,
) -> ExpressionType {
    let left_width = canonical_expression_type(program, &left)
        .as_ref()
        .and_then(integer_bit_width)
        .unwrap_or(32);
    let right_width = canonical_expression_type(program, &right)
        .as_ref()
        .and_then(integer_bit_width)
        .unwrap_or(32);
    if left_width >= right_width {
        left
    } else {
        right
    }
}

fn validate_contextual_integer_literal(
    program: &ProgramTypes,
    literal: &ExpressionType,
    other: &ExpressionType,
) -> Result<(), String> {
    let (ExpressionType::IntLiteral(raw), ExpressionType::Known(other)) = (literal, other) else {
        return Ok(());
    };
    let other = program.canonical_type(other);
    if integer_bit_width(&other).is_none() || integer_literal_fits(raw, &other) {
        return Ok(());
    }
    Err(format!(
        "integer literal `{}` does not fit `{}`; use an explicit cast to select another type",
        raw,
        display_wave_type(&other)
    ))
}

fn contextual_integer_expression(
    program: &ProgramTypes,
    left: ExpressionType,
    right: ExpressionType,
) -> ExpressionType {
    match (&left, &right) {
        (ExpressionType::IntLiteral(_), ExpressionType::Known(ty))
            if integer_bit_width(&program.canonical_type(ty)).is_some() =>
        {
            right
        }
        (ExpressionType::Known(ty), ExpressionType::IntLiteral(_))
            if integer_bit_width(&program.canonical_type(ty)).is_some() =>
        {
            left
        }
        _ => wider_integer_expression(program, left, right),
    }
}

fn binary_type_error(operator: &Operator, left: &ExpressionType, right: &ExpressionType) -> String {
    format!(
        "binary operator `{:?}` is not supported for `{}` and `{}`",
        operator,
        display_expression_type(left),
        display_expression_type(right)
    )
}

fn operator_source_symbol(operator: &Operator) -> Option<&'static str> {
    match operator {
        Operator::Add => Some("+"),
        Operator::Subtract | Operator::Neg => Some("-"),
        Operator::Multiply => Some("*"),
        Operator::Divide => Some("/"),
        Operator::Remainder => Some("%"),
        Operator::GreaterEqual => Some(">="),
        Operator::LessEqual => Some("<="),
        Operator::Greater => Some(">"),
        Operator::Less => Some("<"),
        Operator::Equal => Some("=="),
        Operator::NotEqual => Some("!="),
        Operator::LogicalAnd => Some("&&"),
        Operator::BitwiseAnd => Some("&"),
        Operator::LogicalOr => Some("||"),
        Operator::BitwiseOr => Some("|"),
        Operator::ShiftLeft => Some("<<"),
        Operator::ShiftRight => Some(">>"),
        Operator::BitwiseXor => Some("^"),
        Operator::LogicalNot | Operator::Not => Some("!"),
        Operator::BitwiseNot => Some("~"),
        Operator::Assign => Some("="),
    }
}

fn assign_operator_source_symbol(operator: &AssignOperator) -> &'static str {
    match operator {
        AssignOperator::Assign => "=",
        AssignOperator::AddAssign => "+=",
        AssignOperator::SubAssign => "-=",
        AssignOperator::MulAssign => "*=",
        AssignOperator::DivAssign => "/=",
        AssignOperator::RemAssign => "%=",
    }
}

impl From<WaveType> for ExpressionType {
    fn from(value: WaveType) -> Self {
        Self::Known(value)
    }
}

fn find_base_var(target: &Expression, saw_deref: bool) -> Option<(String, bool)> {
    match target {
        Expression::Variable(name) => Some((name.clone(), saw_deref)),
        Expression::Grouped(inner) => find_base_var(inner, saw_deref),
        Expression::FieldAccess { object, .. } => find_base_var(object, saw_deref),
        Expression::IndexAccess { target, .. } => find_base_var(target, saw_deref),
        Expression::Deref(inner) => find_base_var(inner, true),
        _ => None,
    }
}

fn is_lvalue_expression(expression: &Expression) -> bool {
    match expression {
        Expression::Located { .. } => unreachable!("source wrappers detached before analysis"),
        Expression::Variable(_)
        | Expression::FieldAccess { .. }
        | Expression::IndexAccess { .. }
        | Expression::Deref(_) => true,
        Expression::Grouped(inner) => is_lvalue_expression(inner),
        _ => false,
    }
}

fn is_codegen_supported_index(expression: &Expression) -> bool {
    match expression {
        Expression::Literal(Literal::Int(_))
        | Expression::Variable(_)
        | Expression::FieldAccess { .. }
        | Expression::IndexAccess { .. }
        | Expression::Deref(_)
        | Expression::AddressOf(_) => true,
        Expression::Grouped(inner) => is_codegen_supported_index(inner),
        _ => false,
    }
}

#[derive(Clone, Copy)]
enum ConditionMutation {
    Assignment(&'static str),
    CompoundAssignment(&'static str),
    IncrementOrDecrement(&'static str),
}

impl ConditionMutation {
    fn symbol(self) -> &'static str {
        match self {
            Self::Assignment(symbol)
            | Self::CompoundAssignment(symbol)
            | Self::IncrementOrDecrement(symbol) => symbol,
        }
    }

    fn description(self) -> &'static str {
        match self {
            Self::Assignment(_) => "assignment",
            Self::CompoundAssignment(_) => "compound assignment",
            Self::IncrementOrDecrement(_) => "increment or decrement",
        }
    }

    fn help(self) -> &'static str {
        match self {
            Self::Assignment(_) => {
                "use `==` for comparison, or move the assignment before the condition"
            }
            Self::CompoundAssignment(_) | Self::IncrementOrDecrement(_) => {
                "move the mutation before the condition"
            }
        }
    }
}

fn condition_mutation(expression: &Expression) -> Option<ConditionMutation> {
    match expression {
        Expression::Located { value, .. } => condition_mutation(value),
        Expression::Assignment { .. } => Some(ConditionMutation::Assignment("=")),
        Expression::AssignOperation { operator, .. } => {
            let symbol = assign_operator_source_symbol(operator);
            if matches!(operator, AssignOperator::Assign) {
                Some(ConditionMutation::Assignment(symbol))
            } else {
                Some(ConditionMutation::CompoundAssignment(symbol))
            }
        }
        Expression::IncDec { kind, .. } => {
            Some(ConditionMutation::IncrementOrDecrement(match kind {
                IncDecKind::PreInc | IncDecKind::PostInc => "++",
                IncDecKind::PreDec | IncDecKind::PostDec => "--",
            }))
        }
        Expression::StructLiteral { fields, .. } => fields
            .iter()
            .find_map(|(_, value)| condition_mutation(value)),
        Expression::FunctionCall { args, .. } => args.iter().find_map(condition_mutation),
        Expression::MethodCall { object, args, .. } => {
            condition_mutation(object).or_else(|| args.iter().find_map(condition_mutation))
        }
        Expression::Deref(inner)
        | Expression::AddressOf(inner)
        | Expression::Grouped(inner)
        | Expression::Unary { expr: inner, .. }
        | Expression::Cast { expr: inner, .. }
        | Expression::FieldAccess { object: inner, .. } => condition_mutation(inner),
        Expression::BinaryExpression { left, right, .. }
        | Expression::IndexAccess {
            target: left,
            index: right,
        } => condition_mutation(left).or_else(|| condition_mutation(right)),
        Expression::ArrayLiteral(values) => values.iter().find_map(condition_mutation),
        Expression::AsmBlock {
            inputs, outputs, ..
        } => inputs
            .iter()
            .chain(outputs.iter())
            .find_map(|(_, value)| condition_mutation(value)),
        Expression::Null | Expression::Literal(_) | Expression::Variable(_) => None,
    }
}

fn expression_is_true(expression: &Expression) -> bool {
    matches!(expression, Expression::Literal(Literal::Bool(true)))
}

fn block_breaks_current_loop(nodes: &[ASTNode]) -> bool {
    nodes.iter().any(node_breaks_current_loop)
}

fn node_breaks_current_loop(node: &ASTNode) -> bool {
    let ASTNode::Statement(statement) = node else {
        return false;
    };

    match statement {
        StatementNode::Break => true,
        StatementNode::If {
            body,
            else_if_blocks,
            else_block,
            ..
        } => {
            block_breaks_current_loop(body)
                || else_if_blocks.as_ref().is_some_and(|blocks| {
                    blocks
                        .iter()
                        .any(|(_, block)| block_breaks_current_loop(block))
                })
                || else_block
                    .as_ref()
                    .is_some_and(|block| block_breaks_current_loop(block))
        }
        StatementNode::Match { arms, .. } => {
            arms.iter().any(|arm| block_breaks_current_loop(&arm.body))
        }
        StatementNode::While { .. } | StatementNode::For { .. } => false,
        _ => false,
    }
}

fn int_literal_is_zero(raw: &str) -> bool {
    lexer::number::IntegerLiteral::parse(raw).is_some_and(|n| n.is_zero())
}

fn integer_literal_fits(raw: &str, ty: &WaveType) -> bool {
    let Some((negative, radix, digits)) = integer_literal_parts(raw) else {
        return false;
    };
    let Some(bit_len) = unsigned_literal_bit_len(radix, &digits) else {
        return false;
    };
    let is_zero = bit_len == 0;

    match ty {
        WaveType::Int(bits) if *bits > 0 => {
            let bits = usize::from(*bits);
            if negative {
                is_zero
                    || bit_len < bits
                    || (bit_len == bits && unsigned_is_power_of_two(radix, &digits))
            } else if radix == 10 {
                bit_len < bits
            } else {
                // Non-decimal literals may spell the full-width bit pattern.
                bit_len <= bits
            }
        }
        WaveType::Uint(bits) if *bits > 0 => !negative && bit_len <= usize::from(*bits),
        WaveType::Char | WaveType::Byte => !negative && bit_len <= 8,
        _ => false,
    }
}

fn integer_literal_parts(raw: &str) -> Option<(bool, u32, String)> {
    let n = lexer::number::IntegerLiteral::parse(raw)?;
    Some((
        n.negative,
        n.radix,
        n.digits.trim_start_matches('0').to_string(),
    ))
}

fn unsigned_literal_bit_len(radix: u32, digits: &str) -> Option<usize> {
    if digits.is_empty() {
        return Some(0);
    }
    match radix {
        2 => Some(digits.len()),
        8 | 16 => {
            let bits_per_digit = if radix == 8 { 3 } else { 4 };
            let first = digits.chars().next()?.to_digit(radix)?;
            let first_bits = (u32::BITS - first.leading_zeros()) as usize;
            Some((digits.len() - 1) * bits_per_digit + first_bits)
        }
        10 => {
            let mut decimal: Vec<u8> = digits.bytes().map(|byte| byte - b'0').collect();
            let mut bits = 0usize;
            while decimal.iter().any(|digit| *digit != 0) {
                let mut carry = 0u8;
                for digit in &mut decimal {
                    let value = carry * 10 + *digit;
                    *digit = value / 2;
                    carry = value % 2;
                }
                bits = bits.checked_add(1)?;
            }
            Some(bits)
        }
        _ => None,
    }
}

fn unsigned_is_power_of_two(radix: u32, digits: &str) -> bool {
    if digits.is_empty() {
        return false;
    }
    if radix == 10 {
        let mut decimal: Vec<u8> = digits.bytes().map(|byte| byte - b'0').collect();
        loop {
            let first_nonzero = decimal.iter().position(|digit| *digit != 0);
            let Some(first_nonzero) = first_nonzero else {
                return false;
            };
            if decimal[first_nonzero..] == [1] {
                return true;
            }
            if decimal.last().is_none_or(|digit| digit % 2 != 0) {
                return false;
            }
            let mut carry = 0u8;
            for digit in &mut decimal {
                let value = carry * 10 + *digit;
                *digit = value / 2;
                carry = value % 2;
            }
        }
    }
    let mut seen_one = false;
    for ch in digits.chars() {
        let Some(mut value) = ch.to_digit(radix) else {
            return false;
        };
        while value != 0 {
            if value & 1 == 1 {
                if seen_one {
                    return false;
                }
                seen_one = true;
            }
            value >>= 1;
        }
    }
    seen_one
}

fn is_integer_type(ty: &WaveType) -> bool {
    matches!(
        ty,
        WaveType::Int(_) | WaveType::Uint(_) | WaveType::Char | WaveType::Byte
    )
}

fn integer_bit_width(ty: &WaveType) -> Option<u16> {
    match ty {
        WaveType::Int(bits) | WaveType::Uint(bits) => Some(*bits),
        WaveType::Bool => Some(1),
        WaveType::Char | WaveType::Byte => Some(8),
        _ => None,
    }
}

fn is_numeric_type(ty: &WaveType) -> bool {
    is_integer_type(ty) || matches!(ty, WaveType::Float(_))
}

fn is_pointer_like_type(ty: &WaveType) -> bool {
    matches!(ty, WaveType::Pointer(_) | WaveType::String)
}

fn is_byte_like_type(ty: &WaveType) -> bool {
    matches!(
        ty,
        WaveType::Int(8) | WaveType::Uint(8) | WaveType::Char | WaveType::Byte
    )
}

fn display_expression_type(ty: &ExpressionType) -> String {
    match ty {
        ExpressionType::Known(ty) => display_wave_type(ty),
        ExpressionType::IntLiteral(_) => "integer literal".to_string(),
        ExpressionType::FloatLiteral => "float literal".to_string(),
        ExpressionType::Null => "null".to_string(),
        ExpressionType::ArrayLiteral(_) => "array literal".to_string(),
        ExpressionType::AddressedArrayLiteral(_) => "addressed array literal".to_string(),
        ExpressionType::Unknown => "unknown".to_string(),
    }
}

fn display_wave_type(ty: &WaveType) -> String {
    match ty {
        WaveType::Isz => "isz".to_string(),
        WaveType::Usz => "usz".to_string(),
        WaveType::Int(bits) => format!("i{}", bits),
        WaveType::Uint(bits) => format!("u{}", bits),
        WaveType::Float(bits) => format!("f{}", bits),
        WaveType::Bool => "bool".to_string(),
        WaveType::Char => "char".to_string(),
        WaveType::Byte => "byte".to_string(),
        WaveType::String => "str".to_string(),
        WaveType::Pointer(inner) => format!("ptr<{}>", display_wave_type(inner)),
        WaveType::Array(inner, size) => format!("array<{}, {}>", display_wave_type(inner), size),
        WaveType::Void => "void".to_string(),
        WaveType::Never => "!".to_string(),
        WaveType::Struct(name) => name.clone(),
        WaveType::Variant(name) => name.clone(),
    }
}

fn variant_pattern_key(pattern: &MatchPattern) -> String {
    match pattern {
        MatchPattern::Located { .. } => unreachable!("source wrappers detached before analysis"),
        MatchPattern::Int(raw) => format!("int:{}", raw),
        MatchPattern::Ident(name) => format!("ident:{}", name),
        MatchPattern::Binding(_) | MatchPattern::Wildcard => "*".to_string(),
        MatchPattern::Variant {
            variant_type,
            case_name,
            payloads,
        } => format!(
            "{}::{}({})",
            variant_type,
            case_name,
            payloads
                .iter()
                .map(variant_pattern_key)
                .collect::<Vec<_>>()
                .join(",")
        ),
    }
}

pub fn validate_program(nodes: &Vec<ASTNode>) -> Result<(), String> {
    validate_program_detailed(nodes).map_err(|diagnostic| diagnostic.message)
}

pub fn validate_program_detailed(nodes: &[ASTNode]) -> Result<(), SemanticDiagnostic> {
    let mut syntax = nodes.to_vec().into_boxed_slice();
    let sources = crate::source::SourceMap::detach(&mut syntax);
    analyze_program_types(&syntax, &sources).map(|_| ())
}

pub fn analyze_expression_types(
    nodes: &[ASTNode],
) -> Result<HashMap<usize, WaveType>, SemanticDiagnostic> {
    analyze_program_types(nodes, &crate::source::SourceMap::default())
        .map(|analysis| analysis.expression_types)
}

pub(crate) fn analyze_hir_expression_types(
    nodes: &[ASTNode],
    sources: &crate::source::SourceMap,
) -> Result<
    (
        HashMap<usize, HirExpressionType>,
        HashMap<usize, HirVariantConstruction>,
        HashMap<usize, HirVariantPattern>,
    ),
    SemanticDiagnostic,
> {
    analyze_program_types(nodes, sources).map(|analysis| {
        (
            analysis.hir_expression_types,
            analysis.hir_variant_constructions,
            analysis.hir_variant_patterns,
        )
    })
}

struct ProgramAnalysis {
    expression_types: HashMap<usize, WaveType>,
    hir_expression_types: HashMap<usize, HirExpressionType>,
    hir_variant_constructions: HashMap<usize, HirVariantConstruction>,
    hir_variant_patterns: HashMap<usize, HirVariantPattern>,
}

fn is_supported_foreign_abi(abi: &str) -> bool {
    abi.eq_ignore_ascii_case("c") || abi.eq_ignore_ascii_case("system")
}

fn analyze_program_types(
    nodes: &[ASTNode],
    sources: &crate::source::SourceMap,
) -> Result<ProgramAnalysis, SemanticDiagnostic> {
    let program = ProgramTypes::collect(nodes).map_err(|(index, message, primary)| {
        let mut diagnostic = semantic_diagnostic_for_top_level(nodes, index, message, primary);
        diagnostic.span = nodes
            .get(index)
            .and_then(|node| sources.nodes.get(&(node as *const _ as usize)))
            .cloned();
        diagnostic
    })?;
    validate_declaration_types(nodes, &program).map_err(|mut diagnostic| {
        diagnostic.span = nodes
            .get(diagnostic.top_level_index)
            .and_then(|node| sources.nodes.get(&(node as *const _ as usize)))
            .cloned();
        diagnostic
    })?;
    let mut validator = Validator::new(&program, sources);

    for (index, node) in nodes.iter().enumerate() {
        validator.begin_top_level(index, top_level_span_hint(node));
        validator.source_span = sources.nodes.get(&(node as *const _ as usize)).cloned();
        let result = match node {
            ASTNode::Function(function) => {
                if let Some(export) = &function.export {
                    if !is_supported_foreign_abi(&export.abi) {
                        validator.mark_span(SemanticSpanKind::Keyword, "export");
                        Err(format!(
                            "unsupported export ABI '{}' for function '{}': supported ABIs are export(c) and export(system)",
                            export.abi, function.name
                        ))
                    } else {
                        validator.validate_function(function, &function.name, &[])
                    }
                } else {
                    validator.validate_function(function, &function.name, &[])
                }
            }
            ASTNode::ProtoImpl(implementation) => {
                let mut result = Ok(());
                for method in &implementation.methods {
                    validator.mark_span(SemanticSpanKind::Declaration, method.name.clone());
                    result = validator.validate_function(
                        method,
                        &format!("{}.{}", implementation.target, method.name),
                        &[],
                    );
                    if result.is_err() {
                        break;
                    }
                }
                result
            }
            ASTNode::Struct(structure) => {
                let mut result = Ok(());
                for method in &structure.methods {
                    validator.mark_span(SemanticSpanKind::Declaration, method.name.clone());
                    result = validator.validate_function(
                        method,
                        &format!("{}.{}", structure.name, method.name),
                        &structure.generic_params,
                    );
                    if result.is_err() {
                        break;
                    }
                }
                result
            }
            ASTNode::ExternFunction(function) => {
                if !is_supported_foreign_abi(&function.abi) {
                    validator.mark_span(SemanticSpanKind::Keyword, "extern");
                    Err(format!(
                        "unsupported extern ABI '{}' for function '{}': supported ABIs are extern(c) and extern(system)",
                        function.abi, function.name
                    ))
                } else {
                    Ok(())
                }
            }
            ASTNode::Variable(_) | ASTNode::Statement(_) | ASTNode::Expression(_) => {
                validator.validate_node(node).map(|_| ())
            }
            _ => Ok(()),
        };
        if let Err(message) = result {
            return Err(validator.diagnostic(message));
        }
    }

    Ok(ProgramAnalysis {
        expression_types: validator.expression_types,
        hir_expression_types: validator.hir_expression_types,
        hir_variant_constructions: validator.hir_variant_constructions,
        hir_variant_patterns: validator.hir_variant_patterns,
    })
}

fn top_level_span_hint(node: &ASTNode) -> SemanticSpanHint {
    let (kind, text) = match node {
        ASTNode::Located { value, .. } => return top_level_span_hint(value),
        ASTNode::Function(function) => (SemanticSpanKind::Declaration, function.name.clone()),
        ASTNode::ExternFunction(function) => (SemanticSpanKind::Declaration, function.name.clone()),
        ASTNode::Struct(structure) => (SemanticSpanKind::Declaration, structure.name.clone()),
        ASTNode::ProtoImpl(implementation) => {
            (SemanticSpanKind::Declaration, implementation.target.clone())
        }
        ASTNode::TypeAlias(alias) => (SemanticSpanKind::Declaration, alias.name.clone()),
        ASTNode::Enum(enumeration) => (SemanticSpanKind::Declaration, enumeration.name.clone()),
        ASTNode::Variant(variant) => (SemanticSpanKind::Declaration, variant.name.clone()),
        ASTNode::Variable(variable) => (SemanticSpanKind::Declaration, variable.name.clone()),
        ASTNode::Statement(_) | ASTNode::Expression(_) | ASTNode::Program(_) => {
            (SemanticSpanKind::Keyword, "program".to_string())
        }
    };
    SemanticSpanHint {
        kind,
        text,
        occurrence: 1,
    }
}

fn semantic_diagnostic_for_top_level(
    nodes: &[ASTNode],
    index: usize,
    message: String,
    primary: Option<SemanticSpanHint>,
) -> SemanticDiagnostic {
    let primary = primary.or_else(|| nodes.get(index).map(top_level_span_hint));
    SemanticDiagnostic {
        code: "E3001".to_string(),
        label: message.clone(),
        message,
        top_level_index: index,
        primary,
        span: nodes.get(index).and_then(ASTNode::span).cloned(),
        note: None,
        help: "fix type, mutability, scope, and control-flow errors".to_string(),
    }
}

fn validate_declaration_types(
    nodes: &[ASTNode],
    program: &ProgramTypes,
) -> Result<(), SemanticDiagnostic> {
    let no_generics = HashSet::new();
    let mut checked_aliases = HashSet::new();

    for (index, node) in nodes.iter().enumerate() {
        let result = match node {
            ASTNode::Function(function) => {
                let mut result =
                    validate_unique_generic_params(&function.generic_params, &function.name);
                if result.is_ok() && function.name == "main" {
                    if !function.generic_params.is_empty() {
                        result =
                            Err("entry function `main` cannot declare generic parameters"
                                .to_string());
                    } else {
                        let return_type = function.return_type.clone().unwrap_or(WaveType::Void);
                        let return_type = program.canonical_type(&return_type);
                        if !matches!(return_type, WaveType::Void | WaveType::Int(32)) {
                            result = Err(format!(
                                "entry function `main` must return `i32` or omit its return type, found `{}`",
                                display_wave_type(&return_type)
                            ));
                        }
                    }
                }
                if result.is_ok() && function.export.is_some() {
                    for parameter in &function.parameters {
                        if is_direct_variant_type(program, &parameter.param_type) {
                            result = Err(format!(
                                "export(c) function `{}` cannot expose variant parameter `{}` directly",
                                function.name, parameter.name
                            ));
                            break;
                        }
                    }
                    if result.is_ok()
                        && function
                            .return_type
                            .as_ref()
                            .is_some_and(|ty| is_direct_variant_type(program, ty))
                    {
                        result = Err(format!(
                            "export(c) function `{}` cannot return a variant directly",
                            function.name
                        ));
                    }
                }
                result
            }
            ASTNode::Struct(structure) => {
                let mut result =
                    validate_unique_generic_params(&structure.generic_params, &structure.name);
                if result.is_ok() {
                    let generics: HashSet<String> =
                        structure.generic_params.iter().cloned().collect();
                    for (field, ty) in &structure.fields {
                        result = program.validate_type(
                            ty,
                            &generics,
                            false,
                            &format!("field `{}.{}`", structure.name, field),
                        );
                        if result.is_err() {
                            break;
                        }
                    }
                }
                result
            }
            ASTNode::ProtoImpl(implementation) => {
                if !program.is_known_named_type(&implementation.target) {
                    Err(format!(
                        "proto implementation targets unknown type `{}`",
                        implementation.target
                    ))
                } else {
                    Ok(())
                }
            }
            ASTNode::TypeAlias(alias) => program
                .validate_type(
                    &alias.target,
                    &no_generics,
                    false,
                    &format!("type alias `{}`", alias.name),
                )
                .and_then(|_| {
                    validate_alias_cycle(
                        &alias.name,
                        program,
                        &mut HashSet::new(),
                        &mut checked_aliases,
                    )
                }),
            ASTNode::Enum(enumeration) => {
                let result = program.validate_type(
                    &enumeration.repr_type,
                    &no_generics,
                    false,
                    &format!("representation of enum `{}`", enumeration.name),
                );
                if result.is_ok()
                    && !is_integer_type(&program.canonical_type(&enumeration.repr_type))
                {
                    Err(format!(
                        "enum `{}` representation must be an integer type, found `{}`",
                        enumeration.name,
                        display_wave_type(&enumeration.repr_type)
                    ))
                } else {
                    result
                }
            }
            ASTNode::Variant(variant) => {
                let mut result =
                    validate_unique_generic_params(&variant.generic_params, &variant.name);
                let generics: HashSet<String> = variant.generic_params.iter().cloned().collect();
                if result.is_ok() && variant.cases.is_empty() {
                    result = Err(format!(
                        "variant `{}` must declare at least one case",
                        variant.name
                    ));
                }
                if result.is_ok() {
                    for case in &variant.cases {
                        for payload in &case.payload_types {
                            result = program.validate_type(
                                payload,
                                &generics,
                                false,
                                &format!("payload of `{}::{}`", variant.name, case.name),
                            );
                            if result.is_err() {
                                break;
                            }
                        }
                        if result.is_err() {
                            break;
                        }
                    }
                }
                if result.is_ok() {
                    result = validate_finite_variant(&variant.name, program);
                }
                result
            }
            ASTNode::ExternFunction(function) => {
                let mut params = HashSet::new();
                let mut result = Ok(());
                for (name, ty) in &function.params {
                    if !params.insert(name) {
                        result = Err(format!(
                            "duplicate parameter declaration `{}` in extern function `{}`",
                            name, function.name
                        ));
                        break;
                    }
                    result = program.validate_type(
                        ty,
                        &no_generics,
                        false,
                        &format!(
                            "parameter `{}` of extern function `{}`",
                            name, function.name
                        ),
                    );
                    if result.is_err() {
                        break;
                    }
                }
                if result.is_ok() {
                    result = program.validate_type(
                        &function.return_type,
                        &no_generics,
                        true,
                        &format!("return type of extern function `{}`", function.name),
                    );
                }
                if result.is_ok() {
                    for (name, ty) in &function.params {
                        if is_direct_variant_type(program, ty) {
                            result = Err(format!(
                                "extern(c) function `{}` cannot expose variant parameter `{}` directly",
                                function.name, name
                            ));
                            break;
                        }
                    }
                }
                if result.is_ok() && is_direct_variant_type(program, &function.return_type) {
                    result = Err(format!(
                        "extern(c) function `{}` cannot return a variant directly",
                        function.name
                    ));
                }
                result
            }
            ASTNode::Variable(variable) => program.validate_type(
                &variable.type_name,
                &no_generics,
                false,
                &format!("global variable `{}`", variable.name),
            ),
            _ => Ok(()),
        };
        if let Err(message) = result {
            return Err(semantic_diagnostic_for_top_level(
                nodes, index, message, None,
            ));
        }
    }

    Ok(())
}

fn is_direct_variant_type(program: &ProgramTypes, ty: &WaveType) -> bool {
    matches!(program.canonical_type(ty), WaveType::Variant(_))
}

fn validate_finite_variant(name: &str, program: &ProgramTypes) -> Result<(), String> {
    let mut active = HashSet::new();
    let mut checked = HashSet::new();
    validate_finite_named_type(name, name, program, &mut active, &mut checked)
}

fn validate_finite_named_type(
    root: &str,
    name: &str,
    program: &ProgramTypes,
    active: &mut HashSet<String>,
    checked: &mut HashSet<String>,
) -> Result<(), String> {
    let base = program.named_type_base(name).to_string();
    if checked.contains(&base) || program.generic_type_params.contains(&base) {
        return Ok(());
    }
    if !active.insert(base.clone()) {
        return Err(format!(
            "variant `{}` has infinite size through direct recursive payload `{}`; use `ptr<{}>` for indirection",
            root, base, base
        ));
    }

    if let Some(alias) = program.aliases.get(&base) {
        validate_finite_payload_type(root, alias, program, active, checked)?;
    } else if let Some(fields) = program.structs.get(&base) {
        for field in fields.values() {
            validate_finite_payload_type(root, field, program, active, checked)?;
        }
    } else if let Some(variant) = program.variants.get(&base) {
        for (_, payloads) in &variant.cases {
            for payload in payloads {
                validate_finite_payload_type(root, payload, program, active, checked)?;
            }
        }
    }

    active.remove(&base);
    checked.insert(base);
    Ok(())
}

fn validate_finite_payload_type(
    root: &str,
    ty: &WaveType,
    program: &ProgramTypes,
    active: &mut HashSet<String>,
    checked: &mut HashSet<String>,
) -> Result<(), String> {
    match ty {
        WaveType::Pointer(_) => Ok(()),
        WaveType::Array(inner, _) => {
            validate_finite_payload_type(root, inner, program, active, checked)
        }
        WaveType::Struct(name) | WaveType::Variant(name) => {
            validate_finite_named_type(root, name, program, active, checked)
        }
        _ => Ok(()),
    }
}

fn validate_alias_cycle(
    alias: &str,
    program: &ProgramTypes,
    active: &mut HashSet<String>,
    checked: &mut HashSet<String>,
) -> Result<(), String> {
    if checked.contains(alias) {
        return Ok(());
    }
    if !active.insert(alias.to_string()) {
        return Err(format!("cyclic type alias involving `{}`", alias));
    }
    if let Some(target) = program.aliases.get(alias) {
        validate_alias_type_cycle(target, program, active, checked)?;
    }
    active.remove(alias);
    checked.insert(alias.to_string());
    Ok(())
}

fn validate_alias_type_cycle(
    ty: &WaveType,
    program: &ProgramTypes,
    active: &mut HashSet<String>,
    checked: &mut HashSet<String>,
) -> Result<(), String> {
    match ty {
        WaveType::Struct(name) if program.aliases.contains_key(name) => {
            validate_alias_cycle(name, program, active, checked)
        }
        WaveType::Pointer(inner) | WaveType::Array(inner, _) => {
            validate_alias_type_cycle(inner, program, active, checked)
        }
        _ => Ok(()),
    }
}

fn validate_unique_generic_params(params: &[String], owner: &str) -> Result<(), String> {
    let mut seen = HashSet::new();
    for param in params {
        if !seen.insert(param) {
            return Err(format!(
                "duplicate generic parameter `{}` in `{}`",
                param, owner
            ));
        }
    }
    Ok(())
}
