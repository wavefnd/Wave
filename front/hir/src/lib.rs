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

//! Backend-neutral typed frontend program.
//!
//! [`TypedProgram`] is the boundary after import expansion, generic
//! monomorphization, and semantic validation. It owns the final source AST in
//! stable storage and assigns every expression a stable [`ExpressionId`]. This
//! lets future variant and async lowering attach semantic facts without using
//! backend-owned state or expression addresses as public identities.

// Preserve the existing diagnostic payloads while extracting this crate.
// The parser previously allowed this lint for these same APIs.
#![allow(clippy::result_large_err)]

pub mod async_lower;
pub mod conversions;
mod numeric_checks;
use conversions::{ConversionError, NumericExpressionInfo};
pub use numeric_checks::{integer_literal_float, ConstantValue};
use parser::ast::visit::{walk_expression, walk_node, walk_nodes};

use parser::ast::{ASTNode, Expression, MatchPattern, StatementNode, WaveType};
use parser::types::{parse_type, split_top_level_generic_args, token_type_to_wave_type};
use parser::verification::{analyze_semantic_facts, SemanticDiagnostic, SemanticFacts};
use std::collections::{HashMap, HashSet};
use std::fmt;

/// Stable identity of an AST declaration or statement in one typed program.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(usize);
impl NodeId {
    pub fn index(self) -> usize {
        self.0
    }
}

/// Stable identity of an expression within one [`TypedProgram`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ExpressionId(usize);

impl ExpressionId {
    pub fn index(self) -> usize {
        self.0
    }
}

/// Stable identity of a match pattern within one [`TypedProgram`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct PatternId(usize);

impl PatternId {
    pub fn index(self) -> usize {
        self.0
    }
}

pub use parser::verification::{
    AnalyzedExpressionType as HirExpressionType, VariantConstruction as HirVariantConstruction,
    VariantPattern as HirVariantPattern,
};

/// Semantically validated frontend program consumed by later lowering passes.
///
/// The syntax is boxed before analysis, so moving `TypedProgram` never changes
/// expression addresses. Addresses are only an internal lookup optimization;
/// consumers observe stable `ExpressionId` values.
#[derive(Debug)]
pub struct TypedProgram {
    syntax: Box<[ASTNode]>,
    node_ids: HashMap<usize, NodeId>,
    node_spans: Vec<Option<error::SourceSpan>>,
    expression_ids: HashMap<usize, ExpressionId>,
    expression_types: Vec<HirExpressionType>,
    expected_types: Vec<Option<WaveType>>,
    numeric_expressions: Vec<Option<NumericExpressionInfo>>,
    constant_values: HashMap<ExpressionId, ConstantValue>,
    expression_spans: Vec<Option<error::SourceSpan>>,
    variant_constructions: Vec<Option<HirVariantConstruction>>,
    pattern_ids: HashMap<usize, PatternId>,
    variant_patterns: Vec<Option<HirVariantPattern>>,
    integer_patterns: Vec<Option<String>>,
    pattern_spans: Vec<Option<error::SourceSpan>>,
}

/// Semantic lowering failure that retains the syntax used for source mapping.
#[derive(Debug)]
pub struct HirLoweringError {
    syntax: Box<[ASTNode]>,
    diagnostic: SemanticDiagnostic,
}

impl HirLoweringError {
    pub fn syntax(&self) -> &[ASTNode] {
        &self.syntax
    }

    pub fn diagnostic(&self) -> &SemanticDiagnostic {
        &self.diagnostic
    }

    pub fn into_parts(self) -> (Box<[ASTNode]>, SemanticDiagnostic) {
        (self.syntax, self.diagnostic)
    }
}

impl fmt::Display for HirLoweringError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.diagnostic.fmt(formatter)
    }
}

impl std::error::Error for HirLoweringError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.diagnostic)
    }
}

impl TypedProgram {
    /// Validates a final AST and builds its stable typed frontend representation.
    pub fn lower(syntax: Vec<ASTNode>) -> Result<Self, HirLoweringError> {
        let mut syntax = syntax.into_boxed_slice();
        if let Err(diagnostic) = parser::expression_depth::validate(&syntax) {
            return Err(HirLoweringError { syntax, diagnostic });
        }
        let source_map = parser::source::SourceMap::detach(&mut syntax);
        let SemanticFacts {
            expression_types: analyzed_types,
            variant_constructions: analyzed_variants,
            variant_patterns: analyzed_patterns,
            expected_types: analyzed_expected,
            integer_patterns: analyzed_integers,
        } = match analyze_semantic_facts(&syntax, &source_map) {
            Ok(analysis) => analysis,
            Err(diagnostic) => return Err(HirLoweringError { syntax, diagnostic }),
        };
        // Semantic analysis must see source-level enum and alias identities.
        // Canonicalize only afterward, in place, so backend-visible types are
        // concrete without invalidating the expression addresses used while
        // stable HIR identities are assigned below.
        canonicalize_syntax_types(&mut syntax);
        let mut expected_types = Vec::new();
        let mut expression_spans = Vec::new();
        let mut expression_ids = HashMap::with_capacity(analyzed_types.len());
        let mut expression_types = Vec::with_capacity(analyzed_types.len());
        let mut variant_constructions = Vec::with_capacity(analyzed_variants.len());

        walk_nodes(&syntax, &mut |expression| {
            let address = expression as *const Expression as usize;
            let id = ExpressionId(expression_types.len());
            expression_ids.insert(address, id);
            expression_spans.push(source_map.expressions.get(&address).cloned());
            expected_types.push(analyzed_expected.get(&address).cloned());
            expression_types
                .push(analyzed_types.get(&address).cloned().unwrap_or(HirExpressionType::Unknown));
            variant_constructions.push(analyzed_variants.get(&address).cloned());
        });

        let mut integer_patterns = Vec::new();
        let mut pattern_spans = Vec::new();
        let mut pattern_ids = HashMap::with_capacity(analyzed_patterns.len());
        let mut variant_patterns = Vec::with_capacity(analyzed_patterns.len());
        walk_patterns_in_nodes(&syntax, &mut |pattern| {
            let address = pattern as *const MatchPattern as usize;
            let id = PatternId(variant_patterns.len());
            pattern_ids.insert(address, id);
            pattern_spans.push(source_map.patterns.get(&address).cloned());
            variant_patterns.push(analyzed_patterns.get(&address).cloned());
            integer_patterns.push(analyzed_integers.get(&address).cloned());
        });

        let node_ids = source_map
            .node_order
            .iter()
            .enumerate()
            .map(|(id, address)| (*address, NodeId(id)))
            .collect();
        let node_spans = source_map
            .node_order
            .iter()
            .map(|address| source_map.nodes.get(address).cloned())
            .collect();
        let mut program = Self {
            syntax,
            node_ids,
            node_spans,
            expression_ids,
            expression_types,
            expected_types,
            numeric_expressions: Vec::new(),
            constant_values: HashMap::new(),
            expression_spans,
            variant_constructions,
            pattern_ids,
            variant_patterns,
            integer_patterns,
            pattern_spans,
        };
        program.numeric_expressions = conversions::build(&program);
        program.constant_values = match numeric_checks::validate(&program) {
            Ok(values) => values,
            Err(diagnostic) => return Err(HirLoweringError { syntax: program.syntax, diagnostic }),
        };
        Ok(program)
    }

    pub fn constant_value_of(&self, expression: &Expression) -> Option<&ConstantValue> {
        self.expression_id(expression).and_then(|id| self.constant_values.get(&id))
    }

    pub fn numeric_expression(&self, id: ExpressionId) -> Option<&NumericExpressionInfo> {
        self.numeric_expressions.get(id.index()).and_then(Option::as_ref)
    }

    pub fn numeric_expression_of(&self, expression: &Expression) -> Option<&NumericExpressionInfo> {
        self.expression_id(expression).and_then(|id| self.numeric_expression(id))
    }
    /// Reject missing or inconsistent facts before entering a backend.
    pub fn verify_conversions(&self) -> Result<(), ConversionError> {
        let mut failure = None;
        walk_nodes(self.syntax(), &mut |expr| {
            if failure.is_some() {
                return;
            }
            let id = self.expression_id(expr).expect("owned HIR expression");
            let required = matches!(self.type_of(expr), Some(HirExpressionType::Resolved(t)) if conversions::scalar(t))
                || matches!(
                    self.type_of(expr),
                    Some(HirExpressionType::IntegerLiteral | HirExpressionType::FloatLiteral)
                )
                || (matches!(self.type_of(expr), Some(HirExpressionType::AddressedArrayLiteral))
                    && matches!(self.expected_type_of(expr), Some(WaveType::Pointer(_))));
            let result = match self.numeric_expression(id) {
                Some(fact) => conversions::verify_expression(self, expr, fact),
                None if required => Err("missing required scalar conversion facts".into()),
                None => Ok(()),
            };
            if let Err(message) = result {
                failure = Some(ConversionError {
                    expression: id,
                    message,
                    span: self.expression_span(id).cloned(),
                });
            }
        });
        failure.map_or(Ok(()), Err)
    }

    /// Contextual destination type, retained separately from the expression's
    /// source type so the frontend can record ordered conversions.
    pub fn expected_type_of(&self, expression: &Expression) -> Option<&WaveType> {
        self.expression_id(expression)
            .and_then(|id| self.expected_types.get(id.index()))
            .and_then(Option::as_ref)
    }

    /// Whether this program needs the task executor.
    pub fn uses_async_runtime(&self) -> bool {
        let mut found = false;
        walk_nodes(self.syntax(), &mut |e| {
            if matches!(e,Expression::FunctionCall{name,..} if parser::async_intrinsics::is_intrinsic(name))
            {
                found = true;
            }
        });
        found
    }

    /// Runtime symbols referenced by intrinsic calls, with source locations for diagnostics.
    pub fn async_runtime_requirements(&self) -> Vec<(&'static str, Option<error::SourceSpan>)> {
        let mut requirements = std::collections::BTreeMap::new();
        walk_nodes(self.syntax(), &mut |expression| {
            if let Expression::FunctionCall { name, .. } = expression {
                for &symbol in parser::async_intrinsics::runtime_symbols(name) {
                    requirements.entry(symbol).or_insert_with(|| {
                        self.expression_id(expression)
                            .and_then(|id| self.expression_span(id))
                            .cloned()
                    });
                }
            }
        });
        requirements.into_iter().collect()
    }

    /// Stable await identities and completion types, independent of a backend.
    pub fn await_sites(&self) -> Vec<(ExpressionId, WaveType)> {
        let mut sites = Vec::new();
        walk_nodes(self.syntax(), &mut |expression| {
            if matches!(expression, Expression::Await(_)) {
                if let (Some(id), Some(HirExpressionType::Resolved(ty))) =
                    (self.expression_id(expression), self.type_of(expression))
                {
                    sites.push((id, ty.clone()));
                }
            }
        });
        sites
    }

    pub fn node_id(&self, node: &ASTNode) -> Option<NodeId> {
        self.node_ids.get(&(node as *const _ as usize)).copied()
    }

    pub fn node_span(&self, id: NodeId) -> Option<&error::SourceSpan> {
        self.node_spans.get(id.index())?.as_ref()
    }

    pub fn expression_span(&self, id: ExpressionId) -> Option<&error::SourceSpan> {
        self.expression_spans.get(id.index())?.as_ref()
    }

    pub fn pattern_span(&self, id: PatternId) -> Option<&error::SourceSpan> {
        self.pattern_spans.get(id.index())?.as_ref()
    }

    pub fn syntax(&self) -> &[ASTNode] {
        &self.syntax
    }

    pub fn expression_count(&self) -> usize {
        self.expression_types.len()
    }

    pub fn expression_id(&self, expression: &Expression) -> Option<ExpressionId> {
        self.expression_ids.get(&(expression as *const Expression as usize)).copied()
    }

    pub fn expression_type(&self, id: ExpressionId) -> Option<&HirExpressionType> {
        self.expression_types.get(id.index())
    }

    pub fn expression_types(&self) -> impl Iterator<Item = &HirExpressionType> {
        self.expression_types.iter()
    }

    pub fn type_of(&self, expression: &Expression) -> Option<&HirExpressionType> {
        self.expression_id(expression).and_then(|id| self.expression_type(id))
    }

    pub fn variant_construction(&self, id: ExpressionId) -> Option<&HirVariantConstruction> {
        self.variant_constructions.get(id.index())?.as_ref()
    }

    pub fn variant_constructions(&self) -> impl Iterator<Item = &HirVariantConstruction> {
        self.variant_constructions.iter().flatten()
    }

    /// Returns resolved constructor metadata for a syntax expression in this program.
    pub fn variant_construction_of(
        &self,
        expression: &Expression,
    ) -> Option<&HirVariantConstruction> {
        self.expression_id(expression).and_then(|id| self.variant_construction(id))
    }

    pub fn pattern_id(&self, pattern: &MatchPattern) -> Option<PatternId> {
        self.pattern_ids.get(&(pattern as *const MatchPattern as usize)).copied()
    }

    /// Validated decimal case value in the integer scrutinee's type.
    pub fn integer_pattern_of(&self, pattern: &MatchPattern) -> Option<&str> {
        self.pattern_id(pattern)
            .and_then(|id| self.integer_patterns.get(id.index()))
            .and_then(Option::as_deref)
    }

    pub fn variant_pattern(&self, id: PatternId) -> Option<&HirVariantPattern> {
        self.variant_patterns.get(id.index())?.as_ref()
    }

    pub fn variant_patterns(&self) -> impl Iterator<Item = &HirVariantPattern> {
        self.variant_patterns.iter().flatten()
    }

    /// Returns resolved variant metadata for a syntax pattern in this program.
    pub fn variant_pattern_of(&self, pattern: &MatchPattern) -> Option<&HirVariantPattern> {
        self.pattern_id(pattern).and_then(|id| self.variant_pattern(id))
    }
}

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

fn canonicalize_syntax_types(nodes: &mut [ASTNode]) {
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

fn walk_patterns_in_nodes(nodes: &[ASTNode], visit: &mut impl FnMut(&MatchPattern)) {
    for node in nodes {
        match node {
            ASTNode::Located { value, .. } => {
                walk_patterns_in_nodes(std::slice::from_ref(value), visit)
            },
            ASTNode::Function(function) => walk_patterns_in_nodes(&function.body, visit),
            ASTNode::Struct(structure) => {
                for method in &structure.methods {
                    walk_patterns_in_nodes(&method.body, visit);
                }
            },
            ASTNode::ProtoImpl(implementation) => {
                for method in &implementation.methods {
                    walk_patterns_in_nodes(&method.body, visit);
                }
            },
            ASTNode::Statement(statement) => walk_patterns_in_statement(statement, visit),
            ASTNode::ExternFunction(_)
            | ASTNode::Program(_)
            | ASTNode::Variable(_)
            | ASTNode::Expression(_)
            | ASTNode::TypeAlias(_)
            | ASTNode::Enum(_)
            | ASTNode::Variant(_) => {},
        }
    }
}

fn walk_patterns_in_statement(statement: &StatementNode, visit: &mut impl FnMut(&MatchPattern)) {
    match statement {
        StatementNode::If { body, else_if_blocks, else_block, .. } => {
            walk_patterns_in_nodes(body, visit);
            if let Some(blocks) = else_if_blocks {
                for (_, body) in blocks.iter() {
                    walk_patterns_in_nodes(body, visit);
                }
            }
            if let Some(body) = else_block {
                walk_patterns_in_nodes(body, visit);
            }
        },
        StatementNode::For { initialization, body, .. } => {
            walk_patterns_in_nodes(std::slice::from_ref(initialization.as_ref()), visit);
            walk_patterns_in_nodes(body, visit);
        },
        StatementNode::While { body, .. } => walk_patterns_in_nodes(body, visit),
        StatementNode::Match { arms, .. } => {
            for arm in arms {
                walk_pattern(&arm.pattern, visit);
                walk_patterns_in_nodes(&arm.body, visit);
            }
        },
        StatementNode::Print(_)
        | StatementNode::PrintFormat { .. }
        | StatementNode::Println(_)
        | StatementNode::PrintlnFormat { .. }
        | StatementNode::Input { .. }
        | StatementNode::Variable(_)
        | StatementNode::Import(_)
        | StatementNode::Assign { .. }
        | StatementNode::AsmBlock { .. }
        | StatementNode::Break
        | StatementNode::Continue
        | StatementNode::Return(_)
        | StatementNode::Expression(_) => {},
    }
}

fn walk_pattern(pattern: &MatchPattern, visit: &mut impl FnMut(&MatchPattern)) {
    visit(pattern);
    if let MatchPattern::Variant { payloads, .. } = pattern {
        for payload in payloads {
            walk_pattern(payload, visit);
        }
    }
}
