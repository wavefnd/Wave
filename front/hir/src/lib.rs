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
mod canonicalization;
pub mod conversions;
mod defaults;
pub mod enums;
pub use enums::{EnumConversion, EnumType, EnumVariant, TypeId};
mod numeric_checks;
mod traversal;
use canonicalization::canonicalize_syntax_types;
pub use canonicalization::resolve_target_types;
use conversions::{ConversionError, NumericExpressionInfo};
pub use numeric_checks::{integer_literal_float, ConstantValue};
use parser::ast::visit::walk_nodes;
use traversal::walk_patterns_in_nodes;

use parser::ast::{ASTNode, Expression, MatchPattern, WaveType};
use parser::verification::{analyze_semantic_facts, SemanticDiagnostic, SemanticFacts};
use std::collections::HashMap;
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
    enum_types: Vec<EnumType>,
    enum_sources: HashMap<ExpressionId, TypeId>,
    enum_targets: HashMap<ExpressionId, TypeId>,
    enum_conversions: HashMap<ExpressionId, EnumConversion>,
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
        if let Err(diagnostic) = defaults::prepare(&mut syntax) {
            return Err(HirLoweringError { syntax, diagnostic });
        }
        // Preparation may extend argument vectors; detach spans and allocate
        // stable identities only after every occurrence owns its defaults.
        let source_map = parser::source::SourceMap::detach(&mut syntax);
        let SemanticFacts {
            enum_sources: analyzed_enum_sources,
            enum_targets: analyzed_enum_targets,
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
        let enum_types = enums::collect(&syntax);
        let identities =
            enum_types.iter().map(|ty| (ty.name.clone(), ty.id)).collect::<HashMap<_, _>>();
        let enum_sources = analyzed_enum_sources
            .into_iter()
            .filter_map(|(address, name)| {
                Some((*expression_ids.get(&address)?, *identities.get(&name)?))
            })
            .collect();
        let enum_targets = analyzed_enum_targets
            .into_iter()
            .filter_map(|(address, name)| {
                Some((*expression_ids.get(&address)?, *identities.get(&name)?))
            })
            .collect();
        let mut program = Self {
            enum_types,
            enum_sources,
            enum_targets,
            enum_conversions: HashMap::new(),
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
        program.enum_conversions = enums::conversions(&program);
        program.constant_values = match numeric_checks::validate(&program) {
            Ok(values) => values,
            Err(diagnostic) => return Err(HirLoweringError { syntax: program.syntax, diagnostic }),
        };
        Ok(program)
    }

    pub fn enum_types(&self) -> &[EnumType] {
        &self.enum_types
    }

    pub fn enum_type_of(&self, expression: &Expression) -> Option<TypeId> {
        self.expression_id(expression).and_then(|id| self.enum_sources.get(&id).copied())
    }

    pub fn enum_conversion_of(&self, expression: &Expression) -> Option<&EnumConversion> {
        self.expression_id(expression).and_then(|id| self.enum_conversions.get(&id))
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
