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

//! Semantic facts produced from a stable, source-mapped AST.
use crate::ast::WaveType;
use std::collections::HashMap;

/// The semantic type known before contextual lowering is performed.
///
/// Literal forms remain explicit because their final representation can depend
/// on an assignment, argument, return, or aggregate context. They are not
/// silently committed to a backend type at this boundary.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AnalyzedExpressionType {
    Resolved(WaveType),
    IntegerLiteral,
    FloatLiteral,
    Null,
    ArrayLiteral,
    AddressedArrayLiteral,
    Unknown,
}

/// Fully resolved variant constructor selected by semantic analysis.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariantConstruction {
    pub variant_type: WaveType,
    pub case_name: String,
    pub discriminant: u32,
    pub payload_types: Vec<WaveType>,
}

/// Concrete variant case selected by a semantically validated pattern.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariantPattern {
    pub variant_type: WaveType,
    pub case_name: String,
    pub discriminant: u32,
    pub payload_types: Vec<WaveType>,
}

/// Address-keyed facts for one analyzed AST allocation. Consumers must keep the
/// syntax allocations stable until these facts have been attached to their IR.
#[derive(Debug)]
pub struct SemanticFacts {
    pub enum_sources: HashMap<usize, String>,
    pub enum_targets: HashMap<usize, String>,
    pub expression_types: HashMap<usize, AnalyzedExpressionType>,
    pub variant_constructions: HashMap<usize, VariantConstruction>,
    pub variant_patterns: HashMap<usize, VariantPattern>,
    pub expected_types: HashMap<usize, WaveType>,
    pub integer_patterns: HashMap<usize, String>,
}
