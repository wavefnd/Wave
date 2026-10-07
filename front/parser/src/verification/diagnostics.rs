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

//! Shared semantic diagnostic records and declaration anchors.
use crate::ast::ASTNode;
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

pub(super) fn top_level_span_hint(node: &ASTNode) -> SemanticSpanHint {
    let (kind, text) = match node {
        ASTNode::Located { value, .. } => return top_level_span_hint(value),
        ASTNode::Function(function) => (SemanticSpanKind::Declaration, function.name.clone()),
        ASTNode::ExternFunction(function) => (SemanticSpanKind::Declaration, function.name.clone()),
        ASTNode::Struct(structure) => (SemanticSpanKind::Declaration, structure.name.clone()),
        ASTNode::ProtoImpl(implementation) => {
            (SemanticSpanKind::Declaration, implementation.target.clone())
        },
        ASTNode::TypeAlias(alias) => (SemanticSpanKind::Declaration, alias.name.clone()),
        ASTNode::Enum(enumeration) => (SemanticSpanKind::Declaration, enumeration.name.clone()),
        ASTNode::Variant(variant) => (SemanticSpanKind::Declaration, variant.name.clone()),
        ASTNode::Variable(variable) => (SemanticSpanKind::Declaration, variable.name.clone()),
        ASTNode::Statement(_) | ASTNode::Expression(_) | ASTNode::Program(_) => {
            (SemanticSpanKind::Keyword, "program".to_string())
        },
    };
    SemanticSpanHint { kind, text, occurrence: 1 }
}
