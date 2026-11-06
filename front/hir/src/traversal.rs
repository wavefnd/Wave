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
//! Match-pattern traversal in stable source order for HIR identity assignment.

use parser::ast::{ASTNode, MatchPattern, StatementNode};

pub(super) fn walk_patterns_in_nodes(nodes: &[ASTNode], visit: &mut impl FnMut(&MatchPattern)) {
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

pub(super) fn walk_pattern(pattern: &MatchPattern, visit: &mut impl FnMut(&MatchPattern)) {
    visit(pattern);
    if let MatchPattern::Variant { payloads, .. } = pattern {
        for payload in payloads {
            walk_pattern(payload, visit);
        }
    }
}
