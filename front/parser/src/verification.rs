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

use crate::ast::{ASTNode, Expression, MatchPattern, Mutability, StatementNode};
use std::collections::{HashMap, HashSet};

fn lookup_mutability(
    name: &str,
    scopes: &Vec<HashMap<String, Mutability>>,
    globals: &HashMap<String, Mutability>,
) -> Option<Mutability> {
    for scope in scopes.iter().rev() {
        if let Some(m) = scope.get(name) {
            return Some(*m);
        }
    }
    globals.get(name).copied()
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

fn ensure_mutable_write_target(
    target: &Expression,
    scopes: &Vec<HashMap<String, Mutability>>,
    globals: &HashMap<String, Mutability>,
    why: &str,
) -> Result<(), String> {
    let Some((base, saw_deref)) = find_base_var(target, false) else {
        return Ok(());
    };

    if saw_deref {
        return Ok(());
    }

    if let Some(m) = lookup_mutability(&base, scopes, globals) {
        match m {
            Mutability::Let | Mutability::Const => {
                return Err(format!(
                    "cannot {} immutable binding `{}` ({:?})",
                    why, base, m
                ));
            }
            _ => {}
        }
    }

    Ok(())
}

fn validate_expr(
    expr: &Expression,
    scopes: &Vec<HashMap<String, Mutability>>,
    globals: &HashMap<String, Mutability>,
) -> Result<(), String> {
    match expr {
        Expression::IncDec { target, .. } => {
            ensure_mutable_write_target(target, scopes, globals, "modify with ++/--")?;
            validate_expr(target, scopes, globals)?;
        }

        Expression::AssignOperation { target, value, .. } => {
            ensure_mutable_write_target(target, scopes, globals, "assign")?;
            validate_expr(target, scopes, globals)?;
            validate_expr(value, scopes, globals)?;
        }

        Expression::Assignment { target, value } => {
            ensure_mutable_write_target(target, scopes, globals, "assign")?;
            validate_expr(target, scopes, globals)?;
            validate_expr(value, scopes, globals)?;
        }

        Expression::BinaryExpression { left, right, .. } => {
            validate_expr(left, scopes, globals)?;
            validate_expr(right, scopes, globals)?;
        }

        Expression::Unary { expr, .. } => validate_expr(expr, scopes, globals)?,
        Expression::Cast { expr, .. } => validate_expr(expr, scopes, globals)?,

        Expression::FunctionCall { args, .. } => {
            for a in args {
                validate_expr(a, scopes, globals)?;
            }
        }
        Expression::MethodCall { object, args, .. } => {
            validate_expr(object, scopes, globals)?;
            for a in args {
                validate_expr(a, scopes, globals)?;
            }
        }

        Expression::IndexAccess { target, index } => {
            validate_expr(target, scopes, globals)?;
            validate_expr(index, scopes, globals)?;
        }

        Expression::ArrayLiteral(items) => {
            for it in items {
                validate_expr(it, scopes, globals)?;
            }
        }

        Expression::FieldAccess { object, .. } => validate_expr(object, scopes, globals)?,

        Expression::StructLiteral { fields, .. } => {
            for (_, v) in fields {
                validate_expr(v, scopes, globals)?;
            }
        }

        Expression::AsmBlock {
            inputs, outputs, ..
        } => {
            for (_, e) in inputs {
                validate_expr(e, scopes, globals)?;
            }
            for (_, e) in outputs {
                validate_expr(e, scopes, globals)?;
            }
        }

        Expression::Deref(inner) | Expression::AddressOf(inner) => {
            validate_expr(inner, scopes, globals)?;
        }

        Expression::Null => {}

        Expression::Literal(_) => {}

        #[allow(clippy::collapsible_match)]
        Expression::Variable(name) => {
            if lookup_mutability(name, scopes, globals).is_none() {
                return Err(format!("use of undeclared identifier `{}`", name));
            }
        }

        _ => {}
    }

    Ok(())
}

fn validate_node(
    node: &ASTNode,
    scopes: &mut Vec<HashMap<String, Mutability>>,
    globals: &HashMap<String, Mutability>,
) -> Result<(), String> {
    match node {
        ASTNode::Variable(v) => {
            scopes
                .last_mut()
                .unwrap()
                .insert(v.name.clone(), v.mutability.clone());
        }

        ASTNode::Statement(stmt) => match stmt {
            StatementNode::Expression(e) => validate_expr(e, scopes, globals)?,

            StatementNode::Assign { variable, value } => {
                let fake_target = Expression::Variable(variable.clone());
                ensure_mutable_write_target(&fake_target, scopes, globals, "assign")?;
                validate_expr(value, scopes, globals)?;
            }

            StatementNode::PrintlnFormat { args, .. } | StatementNode::PrintFormat { args, .. } => {
                for a in args {
                    validate_expr(a, scopes, globals)?;
                }
            }

            StatementNode::Return(Some(e)) => validate_expr(e, scopes, globals)?,

            StatementNode::If {
                condition,
                body,
                else_if_blocks,
                else_block,
            } => {
                validate_expr(condition, scopes, globals)?;

                scopes.push(HashMap::new());
                for n in body {
                    validate_node(n, scopes, globals)?;
                }
                scopes.pop();

                if let Some(blocks) = else_if_blocks {
                    for (cond, b) in blocks.iter() {
                        validate_expr(cond, scopes, globals)?;
                        scopes.push(HashMap::new());
                        for n in b {
                            validate_node(n, scopes, globals)?;
                        }
                        scopes.pop();
                    }
                }

                if let Some(b) = else_block {
                    scopes.push(HashMap::new());
                    for n in b.iter() {
                        validate_node(n, scopes, globals)?;
                    }
                    scopes.pop();
                }
            }

            StatementNode::While { condition, body } => {
                validate_expr(condition, scopes, globals)?;
                scopes.push(HashMap::new());
                for n in body {
                    validate_node(n, scopes, globals)?;
                }
                scopes.pop();
            }

            StatementNode::For {
                initialization,
                condition,
                increment,
                body,
            } => {
                scopes.push(HashMap::new());

                validate_node(initialization, scopes, globals)?;
                validate_expr(condition, scopes, globals)?;
                validate_expr(increment, scopes, globals)?;

                for n in body {
                    validate_node(n, scopes, globals)?;
                }

                scopes.pop();
            }

            StatementNode::Match { value, arms } => {
                validate_expr(value, scopes, globals)?;

                let mut seen: HashSet<String> = HashSet::new();
                for arm in arms {
                    let key = match &arm.pattern {
                        MatchPattern::Int(raw) => format!("int:{}", raw.trim().replace('_', "")),
                        MatchPattern::Ident(name) => format!("ident:{}", name),
                        MatchPattern::Wildcard => "wildcard:_".to_string(),
                    };

                    if !seen.insert(key.clone()) {
                        return Err(format!("duplicate match case pattern `{}`", key));
                    }

                    scopes.push(HashMap::new());
                    for n in &arm.body {
                        validate_node(n, scopes, globals)?;
                    }
                    scopes.pop();
                }
            }

            _ => {}
        },

        ASTNode::Function(func) => {
            scopes.push(HashMap::new());

            for p in &func.parameters {
                scopes
                    .last_mut()
                    .unwrap()
                    .insert(p.name.clone(), Mutability::Var);
            }

            for n in &func.body {
                validate_node(n, scopes, globals)?;
            }

            scopes.pop();
        }

        #[allow(clippy::collapsible_match)]
        ASTNode::ExternFunction(ext) => {
            if !ext.abi.eq_ignore_ascii_case("c") {
                return Err(format!(
                    "unsupported extern ABI '{}' for function '{}': only extern(c) is currently supported",
                    ext.abi, ext.name
                ));
            }
        }

        _ => {}
    }

    Ok(())
}

pub fn validate_program(nodes: &Vec<ASTNode>) -> Result<(), String> {
    let mut globals: HashMap<String, Mutability> = HashMap::new();

    for n in nodes {
        match n {
            ASTNode::Variable(v) => {
                if matches!(v.mutability, Mutability::Const | Mutability::Static) {
                    globals.insert(v.name.clone(), v.mutability);
                }
            }

            // NEW: enum variants are constants
            ASTNode::Enum(e) => {
                for v in &e.variants {
                    globals.insert(v.name.clone(), Mutability::Const);
                }
            }

            _ => {}
        }
    }

    let mut scopes: Vec<HashMap<String, Mutability>> = vec![HashMap::new()];

    for n in nodes {
        validate_node(n, &mut scopes, &globals)?;
    }

    Ok(())
}
