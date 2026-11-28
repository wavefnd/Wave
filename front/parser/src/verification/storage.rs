// SPDX-License-Identifier: MPL-2.0
//! Check by-value storage cycles before a backend asks LLVM for a layout.
use super::{program::substitute_wave_type, ProgramTypes, SemanticDiagnostic};
use crate::{ast::*, source::SourceMap};
use std::collections::{HashMap, HashSet};

// Exit markers make deep chains iterative. Pointers and future handles own no
// inline pointee storage and therefore terminate the storage dependency path.
enum Work {
    Enter(WaveType, usize, Option<error::SourceSpan>, usize),
    Exit(String),
}

pub(super) fn validate(
    nodes: &[ASTNode],
    sources: &SourceMap,
    program: &ProgramTypes,
) -> Result<(), SemanticDiagnostic> {
    let definitions: HashMap<_, _> = nodes
        .iter()
        .enumerate()
        .filter_map(|(index, node)| {
            if let ASTNode::Struct(structure) = node {
                Some((structure.name.as_str(), (index, structure)))
            } else {
                None
            }
        })
        .collect();
    let mut complete = HashSet::new();
    for (index, node) in nodes.iter().enumerate() {
        let ASTNode::Struct(structure) = node else {
            continue;
        };
        let name = if structure.generic_params.is_empty() {
            structure.name.clone()
        } else {
            format!("{}<{}>", structure.name, structure.generic_params.join(","))
        };
        let origin = sources.nodes.get(&(node as *const _ as usize)).cloned();
        let mut pending = vec![Work::Enter(WaveType::Struct(name), index, origin, 0)];
        let mut active = HashSet::new();
        while let Some(work) = pending.pop() {
            let Work::Enter(ty, owner, span, generic_depth) = work else {
                let Work::Exit(name) = work else { unreachable!() };
                active.remove(&name);
                complete.insert(name);
                continue;
            };
            let ty = program.canonical_type(&ty);
            let name = match ty {
                WaveType::Array(inner, _) => {
                    pending.push(Work::Enter(*inner, owner, span, generic_depth));
                    continue;
                },
                WaveType::Struct(name) | WaveType::Variant(name) => name,
                _ => continue,
            };
            if active.contains(&name) {
                let message = format!("type `{name}` has infinite size through by-value recursive storage; use pointer indirection");
                return Err(SemanticDiagnostic {
                    related: Vec::new(),
                    suggestions: Vec::new(),
                    code: "E3001".into(),
                    label: message.clone(),
                    message,
                    top_level_index: owner,
                    primary: None,
                    span,
                    note: None,
                    help: "break the storage cycle with `ptr<Type>` instead of an inline field"
                        .into(),
                });
            }
            if complete.contains(&name) {
                continue;
            }
            let depth = generic_depth + usize::from(name.contains('<'));
            if depth > crate::generics::MAX_TYPE_INSTANTIATION_DEPTH {
                // Expanding generic recursion retains the existing instantiation
                // depth error. Concrete instances are checked again after expansion.
                continue;
            }
            let base = program.named_type_base(&name);
            let substitution = program.generic_substitution(&name);
            active.insert(name.clone());
            pending.push(Work::Exit(name.clone()));
            if let Some(&(index, structure)) = definitions.get(base) {
                for (field_index, (_, ty)) in structure.fields.iter().enumerate().rev() {
                    let field_span =
                        structure.field_spans.get(field_index).cloned().flatten().or(span.clone());
                    pending.push(Work::Enter(
                        substitute_wave_type(ty, &substitution),
                        index,
                        field_span,
                        depth,
                    ));
                }
            } else if let Some(variant) = program.variant_type(&name) {
                for (_, payloads) in variant.cases.iter().rev() {
                    for ty in payloads.iter().rev() {
                        pending.push(Work::Enter(
                            substitute_wave_type(ty, &substitution),
                            owner,
                            span.clone(),
                            depth,
                        ));
                    }
                }
            }
        }
    }
    Ok(())
}

fn ungroup(mut value: &Expression) -> &Expression {
    while let Expression::Grouped(inner) | Expression::Located { value: inner, .. } = value {
        value = inner;
    }
    value
}

fn record_name(names: &mut HashSet<String>, expression: &Expression) {
    if let Expression::Variable(name) = ungroup(expression) {
        names.insert(name.clone());
    }
}

/// A source diagnostic requires a stable address. A reassigned or escaped
/// binding (including a loop back-edge) is checked at the actual write instead.
pub(super) fn rebound_names(body: &[ASTNode]) -> HashSet<String> {
    let mut expressions = HashSet::new();
    let mut statements = HashSet::new();
    visit::walk_nodes_with_statements(
        body,
        &mut |expr| match expr {
            Expression::Assignment { target, .. }
            | Expression::AssignOperation { target, .. }
            | Expression::IncDec { target, .. }
            | Expression::AddressOf(target) => record_name(&mut expressions, target),
            Expression::AsmBlock { outputs, .. } => {
                for (_, output) in outputs {
                    record_name(&mut expressions, output);
                }
            },
            _ => {},
        },
        &mut |statement| match statement {
            StatementNode::AsmBlock { outputs, .. } => {
                for (_, output) in outputs {
                    record_name(&mut statements, output);
                }
            },
            StatementNode::Assign { variable, .. } => {
                statements.insert(variable.clone());
            },
            StatementNode::Input { args, .. } => {
                for output in args {
                    record_name(&mut statements, output);
                }
            },
            _ => {},
        },
    );
    expressions.extend(statements);
    expressions
}

impl super::Validator<'_> {
    fn literal_storage(&self, place: &Expression) -> bool {
        match ungroup(place) {
            Expression::Deref(address) | Expression::IndexAccess { target: address, .. } => {
                self.literal_address(address)
            },
            Expression::FieldAccess { object, .. } => {
                self.literal_storage(object) || self.literal_address(object)
            },
            _ => false,
        }
    }

    pub(super) fn literal_address(&self, value: &Expression) -> bool {
        match ungroup(value) {
            Expression::Literal(Literal::String(_)) => true,
            Expression::Variable(name) => {
                self.lookup_binding(name).is_some_and(|b| b.literal_address)
            },
            Expression::Cast { expr, target_type }
                if matches!(
                    self.program.canonical_type(target_type),
                    WaveType::Pointer(_) | WaveType::String
                ) =>
            {
                self.literal_address(expr)
            },
            Expression::AddressOf(place) => self.literal_storage(place),
            _ => false,
        }
    }

    pub(super) fn ensure_writable_storage(
        &mut self,
        target: &Expression,
        operation: &str,
    ) -> Result<(), String> {
        if self.literal_storage(target) {
            if let Some(span) = self.source_map.expressions.get(&(target as *const _ as usize)) {
                self.source_span = Some(span.clone());
            }
            self.diagnostic_help =
                Some("write into a mutable byte buffer or a str backed by that buffer".into());
            return Err("cannot write to read-only string literal storage".into());
        }
        self.ensure_mutable_write_target(target, operation)
    }
}
