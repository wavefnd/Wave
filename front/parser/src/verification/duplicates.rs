// SPDX-License-Identifier: MPL-2.0
//! Duplicate provenance is collected before type maps discard earlier entries.
use super::SemanticDiagnostic;
use crate::ast::ASTNode;
use crate::source::SourceMap;
use error::{RelatedDiagnostic, SourceSpan};
use std::collections::HashMap;

fn focus(span: &SourceSpan) -> SourceSpan {
    span.focus.as_deref().unwrap_or(span).clone()
}

fn record(
    names: &mut HashMap<String, Option<SourceSpan>>,
    name: &str,
    span: Option<SourceSpan>,
    index: usize,
    message: String,
) -> Result<(), SemanticDiagnostic> {
    if let Some(previous) = names.get(name) {
        return Err(SemanticDiagnostic {
            code: "E3001".into(),
            label: message.clone(),
            message,
            top_level_index: index,
            primary: None,
            span,
            related: previous
                .as_ref()
                .map(|span| RelatedDiagnostic {
                    message: "previous declaration is here".into(),
                    span: span.clone(),
                })
                .into_iter()
                .collect(),
            suggestions: Vec::new(),
            note: None,
            help: "rename or remove the conflicting declaration".into(),
        });
    }
    names.insert(name.to_string(), span);
    Ok(())
}

pub(super) fn validate(nodes: &[ASTNode], sources: &SourceMap) -> Result<(), SemanticDiagnostic> {
    let mut types = HashMap::new();
    let mut values = HashMap::new();
    for (index, node) in nodes.iter().enumerate() {
        let span = sources.nodes.get(&(node as *const _ as usize)).map(focus);
        let (name, names) = match node.unspanned() {
            ASTNode::Function(n) => (&n.name, &mut values),
            ASTNode::ExternFunction(n) => (&n.name, &mut values),
            ASTNode::Variable(n) => (&n.name, &mut values),
            ASTNode::TypeAlias(n) => (&n.name, &mut types),
            ASTNode::Enum(n) => (&n.name, &mut types),
            ASTNode::Variant(n) => (&n.name, &mut types),
            ASTNode::Struct(n) => {
                let mut fields = HashMap::new();
                for (field_index, (name, _)) in n.fields.iter().enumerate() {
                    let mut span = n.field_spans.get(field_index).cloned().flatten();
                    if let Some(span) = &mut span {
                        // The field span starts at its name. Narrow by Unicode
                        // scalar and UTF-8 byte lengths, without searching text.
                        span.end = span.start + name.len();
                        span.end_line = span.line;
                        span.end_column = span.column + name.chars().count();
                        span.focus = None;
                    }
                    record(
                        &mut fields,
                        name,
                        span,
                        index,
                        format!("duplicate field `{name}` in struct `{}`", n.name),
                    )?;
                }
                (&n.name, &mut types)
            },
            _ => continue,
        };
        let kind = if matches!(
            node.unspanned(),
            ASTNode::Function(_) | ASTNode::ExternFunction(_) | ASTNode::Variable(_)
        ) {
            "value"
        } else {
            "type"
        };
        record(names, name, span, index, format!("duplicate {kind} declaration `{name}`"))?;
    }
    Ok(())
}
