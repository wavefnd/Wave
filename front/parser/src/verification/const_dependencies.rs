// This file is part of the Wave language project.
// SPDX-License-Identifier: MPL-2.0
//! Validate constant value dependencies before either backend is entered.
use super::*;

pub(super) fn validate(
    nodes: &[ASTNode],
    sources: &crate::source::SourceMap,
) -> Result<(), SemanticDiagnostic> {
    let constants: Vec<_> = nodes
        .iter()
        .enumerate()
        .filter_map(|(index, node)| match node {
            ASTNode::Variable(variable) if variable.mutability == Mutability::Const => {
                Some((index, variable))
            },
            _ => None,
        })
        .collect();
    let names: HashMap<_, _> = constants
        .iter()
        .enumerate()
        .map(|(index, (_, variable))| (variable.name.as_str(), index))
        .collect();
    let mut edges = vec![Vec::new(); constants.len()];
    for (index, (_, variable)) in constants.iter().enumerate() {
        if let Some(initializer) = &variable.initial_value {
            crate::ast::visit::walk_expression(initializer, &mut |expression| {
                if let Expression::Variable(name) = expression {
                    if let Some(&dependency) = names.get(name.as_str()) {
                        edges[index].push((
                            dependency,
                            sources.expressions.get(&(expression as *const _ as usize)).cloned(),
                        ));
                    }
                }
            });
        }
    }
    // Iterative DFS keeps large acyclic declaration chains off the Rust stack.
    let mut colors = vec![0u8; constants.len()];
    for root in 0..constants.len() {
        if colors[root] != 0 {
            continue;
        }
        colors[root] = 1;
        let mut stack = vec![(root, 0usize)];
        while let Some((current, next)) = stack.last_mut() {
            if *next == edges[*current].len() {
                colors[*current] = 2;
                stack.pop();
                continue;
            }
            let current = *current;
            let (dependency, reference_span) = &edges[current][*next];
            *next += 1;
            match colors[*dependency] {
                0 => {
                    colors[*dependency] = 1;
                    stack.push((*dependency, 0));
                },
                1 => {
                    let start = stack.iter().position(|(node, _)| node == dependency).unwrap();
                    let cycle: Vec<_> = stack[start..]
                        .iter()
                        .map(|(node, _)| *node)
                        .chain(std::iter::once(*dependency))
                        .collect();
                    let message = format!(
                        "constant dependency cycle: {}",
                        cycle
                            .iter()
                            .map(|node| constants[*node].1.name.as_str())
                            .collect::<Vec<_>>()
                            .join(" -> ")
                    );
                    let mut diagnostic = semantic_diagnostic_for_top_level(
                        nodes,
                        constants[current].0,
                        message,
                        Some(SemanticSpanHint {
                            kind: SemanticSpanKind::Identifier,
                            text: constants[*dependency].1.name.clone(),
                            occurrence: 1,
                        }),
                    );
                    diagnostic.span = reference_span.clone().or_else(|| {
                        sources
                            .nodes
                            .get(&(&nodes[constants[current].0] as *const _ as usize))
                            .cloned()
                    });
                    diagnostic.note = Some(
                        cycle[..cycle.len() - 1]
                            .iter()
                            .map(|node| {
                                let (index, variable) = constants[*node];
                                match sources.nodes.get(&(&nodes[index] as *const _ as usize)) {
                                    Some(span) => format!(
                                        "{} declared at {}:{}:{}",
                                        variable.name, span.file, span.line, span.column
                                    ),
                                    None => format!("{} participates in this cycle", variable.name),
                                }
                            })
                            .collect::<Vec<_>>()
                            .join("; "),
                    );
                    diagnostic.help = "break the constant dependency cycle; noncyclic forward references are allowed".into();
                    return Err(diagnostic);
                },
                _ => {},
            }
        }
    }
    Ok(())
}
