// SPDX-License-Identifier: MPL-2.0
//! Clone nested statement containers with one heap worklist. Derived cloning
//! otherwise reserves large enum temporaries at every accepted block level.
use super::*;

fn children(node: &ASTNode) -> Vec<&ASTNode> {
    let mut out = Vec::new();
    match node {
        ASTNode::Located { value, .. } => out.push(value.as_ref()),
        ASTNode::Function(n) => out.extend(n.body.iter()),
        ASTNode::Struct(n) => {
            for method in &n.methods {
                out.extend(method.body.iter());
            }
        },
        ASTNode::ProtoImpl(n) => {
            for method in &n.methods {
                out.extend(method.body.iter());
            }
        },
        ASTNode::Statement(statement) => match statement {
            StatementNode::If { body, else_if_blocks, else_block, .. } => {
                out.extend(body.iter());
                if let Some(blocks) = else_if_blocks {
                    for (_, body) in blocks.iter() {
                        out.extend(body.iter());
                    }
                }
                if let Some(body) = else_block {
                    out.extend(body.iter());
                }
            },
            StatementNode::While { body, .. } => out.extend(body.iter()),
            StatementNode::For { initialization, body, .. } => {
                out.push(initialization.as_ref());
                out.extend(body.iter());
            },
            StatementNode::Match { arms, .. } => {
                for arm in arms {
                    out.extend(arm.body.iter());
                }
            },
            _ => {},
        },
        _ => {},
    }
    out
}

fn take(nodes: &mut std::vec::IntoIter<ASTNode>, count: usize) -> Vec<ASTNode> {
    nodes.by_ref().take(count).collect()
}

fn function(n: &FunctionNode, nodes: &mut std::vec::IntoIter<ASTNode>) -> FunctionNode {
    FunctionNode {
        is_async: n.is_async,
        span: n.span.clone(),
        name: n.name.clone(),
        generic_params: n.generic_params.clone(),
        parameters: n.parameters.clone(),
        return_type: n.return_type.clone(),
        return_type_span: n.return_type_span.clone(),
        body: take(nodes, n.body.len()),
        export: n.export.clone(),
        visibility: n.visibility,
    }
}

fn statement(n: &StatementNode, nodes: &mut std::vec::IntoIter<ASTNode>) -> StatementNode {
    match n {
        StatementNode::If { condition, body, else_if_blocks, else_block } => StatementNode::If {
            condition: condition.clone(),
            body: take(nodes, body.len()),
            else_if_blocks: else_if_blocks.as_ref().map(|blocks| {
                Box::new(
                    blocks
                        .iter()
                        .map(|(condition, body)| (condition.clone(), take(nodes, body.len())))
                        .collect(),
                )
            }),
            else_block: else_block.as_ref().map(|body| Box::new(take(nodes, body.len()))),
        },
        StatementNode::While { condition, body } => {
            StatementNode::While { condition: condition.clone(), body: take(nodes, body.len()) }
        },
        StatementNode::For { initialization: _, condition, increment, body } => {
            StatementNode::For {
                initialization: Box::new(nodes.next().expect("cloned initializer")),
                condition: condition.clone(),
                increment: increment.clone(),
                body: take(nodes, body.len()),
            }
        },
        StatementNode::Match { value, arms } => StatementNode::Match {
            value: value.clone(),
            arms: arms
                .iter()
                .map(|arm| MatchArm {
                    span: arm.span.clone(),
                    pattern: arm.pattern.clone(),
                    body: take(nodes, arm.body.len()),
                })
                .collect(),
        },
        other => other.clone(),
    }
}
impl Clone for ASTNode {
    fn clone(&self) -> Self {
        let mut pending = vec![(self, false)];
        let mut values = Vec::new();
        while let Some((node, ready)) = pending.pop() {
            let children = children(node);
            if !ready {
                pending.push((node, true));
                for child in children.into_iter().rev() {
                    pending.push((child, false));
                }
                continue;
            }
            let mut nodes = values.split_off(values.len() - children.len()).into_iter();
            let cloned = match node {
                ASTNode::Located { span, .. } => {
                    ASTNode::Located { value: Box::new(nodes.next().unwrap()), span: span.clone() }
                },
                ASTNode::Function(n) => ASTNode::Function(function(n, &mut nodes)),
                ASTNode::Struct(n) => ASTNode::Struct(StructNode {
                    name: n.name.clone(),
                    generic_params: n.generic_params.clone(),
                    fields: n.fields.clone(),
                    field_spans: n.field_spans.clone(),
                    methods: n.methods.iter().map(|n| function(n, &mut nodes)).collect(),
                    visibility: n.visibility,
                }),
                ASTNode::ProtoImpl(n) => ASTNode::ProtoImpl(ProtoImplNode {
                    target: n.target.clone(),
                    methods: n.methods.iter().map(|n| function(n, &mut nodes)).collect(),
                }),
                ASTNode::Statement(n) => ASTNode::Statement(statement(n, &mut nodes)),
                ASTNode::ExternFunction(n) => ASTNode::ExternFunction(n.clone()),
                ASTNode::Program(n) => ASTNode::Program(n.clone()),
                ASTNode::Variable(n) => ASTNode::Variable(n.clone()),
                ASTNode::Expression(n) => ASTNode::Expression(n.clone()),
                ASTNode::TypeAlias(n) => ASTNode::TypeAlias(n.clone()),
                ASTNode::Enum(n) => ASTNode::Enum(n.clone()),
                ASTNode::Variant(n) => ASTNode::Variant(n.clone()),
            };
            debug_assert!(nodes.next().is_none());
            values.push(cloned);
        }
        values.pop().expect("cloned node")
    }
}
