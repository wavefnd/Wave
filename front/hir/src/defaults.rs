// SPDX-License-Identifier: MPL-2.0
//! Materialize omitted direct-call defaults before HIR identities and facts are
//! assigned. Each occurrence owns its syntax, source provenance and conversion
//! sequence; backends never borrow untyped declaration expressions.
use error::SourceSpan;
use parser::ast::{ASTNode, Expression, FunctionNode, ParameterNode, StatementNode};
use parser::verification::SemanticDiagnostic;
use std::collections::HashMap;

// Applies only to newly materialized defaults, never to existing program size.
// There is no public budget knob; ordinary generated-input failures are E3001.
const MAX_PREPARED_DEFAULT_NODES: usize = 1_000_000;

pub(super) fn prepare(nodes: &mut [ASTNode]) -> Result<(), SemanticDiagnostic> {
    prepare_with_limit(nodes, MAX_PREPARED_DEFAULT_NODES)
}

fn prepare_with_limit(nodes: &mut [ASTNode], limit: usize) -> Result<(), SemanticDiagnostic> {
    let mut remaining = limit;
    let parameters: HashMap<String, Vec<ParameterNode>> = nodes
        .iter()
        .filter_map(|node| match node.unspanned() {
            ASTNode::Function(function)
                if function
                    .parameters
                    .iter()
                    .any(|parameter| parameter.initial_value.is_some()) =>
            {
                Some((function.name.clone(), function.parameters.clone()))
            },
            _ => None,
        })
        .collect();
    for (top_level_index, node) in nodes.iter_mut().enumerate() {
        let mut pending = vec![Work::Node(node)];
        while let Some(work) = pending.pop() {
            match work {
                Work::Node(node) => match node {
                    ASTNode::Located { value, .. } => pending.push(Work::Node(value)),
                    ASTNode::Function(function) => push_function(function, &mut pending),
                    ASTNode::Struct(structure) => {
                        for function in &mut structure.methods {
                            push_function(function, &mut pending);
                        }
                    },
                    ASTNode::ProtoImpl(implementation) => {
                        for function in &mut implementation.methods {
                            push_function(function, &mut pending);
                        }
                    },
                    ASTNode::Statement(statement) => pending.push(Work::Statement(statement)),
                    ASTNode::Variable(variable) => {
                        if let Some(value) = &mut variable.initial_value {
                            pending.push(Work::Expression(value, 0, None));
                        }
                    },
                    ASTNode::Expression(value) => pending.push(Work::Expression(value, 0, None)),
                    ASTNode::Program(parameter) => {
                        if let Some(value) = &mut parameter.initial_value {
                            pending.push(Work::Expression(value, 0, None));
                        }
                    },
                    _ => {},
                },
                Work::Statement(statement) => push_statement(statement, &mut pending),
                Work::Expression(expression, depth, inherited_span) => {
                    let span = expression.span().cloned().or(inherited_span);
                    if let Expression::Located { value, .. } = expression {
                        pending.push(Work::Expression(value, depth, span));
                        continue;
                    }
                    let constructor = !matches!(
                        expression,
                        Expression::Literal(_) | Expression::Variable(_) | Expression::Null
                    );
                    let next_depth = depth + usize::from(constructor);
                    if next_depth > parser::expression_depth::MAX_EXPRESSION_DEPTH {
                        return Err(SemanticDiagnostic {
                            related: Vec::new(),
                            suggestions: Vec::new(),
                            code: "E3001".into(),
                            message: "expression nesting exceeds the maximum of 128 levels".into(),
                            top_level_index,
                            primary: None,
                            span,
                            label: "default argument expansion exceeds expression depth".into(),
                            note: None,
                            help: "split the expression into smaller expressions using intermediate bindings".into(),
                        });
                    }
                    if let Expression::FunctionCall { name, args, .. } = expression {
                        if let Some(params) = parameters.get(name) {
                            // Leave invalid arities to established semantic
                            // diagnostics instead of changing their contract.
                            if args.len() < params.len()
                                && params[args.len()..].iter().all(|p| p.initial_value.is_some())
                            {
                                // Charge template size before cloning. Depth
                                // alone cannot bound a branching default DAG.
                                for parameter in &params[args.len()..] {
                                    let default = parameter.initial_value.as_ref().unwrap();
                                    if !charge(default, &mut remaining) {
                                        return Err(SemanticDiagnostic {
                                            related: Vec::new(),
                                            suggestions: Vec::new(),
                                            code: "E3001".into(),
                                            message: format!("default argument preparation exceeds the limit of {limit} generated expression nodes"),
                                            top_level_index,
                                            primary: None,
                                            span: default.span().cloned().or_else(|| parameter.span.clone()),
                                            label: "default argument expansion exceeds its resource budget".into(),
                                            note: None,
                                            help: "pass explicit arguments or simplify nested default expressions".into(),
                                        });
                                    }
                                }
                                for parameter in &params[args.len()..] {
                                    let default = parameter.initial_value.as_ref().unwrap();
                                    let provenance = default
                                        .span()
                                        .cloned()
                                        .or_else(|| parameter.span.clone())
                                        .map(|s| {
                                            s.generated(format!("default argument for {name}"))
                                        });
                                    args.push(default.clone().with_span(provenance));
                                }
                            }
                        }
                    }
                    push_expression_children(expression, next_depth, &mut pending);
                },
            }
        }
    }
    Ok(())
}

enum Work<'a> {
    Node(&'a mut ASTNode),
    Statement(&'a mut StatementNode),
    Expression(&'a mut Expression, usize, Option<SourceSpan>),
}

fn push_function<'a>(function: &'a mut FunctionNode, pending: &mut Vec<Work<'a>>) {
    for parameter in &mut function.parameters {
        if let Some(default) = &mut parameter.initial_value {
            pending.push(Work::Expression(default, 0, None));
        }
    }
    pending.extend(function.body.iter_mut().map(Work::Node));
}

fn push_statement<'a>(statement: &'a mut StatementNode, pending: &mut Vec<Work<'a>>) {
    match statement {
        StatementNode::PrintFormat { args, .. }
        | StatementNode::PrintlnFormat { args, .. }
        | StatementNode::Input { args, .. } => {
            pending.extend(args.iter_mut().map(|e| Work::Expression(e, 0, None)));
        },
        StatementNode::If { condition, body, else_if_blocks, else_block } => {
            pending.push(Work::Expression(condition, 0, None));
            pending.extend(body.iter_mut().map(Work::Node));
            if let Some(blocks) = else_if_blocks {
                for (condition, body) in blocks.iter_mut() {
                    pending.push(Work::Expression(condition, 0, None));
                    pending.extend(body.iter_mut().map(Work::Node));
                }
            }
            if let Some(body) = else_block {
                pending.extend(body.iter_mut().map(Work::Node));
            }
        },
        StatementNode::For { initialization, condition, increment, body } => {
            pending.push(Work::Node(initialization));
            pending.push(Work::Expression(condition, 0, None));
            pending.push(Work::Expression(increment, 0, None));
            pending.extend(body.iter_mut().map(Work::Node));
        },
        StatementNode::While { condition, body } => {
            pending.push(Work::Expression(condition, 0, None));
            pending.extend(body.iter_mut().map(Work::Node));
        },
        StatementNode::Match { value, arms } => {
            pending.push(Work::Expression(value, 0, None));
            for arm in arms {
                pending.extend(arm.body.iter_mut().map(Work::Node));
            }
        },
        StatementNode::Assign { value, .. }
        | StatementNode::Return(Some(value))
        | StatementNode::Expression(value) => pending.push(Work::Expression(value, 0, None)),
        StatementNode::AsmBlock { inputs, outputs, .. } => {
            pending.extend(
                inputs.iter_mut().chain(outputs).map(|(_, e)| Work::Expression(e, 0, None)),
            );
        },
        _ => {},
    }
}

fn push_expression_children<'a>(
    expression: &'a mut Expression,
    depth: usize,
    pending: &mut Vec<Work<'a>>,
) {
    match expression {
        Expression::StructLiteral { fields, .. } => {
            pending
                .extend(fields.iter_mut().map(|(_, value)| Work::Expression(value, depth, None)));
        },
        Expression::FunctionCall { args, .. } | Expression::ArrayLiteral(args) => {
            pending.extend(args.iter_mut().map(|value| Work::Expression(value, depth, None)));
        },
        Expression::MethodCall { object, args, .. } => {
            pending.push(Work::Expression(object, depth, None));
            pending.extend(args.iter_mut().map(|value| Work::Expression(value, depth, None)));
        },
        Expression::Deref(inner)
        | Expression::AddressOf(inner)
        | Expression::Await(inner)
        | Expression::Grouped(inner)
        | Expression::Unary { expr: inner, .. }
        | Expression::Cast { expr: inner, .. }
        | Expression::FieldAccess { object: inner, .. }
        | Expression::IncDec { target: inner, .. } => {
            pending.push(Work::Expression(inner, depth, None))
        },
        Expression::BinaryExpression { left, right, .. }
        | Expression::IndexAccess { target: left, index: right }
        | Expression::AssignOperation { target: left, value: right, .. }
        | Expression::Assignment { target: left, value: right } => {
            pending.push(Work::Expression(left, depth, None));
            pending.push(Work::Expression(right, depth, None));
        },
        Expression::AsmBlock { inputs, outputs, .. } => {
            pending.extend(
                inputs.iter_mut().chain(outputs).map(|(_, e)| Work::Expression(e, depth, None)),
            );
        },
        Expression::Located { .. } => unreachable!("handled before child traversal"),
        Expression::Null | Expression::Literal(_) | Expression::Variable(_) => {},
    }
}

// Source depth was validated before preparation. This iterative preflight
// counts a template without cloning it or allocating an equally wide worklist.
fn charge(expression: &Expression, remaining: &mut usize) -> bool {
    let mut pending = vec![expression];
    while let Some(expression) = pending.pop() {
        if !matches!(expression, Expression::Located { .. }) {
            let Some(next) = remaining.checked_sub(1) else {
                return false;
            };
            *remaining = next;
        }
        let mut exhausted = false;
        parser::ast::visit::walk_expression_children(expression, &mut |child| {
            if pending.len() >= *remaining {
                exhausted = true;
            } else {
                pending.push(child);
            }
        });
        if exhausted {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    fn syntax(source: &str) -> Vec<ASTNode> {
        let tokens = lexer::Lexer::new_with_file(source, "default-budget.wave").tokenize().unwrap();
        parser::parse_syntax_with_spans(&tokens).unwrap()
    }

    #[test]
    fn default_budget_is_injected_small_and_reports_declaration_provenance() {
        let mut nodes = syntax("fun pick(n: i32 = 7) -> i32 { return n; } fun probe() -> i32 { return pick() + pick() + pick(); }");
        let error = prepare_with_limit(&mut nodes, 2).unwrap_err();
        assert_eq!(error.code, "E3001");
        assert!(error.message.contains("limit of 2"));
        assert_eq!(error.span.as_ref().unwrap().file, "default-budget.wave");
        assert_eq!(error.span.as_ref().unwrap().column, 19);
        let mut nodes = syntax("fun pick(n: i32) -> i32 { return n; } fun probe() -> i32 { return pick(7) + pick(8); }");
        prepare_with_limit(&mut nodes, 0).unwrap();
    }

    #[test]
    fn oversized_template_is_rejected_before_call_arguments_are_cloned() {
        let expression = Expression::ArrayLiteral(vec![
            Expression::Literal(
                parser::ast::Literal::Int("1".into())
            );
            8
        ]);
        let mut remaining = 4;
        assert!(!charge(&expression, &mut remaining));
        let mut remaining = 9;
        assert!(charge(&expression, &mut remaining));
        assert_eq!(remaining, 0);

        let mut nodes = syntax(
            "fun pick(n: i32 = 1) -> i32 { return 0; } fun probe() -> i32 { return pick(); }",
        );
        for node in &mut nodes {
            let mut node = node;
            while let ASTNode::Located { value, .. } = node {
                node = value;
            }
            if let ASTNode::Function(function) = node {
                if function.name == "pick" {
                    function.parameters[0].param_type =
                        parser::ast::WaveType::Array(Box::new(parser::ast::WaveType::Int(32)), 8);
                    function.parameters[0].initial_value = Some(expression.clone());
                }
            }
        }
        assert!(prepare_with_limit(&mut nodes, 4).is_err());
        let mut arities = Vec::new();
        parser::ast::visit::walk_nodes(&nodes, &mut |expression| {
            if let Expression::FunctionCall { name, args, .. } = expression {
                if name == "pick" {
                    arities.push(args.len());
                }
            }
        });
        assert_eq!(arities, [0], "oversized defaults must never be cloned into the call");
    }
}
