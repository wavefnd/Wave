//! Generic methods reuse function specialization after semantic receiver resolution.
//!
//! Explicit arguments bind method parameters in declaration order. Otherwise,
//! receiver and argument types must infer every parameter consistently; plain
//! integer/float literals use the frontend's i32/f32 defaults. Enclosing type
//! parameters are substituted independently and cannot be shadowed by a method.
//! Bodies are then specialized by the shared work queue, preserving source spans.
use crate::ast::{ASTNode, Expression, StatementNode, WaveType};

#[derive(Clone, Debug)]
pub(crate) struct GenericMethodCall {
    pub function: String,
    pub type_args: Vec<WaveType>,
    pub defaults: Vec<Expression>,
    pub rewrite: bool,
}

pub(crate) use crate::ast::method_symbol;

pub(crate) fn lower_generic_methods(mut ast: Vec<ASTNode>) -> Result<Vec<ASTNode>, String> {
    let needs_lowering = |f: &crate::ast::FunctionNode| {
        !f.generic_params.is_empty() || f.parameters.iter().any(|p| p.initial_value.is_some())
    };
    if !ast.iter().any(|node| match node.unspanned() {
        ASTNode::Function(f) => needs_lowering(f),
        ASTNode::Struct(s) => s.methods.iter().any(needs_lowering),
        ASTNode::ProtoImpl(p) => p.methods.iter().any(needs_lowering),
        _ => false,
    }) {
        return Ok(ast);
    }
    let mut snapshot = ast.clone();
    let sources = crate::source::SourceMap::detach(&mut snapshot);
    let mut calls = crate::verification::analyze_generic_method_calls(&snapshot, &sources)
        .map_err(|e| e.to_string())?;
    let mut ordered = Vec::new();
    walk_nodes(&mut snapshot, &mut |expression| {
        ordered.push(calls.remove(&(expression as *const _ as usize)))
    });
    let mut ordered = ordered.into_iter();
    walk_nodes(&mut ast, &mut |expression| {
        let Some(call) = ordered.next().expect("source and semantic traversal must agree") else {
            return;
        };
        if !call.rewrite {
            if let Expression::MethodCall { args, .. } = expression {
                args.extend(call.defaults);
            }
            return;
        }
        let mut arguments = match std::mem::replace(expression, Expression::Null) {
            Expression::MethodCall { object, args, .. } => {
                let mut arguments = vec![*object];
                arguments.extend(args);
                arguments
            },
            Expression::FunctionCall { args, .. } => args,
            _ => unreachable!("semantic generic call resolution"),
        };
        arguments.extend(call.defaults);
        *expression = Expression::FunctionCall {
            name: call.function,
            type_args: call.type_args,
            args: arguments,
        };
    });
    assert!(ordered.next().is_none(), "source and semantic traversal must agree");
    let mut lifted = Vec::new();
    for node in &mut ast {
        lift(node, &mut lifted);
    }
    ast.extend(lifted);
    Ok(ast)
}

fn lift(node: &mut ASTNode, lifted: &mut Vec<ASTNode>) {
    let span = node.span().cloned();
    let (owner, parameters, methods) = match node {
        ASTNode::Located { value, .. } => {
            let start = lifted.len();
            lift(value, lifted);
            for node in &mut lifted[start..] {
                *node = node.clone().with_span(span.clone());
            }
            return;
        },
        ASTNode::Struct(s) => (&s.name, s.generic_params.clone(), &mut s.methods),
        ASTNode::ProtoImpl(p) => (&p.target, Vec::new(), &mut p.methods),
        _ => return,
    };
    let mut retained = Vec::new();
    for mut method in std::mem::take(methods) {
        if method.generic_params.is_empty() {
            retained.push(method);
            continue;
        }
        method.name = method_symbol(owner, &method.name);
        let mut all = parameters.clone();
        all.extend(method.generic_params);
        method.generic_params = all;
        lifted.push(ASTNode::Function(method).with_span(span.clone()));
    }
    *methods = retained;
}

pub(crate) fn walk_nodes(nodes: &mut [ASTNode], visit: &mut impl FnMut(&mut Expression)) {
    for node in nodes {
        walk_node(node, visit);
    }
}

fn walk_node(node: &mut ASTNode, visit: &mut impl FnMut(&mut Expression)) {
    match node {
        ASTNode::Located { value, .. } => walk_node(value, visit),
        ASTNode::Function(function) => {
            for parameter in &mut function.parameters {
                if let Some(default) = &mut parameter.initial_value {
                    walk_expression(default, visit);
                }
            }
            walk_nodes(&mut function.body, visit);
        },
        ASTNode::Struct(structure) => {
            for method in &mut structure.methods {
                for parameter in &mut method.parameters {
                    if let Some(default) = &mut parameter.initial_value {
                        walk_expression(default, visit);
                    }
                }
                walk_nodes(&mut method.body, visit);
            }
        },
        ASTNode::ProtoImpl(implementation) => {
            for method in &mut implementation.methods {
                for parameter in &mut method.parameters {
                    if let Some(default) = &mut parameter.initial_value {
                        walk_expression(default, visit);
                    }
                }
                walk_nodes(&mut method.body, visit);
            }
        },
        ASTNode::Statement(statement) => walk_statement(statement, visit),
        ASTNode::Variable(variable) => {
            if let Some(initializer) = &mut variable.initial_value {
                walk_expression(initializer, visit);
            }
        },
        ASTNode::Expression(expression) => walk_expression(expression, visit),
        ASTNode::ExternFunction(_)
        | ASTNode::Program(_)
        | ASTNode::TypeAlias(_)
        | ASTNode::Enum(_)
        | ASTNode::Variant(_) => {},
    }
}

fn walk_statement(statement: &mut StatementNode, visit: &mut impl FnMut(&mut Expression)) {
    match statement {
        StatementNode::PrintFormat { args, .. }
        | StatementNode::PrintlnFormat { args, .. }
        | StatementNode::Input { args, .. } => {
            for argument in args {
                walk_expression(argument, visit);
            }
        },
        StatementNode::If { condition, body, else_if_blocks, else_block } => {
            walk_expression(condition, visit);
            walk_nodes(body, visit);
            if let Some(blocks) = else_if_blocks {
                for (condition, body) in blocks.iter_mut() {
                    walk_expression(condition, visit);
                    walk_nodes(body, visit);
                }
            }
            if let Some(body) = else_block {
                walk_nodes(body, visit);
            }
        },
        StatementNode::For { initialization, condition, increment, body } => {
            walk_node(initialization, visit);
            walk_expression(condition, visit);
            walk_expression(increment, visit);
            walk_nodes(body, visit);
        },
        StatementNode::While { condition, body } => {
            walk_expression(condition, visit);
            walk_nodes(body, visit);
        },
        StatementNode::Match { value, arms } => {
            walk_expression(value, visit);
            for arm in arms {
                walk_nodes(&mut arm.body, visit);
            }
        },
        StatementNode::Assign { value, .. } => walk_expression(value, visit),
        StatementNode::AsmBlock { inputs, outputs, .. } => {
            for (_, expression) in inputs.iter_mut().chain(outputs.iter_mut()) {
                walk_expression(expression, visit);
            }
        },
        StatementNode::Return(Some(expression)) | StatementNode::Expression(expression) => {
            walk_expression(expression, visit)
        },
        StatementNode::Print(_)
        | StatementNode::Println(_)
        | StatementNode::Variable(_)
        | StatementNode::Import(_)
        | StatementNode::Break
        | StatementNode::Continue
        | StatementNode::Return(None) => {},
    }
}

fn walk_expression(expression: &mut Expression, visit: &mut impl FnMut(&mut Expression)) {
    if let Expression::Located { value, .. } = expression {
        walk_expression(value, visit);
        return;
    }
    match expression {
        Expression::Located { value, .. } => walk_expression(value, visit),
        Expression::StructLiteral { fields, .. } => {
            for (_, value) in fields {
                walk_expression(value, visit);
            }
        },
        Expression::FunctionCall { args, .. } => {
            for argument in args {
                walk_expression(argument, visit);
            }
        },
        Expression::MethodCall { object, args, .. } => {
            walk_expression(object, visit);
            for argument in args {
                walk_expression(argument, visit);
            }
        },
        Expression::Deref(inner)
        | Expression::AddressOf(inner)
        | Expression::Await(inner)
        | Expression::Grouped(inner)
        | Expression::Unary { expr: inner, .. }
        | Expression::Cast { expr: inner, .. }
        | Expression::FieldAccess { object: inner, .. }
        | Expression::IncDec { target: inner, .. } => walk_expression(inner, visit),
        Expression::BinaryExpression { left, right, .. }
        | Expression::IndexAccess { target: left, index: right }
        | Expression::AssignOperation { target: left, value: right, .. }
        | Expression::Assignment { target: left, value: right } => {
            walk_expression(left, visit);
            walk_expression(right, visit);
        },
        Expression::ArrayLiteral(values) => {
            for value in values {
                walk_expression(value, visit);
            }
        },
        Expression::AsmBlock { inputs, outputs, .. } => {
            for (_, expression) in inputs.iter_mut().chain(outputs.iter_mut()) {
                walk_expression(expression, visit);
            }
        },
        Expression::Null | Expression::Literal(_) | Expression::Variable(_) => {},
    }
    visit(expression);
}
