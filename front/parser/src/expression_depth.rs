// SPDX-License-Identifier: MPL-2.0
//! One expression-depth contract for parsing, generated ASTs and HIR input.
use crate::ast::{ASTNode, Expression, FunctionNode, StatementNode};
use crate::parser::ParseError;
use crate::verification::SemanticDiagnostic;
use error::SourceSpan;
use lexer::Token;
use std::cell::Cell;

pub const MAX_EXPRESSION_DEPTH: usize = 128;
const MESSAGE: &str = "expression nesting exceeds the maximum of 128 levels";
const HELP: &str = "split the expression into smaller expressions using intermediate bindings";
thread_local! { static PARSE_DEPTH: Cell<usize> = const { Cell::new(0) }; }

pub(crate) struct Nesting;
impl Nesting {
    pub(crate) fn enter(token: Option<&Token>) -> Result<Self, ParseError> {
        PARSE_DEPTH.with(|depth| {
            if depth.get() == MAX_EXPRESSION_DEPTH {
                Err(ParseError::syntax_at(token, MESSAGE).with_help(HELP))
            } else {
                depth.set(depth.get() + 1);
                Ok(Self)
            }
        })
    }
}
impl Drop for Nesting {
    fn drop(&mut self) {
        PARSE_DEPTH.with(|depth| depth.set(depth.get() - 1));
    }
}

fn error(span: Option<&SourceSpan>) -> ParseError {
    let mut error = ParseError::syntax(MESSAGE).with_help(HELP);
    if let ParseError::Syntax(ref mut diagnostic) = error {
        diagnostic.span = span.cloned();
        if let Some(span) = span {
            diagnostic.line = span.line;
            diagnostic.column = span.column;
        }
    }
    error
}

/// Leaves have depth zero; each user expression constructor adds one level.
/// Source-location wrappers are metadata and never consume the budget.
fn check_expression(expr: &Expression, base: usize) -> Result<(), ParseError> {
    let mut pending = vec![(expr, base, None)];
    while let Some((expr, depth, span)) = pending.pop() {
        if let Expression::Located { value, span } = expr {
            pending.push((value, depth, Some(span)));
            continue;
        }
        if matches!(expr, Expression::Literal(_) | Expression::Variable(_) | Expression::Null) {
            continue;
        }
        let depth = depth + 1;
        if depth > MAX_EXPRESSION_DEPTH {
            return Err(error(span));
        }
        crate::ast::visit::walk_expression_children(expr, &mut |child| {
            pending.push((child, depth, span))
        });
    }
    Ok(())
}

pub(crate) fn parsed(expr: Expression, token: Option<&Token>) -> Result<Expression, ParseError> {
    check_expression(&expr, PARSE_DEPTH.with(Cell::get)).map_err(|failure| {
        if failure.span().is_none() {
            ParseError::syntax_at(token, MESSAGE).with_help(HELP)
        } else {
            failure
        }
    })?;
    Ok(expr)
}

enum Work<'a> {
    Node(&'a ASTNode),
    Statement(&'a StatementNode),
    Function(&'a FunctionNode),
    Expression(&'a Expression),
}

/// Iterative, including statement containers: inspect before any recursive
/// clone, source-map detachment, serialization or semantic traversal.
pub fn validate(nodes: &[ASTNode]) -> Result<(), SemanticDiagnostic> {
    for (index, node) in nodes.iter().enumerate() {
        let mut pending = vec![Work::Node(node)];
        while let Some(work) = pending.pop() {
            match work {
                Work::Expression(expr) => {
                    if let Err(failure) = check_expression(expr, 0) {
                        return Err(SemanticDiagnostic {
                            code: "E3001".into(),
                            message: MESSAGE.into(),
                            top_level_index: index,
                            primary: None,
                            span: failure.span().cloned().or_else(|| node.span().cloned()),
                            label: MESSAGE.into(),
                            note: None,
                            help: HELP.into(),
                        });
                    }
                },
                Work::Function(function) => {
                    for param in &function.parameters {
                        if let Some(value) = &param.initial_value {
                            pending.push(Work::Expression(value));
                        }
                    }
                    pending.extend(function.body.iter().map(Work::Node));
                },
                Work::Node(node) => match node {
                    ASTNode::Located { value, .. } => pending.push(Work::Node(value)),
                    ASTNode::Function(f) => pending.push(Work::Function(f)),
                    ASTNode::Struct(s) => pending.extend(s.methods.iter().map(Work::Function)),
                    ASTNode::ProtoImpl(p) => pending.extend(p.methods.iter().map(Work::Function)),
                    ASTNode::Statement(s) => pending.push(Work::Statement(s)),
                    ASTNode::Variable(v) => {
                        if let Some(e) = &v.initial_value {
                            pending.push(Work::Expression(e));
                        }
                    },
                    ASTNode::Expression(e) => pending.push(Work::Expression(e)),
                    ASTNode::ExternFunction(_) => {},
                    ASTNode::Program(p) => {
                        if let Some(e) = &p.initial_value {
                            pending.push(Work::Expression(e));
                        }
                    },
                    ASTNode::TypeAlias(_) | ASTNode::Enum(_) | ASTNode::Variant(_) => {},
                },
                Work::Statement(statement) => match statement {
                    StatementNode::PrintFormat { args, .. }
                    | StatementNode::PrintlnFormat { args, .. }
                    | StatementNode::Input { args, .. } => {
                        pending.extend(args.iter().map(Work::Expression))
                    },
                    StatementNode::If { condition, body, else_if_blocks, else_block } => {
                        pending.push(Work::Expression(condition));
                        pending.extend(body.iter().map(Work::Node));
                        if let Some(blocks) = else_if_blocks {
                            for (condition, body) in blocks.iter() {
                                pending.push(Work::Expression(condition));
                                pending.extend(body.iter().map(Work::Node));
                            }
                        }
                        if let Some(body) = else_block {
                            pending.extend(body.iter().map(Work::Node));
                        }
                    },
                    StatementNode::For { initialization, condition, increment, body } => {
                        pending.push(Work::Node(initialization));
                        pending.push(Work::Expression(condition));
                        pending.push(Work::Expression(increment));
                        pending.extend(body.iter().map(Work::Node));
                    },
                    StatementNode::While { condition, body } => {
                        pending.push(Work::Expression(condition));
                        pending.extend(body.iter().map(Work::Node));
                    },
                    StatementNode::Match { value, arms } => {
                        pending.push(Work::Expression(value));
                        for arm in arms {
                            pending.extend(arm.body.iter().map(Work::Node));
                        }
                    },
                    StatementNode::Assign { value, .. }
                    | StatementNode::Expression(value)
                    | StatementNode::Return(Some(value)) => pending.push(Work::Expression(value)),
                    StatementNode::AsmBlock { inputs, outputs, .. } => pending.extend(
                        inputs.iter().chain(outputs.iter()).map(|(_, e)| Work::Expression(e)),
                    ),
                    StatementNode::Print(_)
                    | StatementNode::Println(_)
                    | StatementNode::Variable(_)
                    | StatementNode::Import(_)
                    | StatementNode::Break
                    | StatementNode::Continue
                    | StatementNode::Return(None) => {},
                },
            }
        }
    }
    Ok(())
}
