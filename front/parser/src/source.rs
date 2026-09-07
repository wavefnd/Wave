//! Source provenance retained independently of the backend and node allocation.
use crate::ast::*;
use error::SourceSpan;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct SourceMap {
    pub nodes: HashMap<usize, SourceSpan>,
    pub node_order: Vec<usize>,
    pub expressions: HashMap<usize, SourceSpan>,
    pub patterns: HashMap<usize, SourceSpan>,
}

impl SourceMap {
    /// Remove syntax wrappers only after the owning allocation is stable.
    /// Child boxes/vectors keep their allocations; maps refer to the resulting nodes.
    pub fn detach(nodes: &mut [ASTNode]) -> Self {
        let mut map = Self::default();
        for node in nodes {
            map.node(node);
        }
        map
    }

    fn function(&mut self, function: &mut FunctionNode) {
        for parameter in &mut function.parameters {
            if let Some(default) = &mut parameter.initial_value {
                self.expression(default);
            }
        }
        for node in &mut function.body {
            self.node(node);
        }
    }

    fn node(&mut self, node: &mut ASTNode) {
        let span = node.span().cloned();
        while matches!(node, ASTNode::Located { .. }) {
            let ASTNode::Located { value, .. } =
                std::mem::replace(node, ASTNode::Expression(Expression::Null))
            else {
                unreachable!()
            };
            *node = *value;
        }
        self.node_order.push(node as *const _ as usize);
        if let Some(span) = span {
            self.nodes.insert(node as *const _ as usize, span);
        }
        match node {
            ASTNode::Function(f) => self.function(f),
            ASTNode::Struct(s) => {
                for f in &mut s.methods {
                    self.function(f);
                }
            }
            ASTNode::ProtoImpl(p) => {
                for f in &mut p.methods {
                    self.function(f);
                }
            }
            ASTNode::Program(p) => {
                if let Some(e) = &mut p.initial_value {
                    self.expression(e);
                }
            }
            ASTNode::Variable(v) => {
                if let Some(e) = &mut v.initial_value {
                    self.expression(e);
                }
            }
            ASTNode::Expression(e) => self.expression(e),
            ASTNode::Statement(s) => self.statement(s),
            _ => {}
        }
    }

    fn statement(&mut self, statement: &mut StatementNode) {
        match statement {
            StatementNode::PrintFormat { args, .. }
            | StatementNode::PrintlnFormat { args, .. }
            | StatementNode::Input { args, .. } => {
                for e in args {
                    self.expression(e);
                }
            }
            StatementNode::If {
                condition,
                body,
                else_if_blocks,
                else_block,
            } => {
                self.expression(condition);
                for node in body {
                    self.node(node);
                }
                if let Some(blocks) = else_if_blocks {
                    for (condition, body) in blocks.iter_mut() {
                        self.expression(condition);
                        for node in body {
                            self.node(node);
                        }
                    }
                }
                if let Some(body) = else_block {
                    for node in body.iter_mut() {
                        self.node(node);
                    }
                }
            }
            StatementNode::For {
                initialization,
                condition,
                increment,
                body,
            } => {
                self.node(initialization);
                self.expression(condition);
                self.expression(increment);
                for node in body {
                    self.node(node);
                }
            }
            StatementNode::While { condition, body } => {
                self.expression(condition);
                for node in body {
                    self.node(node);
                }
            }
            StatementNode::Match { value, arms } => {
                self.expression(value);
                for arm in arms {
                    self.pattern(&mut arm.pattern);
                    for node in &mut arm.body {
                        self.node(node);
                    }
                }
            }
            StatementNode::Assign { value, .. }
            | StatementNode::Return(Some(value))
            | StatementNode::Expression(value) => self.expression(value),
            StatementNode::AsmBlock {
                inputs, outputs, ..
            } => {
                for (_, e) in inputs.iter_mut().chain(outputs.iter_mut()) {
                    self.expression(e);
                }
            }
            _ => {}
        }
    }

    fn expression(&mut self, expression: &mut Expression) {
        let span = expression.span().cloned();
        while matches!(expression, Expression::Located { .. }) {
            let Expression::Located { value, .. } = std::mem::replace(expression, Expression::Null)
            else {
                unreachable!()
            };
            *expression = *value;
        }
        if let Some(span) = span {
            self.expressions
                .insert(expression as *const _ as usize, span);
        }
        match expression {
            Expression::StructLiteral { fields, .. } => {
                for (_, e) in fields {
                    self.expression(e);
                }
            }
            Expression::FunctionCall { args, .. } | Expression::ArrayLiteral(args) => {
                for e in args {
                    self.expression(e);
                }
            }
            Expression::MethodCall { object, args, .. } => {
                self.expression(object);
                for e in args {
                    self.expression(e);
                }
            }
            Expression::Deref(e)
            | Expression::AddressOf(e)
            | Expression::Grouped(e)
            | Expression::Unary { expr: e, .. }
            | Expression::Cast { expr: e, .. }
            | Expression::FieldAccess { object: e, .. }
            | Expression::IncDec { target: e, .. } => self.expression(e),
            Expression::BinaryExpression { left, right, .. }
            | Expression::IndexAccess {
                target: left,
                index: right,
            }
            | Expression::AssignOperation {
                target: left,
                value: right,
                ..
            }
            | Expression::Assignment {
                target: left,
                value: right,
            } => {
                self.expression(left);
                self.expression(right);
            }
            Expression::AsmBlock {
                inputs, outputs, ..
            } => {
                for (_, e) in inputs.iter_mut().chain(outputs.iter_mut()) {
                    self.expression(e);
                }
            }
            _ => {}
        }
    }

    fn pattern(&mut self, pattern: &mut MatchPattern) {
        let span = pattern.span().cloned();
        while matches!(pattern, MatchPattern::Located { .. }) {
            let MatchPattern::Located { value, .. } =
                std::mem::replace(pattern, MatchPattern::Wildcard)
            else {
                unreachable!()
            };
            *pattern = *value;
        }
        if let Some(span) = span {
            self.patterns.insert(pattern as *const _ as usize, span);
        }
        if let MatchPattern::Variant { payloads, .. } = pattern {
            for p in payloads {
                self.pattern(p);
            }
        }
    }
}

/// Choose a declaration's name from its consumed token stream, never source text.
pub fn node_span<'a, T>(
    before: std::iter::Peekable<T>,
    after: &mut std::iter::Peekable<T>,
    node: &ASTNode,
) -> Option<SourceSpan>
where
    T: Iterator<Item = &'a lexer::Token> + Clone,
{
    let mut span = lexer::consumed_span(before.clone(), after)?;
    let name = match node.unspanned() {
        ASTNode::Function(f) => Some(&f.name),
        ASTNode::ExternFunction(f) => Some(&f.name),
        ASTNode::Variable(v) => Some(&v.name),
        ASTNode::Struct(s) => Some(&s.name),
        ASTNode::Variant(v) => Some(&v.name),
        ASTNode::Enum(e) => Some(&e.name),
        ASTNode::TypeAlias(t) => Some(&t.name),
        ASTNode::ProtoImpl(p) => Some(&p.target),
        _ => None,
    };
    if let Some(name) = name {
        span.focus = before.take_while(|token| token.span.as_ref().is_some_and(|s| s.start < span.end))
            .find(|token| matches!(&token.token_type, lexer::token::TokenType::Identifier(value) if value == name))
            .and_then(|token| token.span.clone()).map(Box::new);
    }
    Some(span)
}
