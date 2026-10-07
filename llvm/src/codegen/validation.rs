//! Target-dependent user errors are rejected before LLVM lowering starts.
use super::{
    plan::{AsmPlan, AsmSafetyMode},
    target::CodegenTarget,
};
use crate::diagnostic::{CodegenError, CodegenPhase};
use hir::TypedProgram;
use parser::ast::{ASTNode, Expression, StatementNode};

pub(crate) fn validate(program: &TypedProgram, target: CodegenTarget) -> Result<(), CodegenError> {
    let mut state = State { program, target, error: None };
    walk_nodes(program.syntax(), &mut state);
    state.error.map_or(Ok(()), Err)
}
struct State<'a> {
    program: &'a TypedProgram,
    target: CodegenTarget,
    error: Option<CodegenError>,
}
impl State<'_> {
    fn block(
        &mut self,
        instructions: &[String],
        inputs: &[(String, Expression)],
        outputs: &[(String, Expression)],
        clobbers: &[String],
        expression: bool,
        span: Option<error::SourceSpan>,
    ) {
        let result = AsmPlan::try_build(
            self.target,
            instructions,
            inputs,
            outputs,
            clobbers,
            AsmSafetyMode::ConservativeKernel,
        )
        .and_then(|plan| {
            if expression && plan.noreturn {
                return Err("asm expression cannot declare clobber(\"noreturn\")".to_string());
            }
            if expression && plan.outputs.len() > 1 {
                return Err("asm expression supports at most one output".to_string());
            }
            Ok(())
        });
        if let Err(message) = result {
            self.error = Some(
                CodegenError::new(CodegenPhase::Validation, "inline assembly", message)
                    .invalid_assembly()
                    .with_span(span),
            );
        }
    }

    fn node(&mut self, node: &ASTNode) {
        if self.error.is_some() {
            return;
        }
        if let ASTNode::Statement(StatementNode::AsmBlock {
            instructions,
            inputs,
            outputs,
            clobbers,
        }) = node
        {
            let span =
                self.program.node_id(node).and_then(|id| self.program.node_span(id)).cloned();
            self.block(instructions, inputs, outputs, clobbers, false, span);
        }
    }

    fn expression(&mut self, expression: &Expression) {
        if self.error.is_some() {
            return;
        }
        if let Expression::AsmBlock { instructions, inputs, outputs, clobbers } = expression {
            let span = self
                .program
                .expression_id(expression)
                .and_then(|id| self.program.expression_span(id))
                .cloned();
            self.block(instructions, inputs, outputs, clobbers, true, span);
        }
    }
}

fn walk_nodes(nodes: &[ASTNode], state: &mut State<'_>) {
    for node in nodes {
        walk_node(node, state);
    }
}

fn walk_node(node: &ASTNode, state: &mut State<'_>) {
    state.node(node);
    if state.error.is_some() {
        return;
    }
    match node {
        ASTNode::Located { value, .. } => walk_node(value, state),
        ASTNode::Function(function) => {
            for parameter in &function.parameters {
                if let Some(default) = &parameter.initial_value {
                    walk_expression(default, state);
                }
            }
            walk_nodes(&function.body, state);
        },
        ASTNode::Struct(structure) => {
            if !structure.generic_params.is_empty() {
                return;
            }
            for method in &structure.methods {
                for parameter in &method.parameters {
                    if let Some(default) = &parameter.initial_value {
                        walk_expression(default, state);
                    }
                }
                walk_nodes(&method.body, state);
            }
        },
        ASTNode::ProtoImpl(implementation) => {
            for method in &implementation.methods {
                for parameter in &method.parameters {
                    if let Some(default) = &parameter.initial_value {
                        walk_expression(default, state);
                    }
                }
                walk_nodes(&method.body, state);
            }
        },
        ASTNode::Statement(statement) => walk_statement(statement, state),
        ASTNode::Variable(variable) => {
            if let Some(initializer) = &variable.initial_value {
                walk_expression(initializer, state);
            }
        },
        ASTNode::Expression(expression) => walk_expression(expression, state),
        ASTNode::ExternFunction(_)
        | ASTNode::Program(_)
        | ASTNode::TypeAlias(_)
        | ASTNode::Enum(_)
        | ASTNode::Variant(_) => {},
    }
}

fn walk_statement(statement: &StatementNode, state: &mut State<'_>) {
    match statement {
        StatementNode::PrintFormat { args, .. }
        | StatementNode::PrintlnFormat { args, .. }
        | StatementNode::Input { args, .. } => {
            for argument in args {
                walk_expression(argument, state);
            }
        },
        StatementNode::If { condition, body, else_if_blocks, else_block } => {
            walk_expression(condition, state);
            walk_nodes(body, state);
            if let Some(blocks) = else_if_blocks {
                for (condition, body) in blocks.iter() {
                    walk_expression(condition, state);
                    walk_nodes(body, state);
                }
            }
            if let Some(body) = else_block {
                walk_nodes(body, state);
            }
        },
        StatementNode::For { initialization, condition, increment, body } => {
            walk_node(initialization, state);
            walk_expression(condition, state);
            walk_expression(increment, state);
            walk_nodes(body, state);
        },
        StatementNode::While { condition, body } => {
            walk_expression(condition, state);
            walk_nodes(body, state);
        },
        StatementNode::Match { value, arms } => {
            walk_expression(value, state);
            for arm in arms {
                walk_nodes(&arm.body, state);
            }
        },
        StatementNode::Assign { value, .. } => walk_expression(value, state),
        StatementNode::AsmBlock { inputs, outputs, .. } => {
            for (_, expression) in inputs.iter().chain(outputs.iter()) {
                walk_expression(expression, state);
            }
        },
        StatementNode::Return(Some(expression)) | StatementNode::Expression(expression) => {
            walk_expression(expression, state)
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

fn walk_expression(expression: &Expression, state: &mut State<'_>) {
    if let Expression::Located { value, .. } = expression {
        walk_expression(value, state);
        return;
    }
    state.expression(expression);
    if state.error.is_some() {
        return;
    }
    match expression {
        Expression::Located { value, .. } => walk_expression(value, state),
        Expression::StructLiteral { fields, .. } => {
            for (_, value) in fields {
                walk_expression(value, state);
            }
        },
        Expression::FunctionCall { args, .. } => {
            for argument in args {
                walk_expression(argument, state);
            }
        },
        Expression::MethodCall { object, args, .. } => {
            walk_expression(object, state);
            for argument in args {
                walk_expression(argument, state);
            }
        },
        Expression::Deref(inner)
        | Expression::AddressOf(inner)
        | Expression::Await(inner)
        | Expression::Grouped(inner)
        | Expression::Unary { expr: inner, .. }
        | Expression::Cast { expr: inner, .. }
        | Expression::FieldAccess { object: inner, .. }
        | Expression::IncDec { target: inner, .. } => walk_expression(inner, state),
        Expression::BinaryExpression { left, right, .. }
        | Expression::IndexAccess { target: left, index: right }
        | Expression::AssignOperation { target: left, value: right, .. }
        | Expression::Assignment { target: left, value: right } => {
            walk_expression(left, state);
            walk_expression(right, state);
        },
        Expression::ArrayLiteral(values) => {
            for value in values {
                walk_expression(value, state);
            }
        },
        Expression::AsmBlock { inputs, outputs, .. } => {
            for (_, expression) in inputs.iter().chain(outputs.iter()) {
                walk_expression(expression, state);
            }
        },
        Expression::Null | Expression::Literal(_) | Expression::Variable(_) => {},
    }
}
