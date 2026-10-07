//! Backend-neutral async frames and explicit suspension control flow.
//!
//! Source expressions are evaluated in order into frame slots. A pending await
//! returns from the resume function; re-entry selects only its saved state.
use crate::{HirExpressionType, TypedProgram};
use parser::ast::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct AsyncLoweringError {
    pub message: String,
    pub span: Option<error::SourceSpan>,
}
#[derive(Debug, Clone)]
pub struct FrameSlot {
    pub name: String,
    pub ty: WaveType,
}
#[derive(Debug, Clone)]
pub enum Transition {
    Jump(usize),
    Branch(Expression, usize, usize),
    Match(Expression, Vec<(MatchPattern, Vec<ASTNode>, usize)>),
    Await { future: Expression, output: String, result: WaveType, next: usize },
    Complete(Option<Expression>),
}
#[derive(Debug, Clone)]
pub struct AsyncState {
    pub operations: Vec<ASTNode>,
    pub transition: Transition,
}
#[derive(Debug, Clone)]
pub struct AsyncFrame {
    pub function: String,
    pub result: WaveType,
    pub slots: Vec<FrameSlot>,
    pub parameters: Vec<String>,
    pub states: Vec<AsyncState>,
}

pub fn plan(
    program: &TypedProgram,
    function: &FunctionNode,
) -> Result<AsyncFrame, AsyncLoweringError> {
    let mut lower = Lower {
        program,
        frame: AsyncFrame {
            function: function.name.clone(),
            result: function.return_type.clone().unwrap_or(WaveType::Void),
            slots: vec![],
            parameters: vec![],
            states: vec![empty_state()],
        },
        current: 0,
        bindings: HashMap::new(),
        loops: vec![],
    };
    for p in &function.parameters {
        let name = lower.new_slot(p.param_type.clone());
        lower.bindings.insert(p.name.clone(), name.clone());
        lower.frame.parameters.push(name);
    }
    lower.block(&function.body)?;
    Ok(lower.frame)
}

fn empty_state() -> AsyncState {
    AsyncState { operations: vec![], transition: Transition::Complete(None) }
}
struct Lower<'a> {
    program: &'a TypedProgram,
    frame: AsyncFrame,
    current: usize,
    bindings: HashMap<String, String>,
    loops: Vec<(usize, usize)>,
}
fn var(name: impl Into<String>) -> Expression {
    Expression::Variable(name.into())
}

fn field(name: impl Into<String>) -> Expression {
    Expression::FieldAccess { object: Box::new(var("$async$frame")), field: name.into() }
}

fn store(target: Expression, value: Expression) -> ASTNode {
    ASTNode::Statement(StatementNode::Expression(Expression::Assignment {
        target: Box::new(target),
        value: Box::new(value),
    }))
}
impl Lower<'_> {
    fn error(&self, e: &Expression, message: impl Into<String>) -> AsyncLoweringError {
        AsyncLoweringError {
            message: message.into(),
            span: self
                .program
                .expression_id(e)
                .and_then(|id| self.program.expression_span(id))
                .cloned(),
        }
    }

    fn ty(
        &self,
        e: &Expression,
        expected: Option<&WaveType>,
    ) -> Result<WaveType, AsyncLoweringError> {
        match self.program.type_of(e) {
            Some(HirExpressionType::Resolved(t)) => Ok(t.clone()),
            Some(HirExpressionType::IntegerLiteral) => {
                Ok(expected.cloned().unwrap_or(WaveType::Int(32)))
            },
            Some(HirExpressionType::FloatLiteral) => {
                Ok(expected.cloned().unwrap_or(WaveType::Float(32)))
            },
            _ => expected
                .cloned()
                .ok_or_else(|| self.error(e, "async temporary needs a concrete type")),
        }
    }

    fn new_slot(&mut self, ty: WaveType) -> String {
        let name = format!("v{}", self.frame.slots.len());
        self.frame.slots.push(FrameSlot { name: name.clone(), ty });
        name
    }

    fn state(&mut self) -> usize {
        let id = self.frame.states.len();
        self.frame.states.push(empty_state());
        id
    }

    fn emit(&mut self, n: ASTNode) {
        self.frame.states[self.current].operations.push(n);
    }

    fn end(&mut self, t: Transition) {
        self.frame.states[self.current].transition = t;
    }

    fn snapshot(&mut self, e: Expression, ty: WaveType) -> Expression {
        if ty == WaveType::Void {
            self.emit(ASTNode::Statement(StatementNode::Expression(e)));
            return Expression::Null;
        }
        let name = self.new_slot(ty);
        self.emit(store(field(&name), e));
        field(name)
    }

    fn place(&mut self, e: &Expression) -> Result<Expression, AsyncLoweringError> {
        Ok(match e.unspanned() {
            Expression::Variable(n) => self.bindings.get(n).map_or_else(|| var(n), field),
            Expression::Grouped(inner) => self.place(inner)?,
            Expression::Deref(inner) => {
                if matches!(
                    inner.unspanned(),
                    Expression::FieldAccess { .. } | Expression::IndexAccess { .. }
                ) {
                    self.place(inner)?
                } else {
                    let t = self.ty(inner, None)?;
                    Expression::Deref(Box::new(Expression::Cast {
                        expr: Box::new(self.expr(inner, None)?),
                        target_type: t,
                    }))
                }
            },
            Expression::FieldAccess { object, field: name } => {
                let obj = if matches!(self.ty(object, None)?, WaveType::Pointer(_)) {
                    self.expr(object, None)?
                } else {
                    self.place(object)?
                };
                Expression::FieldAccess { object: Box::new(obj), field: name.clone() }
            },
            Expression::IndexAccess { target, index } => {
                let obj = if matches!(self.ty(target, None)?, WaveType::Array(..)) {
                    self.place(target)?
                } else {
                    self.expr(target, None)?
                };
                let index = self.expr(index, None)?;
                Expression::IndexAccess { target: Box::new(obj), index: Box::new(index) }
            },
            _ => return Err(self.error(e, "unsupported address across async suspension")),
        })
    }

    fn expr(
        &mut self,
        e: &Expression,
        expected: Option<&WaveType>,
    ) -> Result<Expression, AsyncLoweringError> {
        let ty = self.ty(e, expected)?;
        let value = match e.unspanned() {
            Expression::Await(inner) => {
                let future = self.expr(inner, None)?;
                let output =
                    self.new_slot(if ty == WaveType::Void { WaveType::Byte } else { ty.clone() });
                let wait = self.state();
                let next = self.state();
                self.end(Transition::Jump(wait));
                self.current = wait;
                self.end(Transition::Await { future, output: output.clone(), result: ty, next });
                self.current = next;
                return Ok(field(output));
            },
            Expression::Variable(n) => self.bindings.get(n).map_or_else(|| var(n), field),
            Expression::Literal(_) | Expression::Null => e.clone(),
            Expression::Grouped(inner) => return self.expr(inner, expected),
            Expression::AddressOf(inner) => Expression::AddressOf(Box::new(self.place(inner)?)),
            Expression::Deref(inner) => {
                if matches!(
                    inner.unspanned(),
                    Expression::FieldAccess { .. } | Expression::IndexAccess { .. }
                ) {
                    self.place(inner)?
                } else {
                    let t = self.ty(inner, None)?;
                    Expression::Deref(Box::new(Expression::Cast {
                        expr: Box::new(self.expr(inner, None)?),
                        target_type: t,
                    }))
                }
            },
            Expression::Unary { operator, expr } => Expression::Unary {
                operator: operator.clone(),
                expr: Box::new(self.expr(expr, Some(&ty))?),
            },
            Expression::Cast { expr, target_type } => {
                // A contextual literal must not be truncated into a default i32
                // temporary before its explicit widening cast is performed.
                let source_context = match self.program.type_of(expr) {
                    Some(HirExpressionType::IntegerLiteral) => match target_type {
                        WaveType::Int(_) | WaveType::Uint(_) | WaveType::Byte | WaveType::Char => {
                            Some(target_type.clone())
                        },
                        WaveType::Pointer(_) | WaveType::Float(_) => Some(WaveType::Int(64)),
                        _ => None,
                    },
                    Some(HirExpressionType::FloatLiteral) => match target_type {
                        WaveType::Float(_) => Some(target_type.clone()),
                        _ => Some(WaveType::Float(64)),
                    },
                    Some(HirExpressionType::Null)
                        if matches!(target_type, WaveType::Pointer(_)) =>
                    {
                        Some(target_type.clone())
                    },
                    _ => None,
                };
                Expression::Cast {
                    expr: Box::new(self.expr(expr, source_context.as_ref())?),
                    target_type: target_type.clone(),
                }
            },
            Expression::BinaryExpression { left, operator, right }
                if matches!(operator, Operator::LogicalAnd | Operator::LogicalOr) =>
            {
                let value = self.expr(left, Some(&WaveType::Bool))?;
                let output = self.new_slot(WaveType::Bool);
                self.emit(store(field(&output), value.clone()));
                let rhs = self.state();
                let done = self.state();
                let (yes, no) = if matches!(operator, Operator::LogicalAnd) {
                    (rhs, done)
                } else {
                    (done, rhs)
                };
                self.end(Transition::Branch(value, yes, no));
                self.current = rhs;
                let value = self.expr(right, Some(&WaveType::Bool))?;
                self.emit(store(field(&output), value));
                self.end(Transition::Jump(done));
                self.current = done;
                return Ok(field(output));
            },
            Expression::BinaryExpression { left, operator, right } => {
                let lt = self.ty(
                    left,
                    if ty == WaveType::Bool {
                        match self.program.type_of(right) {
                            Some(HirExpressionType::Resolved(right_ty)) => Some(right_ty),
                            _ => None,
                        }
                    } else {
                        Some(&ty)
                    },
                )?;
                let rt = self.ty(right, Some(&lt))?;
                let left = self.expr(left, Some(&rt))?;
                let right = self.expr(right, Some(&lt))?;
                Expression::BinaryExpression {
                    left: Box::new(left),
                    operator: operator.clone(),
                    right: Box::new(right),
                }
            },
            Expression::FunctionCall { name, type_args, args } => {
                let parameters = self
                    .program
                    .syntax()
                    .iter()
                    .find_map(|n| match n {
                        ASTNode::Function(f) if &f.name == name => Some(
                            f.parameters.iter().map(|p| p.param_type.clone()).collect::<Vec<_>>(),
                        ),
                        ASTNode::ExternFunction(f) if &f.name == name => {
                            Some(f.params.iter().map(|p| p.1.clone()).collect())
                        },
                        ASTNode::Struct(s) => s
                            .methods
                            .iter()
                            .find(|f| format!("{}_{}", s.name, f.name) == *name)
                            .map(|f| f.parameters.iter().map(|p| p.param_type.clone()).collect()),
                        ASTNode::ProtoImpl(p) => p
                            .methods
                            .iter()
                            .find(|f| format!("{}_{}", p.target, f.name) == *name)
                            .map(|f| f.parameters.iter().map(|p| p.param_type.clone()).collect()),
                        _ => None,
                    })
                    .unwrap_or_default();
                let args = args
                    .iter()
                    .enumerate()
                    .map(|(i, a)| self.expr(a, parameters.get(i)))
                    .collect::<Result<_, _>>()?;
                Expression::FunctionCall { name: name.clone(), type_args: type_args.clone(), args }
            },
            Expression::MethodCall { object, name, type_args, args } => {
                let object = self.expr(object, None)?;
                let args = args.iter().map(|a| self.expr(a, None)).collect::<Result<_, _>>()?;
                Expression::MethodCall {
                    object: Box::new(object),
                    name: name.clone(),
                    type_args: type_args.clone(),
                    args,
                }
            },
            Expression::StructLiteral { name, fields } => {
                let types = self
                    .program
                    .syntax()
                    .iter()
                    .find_map(|n| match n {
                        ASTNode::Struct(s) if &s.name == name => Some(s.fields.clone()),
                        _ => None,
                    })
                    .unwrap_or_default();
                Expression::StructLiteral {
                    name: name.clone(),
                    fields: fields
                        .iter()
                        .map(|(n, e)| {
                            Ok((
                                n.clone(),
                                self.expr(e, types.iter().find(|(f, _)| f == n).map(|(_, t)| t))?,
                            ))
                        })
                        .collect::<Result<_, AsyncLoweringError>>()?,
                }
            },
            Expression::ArrayLiteral(values) => {
                let inner = if let WaveType::Array(t, _) = &ty { Some(t.as_ref()) } else { None };
                Expression::ArrayLiteral(
                    values.iter().map(|v| self.expr(v, inner)).collect::<Result<_, _>>()?,
                )
            },
            Expression::IndexAccess { .. } => self.place(e)?,
            Expression::FieldAccess { object, field: name } => Expression::FieldAccess {
                object: Box::new(self.expr(object, None)?),
                field: name.clone(),
            },
            Expression::Assignment { target, value }
            | Expression::AssignOperation { target, value, .. } => {
                let target_ty = self.ty(target, None)?;
                let place = self.place(target)?;
                let pointer = self.snapshot(
                    Expression::AddressOf(Box::new(place)),
                    WaveType::Pointer(Box::new(target_ty.clone())),
                );
                let target = Box::new(Expression::Deref(Box::new(Expression::Cast {
                    expr: Box::new(pointer),
                    target_type: WaveType::Pointer(Box::new(target_ty.clone())),
                })));
                let value = Box::new(self.expr(value, Some(&target_ty))?);
                if let Expression::AssignOperation { operator, .. } = e.unspanned() {
                    Expression::AssignOperation { target, operator: operator.clone(), value }
                } else {
                    Expression::Assignment { target, value }
                }
            },
            Expression::IncDec { kind, target } => {
                Expression::IncDec { kind: kind.clone(), target: Box::new(self.place(target)?) }
            },
            Expression::AsmBlock { instructions, inputs, outputs, clobbers } => {
                Expression::AsmBlock {
                    instructions: instructions.clone(),
                    inputs: inputs
                        .iter()
                        .map(|(n, e)| Ok((n.clone(), self.expr(e, None)?)))
                        .collect::<Result<_, AsyncLoweringError>>()?,
                    outputs: outputs
                        .iter()
                        .map(|(n, e)| Ok((n.clone(), self.place(e)?)))
                        .collect::<Result<_, AsyncLoweringError>>()?,
                    clobbers: clobbers.clone(),
                }
            },
            Expression::Located { .. } => unreachable!(),
        };
        Ok(self.snapshot(value, ty))
    }

    fn block(&mut self, nodes: &[ASTNode]) -> Result<(), AsyncLoweringError> {
        let bindings = self.bindings.clone();
        for n in nodes {
            self.node(n)?;
        }
        self.bindings = bindings;
        Ok(())
    }

    fn node(&mut self, n: &ASTNode) -> Result<(), AsyncLoweringError> {
        match n.unspanned() {
            ASTNode::Variable(v) => {
                let init = v
                    .initial_value
                    .as_ref()
                    .map(|e| self.expr(e, Some(&v.type_name)))
                    .transpose()?;
                let slot = self.new_slot(v.type_name.clone());
                self.bindings.insert(v.name.clone(), slot.clone());
                if let Some(init) = init {
                    self.emit(store(field(slot), init));
                }
            },
            ASTNode::Statement(s) => match s {
                StatementNode::Return(e) => {
                    let ty = self.frame.result.clone();
                    let value = e.as_ref().map(|e| self.expr(e, Some(&ty))).transpose()?;
                    self.end(Transition::Complete(value));
                    self.current = self.state();
                },
                StatementNode::Expression(e) => {
                    self.expr(e, None)?;
                },
                StatementNode::Assign { variable, value } => {
                    let target = self.bindings.get(variable).map_or_else(|| var(variable), field);
                    let value = self.expr(value, None)?;
                    self.emit(store(target, value));
                },
                StatementNode::If { condition, body, else_if_blocks, else_block } => {
                    let done = self.state();
                    let mut branches = vec![(condition, body.as_slice())];
                    if let Some(others) = else_if_blocks {
                        branches.extend(others.iter().map(|(c, b)| (c, b.as_slice())));
                    }
                    for (condition, body) in branches {
                        let condition = self.expr(condition, Some(&WaveType::Bool))?;
                        let yes = self.state();
                        let no = self.state();
                        self.end(Transition::Branch(condition, yes, no));
                        self.current = yes;
                        self.block(body)?;
                        self.end(Transition::Jump(done));
                        self.current = no;
                    }
                    if let Some(body) = else_block {
                        self.block(body)?;
                    }
                    self.end(Transition::Jump(done));
                    self.current = done;
                },
                StatementNode::While { condition, body } => {
                    self.loop_body(None, condition, None, body)?
                },
                StatementNode::For { initialization, condition, increment, body } => {
                    self.loop_body(Some(initialization), condition, Some(increment), body)?
                },
                StatementNode::Break | StatementNode::Continue => {
                    let (exit, next) = *self.loops.last().expect("validated loop");
                    self.end(Transition::Jump(if matches!(s, StatementNode::Break) {
                        exit
                    } else {
                        next
                    }));
                    self.current = self.state();
                },
                StatementNode::Match { value, arms } => {
                    let value_ty = self.ty(value, None)?;
                    let value = self.expr(value, None)?;
                    let done = self.state();
                    let start = self.current;
                    let mut branches = vec![];
                    for arm in arms {
                        let state = self.state();
                        self.current = state;
                        let old = self.bindings.clone();
                        self.bind_pattern(&arm.pattern, &value_ty)?;
                        let binding_stores =
                            std::mem::take(&mut self.frame.states[state].operations);
                        branches.push((arm.pattern.clone(), binding_stores, state));
                        for n in &arm.body {
                            self.node(n)?;
                        }
                        self.bindings = old;
                        self.end(Transition::Jump(done));
                    }
                    self.current = start;
                    self.end(Transition::Match(value, branches));
                    self.current = done;
                },
                StatementNode::Input { format, args } => {
                    let args = args.iter().map(|e| self.place(e)).collect::<Result<_, _>>()?;
                    self.emit(ASTNode::Statement(StatementNode::Input {
                        format: format.clone(),
                        args,
                    }));
                },
                StatementNode::PrintFormat { format, args }
                | StatementNode::PrintlnFormat { format, args } => {
                    let args = args.iter().map(|e| self.expr(e, None)).collect::<Result<_, _>>()?;
                    let s = match s {
                        StatementNode::PrintFormat { .. } => {
                            StatementNode::PrintFormat { format: format.clone(), args }
                        },
                        _ => StatementNode::PrintlnFormat { format: format.clone(), args },
                    };
                    self.emit(ASTNode::Statement(s));
                },
                StatementNode::AsmBlock { instructions, inputs, outputs, clobbers } => {
                    let inputs = inputs
                        .iter()
                        .map(|(n, e)| Ok((n.clone(), self.expr(e, None)?)))
                        .collect::<Result<_, AsyncLoweringError>>()?;
                    let outputs = outputs
                        .iter()
                        .map(|(n, e)| Ok((n.clone(), self.place(e)?)))
                        .collect::<Result<_, AsyncLoweringError>>()?;
                    self.emit(ASTNode::Statement(StatementNode::AsmBlock {
                        instructions: instructions.clone(),
                        inputs,
                        outputs,
                        clobbers: clobbers.clone(),
                    }));
                },
                _ => self.emit(n.clone()),
            },
            ASTNode::Expression(e) => {
                self.expr(e, None)?;
            },
            _ => {
                return Err(AsyncLoweringError {
                    message: "unsupported declaration inside async function".into(),
                    span: n.span().cloned(),
                })
            },
        }
        Ok(())
    }

    fn bind_pattern(&mut self, p: &MatchPattern, ty: &WaveType) -> Result<(), AsyncLoweringError> {
        match p.unspanned() {
            MatchPattern::Binding(name) => {
                let slot = self.new_slot(ty.clone());
                self.bindings.insert(name.clone(), slot.clone());
                self.emit(store(field(slot), var(name)));
            },
            MatchPattern::Variant { payloads, .. } => {
                let info = self.program.variant_pattern_of(p).expect("typed variant pattern");
                let types = info.payload_types.clone();
                for (p, t) in payloads.iter().zip(types) {
                    self.bind_pattern(p, &t)?;
                }
            },
            _ => {},
        }
        Ok(())
    }

    fn loop_body(
        &mut self,
        init: Option<&ASTNode>,
        condition: &Expression,
        increment: Option<&Expression>,
        body: &[ASTNode],
    ) -> Result<(), AsyncLoweringError> {
        let old = self.bindings.clone();
        if let Some(init) = init {
            self.node(init)?;
        }
        let check = self.state();
        let run = self.state();
        let step = self.state();
        let done = self.state();
        self.end(Transition::Jump(check));
        self.current = check;
        let condition = self.expr(condition, Some(&WaveType::Bool))?;
        self.end(Transition::Branch(condition, run, done));
        self.current = run;
        self.loops.push((done, step));
        self.block(body)?;
        self.loops.pop();
        self.end(Transition::Jump(step));
        self.current = step;
        if let Some(e) = increment {
            self.expr(e, None)?;
        }
        self.end(Transition::Jump(check));
        self.current = done;
        self.bindings = old;
        Ok(())
    }
}

fn integer(n: usize) -> Expression {
    Expression::Literal(Literal::Int(n.to_string()))
}

fn call(name: &str, type_args: Vec<WaveType>, args: Vec<Expression>) -> Expression {
    Expression::FunctionCall { name: name.into(), type_args, args }
}

fn statement(e: Expression) -> ASTNode {
    ASTNode::Statement(StatementNode::Expression(e))
}

fn ret(e: Expression) -> ASTNode {
    ASTNode::Statement(StatementNode::Return(Some(e)))
}

fn parameter(name: &str, ty: WaveType) -> ParameterNode {
    ParameterNode { span: None, name: name.into(), param_type: ty, initial_value: None }
}

fn function(
    name: String,
    parameters: Vec<ParameterNode>,
    result: WaveType,
    body: Vec<ASTNode>,
) -> FunctionNode {
    FunctionNode {
        is_async: false,
        span: None,
        name,
        generic_params: vec![],
        parameters,
        return_type: Some(result),
        return_type_span: None,
        body,
        export: None,
        visibility: Visibility::Private,
    }
}

fn jump(next: usize) -> Vec<ASTNode> {
    vec![store(field("state"), integer(next)), ASTNode::Statement(StatementNode::Continue)]
}

fn emit_frame(
    original: &FunctionNode,
    frame: AsyncFrame,
    id: usize,
) -> (FunctionNode, Vec<ASTNode>) {
    let frame_name = format!("$async$frame${id}");
    let poll_name = format!("$async$poll${id}");
    let frame_ty = WaveType::Struct(frame_name.clone());
    let mut fields = frame.slots.iter().map(|s| (s.name.clone(), s.ty.clone())).collect::<Vec<_>>();
    fields.push(("state".into(), WaveType::Int(32)));
    fields.push((
        "result".into(),
        if frame.result == WaveType::Void { WaveType::Byte } else { frame.result.clone() },
    ));
    let structure = ASTNode::Struct(StructNode {
        name: frame_name,
        generic_params: vec![],
        field_spans: vec![None; fields.len()],
        fields,
        methods: vec![],
        visibility: Visibility::Private,
    });
    let mut arms = vec![];
    for (index, state) in frame.states.into_iter().enumerate() {
        let mut body = state.operations;
        match state.transition {
            Transition::Jump(next) => body.extend(jump(next)),
            Transition::Branch(condition, yes, no) => {
                body.push(ASTNode::Statement(StatementNode::If {
                    condition,
                    body: jump(yes),
                    else_if_blocks: None,
                    else_block: Some(Box::new(jump(no))),
                }))
            },
            Transition::Match(value, branches) => {
                body.push(ASTNode::Statement(StatementNode::Match {
                    value,
                    arms: branches
                        .into_iter()
                        .map(|(pattern, mut body, next)| {
                            body.extend(jump(next));
                            MatchArm { span: None, pattern, body }
                        })
                        .collect(),
                }))
            },
            Transition::Await { future, output, result, next } => {
                let take = call("__wave_async_take", vec![], vec![future.clone()]);
                let mut ready = vec![if result == WaveType::Void {
                    statement(take)
                } else {
                    store(field(output), take)
                }];
                ready.extend(jump(next));
                body.push(ASTNode::Statement(StatementNode::If {
                    condition: call("__wave_async_ready", vec![], vec![future.clone()]),
                    body: ready,
                    else_if_blocks: None,
                    else_block: None,
                }));
                body.push(statement(call(
                    "__wave_async_wait",
                    vec![],
                    vec![var("$async$id"), future],
                )));
                body.push(ret(Expression::Literal(Literal::Bool(false))));
            },
            Transition::Complete(value) => {
                if frame.result != WaveType::Void {
                    if let Some(value) = value {
                        body.push(store(field("result"), value));
                    }
                }
                body.push(statement(call("__wave_async_complete", vec![], vec![var("$async$id")])));
                body.push(ret(Expression::Literal(Literal::Bool(true))));
            },
        }
        arms.push(MatchArm { span: None, pattern: MatchPattern::Int(index.to_string()), body });
    }
    arms.push(MatchArm {
        span: None,
        pattern: MatchPattern::Wildcard,
        body: vec![ret(Expression::Literal(Literal::Bool(false)))],
    });
    let resume = function(
        poll_name.clone(),
        vec![
            parameter("$async$frame", WaveType::Pointer(Box::new(frame_ty.clone()))),
            parameter("$async$id", WaveType::Int(64)),
        ],
        WaveType::Bool,
        vec![ASTNode::Statement(StatementNode::While {
            condition: Expression::Literal(Literal::Bool(true)),
            body: vec![ASTNode::Statement(StatementNode::Match { value: field("state"), arms })],
        })],
    );
    let mut constructor = original.clone();
    constructor.is_async = false;
    constructor.return_type = Some(WaveType::Future(Box::new(frame.result.clone())));
    constructor.body = vec![ASTNode::Variable(VariableNode {
        name: "$async$frame".into(),
        type_name: WaveType::Pointer(Box::new(frame_ty.clone())),
        initial_value: Some(call("__wave_async_alloc", vec![frame_ty.clone()], vec![])),
        mutability: Mutability::Var,
        visibility: Visibility::Private,
    })];
    for (p, slot) in original.parameters.iter().zip(frame.parameters) {
        constructor.body.push(store(field(slot), var(&p.name)));
    }
    constructor.body.push(ret(call(
        "__wave_async_create",
        vec![frame_ty, frame.result],
        vec![
            var("$async$frame"),
            Expression::AddressOf(Box::new(field("result"))),
            Expression::Literal(Literal::String(poll_name.into_bytes())),
        ],
    )));
    (constructor, vec![structure, ASTNode::Function(resume)])
}
/// Emit ordinary typed operations from the resumable plan. No LLVM details occur here.
pub fn lower_program(program: &TypedProgram) -> Result<Vec<ASTNode>, AsyncLoweringError> {
    let mut output = program.syntax().to_vec();
    let mut generated = vec![];
    let mut next = 0;
    for (source, target) in program.syntax().iter().zip(output.iter_mut()) {
        match (source, target) {
            (ASTNode::Function(f), ASTNode::Function(out))
                if f.is_async && f.generic_params.is_empty() =>
            {
                let (ctor, extra) = emit_frame(f, plan(program, f)?, next);
                next += 1;
                *out = ctor;
                generated.extend(extra);
            },
            (ASTNode::Struct(s), ASTNode::Struct(out)) if s.generic_params.is_empty() => {
                for (f, target) in s.methods.iter().zip(&mut out.methods) {
                    if f.is_async && f.generic_params.is_empty() {
                        let (ctor, extra) = emit_frame(f, plan(program, f)?, next);
                        next += 1;
                        *target = ctor;
                        generated.extend(extra);
                    }
                }
            },
            (ASTNode::ProtoImpl(s), ASTNode::ProtoImpl(out)) => {
                for (f, target) in s.methods.iter().zip(&mut out.methods) {
                    if f.is_async && f.generic_params.is_empty() {
                        let (ctor, extra) = emit_frame(f, plan(program, f)?, next);
                        next += 1;
                        *target = ctor;
                        generated.extend(extra);
                    }
                }
            },
            _ => {},
        }
    }
    output.extend(generated);
    Ok(output)
}
