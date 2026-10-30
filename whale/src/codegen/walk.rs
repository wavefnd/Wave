// SPDX-License-Identifier: MPL-2.0
//! Iterative, source-ordered expression lowering. Even the maximum permitted
//! nesting depth must work on a 1 MiB stack in debug builds.
use super::*;
use parser::ast::{AssignOperator, IncDecKind, Literal, Operator};

enum Action {
    Enter,
    Convert,
    Unary,
    Binary,
    WideShift,
    Assign,
    CompoundAssign,
    Call,
    LogicalLeft,
    LogicalRight { lhs: ValueId, lhs_block: BlockId, end: BlockId },
}
fn pop(values: &mut Vec<Value>) -> Result<Value> {
    values.pop().ok_or_else(|| ice("expression value stack underflow"))
}

impl Lowerer<'_> {
    pub(super) fn expression(&mut self, root: &Expression) -> Result<Value> {
        let mut pending = vec![(root, Action::Enter)];
        let mut values = Vec::new();
        while let Some((expr, action)) = pending.pop() {
            let result = self.step(expr, action, &mut pending, &mut values);
            result.map_err(|mut e| {
                if e.span.is_none() {
                    e.span = self
                        .program
                        .expression_id(expr)
                        .and_then(|id| self.program.expression_span(id))
                        .cloned();
                }
                e
            })?;
        }
        if values.len() != 1 {
            return Err(ice("expression value stack imbalance"));
        }
        pop(&mut values)
    }

    fn step<'e>(
        &mut self,
        expr: &'e Expression,
        action: Action,
        pending: &mut Vec<(&'e Expression, Action)>,
        values: &mut Vec<Value>,
    ) -> Result<()> {
        let program = self.program;
        let fact = program
            .numeric_expression_of(expr)
            .ok_or_else(|| unsupported(expression_kind(expr)))?;
        let ty = scalar(&fact.evaluation_type)?;
        match action {
            Action::Enter => {
                pending.push((expr, Action::Convert));
                match expr.unspanned() {
                    Expression::Literal(literal) => {
                        values.push(self.literal(literal, &fact.evaluation_type)?)
                    },
                    Expression::Variable(name) => {
                        let value = if let Some(slot) = self.local_slot(name) {
                            let value = self.value(slot.ty.clone());
                            self.emit(Instruction::Load {
                                dst: value.id,
                                ty: slot.ty,
                                ptr: slot.ptr,
                                align: 1,
                            });
                            value
                        } else if let Some((ty, value)) = self.constants.get(name).cloned() {
                            // HIR already evaluated dependencies and ordered casts.
                            // Do not execute a global initializer in each function.
                            self.constant(ty, value)
                        } else {
                            return Err(unsupported(format!("binding `{name}`")));
                        };
                        values.push(value);
                    },
                    Expression::IncDec { kind, target } => {
                        let Expression::Variable(name) = target.unspanned() else {
                            return Err(unsupported("non-local increment/decrement"));
                        };
                        let slot = self
                            .local_slot(name)
                            .ok_or_else(|| unsupported("non-local increment/decrement"))?;
                        let old = self.value(slot.ty.clone());
                        self.emit(Instruction::Load {
                            dst: old.id,
                            ty: slot.ty.clone(),
                            ptr: slot.ptr,
                            align: 1,
                        });
                        let one = self.literal(&Literal::Int("1".into()), &fact.evaluation_type)?;
                        let operator = if matches!(kind, IncDecKind::PreInc | IncDecKind::PostInc) {
                            Operator::Add
                        } else {
                            Operator::Subtract
                        };
                        let new = self.binary_values(
                            old.clone(),
                            &operator,
                            one,
                            &fact.evaluation_type,
                            &slot.ty,
                        )?;
                        self.store(&slot, new.clone())?;
                        values.push(if matches!(kind, IncDecKind::PreInc | IncDecKind::PreDec) {
                            new
                        } else {
                            old
                        });
                    },
                    Expression::Cast { expr, .. } | Expression::Grouped(expr) => {
                        pending.push((expr, Action::Enter))
                    },
                    Expression::Assignment { target, value }
                    | Expression::AssignOperation {
                        target,
                        operator: AssignOperator::Assign,
                        value,
                    } => {
                        if !matches!(target.unspanned(), Expression::Variable(_)) {
                            return Err(unsupported("non-local assignment"));
                        }
                        pending.push((expr, Action::Assign));
                        pending.push((value, Action::Enter));
                    },
                    Expression::AssignOperation { target, operator, value } => {
                        if !matches!(target.unspanned(), Expression::Variable(_)) {
                            return Err(unsupported("non-local assignment"));
                        }
                        if !matches!(
                            operator,
                            AssignOperator::AddAssign
                                | AssignOperator::SubAssign
                                | AssignOperator::MulAssign
                                | AssignOperator::DivAssign
                                | AssignOperator::RemAssign
                        ) {
                            return Err(unsupported(format!("compound operation {operator:?}")));
                        }
                        // Capture the local's old value before evaluating the RHS,
                        // which may itself assign to the same local.
                        pending.push((expr, Action::CompoundAssign));
                        pending.push((value, Action::Enter));
                        pending.push((target, Action::Enter));
                    },
                    Expression::FunctionCall { args, .. } => {
                        pending.push((expr, Action::Call));
                        for arg in args.iter().rev() {
                            pending.push((arg, Action::Enter));
                        }
                    },
                    Expression::Unary { expr: operand, .. } => {
                        pending.push((expr, Action::Unary));
                        pending.push((operand, Action::Enter));
                    },
                    Expression::BinaryExpression { left, operator, right } => {
                        if matches!(operator, Operator::LogicalAnd | Operator::LogicalOr) {
                            pending.push((expr, Action::LogicalLeft));
                        } else if fact
                            .shift_count_type
                            .as_ref()
                            .and_then(hir::conversions::integer_width)
                            .is_some_and(|bits| bits > 128)
                        {
                            pending.push((expr, Action::WideShift));
                        } else {
                            pending.push((expr, Action::Binary));
                            pending.push((right, Action::Enter));
                        }
                        pending.push((left, Action::Enter));
                    },
                    _ => return Err(unsupported(expression_kind(expr))),
                }
            },
            Action::Convert => {
                let mut value = pop(values)?;
                if value.ty != ty {
                    return Err(ice(format!(
                        "expression produced {} but HIR requires {ty}",
                        value.ty
                    )));
                }
                for conversion in &fact.conversions {
                    value = self.convert(value, conversion).map_err(|mut e| {
                        e.span = Some(conversion.span.clone());
                        e
                    })?;
                }
                values.push(value);
            },
            Action::Unary => {
                let Expression::Unary { operator, .. } = expr.unspanned() else { unreachable!() };
                let operand = pop(values)?;
                values.push(self.unary_value(operator, operand, fact)?);
            },
            Action::Binary => {
                let Expression::BinaryExpression { operator, .. } = expr.unspanned() else {
                    unreachable!()
                };
                let rhs = pop(values)?;
                let lhs = pop(values)?;
                let computation = fact
                    .computation_type
                    .as_ref()
                    .ok_or_else(|| ice("missing computation type"))?;
                values.push(if let Some(count_type) = &fact.shift_count_type {
                    self.checked_shift(lhs, operator, rhs, computation, count_type)?
                } else {
                    self.binary_values(lhs, operator, rhs, computation, &ty)?
                });
            },
            Action::WideShift => {
                let Expression::BinaryExpression { operator, right, .. } = expr.unspanned() else {
                    unreachable!()
                };
                let lhs = pop(values)?;
                let computation = fact
                    .computation_type
                    .as_ref()
                    .ok_or_else(|| ice("missing shift computation type"))?;
                values.push(self.wide_constant_shift(lhs, operator, right, computation)?);
            },
            Action::Assign => {
                let (Expression::Assignment { target, .. }
                | Expression::AssignOperation { target, .. }) = expr.unspanned()
                else {
                    unreachable!()
                };
                let Expression::Variable(name) = target.unspanned() else { unreachable!() };
                let value = pop(values)?;
                self.store(&self.slot(name)?, value.clone())?;
                values.push(value);
            },
            Action::CompoundAssign => {
                let Expression::AssignOperation { target, operator, .. } = expr.unspanned() else {
                    unreachable!()
                };
                let Expression::Variable(name) = target.unspanned() else { unreachable!() };
                let rhs = pop(values)?;
                let lhs = pop(values)?;
                let computation = fact
                    .computation_type
                    .as_ref()
                    .ok_or_else(|| ice("missing compound assignment computation type"))?;
                let operator = match operator {
                    AssignOperator::AddAssign => Operator::Add,
                    AssignOperator::SubAssign => Operator::Subtract,
                    AssignOperator::MulAssign => Operator::Multiply,
                    AssignOperator::DivAssign => Operator::Divide,
                    AssignOperator::RemAssign => Operator::Remainder,
                    _ => unreachable!(),
                };
                let value = self.binary_values(lhs, &operator, rhs, computation, &ty)?;
                self.store(&self.slot(name)?, value.clone())?;
                values.push(value);
            },
            Action::Call => {
                let Expression::FunctionCall { name, args, .. } = expr.unspanned() else {
                    unreachable!()
                };
                let start = values
                    .len()
                    .checked_sub(args.len())
                    .ok_or_else(|| ice("call value stack underflow"))?;
                let args = values.split_off(start);
                values.push(
                    self.call_values(name, args)?
                        .ok_or_else(|| unsupported("void calls in value expressions"))?,
                );
            },
            Action::LogicalLeft => {
                let Expression::BinaryExpression { operator, right, .. } = expr.unspanned() else {
                    unreachable!()
                };
                let lhs = pop(values)?;
                if lhs.ty != Type::Bool {
                    return Err(ice("logical operand missing HIR bool conversion"));
                }
                let lhs_block = BlockId(self.block as u32);
                let rhs = self.new_block("logical.rhs");
                let end = self.new_block("logical.end");
                let (then_bb, else_bb) =
                    if matches!(operator, Operator::LogicalAnd) { (rhs, end) } else { (end, rhs) };
                self.terminate(Terminator::CBr { cond: lhs.id, then_bb, else_bb });
                self.switch(rhs);
                pending.push((expr, Action::LogicalRight { lhs: lhs.id, lhs_block, end }));
                pending.push((right, Action::Enter));
            },
            Action::LogicalRight { lhs, lhs_block, end } => {
                let rhs = pop(values)?;
                if rhs.ty != Type::Bool {
                    return Err(ice("logical operand missing HIR bool conversion"));
                }
                let rhs_exit = BlockId(self.block as u32);
                self.terminate(Terminator::Br { target: end });
                self.switch(end);
                let value = self.value(Type::Bool);
                self.emit(Instruction::Phi {
                    dst: value.id,
                    ty: Type::Bool,
                    incomings: vec![(lhs, lhs_block), (rhs.id, rhs_exit)],
                });
                values.push(value);
            },
        }
        Ok(())
    }
}

fn expression_kind(expression: &Expression) -> &'static str {
    match expression.unspanned() {
        Expression::StructLiteral { .. } => "struct construction",
        Expression::MethodCall { .. } => "method call",
        Expression::Await(_) => "await expression",
        Expression::Null => "null pointer expression",
        Expression::Deref(_) => "pointer dereference",
        Expression::AddressOf(_) => "address-of expression",
        Expression::IndexAccess { .. } => "index access",
        Expression::ArrayLiteral(_) => "array literal",
        Expression::AsmBlock { .. } => "inline assembly expression",
        Expression::FieldAccess { .. } => "field access",
        Expression::IncDec { .. } => "increment/decrement expression",
        Expression::Literal(_) => "non-scalar literal",
        Expression::Variable(_) => "non-scalar variable",
        Expression::FunctionCall { .. } => "non-scalar function call",
        Expression::Grouped(_) => "non-scalar grouped expression",
        Expression::Unary { .. } => "non-scalar unary expression",
        Expression::BinaryExpression { .. } => "non-scalar binary expression",
        Expression::Cast { .. } => "non-scalar cast",
        Expression::Assignment { .. } | Expression::AssignOperation { .. } => {
            "non-scalar assignment"
        },
        Expression::Located { .. } => unreachable!("unspanned expression"),
    }
}
