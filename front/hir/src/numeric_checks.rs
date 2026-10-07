// SPDX-License-Identifier: MPL-2.0
//! Diagnose statically invalid checked numeric operations using the same ordered
//! HIR conversions as runtime lowering. Mutable bindings are never propagated.
use super::{conversions::*, *};
use parser::ast::{FunctionNode, Literal, Mutability, Operator};
use utils::const_int::ConstInt;

/// Fully converted numeric value, independent of backend representation.
#[derive(Clone, Debug)]
pub enum ConstantValue {
    Int(ConstInt),
    Float(f64),
}

/// Materialize an integer token at the floating width already selected by HIR.
/// In particular, f32 must round once, without an intermediate f64 conversion.
pub fn integer_literal_float(raw: &str, bits: u16) -> Option<f64> {
    let parsed = lexer::number::IntegerLiteral::parse(raw)?;
    let mut n = ConstInt::from_digits(&parsed.digits, parsed.radix)?;
    if n.bits() > 1024 {
        return None;
    }
    if parsed.negative {
        n = n.negated();
    }
    match Number::Int(n).normalize(&WaveType::Float(bits)) {
        Number::Float(n) => Some(n),
        _ => None,
    }
}

type Scope = HashMap<String, Option<Number>>;
#[derive(Clone)]
enum Number {
    Int(ConstInt),
    Float(f64),
    Array(Vec<Option<Number>>),
    Struct(HashMap<String, Option<Number>>),
}
impl Number {
    fn truth(&self) -> bool {
        match self {
            Self::Int(n) => !n.is_zero(),
            Self::Float(n) => *n != 0.0,
            _ => unreachable!("aggregate is not a numeric truth value"),
        }
    }

    fn normalize(self, ty: &WaveType) -> Self {
        match (self, ty) {
            (Self::Int(n), WaveType::Float(32)) => Self::Float(n.to_f32() as f64),
            (Self::Int(n), WaveType::Float(64)) => Self::Float(n.to_f64()),
            (Self::Int(n), ty) if integer_width(ty).is_some() => {
                let bits = integer_width(ty).unwrap();
                Self::Int(n.normalize(bits, !unsigned(ty)))
            },
            (Self::Float(n), WaveType::Float(32)) => Self::Float((n as f32) as f64),
            (n, _) => n,
        }
    }
}
struct Checker<'a> {
    program: &'a TypedProgram,
    globals: Scope,
    constants: HashMap<ExpressionId, ConstantValue>,
    required: bool,
}
type Failure = (ExpressionId, String);
impl Checker<'_> {
    fn fail(&self, expr: &Expression, message: &str) -> Failure {
        (self.program.expression_id(expr).unwrap(), message.into())
    }
    // The evaluator recurses only through compact dispatch frames. Conversion
    // and arithmetic temporaries are materialized after operands have returned.
    fn eval(&mut self, expr: &Expression, scope: &Scope) -> Result<Option<Number>, Failure> {
        let value = self.eval_value(expr, scope)?;
        self.convert_value(expr, value)
    }

    fn eval_value(&mut self, expr: &Expression, scope: &Scope) -> Result<Option<Number>, Failure> {
        Ok(match expr {
            Expression::ArrayLiteral(items) => {
                let mut values = Vec::new();
                for item in items {
                    values.push(self.eval(item, scope)?);
                }
                Some(Number::Array(values))
            },
            Expression::StructLiteral { fields, .. } => {
                let mut values = HashMap::new();
                for (name, item) in fields {
                    values.insert(name.clone(), self.eval(item, scope)?);
                }
                Some(Number::Struct(values))
            },
            Expression::FieldAccess { object, field } => {
                if let Some(Number::Struct(fields)) = self.eval(object, scope)? {
                    fields.get(field).cloned().flatten()
                } else {
                    None
                }
            },
            Expression::IndexAccess { target, index } => {
                match (self.eval(target, scope)?, self.eval(index, scope)?) {
                    (Some(Number::Array(items)), Some(Number::Int(index))) => {
                        index.to_usize().and_then(|i| items.get(i).cloned().flatten())
                    },
                    _ => None,
                }
            },
            Expression::Literal(Literal::Int(raw)) => {
                let parsed = lexer::number::IntegerLiteral::parse(raw).unwrap();
                let mut n =
                    ConstInt::from_digits(&parsed.digits, parsed.radix).ok_or_else(|| {
                        self.fail(expr, "integer literal exceeds the supported 1024-bit range")
                    })?;
                if n.bits() > 1024 {
                    return Err(
                        self.fail(expr, "integer literal exceeds the supported 1024-bit range")
                    );
                }
                if parsed.negative {
                    n = n.negated();
                }
                Some(Number::Int(n))
            },
            Expression::Literal(Literal::Float(n)) => Some(Number::Float(*n)),
            Expression::Literal(Literal::Bool(n)) => {
                Some(Number::Int(ConstInt::from_u64((*n) as u64)))
            },
            Expression::Literal(Literal::Byte(n)) => {
                Some(Number::Int(ConstInt::from_u64((*n) as u64)))
            },
            Expression::Literal(Literal::Char(n)) => {
                Some(Number::Int(ConstInt::from_u64((*n as u32) as u64)))
            },
            Expression::Variable(name) => {
                if let Some(value) = scope.get(name) {
                    value.clone()
                } else {
                    self.globals.get(name).cloned().flatten()
                }
            },

            Expression::Cast { expr, .. } | Expression::Grouped(expr) => self.eval(expr, scope)?,
            Expression::Unary { operator, expr: inner } => {
                match (operator, self.eval(inner, scope)?) {
                    (Operator::Neg, Some(Number::Int(n))) => Some(Number::Int(n.negated())),
                    (Operator::Neg, Some(Number::Float(n))) => Some(Number::Float(-n)),
                    (Operator::BitwiseNot, Some(Number::Int(n))) => {
                        n.checked_not().map(Number::Int)
                    },
                    (Operator::Not | Operator::LogicalNot, Some(n)) => {
                        Some(Number::Int(ConstInt::from_u64((!n.truth()) as u64)))
                    },
                    _ => None,
                }
            },
            Expression::BinaryExpression { left, operator, right } => {
                self.eval_binary(expr, left, operator, right, scope)?
            },
            _ => {
                let mut result = Ok(());
                parser::ast::visit::walk_expression_children(expr, &mut |child| {
                    if result.is_ok() {
                        result = self.inspect(child, scope);
                    }
                });
                result?;
                None
            },
        })
    }

    fn eval_binary(
        &mut self,
        expr: &Expression,
        left: &Expression,
        operator: &Operator,
        right: &Expression,
        scope: &Scope,
    ) -> Result<Option<Number>, Failure> {
        let a = self.eval(left, scope)?;
        let short = match (&a, operator) {
            (Some(n), Operator::LogicalAnd) if !n.truth() => Some(false),
            (Some(n), Operator::LogicalOr) if n.truth() => Some(true),
            _ => None,
        };
        let b = if short.is_some() { None } else { self.eval(right, scope)? };
        self.binary_value(expr, right, operator, a, b, short)
    }

    fn binary_value(
        &self,
        expr: &Expression,
        right: &Expression,
        operator: &Operator,
        a: Option<Number>,
        b: Option<Number>,
        short: Option<bool>,
    ) -> Result<Option<Number>, Failure> {
        let fact = self.program.numeric_expression_of(expr);
        let Some(fact) = fact else {
            return Ok(None);
        };
        if fact.shift_count_type.is_some() {
            if let Some(Number::Int(n)) = &b {
                let width = integer_width(fact.computation_type.as_ref().unwrap()).unwrap();
                if n.is_negative() || n >= &ConstInt::from_u64((width) as u64) {
                    return Err(self.fail(right, &format!("shift count must be in 0..{width}")));
                }
            }
        }
        Ok(if let Some(value) = short {
            Some(Number::Int(ConstInt::from_u64(value as u64)))
        } else {
            match (a, b) {
                (Some(Number::Int(a)), Some(Number::Int(b))) => {
                    use Operator::*;
                    if self.required && matches!(operator, Divide | Remainder) {
                        if b.is_zero() {
                            return Err(self.fail(
                                right,
                                "division or remainder by zero in constant expression",
                            ));
                        }
                        if matches!(operator, Divide) {
                            let ty = fact.computation_type.as_ref().unwrap();
                            let width = integer_width(ty).unwrap();
                            let min = ConstInt::from_u64(1)
                                .checked_shl(width as usize - 1)
                                .unwrap()
                                .negated();
                            if !unsigned(ty) && a == min && b == ConstInt::from_u64(1).negated() {
                                return Err(self.fail(
                                    expr,
                                    "signed division overflows in constant expression",
                                ));
                            }
                        }
                    }
                    let n = match operator {
                        Add => a.checked_add(&b),
                        Subtract => a.checked_sub(&b),
                        Multiply => a.checked_mul(&b),
                        Divide if !b.is_zero() => a.div_rem(&b).map(|(q, _)| q),
                        Remainder if !b.is_zero() => a.div_rem(&b).map(|(_, r)| r),
                        ShiftLeft => b.to_usize().and_then(|n| a.checked_shl(n)),
                        ShiftRight => b.to_usize().map(|n| a.shifted_right(n)),
                        BitwiseAnd => a.bitand(&b),
                        BitwiseOr => a.bitor(&b),
                        BitwiseXor => a.bitxor(&b),
                        Equal => Some(ConstInt::from_u64((a == b) as u64)),
                        NotEqual => Some(ConstInt::from_u64((a != b) as u64)),
                        Less => Some(ConstInt::from_u64((a < b) as u64)),
                        LessEqual => Some(ConstInt::from_u64((a <= b) as u64)),
                        Greater => Some(ConstInt::from_u64((a > b) as u64)),
                        GreaterEqual => Some(ConstInt::from_u64((a >= b) as u64)),
                        LogicalAnd => {
                            Some(ConstInt::from_u64((!a.is_zero() && !b.is_zero()) as u64))
                        },
                        LogicalOr => {
                            Some(ConstInt::from_u64((!a.is_zero() || !b.is_zero()) as u64))
                        },
                        _ => None,
                    };
                    n.map(Number::Int)
                },
                (Some(Number::Float(a)), Some(Number::Float(b))) => {
                    use Operator::*;
                    let single = fact.computation_type == Some(WaveType::Float(32));
                    let result = match operator {
                        Add => Some(if single { ((a as f32) + (b as f32)) as f64 } else { a + b }),
                        Subtract => {
                            Some(if single { ((a as f32) - (b as f32)) as f64 } else { a - b })
                        },
                        Multiply => {
                            Some(if single { ((a as f32) * (b as f32)) as f64 } else { a * b })
                        },
                        Divide => {
                            Some(if single { ((a as f32) / (b as f32)) as f64 } else { a / b })
                        },
                        Remainder => {
                            Some(if single { ((a as f32) % (b as f32)) as f64 } else { a % b })
                        },
                        _ => None,
                    };
                    match operator {
                        Equal => Some(Number::Int(ConstInt::from_u64((a == b) as u64))),
                        NotEqual => Some(Number::Int(ConstInt::from_u64((a != b) as u64))),
                        Less => Some(Number::Int(ConstInt::from_u64((a < b) as u64))),
                        LessEqual => Some(Number::Int(ConstInt::from_u64((a <= b) as u64))),
                        Greater => Some(Number::Int(ConstInt::from_u64((a > b) as u64))),
                        GreaterEqual => Some(Number::Int(ConstInt::from_u64((a >= b) as u64))),
                        _ => result.map(Number::Float),
                    }
                },
                _ => None,
            }
        })
    }

    fn convert_value(
        &mut self,
        expr: &Expression,
        value: Option<Number>,
    ) -> Result<Option<Number>, Failure> {
        let fact = self.program.numeric_expression_of(expr);
        let Some(mut value) = value else {
            if self.required
                && fact.is_some_and(|f| {
                    matches!(
                        f.result_type,
                        WaveType::Int(_) | WaveType::Uint(_) | WaveType::Float(_) | WaveType::Bool
                    )
                })
            {
                return Err(self.fail(expr, "unsupported numeric constant expression"));
            }
            return Ok(None);
        };
        let Some(fact) = fact else {
            return Ok(Some(value));
        };
        value = value.normalize(&fact.evaluation_type);
        for step in &fact.conversions {
            use ConversionKind::*;
            value = match step.kind {
                IntegerToBool | FloatToBool => Number::Int(ConstInt::from_u64((value.truth()) as u64)),
                SignedToFloat | UnsignedToFloat => {
                    let Number::Int(n) = value else { return Ok(None); };
                    Number::Float(if step.target_type == WaveType::Float(32) { n.to_f32() as f64 } else { n.to_f64() })
                }
                FloatToSigned | FloatToUnsigned => {
                    let Number::Float(n) = value else { return Ok(None); };
                    let integer = ConstInt::from_f64(n.trunc()).ok_or_else(|| self.fail(expr, "invalid float-to-integer conversion: NaN or infinity"))?;
                    if !float_integer_range(&step.source_type, &step.target_type).contains(n) {
                        return Err(self.fail(expr, "float-to-integer conversion is out of range after truncation toward zero"));
                    }
                    Number::Int(integer)
                }
                PointerCast | PointerToInteger | IntegerToPointer => return Ok(None),
                _ => value,
            }.normalize(&step.target_type);
        }
        if self.required {
            let constant = match &value {
                Number::Int(n) => Some(ConstantValue::Int(n.clone())),
                Number::Float(n) => Some(ConstantValue::Float(*n)),
                _ => None,
            };
            if let Some(constant) = constant {
                self.constants.insert(self.program.expression_id(expr).unwrap(), constant);
            }
        }
        Ok(Some(value))
    }

    fn inspect(&mut self, expr: &Expression, scope: &Scope) -> Result<(), Failure> {
        self.eval(expr, scope).map(|_| ())
    }
    // Syntactic eligibility is checked even in a dead logical operand; only
    // evaluation errors are suppressed by short-circuiting.
    fn require_constant(&self, expr: &Expression) -> Result<(), Failure> {
        let allowed = match expr {
            Expression::Literal(_)
            | Expression::Null
            | Expression::ArrayLiteral(_)
            | Expression::StructLiteral { .. }
            | Expression::Grouped(_)
            | Expression::Cast { .. } => true,
            Expression::BinaryExpression { .. } => self
                .program
                .numeric_expression_of(expr)
                .is_some_and(|fact| fact.computation_type.is_some()),
            Expression::Unary { operator, .. } => matches!(
                operator,
                Operator::Neg | Operator::BitwiseNot | Operator::Not | Operator::LogicalNot
            ),
            Expression::Variable(name) => {
                self.globals.contains_key(name)
                    || self.program.variant_construction_of(expr).is_some()
            },
            Expression::FunctionCall { .. } => self.program.variant_construction_of(expr).is_some(),
            _ => false,
        };
        if !allowed {
            return Err(self.fail(expr, "unsupported constant expression: calls and memory access require runtime evaluation"));
        }
        let mut result = Ok(());
        parser::ast::visit::walk_expression_children(expr, &mut |child| {
            if result.is_ok() {
                result = self.require_constant(child);
            }
        });
        result
    }

    fn function(&mut self, f: &FunctionNode) -> Result<(), Failure> {
        let mut scope = Scope::new();
        for p in &f.parameters {
            if let Some(default) = &p.initial_value {
                self.inspect(default, &scope)?;
            }
            scope.insert(p.name.clone(), None);
        }
        self.nodes(&f.body, &mut scope)
    }

    fn nodes(&mut self, nodes: &[ASTNode], scope: &mut Scope) -> Result<(), Failure> {
        for node in nodes {
            self.node(node, scope)?;
        }
        Ok(())
    }

    fn node(&mut self, node: &ASTNode, scope: &mut Scope) -> Result<(), Failure> {
        match node {
            ASTNode::Located { value, .. } => self.node(value, scope)?,
            ASTNode::Function(f) => self.function(f)?,
            ASTNode::Struct(s) => {
                for f in &s.methods {
                    self.function(f)?;
                }
            },
            ASTNode::ProtoImpl(p) => {
                for f in &p.methods {
                    self.function(f)?;
                }
            },
            ASTNode::Variable(v) => {
                let mut value = None;
                if let Some(expr) = &v.initial_value {
                    self.inspect(expr, scope)?;
                    if v.mutability == Mutability::Const {
                        value = self.eval(expr, scope)?;
                    }
                }
                scope.insert(v.name.clone(), value);
            },
            ASTNode::Statement(StatementNode::If {
                condition,
                body,
                else_if_blocks,
                else_block,
            }) => {
                self.inspect(condition, scope)?;
                self.nodes(body, &mut scope.clone())?;
                if let Some(blocks) = else_if_blocks {
                    for (c, b) in blocks.iter() {
                        self.inspect(c, scope)?;
                        self.nodes(b, &mut scope.clone())?;
                    }
                }
                if let Some(b) = else_block {
                    self.nodes(b, &mut scope.clone())?;
                }
            },
            ASTNode::Statement(StatementNode::While { condition, body }) => {
                self.inspect(condition, scope)?;
                self.nodes(body, &mut scope.clone())?;
            },
            ASTNode::Statement(StatementNode::For {
                initialization,
                condition,
                increment,
                body,
            }) => {
                let mut scope = scope.clone();
                self.node(initialization, &mut scope)?;
                self.inspect(condition, &scope)?;
                self.inspect(increment, &scope)?;
                self.nodes(body, &mut scope)?;
            },
            ASTNode::Statement(StatementNode::Match { value, arms }) => {
                self.inspect(value, scope)?;
                for arm in arms {
                    let mut scope = scope.clone();
                    super::walk_pattern(&arm.pattern, &mut |p| {
                        if let MatchPattern::Binding(name) = p {
                            scope.insert(name.clone(), None);
                        }
                    });
                    self.nodes(&arm.body, &mut scope)?;
                }
            },
            _ => {
                let mut result = Ok(());
                let mut visited = HashSet::new();
                super::walk_node(node, &mut |expr| {
                    let id = self.program.expression_id(expr).unwrap();
                    if result.is_ok() && !visited.contains(&id) {
                        result = self.inspect(expr, scope);
                        super::walk_expression(expr, &mut |child| {
                            visited.insert(self.program.expression_id(child).unwrap());
                        });
                    }
                });
                result?;
            },
        }
        Ok(())
    }
}
pub(super) fn validate(
    program: &TypedProgram,
) -> Result<HashMap<ExpressionId, ConstantValue>, SemanticDiagnostic> {
    let diagnostic = |index, (id, message): Failure| SemanticDiagnostic {
        code: "E3001".into(),
        message: message.clone(),
        top_level_index: index,
        primary: None,
        span: program.expression_span(id).cloned(),
        label: message,
        note: None,
        help: "use a supported constant expression with valid numeric operations".into(),
    };
    let definitions: HashMap<_, _> = program
        .syntax()
        .iter()
        .enumerate()
        .filter_map(|(i, node)| match node {
            ASTNode::Variable(v) if v.mutability == Mutability::Const => {
                v.initial_value.as_ref().map(|e| (v.name.clone(), (i, e)))
            },
            _ => None,
        })
        .collect();
    let mut checker =
        Checker { program, globals: Scope::new(), constants: HashMap::new(), required: true };
    for node in program.syntax() {
        if let ASTNode::Enum(e) = node {
            let mut next = ConstInt::zero();
            for variant in &e.variants {
                if let Some(raw) = &variant.explicit_value {
                    let parsed = lexer::number::IntegerLiteral::parse(raw).unwrap();
                    next = ConstInt::from_digits(&parsed.digits, parsed.radix)
                        .expect("validated enum integer");
                    if parsed.negative {
                        next = next.negated();
                    }
                }
                checker.globals.insert(
                    variant.name.clone(),
                    Some(Number::Int(next.clone()).normalize(&e.repr_type)),
                );
                next = next
                    .checked_add(&ConstInt::from_u64(1))
                    .expect("enum value fits constant storage");
            }
        }
    }
    // Constant dependency cycles are rejected by semantic validation. Evaluate
    // this DAG iteratively so long forward-reference chains do not use the stack.
    let mut visited = HashSet::new();
    for root in definitions.keys() {
        let mut stack = vec![(root.clone(), false)];
        while let Some((name, ready)) = stack.pop() {
            let &(index, expr) = &definitions[&name];
            if ready {
                checker.require_constant(expr).map_err(|e| diagnostic(index, e))?;
                let value = checker.eval(expr, &Scope::new()).map_err(|e| diagnostic(index, e))?;
                checker.globals.insert(name, value);
            } else if visited.insert(name.clone()) {
                stack.push((name, true));
                super::walk_expression(expr, &mut |e| {
                    if let Expression::Variable(name) = e {
                        if definitions.contains_key(name) {
                            stack.push((name.clone(), false));
                        }
                    }
                });
            }
        }
    }
    for (index, node) in program.syntax().iter().enumerate() {
        if let ASTNode::Variable(v) = node {
            if v.mutability == Mutability::Static {
                if let Some(expr) = &v.initial_value {
                    checker.require_constant(expr).map_err(|e| diagnostic(index, e))?;
                    checker.eval(expr, &Scope::new()).map_err(|e| diagnostic(index, e))?;
                }
            }
        }
    }
    checker.required = false;
    for (index, node) in program.syntax().iter().enumerate() {
        checker.node(node, &mut Scope::new()).map_err(|e| diagnostic(index, e))?;
    }
    Ok(checker.constants)
}
