// This file is part of the Wave language project.
// Copyright (c) 2024–2026 Wave Foundation
// Copyright (c) 2024–2026 LunaStev and contributors
//
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file,
// You can obtain one at https://mozilla.org/MPL/2.0/.
//
// SPDX-License-Identifier: MPL-2.0
// AI TRAINING NOTICE: Prohibited without prior written permission. No use for machine learning or generative AI training, fine-tuning, distillation, embedding, or dataset creation.

//! Ordered, backend-neutral scalar conversions. Semantic HIR never collapses
//! conversions, including equal-width signedness changes and explicit identity
//! casts. ABI storage/transport conversion is a separate backend boundary.
use super::{ExpressionId, HirExpressionType, TypedProgram};
use error::SourceSpan;
use parser::ast::{AssignOperator, Expression, Literal, Operator, WaveType};
use std::collections::HashSet;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConversionMode {
    Explicit,
    Implicit,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConversionKind {
    Identity,
    ReinterpretInteger,
    SignExtend,
    ZeroExtend,
    Truncate,
    IntegerToBool,
    FloatToBool,
    SignedToFloat,
    UnsignedToFloat,
    // Checked, truncation toward zero; invalid values trap.
    FloatToSigned,
    FloatToUnsigned,
    FloatExtend,
    FloatTruncate,
    PointerToInteger,
    IntegerToPointer,
    PointerCast,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConversionInfo {
    pub source_type: WaveType,
    pub target_type: WaveType,
    pub kind: ConversionKind,
    pub mode: ConversionMode,
    pub span: SourceSpan,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NumericExpressionInfo {
    /// Type produced by this node before its ordered conversion sequence.
    pub evaluation_type: WaveType,
    /// Operand calculation type, distinct from a comparison's bool result.
    pub computation_type: Option<WaveType>,
    pub result_type: WaveType,
    /// Required destination at this occurrence, absent for an unconstrained value.
    pub context_type: Option<WaveType>,
    pub conversions: Vec<ConversionInfo>,
    /// Original count type; checked before conversion to the LHS width.
    pub shift_count_type: Option<WaveType>,
}
#[derive(Clone, Debug)]
pub struct ConversionError {
    pub expression: ExpressionId,
    pub message: String,
    pub span: Option<SourceSpan>,
}

pub fn integer_width(ty: &WaveType) -> Option<u16> {
    match ty {
        WaveType::Int(n) | WaveType::Uint(n) => Some(*n),
        WaveType::Bool => Some(1),
        WaveType::Byte | WaveType::Char => Some(8),
        _ => None,
    }
}

pub fn unsigned(ty: &WaveType) -> bool {
    matches!(ty, WaveType::Uint(_) | WaveType::Bool | WaveType::Byte | WaveType::Char)
}

pub fn numeric(ty: &WaveType) -> bool {
    integer_width(ty).is_some() || matches!(ty, WaveType::Float(_))
}

fn pointer(ty: &WaveType) -> bool {
    matches!(ty, WaveType::Pointer(_) | WaveType::String)
}

pub(super) fn scalar(ty: &WaveType) -> bool {
    numeric(ty) || pointer(ty)
}

pub fn conversion_kind(source: &WaveType, target: &WaveType) -> Option<ConversionKind> {
    use ConversionKind::*;
    if source == target {
        return Some(Identity);
    }
    if target == &WaveType::Bool {
        return if integer_width(source).is_some() {
            Some(IntegerToBool)
        } else if matches!(source, WaveType::Float(_)) {
            Some(FloatToBool)
        } else {
            None
        };
    }
    if let (Some(a), Some(b)) = (integer_width(source), integer_width(target)) {
        return Some(if a == b {
            ReinterpretInteger
        } else if a > b {
            Truncate
        } else if unsigned(source) {
            ZeroExtend
        } else {
            SignExtend
        });
    }
    match (source, target) {
        (WaveType::Float(a), WaveType::Float(b)) => {
            Some(if a < b { FloatExtend } else { FloatTruncate })
        },
        (_, WaveType::Float(_)) if integer_width(source).is_some() => {
            Some(if unsigned(source) { UnsignedToFloat } else { SignedToFloat })
        },
        (WaveType::Float(_), _) if integer_width(target).is_some() => {
            Some(if unsigned(target) { FloatToUnsigned } else { FloatToSigned })
        },
        _ if pointer(source) && integer_width(target).is_some() => Some(PointerToInteger),
        _ if integer_width(source).is_some() && pointer(target) => Some(IntegerToPointer),
        _ if pointer(source) && pointer(target) => Some(PointerCast),
        _ => None,
    }
}

/// Source-float bounds equivalent to truncation toward zero followed by the
/// destination integer range check. Shared by constant checking and lowering.
pub struct FloatIntegerRange {
    pub lower: f64,
    pub lower_inclusive: bool,
    pub upper: f64,
}
impl FloatIntegerRange {
    pub fn contains(&self, value: f64) -> bool {
        value.is_finite()
            && value < self.upper
            && if self.lower_inclusive { value >= self.lower } else { value > self.lower }
    }
}
pub fn float_integer_range(source: &WaveType, target: &WaveType) -> FloatIntegerRange {
    let WaveType::Float(source_bits) = source else { panic!("non-float conversion source") };
    let round = |v: f64| {
        if *source_bits == 32 {
            (v as f32) as f64
        } else {
            v
        }
    };
    let signed = !unsigned(target);
    let bits = integer_width(target).expect("integer conversion destination");
    let upper = round(2f64.powi(i32::from(bits - u16::from(signed))));
    let minimum = if signed { -upper } else { 0.0 };
    let before_minimum = round(minimum - 1.0);
    FloatIntegerRange { lower: before_minimum, lower_inclusive: before_minimum == minimum, upper }
}

impl NumericExpressionInfo {
    pub fn verify(&self) -> Result<(), String> {
        let mut previous = &self.evaluation_type;
        if !scalar(previous) || self.computation_type.as_ref().is_some_and(|t| !numeric(t)) {
            return Err("unresolved scalar/computation type".into());
        }
        for step in &self.conversions {
            if &step.source_type != previous {
                return Err("disconnected ordered conversion chain".into());
            }
            if conversion_kind(&step.source_type, &step.target_type) != Some(step.kind) {
                return Err("conversion kind does not match its semantic types".into());
            }
            previous = &step.target_type;
        }
        if previous != &self.result_type {
            return Err("conversion chain does not reach its result type".into());
        }
        if self.context_type.as_ref().is_some_and(|target| target != &self.result_type) {
            return Err("conversion chain does not satisfy its contextual destination".into());
        }
        Ok(())
    }
}

fn comparison(op: &Operator) -> bool {
    matches!(
        op,
        Operator::Equal
            | Operator::NotEqual
            | Operator::Less
            | Operator::LessEqual
            | Operator::Greater
            | Operator::GreaterEqual
    )
}

fn literal(expr: &Expression) -> bool {
    match expr {
        Expression::Literal(Literal::Int(_) | Literal::Float(_)) => true,
        Expression::Grouped(inner) | Expression::Unary { operator: Operator::Neg, expr: inner } => {
            literal(inner)
        },
        _ => false,
    }
}

pub(super) fn build(program: &TypedProgram) -> Vec<Option<NumericExpressionInfo>> {
    let mut planner =
        Planner { program, facts: vec![None; program.expression_count()], seen: HashSet::new() };
    super::walk_nodes(program.syntax(), &mut |expr| {
        if !planner.seen.contains(&program.expression_id(expr).unwrap()) {
            planner.plan(expr, program.expected_type_of(expr).cloned(), None);
        }
    });
    planner.facts
}
struct Planner<'a> {
    program: &'a TypedProgram,
    facts: Vec<Option<NumericExpressionInfo>>,
    seen: HashSet<ExpressionId>,
}
impl Planner<'_> {
    fn source_type(&self, expr: &Expression, hint: Option<&WaveType>) -> Option<WaveType> {
        match self.program.type_of(expr)? {
            HirExpressionType::Resolved(ty) if scalar(ty) => Some(ty.clone()),
            HirExpressionType::IntegerLiteral => Some(
                hint.filter(|t| numeric(t))
                    .cloned()
                    .unwrap_or(WaveType::Int(32)),
            ),
            HirExpressionType::FloatLiteral => Some(
                hint.filter(|t| matches!(t, WaveType::Float(_)))
                    .cloned()
                    .unwrap_or(WaveType::Float(32)),
            ),
            HirExpressionType::Null => hint.filter(|t| pointer(t)).cloned(),
            HirExpressionType::AddressedArrayLiteral => hint
                .filter(|ty| matches!(ty, WaveType::Pointer(inner) if matches!(inner.as_ref(), WaveType::Array(_, _))))
                .cloned(),
            _ => None,
        }
    }

    fn span(&self, expr: &Expression) -> SourceSpan {
        self.program
            .expression_id(expr)
            .and_then(|id| self.program.expression_span(id))
            .cloned()
            .unwrap_or_else(|| SourceSpan {
                file: "<unlocated AST>".into(),
                start: 0,
                end: 0,
                line: 0,
                column: 0,
                end_line: 0,
                end_column: 0,
                expansion: vec!["conversion of an unlocated frontend AST".into()],
                focus: None,
            })
    }

    fn append(
        &self,
        fact: &mut NumericExpressionInfo,
        target: WaveType,
        mode: ConversionMode,
        expr: &Expression,
    ) {
        if mode == ConversionMode::Implicit && target == fact.result_type {
            return;
        }
        if let Some(kind) = conversion_kind(&fact.result_type, &target) {
            fact.conversions.push(ConversionInfo {
                source_type: fact.result_type.clone(),
                target_type: target.clone(),
                kind,
                mode,
                span: self.span(expr),
            });
        }
        fact.result_type = target;
    }

    fn plan(
        &mut self,
        expr: &Expression,
        demand: Option<WaveType>,
        literal_hint: Option<WaveType>,
    ) -> Option<WaveType> {
        let id = self.program.expression_id(expr)?;
        self.seen.insert(id);
        let hint = literal_hint.as_ref().or(demand.as_ref());
        let source = self.source_type(expr, hint)?;
        let mut fact = NumericExpressionInfo {
            evaluation_type: source.clone(),
            computation_type: None,
            result_type: source,
            context_type: demand.clone().filter(scalar),
            conversions: Vec::new(),
            shift_count_type: None,
        };
        match expr {
            Expression::Cast { expr: inner, target_type } => {
                let hint = if matches!(self.program.type_of(inner), Some(HirExpressionType::Null)) {
                    Some(target_type.clone())
                } else {
                    literal(inner).then(|| {
                        if target_type == &WaveType::Bool {
                            WaveType::Int(1024)
                        } else if pointer(target_type) {
                            WaveType::Int(64)
                        } else {
                            target_type.clone()
                        }
                    })
                };
                let inner_type = self.plan(inner, None, hint)?;
                fact.evaluation_type = inner_type.clone();
                fact.result_type = inner_type;
                self.append(&mut fact, target_type.clone(), ConversionMode::Explicit, expr);
            },
            Expression::Grouped(inner) => {
                let inner_type = self.plan(inner, demand.clone(), literal_hint)?;
                fact.evaluation_type = inner_type.clone();
                fact.result_type = inner_type;
            },
            Expression::BinaryExpression { left, operator, right } => {
                let left_hint = self.program.expected_type_of(left);
                let right_hint = self.program.expected_type_of(right);
                let mut lt = self.source_type(left, left_hint);
                let mut rt = self.source_type(right, right_hint);
                if lt.is_none()
                    && matches!(self.program.type_of(left), Some(HirExpressionType::Null))
                {
                    lt = rt.clone().filter(pointer);
                }
                if rt.is_none()
                    && matches!(self.program.type_of(right), Some(HirExpressionType::Null))
                {
                    rt = lt.clone().filter(pointer);
                }
                // A floating literal borrows a concrete floating operand's
                // width, including when the literal is on the left of a comparison.
                if matches!(self.program.type_of(left), Some(HirExpressionType::FloatLiteral))
                    && matches!(rt, Some(WaveType::Float(_)))
                {
                    lt = rt.clone();
                }
                if matches!(self.program.type_of(right), Some(HirExpressionType::FloatLiteral))
                    && matches!(lt, Some(WaveType::Float(_)))
                {
                    rt = lt.clone();
                }
                if let (Some(lt), Some(rt)) = (lt, rt) {
                    if pointer(&lt) || pointer(&rt) {
                        self.plan(
                            left,
                            Some(if numeric(&lt) { WaveType::Int(64) } else { lt }),
                            None,
                        );
                        self.plan(
                            right,
                            Some(if numeric(&rt) { WaveType::Int(64) } else { rt }),
                            None,
                        );
                    } else if matches!(operator, Operator::ShiftLeft | Operator::ShiftRight) {
                        let left_type = self.plan(left, None, left_hint.cloned())?;
                        // An untyped count must not wrap at the default i32 width
                        // before the checked-shift range test.
                        let count_hint = literal(right).then_some(WaveType::Int(1024));
                        let right_type = self.plan(right, None, count_hint)?;
                        fact.evaluation_type = left_type.clone();
                        fact.result_type = left_type.clone();
                        fact.computation_type = Some(left_type);
                        fact.shift_count_type = Some(right_type);
                    } else if numeric(&lt) && numeric(&rt) {
                        if matches!(operator, Operator::LogicalAnd | Operator::LogicalOr) {
                            self.plan(left, None, None);
                            self.plan(right, None, None);
                            fact.evaluation_type = WaveType::Bool;
                            fact.result_type = WaveType::Bool;
                            fact.computation_type = Some(WaveType::Bool);
                        } else {
                            let computation = if !comparison(operator)
                                && !matches!(operator, Operator::ShiftLeft | Operator::ShiftRight)
                            {
                                fact.evaluation_type.clone()
                            } else if matches!(lt, WaveType::Float(_)) {
                                lt
                            } else if matches!(rt, WaveType::Float(_)) {
                                rt
                            } else if matches!(operator, Operator::ShiftLeft | Operator::ShiftRight)
                                || integer_width(&lt) >= integer_width(&rt)
                            {
                                lt
                            } else {
                                rt
                            };
                            self.plan(left, Some(computation.clone()), None);
                            self.plan(right, Some(computation.clone()), None);
                            fact.evaluation_type = if comparison(operator) {
                                WaveType::Bool
                            } else {
                                computation.clone()
                            };
                            fact.result_type = fact.evaluation_type.clone();
                            fact.computation_type = Some(computation);
                        }
                    }
                }
            },
            Expression::AssignOperation { target, value, operator } => {
                let ty = self.source_type(target, None)?;
                self.plan(target, None, None);
                self.plan(value, Some(ty.clone()), None);
                fact.computation_type = (!matches!(operator, AssignOperator::Assign)).then_some(ty);
            },
            Expression::Unary { operator, expr: inner } => {
                let input = if matches!(operator, Operator::Not | Operator::LogicalNot) {
                    self.plan(inner, None, None)?
                } else {
                    self.plan(inner, None, hint.cloned())?
                };
                fact.computation_type = Some(input.clone());
                fact.evaluation_type = if matches!(operator, Operator::Not | Operator::LogicalNot) {
                    WaveType::Bool
                } else {
                    input
                };
                fact.result_type = fact.evaluation_type.clone();
            },
            _ => {},
        }
        if let Some(target) = demand.filter(scalar) {
            self.append(&mut fact, target, ConversionMode::Implicit, expr);
        }
        let result = fact.result_type.clone();
        self.facts[id.index()] = Some(fact);
        Some(result)
    }
}

/// Verify operand/result contracts as well as each sequence's local continuity.
pub(super) fn verify_expression(
    program: &TypedProgram,
    expr: &Expression,
    fact: &NumericExpressionInfo,
) -> Result<(), String> {
    fact.verify()?;
    if fact.shift_count_type.is_some()
        != matches!(
            expr,
            Expression::BinaryExpression {
                operator: Operator::ShiftLeft | Operator::ShiftRight,
                ..
            }
        )
    {
        return Err("missing or extraneous shift count contract".into());
    }
    let child = |expr: &Expression| {
        program
            .numeric_expression_of(expr)
            .ok_or_else(|| "missing scalar operand facts".to_string())
    };
    match expr {
        Expression::Cast { expr: inner, target_type } => {
            if child(inner)?.result_type != fact.evaluation_type {
                return Err("cast input differs from operand result".into());
            }
            let Some(step) = fact.conversions.first() else {
                return Err("missing explicit cast conversion".into());
            };
            if step.mode != ConversionMode::Explicit || &step.target_type != target_type {
                return Err("cast lost its explicit conversion".into());
            }
        },
        Expression::Grouped(inner) => {
            if child(inner)?.result_type != fact.evaluation_type {
                return Err("group input differs from operand result".into());
            }
        },
        Expression::BinaryExpression { left, operator, right } => {
            for operand in [left.as_ref(), right.as_ref()] {
                if matches!(program.type_of(operand), Some(HirExpressionType::Null))
                    || program.type_of(operand).is_some_and(|ty| match ty {
                        HirExpressionType::Resolved(ty) => scalar(ty),
                        HirExpressionType::IntegerLiteral | HirExpressionType::FloatLiteral => true,
                        _ => false,
                    })
                {
                    child(operand)?;
                }
            }
            let operands =
                (program.numeric_expression_of(left), program.numeric_expression_of(right));
            if let (Some(left), Some(right)) = operands {
                if numeric(&left.result_type) && numeric(&right.result_type) {
                    let Some(computation) = &fact.computation_type else {
                        return Err("missing numeric computation type".into());
                    };
                    if matches!(operator, Operator::LogicalAnd | Operator::LogicalOr) {
                        if computation != &WaveType::Bool || fact.evaluation_type != WaveType::Bool
                        {
                            return Err("invalid logical computation".into());
                        }
                    } else if matches!(operator, Operator::ShiftLeft | Operator::ShiftRight) {
                        if &left.result_type != computation
                            || fact.evaluation_type != *computation
                            || fact.shift_count_type.as_ref() != Some(&right.result_type)
                            || !matches!(
                                right.result_type,
                                WaveType::Int(_)
                                    | WaveType::Uint(_)
                                    | WaveType::Byte
                                    | WaveType::Char
                            )
                        {
                            return Err("invalid checked shift contract".into());
                        }
                    } else {
                        if &left.result_type != computation || &right.result_type != computation {
                            return Err("operand conversion does not reach computation type".into());
                        }
                        let result =
                            if comparison(operator) { &WaveType::Bool } else { computation };
                        if &fact.evaluation_type != result {
                            return Err("binary result disagrees with computation".into());
                        }
                    }
                }
            }
        },
        Expression::Unary { operator, expr: inner } => {
            let input = &child(inner)?.result_type;
            if fact.computation_type.as_ref() != Some(input) {
                return Err("unary computation differs from operand result".into());
            }
            let result = if matches!(operator, Operator::Not | Operator::LogicalNot) {
                &WaveType::Bool
            } else {
                input
            };
            if &fact.evaluation_type != result {
                return Err("unary result disagrees with computation".into());
            }
        },
        Expression::AssignOperation { target, value, operator } => {
            let lhs = &child(target)?.result_type;
            if (!matches!(operator, AssignOperator::Assign)
                && fact.computation_type.as_ref() != Some(lhs))
                || child(value)?.result_type != *lhs
            {
                return Err("compound assignment operands differ from computation type".into());
            }
        },
        _ => {},
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::ast::ASTNode;
    fn program(source: &str) -> TypedProgram {
        let tokens = lexer::Lexer::new_with_file(source, "conversions.wave").tokenize().unwrap();
        TypedProgram::lower(parser::parse_syntax_with_spans(&tokens).unwrap()).unwrap()
    }

    fn initializer(p: &TypedProgram, index: usize) -> &Expression {
        let ASTNode::Function(f) = &p.syntax()[0] else { panic!() };
        let ASTNode::Variable(v) = &f.body[index] else { panic!() };
        v.initial_value.as_ref().unwrap()
    }
    #[test]
    fn checked_ranges_match_exact_integer_boundaries() {
        use utils::const_int::ConstInt;
        for source_bits in [32, 64] {
            for bits in [8, 16, 32, 64, 128, 256, 512, 1024] {
                for signed in [false, true] {
                    let target = if signed { WaveType::Int(bits) } else { WaveType::Uint(bits) };
                    let range = float_integer_range(&WaveType::Float(source_bits), &target);
                    let edge = 2f64.powi(i32::from(bits - u16::from(signed)));
                    let mut values = vec![
                        0.0,
                        -0.0,
                        -0.5,
                        -1.0,
                        f64::NAN,
                        f64::INFINITY,
                        f64::NEG_INFINITY,
                        edge,
                        -edge,
                        edge - 1.0,
                        -edge - 1.0,
                    ];
                    if source_bits == 32 {
                        let raw = (edge as f32).to_bits();
                        for delta in [raw.saturating_sub(1), raw, raw.saturating_add(1)] {
                            let n = f32::from_bits(delta) as f64;
                            values.extend([n, -n]);
                        }
                    } else {
                        for delta in [
                            edge.to_bits().saturating_sub(1),
                            edge.to_bits(),
                            edge.to_bits().saturating_add(1),
                        ] {
                            let n = f64::from_bits(delta);
                            values.extend([n, -n]);
                        }
                    }
                    for value in values {
                        let value = if source_bits == 32 { (value as f32) as f64 } else { value };
                        let exact = ConstInt::from_f64(value).is_some_and(|n| n.fits(bits, signed));
                        assert_eq!(
                            range.contains(value),
                            exact,
                            "f{source_bits} to {target:?}: {value}"
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn shifts_preserve_the_original_count_and_bool_casts_compare_with_zero() {
        let p =
            program("fun check(x: u8, n: u64) { var y: u8 = x << n; var b: bool = n as bool; }");
        p.verify_conversions().unwrap();
        let shift = p.numeric_expression_of(initializer(&p, 0)).unwrap();
        assert_eq!(shift.shift_count_type, Some(WaveType::Uint(64)));
        assert_eq!(shift.computation_type, Some(WaveType::Uint(8)));
        assert_eq!(
            p.numeric_expression_of(initializer(&p, 1)).unwrap().conversions[0].kind,
            ConversionKind::IntegerToBool
        );
    }

    #[test]
    fn ordered_casts_preserve_truncation_signedness_modes_and_spans() {
        let source = "fun check(x: i32) { var y: i64 = x as u8 as i32; var z: u32 = x as u32; var identity: i32 = x as i32; }";
        let p = program(source);
        p.verify_conversions().unwrap();
        let outer = initializer(&p, 0);
        let Expression::Cast { expr: inner, .. } = outer else { panic!() };
        let small = p.numeric_expression_of(inner).unwrap();
        assert_eq!(
            small.conversions.iter().map(|c| c.kind).collect::<Vec<_>>(),
            [ConversionKind::Truncate]
        );
        let outer = p.numeric_expression_of(outer).unwrap();
        assert_eq!(
            outer.conversions.iter().map(|c| c.kind).collect::<Vec<_>>(),
            [ConversionKind::ZeroExtend, ConversionKind::SignExtend]
        );
        assert_eq!(outer.conversions[0].mode, ConversionMode::Explicit);
        assert_eq!(outer.conversions[1].mode, ConversionMode::Implicit);
        for conversion in small.conversions.iter().chain(&outer.conversions) {
            assert_eq!(conversion.span.file, "conversions.wave");
            assert!(conversion.span.end > conversion.span.start);
            assert!(source[conversion.span.start..conversion.span.end].contains("as"));
        }
        assert_eq!(
            p.numeric_expression_of(initializer(&p, 1)).unwrap().conversions[0].kind,
            ConversionKind::ReinterpretInteger
        );
        assert_eq!(
            p.numeric_expression_of(initializer(&p, 2)).unwrap().conversions[0].kind,
            ConversionKind::Identity
        );
    }
    #[test]
    fn computation_precedes_destination_conversion_and_is_backend_neutral() {
        let p = program("fun check(s: i8, u: u32) { var f: f64 = 5 / 2; var sum: i64 = s + u; var cmp: bool = s < u; }");
        p.verify_conversions().unwrap();
        let f = p.numeric_expression_of(initializer(&p, 0)).unwrap();
        assert_eq!(f.computation_type, Some(WaveType::Int(32)));
        assert_eq!(f.conversions[0].kind, ConversionKind::SignedToFloat);
        let sum = initializer(&p, 1);
        let Expression::BinaryExpression { left, right, .. } = sum else { panic!() };
        assert_eq!(
            p.numeric_expression_of(sum).unwrap().computation_type,
            Some(WaveType::Uint(32))
        );
        assert_eq!(
            p.numeric_expression_of(left).unwrap().conversions[0].kind,
            ConversionKind::SignExtend
        );
        assert_eq!(p.numeric_expression_of(right).unwrap().result_type, WaveType::Uint(32));
    }
    #[test]
    fn verifier_rejects_missing_facts_disconnected_chains_wrong_kind_and_wrong_computation() {
        for mutation in 0..6 {
            let mut p =
                program("fun check(x: i32) { var y: i64 = x as u8 as i32; var z: i64 = x + x; }");
            let id = p.expression_id(initializer(&p, if mutation == 4 { 1 } else { 0 })).unwrap();
            let fact = p.numeric_expressions[id.index()].as_mut().unwrap();
            match mutation {
                0 => p.numeric_expressions[id.index()] = None,
                1 => fact.conversions[1].source_type = WaveType::Float(32),
                2 => fact.conversions[0].kind = ConversionKind::SignExtend,
                3 => fact.result_type = WaveType::Uint(16),
                4 => fact.computation_type = None,
                5 => {
                    fact.conversions.pop();
                    fact.result_type = WaveType::Int(32);
                },
                _ => unreachable!(),
            }
            assert!(p.verify_conversions().is_err(), "mutation {mutation} escaped verifier");
        }
    }
}
