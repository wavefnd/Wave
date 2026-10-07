// SPDX-License-Identifier: MPL-2.0
//! Checked numeric lowering. Bounds and operand types are supplied by Typed HIR.
use super::*;
use hir::conversions::{
    float_integer_range, integer_width, unsigned, ConversionInfo, ConversionKind,
};
use parser::ast::{Literal, Operator};
use utils::const_int::ConstInt;

impl Lowerer<'_> {
    fn cast_value(&mut self, op: CastOp, input: Value, ty: Type) -> Value {
        let value = self.value(ty.clone());
        self.emit(Instruction::Cast {
            dst: value.id,
            op,
            src_ty: input.ty,
            dst_ty: ty,
            src: input.id,
        });
        value
    }

    fn trap_unless(&mut self, valid: Value, reason: &str) -> Result<()> {
        let no = self.constant(Type::Bool, ConstValue::Bool(false));
        let invalid = self.icmp(ICmpPred::Eq, valid, no)?;
        self.emit(Instruction::TrapIf { cond: invalid.id, reason: reason.into() });
        Ok(())
    }

    pub(super) fn checked_float_to_integer(
        &mut self,
        input: Value,
        conversion: &ConversionInfo,
    ) -> Result<Value> {
        let range = float_integer_range(&conversion.source_type, &conversion.target_type);
        // These bounds already account for truncation toward zero and rounding
        // at the source float width. In particular, (-1, 0) converts to unsigned 0.
        for (pred, bound) in [
            (if range.lower_inclusive { FCmpPred::Oge } else { FCmpPred::Ogt }, range.lower),
            (FCmpPred::Olt, range.upper),
            (FCmpPred::Ogt, f64::NEG_INFINITY),
        ] {
            let bound = self.literal(&Literal::Float(bound), &conversion.source_type)?;
            let valid = self.fcmp(pred, input.clone(), bound)?;
            self.trap_unless(valid, "float-to-integer conversion out of range")?;
        }
        let op = match conversion.kind {
            ConversionKind::FloatToSigned => CastOp::FToI_S,
            ConversionKind::FloatToUnsigned => CastOp::FToI_U,
            _ => return Err(ice("non-float conversion passed to checked float lowering")),
        };
        Ok(self.cast_value(op, input, scalar(&conversion.target_type)?))
    }

    pub(super) fn checked_shift(
        &mut self,
        lhs: Value,
        operator: &Operator,
        rhs: Value,
        computation: &WaveType,
        count_type: &WaveType,
    ) -> Result<Value> {
        if lhs.ty != scalar(computation)? || rhs.ty != scalar(count_type)? {
            return Err(ice("shift operands disagree with HIR types"));
        }
        let width = integer_width(computation).ok_or_else(|| ice("non-integer shift LHS"))?;
        let count_width = integer_width(count_type).ok_or_else(|| ice("non-integer shift RHS"))?;
        // Sign-extend before interpreting the count as unsigned. Negative counts
        // then exceed the bound, and a wide count is never truncated before testing.
        let check_width = count_width.max(16);
        let check_type = WaveType::Uint(check_width);
        let ty = scalar(&check_type)?;
        let count = if rhs.ty == ty {
            rhs
        } else {
            let op = if count_width == check_width {
                CastOp::Bitcast
            } else if unsigned(count_type) {
                CastOp::ZExt
            } else {
                CastOp::SExt
            };
            self.cast_value(op, rhs, ty)
        };
        let bound = self.literal(&Literal::Int(width.to_string()), &check_type)?;
        let valid = self.icmp(ICmpPred::Ult, count.clone(), bound)?;
        self.trap_unless(valid, "shift count out of range")?;
        let count = if count.ty == lhs.ty {
            count
        } else {
            let op = match check_width.cmp(&width) {
                std::cmp::Ordering::Less => CastOp::ZExt,
                std::cmp::Ordering::Equal => CastOp::Bitcast,
                std::cmp::Ordering::Greater => CastOp::Trunc,
            };
            self.cast_value(op, count, lhs.ty.clone())
        };
        self.shift_value(lhs, operator, count, computation)
    }

    pub(super) fn wide_constant_shift(
        &mut self,
        lhs: Value,
        operator: &Operator,
        right: &Expression,
        computation: &WaveType,
    ) -> Result<Value> {
        // HIR deliberately keeps an untyped count literal at i1024. Check that
        // value with Utils before selecting its IR representation. This does not
        // enable runtime integers wider than Whale's supported scalar types.
        let n = if let Some(hir::ConstantValue::Int(n)) = self.program.constant_value_of(right) {
            n.clone()
        } else {
            // Short-circuited operands need not have a cached constant. Retain
            // their trap in the unreachable RHS block rather than rejecting it.
            let mut expression = right;
            let mut negative = false;
            loop {
                let fact = self
                    .program
                    .numeric_expression_of(expression)
                    .ok_or_else(|| ice("missing shift literal facts"))?;
                if !fact.conversions.is_empty() {
                    return Err(unsupported("runtime shift counts wider than 128 bits"));
                }
                match expression.unspanned() {
                    Expression::Grouped(inner) => expression = inner,
                    Expression::Unary { operator: Operator::Neg, expr } => {
                        negative = !negative;
                        expression = expr;
                    },
                    Expression::Literal(Literal::Int(raw)) => {
                        let token = lexer::number::IntegerLiteral::parse(raw)
                            .ok_or_else(|| ice("invalid shift literal"))?;
                        let n = ConstInt::from_digits(&token.digits, token.radix)
                            .ok_or_else(|| ice("invalid shift literal digits"))?;
                        break if negative ^ token.negative { n.negated() } else { n };
                    },
                    _ => return Err(unsupported("runtime shift counts wider than 128 bits")),
                }
            }
        };
        let width = integer_width(computation).ok_or_else(|| ice("non-integer shift LHS"))?;
        let valid = !n.is_negative() && n < ConstInt::from_u64(u64::from(width));
        let valid_value = self.constant(Type::Bool, ConstValue::Bool(valid));
        self.trap_unless(valid_value, "shift count out of range")?;
        // An invalid constant can occur in a short-circuited branch. The trap
        // precedes a harmless placeholder; never truncate the invalid count.
        let count =
            if valid { n.to_usize().ok_or_else(|| ice("bounded count does not fit"))? } else { 0 };
        let count = self.literal(&Literal::Int(count.to_string()), computation)?;
        self.shift_value(lhs, operator, count, computation)
    }

    fn shift_value(
        &mut self,
        lhs: Value,
        operator: &Operator,
        count: Value,
        computation: &WaveType,
    ) -> Result<Value> {
        let op = match operator {
            Operator::ShiftLeft => BinOp::Shl,
            Operator::ShiftRight if unsigned(computation) => BinOp::LShr,
            Operator::ShiftRight => BinOp::AShr,
            _ => return Err(ice("shift facts attached to a non-shift operation")),
        };
        self.bin(op, scalar(computation)?, lhs, count)
    }
}
