// SPDX-License-Identifier: MPL-2.0
use super::*;
use hir::conversions::{integer_width, unsigned, ConversionInfo, ConversionKind};
use parser::ast::{Literal, Operator};
use utils::const_int::ConstInt;

impl Lowerer<'_> {
    pub(super) fn effect(&mut self, e: &Expression) -> Result<()> {
        if let Expression::FunctionCall { name, args, .. } = e.unspanned() {
            self.call(name, args).map(|_| ()).map_err(|mut error| {
                if error.span.is_none() {
                    error.span = self
                        .program
                        .expression_id(e)
                        .and_then(|id| self.program.expression_span(id))
                        .cloned()
                        .or_else(|| e.span().cloned());
                }
                error
            })
        } else {
            self.expr(e).map(|_| ())
        }
    }

    pub(super) fn expr(&mut self, e: &Expression) -> Result<Value> {
        self.expression(e).map_err(|mut error| {
            if error.span.is_none() {
                error.span = self
                    .program
                    .expression_id(e)
                    .and_then(|id| self.program.expression_span(id))
                    .cloned()
                    .or_else(|| e.span().cloned());
            }
            error
        })
    }

    pub(super) fn unary_value(
        &mut self,
        operator: &Operator,
        operand: Value,
        fact: &hir::conversions::NumericExpressionInfo,
    ) -> Result<Value> {
        let ty = scalar(&fact.evaluation_type)?;
        Ok(match operator {
            Operator::Neg if matches!(ty, Type::F32 | Type::F64) => {
                // Multiplication by -1 preserves both signs of zero.
                let minus_one = self.literal(&Literal::Float(-1.0), &fact.evaluation_type)?;
                self.bin(BinOp::FMul, ty.clone(), operand, minus_one)?
            },
            Operator::Neg => {
                let zero = self.literal(&Literal::Int("0".into()), &fact.evaluation_type)?;
                self.bin(BinOp::Sub, ty.clone(), zero, operand)?
            },
            Operator::LogicalNot | Operator::Not if ty == Type::Bool => {
                let input = fact
                    .computation_type
                    .as_ref()
                    .ok_or_else(|| ice("missing unary computation type"))?;
                if *input == WaveType::Bool {
                    let zero = self.constant(Type::Bool, ConstValue::Bool(false));
                    self.icmp(ICmpPred::Eq, operand, zero)?
                } else if matches!(input, WaveType::Float(_)) {
                    let zero = self.literal(&Literal::Float(0.0), input)?;
                    self.fcmp(FCmpPred::Oeq, operand, zero)?
                } else {
                    let zero = self.literal(&Literal::Int("0".into()), input)?;
                    self.icmp(ICmpPred::Eq, operand, zero)?
                }
            },
            Operator::BitwiseNot => {
                let value = self.value(ty.clone());
                self.emit(Instruction::Not { dst: value.id, ty: ty.clone(), src: operand.id });
                value
            },
            _ => return Err(unsupported("this unary operation")),
        })
    }

    pub(super) fn constant(&mut self, ty: Type, literal: ConstValue) -> Value {
        let value = self.value(ty.clone());
        self.emit(Instruction::Const { dst: value.id, ty, value: literal });
        value
    }

    pub(super) fn literal(&mut self, literal: &Literal, ty: &WaveType) -> Result<Value> {
        let ir_ty = scalar(ty)?;
        let constant = match literal {
            Literal::Bool(v) if *ty == WaveType::Bool => ConstValue::Bool(*v),
            Literal::Float(v) if matches!(ty, WaveType::Float(_)) => {
                let WaveType::Float(bits) = ty else { unreachable!() };
                ConstValue::F(FloatBits::from_f64(*bits, *v).map_err(ice)?)
            },
            Literal::Int(raw) if matches!(ty, WaveType::Float(_)) => {
                let WaveType::Float(bits) = ty else { unreachable!() };
                let n = hir::integer_literal_float(raw, *bits)
                    .ok_or_else(|| ice("invalid typed floating literal"))?;
                ConstValue::F(FloatBits::from_f64(*bits, n).map_err(ice)?)
            },
            Literal::Int(raw) => {
                let token = lexer::number::IntegerLiteral::parse(raw)
                    .ok_or_else(|| ice("invalid integer token"))?;
                let mut n = ConstInt::from_digits(&token.digits, token.radix)
                    .ok_or_else(|| ice("invalid integer digits"))?;
                if token.negative {
                    n = n.negated();
                }
                let bits = integer_width(ty)
                    .ok_or_else(|| ice("integer literal with non-integer HIR type"))?;
                let words = n.normalize(bits, !unsigned(ty)).to_le_words(128);
                let raw = u128::from(words[0]) | (u128::from(words[1]) << 64);
                if unsigned(ty) {
                    ConstValue::U(raw)
                } else {
                    ConstValue::I(raw as i128)
                }
            },
            _ => return Err(unsupported("this literal")),
        };
        Ok(self.constant(ir_ty, constant))
    }

    pub(super) fn convert(&mut self, input: Value, conversion: &ConversionInfo) -> Result<Value> {
        let src_ty = scalar(&conversion.source_type)?;
        let dst_ty = scalar(&conversion.target_type)?;
        if input.ty != src_ty {
            return Err(ice("conversion source type mismatch"));
        }
        let op = match conversion.kind {
            ConversionKind::Identity => return Ok(input),
            ConversionKind::ReinterpretInteger => CastOp::Bitcast,
            ConversionKind::SignExtend => CastOp::SExt,
            ConversionKind::ZeroExtend => CastOp::ZExt,
            ConversionKind::Truncate => CastOp::Trunc,
            ConversionKind::FloatExtend => CastOp::FExt,
            ConversionKind::FloatTruncate => CastOp::FTrunc,
            ConversionKind::SignedToFloat => CastOp::IToF_S,
            ConversionKind::UnsignedToFloat => CastOp::IToF_U,
            ConversionKind::IntegerToBool => {
                let zero = self.literal(&Literal::Int("0".into()), &conversion.source_type)?;
                return self.icmp(ICmpPred::Ne, input, zero);
            },
            ConversionKind::FloatToBool => {
                let zero = self.literal(&Literal::Float(0.0), &conversion.source_type)?;
                return self.fcmp(FCmpPred::Une, input, zero);
            },
            ConversionKind::FloatToSigned | ConversionKind::FloatToUnsigned => {
                return self.checked_float_to_integer(input, conversion);
            },
            _ => return Err(unsupported(format!("conversion {:?}", conversion.kind))),
        };
        let value = self.value(dst_ty.clone());
        self.emit(Instruction::Cast { dst: value.id, op, dst_ty, src_ty, src: input.id });
        Ok(value)
    }

    fn call(&mut self, name: &str, args: &[Expression]) -> Result<Option<Value>> {
        let mut values = Vec::with_capacity(args.len());
        for arg in args {
            values.push(self.expr(arg)?);
        }
        self.call_values(name, values)
    }

    pub(super) fn call_values(&mut self, name: &str, args: Vec<Value>) -> Result<Option<Value>> {
        let (id, signature) = self
            .functions
            .get(name)
            .cloned()
            .ok_or_else(|| unsupported(format!("external or intrinsic call `{name}`")))?;
        if args.len() != signature.params.len() {
            return Err(unsupported("omitted default arguments"));
        }
        if args.iter().zip(&signature.params).any(|(v, t)| &v.ty != t) {
            return Err(ice("call argument disagrees with HIR signature"));
        }
        let value = (signature.ret != Type::Void).then(|| self.value(signature.ret.clone()));
        self.emit(Instruction::Call {
            convention: CallingConvention::Whale,
            dst: value.as_ref().map(|v| v.id),
            ret_ty: signature.ret,
            callee: Callee::Direct(id),
            args: args.into_iter().map(|v| v.id).collect(),
        });
        Ok(value)
    }

    pub(super) fn bin(&mut self, op: BinOp, ty: Type, lhs: Value, rhs: Value) -> Result<Value> {
        if lhs.ty != ty || rhs.ty != ty {
            return Err(ice("arithmetic operands disagree with HIR computation type"));
        }
        let value = self.value(ty.clone());
        self.emit(Instruction::Bin { dst: value.id, op, ty, lhs: lhs.id, rhs: rhs.id });
        Ok(value)
    }

    pub(super) fn icmp(&mut self, pred: ICmpPred, lhs: Value, rhs: Value) -> Result<Value> {
        if lhs.ty != rhs.ty {
            return Err(ice("comparison operand type mismatch"));
        }
        let value = self.value(Type::Bool);
        self.emit(Instruction::ICmp { dst: value.id, pred, ty: lhs.ty, lhs: lhs.id, rhs: rhs.id });
        Ok(value)
    }

    pub(super) fn fcmp(&mut self, pred: FCmpPred, lhs: Value, rhs: Value) -> Result<Value> {
        if lhs.ty != rhs.ty {
            return Err(ice("comparison operand type mismatch"));
        }
        let value = self.value(Type::Bool);
        self.emit(Instruction::FCmp { dst: value.id, pred, ty: lhs.ty, lhs: lhs.id, rhs: rhs.id });
        Ok(value)
    }

    pub(super) fn binary_values(
        &mut self,
        lhs: Value,
        op: &Operator,
        rhs: Value,
        computation: &WaveType,
        result: &Type,
    ) -> Result<Value> {
        use Operator::*;
        // These operations require additional Wave-specific checked lowering.
        if matches!(op, ShiftLeft | ShiftRight | Divide | Remainder) {
            return Err(unsupported(format!("operation {op:?}")));
        }
        let ty = scalar(computation)?;
        if lhs.ty != ty || rhs.ty != ty {
            return Err(ice("binary operands disagree with HIR computation type"));
        }
        let float = matches!(ty, Type::F32 | Type::F64);
        if *result == Type::Bool {
            if float {
                let pred = match op {
                    Equal => FCmpPred::Oeq,
                    NotEqual => FCmpPred::Une,
                    Less => FCmpPred::Olt,
                    LessEqual => FCmpPred::Ole,
                    Greater => FCmpPred::Ogt,
                    GreaterEqual => FCmpPred::Oge,
                    _ => return Err(unsupported(format!("comparison {op:?}"))),
                };
                return self.fcmp(pred, lhs, rhs);
            }
            let pred = match (op, unsigned(computation)) {
                (Equal, _) => ICmpPred::Eq,
                (NotEqual, _) => ICmpPred::Ne,
                (Less, true) => ICmpPred::Ult,
                (LessEqual, true) => ICmpPred::Ule,
                (Greater, true) => ICmpPred::Ugt,
                (GreaterEqual, true) => ICmpPred::Uge,
                (Less, false) => ICmpPred::Slt,
                (LessEqual, false) => ICmpPred::Sle,
                (Greater, false) => ICmpPred::Sgt,
                (GreaterEqual, false) => ICmpPred::Sge,
                _ => return Err(unsupported(format!("comparison {op:?}"))),
            };
            return self.icmp(pred, lhs, rhs);
        }
        let op = match (op, float) {
            (Add, false) => BinOp::Add,
            (Subtract, false) => BinOp::Sub,
            (Multiply, false) => BinOp::Mul,
            (Add, true) => BinOp::FAdd,
            (Subtract, true) => BinOp::FSub,
            (Multiply, true) => BinOp::FMul,
            (BitwiseAnd, false) => BinOp::And,
            (BitwiseOr, false) => BinOp::Or,
            (BitwiseXor, false) => BinOp::Xor,
            _ => return Err(unsupported(format!("operation {op:?}"))),
        };
        self.bin(op, ty, lhs, rhs)
    }
}
