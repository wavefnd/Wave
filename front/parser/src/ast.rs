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

//! Source-level syntax tree shared by parsing, semantic analysis, and codegen.
//!
//! Nodes describe Wave concepts, not LLVM storage or ABI decisions. Generic
//! monomorphization rewrites this representation before backend entry, so new
//! node forms must be handled by both semantic passes and that rewrite where
//! they may contain types or expressions.

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WaveType {
    /// Target-sized integers remain symbolic until the target-resolution pass.
    Isz,
    Usz,
    Int(u16),
    Uint(u16),
    Float(u16),
    Bool,
    Char,
    Byte,
    String,
    Pointer(Box<WaveType>),
    Array(Box<WaveType>, u32),
    Void,
    /// A function that cannot return to its caller (return position only).
    Never,
    Struct(String),
    Variant(String),
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Located {
        value: Box<ASTNode>,
        span: error::SourceSpan,
    },
    Function(FunctionNode),
    ExternFunction(ExternFunctionNode),
    Program(ParameterNode),
    Statement(StatementNode),
    Variable(VariableNode),
    Expression(Expression),
    Struct(StructNode),
    ProtoImpl(ProtoImplNode),
    TypeAlias(TypeAliasNode),
    Enum(EnumNode),
    Variant(VariantNode),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Visibility {
    #[default]
    Private,
    Public,
}

#[derive(Debug, Clone)]
pub struct TypeAliasNode {
    pub name: String,
    pub target: WaveType,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct EnumNode {
    pub name: String,
    pub repr_type: WaveType,
    pub variants: Vec<EnumVariantNode>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct EnumVariantNode {
    pub span: Option<error::SourceSpan>,
    pub name: String,
    pub explicit_value: Option<String>,
}

#[derive(Debug, Clone)]
pub struct VariantNode {
    pub name: String,
    pub generic_params: Vec<String>,
    pub cases: Vec<VariantCaseNode>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct VariantCaseNode {
    pub span: Option<error::SourceSpan>,
    pub name: String,
    pub payload_types: Vec<WaveType>,
}

#[derive(Debug, Clone)]
pub struct FunctionNode {
    pub span: Option<error::SourceSpan>,
    pub name: String,
    pub generic_params: Vec<String>,
    pub parameters: Vec<ParameterNode>,
    pub return_type: Option<WaveType>,
    pub return_type_span: Option<error::SourceSpan>,
    pub body: Vec<ASTNode>,
    pub export: Option<ExportAttribute>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct ExportAttribute {
    pub abi: String,
    pub symbol: Option<String>,
}

#[derive(Debug, Clone)]
pub struct StructNode {
    pub name: String,
    pub generic_params: Vec<String>,
    pub fields: Vec<(String, WaveType)>,
    pub field_spans: Vec<Option<error::SourceSpan>>,
    pub methods: Vec<FunctionNode>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct ProtoImplNode {
    pub target: String,
    pub methods: Vec<FunctionNode>,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub params: Vec<(String, WaveType)>,
    pub return_type: WaveType,
}

#[derive(Debug, Clone)]
pub struct ParameterNode {
    pub span: Option<error::SourceSpan>,
    pub name: String,
    pub param_type: WaveType,
    pub initial_value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct ExternFunctionNode {
    pub name: String,
    pub abi: String,
    pub symbol: Option<String>,
    pub params: Vec<(String, WaveType)>,
    pub variadic: bool,
    pub return_type: WaveType,
}

#[derive(Debug, Clone)]
pub enum FormatPart {
    Literal(String),
    Placeholder,
}

#[derive(Debug, Clone)]
pub enum IncDecKind {
    PreInc,
    PreDec,
    PostInc,
    PostDec,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Located {
        value: Box<Expression>,
        span: error::SourceSpan,
    },
    StructLiteral {
        name: String,
        fields: Vec<(String, Expression)>,
    },
    FunctionCall {
        name: String,
        type_args: Vec<WaveType>,
        args: Vec<Expression>,
    },
    MethodCall {
        object: Box<Expression>,
        name: String,
        args: Vec<Expression>,
    },
    Null,
    Literal(Literal),
    Variable(String),
    Deref(Box<Expression>),
    AddressOf(Box<Expression>),
    BinaryExpression {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },
    IndexAccess {
        target: Box<Expression>,
        index: Box<Expression>,
    },
    ArrayLiteral(Vec<Expression>),
    Grouped(Box<Expression>),
    AssignOperation {
        target: Box<Expression>,
        operator: AssignOperator,
        value: Box<Expression>,
    },
    Assignment {
        target: Box<Expression>,
        value: Box<Expression>,
    },
    AsmBlock {
        instructions: Vec<String>,
        inputs: Vec<(String, Expression)>,
        outputs: Vec<(String, Expression)>,
        clobbers: Vec<String>,
    },
    FieldAccess {
        object: Box<Expression>,
        field: String,
    },
    Unary {
        operator: Operator,
        expr: Box<Expression>,
    },
    Cast {
        expr: Box<Expression>,
        target_type: WaveType,
    },
    IncDec {
        kind: IncDecKind,
        target: Box<Expression>,
    },
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(String),
    Float(f64),
    String(String),
    Bool(bool),
    Char(char),
    Byte(u8),
}

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    GreaterEqual,
    LessEqual,
    Greater,
    Less,
    Equal,
    NotEqual,
    LogicalAnd,
    BitwiseAnd,
    LogicalOr,
    BitwiseOr,
    Assign,
    ShiftLeft,  // <<
    ShiftRight, // >>
    BitwiseXor,
    LogicalNot,
    BitwiseNot,
    Not,
    Neg,
}

#[derive(Debug, Clone)]
pub enum AssignOperator {
    Assign,    // =
    AddAssign, // +=
    SubAssign, // -=
    MulAssign, // *=
    DivAssign, // /=
    RemAssign, // %=
}

#[derive(Debug, Clone)]
pub enum MatchPattern {
    Located {
        value: Box<MatchPattern>,
        span: error::SourceSpan,
    },
    Int(String),
    Ident(String),
    Binding(String),
    Wildcard,
    Variant {
        variant_type: String,
        case_name: String,
        payloads: Vec<MatchPattern>,
    },
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub span: Option<error::SourceSpan>,
    pub pattern: MatchPattern,
    pub body: Vec<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum StatementNode {
    Print(String),
    PrintFormat {
        format: String,
        args: Vec<Expression>,
    },
    Println(String),
    PrintlnFormat {
        format: String,
        args: Vec<Expression>,
    },
    Input {
        format: String,
        args: Vec<Expression>,
    },
    Variable(String),
    If {
        condition: Expression,
        body: Vec<ASTNode>,
        else_if_blocks: Option<Box<Vec<(Expression, Vec<ASTNode>)>>>,
        else_block: Option<Box<Vec<ASTNode>>>,
    },
    For {
        initialization: Box<ASTNode>,
        condition: Expression,
        increment: Expression,
        body: Vec<ASTNode>,
    },
    While {
        condition: Expression,
        body: Vec<ASTNode>,
    },
    Match {
        value: Expression,
        arms: Vec<MatchArm>,
    },
    Import(ImportNode),
    Assign {
        variable: String,
        value: Expression,
    },
    AsmBlock {
        instructions: Vec<String>,
        inputs: Vec<(String, Expression)>,
        outputs: Vec<(String, Expression)>,
        clobbers: Vec<String>,
    },
    Break,
    Continue,
    Return(Option<Expression>),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportNode {
    pub path: String,
    pub alias: Option<String>,
    pub selections: Vec<String>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mutability {
    Static,
    Var,
    Const,
}

#[derive(Debug, Clone)]
pub struct VariableNode {
    pub name: String,
    pub type_name: WaveType,
    pub initial_value: Option<Expression>,
    pub mutability: Mutability,
    pub visibility: Visibility,
}

#[derive(Clone)]
pub struct VariableInfo {
    pub name: String,
    pub mutable: bool,
    pub ty: WaveType,
}

impl Expression {
    pub fn as_identifier(&self) -> Option<&str> {
        match self.unspanned() {
            Expression::Variable(name) => Some(name.as_str()),
            Expression::AddressOf(inner) => {
                if let Expression::Variable(name) = &**inner {
                    Some(name.as_str())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn get_wave_type(&self, variables: &HashMap<String, VariableInfo>) -> WaveType {
        match self.unspanned() {
            Expression::Variable(name) => variables
                .get(name)
                .unwrap_or_else(|| panic!("Variable '{}' not found", name))
                .ty
                .clone(),
            Expression::Literal(Literal::Int(_)) => {
                panic!("integer literal type is context-dependent and must be resolved by type checking")
            }
            Expression::Literal(Literal::Float(_)) => WaveType::Float(32),
            Expression::Literal(Literal::String(_)) => WaveType::String,
            Expression::MethodCall { .. } => {
                panic!("nested method call type inference not supported yet")
            }
            Expression::Unary { operator, expr } => {
                let t = expr.get_wave_type(variables);
                match operator {
                    Operator::Neg => match &t {
                        WaveType::Int(_) | WaveType::Uint(_) | WaveType::Float(_) => t,
                        _ => panic!("unary '-' not allowed for type {:?}", t),
                    },
                    Operator::Not | Operator::LogicalNot => WaveType::Bool,
                    Operator::BitwiseNot => t,
                    _ => panic!("unary op type inference not supported: {:?}", operator),
                }
            }
            Expression::Cast { target_type, .. } => target_type.clone(),
            _ => panic!("get_wave_type not implemented for {:?}", self),
        }
    }
}

impl ASTNode {
    pub fn unspanned(&self) -> &Self {
        match self {
            Self::Located { value, .. } => value.unspanned(),
            other => other,
        }
    }
    pub fn into_unspanned(self) -> Self {
        match self {
            Self::Located { value, .. } => value.into_unspanned(),
            other => other,
        }
    }
    pub fn span(&self) -> Option<&error::SourceSpan> {
        match self {
            Self::Located { span, .. } => Some(span),
            _ => None,
        }
    }
    pub fn with_span(self, span: Option<error::SourceSpan>) -> Self {
        if self.span() == span.as_ref() {
            return self;
        }
        match span {
            Some(span) => Self::Located {
                value: Box::new(self),
                span,
            },
            None => self,
        }
    }
}

impl Expression {
    pub fn unspanned(&self) -> &Self {
        match self {
            Self::Located { value, .. } => value.unspanned(),
            other => other,
        }
    }
    pub fn into_unspanned(self) -> Self {
        match self {
            Self::Located { value, .. } => value.into_unspanned(),
            other => other,
        }
    }
    pub fn span(&self) -> Option<&error::SourceSpan> {
        match self {
            Self::Located { span, .. } => Some(span),
            _ => None,
        }
    }
    pub fn with_span(self, span: Option<error::SourceSpan>) -> Self {
        if self.span() == span.as_ref() {
            return self;
        }
        match span {
            Some(span) => Self::Located {
                value: Box::new(self),
                span,
            },
            None => self,
        }
    }
}

impl MatchPattern {
    pub fn unspanned(&self) -> &Self {
        match self {
            Self::Located { value, .. } => value.unspanned(),
            other => other,
        }
    }
    pub fn into_unspanned(self) -> Self {
        match self {
            Self::Located { value, .. } => value.into_unspanned(),
            other => other,
        }
    }
    pub fn span(&self) -> Option<&error::SourceSpan> {
        match self {
            Self::Located { span, .. } => Some(span),
            _ => None,
        }
    }
    pub fn with_span(self, span: Option<error::SourceSpan>) -> Self {
        if self.span() == span.as_ref() {
            return self;
        }
        match span {
            Some(span) => Self::Located {
                value: Box::new(self),
                span,
            },
            None => self,
        }
    }
}

impl Expression {
    pub fn binary(left: Expression, operator: Operator, right: Expression) -> Self {
        let span = left
            .span()
            .zip(right.span())
            .map(|(first, last)| first.through(last));
        Self::BinaryExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
        .with_span(span)
    }
}
