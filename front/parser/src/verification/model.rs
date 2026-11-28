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

//! Types shared by immutable collection and semantic rules.
use crate::ast::{Expression, Mutability, WaveType};

#[derive(Clone, Debug)]
pub(super) struct Binding {
    pub(super) literal_address: bool,
    pub(super) mutability: Mutability,
    pub(super) ty: WaveType,
}

#[derive(Clone, Debug)]
pub(super) struct FunctionType {
    pub(super) defaults: Vec<Option<Expression>>,
    pub(super) params: Vec<WaveType>,
    pub(super) required_params: usize,
    pub(super) return_type: WaveType,
    pub(super) generic_params: Vec<String>,
    pub(super) variadic: bool,
}

#[derive(Clone, Debug)]
pub(super) struct VariantType {
    pub(super) generic_params: Vec<String>,
    pub(super) cases: Vec<(String, Vec<WaveType>)>,
}

#[derive(Clone, Debug)]
pub(super) enum ExpressionType {
    // Literal and null states stay distinct until an expected type supplies the
    // width, signedness, element type, or pointer pointee required to commit.
    Known(WaveType),
    IntLiteral(String),
    FloatLiteral,
    Null,
    ArrayLiteral(Vec<ExpressionType>),
    AddressedArrayLiteral(Vec<ExpressionType>),
    Unknown,
}

pub(super) fn display_expression_type(ty: &ExpressionType) -> String {
    match ty {
        ExpressionType::Known(ty) => display_wave_type(ty),
        ExpressionType::IntLiteral(_) => "integer literal".to_string(),
        ExpressionType::FloatLiteral => "float literal".to_string(),
        ExpressionType::Null => "null".to_string(),
        ExpressionType::ArrayLiteral(_) => "array literal".to_string(),
        ExpressionType::AddressedArrayLiteral(_) => "addressed array literal".to_string(),
        ExpressionType::Unknown => "unknown".to_string(),
    }
}

pub(super) fn display_wave_type(ty: &WaveType) -> String {
    match ty {
        WaveType::Isz => "isz".to_string(),
        WaveType::Usz => "usz".to_string(),
        WaveType::Int(bits) => format!("i{}", bits),
        WaveType::Uint(bits) => format!("u{}", bits),
        WaveType::Float(bits) => format!("f{}", bits),
        WaveType::Bool => "bool".to_string(),
        WaveType::Char => "char".to_string(),
        WaveType::Byte => "byte".to_string(),
        WaveType::String => "str".to_string(),
        WaveType::Future(inner) => format!("Future<{}>", display_wave_type(inner)),
        WaveType::Pointer(inner) => format!("ptr<{}>", display_wave_type(inner)),
        WaveType::Array(inner, size) => format!("array<{}, {}>", display_wave_type(inner), size),
        WaveType::Void => "void".to_string(),
        WaveType::Never => "!".to_string(),
        WaveType::Struct(name) => name.clone(),
        WaveType::Variant(name) => name.clone(),
    }
}

impl From<WaveType> for ExpressionType {
    fn from(value: WaveType) -> Self {
        Self::Known(value)
    }
}
