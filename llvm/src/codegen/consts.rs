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

//! Compile-time evaluation of Wave constants into LLVM constant values.
//!
//! Evaluation is intentionally separate from runtime expression lowering:
//! globals require LLVM constants and cannot emit instructions. Unknown names
//! are reported distinctly so module construction can resolve forward constant
//! references in dependency rounds.

use hir::HirExpressionType;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::targets::TargetData;
use inkwell::types::{BasicTypeEnum, StringRadix, StructType};
use inkwell::values::{BasicValue, BasicValueEnum};

use hir::TypedProgram;
use parser::ast::{Expression, Literal, WaveType};
use std::collections::HashMap;
use std::fmt;

use super::types::{wave_type_to_llvm_type, TypeFlavor};

#[derive(Debug, Clone)]
pub enum ConstEvalError {
    UnknownIdentifier(String),
    TypeMismatch { expected: String, got: String, note: String },
    InvalidLiteral(String),
    Unsupported(String),
}

impl fmt::Display for ConstEvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConstEvalError::UnknownIdentifier(n) => write!(f, "unknown const identifier `{}`", n),
            ConstEvalError::TypeMismatch { expected, got, note } => {
                write!(f, "type mismatch (expected {}, got {}): {}", expected, got, note)
            },
            ConstEvalError::InvalidLiteral(s) => write!(f, "invalid literal: {}", s),
            ConstEvalError::Unsupported(s) => write!(f, "unsupported const expression: {}", s),
        }
    }
}

fn type_name<'ctx>(t: BasicTypeEnum<'ctx>) -> String {
    format!("{:?}", t)
}

fn value_type_name<'ctx>(v: BasicValueEnum<'ctx>) -> String {
    format!("{:?}", v.get_type())
}

fn parse_signed_and_radix(s: &str) -> (bool, StringRadix, String) {
    super::number::parse_integer(s).unwrap_or((false, StringRadix::Decimal, String::new()))
}

fn is_zero_like(s: &str) -> bool {
    lexer::number::IntegerLiteral::parse(s).is_some_and(|n| n.is_zero())
}

fn strip_struct_prefix(raw: &str) -> &str {
    raw.strip_prefix("struct.").unwrap_or(raw)
}

fn const_from_expected<'ctx>(
    context: &'ctx Context,
    module: &'ctx Module<'ctx>,
    expected: BasicTypeEnum<'ctx>,
    expr: &Expression,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
    const_env: &HashMap<String, BasicValueEnum<'ctx>>,
    program: &TypedProgram,
    target_data: &TargetData,
) -> Result<BasicValueEnum<'ctx>, ConstEvalError> {
    if let Some(value) = program.constant_value_of(expr) {
        match (value, expected) {
            (hir::ConstantValue::Int(n), BasicTypeEnum::IntType(ty)) => {
                return Ok(ty
                    .const_int_arbitrary_precision(&n.to_le_words(ty.get_bit_width() as u16))
                    .into());
            },
            (hir::ConstantValue::Float(n), BasicTypeEnum::FloatType(ty)) => {
                return Ok(ty.const_float(*n).into());
            },
            _ => {
                return Err(ConstEvalError::Unsupported(
                    "ICE: folded constant differs from HIR storage type".into(),
                ))
            },
        }
    }
    let Some(fact) = program.numeric_expression_of(expr) else {
        return const_raw(
            context,
            module,
            expected,
            expr,
            struct_types,
            struct_field_indices,
            const_env,
            program,
            target_data,
        );
    };
    let native =
        wave_type_to_llvm_type(context, &fact.evaluation_type, struct_types, TypeFlavor::Value);
    let mut value = match expr {
        Expression::Cast { expr: inner, .. } | Expression::Grouped(inner) => const_from_expected(
            context,
            module,
            native,
            inner,
            struct_types,
            struct_field_indices,
            const_env,
            program,
            target_data,
        )?,
        _ => const_raw(
            context,
            module,
            native,
            expr,
            struct_types,
            struct_field_indices,
            const_env,
            program,
            target_data,
        )?,
    };
    let scratch = context.create_module("constant.conversions");
    let function = scratch.add_function("fold", context.void_type().fn_type(&[], false), None);
    let block = context.append_basic_block(function, "entry");
    let builder = context.create_builder();
    builder.position_at_end(block);
    // Global bool values arrive from byte storage; semantic conversions use i1.
    if fact.evaluation_type == WaveType::Bool && value.get_type() != native {
        value = builder
            .build_int_truncate(value.into_int_value(), context.bool_type(), "bool.value")
            .unwrap()
            .into();
    }
    for conversion in &fact.conversions {
        value =
            super::conversions::apply(context, &builder, &scratch, struct_types, value, conversion);
    }
    if value.get_type() != expected {
        // Storage adaptation only. Numeric language conversions must be in HIR.
        if fact.result_type == WaveType::Bool && expected == context.i8_type().into() {
            value = builder
                .build_int_z_extend(value.into_int_value(), context.i8_type(), "bool.storage")
                .unwrap()
                .into();
        } else {
            return Err(ConstEvalError::Unsupported(format!(
                "ICE: constant result differs from HIR: {:?} vs {:?}",
                value.get_type(),
                expected
            )));
        }
    }
    if value.as_instruction_value().is_some() {
        return Err(ConstEvalError::Unsupported("conversion did not fold to a constant".into()));
    }
    Ok(value)
}

fn const_raw<'ctx>(
    context: &'ctx Context,
    module: &'ctx Module<'ctx>,
    expected: BasicTypeEnum<'ctx>,
    expr: &Expression,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
    const_env: &HashMap<String, BasicValueEnum<'ctx>>,
    program: &TypedProgram,
    target_data: &TargetData,
) -> Result<BasicValueEnum<'ctx>, ConstEvalError> {
    if let Some(construction) = program.variant_construction_of(expr) {
        let variant_ty = match expected {
            BasicTypeEnum::StructType(variant_ty) => variant_ty,
            _ => {
                return Err(ConstEvalError::TypeMismatch {
                    expected: type_name(expected),
                    got: format!(
                        "variant constructor '{}::{}'",
                        match &construction.variant_type {
                            WaveType::Variant(name) => name,
                            _ => "<invalid>",
                        },
                        construction.case_name
                    ),
                    note: "variant constructor used where a non-variant constant is expected"
                        .to_string(),
                });
            },
        };
        let args: &[Expression] = match expr {
            Expression::FunctionCall { args, .. } => args,
            Expression::Variable(_) if construction.payload_types.is_empty() => &[],
            _ => {
                return Err(ConstEvalError::Unsupported(format!(
                    "variant constructor '{}::{}' has an unsupported syntax form",
                    match &construction.variant_type {
                        WaveType::Variant(name) => name,
                        _ => "<invalid>",
                    },
                    construction.case_name
                )));
            },
        };
        if args.len() != construction.payload_types.len() {
            return Err(ConstEvalError::Unsupported(format!(
                "variant constructor payload count changed after semantic validation: expected {}, got {}",
                construction.payload_types.len(),
                args.len()
            )));
        }

        let payload_ty =
            super::variants::payload_type(context, &construction.payload_types, struct_types);
        if payload_ty.count_fields() as usize != args.len() {
            return Err(ConstEvalError::Unsupported(format!(
                "variant case '{}' LLVM payload layout expects {} fields, got {}",
                construction.case_name,
                payload_ty.count_fields(),
                args.len()
            )));
        }

        let mut payload_values = Vec::with_capacity(args.len());
        for (index, argument) in args.iter().enumerate() {
            let field_type = payload_ty.get_field_type_at_index(index as u32).ok_or_else(|| {
                ConstEvalError::Unsupported(format!(
                    "variant case '{}' has no payload field {}",
                    construction.case_name, index
                ))
            })?;
            payload_values.push(const_from_expected(
                context,
                module,
                field_type,
                argument,
                struct_types,
                struct_field_indices,
                const_env,
                program,
                target_data,
            )?);
        }

        let mut fields = (0..variant_ty.count_fields())
            .map(|index| {
                variant_ty
                    .get_field_type_at_index(index)
                    .expect("variant field count changed")
                    .const_zero()
            })
            .collect::<Vec<_>>();
        fields[0] = context
            .i32_type()
            .const_int(construction.discriminant as u64, false)
            .as_basic_value_enum();
        let payload_value = payload_ty.const_named_struct(&payload_values);
        let storage_ty = variant_ty.get_field_type_at_index(2).unwrap().into_array_type();
        let mut bytes = vec![0; storage_ty.len() as usize];
        super::variants::constant_storage_bytes(
            context,
            target_data,
            payload_value.into(),
            &mut bytes,
        )?;
        fields[2] = context.const_string(&bytes, false).into();
        return Ok(variant_ty.const_named_struct(&fields).as_basic_value_enum());
    }

    match expr {
        Expression::Grouped(inner) => {
            return const_from_expected(
                context,
                module,
                expected,
                inner,
                struct_types,
                struct_field_indices,
                const_env,
                program,
                target_data,
            );
        },

        Expression::Variable(name) => {
            let v = match const_env.get(name) {
                Some(v) => *v,
                None => return Err(ConstEvalError::UnknownIdentifier(name.clone())),
            };

            if v.get_type() != expected
                && !matches!(
                    program.type_of(expr),
                    Some(HirExpressionType::Resolved(WaveType::Bool))
                )
            {
                return Err(ConstEvalError::TypeMismatch {
                    expected: type_name(expected),
                    got: value_type_name(v),
                    note: format!(
                        "identifier `{}` resolved to a const of different LLVM type",
                        name
                    ),
                });
            }

            Ok(v)
        },

        Expression::Null => match expected {
            BasicTypeEnum::PointerType(pt) => Ok(pt.const_null().as_basic_value_enum()),
            _ => Err(ConstEvalError::TypeMismatch {
                expected: type_name(expected),
                got: "null".to_string(),
                note: "null can only be used where a pointer is expected".to_string(),
            }),
        },

        Expression::Cast { .. } => {
            Err(ConstEvalError::Unsupported("ICE: cast missing HIR conversion facts".into()))
        },
        Expression::Literal(Literal::String(bytes)) => {
            let value = context.const_string(bytes, true);
            let global = module.add_global(value.get_type(), None, "$const$str");
            global.set_initializer(&value);
            global.set_constant(true);
            global.set_linkage(Linkage::Private);
            Ok(global.as_pointer_value().into())
        },
        Expression::Literal(Literal::Char(value)) => {
            Ok(context.i8_type().const_int(*value as u64, false).into())
        },
        Expression::Literal(Literal::Byte(value)) => {
            Ok(context.i8_type().const_int(u64::from(*value), false).into())
        },
        Expression::Literal(Literal::Bool(value)) => match expected {
            BasicTypeEnum::IntType(ty) => {
                Ok(ty.const_int(u64::from(*value), false).as_basic_value_enum())
            },
            _ => Err(ConstEvalError::TypeMismatch {
                expected: type_name(expected),
                got: "bool".into(),
                note: "boolean constant requires integer storage".into(),
            }),
        },
        // --- ints ---
        Expression::Literal(Literal::Int(s)) => match expected {
            BasicTypeEnum::FloatType(float_ty) => {
                let bits = if float_ty == context.f32_type() { 32 } else { 64 };
                let value = hir::integer_literal_float(s, bits)
                    .ok_or_else(|| ConstEvalError::InvalidLiteral(s.clone()))?;
                Ok(float_ty.const_float(value).as_basic_value_enum())
            },
            BasicTypeEnum::IntType(int_ty) => {
                let (neg, radix, digits) = parse_signed_and_radix(s);
                let mut iv = int_ty
                    .const_int_from_string(&digits, radix)
                    .ok_or_else(|| ConstEvalError::InvalidLiteral(s.clone()))?;

                if neg {
                    iv = iv.const_neg();
                }
                Ok(iv.as_basic_value_enum())
            },
            BasicTypeEnum::PointerType(ptr_ty) => {
                if is_zero_like(s) {
                    Ok(ptr_ty.const_null().as_basic_value_enum())
                } else {
                    Err(ConstEvalError::TypeMismatch {
                        expected: type_name(expected),
                        got: format!("int({})", s),
                        note: "only 0 can be used as a const null pointer literal".to_string(),
                    })
                }
            },
            _ => Err(ConstEvalError::TypeMismatch {
                expected: type_name(expected),
                got: format!("int({})", s),
                note: "const int literal not compatible with expected type".to_string(),
            }),
        },

        // --- floats ---
        Expression::Literal(Literal::Float(fv)) => match expected {
            BasicTypeEnum::FloatType(float_ty) => {
                Ok(float_ty.const_float(*fv).as_basic_value_enum())
            },
            _ => Err(ConstEvalError::TypeMismatch {
                expected: type_name(expected),
                got: "float".to_string(),
                note: "const float literal not compatible with expected type".to_string(),
            }),
        },

        // --- struct literal ---
        Expression::StructLiteral { name, fields } => {
            let st = match expected {
                BasicTypeEnum::StructType(st) => st,
                _ => {
                    return Err(ConstEvalError::TypeMismatch {
                        expected: type_name(expected),
                        got: "struct-literal".to_string(),
                        note: format!("StructLiteral '{}' used where non-struct expected", name),
                    })
                },
            };

            let field_count = st.count_fields() as usize;

            let struct_name = if !name.is_empty() {
                name.as_str()
            } else {
                st.get_name().and_then(|c| c.to_str().ok()).map(strip_struct_prefix).unwrap_or("")
            };

            let positional = fields.iter().all(|(n, _)| n.is_empty());
            let mut slots: Vec<Option<BasicValueEnum<'ctx>>> = vec![None; field_count];

            if positional {
                if fields.len() != field_count {
                    return Err(ConstEvalError::Unsupported(format!(
                        "StructLiteral '{}' positional init expects {} fields, got {}",
                        struct_name,
                        field_count,
                        fields.len()
                    )));
                }

                for (i, (_, vexpr)) in fields.iter().enumerate() {
                    let fty = st.get_field_type_at_index(i as u32).ok_or_else(|| {
                        ConstEvalError::Unsupported(format!(
                            "Struct '{}' has no field index {}",
                            struct_name, i
                        ))
                    })?;

                    let cv = const_from_expected(
                        context,
                        module,
                        fty,
                        vexpr,
                        struct_types,
                        struct_field_indices,
                        const_env,
                        program,
                        target_data,
                    )?;
                    slots[i] = Some(cv);
                }
            } else {
                let idx_map = struct_field_indices.get(struct_name).ok_or_else(|| {
                    ConstEvalError::Unsupported(format!(
                        "Struct '{}' field map not found",
                        struct_name
                    ))
                })?;

                for (fname, vexpr) in fields {
                    let idx = *idx_map.get(fname).ok_or_else(|| {
                        ConstEvalError::Unsupported(format!(
                            "Field '{}' not found in struct '{}'",
                            fname, struct_name
                        ))
                    })? as usize;

                    let fty = st.get_field_type_at_index(idx as u32).ok_or_else(|| {
                        ConstEvalError::Unsupported(format!(
                            "Struct '{}' has no field index {}",
                            struct_name, idx
                        ))
                    })?;

                    let cv = const_from_expected(
                        context,
                        module,
                        fty,
                        vexpr,
                        struct_types,
                        struct_field_indices,
                        const_env,
                        program,
                        target_data,
                    )?;
                    slots[idx] = Some(cv);
                }
            }

            let mut ordered: Vec<BasicValueEnum<'ctx>> = Vec::with_capacity(field_count);
            for i in 0..field_count {
                let fty = st.get_field_type_at_index(i as u32).unwrap();
                ordered.push(slots[i].unwrap_or_else(|| fty.const_zero()));
            }

            Ok(st.const_named_struct(&ordered).as_basic_value_enum())
        },

        // --- array literal ---
        Expression::ArrayLiteral(elems) => match expected {
            BasicTypeEnum::ArrayType(at) => {
                let len = at.len() as usize;
                if elems.len() != len {
                    return Err(ConstEvalError::Unsupported(format!(
                        "Array literal length mismatch: expected {}, got {}",
                        len,
                        elems.len()
                    )));
                }

                let elem_ty = at.get_element_type();

                let elem_vals: Vec<BasicValueEnum<'ctx>> = elems
                    .iter()
                    .map(|e| {
                        const_from_expected(
                            context,
                            module,
                            elem_ty,
                            e,
                            struct_types,
                            struct_field_indices,
                            const_env,
                            program,
                            target_data,
                        )
                    })
                    .collect::<Result<_, _>>()?;

                match elem_ty {
                    BasicTypeEnum::IntType(int_ty) => {
                        let mut vs = Vec::with_capacity(len);
                        for v in elem_vals {
                            match v {
                                BasicValueEnum::IntValue(iv) => vs.push(iv),
                                other => {
                                    return Err(ConstEvalError::TypeMismatch {
                                        expected: type_name(elem_ty),
                                        got: value_type_name(other),
                                        note: "array element expected int".to_string(),
                                    })
                                },
                            }
                        }
                        Ok(int_ty.const_array(&vs).as_basic_value_enum())
                    },

                    BasicTypeEnum::FloatType(float_ty) => {
                        let mut vs = Vec::with_capacity(len);
                        for v in elem_vals {
                            match v {
                                BasicValueEnum::FloatValue(fv) => vs.push(fv),
                                other => {
                                    return Err(ConstEvalError::TypeMismatch {
                                        expected: type_name(elem_ty),
                                        got: value_type_name(other),
                                        note: "array element expected float".to_string(),
                                    })
                                },
                            }
                        }
                        Ok(float_ty.const_array(&vs).as_basic_value_enum())
                    },

                    BasicTypeEnum::PointerType(ptr_ty) => {
                        let mut vs = Vec::with_capacity(len);
                        for v in elem_vals {
                            match v {
                                BasicValueEnum::PointerValue(pv) => vs.push(pv),
                                other => {
                                    return Err(ConstEvalError::TypeMismatch {
                                        expected: type_name(elem_ty),
                                        got: value_type_name(other),
                                        note: "array element expected pointer".to_string(),
                                    })
                                },
                            }
                        }
                        Ok(ptr_ty.const_array(&vs).as_basic_value_enum())
                    },

                    BasicTypeEnum::StructType(st_ty) => {
                        let mut vs = Vec::with_capacity(len);
                        for v in elem_vals {
                            match v {
                                BasicValueEnum::StructValue(sv) => vs.push(sv),
                                other => {
                                    return Err(ConstEvalError::TypeMismatch {
                                        expected: type_name(elem_ty),
                                        got: value_type_name(other),
                                        note: "array element expected struct".to_string(),
                                    })
                                },
                            }
                        }
                        Ok(st_ty.const_array(&vs).as_basic_value_enum())
                    },

                    BasicTypeEnum::ArrayType(inner_at) => {
                        let mut vs = Vec::with_capacity(len);
                        for v in elem_vals {
                            match v {
                                BasicValueEnum::ArrayValue(av) => vs.push(av),
                                other => {
                                    return Err(ConstEvalError::TypeMismatch {
                                        expected: type_name(elem_ty),
                                        got: value_type_name(other),
                                        note: "array element expected array".to_string(),
                                    })
                                },
                            }
                        }
                        Ok(inner_at.const_array(&vs).as_basic_value_enum())
                    },

                    other => Err(ConstEvalError::Unsupported(format!(
                        "Unsupported const array element type: {:?}",
                        other
                    ))),
                }
            },
            _ => Err(ConstEvalError::TypeMismatch {
                expected: type_name(expected),
                got: "array-literal".to_string(),
                note: "Array literal used where non-array expected".to_string(),
            }),
        },

        _ => Err(ConstEvalError::Unsupported(format!(
            "Constant expression must be a literal/struct/array/identifier, got {:?}",
            expr
        ))),
    }
}

pub(super) fn create_llvm_const_value<'ctx>(
    context: &'ctx Context,
    module: &'ctx Module<'ctx>,
    ty: &WaveType,
    expr: &Expression,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
    const_env: &HashMap<String, BasicValueEnum<'ctx>>,
    program: &TypedProgram,
    target_data: &TargetData,
) -> Result<BasicValueEnum<'ctx>, ConstEvalError> {
    if matches!(expr, Expression::Null) && !matches!(ty, WaveType::Pointer(_)) {
        return Err(ConstEvalError::TypeMismatch {
            expected: format!("{:?}", ty),
            got: "null".to_string(),
            note: "null can only be assigned to ptr<T>".to_string(),
        });
    }

    let expected = wave_type_to_llvm_type(context, ty, struct_types, TypeFlavor::AbiC);
    const_from_expected(
        context,
        module,
        expected,
        expr,
        struct_types,
        struct_field_indices,
        const_env,
        program,
        target_data,
    )
}
