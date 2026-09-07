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

use inkwell::context::Context;
use inkwell::types::{BasicTypeEnum, StringRadix, StructType};
use inkwell::values::{BasicValue, BasicValueEnum};

use parser::ast::{Expression, Literal, WaveType};
use parser::hir::TypedProgram;
use std::collections::HashMap;
use std::fmt;

use super::types::{wave_type_to_llvm_type, TypeFlavor};

#[derive(Debug, Clone)]
pub enum ConstEvalError {
    UnknownIdentifier(String),
    TypeMismatch {
        expected: String,
        got: String,
        note: String,
    },
    InvalidLiteral(String),
    Unsupported(String),
}

impl fmt::Display for ConstEvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConstEvalError::UnknownIdentifier(n) => write!(f, "unknown const identifier `{}`", n),
            ConstEvalError::TypeMismatch {
                expected,
                got,
                note,
            } => {
                write!(
                    f,
                    "type mismatch (expected {}, got {}): {}",
                    expected, got, note
                )
            }
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

fn cast_const_int_to_int<'ctx>(
    iv: inkwell::values::IntValue<'ctx>,
    int_ty: inkwell::types::IntType<'ctx>,
) -> Result<BasicValueEnum<'ctx>, ConstEvalError> {
    let src_bw = iv.get_type().get_bit_width();
    let dst_bw = int_ty.get_bit_width();

    if src_bw == dst_bw {
        return Ok(iv.as_basic_value_enum());
    }

    if src_bw > dst_bw {
        return Ok(iv.const_truncate(int_ty).as_basic_value_enum());
    }

    if let Some(sext) = iv.get_sign_extended_constant() {
        return Ok(int_ty.const_int(sext as u64, true).as_basic_value_enum());
    }

    Err(ConstEvalError::Unsupported(format!(
        "cannot sign-extend {}-bit const integer to {}-bit at compile time",
        src_bw, dst_bw
    )))
}

fn const_from_expected<'ctx>(
    context: &'ctx Context,
    expected: BasicTypeEnum<'ctx>,
    expr: &Expression,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
    const_env: &HashMap<String, BasicValueEnum<'ctx>>,
    program: Option<&TypedProgram>,
) -> Result<BasicValueEnum<'ctx>, ConstEvalError> {
    if let Some(construction) = program.and_then(|program| program.variant_construction_of(expr)) {
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
            }
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
            }
        };
        if args.len() != construction.payload_types.len() {
            return Err(ConstEvalError::Unsupported(format!(
                "variant constructor payload count changed after semantic validation: expected {}, got {}",
                construction.payload_types.len(),
                args.len()
            )));
        }

        let case_index = construction.discriminant + 1;
        let payload_ty = variant_ty
            .get_field_type_at_index(case_index)
            .ok_or_else(|| {
                ConstEvalError::Unsupported(format!(
                    "variant case '{}' has no LLVM payload slot",
                    construction.case_name
                ))
            })?
            .into_struct_type();
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
            let field_type = payload_ty
                .get_field_type_at_index(index as u32)
                .ok_or_else(|| {
                    ConstEvalError::Unsupported(format!(
                        "variant case '{}' has no payload field {}",
                        construction.case_name, index
                    ))
                })?;
            payload_values.push(const_from_expected(
                context,
                field_type,
                argument,
                struct_types,
                struct_field_indices,
                const_env,
                program,
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
        fields[case_index as usize] = payload_ty
            .const_named_struct(&payload_values)
            .as_basic_value_enum();
        return Ok(variant_ty.const_named_struct(&fields).as_basic_value_enum());
    }

    match expr {
        Expression::Grouped(inner) => {
            return const_from_expected(
                context,
                expected,
                inner,
                struct_types,
                struct_field_indices,
                const_env,
                program,
            );
        }

        Expression::Variable(name) => {
            let v = match const_env.get(name) {
                Some(v) => *v,
                None => return Err(ConstEvalError::UnknownIdentifier(name.clone())),
            };

            if v.get_type() != expected {
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
        }

        Expression::Null => match expected {
            BasicTypeEnum::PointerType(pt) => Ok(pt.const_null().as_basic_value_enum()),
            _ => Err(ConstEvalError::TypeMismatch {
                expected: type_name(expected),
                got: "null".to_string(),
                note: "null can only be used where a pointer is expected".to_string(),
            }),
        },

        Expression::Cast {
            expr: inner,
            target_type,
        } => {
            let cast_ty =
                wave_type_to_llvm_type(context, target_type, struct_types, TypeFlavor::AbiC);
            if cast_ty != expected {
                return Err(ConstEvalError::TypeMismatch {
                    expected: type_name(expected),
                    got: type_name(cast_ty),
                    note: format!(
                        "cast target type {:?} does not match declaration type",
                        target_type
                    ),
                });
            }

            match (inner.as_ref(), expected) {
                (Expression::Literal(Literal::Int(s)), BasicTypeEnum::PointerType(ptr_ty)) => {
                    let (neg, radix, digits) = parse_signed_and_radix(s);
                    let mut iv = context
                        .i64_type()
                        .const_int_from_string(&digits, radix)
                        .ok_or_else(|| ConstEvalError::InvalidLiteral(s.clone()))?;
                    if neg {
                        iv = iv.const_neg();
                    }
                    Ok(iv.const_to_pointer(ptr_ty).as_basic_value_enum())
                }

                (Expression::Variable(name), BasicTypeEnum::PointerType(ptr_ty)) => {
                    let src = *const_env
                        .get(name)
                        .ok_or_else(|| ConstEvalError::UnknownIdentifier(name.clone()))?;
                    match src {
                        BasicValueEnum::IntValue(iv) => {
                            Ok(iv.const_to_pointer(ptr_ty).as_basic_value_enum())
                        }
                        BasicValueEnum::PointerValue(pv) => {
                            Ok(pv.const_cast(ptr_ty).as_basic_value_enum())
                        }
                        other => Err(ConstEvalError::TypeMismatch {
                            expected: type_name(expected),
                            got: value_type_name(other),
                            note: format!("cannot cast const `{}` to pointer", name),
                        }),
                    }
                }

                (Expression::Variable(name), BasicTypeEnum::IntType(int_ty)) => {
                    let src = *const_env
                        .get(name)
                        .ok_or_else(|| ConstEvalError::UnknownIdentifier(name.clone()))?;
                    match src {
                        BasicValueEnum::IntValue(iv) => cast_const_int_to_int(iv, int_ty),
                        BasicValueEnum::PointerValue(pv) => {
                            Ok(pv.const_to_int(int_ty).as_basic_value_enum())
                        }
                        other => Err(ConstEvalError::TypeMismatch {
                            expected: type_name(expected),
                            got: value_type_name(other),
                            note: format!("cannot cast const `{}` to integer", name),
                        }),
                    }
                }

                _ => const_from_expected(
                    context,
                    expected,
                    inner,
                    struct_types,
                    struct_field_indices,
                    const_env,
                    program,
                ),
            }
        }

        // --- ints ---
        Expression::Literal(Literal::Int(s)) => match expected {
            BasicTypeEnum::FloatType(float_ty) => {
                let value = lexer::number::IntegerLiteral::parse(s)
                    .and_then(|value| value.to_f64())
                    .ok_or_else(|| ConstEvalError::InvalidLiteral(s.clone()))?;
                Ok(float_ty.const_float(value).as_basic_value_enum())
            }
            BasicTypeEnum::IntType(int_ty) => {
                let (neg, radix, digits) = parse_signed_and_radix(s);
                let mut iv = int_ty
                    .const_int_from_string(&digits, radix)
                    .ok_or_else(|| ConstEvalError::InvalidLiteral(s.clone()))?;

                if neg {
                    iv = iv.const_neg();
                }
                Ok(iv.as_basic_value_enum())
            }
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
            }
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
            }
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
                }
            };

            let field_count = st.count_fields() as usize;

            let struct_name = if !name.is_empty() {
                name.as_str()
            } else {
                st.get_name()
                    .and_then(|c| c.to_str().ok())
                    .map(strip_struct_prefix)
                    .unwrap_or("")
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
                        fty,
                        vexpr,
                        struct_types,
                        struct_field_indices,
                        const_env,
                        program,
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
                        fty,
                        vexpr,
                        struct_types,
                        struct_field_indices,
                        const_env,
                        program,
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
        }

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
                            elem_ty,
                            e,
                            struct_types,
                            struct_field_indices,
                            const_env,
                            program,
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
                                }
                            }
                        }
                        Ok(int_ty.const_array(&vs).as_basic_value_enum())
                    }

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
                                }
                            }
                        }
                        Ok(float_ty.const_array(&vs).as_basic_value_enum())
                    }

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
                                }
                            }
                        }
                        Ok(ptr_ty.const_array(&vs).as_basic_value_enum())
                    }

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
                                }
                            }
                        }
                        Ok(st_ty.const_array(&vs).as_basic_value_enum())
                    }

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
                                }
                            }
                        }
                        Ok(inner_at.const_array(&vs).as_basic_value_enum())
                    }

                    other => Err(ConstEvalError::Unsupported(format!(
                        "Unsupported const array element type: {:?}",
                        other
                    ))),
                }
            }
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
    ty: &WaveType,
    expr: &Expression,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
    const_env: &HashMap<String, BasicValueEnum<'ctx>>,
    program: Option<&TypedProgram>,
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
        expected,
        expr,
        struct_types,
        struct_field_indices,
        const_env,
        program,
    )
}
