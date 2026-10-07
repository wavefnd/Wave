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

//! Reading an indexed array or pointer element.
//!
//! Address computation is shared with lvalue lowering. Struct elements are
//! loaded by value; arrays retain their address for the existing array handling.

use super::ExprGenEnv;
use crate::codegen::generate_address_and_type_ir;
use inkwell::values::{BasicValue, BasicValueEnum};
use parser::ast::Expression;

pub(crate) fn gen<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    expression: &Expression,
    target: &Expression,
    index: &Expression,
) -> BasicValueEnum<'ctx> {
    if let Some(value) = super::const_projection::try_gen_index_access(env, target, index) {
        return value;
    }

    let (addr, elem_ty) = generate_address_and_type_ir(env, expression);

    if elem_ty.is_array_type() {
        return addr.as_basic_value_enum();
    }

    env.builder.build_load(elem_ty, addr, "load_index_elem").unwrap().as_basic_value_enum()
}
