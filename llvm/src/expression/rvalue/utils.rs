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

//! Small value-normalization helpers shared by expression lowerers.

use inkwell::builder::Builder;
use inkwell::values::IntValue;
use inkwell::IntPredicate;

pub(crate) fn to_bool<'ctx>(builder: &Builder<'ctx>, v: IntValue<'ctx>) -> IntValue<'ctx> {
    if v.get_type().get_bit_width() == 1 {
        return v;
    }

    let zero = v.get_type().const_zero();
    builder.build_int_compare(IntPredicate::NE, v, zero, "tobool").unwrap()
}
