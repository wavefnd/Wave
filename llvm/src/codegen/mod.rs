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

//! Public assembly point for LLVM code-generation services.
//!
//! New lowering belongs in a focused submodule. Re-export only the small set of
//! entry points required by the compiler driver and expression/statement layers.

pub mod abi_c;
pub mod address;
pub mod arch;
pub mod consts;
pub mod format;
pub mod ir;
pub mod legacy;
pub mod plan;
pub mod platform;
pub mod target;
pub mod types;
pub mod variants;

pub use address::{generate_address_and_type_ir, generate_address_ir};
pub use format::{wave_format_to_c, wave_format_to_scanf};
pub use ir::{emit_codegen_file, generate_ir, CodegenFileKind};
pub use types::{wave_type_to_llvm_type, VariableInfo};

pub use legacy::{create_alloc, get_llvm_type};

pub(crate) mod number;
