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

//! Pre-link RISC-V floating-point ABI compatibility checks.
//!
//! ELF `e_flags` distinguish LP64, LP64F, and LP64D objects. Every RISC-V member
//! discovered in direct objects or archives must agree with the effective Wave
//! target ABI; inputs for other architectures are left to the linker.

use super::elf::{inspect_link_inputs, LinkInputInspectionError};
use std::fmt;

const EM_RISCV: u16 = 243;
const EF_RISCV_FLOAT_ABI_MASK: u32 = 0x6;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RiscvFloatAbi {
    Lp64,
    Lp64f,
    Lp64d,
}

impl RiscvFloatAbi {
    pub fn from_target_abi(value: &str) -> Option<Self> {
        match value.to_ascii_lowercase().as_str() {
            "lp64" => Some(Self::Lp64),
            "lp64f" => Some(Self::Lp64f),
            "lp64d" => Some(Self::Lp64d),
            _ => None,
        }
    }

    fn from_elf_flags(flags: u32) -> Option<Self> {
        match flags & EF_RISCV_FLOAT_ABI_MASK {
            0x0 => Some(Self::Lp64),
            0x2 => Some(Self::Lp64f),
            0x4 => Some(Self::Lp64d),
            _ => None,
        }
    }
}

impl fmt::Display for RiscvFloatAbi {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::Lp64 => "LP64",
            Self::Lp64f => "LP64F",
            Self::Lp64d => "LP64D",
        })
    }
}

#[derive(Debug)]
pub enum AbiValidationError {
    Inspection(LinkInputInspectionError),
    Unsupported { input: String },
    Mismatch { target: RiscvFloatAbi, input: String, input_abi: RiscvFloatAbi },
}

impl fmt::Display for AbiValidationError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Inspection(error) => error.fmt(formatter),
            Self::Unsupported { input } => write!(
                formatter,
                "invalid RISC-V linker input '{}': unsupported floating-point ABI flag",
                input
            ),
            Self::Mismatch {
                target,
                input,
                input_abi,
            } => write!(
                formatter,
                "RISC-V floating-point ABI mismatch before linking\ntarget ABI: {}\ninput: {}\ninput ABI: {}",
                target, input, input_abi
            ),
        }
    }
}

impl std::error::Error for AbiValidationError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Inspection(error) => Some(error),
            Self::Unsupported { .. } | Self::Mismatch { .. } => None,
        }
    }
}

pub fn validate_riscv_link_inputs(
    target_abi: RiscvFloatAbi,
    inputs: &[String],
) -> Result<(), AbiValidationError> {
    // Inspect every archive member rather than trusting the archive filename:
    // one incompatible member is sufficient to make the final link invalid.
    for metadata in inspect_link_inputs(inputs).map_err(AbiValidationError::Inspection)? {
        if metadata.machine != EM_RISCV {
            continue;
        }
        let input_abi = RiscvFloatAbi::from_elf_flags(metadata.flags)
            .ok_or_else(|| AbiValidationError::Unsupported { input: metadata.input.clone() })?;
        if input_abi != target_abi {
            return Err(AbiValidationError::Mismatch {
                target: target_abi,
                input: metadata.input,
                input_abi,
            });
        }
    }
    Ok(())
}
