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

//! Pre-link LoongArch floating-point ABI compatibility checks.

use super::elf::{inspect_link_inputs, LinkInputInspectionError};
use std::fmt;

const EM_LOONGARCH: u16 = 258;
const EF_LOONGARCH_ABI_MODIFIER_MASK: u32 = 0x7;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LoongArchFloatAbi {
    Lp64s,
    Lp64f,
    Lp64d,
}

impl LoongArchFloatAbi {
    pub fn from_target_abi(value: &str) -> Option<Self> {
        match value.to_ascii_lowercase().as_str() {
            "lp64s" => Some(Self::Lp64s),
            "lp64f" => Some(Self::Lp64f),
            "lp64d" => Some(Self::Lp64d),
            _ => None,
        }
    }

    fn from_elf_flags(flags: u32) -> Option<Self> {
        match flags & EF_LOONGARCH_ABI_MODIFIER_MASK {
            0x1 => Some(Self::Lp64s),
            0x2 => Some(Self::Lp64f),
            0x3 => Some(Self::Lp64d),
            _ => None,
        }
    }
}

impl fmt::Display for LoongArchFloatAbi {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::Lp64s => "LP64S",
            Self::Lp64f => "LP64F",
            Self::Lp64d => "LP64D",
        })
    }
}

#[derive(Debug)]
pub enum LoongArchAbiValidationError {
    Inspection(LinkInputInspectionError),
    Unsupported { input: String },
    Mismatch { target: LoongArchFloatAbi, input: String, input_abi: LoongArchFloatAbi },
}

impl fmt::Display for LoongArchAbiValidationError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Inspection(error) => error.fmt(formatter),
            Self::Unsupported { input } => write!(
                formatter,
                "invalid LoongArch linker input '{}': unsupported base ABI modifier",
                input
            ),
            Self::Mismatch {
                target,
                input,
                input_abi,
            } => write!(
                formatter,
                "LoongArch floating-point ABI mismatch before linking\ntarget ABI: {}\ninput: {}\ninput ABI: {}",
                target, input, input_abi
            ),
        }
    }
}

impl std::error::Error for LoongArchAbiValidationError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Inspection(error) => Some(error),
            Self::Unsupported { .. } | Self::Mismatch { .. } => None,
        }
    }
}

/// Verifies that every LoongArch ELF object, including archive members, uses
/// the base ABI selected by Wave's Linux LoongArch64 target.
pub fn validate_loongarch64_link_inputs(
    target_abi: LoongArchFloatAbi,
    inputs: &[String],
) -> Result<(), LoongArchAbiValidationError> {
    for metadata in inspect_link_inputs(inputs).map_err(LoongArchAbiValidationError::Inspection)? {
        if metadata.machine != EM_LOONGARCH {
            continue;
        }
        let input_abi = LoongArchFloatAbi::from_elf_flags(metadata.flags).ok_or_else(|| {
            LoongArchAbiValidationError::Unsupported { input: metadata.input.clone() }
        })?;
        if input_abi != target_abi {
            return Err(LoongArchAbiValidationError::Mismatch {
                target: target_abi,
                input: metadata.input,
                input_abi,
            });
        }
    }
    Ok(())
}
