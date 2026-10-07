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

//! Target-platform roadmap kept separate from ISA lowering.
//!
//! Entries here describe OS, SDK, object-format, and linker requirements. They
//! deliberately do not enter the supported target registry in `target`; doing
//! that requires completed ABI, standard-library, linker, and runtime work.

pub mod android;
pub mod ios;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LinkerFamily {
    ElfLld,
    DarwinLld,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PlatformPlan {
    pub os: &'static str,
    pub architectures: &'static [&'static str],
    pub object_format: &'static str,
    pub sdk: &'static str,
    pub linker: LinkerFamily,
}

pub const ROADMAP: &[PlatformPlan] = &[android::PLAN, ios::PLAN];

pub fn roadmap_platform(os: &str) -> Option<&'static PlatformPlan> {
    ROADMAP.iter().find(|plan| plan.os == os)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::target::target_spec_for_triple;

    #[test]
    fn roadmap_platforms_are_explicit_but_not_advertised_as_supported() {
        for plan in ROADMAP {
            assert!(!plan.os.is_empty());
            assert!(!plan.architectures.is_empty());
            assert!(!plan.object_format.is_empty());
            assert!(!plan.sdk.is_empty());
        }

        assert_eq!(roadmap_platform("android"), Some(&android::PLAN));
        assert_eq!(roadmap_platform("ios"), Some(&ios::PLAN));
        assert_eq!(roadmap_platform("linux"), None);

        for triple in android::CANDIDATE_TRIPLES.iter().chain(ios::CANDIDATE_TRIPLES.iter()) {
            assert_eq!(target_spec_for_triple(triple), None, "{triple}");
        }
    }
}
