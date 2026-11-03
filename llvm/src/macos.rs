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

//! Validated macOS linker platform versions, shared by the driver and backend.
//!
//! The deployment minimum and SDK version are independent settings. An SDK
//! directory such as MacOSX15.4.sdk can supply its version without requiring
//! a compiler-specific SDK metadata parser.

pub const DEFAULT_DEPLOYMENT_TARGET: &str = "11.0";
pub const DEFAULT_SDK_VERSION: &str = "15.0";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlatformVersions {
    pub deployment: String,
    pub sdk: String,
}

fn parsed_version(text: &str, label: &str) -> Result<[u32; 3], String> {
    let parts = text.split('.').collect::<Vec<_>>();
    if !(2..=3).contains(&parts.len())
        || parts.iter().any(|part| part.is_empty() || !part.bytes().all(|b| b.is_ascii_digit()))
    {
        return Err(format!("invalid {label} version {text:?}; expected major.minor[.patch]"));
    }
    let mut version = [0; 3];
    for (index, part) in parts.iter().enumerate() {
        version[index] = part
            .parse::<u32>()
            .map_err(|_| format!("invalid {label} version {text:?}"))?;
    }
    if !(10..=999).contains(&version[0]) {
        return Err(format!("invalid {label} version {text:?}; major must be at least 10"));
    }
    Ok(version)
}

/// Extract a version only from a versioned macOS SDK bundle name.
pub fn sdk_version_from_path(sysroot: &str) -> Option<String> {
    let file = std::path::Path::new(sysroot).file_name()?.to_str()?;
    let value = file.strip_prefix("MacOSX")?.strip_suffix(".sdk")?;
    parsed_version(value, "macOS SDK").ok().map(|_| value.to_string())
}

/// Resolve and validate both linker versions before constructing a link command.
///
/// Explicit Wave flags take precedence over environment variables. An SDK
/// version from the selected SDK directory takes precedence over the host SDK
/// discovery result. Defaults are independent, and never use the host macOS
/// release as a proxy for the SDK or deployment policy.
pub fn resolve_versions(
    deployment_override: Option<&str>,
    deployment_env: Option<&str>,
    sdk_override: Option<&str>,
    sdk_env: Option<&str>,
    sysroot: Option<&str>,
    discovered_sdk: Option<&str>,
) -> Result<PlatformVersions, String> {
    let deployment = deployment_override
        .or(deployment_env)
        .unwrap_or(DEFAULT_DEPLOYMENT_TARGET);
    let path_sdk = sysroot.and_then(sdk_version_from_path);
    let sdk = sdk_override
        .or(sdk_env)
        .or(path_sdk.as_deref())
        .or(discovered_sdk)
        .unwrap_or(DEFAULT_SDK_VERSION);
    let minimum = parsed_version(deployment, "macOS deployment target")?;
    let selected = parsed_version(sdk, "macOS SDK")?;
    if selected < minimum {
        return Err(format!(
            "macOS SDK version {sdk} is older than deployment target {deployment}"
        ));
    }
    Ok(PlatformVersions { deployment: deployment.into(), sdk: sdk.into() })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn explicit_overrides_env_and_sdk_path() {
        let versions = resolve_versions(
            Some("12.3"), Some("11.2"), Some("16.1"), Some("15.2"),
            Some("/SDKs/MacOSX14.4.sdk"), Some("13.3"),
        ).unwrap();
        assert_eq!(versions.deployment, "12.3");
        assert_eq!(versions.sdk, "16.1");
        let versions = resolve_versions(
            None, Some("11.2"), None, None,
            Some("/SDKs/MacOSX14.4.sdk"), Some("13.3"),
        ).unwrap();
        assert_eq!(versions.deployment, "11.2");
        assert_eq!(versions.sdk, "14.4");
        let versions = resolve_versions(None, None, None, Some("15.2"), None, Some("14.0")).unwrap();
        assert_eq!(versions.sdk, "15.2");
    }

    #[test]
    fn bad_versions_and_incompatible_sdks_are_rejected() {
        for invalid in ["", "11", "11.", ".1", "11.0.0.1", "11.a", "11.0 -arch", "0.0", "999999999999999999.1"] {
            assert!(resolve_versions(Some(invalid), None, None, None, None, None).is_err());
            assert!(resolve_versions(None, None, Some(invalid), None, None, None).is_err());
        }
        assert!(resolve_versions(Some("15.1"), None, Some("15.0"), None, None, None).is_err());
        assert_eq!(sdk_version_from_path("/SDKs/MacOSX15.5.sdk"), Some("15.5".into()));
        assert_eq!(sdk_version_from_path("/SDKs/MacOSX.sdk"), None);
        assert_eq!(sdk_version_from_path("/SDKs/iPhoneOS18.0.sdk"), None);
        assert_eq!(sdk_version_from_path("/SDKs/MacOSX15.4_bad.sdk"), None);
    }
}
