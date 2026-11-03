# macOS platform version selection

The two Darwin link values are independent: the deployment target is the
minimum supported macOS release, while the SDK version describes the SDK
used to link the binary. Both values appear in the `ld64.lld` command as
`-platform_version macos <deployment> <sdk>`.

Wave accepts `--macos-deployment-target=<major.minor[.patch]>` and
`--macos-sdk-version=<major.minor[.patch]>` with both `=` and separate-value
forms. These flags are accepted with Darwin targets only.

Deployment version precedence:

1. `--macos-deployment-target`
2. `MACOSX_DEPLOYMENT_TARGET`
3. Compiler minimum, `11.0`

SDK version precedence:

1. `--macos-sdk-version`
2. `MACOSX_SDK_VERSION`
3. Versioned SDK bundle selected by `--sysroot`, `SDKROOT` or `xcrun`
   (for example, `MacOSX15.4.sdk` resolves to SDK `15.4`)
4. The host's `xcrun --sdk macosx --show-sdk-version`, when available
5. Compiler fallback `15.0` when no SDK metadata is available

The fallback is a link-plan assumption, **not** a claim that an SDK is
installed or that the host runs that macOS version. For deterministic
cross-linking, set `--sysroot` and `--macos-sdk-version` explicitly. A
selected SDK must not be older than the deployment target. Malformed
versions are rejected with usage diagnostics before any link process.

Both values appear in `wavec print target-spec --format=json` and
`wavec build ... --dry-run --error-format=json`, as well as the human
renderings. x86_64 and AArch64 Darwin use the same contract.
