# Contributing to Wave

Thank you for your interest in contributing to Wave, a general-purpose
programming language with explicit native and low-level capabilities.
Wave welcomes contributions through GitHub Pull Requests and email-based
patches. This document explains how to contribute in both ways, the required
development setup, and contribution rules.

## Choosing a first contribution

Documentation fixes, clearer examples, and small bug fixes are welcome. Start
with the [open good first issues](https://github.com/wavefnd/Wave/issues?q=is%3Aissue%20is%3Aopen%20label%3A%22good%20first%20issue%22),
then read the issue, recent comments, and linked pull requests before choosing
one. Check whether someone is already working on it, and comment with your
proposed approach or questions about the scope before starting substantial work.

If no listed issue fits, ask in [GitHub Discussions](https://github.com/wavefnd/Wave/discussions)
for a small task that matches your experience. You can also submit a focused
typo or broken-link fix directly. Read the [Code of Conduct](CODE_OF_CONDUCT.md)
before participating.

---

## 1. Development Setup

Wave uses a dedicated setup repository for tools and environment preparation.

Before building or testing the compiler, please follow:

https://github.com/wavefnd/setup

This includes installation instructions for Rust, LLVM, Clang tools, and other
dependencies required to build Wave.

Build automation uses Meson and Ninja; Cargo still compiles the Rust workspace.
Python 3.11 or newer runs Meson. The pinned Rust toolchain also builds the native maintenance tool, including when `compiler=false`. Install the pinned automation tools and build:

```bash
python3 -m pip install -r tools/ci/requirements-build.txt
meson setup build
meson compile -C build --jobs 2
```

The compiler remains at `target/release/wavec` (`wavec.exe` on Windows), with
a copy in the Meson build directory. Run `meson configure build` to inspect build
options. Published documentation is maintained in
[wavefnd/wave-platform](https://github.com/wavefnd/wave-platform).

For prose or link changes only, you do not need to install Rust, LLVM, or Clang.
Start with the [documentation-only checks](#documentation-only-changes) below.

---

## 2. Contribution Methods

Wave accepts contributions in two ways:

### 2.1 GitHub Pull Request (Recommended for most contributors)

1. Fork the repository
2. Create a branch
3. Commit changes with `git commit -s`
4. Open a Pull Request targeting `master`

Example:

```bash
git checkout -b fix/parser-bug
git commit -s -m "Fix incorrect precedence handling"
git push origin fix/parser-bug
```

Then open a PR on GitHub.

### 2.2 Email Patch Submission

Wave also accepts patches through email, similar to the Linux kernel and LLVM
workflows.

#### Steps to submit a patch via email

```bash
git checkout -b fix-issue
git commit -s
git format-patch -1
git send-email --to patchs@wave-lang.dev *.patch
```

#### Requirements

- ALL commits must include `Signed-off-by:` (DCO)
- One patch should address one logical change
- Patch series are allowed (`git format-patch` supports them)

---

## 3. DCO Requirement (Developer Certificate of Origin)

Wave requires all commits (PRs and patches) to be signed off:

```bash
git commit -s
```

This adds:

```text
Signed-off-by: Your Name <email@example.com>
```

Commits without DCO will be rejected.

---

## 4. Local Verification

Choose local checks that match your change, and list what you ran in the PR
description. If you could not run a relevant check, say which one and why.
CI still runs the repository's configured checks; the guidance below describes
what to verify locally.

### Documentation-only changes

For changes limited to prose, spelling, or links in Markdown files:

1. Preview the changed Markdown and check headings, lists, and code-block layout.
2. Open changed links and verify relative paths and section anchors.
3. Run `git diff --check` from the repository root to catch whitespace errors.

You do not need to build the compiler for these edits. If you change a code
example, command, or documented behavior, also verify it against the current
implementation and run the affected example or command when possible. Rust
documentation comments and doctests need the relevant Rust checks below.

### Code and tooling changes

For compiler or standard-library changes, complete the development setup and
run the common checks below from the repository root. Project maintenance,
validation and packaging use the native `wave-tools` Cargo package. For tooling
changes, `meson setup build-tools -Dcompiler=false` and
`meson test -C build-tools --suite tooling --print-errorlogs` run its contracts
without building wavec or requiring LLVM. The pinned Rust toolchain is required.
Python remains the runtime for Meson; the offline trigonometric reference-vector
generator additionally uses mpmath. Node runs WebAssembly hosts, and the Windows
bootstrap scripts provision native SDK inputs before compiler builds.

[`.github/workflows/ci.yml`](.github/workflows/ci.yml) contains the target-specific
job graph and explicit check steps. Cargo owns compilation, Meson owns local build
dependencies and test suites, and Actions owns job conditions and artifact transport.
`build-tools/wave-tools matrix --json` prints the validated case manifest projection;
`build-tools/wave-tools check ci-jobs` lists the required canonical CI job names.

CI job names follow `<OS> / <architecture> / <operation>`,
for example `Linux / amd64 / Build & test`.
Use `amd64`, `arm64`, `loong64`, and `riscv64` consistently in display names;
target triples and artifact names retain their toolchain spelling. Group platform
jobs as Linux, macOS, Windows, other cross targets, and WebAssembly, with a
consistent architecture order within each group. Keep job IDs stable when
renaming checks, and check required status contexts before merging a rename.

Prefer `--jobs 2` on resource-intensive Cargo commands
(CI sets `CARGO_BUILD_JOBS=2`). Common local checks include:

```bash
meson setup build-tools -Dcompiler=false
meson test -C build-tools --suite tooling --suite policy --print-errorlogs
build-tools/wave-tools maintain format --check
RUSTDOCFLAGS="-D warnings" cargo doc --locked --workspace --no-deps --jobs 2
cargo clippy --locked --workspace --all-targets --jobs 2 -- -D warnings
meson setup build -Dbuildtype=release
meson compile -C build --jobs 2 wavec
cargo test --locked --workspace --all-targets --jobs 2
build-tools/wave-tools corpus --wavec target/release/wavec --run-std-examples
```

The Rust tooling contracts cover process-tree ownership, timeout/output limits,
configuration and report codecs, artifact inspection, package transactions and
publication policy. Tests use malformed binary/archive fixtures and disposable Git
repositories; they do not publish releases or require LLVM. Compiler execution,
platform ABI probes and extracted-package smoke checks remain separate evidence.
`maintain format --check` combines rustfmt with function-spacing validation.
`check std-policy` enforces std provider/import/declaration rules.

Trace a failing CI step directly to its Cargo, Meson or `wave-tools` command.
The shared process owner in [`tools/src/process.rs`](tools/src/process.rs) bounds
and records child output and reaps descendants on failure, timeout or cancellation.
Case execution in [`tools/src/runner.rs`](tools/src/runner.rs) checks build success
and the produced artifact before launching the program. A compiler failure cannot
satisfy a program's expected nonzero exit status. Platform-specific acceptance
commands retain their own ABI/runtime assertions.

Case reports (`wave-tools test --report-json PATH`) retain `schema_version: 1`,
`host`, `compiler`, `selection`, `summary` and `tests`. Selection distinguishes
`auto`, `target` and `suites`, with the selected manifest ID, default target,
executor, shard and suites. Each case records separate build/artifact/run phases;
failures preserve reasons and exact exit codes. Captured stream excerpts are
bounded at 4096 characters and include truncation flags. Timeouts record their
limit. Cancelled active cases are interrupted and pending cases stay `not_run`.

QEMU and WebAssembly cases share one executor. Eligible sources produce their
object and executable in one build; other sources retain compile-only checks.
`wave-tools ci runtime-evidence` validates the same shard's successful run records
and emits a derived runtime report without compiling or executing them a second
time. ABI smoke probes and isolated extracted-package execution remain independent.
Corpus socket skips are limited to explicit local permission/family restrictions
and retain their reason; other socket failures are errors. Nightly eligibility
still requires every canonical CI job to succeed.

### Native Windows ARM64 LLVM dependency

The official LLVM 21.1.8 ARM64 MSVC SDK lists `xml2s.lib` in
`llvm-config --system-libs --link-static` without shipping the library.
`llvm-sys` 211 rejects dynamic LLVM linking on MSVC, so `prefer-dynamic`
still falls back to this static dependency.

The build, cases, and release workflows run
`tools/provision_windows_arm64_libxml2.ps1` after installing LLVM. It builds
libxml2 2.13.9 from a SHA-256-pinned
[GNOME source archive](https://download.gnome.org/sources/libxml2/2.13/),
retaining the pre-2.14 XML ABI used by LLVM's static code. The build uses
native ARM64 clang-cl/MSVC tools and the static CRT matching the pinned LLVM
SDK, with no optional iconv, compression, Python, or XML DLL dependencies.
Use an explicit `--target aarch64-pc-windows-msvc` for native Cargo commands:
this keeps target CRT flags off host build scripts. In llvm-sys 211, applying
those flags to the build script changes Windows SDK import libraries such as
`psapi` into static-bundling requests before the final MSVC link.
It supplies the SDK's `xml2s.lib` name and the Windows `bcrypt`/`ws2_32`
imports, then checks every COFF archive member for machine `0xaa64` before
Cargo links it. Release packaging includes the libxml2 copyright notice.
The upstream LLVM release configuration is available in
[the LLVM 21.1.8 release script](https://github.com/llvm/llvm-project/blob/llvmorg-21.1.8/llvm/utils/release/build_llvm_release.bat).

With LLVM tools and PowerShell available, run
`pwsh -NoProfile -File tools/test_windows_arm64_libxml2.ps1` to check the
script syntax and its rejection of x64, mixed, and empty archives. This
portable check complements the actual native Windows ARM64 build and cases;
it does not replace them.

### 4.1 Patch Verification (Maintainers Only)

Maintainers must verify incoming email patches using:

```bash
build-tools/wave-tools maintain verify-patch your_patch.patch
```

When reviewing GitHub PRs, use the checks appropriate to the change above and
confirm the configured CI checks pass.

---

## 5. Finding the Appropriate Maintainer

To determine which maintainer should review your patch, use:

```bash
build-tools/wave-tools maintain maintainers path/to/changed/file.rs
```

This script reads the repository's `MAINTAINERS` file and prints the appropriate
individuals.

Patch authors may CC maintainers manually when sending patches via email.

---

## 6. Code Style

Wave follows standard Rust conventions:

- snake_case for functions and variables
- PascalCase for structs, enums, and types
- SCREAMING_SNAKE_CASE for constants
- Opening braces on the same line (K&R style)
- No trailing whitespace

For Rust changes, formatting and lint rules must pass:

```bash
cargo fmt --all --check
cargo clippy --locked --workspace --all-targets -- -D warnings
```

---

## 7. Project Scope and Philosophy

Wave is a general-purpose programming language with explicit native and
low-level capabilities. It emphasizes:

- No builtin functions
- No implicit runtime
- Strict explicit behavior
- A powerful compiler-first architecture

Do not add builtin functions or hidden magic to the compiler.
All additional functionality should be provided through external libraries
(e.g., Vex).

---

## 8. Tests

Wave uses:

- Locked Rust tests: `cargo test --locked --workspace --all-targets`
- Automated `.wave` language cases: `build-tools/wave-tools test`
- Std/examples checks and execution: `build-tools/wave-tools corpus --run-std-examples`
- Native tooling contracts: `meson test -C build-tools --suite tooling --print-errorlogs`

Contributors should:

- Add unit tests for new Rust functionality
- Add `.wave` examples for new language features

---

## 9. Pull Request Guidelines

A PR should include:

- A clear description of the change
- Why the change is needed
- Checks run and their results, including any relevant checks not run and why
- Tests if applicable
- Documentation updates if necessary
- Signed-off commits (`-s`)

All pull request descriptions and comments must be written in English.

Small, focused PRs are preferred. Target the `master` branch.

---

## 10. Communication

- [GitHub Issues](https://github.com/wavefnd/Wave/issues/new/choose): bug reports,
  performance reports, and concrete feature requests using the available templates
- [GitHub Discussions](https://github.com/wavefnd/Wave/discussions): usage and
  contribution questions, design conversations, feedback, and ideas before
  opening an implementation issue
- [Discord community](https://discord.gg/3nev5nHqq9): informal communication and help

---

## 11. License

By contributing to Wave, you agree that your contributions are licensed under
the license governing the part of the repository you modify:

- Contributions outside [`std/`](std/) are licensed under the
  [Mozilla Public License 2.0](LICENSE).
- Contributions to the Wave standard library under [`std/`](std/) are licensed
  under the [Apache License 2.0](std/LICENSE).

---

## 12. Thank You

Every contribution helps Wave grow into a robust, general-purpose language with
strong native and low-level capabilities. Thank you for helping shape the future
of Wave.

### Standard-library documentation

After changing a public std declaration or provider, run `build-tools/wave-tools maintain std-api`
and verify `build-tools/wave-tools maintain std-api --check`. Review the capability summary,
`tests/cases/cases.toml` and `.github/workflows/ci.yml` when behavior changes. Public
imports belong in the inventory; private implementation imports do not.
