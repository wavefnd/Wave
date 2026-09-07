# Contributing to Wave

Thank you for your interest in contributing to Wave, a general-purpose
programming language with explicit native and low-level capabilities.
Wave welcomes contributions through GitHub Pull Requests and email-based
patches. This document explains how to contribute in both ways, the required
development setup, and contribution rules.

---

## 1. Development Setup

Wave uses a dedicated setup repository for tools and environment preparation.

Before contributing, please follow:

https://github.com/wavefnd/setup

This includes installation instructions for Rust, LLVM, Clang tools, and other
dependencies required to build Wave.

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

## 4. Local Verification (mirrors CI)

From the repository root, run the same gates the Linux amd64 job in
`.github/workflows/rust.yml` uses before you open a PR. Prefer `--jobs 2` on
resource-intensive Cargo commands (CI sets `CARGO_BUILD_JOBS=2`).

```bash
cargo fmt --all --check
./tools/check_std_policy.sh
RUSTDOCFLAGS="-D warnings" cargo doc --locked --workspace --no-deps --jobs 2
cargo clippy --locked --workspace --all-targets -- -D warnings
python3 -m py_compile x.py tools/check_wave_corpus.py tools/case_manifest.py \
  tools/populate_case_matrix.py tools/run_tests.py tools/test_contracts.py \
  tools/test_case_manifest.py tools/test_test_contracts.py
python3 -m unittest tools.test_case_manifest tools.test_test_contracts
cargo build --locked --release --jobs 2
cargo test --locked --workspace --all-targets --verbose
python3 tools/check_wave_corpus.py --wavec target/release/wavec --run-std-examples
```

Notes:

- Formatting must use `cargo fmt --all --check` (not bare `cargo fmt --check`).
- Clippy denies warnings: `cargo clippy --locked --workspace --all-targets -- -D warnings`.
- rustdoc must be warning-free via `RUSTDOCFLAGS="-D warnings"`.
- Standard-library policy is enforced by `./tools/check_std_policy.sh`.
- Wave language corpus / std examples are checked with `tools/check_wave_corpus.py`
  after a release `wavec` build.

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
native ARM64 clang-cl/MSVC tools, the DLL CRT used by default Rust MSVC
builds, and no optional iconv, compression, Python, or XML DLL dependencies.
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
tools/verify_patch.sh your_patch.patch
```

Prefer the full local verification block above when reviewing GitHub PRs.

---

## 5. Finding the Appropriate Maintainer

To determine which maintainer should review your patch, use:

```bash
python3 tools/get_maintainer.py path/to/changed/file.rs
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

All formatting and lint rules must pass:

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
- Automated `.wave` language cases and std examples via
  `python3 tools/check_wave_corpus.py`
- Python tooling unit tests: `python3 -m unittest tools.test_case_manifest tools.test_test_contracts`

Contributors should:

- Add unit tests for new Rust functionality
- Add `.wave` examples for new language features

---

Frontend syntax changes must update [the Alpha grammar and token inventory](spec/README.md)
and its positive/negative fixtures. Run `cargo test --locked -p lexer -p parser --jobs 2`
for backend-independent frontend tests, then the workspace tests for driver and codegen coverage.

## 9. Pull Request Guidelines

A PR should include:

- A clear description of the change
- Why the change is needed
- Tests if applicable
- Documentation updates if necessary
- Signed-off commits (`-s`)

All pull request descriptions and comments must be written in English.

Small, focused PRs are preferred. Target the `master` branch.

---

## 10. Communication

- GitHub Issues: bug reports, proposals, questions
- GitHub Discussions: design conversations, feedback
- Discord community: informal communication and help

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
