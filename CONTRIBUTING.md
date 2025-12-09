# Contributing to Wave

Thank you for your interest in contributing to Wave, a modern systems programming language.
Wave welcomes contributions through GitHub Pull Requests and email-based patches.
This document explains how to contribute in both ways, the required development setup, and contribution rules.

---

## 1. Development Setup

Wave uses a dedicated setup repository for tools and environment preparation.

Before contributing, please follow:

-> https://github.com/wavfnd/setup

This includes installation instructions for Rust, LLVM, Clang tools, and other dependencies required to build Wave

---

## 2. Contribution Methods

Wave accepts contributions in two ways:

### 2.1 GitHub pull Request (Recommended for most contributor)

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

Then open a PR on GitHub

### 2.2 Email Patch Submission

Wave also accepts patches through email, similar to the Linux kernel and LLVM workflows.

#### Steps to submit a patch via email

```bash
git checkout -b fix-issue
git commit -s
git format-patch -1
git send-email --to wave-patches@lunastev.org *.patch
```

#### Requirements:
- ALL commits must include `Signed-off-by:` (DCO)
- One patch should address on logical change
- Patch series is allowed (`git format-patch` supports it)

---

## 3. DCO Requirement (Developer Certificate of Origin)

Wave requires all commits-PRs and patches-to be signed off:

```bash
git commit -s
```

This adds:

```text
Signed-off-by: Your Name <email@example.com>
```

Commits without DCO will be rejected.

----

## 4. Patch Verification (Maintainers Only)

Maintainters must verify all incoming patches using:

```bash
tools/verify_patch.sh your_patch.patch
```

This script checks:

- Patch applies cleanly (`git am`)
- DCO signature is present
- `cargo fmt --check`
- `cargo build`
- `cargo test`
- `cargo clippy -- -D warnings`

This ensures that patches do not break Wave's compiler.

---

## 5. Finding the Appropriate Maintainer

To determine which maintainer should review your patch, use:

```bash
python3 tools/get_maintainer.py path/to/changed/file.rs
```

This script reads the repository's `MAINTAINERS` file and prints the appropriate individuals.

Patch authors may CC maintainers manually when sending patches via email.

---

## 6. Code Style

Wave follows standard Rust conventions:

- snake_case for functions and variables
- PascalCase for structs, enums, and types
- SCREAMING_SNAKE_CASE for constants
- Opening braces on the same line (K&R style)
- No trailing whitespace

All formatting rules must pass:

```bash
cargo fmt --check
cargo clippy -- -D warnings
```

---

## 7. Project Scope and Philosophy

Wave is a systems programming language with:

- No builtin functions
- No implicit runtime
- Strict explicit behavior
- A powerful compiler-first architecture

Do not add builtin functions or hidden magic to the compiler.
All additional functionality should be provided through external libraries (e.g., Vex).

---

## 8. Tests

Wave uses:
- Rust unit tests (`cargo test`)
- Manually executed `.wave` examples in `test/` (not automated)

Contributors should:

- Add unit tests for new Rust functionality
- Add `.wave` examples for new language features

---

## 9. Pull Request Guidelines

A PR should include:

- A clear description of the change
- Why the change is needed
- Tests if applicable
- Documentation updates if necessary
- Signed-off commits (`-s`)

Small, focused PRs are preferred.

---

## 10. Communication

- GitHub Issues -> bug reports, proposals, questions
- GitHub Discussions -> design conversations, feedback
- Discord community -> informal communication and help

---

## 11. License

By contributing to Wave, you agree that your contributions are licensed under:

[Mozilla Public License 2.0](LICENSE)

---

## 12. Thank You

Every contribution helps Wave grow into a robust, modern systems language.
Thank you for helping shape the future of Wave.