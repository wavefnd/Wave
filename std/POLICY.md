# Wave STD Policy

This document defines non-negotiable rules for the Wave standard library (`std/`).

## 1) Layering Rules

- General-purpose C library bindings belong in external libraries, not `std/`.
- `extern(c)` declarations are limited to the target-specific C ABI providers
  explicitly approved by `wave-tools check std-policy`. This includes hosted OS
  APIs and WASI imports; it does not require replacing existing OS bindings.
- Portable modules use Wave code and the target-selected `std/sys/**` provider
  surface. No standard library module may import the removed `std::libc::*`
  binding modules.

## 2) Syntax Rules

- `var` is the only local variable declaration syntax.
- The retired `let` and `let mut` declaration forms are forbidden.
- Use top-level `const` declarations for immutable constants.
- Every symbol used by another module must be explicitly marked `pub`.
- Imports must select their unqualified symbols; dispatcher modules may use
  `pub import("module")::{symbol};` for deliberate re-exports.

## 3) Runtime Contract Rules

- `std/sys/**` should preserve raw syscall style: success `>= 0`, errors as negative `-errno`.
- High-level `std/**` modules may define additional library-level error codes when needed.

## 4) Compatibility Rules

- All `std/` code must remain compatible with the compiler version shipped in this repository.
- Avoid patterns known to break older codegen paths (for example, complex index expressions in single brackets).

## 5) Automated Check

Run:

```bash
build-tools/wave-tools check std-policy
```

The checker validates:

- `extern(c)` appears only in explicitly approved target C ABI providers
- no `std::libc::*` import in standard library modules
- no retired `let` or `let mut` declarations in Wave sources
