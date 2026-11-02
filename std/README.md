# Wave Standard Library

This is Wave's standard library. This standard library operates independently of Wave's compiler and is not part of the compiler itself.

## License

The Wave standard library in this directory is licensed under the

[Apache License 2.0](LICENSE). It may be modified, redistributed, and embedded

in other products under the terms of that license.

The Wave compiler and other repository components outside `std/` remain

licensed under the repository's [Mozilla Public License 2.0](../LICENSE).

## Dependency Policy

- General-purpose C library bindings are not bundled with the standard library.
- Target providers under `std/sys/*` may use the small set of hosted C ABI
  bindings explicitly approved by `wave-tools check std-policy` when the OS does
  not expose an equivalent stable raw syscall contract.
- `std/sys/wasm/*` maps its raw imports to WASI Preview 1 capabilities. The
  provider covers descriptor I/O, clocks and sleep, preopened paths,
  environment access, process exit, and linear-memory allocation. The allocator
  is also available to bare `wasm32-unknown-unknown` modules. A separate
  Memory64 allocator backs `wasm64-unknown-unknown` without narrowing pointers.
- Standard library modules must not import `std::libc::*`; portable modules
  depend on the target-independent `std::sys::*` provider surface instead.

The former `std::libc::*` binding modules have been removed during alpha
development. Existing imports of these modules no longer resolve. Use the
relevant Wave standard library APIs, or provide C interoperability bindings in
an external library for the required target ABI. The target providers under
`std::sys::*` retain their existing OS API bindings.

## Core Modules

- `std::time`: portable clocks and sleep, normalized durations, UTC calendar
  conversion, and fixed ISO 8601 parsing/formatting.
- `std::env`: cwd and environment lookup helpers.
- `std::path`: allocation-free path utilities.
- `std::math`: checked integer arithmetic, bit operations, IEEE-754 helpers,
  roots, and trigonometry.
- `std::mem`: raw allocation plus checked byte-size, copy, move, comparison,
  alignment, runtime target page sizing, and bounded C-string helpers for
  non-GC code.
- `std::buffer`: checked growable byte storage built on `std::mem`.
- `std::io`: fd-level read/write/seek/copy helpers.
- `std::fs`: basic open/read/write/copy/metadata helpers.
- `fs_read_all` reads into caller-owned byte storage; it does not allocate or return `str`.
- `std::bytes`: endian primitives, borrowed byte views, checked random access,
  and transactional byte readers/writers.
- `std::process`: fork/exec/wait and stdio redirection helpers.
- `std::debug`: lightweight marked logging, value tracing, assertions, and
  fatal diagnostics for development builds.

## Layout

- Public modules are split by role under `std/<name>/*.wave` (for example,
  `std/time/duration.wave` and `std/bytes/cursor.wave`).
- Target-specific providers stay under `std/sys/<os>/<arch>/` when the native
  ABI differs by architecture.

## API and target availability

See the [public declaration inventory](API.md) for exact signatures and public re-exports,
and [system capabilities](SYSTEMS.md) for provider boundaries. Refresh both inventories
with `wave-tools maintain std-api` after changing public std declarations.
