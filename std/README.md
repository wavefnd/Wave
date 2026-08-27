# Wave Standard Library

This is Wave's standard library. This standard library operates independently of Wave's compiler and is not part of the compiler itself.

## License

The Wave standard library in this directory is licensed under the

[Apache License 2.0](LICENSE). It may be modified, redistributed, and embedded

in other products under the terms of that license.

The Wave compiler and other repository components outside `std/` remain

licensed under the repository's [Mozilla Public License 2.0](../LICENSE).

## Dependency Policy

- `std/libc/*` is the only place where `extern(c)` bindings are allowed.
- Modules outside `std/libc/*` must not import or rely on libc bindings.
- Non-libc modules should be implemented directly in Wave (or raw syscall layers under `std/sys/linux/*`).

## Core Modules

- `std::time`: sleep and clock helpers.
- `std::env`: cwd and environment lookup helpers.
- `std::path`: allocation-free path utilities.
- `std::mem`: manual memory utilities for non-GC code.
- `std::buffer`: growable byte buffer built on `std::mem`.
- `std::io`: fd-level read/write/seek/copy helpers.
- `std::fs`: basic open/read/write/copy/metadata helpers.
- `fs_read_all` reads into caller-owned byte storage; it does not allocate or return `str`.
- `std::bytes`: endian swap/load/store helpers.
- `std::process`: fork/exec/wait and stdio redirection helpers.

## Layout

- High-level module entry points stay as `std/<name>.wave`.
- Implementations are split by role under `std/<name>/*.wave` (example: `std/time/*.wave`).