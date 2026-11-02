# Wave tests

End-to-end Wave programs live here. `shared/` contains portable tests; other
folders contain architecture or operating-system tests. `cases.toml` selects
supported targets and their CI execution.

Run `build-tools/wave-tools test` from the repository root.

`shared/test142.wave` exercises `std::env` boundaries using only the variables
in `tests/fixtures/env_boundaries.json`: missing and empty values, UTF-8 bytes,
capacity errors, signed integer limits and defaults. Its `env-file` metadata
injects those values for native Linux, macOS, Windows and FreeBSD execution.
Bare WebAssembly and freestanding targets exclude this case in `cases.toml`
because they have no process environment provider. WASI still compiles it;
its runtime selector classifies this native-only case as unsupported rather
than counting it as executed.
