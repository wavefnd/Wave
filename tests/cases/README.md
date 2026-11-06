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

## Target identities and CPU profiles

`cases.toml` version 3 separates base architectures in `[supported]` and
`[ci]` from optional processor profiles in `[profiles]`

Rhea uses the `arm64` base ISA; SHAKTI, XiangShan and T-Head use `riscv64`
These Linux profiles remain disabled until their compiler/runtime contracts exist

A base target runs `shared`, `shared/<arch>`, then `<os>/<arch>`
A profile target also inherits `shared/<profile>` and `<os>/<profile>`
after the corresponding base-architecture suite of each kind

A supported profile requires an explicit `[[target]]` entry with matching
`os`, base `arch` and `profile` fields, plus an explicit target runner
Profile identity does not imply new compiler target triples or ISA support
Existing CI-enabled base targets and their matrices are unchanged
