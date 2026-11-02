# Code generation regression suites

`tests/codegen_regressions.rs` remains the Cargo integration-test target so existing
CI and release commands using `--test codegen_regressions` and a test-name filter
keep selecting the same cases.

The tests are grouped by purpose in `language.rs`, `abi.rs`, `platform.rs`
and `diagnostics.rs`. Shared setup, target gates, temporary fixture isolation,
CLI helpers and timeouts live in `support.rs`.

Run every regression:

```sh
cargo test --locked --test codegen_regressions
```

Run an individual regression:

```sh
cargo test --locked --test codegen_regressions riscv64_c_abi_interoperates_with_c
```
