<div align="center">
  <a href="https://wave-lang.dev/">
    <img src="https://wave-lang.dev/img/wave-logo.ico" width="128" alt="Wave programming language logo">
  </a>

  <h1>Wave</h1>

  <p><strong>A general-purpose programming language built as a modern evolution of C.</strong></p>
  <p>Native compilation and direct control, rebuilt for modern software and tooling.</p>

  <p>
    <a href="https://wave-lang.dev/"><strong>Website</strong></a> ·
    <a href="https://wave-lang.dev/docs/"><strong>Documentation</strong></a> ·
    <a href="https://wave-lang.dev/releases"><strong>Releases</strong></a> ·
    <a href="https://wave-lang.dev/community"><strong>Community</strong></a> ·
    <a href="https://opencollective.com/wave-lang/contribute"><strong>Sponsor</strong></a>
  </p>

  <p>
    <a href="https://github.com/wavefnd/Wave/actions/workflows/rust.yml"><img src="https://img.shields.io/github/actions/workflow/status/wavefnd/Wave/rust.yml?branch=master&style=flat-square&label=build&labelColor=17132B&color=6654F1" alt="Build status"></a>
    <a href="https://github.com/wavefnd/Wave/releases"><img src="https://img.shields.io/github/v/release/wavefnd/Wave?include_prereleases&style=flat-square&label=release&labelColor=17132B&color=6654F1" alt="Latest release"></a>
    <a href="https://opencollective.com/wave-lang/contribute"><img src="https://img.shields.io/badge/sponsor-Wave-6654F1?style=flat-square&labelColor=17132B&logo=opencollective&logoColor=white" alt="Sponsor Wave on OpenCollective"></a>
  </p>
</div>

## A modern evolution of C

C made native software portable, direct, and durable. Wave starts from those fundamentals and modernizes the language around them with modules, packages, generics, structured diagnostics, and cross-target tooling.

Wave is not a C dialect and does not aim for C source compatibility. It is a new general-purpose language with its own syntax, semantics, module system, and toolchain.

- **General-purpose by design.** Use the same language for applications, command-line tools, libraries, and freestanding software.
- **Native and direct.** Compile to executables, WebAssembly modules, objects, assembly, LLVM IR, or bitcode without hiding target details.
- **Machine access when needed.** Use pointers, C ABI boundaries, inline assembly, and freestanding targets when system contracts must stay visible.
- **Modern language structure.** Organize code with modules, `pub` visibility, generics, structs, enums, variants, `proto`, and explicit `var` declarations.
- **Cross-target compilation.** Generate code for x86-64, AArch64, RISC-V 64, LoongArch64, and WebAssembly from supported compiler hosts.
- **Tool-friendly interfaces.** Query targets and compiler capabilities in human-readable or JSON form for build tools and editors.

Wave is under active pre-beta development. Syntax and toolchain contracts are being stabilized and may still change between releases.

## A first Wave program

```wave
fun main() {
    var language: str = "Wave";
    var count: i32 = 1;

    println("Hello from {} #{}", language, count);
}
```

Save this as `main.wave`, then run it directly:

```shell
wavec run main.wave
```

## Modules and Vex packages

Wave keeps each imported file in its own module namespace. A bare import names
a Vex dependency, a qualified package path names a source module, and `./`
explicitly names a file relative to the importing module:

```wave
import("add");
import("add::math");
import("./helpers" as helpers);
import("add")::{sum, Point};

fun main() {
    var qualified: i32 = add::sum(1, 2);
    var selected: i32 = sum(1, 2);
    var local: i32 = helpers::triple(3);
    var point: Point = Point();
}
```

A Wave variable declaration always names its type; `var value = ...` is rejected.

A dependency named `add` resolves to its canonical `src/lib.wave` entry;
`add::math` resolves to `src/math.wave`. Only declarations marked `pub` can be
selected or accessed through another module:

```wave
fun internal_sum(a: i32, b: i32) -> i32 { return a + b; }
pub fun sum(a: i32, b: i32) -> i32 { return internal_sum(a, b); }
pub struct Point {}
```

`pub` controls Wave module visibility and is independent from `export(c)`,
which controls the C ABI boundary. `main` is always a private entry point, so
`pub fun main()` is rejected. A module can deliberately forward public API with
`pub import("module")::{symbol};`.

## Install

Linux and macOS:

```shell
curl -fsSL https://wave-lang.dev/install.sh | bash -s -- latest
```

Windows PowerShell:

```powershell
irm https://wave-lang.dev/install.ps1 -OutFile install.ps1
powershell -ExecutionPolicy Bypass -File .\install.ps1 -Latest
```

See the [installation guide](https://wave-lang.dev/docs/getting-started/install) for platform requirements and release selection.

Release archives are built and smoke-tested natively for Linux x86-64 and
AArch64, macOS x86-64 and AArch64, and Windows x86-64 and ARM64. Linux
RISC-V64 archives are built inside a native-architecture userspace under QEMU
and executed there before publication. Linux LoongArch64 archives use the
official Loongson cross-toolchain to build a native compiler and execute that
compiler through QEMU before publication.

## Use `wavec`

```shell
# Check without producing a binary.
wavec check main.wave

# Build and run.
wavec run main.wave -- arg1 arg2

# Build an optimized hosted executable.
wavec -O2 build main.wave -o app

# Emit a freestanding RISC-V object.
wavec --target=riscv64-unknown-none-elf build kernel.wave --freestanding --emit=obj

# Build a browser-hosted module or a WASI Preview 1 command.
wavec --target=wasm32-unknown-unknown build module.wave
wavec --target=wasm32-wasip1 build command.wave
wavec --target=wasm32-wasip1 run command.wave
wavec --target=wasm64-unknown-unknown run memory64-module.wave
```

The compiler exposes its current capabilities instead of requiring tools to maintain hard-coded lists:

```shell
wavec print supported-targets
wavec print supported-input-types
wavec print supported-emit-kinds
wavec print target-spec --target riscv64-unknown-linux-gnu --format=json
```

Run `wavec --help` for the complete CLI contract.

## Target families

| Architecture | Hosted targets | Freestanding target |
| --- | --- | --- |
| x86-64 | Linux GNU, macOS, Windows GNU | `x86_64-unknown-none-elf` |
| AArch64 | Linux GNU, macOS, Windows GNU | `aarch64-unknown-none-elf` |
| RISC-V 64 | Linux GNU | `riscv64-unknown-none-elf` |
| LoongArch64 | Linux GNU (LP64S, LP64D; LP64F object ABI) | — |
| WebAssembly 32 | WASI Preview 1 | `wasm32-unknown-unknown` |
| WebAssembly 64 (Memory64) | — | `wasm64-unknown-unknown` |

Hosted cross-linking requires a compatible linker, system libraries, and sysroot for the selected target. For Linux RISC-V 64 and LoongArch64, `wavec` discovers complete cross-toolchain sysroots automatically; an explicit `--sysroot` always takes precedence. Linux LoongArch64 defaults to LP64D, also implements the LP64S soft-float contract, ships Wave-owned startup objects for LP64S/LP64F/LP64D, and validates input ELF ABI flags before linking. LP64F object emission is available for toolchain work, but hosted LP64F linking is rejected because glibc does not provide that ABI. The LoongArch CI sysroot is pinned to Loongson's official cross-toolchain release, and releases include a native LoongArch64 compiler package. Windows ARM64 binaries use an `aarch64-w64-mingw32-gcc` driver and runtime; set `WAVE_WINDOWS_ARM64_LINKER` or pass `-C linker=<path>` when the driver is not in `PATH`. WebAssembly builds use `wasm-ld`; bare wasm32/wasm64 modules import host functions from `env`, while `wasm32-wasip1` emits a `_start` command entry and WASI Preview 1 imports. `wavec run` executes WebAssembly through Node.js and enables Memory64 for wasm64; WASI commands receive the current directory as their `.` preopen. A 64-bit WASI target is not advertised until its ABI and host runtime are stable. Freestanding builds omit the default hosted runtime assumptions and are intended for kernels, firmware, boot code, and other no-OS environments.

The WebAssembly backend supports 32-bit and 64-bit (Memory64) pointers and linear
memory, plus Wave control flow, structs, arrays, enums, payload variants, generics,
stable `extern(c)`/`export(c)` host
boundaries, and the portable standard-library layers backed by WASI descriptor
I/O, preopened paths, clocks, sleep, environment access, process exit, and a
single-threaded allocator. WASI Preview 1 does not provide POSIX process trees,
native sockets, or terminal control; process-tree calls report unsupported, and
socket and terminal providers are not exposed on WASI.

## Build from source

Follow the [Wave development setup](https://github.com/wavefnd/setup), then build with the locked dependency graph:

```shell
git clone https://github.com/wavefnd/Wave.git
cd Wave
cargo build --locked
```

The development compiler is written to `target/debug/wavec`. Before submitting compiler changes, run:

```shell
cargo fmt --all --check
cargo test --locked --all-targets
cargo clippy --locked --all-targets -- -D warnings
python3 tools/run_tests.py
```

## Ecosystem

| Project | Role |
| --- | --- |
| [Wave](https://github.com/wavefnd/Wave) | Language frontend, compiler driver, LLVM backend, and standard library source |
| [Vex](https://github.com/wavefnd/Vex) | Manifest-based package manager and build tool |
| [Whale](https://github.com/wavefnd/Whale) | Native assembler, object tooling, and linker under development |

Useful project references:

- [Language documentation](https://wave-lang.dev/docs/)
- [Examples](examples/)
- [Contributing guide](CONTRIBUTING.md)
- [Versioning policy](VERSION.md)
- [Release process](RELEASING.md)
- [Issue tracker](https://github.com/wavefnd/Wave/issues)

## Contributing

Contributions are welcome through GitHub pull requests and email patches. Read [CONTRIBUTING.md](CONTRIBUTING.md) before submitting changes; all commits require a DCO `Signed-off-by` line.

## License

- The compiler and repository components outside [`std/`](std/) are licensed under the [Mozilla Public License 2.0](LICENSE).
- The standard library in [`std/`](std/) is licensed separately under the [Apache License 2.0](std/LICENSE), allowing modification, redistribution, and embedding under that license.

## Sponsors

Wave is developed in public with support from individuals and organizations. You can contribute monthly or once through [OpenCollective](https://opencollective.com/wave-lang/contribute).

<p align="center">
  <a href="https://opencollective.com/wave-lang#sponsors">
    <img src="https://opencollective.com/wave-lang/sponsors.svg?width=890&button=false" alt="Wave sponsors">
  </a>
  <br>
  <a href="https://opencollective.com/wave-lang#backers">
    <img src="https://opencollective.com/wave-lang/backers.svg?width=890&button=false" alt="Wave backers">
  </a>
</p>

Thank you to everyone who contributes code, documentation, testing, funding, or time to Wave.
