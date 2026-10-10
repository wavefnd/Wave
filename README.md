<div align="center">
  <a href="https://wave-lang.dev/">
    <img src="https://wave-lang.dev/img/wave.svg" width="128" alt="Wave programming language logo">
  </a>

---

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
    <a href="https://github.com/wavefnd/Wave/actions/workflows/ci.yml"><img src="https://img.shields.io/github/actions/workflow/status/wavefnd/Wave/ci.yml?branch=master&style=flat-square&label=build&labelColor=17132B&color=6654F1" alt="Build status"></a>
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
curl -fsSL https://wave-lang.dev/install.sh | bash
```

Windows PowerShell:

```powershell
irm https://wave-lang.dev/install.ps1 -OutFile install.ps1; powershell -ExecutionPolicy Bypass -File .\install.ps1
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

## Experimental Whale backend

Development is now `0.0.1-alpha-dev`. LLVM remains the default backend. The
existing `--whale` flag selects the initial Linux amd64, O0 integration:

```shell
wavec --whale check sample.wave
wavec --whale build sample.wave --emit=ir --out-dir output
```

The second command writes verified Whale IR to `output/sample.wir`. This path
supports scalar functions and direct calls, initialized local variables,
assignment, `if`/`else`, `while`, and scalar arithmetic and comparisons. Wave's
typed HIR supplies calculation types and ordered conversions. Short-circuit
logical operators preserve conditional evaluation.

This integration does not yet generate executables, objects, or assembly.
Aggregates, pointers, async, foreign calls, I/O, integers wider than 128 bits,
checked division/shifts, and float-to-integer conversions are rejected explicitly.
Unsupported requests never fall back to LLVM. `--whale check` also verifies
backend support, whereas the default `check` performs frontend validation.

The root `whale/` workspace crate owns the Whale backend alongside `llvm/`.
The compiler driver in `src/` selects and invokes these backends. The `whale/`
crate uses upstream Whale IR as a Rust Git dependency from the official
repository's `master` branch.
`Cargo.lock` records the tested commit; `cargo build --locked` reuses it. Advancing
Whale uses `cargo update -p ir`, followed by integration tests and review of the
lockfile change. Wave does not require a separately installed Whale executable.

## Target families

| Architecture | Hosted targets | Freestanding target |
| --- | --- | --- |
| x86-64 | Linux GNU, macOS, Windows MSVC | `x86_64-unknown-none-elf` |
| AArch64 | Linux GNU, macOS, Windows MSVC | `aarch64-unknown-none-elf` |
| RISC-V 64 | Linux GNU | `riscv64-unknown-none-elf` |
| LoongArch64 | Linux GNU (LP64S, LP64D; LP64F object ABI) | — |
| WebAssembly 32 | WASI Preview 1 | `wasm32-unknown-unknown` |
| WebAssembly 64 (Memory64) | — | `wasm64-unknown-unknown` |

Hosted cross-linking requires a compatible linker, system libraries, and sysroot for the selected target. For Linux RISC-V 64 and LoongArch64, `wavec` discovers complete cross-toolchain sysroots automatically; an explicit `--sysroot` always takes precedence. Linux LoongArch64 defaults to LP64D, also implements the LP64S soft-float contract, ships Wave-owned startup objects for LP64S/LP64F/LP64D, and validates input ELF ABI flags before linking. LP64F object emission is available for toolchain work, but hosted LP64F linking is rejected because glibc does not provide that ABI. The LoongArch CI sysroot is pinned to Loongson's official cross-toolchain release, and releases include a native LoongArch64 compiler package. Windows uses native MSVC targets on amd64 and ARM64; see the installation prerequisites below. WebAssembly builds use `wasm-ld`; bare wasm32/wasm64 modules import host functions from `env`, while `wasm32-wasip1` emits a `_start` command entry and WASI Preview 1 imports. `wavec run` executes WebAssembly through Node.js and enables Memory64 for wasm64; WASI commands receive the current directory as their `.` preopen. A 64-bit WASI target is not advertised until its ABI and host runtime are stable. Freestanding builds omit the default hosted runtime assumptions and are intended for kernels, firmware, boot code, and other no-OS environments.

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

The development compiler is written to `target/debug/wavec`. Before submitting
compiler changes, follow the [local verification guide](CONTRIBUTING.md#4-local-verification)
for the relevant checks and current CI commands.

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

New to Wave? See [choosing a first contribution](CONTRIBUTING.md#choosing-a-first-contribution)
for help finding a small task. Prose and link fixes do not require a compiler
toolchain; use the [documentation-only checks](CONTRIBUTING.md#documentation-only-changes).
Ask usage and contribution questions in [GitHub Discussions](https://github.com/wavefnd/Wave/discussions),
or get informal help on [Discord](https://discord.gg/3nev5nHqq9).

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

### Windows MSVC installation

Windows packages require native x64 or ARM64 Windows, matching Visual Studio
Build Tools C++ libraries, a Windows 10/11 SDK (UM and UCRT), and the matching
Visual C++ v14 Redistributable. The package includes LLVM 21 link/codegen tools,
compiler-rt arithmetic builtins, Wave std and redistribution notices. Windows SDK
and Visual Studio libraries are discovered from their installed locations and
are not redistributed in the archive.

Extract the archive and add its root to `PATH`. Copy the included `std` directory
to `%USERPROFILE%\.wave\lib\wave\std`. Ordinary `wavec build` and `wavec run`
use `x86_64-pc-windows-msvc` or `aarch64-pc-windows-msvc` for the native host.
Installed SDK/Build Tools discovery works from a normal shell; a matching
Developer Prompt or explicit `-L` library paths can select a specific toolchain.
Missing SDK libraries produce a diagnostic rather than selecting another ABI.

Windows GNU/MinGW triples are retired. Replace old `*-windows-gnu` output targets
with the corresponding `*-pc-windows-msvc` target and rebuild linked objects and
libraries for the MSVC ABI. Linux GNU targets are unaffected.
