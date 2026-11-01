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

Wave is under active alpha development (`0.0.1-alpha-dev`). Syntax and toolchain contracts are being stabilized and may still change between releases.

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

Floating-point literals use their floating type context, such as an `f64`
declaration, parameter, or explicit cast. Without a floating type context, they
default to `f32`, including arguments passed directly to `print` or `println`.
Both `f32` and `f64` use binary floating-point rounding: decimal values such as
`0.1` need not be exact, and `f64` provides more precision rather than exact
decimal arithmetic. A literal that overflows its selected type to infinity is
a compilation error; ordinary rounding is allowed. Converting an existing
`f32` value to `f64` cannot recover precision already lost.

```wave
fun main() {
    var value: f64 = 1.23456789;
    println("{}", value);
    println("{}", 1.23456789 as f64);
    println("{:.2}", value);
}
```

For floating-point output, `{:.N}` selects `N` digits after the decimal point
(`{:.2}` prints `1.23` above), where `N` is a nonnegative decimal integer.
It controls display precision, not the stored type. Existing `{d}`, `{x}`, `{c}`,
`{s}`, and `{p}` placeholders remain available. Width, alignment, and fill
options are not supported.

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

Local imports stay within the importing file's directory and its descendants.
Parent traversal (`../` or `./../`) and symlinks that escape that directory are
rejected. For sibling `src` and `tests` directories, map the project root as a
dependency instead:

```text
project/
    src/number.wave
    tests/number_test.wave
```

In `src/number.wave`, expose the function to other modules:

```wave
pub fun double(value: i32) -> i32 {
    return value * 2;
}
```

In `tests/number_test.wave`, import the source module through that dependency:

```wave
import("pkg::number");

fun main() -> i32 {
    if (pkg::number::double(21) != 42) {
        return 1;
    }
    return 0;
}
```

Run from `project/` with `wavec run tests/number_test.wave --dep=pkg=.`.
This resolves `pkg::number` directly to `src/number.wave`; no `src/lib.wave`
entry or re-export is required. The imported declarations still need `pub`.

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

For source builds, use Meson with Cargo as described in [CONTRIBUTING](CONTRIBUTING.md).
Meson owns build, test and package automation; Cargo continues to compile the
Rust workspace.

The compiler uses the `std/` directory beside its executable when present,
then falls back to the legacy `~/.wave/lib/wave/std` installation. An explicit
`--std-root <path>` takes precedence over both. The selected library must pass
compatibility validation; an invalid selection does not silently fall back to
another installation. `wavec install std` and `wavec update std` continue to
manage the legacy per-user location.

Release archives are built and smoke-tested natively for Linux x86-64 and
AArch64, macOS x86-64 and AArch64, and Windows x86-64 and ARM64. Linux
RISC-V64 archives are built inside a native-architecture userspace under QEMU
and executed there before publication. Linux LoongArch64 archives use the
official Loongson cross-toolchain to build a native compiler and execute that
compiler through QEMU before publication.

## Use `wavec`

`wavec build` and `wavec run` default to `-O0` (unoptimized). For performance
comparisons, select an explicit optimization level such as `-O2` and compare
equivalent targets, inputs, and compiler settings. Results depend on the program
and workload.

```shell
# Check without producing a binary.
wavec check main.wave

# Build and run.
wavec run main.wave -- arg1 arg2

# Build an optimized hosted executable.
wavec -O2 build main.wave -o app

# Build and run with optimization.
wavec run main.wave -O2 -- arg1 arg2

# Emit a freestanding RISC-V object.
wavec --target=riscv64-unknown-none-elf build kernel.wave --freestanding --emit=obj

# Build a browser-hosted module or a WASI Preview 1 command.
wavec --target=wasm32-unknown-unknown build module.wave
wavec --target=wasm32-wasip1 build command.wave
wavec --target=wasm32-wasip1 run command.wave
wavec --target=wasm64-unknown-unknown run memory64-module.wave
```

Optimization flags can appear before or after the subcommand, before `--`.
Arguments after `--` are passed to the program.

The compiler exposes its current capabilities instead of requiring tools to maintain hard-coded lists:

```shell
wavec print supported-targets
wavec print supported-input-types
wavec print supported-emit-kinds
wavec print target-spec --target riscv64-unknown-linux-gnu --format=json
```

Run `wavec --help` for the complete CLI contract.

## Machine-readable diagnostics

Select compiler errors with `--error-format=json` or `--error-format=human`:

```shell
wavec --error-format=json check main.wave
wavec build main.wave --emit=ast --error-format=json
wavec --error-format=human check main.wave
```

The last valid selection before `--` takes precedence, including a build-local
selection after a global one. An invalid or missing format value is a usage
error; the last valid selection still controls how that failure is rendered.
This option selects diagnostics, independently of a query's `--format=json`
or an AST dump's `--ast-format=json`.

Compiler diagnostics are newline-delimited JSON records on **stderr**, one
object per diagnostic, with an `error` envelope. Normal query, debug and program
output remains on stdout. For example,
`wavec --error-format=json print unknown-item` exits with status 2 and emits:

```json
{"error":{"kind":"usage","message":"unknown print item: unknown-item","exit_code":2}}
```

There are two current payload families:

| Fields inside `error` | Meaning |
| --- | --- |
| `kind`, `message` | Machine category and human-readable explanation. Use the category rather than parsing English messages. |
| `exit_code` | Included for CLI errors, such as usage, missing tools and command failures. Source diagnostics use the process status instead. |
| `phase`, `operation` | Additional fields on CLI errors returned through the typed backend error channel. |
| `file`, `line`, `column`, `span_len` | Source-diagnostic location. Lines and columns are one-based Unicode scalar coordinates; zero means no source position. `span_len` is a scalar length for the primary line, with a minimum of one. |
| `span` | Nullable physical range: `file`, half-open UTF-8 byte offsets `start`/`end`, `line`/`column`, `end_line`/`end_column`, and an `expansion` array describing generated syntax. |
| `related` | Array of additional `{message, span}` locations, such as an unmatched opener or the importing declaration. |
| `severity`, `code` | Source diagnostic severity and nullable diagnostic code. Codes alone do not identify every failure category. |
| `label`, `context`, `found`, `note`, `help` | Nullable source-diagnostic details. |
| `expected`, `suggestions` | Arrays of expected syntax and suggested actions; either may be empty. |

Source payloads include these source fields; CLI payloads need not contain a
source location. EOF spans may be empty. End coordinates are exclusive, and
CRLF counts as one newline. Human carets use terminal display cells, so tabs,
wide characters and escaped controls can occupy a different width from JSON
columns. JSON preserves the original source coordinates and string values.

Consumers should accept nullable locations, empty arrays, unknown categories
and additional fields. Treat the documented envelope, field names and meanings
as the interface; message wording and optional detail can improve independently.
Read records line by line, without assuming a human summary or a single error.

The current process-status categories are:

- `0`: command succeeded.
- `1`: source compilation/check failure, failed external command, or an operation
  such as installing an already installed std. Source-read errors also use 1.
- `2`: invalid command-line usage.
- `3`: CLI I/O, missing required tools or unavailable home-directory settings.

`run`/`--run` also forwards the program's output and exit status (on Unix,
termination by a signal maps to `128 + signal`). Program stderr is not rewritten
as compiler JSON. Use `check` or a build without execution when consuming a pure
compiler-diagnostic stream.

## Experimental Whale backend

Development is now `0.0.1-alpha-dev`. LLVM remains the default backend. The
existing `--whale` flag selects the initial Linux amd64, O0 integration:

```shell
# Save as scalar.wave:
# fun main() -> i32 { var c: char = 'A'; match (c) { 65 => { return 0; } _ => { return 1; } } }
wavec --whale print supported-targets
wavec --whale print supported-emit-kinds --format=json
wavec --whale check scalar.wave
wavec --whale build scalar.wave --emit=ir --out-dir output
```

The build command writes verified Whale IR to `output/scalar.wir`. This path
supports scalar functions and direct calls, scalar aliases and global constants,
local declarations with tracked initialization, prepared trailing default arguments,
assignment (including `+=`, `-=`, `*=`, `/=`, `%=`),
`char` values, integer `match`, `if`/`else`, `while`/`for`, and scalar arithmetic and comparisons. Wave's typed
HIR supplies constant values, calculation types and ordered conversions.
Short-circuit logical operators preserve conditional evaluation. Checked shifts
and f32/f64-to-integer conversions support the existing 8–128-bit integer types.

This integration does not yet generate executables, objects, or assembly.
Aggregates, pointers, mutable globals, async, foreign calls, I/O, integers wider
than 128 bits are rejected explicitly. Integer division/remainder trap on zero;
signed MIN / -1 traps and MIN % -1 produces zero. Floating division/remainder
emit verified IR with existing f32/f64 semantics.
Unsupported requests never fall back to LLVM. `--whale check` also verifies
backend support, whereas the default `check` performs frontend validation.
The supported scalar/control-flow subset is tested with Whale's bounded
interpreter, including direct floating execution. This does not establish native
Whale execution or complete backend parity.

The root `whale/` workspace crate owns the Whale backend alongside `llvm/`.
The compiler driver in `src/` selects and invokes these backends. The `whale/`
crate uses upstream Whale IR as a Rust Git dependency from the official
repository's `master` branch.
`Cargo.lock` records the tested commit; `cargo build --locked` reuses it. Advancing
Whale uses `cargo update -p ir`, followed by integration tests and review of the
lockfile change. Wave does not require a separately installed Whale executable.

## Target families

The table below is a family overview. Registered triples and CI execution levels
are defined in `tests/cases/cases.toml` and `tools/ci/plans.json`.

| Architecture | Hosted targets | Freestanding target |
| --- | --- | --- |
| x86-64 | Linux GNU, macOS, Windows MSVC, FreeBSD | `x86_64-unknown-none-elf` |
| AArch64 | Linux GNU, macOS, Windows MSVC; FreeBSD emission | `aarch64-unknown-none-elf` |
| RISC-V 64 | Linux GNU; FreeBSD emission | `riscv64-unknown-none-elf` |
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
- [Examples and runnable learning projects](examples/README.md)
- [Documentation source and contributions](https://github.com/wavefnd/wave-platform)
- [Public standard-library API](std/API.md)
- [Standard-library system capabilities](std/SYSTEMS.md)
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
