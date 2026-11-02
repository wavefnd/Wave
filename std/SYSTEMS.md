# Standard-library system capabilities

Compiler target acceptance and std provider coverage are separate. `source` below
means provider code or a target-selected facade is present; it does not promise every
operation is implemented or runtime-tested. `limited` has explicit unsupported paths.
Native/QEMU/VM describes the configured CI execution route, not universal API coverage.
Exact declarations and target attributes are in [the public inventory](API.md).

| Platform | Compiler | fs/process/memory/time | env | sockets/tty/random | Signals | CI route |
|---|---|---|---|---|---|---|
| Linux amd64/arm64 | registered | source | source | source | unsupported raw signal API | native |
| Linux riscv64/loong64 | registered | source | source | source | unsupported raw signal API | QEMU user |
| macOS amd64/arm64 | registered | source | source; dedicated boundary regression | source | unsupported raw signal API | native |
| Windows amd64/arm64 | registered MSVC | source; some process/console operations limited | source | source; interface enumeration limited | separate console-control contract pending | native |
| FreeBSD amd64 | registered | source; incomplete operations remain | source/limited | source/limited | unsupported raw signal API | VM |
| FreeBSD arm64/riscv64 | registered | source contracts; not native-validated | source contracts | partial source contracts | unsupported | compile only |
| wasm32 WASI Preview 1 | registered | WASI fs/clocks/memory, limited process | source | sockets/tty unsupported; randomness reports NOSYS | unsupported | Node WASI |
| bare wasm32/wasm64 | registered | linear memory; hosted OS operations unsupported | unsupported | unsupported | unsupported | Node host, Memory64 for wasm64 |

Linux startup objects are owned by the compiler CRT; native Darwin/Windows/FreeBSD
linking requires the platform runtime. WASI `_start` is compiler-generated.
Freestanding object emission supplies no boot/startup contract. Other BSDs, Android,
iOS, Fuchsia and wasm64 WASI are planned and have no supported runtime claim here.

Remaining work: [Windows providers #385](https://github.com/wavefnd/Wave/issues/385),
[FreeBSD #386](https://github.com/wavefnd/Wave/issues/386),
[signals #384](https://github.com/wavefnd/Wave/issues/384),
[Linux interfaces #566](https://github.com/wavefnd/Wave/issues/566),
[Windows interfaces #790](https://github.com/wavefnd/Wave/issues/790).

## Provider source index

Every current `std::sys` source module is listed below. Facades preserve target
attributes and re-exports; an implementation with an unavailable path is not classified
as fully implemented from its filename. Follow its exact declarations in the API inventory.

| Module | Source role |
|---|---|
| [sys/env.wave](sys/env.wave) | target-selected facade/re-export; unavailability markers present: inspect the operation contract; runtime coverage must be checked separately |
| [sys/errors.wave](sys/errors.wave) | implementation source; runtime coverage must be checked separately |
| [sys/event.wave](sys/event.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/amd64/env.wave](sys/freebsd/amd64/env.wave) | target-selected facade/re-export; unavailability markers present: inspect the operation contract; runtime coverage must be checked separately |
| [sys/freebsd/amd64/fs.wave](sys/freebsd/amd64/fs.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/amd64/memory.wave](sys/freebsd/amd64/memory.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/amd64/process.wave](sys/freebsd/amd64/process.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/amd64/random.wave](sys/freebsd/amd64/random.wave) | implementation source; runtime coverage must be checked separately |
| [sys/freebsd/amd64/socket.wave](sys/freebsd/amd64/socket.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/amd64/syscall.wave](sys/freebsd/amd64/syscall.wave) | implementation source; runtime coverage must be checked separately |
| [sys/freebsd/amd64/time.wave](sys/freebsd/amd64/time.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/amd64/tty.wave](sys/freebsd/amd64/tty.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/arm64/env.wave](sys/freebsd/arm64/env.wave) | target-selected facade/re-export; unavailability markers present: inspect the operation contract; runtime coverage must be checked separately |
| [sys/freebsd/arm64/fs.wave](sys/freebsd/arm64/fs.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/arm64/memory.wave](sys/freebsd/arm64/memory.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/arm64/process.wave](sys/freebsd/arm64/process.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/arm64/socket.wave](sys/freebsd/arm64/socket.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/arm64/syscall.wave](sys/freebsd/arm64/syscall.wave) | implementation source; runtime coverage must be checked separately |
| [sys/freebsd/arm64/time.wave](sys/freebsd/arm64/time.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/arm64/tty.wave](sys/freebsd/arm64/tty.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/common/env.wave](sys/freebsd/common/env.wave) | implementation source; unavailability markers present: inspect the operation contract; runtime coverage must be checked separately |
| [sys/freebsd/common/fs.wave](sys/freebsd/common/fs.wave) | implementation source; runtime coverage must be checked separately |
| [sys/freebsd/common/memory.wave](sys/freebsd/common/memory.wave) | implementation source; runtime coverage must be checked separately |
| [sys/freebsd/common/process.wave](sys/freebsd/common/process.wave) | implementation source; runtime coverage must be checked separately |
| [sys/freebsd/common/socket.wave](sys/freebsd/common/socket.wave) | implementation source; runtime coverage must be checked separately |
| [sys/freebsd/common/time.wave](sys/freebsd/common/time.wave) | implementation source; runtime coverage must be checked separately |
| [sys/freebsd/common/tty.wave](sys/freebsd/common/tty.wave) | implementation source; runtime coverage must be checked separately |
| [sys/freebsd/env.wave](sys/freebsd/env.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/event.wave](sys/freebsd/event.wave) | implementation source; runtime coverage must be checked separately |
| [sys/freebsd/fs.wave](sys/freebsd/fs.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/interfaces.wave](sys/freebsd/interfaces.wave) | implementation source; runtime coverage must be checked separately |
| [sys/freebsd/local_addr.wave](sys/freebsd/local_addr.wave) | implementation source; runtime coverage must be checked separately |
| [sys/freebsd/memory.wave](sys/freebsd/memory.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/process.wave](sys/freebsd/process.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/resolver.wave](sys/freebsd/resolver.wave) | implementation source; runtime coverage must be checked separately |
| [sys/freebsd/riscv64/env.wave](sys/freebsd/riscv64/env.wave) | target-selected facade/re-export; unavailability markers present: inspect the operation contract; runtime coverage must be checked separately |
| [sys/freebsd/riscv64/fs.wave](sys/freebsd/riscv64/fs.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/riscv64/memory.wave](sys/freebsd/riscv64/memory.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/riscv64/process.wave](sys/freebsd/riscv64/process.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/riscv64/socket.wave](sys/freebsd/riscv64/socket.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/riscv64/syscall.wave](sys/freebsd/riscv64/syscall.wave) | implementation source; runtime coverage must be checked separately |
| [sys/freebsd/riscv64/time.wave](sys/freebsd/riscv64/time.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/riscv64/tty.wave](sys/freebsd/riscv64/tty.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/socket.wave](sys/freebsd/socket.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/socket_addr.wave](sys/freebsd/socket_addr.wave) | implementation source; runtime coverage must be checked separately |
| [sys/freebsd/syscall.wave](sys/freebsd/syscall.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/time.wave](sys/freebsd/time.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/tty.wave](sys/freebsd/tty.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/freebsd/vector_io.wave](sys/freebsd/vector_io.wave) | implementation source; runtime coverage must be checked separately |
| [sys/fs.wave](sys/fs.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/interfaces.wave](sys/interfaces.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/linux/amd64/env.wave](sys/linux/amd64/env.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/amd64/fs.wave](sys/linux/amd64/fs.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/amd64/memory.wave](sys/linux/amd64/memory.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/amd64/process.wave](sys/linux/amd64/process.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/amd64/socket.wave](sys/linux/amd64/socket.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/amd64/syscall.wave](sys/linux/amd64/syscall.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/amd64/time.wave](sys/linux/amd64/time.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/amd64/tty.wave](sys/linux/amd64/tty.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/arm64/env.wave](sys/linux/arm64/env.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/arm64/fs.wave](sys/linux/arm64/fs.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/arm64/memory.wave](sys/linux/arm64/memory.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/arm64/process.wave](sys/linux/arm64/process.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/arm64/socket.wave](sys/linux/arm64/socket.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/arm64/syscall.wave](sys/linux/arm64/syscall.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/arm64/time.wave](sys/linux/arm64/time.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/arm64/tty.wave](sys/linux/arm64/tty.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/env.wave](sys/linux/env.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/linux/event.wave](sys/linux/event.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/fs.wave](sys/linux/fs.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/linux/interfaces.wave](sys/linux/interfaces.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/local_addr.wave](sys/linux/local_addr.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/loong64/env.wave](sys/linux/loong64/env.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/linux/loong64/fs.wave](sys/linux/loong64/fs.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/linux/loong64/memory.wave](sys/linux/loong64/memory.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/linux/loong64/process.wave](sys/linux/loong64/process.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/linux/loong64/socket.wave](sys/linux/loong64/socket.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/linux/loong64/syscall.wave](sys/linux/loong64/syscall.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/loong64/time.wave](sys/linux/loong64/time.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/linux/loong64/tty.wave](sys/linux/loong64/tty.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/linux/memory.wave](sys/linux/memory.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/linux/process.wave](sys/linux/process.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/linux/resolver.wave](sys/linux/resolver.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/riscv64/env.wave](sys/linux/riscv64/env.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/riscv64/fs.wave](sys/linux/riscv64/fs.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/riscv64/memory.wave](sys/linux/riscv64/memory.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/riscv64/process.wave](sys/linux/riscv64/process.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/riscv64/socket.wave](sys/linux/riscv64/socket.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/riscv64/syscall.wave](sys/linux/riscv64/syscall.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/riscv64/time.wave](sys/linux/riscv64/time.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/riscv64/tty.wave](sys/linux/riscv64/tty.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/socket.wave](sys/linux/socket.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/linux/socket_addr.wave](sys/linux/socket_addr.wave) | implementation source; runtime coverage must be checked separately |
| [sys/linux/syscall.wave](sys/linux/syscall.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/linux/time.wave](sys/linux/time.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/linux/tty.wave](sys/linux/tty.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/linux/vector_io.wave](sys/linux/vector_io.wave) | implementation source; runtime coverage must be checked separately |
| [sys/local_addr.wave](sys/local_addr.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/macos/amd64/env.wave](sys/macos/amd64/env.wave) | target-selected facade/re-export; unavailability markers present: inspect the operation contract; runtime coverage must be checked separately |
| [sys/macos/amd64/fs.wave](sys/macos/amd64/fs.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/amd64/memory.wave](sys/macos/amd64/memory.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/amd64/process.wave](sys/macos/amd64/process.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/amd64/socket.wave](sys/macos/amd64/socket.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/amd64/syscall.wave](sys/macos/amd64/syscall.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/amd64/time.wave](sys/macos/amd64/time.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/amd64/tty.wave](sys/macos/amd64/tty.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/arm64/env.wave](sys/macos/arm64/env.wave) | target-selected facade/re-export; unavailability markers present: inspect the operation contract; runtime coverage must be checked separately |
| [sys/macos/arm64/fs.wave](sys/macos/arm64/fs.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/arm64/memory.wave](sys/macos/arm64/memory.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/arm64/process.wave](sys/macos/arm64/process.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/arm64/socket.wave](sys/macos/arm64/socket.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/arm64/syscall.wave](sys/macos/arm64/syscall.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/arm64/time.wave](sys/macos/arm64/time.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/arm64/tty.wave](sys/macos/arm64/tty.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/common/env.wave](sys/macos/common/env.wave) | implementation source; unavailability markers present: inspect the operation contract; runtime coverage must be checked separately |
| [sys/macos/env.wave](sys/macos/env.wave) | target-selected facade/re-export; unavailability markers present: inspect the operation contract; runtime coverage must be checked separately |
| [sys/macos/event.wave](sys/macos/event.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/fs.wave](sys/macos/fs.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/macos/interfaces.wave](sys/macos/interfaces.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/local_addr.wave](sys/macos/local_addr.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/memory.wave](sys/macos/memory.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/macos/process.wave](sys/macos/process.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/macos/resolver.wave](sys/macos/resolver.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/socket.wave](sys/macos/socket.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/macos/socket_addr.wave](sys/macos/socket_addr.wave) | implementation source; runtime coverage must be checked separately |
| [sys/macos/syscall.wave](sys/macos/syscall.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/macos/time.wave](sys/macos/time.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/macos/tty.wave](sys/macos/tty.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/macos/vector_io.wave](sys/macos/vector_io.wave) | implementation source; runtime coverage must be checked separately |
| [sys/memory.wave](sys/memory.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/process.wave](sys/process.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/random.wave](sys/random.wave) | target-selected facade/re-export; unavailability markers present: inspect the operation contract; runtime coverage must be checked separately |
| [sys/resolver.wave](sys/resolver.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/socket.wave](sys/socket.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/socket_addr.wave](sys/socket_addr.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/time.wave](sys/time.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/tty.wave](sys/tty.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/vector_io.wave](sys/vector_io.wave) | target-selected facade/re-export; runtime coverage must be checked separately |
| [sys/wasm/env.wave](sys/wasm/env.wave) | implementation source; runtime coverage must be checked separately |
| [sys/wasm/fs.wave](sys/wasm/fs.wave) | implementation source; unavailability markers present: inspect the operation contract; runtime coverage must be checked separately |
| [sys/wasm/io.wave](sys/wasm/io.wave) | implementation source; runtime coverage must be checked separately |
| [sys/wasm/memory.wave](sys/wasm/memory.wave) | implementation source; runtime coverage must be checked separately |
| [sys/wasm/process.wave](sys/wasm/process.wave) | implementation source; unavailability markers present: inspect the operation contract; runtime coverage must be checked separately |
| [sys/wasm/time.wave](sys/wasm/time.wave) | implementation source; runtime coverage must be checked separately |
| [sys/wasm64/memory.wave](sys/wasm64/memory.wave) | implementation source; runtime coverage must be checked separately |
| [sys/wasm_blocks.wave](sys/wasm_blocks.wave) | implementation source; runtime coverage must be checked separately |
| [sys/windows/completion_port.wave](sys/windows/completion_port.wave) | implementation source; runtime coverage must be checked separately |
| [sys/windows/env.wave](sys/windows/env.wave) | implementation source; runtime coverage must be checked separately |
| [sys/windows/event.wave](sys/windows/event.wave) | implementation source; runtime coverage must be checked separately |
| [sys/windows/fs.wave](sys/windows/fs.wave) | implementation source; unavailability markers present: inspect the operation contract; runtime coverage must be checked separately |
| [sys/windows/fs_errors.wave](sys/windows/fs_errors.wave) | implementation source; runtime coverage must be checked separately |
| [sys/windows/interfaces.wave](sys/windows/interfaces.wave) | implementation source; runtime coverage must be checked separately |
| [sys/windows/iocp.wave](sys/windows/iocp.wave) | implementation source; runtime coverage must be checked separately |
| [sys/windows/local_addr.wave](sys/windows/local_addr.wave) | implementation source; runtime coverage must be checked separately |
| [sys/windows/memory.wave](sys/windows/memory.wave) | implementation source; runtime coverage must be checked separately |
| [sys/windows/path_encoding.wave](sys/windows/path_encoding.wave) | implementation source; runtime coverage must be checked separately |
| [sys/windows/process.wave](sys/windows/process.wave) | implementation source; unavailability markers present: inspect the operation contract; runtime coverage must be checked separately |
| [sys/windows/random.wave](sys/windows/random.wave) | implementation source; runtime coverage must be checked separately |
| [sys/windows/resolver.wave](sys/windows/resolver.wave) | implementation source; runtime coverage must be checked separately |
| [sys/windows/socket.wave](sys/windows/socket.wave) | implementation source; runtime coverage must be checked separately |
| [sys/windows/socket_addr.wave](sys/windows/socket_addr.wave) | implementation source; runtime coverage must be checked separately |
| [sys/windows/time.wave](sys/windows/time.wave) | implementation source; runtime coverage must be checked separately |
| [sys/windows/timed_socket.wave](sys/windows/timed_socket.wave) | implementation source; runtime coverage must be checked separately |
| [sys/windows/tty.wave](sys/windows/tty.wave) | implementation source; unavailability markers present: inspect the operation contract; runtime coverage must be checked separately |
| [sys/windows/vector_io.wave](sys/windows/vector_io.wave) | implementation source; runtime coverage must be checked separately |

Refresh this index with `wave-tools maintain std-api` after adding a provider.
Review the capability summary and exact target/CI table when its contract changes.
