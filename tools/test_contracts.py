# This file is part of the Wave language project.
# Copyright (c) 2024–2026 Wave Foundation
# Copyright (c) 2024–2026 LunaStev and contributors
#
# This Source Code Form is subject to the terms of the
# Mozilla Public License, v. 2.0.
# If a copy of the MPL was not distributed with this file,
# You can obtain one at https://mozilla.org/MPL/2.0/.
#
# SPDX-License-Identifier: MPL-2.0
# AI TRAINING NOTICE: Prohibited without prior written permission. No use for machine learning or generative AI training, fine-tuning, distillation, embedding, or dataset creation.

from dataclasses import dataclass
from pathlib import Path, PurePosixPath, PureWindowsPath
import difflib
import json
import os
import re


ARCH_ALIASES = {
    "amd64": "x86_64",
    "arm64": "aarch64",
    "loong64": "loongarch64",
}

ELF_MACHINES = {
    "x86_64": 62,
    "aarch64": 183,
    "riscv64": 243,
    "loongarch64": 258,
}

RISCV_FLOAT_ABI_FLAGS = {
    "lp64": 0x0,
    "lp64f": 0x2,
    "lp64d": 0x4,
}

LOONGARCH_FLOAT_ABI_FLAGS = {
    "lp64s": 0x1,
    "lp64f": 0x2,
    "lp64d": 0x3,
}

ARTIFACT_SUFFIXES = {
    "obj": ".o",
    "asm": ".s",
    "ir": ".ll",
    "bc": ".bc",
}


@dataclass(frozen=True)
class TestMetadata:
    mode: str = "run"
    runner: str = "native"
    stdin: str | None = None
    stdin_file: str | None = None
    stdout_file: str | None = None
    stderr_file: str | None = None
    env_file: str | None = None
    runtime: str = "portable"
    requires: tuple[str, ...] = ()
    target: str | None = None
    emit: str = "obj"
    freestanding: bool = False
    expected_exit: int = 0
    udp_input: bool = False
    object_arch: str | None = None
    object_bits: int | None = None
    riscv_float_abi: str | None = None
    loongarch_float_abi: str | None = None
    asm_contains: tuple[str, ...] = ()
    asm_not_contains: tuple[str, ...] = ()


def normalize_arch(arch: str) -> str:
    lowered = arch.lower()
    return ARCH_ALIASES.get(lowered, lowered)


def _parse_bool(key: str, value: str, display_path: str) -> bool:
    lowered = value.lower()
    if lowered in {"1", "true", "yes"}:
        return True
    if lowered in {"0", "false", "no"}:
        return False
    raise ValueError(
        f"metadata '{key}' in {display_path} expects true/false, found '{value}'"
    )


def _parse_int(key: str, value: str, display_path: str) -> int:
    try:
        return int(value)
    except ValueError as error:
        raise ValueError(
            f"metadata '{key}' in {display_path} expects an integer, found '{value}'"
        ) from error


def _parse_patterns(key: str, value: str, display_path: str) -> tuple[str, ...]:
    patterns = tuple(pattern.strip() for pattern in value.split("|") if pattern.strip())
    if not patterns:
        raise ValueError(f"metadata '{key}' in {display_path} must not be empty")
    return patterns


def parse_test_metadata(path: Path, display_path: str | None = None) -> TestMetadata:
    display = display_path or str(path)
    values = {}
    marker = "// wave-test:"

    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except (OSError, UnicodeDecodeError) as error:
        raise ValueError(f"failed to read wave-test metadata from {display}: {error}") from error

    for line in lines:
        stripped = line.strip()
        if not stripped.startswith("//"):
            if stripped:
                break
            continue
        if not stripped.startswith(marker):
            continue

        body = stripped[len(marker):].strip()
        if not body:
            raise ValueError(f"empty wave-test metadata in {display}")

        for raw_item in body.split(","):
            item = raw_item.strip()
            if not item or "=" not in item:
                raise ValueError(f"malformed wave-test metadata '{item}' in {display}")
            key, value = (part.strip() for part in item.split("=", 1))
            if not key or not value:
                raise ValueError(f"malformed wave-test metadata '{item}' in {display}")
            if key in values:
                raise ValueError(f"duplicate wave-test metadata key '{key}' in {display}")
            values[key] = value

    converters = {
        "mode": lambda value: value.lower(),
        "runner": lambda value: value.lower(),
        "stdin": str,
        "stdin-file": str,
        "stdout-file": str,
        "stderr-file": str,
        "env-file": str,
        "runtime": lambda value: value.lower(),
        "requires": lambda value: _parse_patterns("requires", value, display),
        "target": str,
        "emit": lambda value: value.lower(),
        "freestanding": lambda value: _parse_bool("freestanding", value, display),
        "expected-exit": lambda value: _parse_int("expected-exit", value, display),
        "udp-input": lambda value: _parse_bool("udp-input", value, display),
        "object-arch": normalize_arch,
        "object-bits": lambda value: _parse_int("object-bits", value, display),
        "riscv-float-abi": lambda value: value.lower(),
        "loongarch-float-abi": lambda value: value.lower(),
        "asm-contains": lambda value: _parse_patterns("asm-contains", value, display),
        "asm-not-contains": lambda value: _parse_patterns(
            "asm-not-contains", value, display
        ),
    }

    converted = {}
    field_names = {
        "stdin-file": "stdin_file",
        "stdout-file": "stdout_file",
        "stderr-file": "stderr_file",
        "env-file": "env_file",
        "expected-exit": "expected_exit",
        "udp-input": "udp_input",
        "object-arch": "object_arch",
        "object-bits": "object_bits",
        "riscv-float-abi": "riscv_float_abi",
        "loongarch-float-abi": "loongarch_float_abi",
        "asm-contains": "asm_contains",
        "asm-not-contains": "asm_not_contains",
    }
    for key, value in values.items():
        converter = converters.get(key)
        if converter is None:
            raise ValueError(f"unsupported wave-test metadata key '{key}' in {display}")
        converted[field_names.get(key, key)] = converter(value)

    metadata = TestMetadata(**converted)
    compile_keys = {
        "target",
        "emit",
        "freestanding",
        "object-arch",
        "object-bits",
        "riscv-float-abi",
        "loongarch-float-abi",
        "asm-contains",
        "asm-not-contains",
    }
    if compile_keys.intersection(values) and metadata.runner != "compile":
        raise ValueError(
            f"compile artifact metadata requires runner 'compile' in {display}"
        )
    validate_test_metadata(metadata, display)
    return metadata


def validate_test_metadata(metadata: TestMetadata, display_path: str) -> None:
    if metadata.mode not in {"run", "check", "build"}:
        raise ValueError(
            f"unsupported wave-test mode '{metadata.mode}' in {display_path}"
        )
    if metadata.runner not in {"native", "server", "compile"}:
        raise ValueError(
            f"unsupported wave-test runner '{metadata.runner}' in {display_path}"
        )
    if (metadata.mode == "build") != (metadata.runner == "compile"):
        raise ValueError(
            f"mode 'build' and runner 'compile' must be used together in {display_path}"
        )
    if metadata.runner == "server" and metadata.mode != "run":
        raise ValueError(f"runner 'server' requires mode=run in {display_path}")

    compile_only_fields = any(
        (
            metadata.target,
            metadata.freestanding,
            metadata.object_arch,
            metadata.object_bits,
            metadata.riscv_float_abi,
            metadata.loongarch_float_abi,
            metadata.asm_contains,
            metadata.asm_not_contains,
        )
    )
    if compile_only_fields and metadata.runner != "compile":
        raise ValueError(
            f"compile artifact metadata requires runner 'compile' in {display_path}"
        )
    if metadata.runner == "compile" and metadata.emit not in ARTIFACT_SUFFIXES:
        raise ValueError(
            f"unsupported compile artifact emit '{metadata.emit}' in {display_path}"
        )
    if metadata.udp_input and (metadata.runner != "native" or metadata.mode != "run"):
        raise ValueError(
            f"udp-input requires native run mode in {display_path}"
        )
    if metadata.runtime not in {"portable", "native", "compile"}:
        raise ValueError(f"unsupported runtime portability in {display_path}: {metadata.runtime}")
    capabilities = {"stdio", "filesystem", "environment", "clock", "sockets", "process"}
    if set(metadata.requires) - capabilities or len(set(metadata.requires)) != len(metadata.requires):
        raise ValueError(f"invalid or repeated runtime capability in {display_path}")
    for field in ("stdin_file", "stdout_file", "stderr_file", "env_file"):
        value = getattr(metadata, field)
        if value is not None:
            validate_fixture_name(value)
            if metadata.runner != "native" or metadata.mode != "run":
                raise ValueError(f"{field.replace('_', '-')} requires native run mode in {display_path}")
    if metadata.stdin is not None and metadata.stdin_file is not None:
        raise ValueError(f"stdin and stdin-file are mutually exclusive in {display_path}")
    if metadata.stdin is not None and (
        metadata.runner != "native" or metadata.mode != "run"
    ):
        raise ValueError(f"stdin requires native run mode in {display_path}")

    object_contract = any(
        (
            metadata.object_arch,
            metadata.object_bits,
            metadata.riscv_float_abi,
            metadata.loongarch_float_abi,
        )
    )
    if object_contract and metadata.emit != "obj":
        raise ValueError(f"ELF object metadata requires emit=obj in {display_path}")
    if metadata.object_arch and metadata.object_arch not in {*ELF_MACHINES, "wasm32", "wasm64"}:
        raise ValueError(
            f"unsupported object architecture '{metadata.object_arch}' in {display_path}"
        )
    if metadata.object_bits is not None and metadata.object_bits not in {32, 64}:
        raise ValueError(
            f"unsupported object bit width '{metadata.object_bits}' in {display_path}"
        )
    if metadata.riscv_float_abi:
        if metadata.riscv_float_abi not in RISCV_FLOAT_ABI_FLAGS:
            raise ValueError(
                f"unsupported RISC-V float ABI '{metadata.riscv_float_abi}' in {display_path}"
            )
        if metadata.object_arch != "riscv64":
            raise ValueError(
                f"riscv-float-abi requires object-arch=riscv64 in {display_path}"
            )
    if metadata.loongarch_float_abi:
        if metadata.loongarch_float_abi not in LOONGARCH_FLOAT_ABI_FLAGS:
            raise ValueError(
                f"unsupported LoongArch float ABI '{metadata.loongarch_float_abi}' in {display_path}"
            )
        if metadata.object_arch != "loongarch64":
            raise ValueError(
                f"loongarch-float-abi requires object-arch=loongarch64 in {display_path}"
            )

    assembly_contract = metadata.asm_contains or metadata.asm_not_contains
    if assembly_contract and metadata.emit != "asm":
        raise ValueError(f"assembly pattern metadata requires emit=asm in {display_path}")
    if metadata.expected_exit != 0 and (object_contract or assembly_contract):
        raise ValueError(
            f"artifact expectations require a successful compile in {display_path}"
        )


def validate_fixture_name(value: str) -> None:
    path = PurePosixPath(value)
    if (not value or ":" in value or "\\" in value or "\x00" in value or path.is_absolute()
            or PureWindowsPath(value).drive or ".." in path.parts
            or path.as_posix() != value or value == "."):
        raise ValueError(f"fixture path must stay below tests/fixtures: {value!r}")


def read_fixture(value: str, fixture_root: Path) -> str:
    validate_fixture_name(value)
    try:
        root = fixture_root.resolve()
        path = (root / value).resolve()
    except (OSError, RuntimeError) as error:
        raise ValueError(f"cannot resolve fixture {value!r}: {error}") from error
    if not path.is_relative_to(root):
        raise ValueError(f"fixture path escapes tests/fixtures: {value!r}")
    try:
        # Universal newline handling matches subprocess text=True on every host.
        return path.read_text(encoding="utf-8")
    except (OSError, UnicodeError) as error:
        raise ValueError(f"cannot read UTF-8 fixture {value!r}: {error}") from error


def standard_input(metadata: TestMetadata, fixture_root: Path) -> str | None:
    if metadata.stdin_file is not None:
        return read_fixture(metadata.stdin_file, fixture_root)
    return f"{metadata.stdin}\n" if metadata.stdin is not None else None


def test_environment(metadata: TestMetadata, fixture_root: Path, *, base_env=None):
    if metadata.env_file is None:
        return None
    try:
        values = json.loads(read_fixture(metadata.env_file, fixture_root))
    except (ValueError, UnicodeError) as error:
        raise ValueError(f"invalid environment fixture {metadata.env_file!r}: {error}") from error
    if not isinstance(values, dict):
        raise ValueError("environment fixture must be a JSON object of strings")
    names = set()
    for key, value in values.items():
        if (not isinstance(key, str) or not key or "=" in key or "\x00" in key
                or not isinstance(value, str) or "\x00" in value):
            raise ValueError("environment fixture requires nonempty names and string values without NUL")
        normalized = key.upper() if os.name == "nt" else key
        if normalized in names:
            raise ValueError("environment fixture repeats a platform environment name")
        names.add(normalized)
    environment = dict(os.environ if base_env is None else base_env)
    if os.name == "nt":
        environment = {key.upper(): value for key, value in environment.items()}
        values = {key.upper(): value for key, value in values.items()}
    environment.update(values)
    return environment


def validate_output(metadata: TestMetadata, stdout: str, stderr: str, fixture_root: Path):
    for stream, actual in (("stdout", stdout), ("stderr", stderr)):
        fixture = getattr(metadata, stream + "_file")
        if fixture is None:
            continue
        expected = read_fixture(fixture, fixture_root)
        if actual != expected:
            difference = "".join(difflib.unified_diff(
                expected.splitlines(keepends=True), actual.splitlines(keepends=True),
                fromfile=f"expected {stream} ({fixture})", tofile=f"actual {stream}",
            ))
            return f"{stream} fixture mismatch\n{difference[:4096]}"
    return None


def artifact_path_for_test(
    name: str, source_path: Path, output_root: Path, metadata: TestMetadata
) -> Path:
    suffix = ARTIFACT_SUFFIXES[metadata.emit]
    output_dir = output_root / name.replace(" ", "-")
    return output_dir / f"{source_path.stem}{suffix}"


def read_elf_contract(path: Path):
    try:
        data = path.read_bytes()
    except OSError as error:
        return None, f"failed to read artifact {path}: {error}"

    if len(data) < 52 or data[:4] != b"\x7fELF":
        return None, f"expected ELF object artifact, found {path}"

    elf_class = data[4]
    encoding = data[5]
    if encoding == 1:
        byteorder = "little"
    elif encoding == 2:
        byteorder = "big"
    else:
        return None, f"invalid ELF data encoding {encoding} in {path}"

    flags_offset = {1: 36, 2: 48}.get(elf_class)
    if flags_offset is None or len(data) < flags_offset + 4:
        return None, f"invalid or truncated ELF header in {path}"

    header_size = {1: 52, 2: 64}[elf_class]
    size_offset = {1: 40, 2: 52}[elf_class]
    if len(data) < header_size:
        return None, f"truncated ELF header in {path}"
    if data[6] != 1 or int.from_bytes(data[20:24], byteorder) != 1:
        return None, f"invalid ELF version in {path}"
    if int.from_bytes(data[size_offset:size_offset + 2], byteorder) != header_size:
        return None, f"invalid ELF header size in {path}"
    if int.from_bytes(data[16:18], byteorder) != 1:
        return None, f"expected relocatable ELF object (ET_REL) in {path}"

    return {
        "bits": {1: 32, 2: 64}[elf_class],
        "machine": int.from_bytes(data[18:20], byteorder),
        "flags": int.from_bytes(data[flags_offset:flags_offset + 4], byteorder),
    }, None


class _WasmReader:
    def __init__(self, data):
        self.data = data
        self.offset = 0

    def byte(self):
        if self.offset >= len(self.data):
            raise ValueError("truncated WebAssembly section")
        result = self.data[self.offset]
        self.offset += 1
        return result

    def uint(self, bits=32):
        value = 0
        for shift in range(0, bits, 7):
            byte = self.byte()
            value |= (byte & 0x7f) << shift
            if not byte & 0x80:
                if value >= 1 << bits:
                    break
                return value
        raise ValueError("invalid WebAssembly unsigned LEB128")

    def take(self, length):
        if length > len(self.data) - self.offset:
            raise ValueError("truncated WebAssembly section payload")
        start = self.offset
        self.offset += length
        return self.data[start:self.offset]

    def name(self):
        return self.take(self.uint()).decode("utf-8")

    def limits(self):
        flags = self.uint()
        if flags & ~7 or flags & 2 and not flags & 1:
            raise ValueError("invalid WebAssembly memory/table limits")
        bits = 64 if flags & 4 else 32
        minimum = self.uint(bits)
        if flags & 1 and self.uint(bits) < minimum:
            raise ValueError("invalid WebAssembly maximum below minimum")
        return bits

    def end(self):
        if self.offset != len(self.data):
            raise ValueError("trailing WebAssembly section payload")


def read_wasm_contract(path: Path):
    try:
        data = path.read_bytes()
        if data[:8] != b"\x00asm\x01\x00\x00\x00":
            raise ValueError("expected WebAssembly magic and version 1")
        reader = _WasmReader(data[8:])
        memory_widths = set()
        memory64_feature = False
        seen = set()
        while reader.offset < len(reader.data):
            kind = reader.byte()
            if kind > 13 or kind and kind in seen:
                raise ValueError("invalid or duplicate WebAssembly section")
            seen.add(kind)
            section = _WasmReader(reader.take(reader.uint()))
            if kind == 0:
                name = section.name()
                if name == "target_features":
                    for _ in range(section.uint()):
                        prefix, feature = section.byte(), section.name()
                        if prefix not in (ord("+"), ord("-"), ord("=")):
                            raise ValueError("invalid WebAssembly target feature prefix")
                        if feature == "memory64" and prefix in (ord("+"), ord("=")):
                            memory64_feature = True
                    section.end()
            elif kind == 2:
                for _ in range(section.uint()):
                    section.name()
                    section.name()
                    imported = section.byte()
                    if imported == 0:
                        section.uint()
                    elif imported == 1:
                        section.byte()  # reference type, followed by table limits
                        section.limits()
                    elif imported == 2:
                        memory_widths.add(section.limits())
                    elif imported == 3:
                        section.byte()  # value type
                        section.byte()  # mutability
                    elif imported == 4:
                        section.uint()
                        section.uint()
                    else:
                        raise ValueError("invalid WebAssembly import kind")
                section.end()
            elif kind == 5:
                for _ in range(section.uint()):
                    memory_widths.add(section.limits())
                section.end()
        if len(memory_widths) > 1 or memory64_feature and memory_widths == {32}:
            raise ValueError("inconsistent WebAssembly memory address widths")
        if not memory_widths and not memory64_feature:
            raise ValueError("WebAssembly artifact has no memory/address-width contract")
        return {"bits": 64 if memory64_feature else next(iter(memory_widths))}, None
    except (OSError, UnicodeError, ValueError) as error:
        return None, f"invalid WebAssembly artifact {path}: {error}"


def _assembly_code_lines(assembly: str, target: str):
    arch = normalize_arch(target.split("-", 1)[0])
    comment = "#"
    if arch == "aarch64":
        comment = ";" if "apple" in target or "darwin" in target else "//"
    assembly = re.sub(
        r"/\*.*?\*/", lambda match: " " + "\n" * match[0].count("\n"),
        assembly, flags=re.DOTALL,
    )
    for raw_line in assembly.splitlines():
        line = raw_line.split(comment, 1)[0].strip()
        if arch == "aarch64" and line.startswith("#"):
            continue
        if not line or line.startswith(".") or line.endswith(":"):
            continue
        yield line


def _assembly_contains(assembly: str, pattern: str, target: str) -> bool:
    token = re.compile(
        rf"(?<![A-Za-z0-9_.$]){re.escape(pattern)}(?![A-Za-z0-9_.$])"
    )
    return any(token.search(line) for line in _assembly_code_lines(assembly, target))


def validate_compiled_artifact(
    name: str, source_path: Path, output_root: Path, metadata: TestMetadata,
    *, target: str | None = None,
):
    if metadata.runner != "compile":
        return None

    artifact = artifact_path_for_test(name, source_path, output_root, metadata)
    if not artifact.is_file():
        return f"compiler did not produce expected artifact {artifact}"

    effective_target = metadata.target or target or ""
    wasm_arch = metadata.object_arch if metadata.object_arch in {"wasm32", "wasm64"} else None
    if effective_target.startswith(("wasm32-", "wasm64-")):
        resolved_arch = effective_target.split("-", 1)[0]
        if wasm_arch and wasm_arch != resolved_arch:
            return f"conflicting WebAssembly object architecture for {artifact}"
        wasm_arch = resolved_arch
    if metadata.emit == "obj" and wasm_arch:
        contract, error = read_wasm_contract(artifact)
        if error:
            return error
        expected_bits = 64 if wasm_arch == "wasm64" else 32
        if metadata.object_bits is not None and metadata.object_bits != expected_bits:
            return f"conflicting WebAssembly address widths for {artifact}"
        if contract["bits"] != expected_bits:
            return (f"WebAssembly address width mismatch for {artifact}: "
                    f"expected {expected_bits}-bit, found {contract['bits']}-bit")

    if not wasm_arch and (
        metadata.object_arch
        or metadata.object_bits
        or metadata.riscv_float_abi
        or metadata.loongarch_float_abi
    ):
        elf, error = read_elf_contract(artifact)
        if error:
            return error

        if metadata.object_arch:
            expected_machine = ELF_MACHINES[metadata.object_arch]
            if elf["machine"] != expected_machine:
                return (
                    f"ELF machine mismatch for {artifact}: "
                    f"expected {metadata.object_arch} ({expected_machine}), "
                    f"found {elf['machine']}"
                )

        if metadata.object_bits is not None and elf["bits"] != metadata.object_bits:
            return (
                f"ELF class mismatch for {artifact}: "
                f"expected {metadata.object_bits}-bit, found {elf['bits']}-bit"
            )

        if metadata.riscv_float_abi:
            if elf["machine"] != ELF_MACHINES["riscv64"]:
                return f"RISC-V ABI metadata found on non-RISC-V artifact {artifact}"
            expected_flags = RISCV_FLOAT_ABI_FLAGS[metadata.riscv_float_abi]
            actual_flags = elf["flags"] & 0x6
            if actual_flags != expected_flags:
                return (
                    f"RISC-V float ABI mismatch for {artifact}: "
                    f"expected {metadata.riscv_float_abi} flags 0x{expected_flags:x}, "
                    f"found 0x{actual_flags:x}"
                )

        if metadata.loongarch_float_abi:
            if elf["machine"] != ELF_MACHINES["loongarch64"]:
                return f"LoongArch ABI metadata found on non-LoongArch artifact {artifact}"
            expected_flags = LOONGARCH_FLOAT_ABI_FLAGS[metadata.loongarch_float_abi]
            actual_flags = elf["flags"] & 0x7
            if actual_flags != expected_flags:
                return (
                    f"LoongArch float ABI mismatch for {artifact}: "
                    f"expected {metadata.loongarch_float_abi} flags 0x{expected_flags:x}, "
                    f"found 0x{actual_flags:x}"
                )

    if metadata.asm_contains or metadata.asm_not_contains:
        target = metadata.target or target
        if not target:
            return f"assembly artifact {artifact} requires the effective compiler target"
        try:
            assembly = artifact.read_text()
        except (OSError, UnicodeError) as error:
            return f"failed to read assembly artifact {artifact}: {error}"

        for pattern in metadata.asm_contains:
            if not _assembly_contains(assembly, pattern, target):
                return f"assembly artifact {artifact} is missing instruction token '{pattern}'"
        for pattern in metadata.asm_not_contains:
            if _assembly_contains(assembly, pattern, target):
                return (
                    f"assembly artifact {artifact} unexpectedly contains "
                    f"instruction token '{pattern}'"
                )

    return None
