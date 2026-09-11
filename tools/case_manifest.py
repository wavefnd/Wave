#!/usr/bin/env python3

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

import argparse
from dataclasses import dataclass
import json
from pathlib import Path
import re
import sys
import tomllib


ROOT = Path(__file__).resolve().parent.parent
CASES_ROOT = ROOT / "tests" / "cases"
DEFAULT_MANIFEST = CASES_ROOT / "cases.toml"

EXECUTORS = {"native", "compile", "qemu", "wasm", "none"}
CI_GROUPS = {
    "linux",
    "macos",
    "windows",
    "windows_arm64",
    "cross",
    "qemu",
    "loongarch",
    "wasm",
}
MIN_CASES_PER_SUITE = 10
TARGET_KEYS = {
    "id",
    "os",
    "arch",
    "executor",
    "runner",
    "ci_group",
    "target",
    "smoke_case",
    "smoke_stdout",
    "exclude",
}
NAME_PATTERN = re.compile(r"^[a-z0-9][a-z0-9_-]*$")
ROOT_KEYS = {"version", "supported", "ci", "target"}
LEGACY_PLATFORM_KEYS = {"host-os", "host-arch"}
WAVE_TEST_MARKER = "// wave-test:"


class CaseManifestError(ValueError):
    pass


@dataclass(frozen=True)
class CaseTarget:
    id: str
    os: str
    arch: str
    suite: str
    status: str
    enabled: bool
    ci: bool
    executor: str
    suites: tuple[str, ...]
    exclude: tuple[str, ...]
    runner: str | None = None
    ci_group: str | None = None
    target: str | None = None
    smoke_case: str | None = None
    smoke_stdout: str | None = None


@dataclass(frozen=True)
class CaseManifest:
    version: int
    targets: tuple[CaseTarget, ...]

    def target(self, target_id: str) -> CaseTarget:
        for target in self.targets:
            if target.id == target_id:
                return target
        raise CaseManifestError(f"unknown case target '{target_id}'")

    def native_target(self, host_os: str, host_arch: str) -> CaseTarget:
        matches = [
            target
            for target in self.targets
            if target.enabled
            and target.executor == "native"
            and target.os == host_os
            and target.arch == host_arch
        ]
        if len(matches) != 1:
            raise CaseManifestError(
                f"expected one enabled native target for {host_os}/{host_arch}, "
                f"found {len(matches)}"
            )
        return matches[0]

    def github_matrices(self):
        matrices = {group: {"include": []} for group in sorted(CI_GROUPS)}
        for target in self.targets:
            if not target.ci:
                continue
            entry = {
                "id": target.id,
                "os": target.os,
                "arch": target.arch,
                "runner": target.runner,
            }
            if target.target:
                entry["target"] = target.target
            if target.smoke_case:
                entry["smoke_case"] = target.smoke_case
                entry["smoke_stdout"] = target.smoke_stdout
            matrices[target.ci_group]["include"].append(entry)
        return matrices

    def sources(self):
        suites = {
            suite
            for target in self.targets
            for suite in (target.suite, *target.suites)
        }
        paths = []
        for suite in sorted(suites):
            directory = CASES_ROOT / suite
            entries = list(directory.glob("test*.wave"))
            entries.extend(directory.glob("test*/main.wave"))
            paths.extend(sorted(entries, key=_test_number))
        return tuple(path.relative_to(CASES_ROOT).as_posix() for path in paths)

    def runtime_sources(self, target_id: str):
        target = self.target(target_id)
        paths = []
        for suite in (f"shared/{target.arch}", target.suite):
            directory = CASES_ROOT / suite
            entries = list(directory.glob("test*.wave"))
            entries.extend(directory.glob("test*/main.wave"))
            for path in sorted(entries, key=_test_number):
                metadata = _parse_test_metadata(path)
                relative = path.relative_to(CASES_ROOT).as_posix()
                if (
                    metadata.mode == "run"
                    and metadata.runner == "native"
                    and metadata.stdin is None
                    and not metadata.udp_input
                    and metadata.expected_exit == 0
                    and relative not in target.exclude
                ):
                    paths.append(relative)
        return tuple(paths)


def _require_string(raw, key, target_id):
    value = raw.get(key)
    if not isinstance(value, str) or not value:
        raise CaseManifestError(f"target '{target_id}' requires non-empty string '{key}'")
    return value


def _string_list(raw, key, target_id):
    value = raw.get(key, [])
    if not isinstance(value, list) or not all(isinstance(item, str) for item in value):
        raise CaseManifestError(f"target '{target_id}' requires string array '{key}'")
    if len(value) != len(set(value)):
        raise CaseManifestError(f"target '{target_id}' contains duplicate '{key}' entries")
    return tuple(value)


def _parse_test_metadata(path):
    try:
        from tools.test_contracts import parse_test_metadata
    except ModuleNotFoundError:
        from test_contracts import parse_test_metadata

    return parse_test_metadata(path)


def _target_suites(os_name, arch):
    return ("shared", f"shared/{arch}", f"{os_name}/{arch}")


def _safe_case_path(value, kind, target_id):
    path = Path(value)
    if path.is_absolute() or ".." in path.parts or not value or value != path.as_posix():
        raise CaseManifestError(
            f"target '{target_id}' has unsafe {kind} path '{value}'"
        )
    return path


def _parse_target(raw, supported, ci):
    if not isinstance(raw, dict):
        raise CaseManifestError("each [[target]] entry must be a table")
    target_id = _require_string(raw, "id", "<unknown>")
    unknown_keys = sorted(set(raw) - TARGET_KEYS)
    if unknown_keys:
        raise CaseManifestError(
            f"target '{target_id}' has unknown fields: {', '.join(unknown_keys)}"
        )
    if not NAME_PATTERN.fullmatch(target_id):
        raise CaseManifestError(f"invalid target id '{target_id}'")

    os_name = _require_string(raw, "os", target_id)
    arch = _require_string(raw, "arch", target_id)
    executor = _require_string(raw, "executor", target_id)
    suite = f"{os_name}/{arch}"
    suites = _target_suites(os_name, arch)
    exclude = _string_list(raw, "exclude", target_id)
    runner = raw.get("runner")
    ci_group = raw.get("ci_group")
    target_triple = raw.get("target")
    smoke_case = raw.get("smoke_case")
    smoke_stdout = raw.get("smoke_stdout")

    for key, value in (
        ("runner", runner),
        ("ci_group", ci_group),
        ("target", target_triple),
        ("smoke_case", smoke_case),
        ("smoke_stdout", smoke_stdout),
    ):
        if value is not None and (not isinstance(value, str) or not value):
            raise CaseManifestError(f"target '{target_id}' has invalid '{key}'")

    if executor not in EXECUTORS:
        raise CaseManifestError(
            f"target '{target_id}' has unsupported executor '{executor}'"
        )
    if not supported:
        raise CaseManifestError(
            f"target '{target_id}' has execution details but supported=false"
        )
    if executor == "none":
        raise CaseManifestError(f"target '{target_id}' is supported but has executor=none")
    if ci and (not runner or ci_group not in CI_GROUPS):
        raise CaseManifestError(
            f"target '{target_id}' needs runner and a supported ci_group when ci=true"
        )
    if not ci and (runner or ci_group):
        raise CaseManifestError(
            f"target '{target_id}' sets CI fields while ci=false"
        )
    if executor in {"compile", "qemu", "wasm"} and not target_triple:
        raise CaseManifestError(
            f"target '{target_id}' requires a Wave target triple for executor={executor}"
        )
    if (smoke_case is None) != (smoke_stdout is None):
        raise CaseManifestError(
            f"target '{target_id}' must set smoke_case and smoke_stdout together"
        )
    if smoke_case and executor != "qemu":
        raise CaseManifestError(
            f"target '{target_id}' smoke output is only supported by executor=qemu"
        )

    suite_path = _safe_case_path(suite, "suite", target_id)
    if not (CASES_ROOT / suite_path).is_dir():
        raise CaseManifestError(f"target '{target_id}' suite does not exist: {suite}")

    for selected_suite in suites:
        path = _safe_case_path(selected_suite, "suite", target_id)
        if not (CASES_ROOT / path).is_dir():
            raise CaseManifestError(
                f"target '{target_id}' selected suite does not exist: {selected_suite}"
            )
    if smoke_case:
        path = _safe_case_path(smoke_case, "smoke case", target_id)
        if not (CASES_ROOT / path).is_file():
            raise CaseManifestError(
                f"target '{target_id}' smoke case does not exist: {smoke_case}"
            )
    for excluded_case in exclude:
        path = _safe_case_path(excluded_case, "excluded case", target_id)
        if not (CASES_ROOT / path).exists():
            raise CaseManifestError(
                f"target '{target_id}' excluded case does not exist: {excluded_case}"
            )

    return CaseTarget(
        id=target_id,
        os=os_name,
        arch=arch,
        suite=suite,
        status="compile-only" if executor == "compile" else "supported",
        enabled=True,
        ci=ci,
        executor=executor,
        suites=suites,
        exclude=exclude,
        runner=runner,
        ci_group=ci_group,
        target=target_triple,
        smoke_case=smoke_case,
        smoke_stdout=smoke_stdout,
    )


def _boolean_matrix(data, name):
    raw = data.get(name)
    if not isinstance(raw, dict) or not raw:
        raise CaseManifestError(f"case manifest requires a non-empty [{name}] table")

    result = {}
    for os_name, arches in raw.items():
        if not NAME_PATTERN.fullmatch(os_name) or not isinstance(arches, dict) or not arches:
            raise CaseManifestError(f"invalid operating-system row '{os_name}' in [{name}]")
        for arch, value in arches.items():
            if not NAME_PATTERN.fullmatch(arch) or not isinstance(value, bool):
                raise CaseManifestError(
                    f"[{name}].{os_name}.{arch} must be true or false"
                )
            result[(os_name, arch)] = value
    return result


def _test_number(path):
    unit = path.parent.name if path.name == "main.wave" else path.stem
    suffix = unit.removeprefix("test")
    return int(suffix) if suffix.isdigit() else 0


def _has_legacy_platform_metadata(path: Path) -> bool:
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except OSError:
        return False

    for line in lines:
        stripped = line.strip()
        if not stripped.startswith("//"):
            if stripped:
                break
            continue
        if not stripped.startswith(WAVE_TEST_MARKER):
            continue

        body = stripped[len(WAVE_TEST_MARKER):].strip()
        for raw_item in body.split(","):
            item = raw_item.strip()
            if "=" in item:
                key = item.split("=", 1)[0].strip()
                if key in LEGACY_PLATFORM_KEYS:
                    return True
    return False


def _validate_case_layout(targets):
    suites = {
        suite
        for target in targets
        for suite in (target.suite, *target.suites)
    }
    for suite in sorted(suites):
        directory = CASES_ROOT / suite
        if not directory.is_dir():
            raise CaseManifestError(f"configured suite does not exist: {suite}")
        paths = list(directory.glob("test*.wave"))
        paths.extend(directory.glob("test*/main.wave"))
        if len(paths) < MIN_CASES_PER_SUITE:
            raise CaseManifestError(
                f"suite '{suite}' requires at least {MIN_CASES_PER_SUITE} cases, "
                f"found {len(paths)}"
            )
        numbers = sorted(_test_number(path) for path in paths)
        if numbers != list(range(1, len(paths) + 1)):
            raise CaseManifestError(
                f"suite '{suite}' must use contiguous test numbers starting at 1"
            )

    legacy_root_cases = sorted(CASES_ROOT.glob("test*.wave"))
    legacy_root_cases.extend(CASES_ROOT.glob("test*/main.wave"))
    if legacy_root_cases:
        names = ", ".join(path.relative_to(CASES_ROOT).as_posix() for path in legacy_root_cases)
        raise CaseManifestError(f"case files must live in a configured suite: {names}")

    for source in CASES_ROOT.rglob("*.wave"):
        if _has_legacy_platform_metadata(source):
            relative = source.relative_to(CASES_ROOT).as_posix()
            raise CaseManifestError(
                f"case '{relative}' uses legacy platform metadata; use its directory"
            )


def load_case_manifest(path=DEFAULT_MANIFEST):
    try:
        data = tomllib.loads(Path(path).read_text(encoding="utf-8"))
    except (OSError, tomllib.TOMLDecodeError) as error:
        raise CaseManifestError(f"cannot read case manifest {path}: {error}") from error

    unknown_keys = sorted(set(data) - ROOT_KEYS)
    if unknown_keys:
        raise CaseManifestError(
            f"case manifest has unknown top-level fields: {', '.join(unknown_keys)}"
        )

    version = data.get("version")
    if version != 2:
        raise CaseManifestError(f"unsupported case manifest version: {version!r}")
    supported = _boolean_matrix(data, "supported")
    ci = _boolean_matrix(data, "ci")
    if supported.keys() != ci.keys():
        raise CaseManifestError("[supported] and [ci] must define the same OS/arch cells")
    for key in supported:
        if ci[key] and not supported[key]:
            os_name, arch = key
            raise CaseManifestError(
                f"CI cannot be true while support is false for {os_name}/{arch}"
            )

    raw_targets = data.get("target")
    if not isinstance(raw_targets, list):
        raise CaseManifestError("case manifest requires [[target]] execution tables")

    configured = []
    configured_pairs = set()
    for raw in raw_targets:
        if not isinstance(raw, dict):
            raise CaseManifestError("each [[target]] entry must be a table")
        os_name = _require_string(raw, "os", raw.get("id", "<unknown>"))
        arch = _require_string(raw, "arch", raw.get("id", "<unknown>"))
        pair = (os_name, arch)
        if pair not in supported:
            raise CaseManifestError(
                f"target '{raw.get('id', '<unknown>')}' is missing from the support matrix"
            )
        if pair in configured_pairs:
            raise CaseManifestError(
                f"multiple execution tables configure {os_name}/{arch}"
            )
        configured_pairs.add(pair)
        configured.append(_parse_target(raw, supported[pair], ci[pair]))

    expected_pairs = {pair for pair, value in supported.items() if value}
    missing_pairs = sorted(expected_pairs - configured_pairs)
    if missing_pairs:
        names = ", ".join(f"{os_name}/{arch}" for os_name, arch in missing_pairs)
        raise CaseManifestError(f"supported targets need execution tables: {names}")

    planned = [
        CaseTarget(
            id=f"{os_name}-{arch}",
            os=os_name,
            arch=arch,
            suite=f"{os_name}/{arch}",
            status="planned",
            enabled=False,
            ci=False,
            executor="none",
            suites=_target_suites(os_name, arch),
            exclude=(),
        )
        for (os_name, arch), value in supported.items()
        if not value
    ]
    targets = tuple(configured + planned)
    ids = [target.id for target in targets]
    if len(ids) != len(set(ids)):
        raise CaseManifestError("case manifest contains duplicate target ids")
    native_keys = [
        (target.os, target.arch)
        for target in targets
        if target.enabled and target.executor == "native"
    ]
    if len(native_keys) != len(set(native_keys)):
        raise CaseManifestError(
            "case manifest contains multiple enabled native targets for one host"
        )
    _validate_case_layout(targets)
    return CaseManifest(version=version, targets=targets)


def parse_args():
    parser = argparse.ArgumentParser(description="Inspect the Wave case target manifest")
    subcommands = parser.add_subparsers(dest="command", required=True)
    subcommands.add_parser("validate")
    subcommands.add_parser("github-output")
    subcommands.add_parser("sources")

    for command in ("runtime-sources", "suites"):
        selected = subcommands.add_parser(command)
        selected.add_argument("target_id")

    field = subcommands.add_parser("field")
    field.add_argument("target_id")
    field.add_argument(
        "name",
        choices=(
            "id",
            "os",
            "arch",
            "suite",
            "status",
            "executor",
            "runner",
            "target",
            "smoke_case",
            "smoke_stdout",
        ),
    )
    return parser.parse_args()


def main():
    args = parse_args()
    try:
        manifest = load_case_manifest()
        if args.command == "validate":
            print(f"case manifest OK: {len(manifest.targets)} targets")
            return
        if args.command == "github-output":
            for group, matrix in manifest.github_matrices().items():
                print(f"{group}={json.dumps(matrix, separators=(',', ':'))}")
            return
        if args.command == "sources":
            print("\n".join(manifest.sources()))
            return

        target = manifest.target(args.target_id)
        if args.command == "runtime-sources":
            print("\n".join(manifest.runtime_sources(target.id)))
        elif args.command == "suites":
            print("\n".join(target.suites))
        elif args.command == "field":
            value = getattr(target, args.name)
            if value is None:
                raise CaseManifestError(
                    f"target '{target.id}' does not define field '{args.name}'"
                )
            print(value)
    except CaseManifestError as error:
        print(f"invalid case manifest: {error}", file=sys.stderr)
        raise SystemExit(2) from error


if __name__ == "__main__":
    main()
