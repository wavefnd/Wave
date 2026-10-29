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

"""Check every maintained Wave example and standard-library source file."""

import argparse
import math
import os
from pathlib import Path
import shutil
import subprocess
import sys

try:
    from tools.process_tree import run_process, timeout_output
    from tools.host_capabilities import local_network_unavailable
    from tools.test_contracts import (
        parse_test_metadata, standard_input, test_environment, validate_output,
    )
    from tools.validation_reports import validate_report_path, write_report
except ModuleNotFoundError:
    from process_tree import run_process, timeout_output
    from host_capabilities import local_network_unavailable
    from test_contracts import (
        parse_test_metadata, standard_input, test_environment, validate_output,
    )
    from validation_reports import validate_report_path, write_report


ROOT = Path(__file__).resolve().parent.parent


def _parse_timeout(value: str) -> float:
    try:
        timeout = float(value)
    except ValueError:
        raise argparse.ArgumentTypeError(
            f"timeout must be a finite positive number, got {value!r}"
        ) from None
    if not math.isfinite(timeout) or timeout <= 0:
        raise argparse.ArgumentTypeError(
            f"timeout must be a finite positive number, got {value!r}"
        )
    return timeout


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--wavec",
        type=Path,
        help="compiler executable (defaults to WAVEC or a local build)",
    )
    parser.add_argument(
        "--timeout",
        type=_parse_timeout,
        default=15.0,
        help="per-file timeout in seconds (default: 15)",
    )
    parser.add_argument(
        "--run-std-examples",
        action="store_true",
        help="run every examples/std/*.wave program after checking the corpus",
    )
    parser.add_argument("--report-json", type=Path, help="write source/run/skip evidence as JSON")
    return parser.parse_args(argv)


def _resolve_compiler_path(candidate: Path | str) -> Path | None:
    raw = Path(candidate)
    if not str(raw).strip():
        return None
    if raw.is_file():
        return raw.resolve()
    if not raw.is_absolute():
        rel = ROOT / raw
        if rel.is_file():
            return rel.resolve()
    which = shutil.which(str(candidate))
    if which:
        return Path(which).resolve()
    return None


def _require_launchable(path: Path) -> Path:
    if os.name != "nt" and not os.access(path, os.X_OK):
        raise PermissionError(f"wavec executable is not launchable: {path}")
    return path


def resolve_wavec(explicit: Path | None) -> Path:
    if explicit is not None:
        resolved = _resolve_compiler_path(explicit)
        if resolved is not None:
            return _require_launchable(resolved)
        raise FileNotFoundError(f"wavec executable not found at {explicit}")

    env_wavec = os.environ.get("WAVEC")
    if env_wavec and env_wavec.strip():
        resolved = _resolve_compiler_path(env_wavec)
        if resolved is not None:
            return _require_launchable(resolved)
        raise FileNotFoundError(f"wavec executable specified by WAVEC not found: {env_wavec}")

    candidates = [
        ROOT / "target" / "release" / "wavec.exe",
        ROOT / "target" / "release" / "wavec",
        ROOT / "target" / "debug" / "wavec.exe",
        ROOT / "target" / "debug" / "wavec",
        ROOT / "target" / "x86_64-pc-windows-msvc" / "release" / "wavec.exe",
        ROOT / "target" / "x86_64-pc-windows-msvc" / "debug" / "wavec.exe",
        ROOT / "target" / "aarch64-pc-windows-msvc" / "release" / "wavec.exe",
        ROOT / "target" / "aarch64-pc-windows-msvc" / "debug" / "wavec.exe",
    ]

    for candidate in candidates:
        if candidate.is_file():
            return _require_launchable(candidate)

    raise FileNotFoundError("wavec not found; build it or pass --wavec")


def corpus_files() -> list[Path]:
    files = set((ROOT / "examples").rglob("*.wave"))
    files.update((ROOT / "std").rglob("*.wave"))
    return sorted(files)


def run_std_examples(
    wavec: Path,
    compiler_env: dict[str, str],
    timeout: float,
    *, records=None, capability_probe=None,
) -> list[tuple[Path, str]]:
    examples = sorted((ROOT / "examples" / "std").glob("*.wave"))
    failures: list[tuple[Path, str]] = []
    records = [] if records is None else records
    capability_probe = capability_probe or local_network_unavailable
    network_checked = False
    network_reason = None
    skipped = 0
    if not examples:
        return [(Path("examples/std"), "no standard-library examples discovered")]

    print(f"Running {len(examples)} standard-library examples")
    for path in examples:
        relative = path.relative_to(ROOT)
        row = {"name": relative.as_posix(), "phase": "run", "status": "fail"}
        records.append(row)
        try:
            metadata = parse_test_metadata(path)
            if metadata.mode != "run" or metadata.runner != "native" or metadata.udp_input:
                raise ValueError("standard-library examples require native run mode without udp-input")
            if metadata.runtime == "compile":
                raise ValueError("standard-library examples cannot use compile-only runtime")
            fixture_root = ROOT / "tests/fixtures"
            stdin = standard_input(metadata, fixture_root)
            environment = test_environment(metadata, fixture_root)
            if environment is None:
                environment = compiler_env
            row["expected_exit"] = metadata.expected_exit
            if "sockets" in metadata.requires:
                if not network_checked:
                    network_reason = capability_probe()
                    network_checked = True
                if network_reason:
                    skipped += 1
                    row.update(status="skip", reason=network_reason)
                    print(f"[RUN SKIP] {relative}: {network_reason}")
                    continue
        except (OSError, ValueError) as error:
            failures.append((relative, str(error)))
            row["reason"] = str(error)
            print(f"[RUN FAIL] {relative}: {error}")
            continue
        try:
            result = run_process(
                [str(wavec), "--std-root", str(ROOT / "std"), "run", str(relative)],
                cwd=ROOT,
                env=environment,
                input=stdin,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                encoding="utf-8",
                errors="strict" if metadata.stdout_file or metadata.stderr_file else "replace",
                timeout=timeout,
                check=False,
            )
        except subprocess.TimeoutExpired as error:
            detail = timeout_output(error)
            failures.append((relative, f"timed out after {timeout:g}s" + (f"\n{detail}" if detail else "")))
            row.update(status="timeout", reason=failures[-1][1])
            print(f"[RUN TIMEOUT] {relative}")
            continue
        except (OSError, UnicodeError) as error:
            failures.append((relative, f"example execution failed: {error}"))
            row["reason"] = failures[-1][1]
            print(f"[RUN FAIL] {relative}")
            continue

        row.update(actual_exit=result.returncode, stdout=result.stdout, stderr=result.stderr)
        reason = None
        if result.returncode != metadata.expected_exit:
            reason = f"exit status {result.returncode}, expected {metadata.expected_exit}"
            detail = "\n".join(
                part.rstrip() for part in (result.stdout, result.stderr) if part.strip()
            )
            if detail:
                reason += f"\n{detail}"
        else:
            try:
                reason = validate_output(metadata, result.stdout, result.stderr, fixture_root)
            except (OSError, UnicodeError, ValueError) as error:
                reason = str(error)

        if reason is None:
            row["status"] = "pass"
            print(f"[RUN PASS] {relative}")
        else:
            failures.append((relative, reason))
            row["reason"] = reason
            print(f"[RUN FAIL] {relative}")

    print(
        f"Example result: {len(examples) - len(failures) - skipped} passed, "
        f"{len(failures)} failed, {skipped} skipped"
    )
    return failures


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    try:
        wavec = resolve_wavec(args.wavec)
    except OSError as error:
        print(error, file=sys.stderr)
        return 2

    files = corpus_files()
    if not files:
        print("no Wave sources discovered in the maintained corpus", file=sys.stderr)
        return 2
    if args.run_std_examples and not any((ROOT / "examples/std").glob("*.wave")):
        print("no standard-library examples discovered", file=sys.stderr)
        return 2
    records = []
    if args.report_json is not None:
        try:
            inputs = [wavec, *files, *ROOT.glob("std/**/*")]
            if args.run_std_examples:
                inputs.extend(
                    path for path in (ROOT / "tests/fixtures").rglob("*") if path.is_file()
                )
            validate_report_path(args.report_json, inputs)
        except (ValueError, OSError) as error:
            print(error, file=sys.stderr)
            return 2
    failures: list[tuple[Path, str]] = []
    example_failures: list[tuple[Path, str]] = []

    print(f"Checking {len(files)} Wave corpus files with {wavec}")
    compiler_env = os.environ.copy()
    for path in files:
        relative = path.relative_to(ROOT)
        row = {"name": relative.as_posix(), "phase": "check", "status": "fail"}
        records.append(row)
        try:
            result = run_process(
                [str(wavec), "--std-root", str(ROOT / "std"), "check", str(relative)],
                cwd=ROOT,
                env=compiler_env,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                timeout=args.timeout,
                check=False,
            )
        except subprocess.TimeoutExpired as error:
            detail = timeout_output(error)
            failures.append(
                (relative, f"timed out after {args.timeout:g}s" + (f"\n{detail}" if detail else ""))
            )
            row.update(status="timeout", reason=failures[-1][1])
            print(f"[TIMEOUT] {relative}")
            continue
        except OSError as error:
            failures.append((relative, f"compiler launch failed: {error}"))
            row["reason"] = failures[-1][1]
            print(f"[FAIL] {relative}")
            continue

        row.update(actual_exit=result.returncode, stdout=result.stdout, stderr=result.stderr)
        if result.returncode == 0:
            row["status"] = "pass"
            print(f"[PASS] {relative}")
            continue

        detail = "\n".join(
            part.rstrip() for part in (result.stdout, result.stderr) if part.strip()
        )
        failures.append((relative, detail or f"exit status {result.returncode}"))
        row["reason"] = failures[-1][1]
        print(f"[FAIL] {relative}")

    if args.run_std_examples:
        example_failures = run_std_examples(wavec, compiler_env, args.timeout, records=records)

    print(f"Corpus result: {len(files) - len(failures)} passed, {len(failures)} failed")
    for relative, detail in failures:
        print(f"\n--- {relative} ---\n{detail}", file=sys.stderr)

    for relative, detail in example_failures:
        print(f"\n--- run {relative} ---\n{detail}", file=sys.stderr)

    if args.report_json is not None:
        try:
            write_report(args.report_json, {
                "schema_version": 1, "compiler": str(wavec), "tests": records,
                "summary": {status: sum(row["status"] == status for row in records)
                            for status in ("pass", "fail", "timeout", "skip")},
            })
        except OSError as error:
            print(f"cannot write corpus report: {error}", file=sys.stderr)
            return 1
    return 1 if failures or example_failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
