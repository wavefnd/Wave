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
import tempfile

try:
    from tools.process_tree import run_process, timeout_output
except ModuleNotFoundError:
    from process_tree import run_process, timeout_output


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
    return parser.parse_args(argv)


def _resolve_compiler_path(candidate: Path | str) -> Path | None:
    raw = Path(candidate)
    if not str(raw).strip():
        return None
    if raw.is_file():
        return raw
    if not raw.is_absolute():
        rel = ROOT / raw
        if rel.is_file():
            return rel
    which = shutil.which(str(candidate))
    if which:
        return Path(which)
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
        ROOT / "target" / "x86_64-pc-windows-gnu" / "release" / "wavec.exe",
        ROOT / "target" / "x86_64-pc-windows-gnu" / "debug" / "wavec.exe",
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
) -> list[tuple[Path, str]]:
    examples = sorted((ROOT / "examples" / "std").glob("*.wave"))
    failures: list[tuple[Path, str]] = []

    print(f"Running {len(examples)} standard-library examples")
    for path in examples:
        relative = path.relative_to(ROOT)
        try:
            result = run_process(
                [str(wavec), "run", str(relative)],
                cwd=ROOT,
                env=compiler_env,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                timeout=timeout,
                check=False,
            )
        except subprocess.TimeoutExpired as error:
            detail = timeout_output(error)
            failures.append((relative, f"timed out after {timeout:g}s" + (f"\n{detail}" if detail else "")))
            print(f"[RUN TIMEOUT] {relative}")
            continue

        if result.returncode == 0:
            print(f"[RUN PASS] {relative}")
            continue

        detail = "\n".join(
            part.rstrip() for part in (result.stdout, result.stderr) if part.strip()
        )
        failures.append((relative, detail or f"exit status {result.returncode}"))
        print(f"[RUN FAIL] {relative}")

    print(
        f"Example result: {len(examples) - len(failures)} passed, "
        f"{len(failures)} failed"
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
    failures: list[tuple[Path, str]] = []
    example_failures: list[tuple[Path, str]] = []

    print(f"Checking {len(files)} Wave corpus files with {wavec}")
    with tempfile.TemporaryDirectory(prefix="wave-corpus-home-") as temp_home:
        std_dest = Path(temp_home) / ".wave" / "lib" / "wave" / "std"
        std_dest.parent.mkdir(parents=True)
        shutil.copytree(ROOT / "std", std_dest)
        compiler_env = os.environ.copy()
        compiler_env["HOME"] = temp_home

        for path in files:
            relative = path.relative_to(ROOT)
            try:
                result = run_process(
                    [str(wavec), "check", str(relative)],
                    cwd=ROOT,
                    env=compiler_env,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    text=True,
                    timeout=args.timeout,
                    check=False,
                )
            except subprocess.TimeoutExpired:
                failures.append((relative, f"timed out after {args.timeout:g}s"))
                print(f"[TIMEOUT] {relative}")
                continue

            if result.returncode == 0:
                print(f"[PASS] {relative}")
                continue

            detail = "\n".join(
                part.rstrip() for part in (result.stdout, result.stderr) if part.strip()
            )
            failures.append((relative, detail or f"exit status {result.returncode}"))
            print(f"[FAIL] {relative}")

        if args.run_std_examples:
            example_failures = run_std_examples(wavec, compiler_env, args.timeout)

    print(f"Corpus result: {len(files) - len(failures)} passed, {len(failures)} failed")
    for relative, detail in failures:
        print(f"\n--- {relative} ---\n{detail}", file=sys.stderr)

    for relative, detail in example_failures:
        print(f"\n--- run {relative} ---\n{detail}", file=sys.stderr)

    return 1 if failures or example_failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
