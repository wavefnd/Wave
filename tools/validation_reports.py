# SPDX-License-Identifier: MPL-2.0
"""Protect validation inputs and replace progress reports atomically."""
import json
import os
from pathlib import Path
import tempfile


def case_report_inputs(root, sources):
    """Protect case configuration, imports and std before even preflight saves.

    Local Wave imports are confined to their module directories. Protecting the
    source tree also covers target-gated imports and malformed sources without
    launching a compiler or guessing which branch of an import will be selected.
    """
    root = Path(root)
    inputs = list(sources)
    inputs.append(root / "tests/cases/cases.toml")
    inputs.extend(path for path in (root / "tests").rglob("*.wave") if path.is_file())
    inputs.extend(path for path in (root / "std").rglob("*") if path.is_file())
    return inputs


def validate_report_path(report, inputs):
    report = Path(report)
    resolved = report.resolve()
    for source in inputs:
        source = Path(source)
        if resolved == source.resolve() or (
            report.exists() and source.exists() and report.samefile(source)
        ):
            raise ValueError(f"report path aliases an input: {report} ({source})")


def write_report(report, payload):
    report = Path(report)
    report.parent.mkdir(parents=True, exist_ok=True)
    fd, name = tempfile.mkstemp(prefix=f".{report.name}.", suffix=".tmp", dir=report.parent)
    temporary = Path(name)
    try:
        try:
            stream = os.fdopen(fd, "w", encoding="utf-8")
        except Exception:
            os.close(fd)
            raise
        with stream:
            stream.write(json.dumps(payload, indent=2, ensure_ascii=False) + "\n")
        temporary.replace(report)
    finally:
        temporary.unlink(missing_ok=True)
