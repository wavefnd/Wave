# SPDX-License-Identifier: MPL-2.0
"""Expand the CI job names from the validated workflow and case manifest.

Only the repository's declarative job/name/matrix subset is accepted. Unknown
matrix shapes fail closed instead of silently dropping validation requirements.
"""

import re
import tempfile
from pathlib import Path

from tools.case_manifest import load_case_manifest
from tools.ci.common import evaluate


def required_names(workflow, manifest):
    with tempfile.TemporaryDirectory(prefix="wave-ci-contract-") as directory:
        path = Path(directory) / "cases.toml"
        path.write_text(manifest, encoding="utf-8")
        matrices = load_case_manifest(path, validate_layout=False).github_matrices()
    cross = matrices["cross"]["include"]
    for group in ("freebsd", "freestanding"):
        matrices[group] = {
            "include": [entry for entry in cross if entry["os"] == group]
        }
    result = set()
    # API contents preserve checkout bytes, including Windows CRLF. Normalize
    # record boundaries before applying the supported workflow-shape grammar.
    workflow = workflow.replace("\r\n", "\n")
    sections = re.split(r"(?m)^jobs:\n", workflow, maxsplit=1)
    if len(sections) != 2:
        raise ValueError("CI workflow has no jobs contract")
    workflow = sections[1]
    blocks = re.split(r"(?m)^  [a-zA-Z0-9_-]+:\s*$", workflow)[1:]
    for block in blocks:
        match = re.search(r"(?m)^    name: (.+)$", block)
        if not match:
            raise ValueError("CI job has no explicit name contract")
        template = match[1]
        if "${{" not in template:
            names = [template]
        else:
            dynamic = re.search(
                r"matrix: \$\{\{ fromJSON\(needs.case-matrix.outputs.([a-z_0-9]+)\) \}\}",
                block,
            )
            if dynamic:
                entries = matrices[dynamic[1]]["include"]
            else:
                shape = re.search(
                    r"(?ms)^      matrix:\n        include:\n(.*?)(?=^    [a-zA-Z-]+:)",
                    block,
                )
                if not shape:
                    raise ValueError("unsupported CI matrix contract")
                entries = []
                for line in shape[1].splitlines():
                    item = re.fullmatch(r"        - ([a-z_]+): ([a-zA-Z0-9_.-]+)", line)
                    field = re.fullmatch(
                        r"          ([a-z_]+): ([a-zA-Z0-9_.-]+)", line
                    )
                    if item:
                        entries.append({item[1]: item[2]})
                    elif field and entries:
                        entries[-1][field[1]] = field[2]
                    elif line.strip():
                        raise ValueError("unsupported static CI matrix entry")
            if not entries:
                raise ValueError("empty required CI matrix")
            names = []
            for entry in entries:
                context = {"matrix." + key: value for key, value in entry.items()}
                names.append(
                    re.sub(
                        r"\$\{\{(.*?)\}\}",
                        lambda m, context=context: str(evaluate(m[1], context)),
                        template,
                    )
                )
        for name in names:
            if name in result:
                raise ValueError("duplicate required CI job name")
            result.add(name)
    if not result:
        raise ValueError("empty CI workflow contract")
    return result
