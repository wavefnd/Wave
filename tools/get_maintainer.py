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

"""
Wave: get_maintainer.py

Reads MAINTAINERS file and determines which maintainers should be CC'd
based on file paths included in a patch or manually provided.

Usage:
    python3 tools/get_maintainer.py <file1> <file2> ...

Example:
    python3 tools/get_maintainer.py front/parser/ast.rs
"""

import sys
import os
from pathlib import Path

MAINTAINERS_FILE = Path(__file__).resolve().parent.parent / "MAINTAINERS"

if not os.path.exists(MAINTAINERS_FILE):
    print(f"Error: MAINTAINERS file not found at {MAINTAINERS_FILE}.")
    sys.exit(1)

if len(sys.argv) < 2:
    print("Usage: get_maintainer.py <file1> [file2] ...")
    sys.exit(1)

# Parse MAINTAINERS file
sections = []
current = None

with open(MAINTAINERS_FILE, "r") as f:
    for line in f:
        line = line.strip()

        if not line:
            continue

        # Start of section
        if line.startswith("[") and line.endswith("]"):
            if current:
                sections.append(current)
            current = {"name": line[1:-1], "maintainers": [], "files": []}
            continue

        if line.startswith("M:"):
            current["maintainers"].append(line[2:].strip())
        elif line.startswith("F:"):
            current["files"].append(line[2:].strip())

# Add last section
if current:
    sections.append(current)

files_to_check = sys.argv[1:]
matched_maintainers = set()

for file in files_to_check:
    for section in sections:
        for path in section["files"]:
            # Direct folder matching (Wave-style directory structure)
            if file.startswith(path):
                matched_maintainers.update(section["maintainers"])

if matched_maintainers:
    print("Maintainers to CC:")
    for m in sorted(matched_maintainers):
        print("  " + m)
else:
    print("No specific maintainers found. Using default:")
    print("  luna@lunastev.org")
