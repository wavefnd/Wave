#!/usr/bin/env python3
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

MAINTAINERS_FILE = "MAINTAINERS"

if not os.path.exists(MAINTAINERS_FILE):
    print("Error: MAINTAINERS file not found.")
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
