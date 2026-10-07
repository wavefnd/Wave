#!/usr/bin/env python3

import argparse
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent

SKIP_DIRS = {
    ".git",
    "target",
    "vendor",
}


def rust_files():
    result = subprocess.run(
        ["git", "ls-files", "*.rs"],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )

    for name in result.stdout.splitlines():
        path = ROOT / name

        if any(part in SKIP_DIRS for part in path.parts):
            continue

        if path.is_file():
            yield path


def strip_lexical_content(line, state):
    result = []
    i = 0

    while i < len(line):
        if state["block_comment"] > 0:
            if line.startswith("/*", i):
                state["block_comment"] += 1
                result.extend("  ")
                i += 2
            elif line.startswith("*/", i):
                state["block_comment"] -= 1
                result.extend("  ")
                i += 2
            else:
                result.append(" ")
                i += 1
            continue

        if state["string"]:
            if state["escape"]:
                state["escape"] = False
                result.append(" ")
                i += 1
            elif line[i] == "\\":
                state["escape"] = True
                result.append(" ")
                i += 1
            elif line[i] == '"':
                state["string"] = False
                result.append(" ")
                i += 1
            else:
                result.append(" ")
                i += 1
            continue

        if state["char"]:
            if state["escape"]:
                state["escape"] = False
                result.append(" ")
                i += 1
            elif line[i] == "\\":
                state["escape"] = True
                result.append(" ")
                i += 1
            elif line[i] == "'":
                state["char"] = False
                result.append(" ")
                i += 1
            else:
                result.append(" ")
                i += 1
            continue

        if line.startswith("//", i):
            result.extend(" " * (len(line) - i))
            break

        if line.startswith("/*", i):
            state["block_comment"] += 1
            result.extend("  ")
            i += 2
            continue

        if line[i] == '"':
            state["string"] = True
            result.append(" ")
            i += 1
            continue

        if line[i] == "'":
            # Rust lifetimes such as 'a must not be treated as char literals.
            if i + 1 < len(line) and (
                line[i + 1].isalpha() or line[i + 1] == "_"
            ):
                result.append("'")
                i += 1
                continue

            state["char"] = True
            result.append(" ")
            i += 1
            continue

        result.append(line[i])
        i += 1

    return "".join(result)


def is_function_start(text):
    stripped = text.lstrip()

    while stripped.startswith("#["):
        return False

    prefixes = (
        "fn ",
        "pub fn ",
        "pub(crate) fn ",
        "pub(super) fn ",
        "pub(self) fn ",
        "async fn ",
        "pub async fn ",
        "pub(crate) async fn ",
        "pub(super) async fn ",
        "unsafe fn ",
        "pub unsafe fn ",
        "const fn ",
        "pub const fn ",
        "extern ",
        "pub extern ",
    )

    return stripped.startswith(prefixes)


def function_ranges(lines):
    state = {
        "block_comment": 0,
        "string": False,
        "char": False,
        "escape": False,
    }

    depth = 0
    active = None
    ranges = []

    for index, line in enumerate(lines):
        lexical = strip_lexical_content(line, state)
        depth_before = depth

        if active is None and is_function_start(lexical):
            active = {
                "start": index,
                "parent_depth": depth_before,
                "body_seen": False,
            }

        opens = lexical.count("{")
        closes = lexical.count("}")

        # A trait/extern declaration has no body. Do not carry it forward into
        # the next impl block and mistake that block's opening for its end.
        if (
            active is not None
            and not active["body_seen"]
            and opens == 0
            and lexical.rstrip().endswith(";")
        ):
            active = None

        if active is not None and opens > 0:
            active["body_seen"] = True

        depth += opens
        depth -= closes

        if (
            active is not None
            and active["body_seen"]
            and depth == active["parent_depth"]
        ):
            ranges.append((active["start"], index, active["parent_depth"]))
            active = None

    return ranges


def add_function_spacing(text):
    lines = text.splitlines(keepends=True)
    ranges = function_ranges(lines)

    if not ranges:
        return text

    insert_after = set()

    for (_, end, parent_depth), (next_start, _, next_parent_depth) in zip(
        ranges,
        ranges[1:],
    ):
        if parent_depth != next_parent_depth:
            continue

        between = lines[end + 1:next_start]

        if any(line.strip() for line in between):
            continue

        blank_count = sum(1 for line in between if not line.strip())

        if blank_count == 0:
            insert_after.add(end)

    output = []

    for index, line in enumerate(lines):
        output.append(line)

        if index in insert_after:
            output.append("\n")

    return "".join(output)


def format_file(path, check):
    original = path.read_text(encoding="utf-8")
    formatted = add_function_spacing(original)

    if formatted == original:
        return False

    if check:
        print(path.relative_to(ROOT))
        return True

    path.write_text(formatted, encoding="utf-8")
    print(f"formatted {path.relative_to(ROOT)}")
    return True


def run_rustfmt(check):
    command = ["cargo", "fmt", "--all"]

    if check:
        command.extend(["--", "--check"])

    subprocess.run(command, cwd=ROOT, check=True)


def main():
    parser = argparse.ArgumentParser(
        description="Format Wave Rust sources and enforce blank lines between functions.",
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help="Check formatting without modifying files.",
    )
    args = parser.parse_args()

    run_rustfmt(args.check)

    changed = False

    for path in rust_files():
        changed |= format_file(path, args.check)

    if args.check and changed:
        print(
            "Rust function spacing does not match the Wave style.",
            file=sys.stderr,
        )
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
