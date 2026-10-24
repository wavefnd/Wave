#!/usr/bin/env python3

import argparse
import subprocess
import sys
import unicodedata
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent

SKIP_DIRS = {
    ".git",
    "target",
    "vendor",
}


def rust_files():
    result = subprocess.run(
        ["git", "ls-files", "-z", "*.rs"],
        cwd=ROOT,
        check=True,
        capture_output=True,
    )

    for entry in result.stdout.split(b"\x00"):
        if not entry:
            continue;

        name = entry.decode("utf-8")
        path = ROOT / name

        if any(part in SKIP_DIRS for part in path.parts):
            continue

        if path.is_file():
            yield path


# Only lexical classification is needed here, not full rustc validation.
# Rust's Unicode identifier tables can differ from Python's isidentifier().
# Recognize letters and identifier continuations, including combining marks.
_RUST_IDENT_START_CATEGORIES = frozenset(("Lu", "Ll", "Lt", "Lm", "Lo", "Nl"))
_RUST_IDENT_CONTINUE_CATEGORIES = _RUST_IDENT_START_CATEGORIES | {"Mn", "Mc", "Nd", "Pc"}
_RUST_OTHER_IDENT_START = frozenset("\u1885\u1886\u2118\u212e\u309b\u309c")
_RUST_OTHER_IDENT_CONTINUE = frozenset("\u00b7\u0387\u1369\u136a\u136b\u136c\u136d\u136e\u136f\u1370\u1371\u19da")


def rust_lifetime_identifier_end(line, start):
    """Return the end of a potential Rust lifetime/label identifier."""
    if start >= len(line):
        return start

    first = line[start]
    if not (
        first == "_"
        or unicodedata.category(first) in _RUST_IDENT_START_CATEGORIES
        or first in _RUST_OTHER_IDENT_START
    ):
        return start

    end = start + 1
    while end < len(line):
        char = line[end]
        if not (
            char == "_"
            or unicodedata.category(char) in _RUST_IDENT_CONTINUE_CATEGORIES
            or char in _RUST_OTHER_IDENT_START
            or char in _RUST_OTHER_IDENT_CONTINUE
        ):
            break
        end += 1
    return end


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

        if state["string"] is not None:
            if type(state["string"]) is int:
                hashes = "#" * state["string"]
                terminator = '"' + hashes
                if line.startswith(terminator, i):
                    state["string"] = None
                    result.extend(" " * len(terminator))
                    i += len(terminator)
                else:
                    result.append(" ")
                    i += 1
            else:
                if state["escape"]:
                    state["escape"] = False
                    result.append(" ")
                    i += 1
                elif line[i] == "\\":
                    state["escape"] = True
                    result.append(" ")
                    i += 1
                elif line[i] == '"':
                    state["string"] = None
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

        raw_prefix_matched = False

        if line.startswith("br\"", i):
            state["string"] = 0
            result.extend("   ")
            i += 3
            raw_prefix_matched = True
        elif line.startswith("r\"", i):
            state["string"] = 0
            result.extend("  ")
            i += 2
            raw_prefix_matched = True
        else:
            # Check hashed raw strings like r#"..."# or br##"..."##
            j = i
            is_byte = False
            if j < len(line) and line[j] == 'b':
                is_byte = True
                j += 1

            if j < len(line) and line[j] == 'r':
                j += 1
                hash_count = 0
                while j < len(line) and line[j] == '#':
                    hash_count += 1
                    j += 1
                if j < len(line) and line[j] == '"':
                    prefix_len = j + 1 - i
                    state["string"] = hash_count
                    result.extend(" " * prefix_len)
                    i = j + 1
                    raw_prefix_matched = True

        if raw_prefix_matched:
            continue
        if line[i] == '"':
            state["string"] = True
            result.append(" ")
            i += 1
            continue

        if line[i] == "'":
            # Preserve Unicode lifetime/label continuations (including marks).
            # A single scalar followed by a quote is a character literal.
            lifetime_end = rust_lifetime_identifier_end(line, i + 1)
            if lifetime_end > i + 1 and not (
                lifetime_end == i + 2
                and lifetime_end < len(line)
                and line[lifetime_end] == "'"
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
        "string": None,
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


def attached_item_start(lines, previous_end, function_start):
    """Find outer attributes and comments belonging to the next function."""
    state = {"block_comment": 0, "string": None, "char": False, "escape": False}
    start = previous_end + 1
    brackets = 0
    anchored = False

    for index in range(start, function_start):
        line = lines[index]
        in_comment = state["block_comment"] > 0
        stripped = line.lstrip()
        if not in_comment and brackets == 0:
            # Inner documentation describes the surrounding scope.
            if stripped.startswith(("//!", "/*!")):
                return function_start
            if stripped.startswith(("///", "/**")) and not stripped.startswith(("////", "/***")):
                anchored = True
            if not line.strip() and not anchored:
                start = index + 1

        lexical = strip_lexical_content(line, state)
        i = 0
        while i < len(lexical):
            if brackets:
                if lexical[i] == "[":
                    brackets += 1
                elif lexical[i] == "]":
                    brackets -= 1
                i += 1
            elif lexical[i].isspace():
                i += 1
            elif lexical.startswith("#[", i):
                anchored = True
                brackets = 1
                i += 2
            else:
                # An intervening item or scope boundary is not an attachment.
                return function_start

    if brackets or state["block_comment"] or state["string"] is not None or state["char"]:
        return function_start
    return start


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

        item_start = attached_item_start(lines, end, next_start)
        between = lines[end + 1:item_start]

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
