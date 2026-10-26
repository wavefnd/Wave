# SPDX-License-Identifier: MPL-2.0
"""Refresh the public std inventory from declarations, without exporting private imports."""

import argparse
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parent.parent


def _masked_source(source):
    # Keep source offsets in both views. Signatures retain quoted literals,
    # but comments and quoted delimiters are hidden from declaration parsing
    clean = list(source)
    structure = list(source)
    index = 0
    while index < len(source):
        if source.startswith('//', index):
            end = source.find('\n', index)
            if end < 0:
                end = len(source)
            for pos in range(index, end):
                clean[pos] = structure[pos] = ' '
            index = end
        elif source.startswith('/*', index):
            depth = 1
            end = index + 2
            while end < len(source) and depth:
                if source.startswith('/*', end):
                    depth += 1
                    end += 2
                elif source.startswith('*/', end):
                    depth -= 1
                    end += 2
                else:
                    end += 1
            for pos in range(index, end):
                if source[pos] != '\n':
                    clean[pos] = structure[pos] = ' '
            index = end
        elif source[index] in ('"', "'"):
            quote = source[index]
            end = index + 1
            while end < len(source):
                if source[end] == '\\':
                    end += 2
                elif source[end] == quote:
                    end += 1
                    break
                else:
                    end += 1
            for pos in range(index, min(end, len(source))):
                if source[pos] != '\n':
                    structure[pos] = ' '
            index = end
        else:
            index += 1
    return ''.join(clean), ''.join(structure)


def declarations(source):
    clean, structure = _masked_source(source)
    public = re.compile(r'(?m)^\s*pub\s+(?:(async)\s+)?(fun|struct|enum|variant|type|const|var|import)\b')
    for match in public.finditer(structure):
        start = match.start()
        start += len(structure[start:match.end()]) - len(structure[start:match.end()].lstrip())
        kind = match[2]
        tail = clean[start:]
        parentheses = braces = 0
        stop = None
        for token in re.finditer(r'[(){};]', structure[start:]):
            value = token[0]
            if value == '(':
                parentheses += 1
            elif value == ')':
                parentheses -= 1
            elif value == '{':
                if kind == 'fun' and parentheses == 0:
                    stop = token.start()
                    break
                braces += 1
            elif value == '}':
                braces -= 1
                if kind in ('struct', 'enum', 'variant') and braces == 0:
                    stop = token.end()
                    break
            elif value == ';' and parentheses == 0 and braces == 0:
                stop = token.end()
                break
        if stop is None:
            raise ValueError(f'unterminated public {kind} declaration')
        line = clean.count('\n', 0, start) + 1
        before = clean[:start].rstrip().splitlines()
        guards = []
        while before and before[-1].lstrip().startswith('#['):
            guards.insert(0, before.pop().strip())
        signature = tail[:stop].strip()
        yield line, guards, signature


def render(root=ROOT):
    lines = [
        '# Public standard-library API', '',
        'Generated with `python3 tools/document_std.py`; run the same command after changing',
        'public declarations. `--check` detects a stale inventory. Signatures and public',
        're-exports below come from source; private imports are omitted. A source path under',
        '`sys/<os>/<arch>` is target-specific even when no additional attribute is shown.',
        'Attributes select declarations; source presence alone does not establish runtime support.',
        'See [system capabilities](SYSTEMS.md) and [compiler targets](../docs/targets.md).', '',
        '```wave', 'import("std::fs::file")::{fs_open};',
        'import("std::fs::file" as file);', '```', '',
        '`fs_open` is the portable file API; `std::sys::fs::open` is its raw OS boundary.',
        'The former `std::libc` binding modules have been removed; they are not public std API.', '',
    ]
    for path in sorted((root / 'std').rglob('*.wave')):
        relative = path.relative_to(root / 'std').as_posix()
        module = 'std::' + relative.removesuffix('.wave').replace('/', '::')
        lines += [f'## `{module}`', '', f'[Source]({relative})', '']
        entries = list(declarations(path.read_text(encoding='utf-8')))
        if not entries:
            lines += ['No public declarations.', '']
        for line, guards, signature in entries:
            lines += [f'[Declaration]({relative}#L{line})', '', '```wave', *guards, signature, '```', '']
    return '\n'.join(lines)


def render_systems(root=ROOT):
    lines = [
        '# Standard-library system capabilities', '',
        'Compiler target acceptance and std provider coverage are separate. `source` below',
        'means provider code or a target-selected facade is present; it does not promise every',
        'operation is implemented or runtime-tested. `limited` has explicit unsupported paths.',
        'Native/QEMU/VM describes the configured CI execution route, not universal API coverage.',
        'Exact declarations and target attributes are in [the public inventory](API.md).', '',
        '| Platform | Compiler | fs/process/memory/time | env | sockets/tty/random | Signals | CI route |',
        '|---|---|---|---|---|---|---|',
        '| Linux amd64/arm64 | registered | source | source | source | unsupported raw signal API | native |',
        '| Linux riscv64/loong64 | registered | source | source | source | unsupported raw signal API | QEMU user |',
        '| macOS amd64/arm64 | registered | source | source; dedicated boundary regression | source | unsupported raw signal API | native |',
        '| Windows amd64/arm64 | registered MSVC | source; some process/console operations limited | source | source; interface enumeration limited | separate console-control contract pending | native |',
        '| FreeBSD amd64 | registered | source; incomplete operations remain | source/limited | source/limited | unsupported raw signal API | VM |',
        '| FreeBSD arm64/riscv64 | registered | source contracts; not native-validated | source contracts | partial source contracts | unsupported | compile only |',
        '| wasm32 WASI Preview 1 | registered | WASI fs/clocks/memory, limited process | source | sockets/tty unsupported; randomness reports NOSYS | unsupported | Node WASI |',
        '| bare wasm32/wasm64 | registered | linear memory; hosted OS operations unsupported | unsupported | unsupported | unsupported | Node host, Memory64 for wasm64 |', '',
        'Linux startup objects are owned by the compiler CRT; native Darwin/Windows/FreeBSD',
        'linking requires the platform runtime. WASI `_start` is compiler-generated.',
        'Freestanding object emission supplies no boot/startup contract. Other BSDs, Android,',
        'iOS, Fuchsia and wasm64 WASI are planned and have no supported runtime claim here.', '',
        'Remaining work: [Windows providers #385](https://github.com/wavefnd/Wave/issues/385),',
        '[FreeBSD #386](https://github.com/wavefnd/Wave/issues/386),',
        '[signals #384](https://github.com/wavefnd/Wave/issues/384),',
        '[Linux interfaces #566](https://github.com/wavefnd/Wave/issues/566),',
        '[Windows interfaces #790](https://github.com/wavefnd/Wave/issues/790).', '',
        '## Provider source index', '',
        'Every current `std::sys` source module is listed below. Facades preserve target',
        'attributes and re-exports; an implementation with an unavailable path is not classified',
        'as fully implemented from its filename. Follow its exact declarations in the API inventory.', '',
        '| Module | Source role |', '|---|---|',
    ]
    for path in sorted((root / 'std/sys').rglob('*.wave')):
        relative = path.relative_to(root / 'std').as_posix()
        text = path.read_text(encoding='utf-8')
        role = 'target-selected facade/re-export' if 'pub import(' in text else 'implementation source'
        if any(marker in text for marker in ['ERR_NOT_IMPLEMENTED', 'return -38;', 'return -52;']):
            role += '; unavailability markers present: inspect the operation contract'
        lines.append(f'| [{relative}]({relative}) | {role}; runtime coverage must be checked separately |')
    lines += ['', 'Refresh this index with `python3 tools/document_std.py` after adding a provider.',
              'Review the capability summary and exact target/CI table when its contract changes.', '']
    return '\n'.join(lines)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--check', action='store_true')
    args = parser.parse_args()
    for name, rendered in [('API.md', render()), ('SYSTEMS.md', render_systems())]:
        destination = ROOT / 'std' / name
        if args.check:
            if not destination.exists() or destination.read_text(encoding='utf-8') != rendered:
                parser.exit(1, f'std/{name} is stale; run python3 tools/document_std.py\n')
        else:
            destination.write_text(rendered, encoding='utf-8')


if __name__ == '__main__':
    main()
