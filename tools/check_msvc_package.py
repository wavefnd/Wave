# SPDX-License-Identifier: MPL-2.0
"""Run the extracted MSVC release with only packaged tools and installed SDKs."""
import argparse
import ctypes
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import zipfile

if __package__ in (None, ''):
    sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from tools import windows_package as pe
from tools.process_tree import run_process, timeout_output

ROOT = Path(__file__).resolve().parent.parent
TOOLS = ('ld.lld', 'lld-link', 'llvm-readobj', 'llc', 'llvm-as', 'llvm-mc')


def controlled_environment(package, home, inherited):
    # Whitelist Windows process/SDK discovery essentials. In particular no
    # checkout LLVM, Developer Prompt LIB/INCLUDE, compiler overrides or PATH.
    keep = {'systemroot', 'windir', 'comspec', 'pathext', 'programfiles',
            'programfiles(x86)', 'programw6432', 'systemdrive'}
    env = {k: v for k, v in inherited.items() if k.lower() in keep}
    system = Path(next(v for k, v in env.items() if k.lower() == 'systemroot'))
    temp = home / 'temp'
    temp.mkdir(parents=True)
    env.update(HOME=str(home), USERPROFILE=str(home), TEMP=str(temp), TMP=str(temp),
               PATH=os.pathsep.join(map(str, [package, package / 'llvm/bin', system / 'System32', system])),
               NO_COLOR='1')
    return env


def require_native(target):
    if os.name != 'nt':
        raise RuntimeError('package acceptance requires native Windows')
    from ctypes import wintypes
    api = ctypes.WinDLL('kernel32', use_last_error=True)
    api.GetCurrentProcess.restype = wintypes.HANDLE
    api.IsWow64Process2.argtypes = [wintypes.HANDLE, ctypes.POINTER(wintypes.USHORT), ctypes.POINTER(wintypes.USHORT)]
    api.IsWow64Process2.restype = wintypes.BOOL
    process, native = wintypes.USHORT(), wintypes.USHORT()
    if not api.IsWow64Process2(api.GetCurrentProcess(), ctypes.byref(process), ctypes.byref(native)):
        raise ctypes.WinError(ctypes.get_last_error())
    if process.value or native.value != pe.MACHINES[target]:
        raise RuntimeError(f'native {target} required, process={process.value:#x}, host={native.value:#x}')


def audit(options):
    output = options.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    report = {'target': options.target, 'commands': [], 'passed': False}
    try:
        require_native(options.target)
        extracted = output / '한글 package with spaces'
        with zipfile.ZipFile(options.archive) as archive:
            for name in archive.namelist():
                normalized = name.replace('\\', '/')
                if normalized.startswith('/') or ':' in normalized or '..' in normalized.split('/'):
                    raise ValueError(f'unsafe archive member: {name}')
            archive.extractall(extracted)
        packages = list(extracted.iterdir())
        if len(packages) != 1 or not packages[0].is_dir():
            raise ValueError('expected one package root in ZIP')
        package = packages[0]
        files = list(package.rglob('*'))
        for path in files:
            if any(part in path.name.lower() for part in ('mingw', 'libgcc', 'libstdc++', 'winpthread')):
                raise ValueError(f'retired GNU payload: {path}')
        binaries = [p for p in files if p.suffix.lower() in ('.exe', '.dll')]
        required = [package / 'wavec.exe', *[package / 'llvm/bin' / (t + '.exe') for t in TOOLS]]
        required.extend(package / 'licenses' / (t + '.txt') for t in ('LLVM', 'compiler-rt', 'libxml2', 'Wave', 'std'))
        required.append(package / 'llvm/lib/clang/21/lib/windows' /
                        f'clang_rt.builtins-{options.target.split("-")[0]}.lib')
        for path in required:
            if not path.is_file():
                raise FileNotFoundError(f'missing package component: {path}')
        inspector = package / 'llvm/bin/llvm-readobj.exe'
        payloads, prerequisites = pe.dependency_closure(
            binaries, options.target, inspector, [package, package / 'llvm/bin'],
            [Path(os.environ['SystemRoot']) / 'System32'])
        report['vc_runtime_prerequisites'] = list(prerequisites)
        report['payload_dependencies'] = list(payloads)
        home = output / 'isolated home'
        home.mkdir()
        env = controlled_environment(package, home, os.environ)
        work = output / 'detached working directory'
        work.mkdir()
        def command(args, expected=None):
            record = {'command': list(map(str, args)), 'cwd': str(work)}
            report['commands'].append(record)
            try:
                result = run_process(record['command'], cwd=work, env=env,
                                     timeout=90, text=True, errors='replace', capture_output=True)
                record.update(exit=result.returncode, stdout=result.stdout, stderr=result.stderr)
            except subprocess.TimeoutExpired as error:
                record.update(timeout=True, output=timeout_output(error))
                raise
            if result.returncode:
                raise RuntimeError(f'{args[0]} failed ({result.returncode}): {result.stderr}')
            if expected is not None and result.stdout.replace('\r\n', '\n') != expected:
                raise AssertionError(f'unexpected stdout: {result.stdout!r}')
        wavec = package / 'wavec.exe'
        command([wavec, '-V'])
        command([wavec, 'print', 'host-target'], options.target + '\n')
        command([wavec, 'print', 'default-target'], options.target + '\n')
        for tool in TOOLS:
            command([package / 'llvm/bin' / (tool + '.exe'), '--version'])
        for source, stdout in [('implicit', 'native implicit main\n'),
                               ('wide_numeric', 'native wide numeric checked\n'),
                               ('package_std', 'packaged std checked\n')]:
            # Inputs are copied before launching; children have no checkout or
            # provisioning paths, including when a source imports Wave std.
            copied = work / (source + '.wave')
            shutil.copy2(ROOT / 'tests/fixtures/msvc_native' / copied.name, copied)
            for static in (False, True):
                exe = work / (source + ('-static' if static else '-dynamic') + '.exe')
                command([wavec, 'build', copied, '-o', exe, *(['--static'] if static else [])])
                if pe.pe_machine(exe) != pe.MACHINES[options.target]:
                    raise ValueError(f'wrong generated machine: {exe}')
                # Neither generated programs nor packaged tools may acquire
                # unnoticed MinGW or host-build dependencies.
                pe.dependency_closure([exe], options.target, inspector, [package, package / 'llvm/bin'],
                                      [Path(os.environ['SystemRoot']) / 'System32'])
                command([exe], stdout)
            command([wavec, 'run', copied], stdout)
        # Record the installed SDK and VC library paths selected without a
        # Developer Prompt, alongside the actual executable commands above.
        command([wavec, 'build', work / 'package_std.wave', '--std-root', package / 'std', '--dry-run', '-o', work / 'sdk-plan.exe'])
        report['passed'] = True
    except (OSError, ValueError, RuntimeError, AssertionError, subprocess.TimeoutExpired, zipfile.BadZipFile) as error:
        report['error'] = str(error)
        print(error, file=sys.stderr)
    finally:
        (output / 'report.json').write_text(json.dumps(report, indent=2, ensure_ascii=False) + '\n', encoding='utf-8')
    return 0 if report['passed'] else 1


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--archive', type=Path, required=True)
    parser.add_argument('--target', choices=pe.MACHINES, required=True)
    parser.add_argument('--output', type=Path, required=True)
    return audit(parser.parse_args())


if __name__ == '__main__':
    sys.exit(main())
