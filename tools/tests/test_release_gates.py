# SPDX-License-Identifier: MPL-2.0
"""Release-gate failure paths use temporary inputs and controlled processes."""
import argparse
import contextlib
import errno
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch

from tools import check_case_sources as source_check, run_runtime_cases as runtime
from tools import check_wave_corpus as corpus, run_tests as native
from tools import check_std_policy as policy, check_freebsd_sys as freebsd
from tools.test_contracts import TestMetadata, read_elf_contract
from tools.test_test_contracts import make_elf


class ReportProtectionTests(unittest.TestCase):
    def test_both_runners_reject_source_and_compiler_aliases_before_execution(self):
        for runner in ('source', 'runtime'):
            for protected in ('source', 'compiler'):
                for alias in ('same', 'normalized', 'symlink', 'hardlink'):
                    with self.subTest(runner=runner, protected=protected, alias=alias), tempfile.TemporaryDirectory() as directory:
                        root = Path(directory)
                        cases = root / 'tests/cases'
                        cases.mkdir(parents=True)
                        source, compiler = cases / 'input.wave', root / 'wavec'
                        source.write_bytes(b'fun main() {}\n')
                        compiler.write_bytes(b'compiler bytes')
                        target = source if protected == 'source' else compiler
                        report = target
                        if alias == 'normalized':
                            (root / 'sub').mkdir()
                            report = root / 'sub' / '..' / target.relative_to(root)
                        elif alias in ('symlink', 'hardlink'):
                            report = root / 'report.json'
                            try:
                                if alias == 'symlink': report.symlink_to(target)
                                else: os.link(target, report)
                            except OSError as error:
                                if error.errno in (errno.EPERM, errno.EACCES, errno.ENOTSUP):
                                    continue
                                raise
                        before = (source.read_bytes(), compiler.read_bytes())
                        with patch.object(source_check, 'ROOT', root), patch.object(source_check, 'CASES_ROOT', cases), \
                             patch.object(runtime, 'ROOT', root), patch.object(source_check, 'run_process') as check, \
                             patch.object(runtime, 'run_process') as run, contextlib.redirect_stderr(io.StringIO()):
                            if runner == 'source':
                                status = source_check.check_sources(compiler, ['input.wave'], report)
                            else:
                                status = runtime.execute(argparse.Namespace(wavec=compiler, sources=['input.wave'], report_json=report))
                            self.assertNotEqual(status, 0)
                            check.assert_not_called()
                            run.assert_not_called()
                        self.assertEqual((source.read_bytes(), compiler.read_bytes()), before)

    def test_manifest_failure_report_cannot_replace_source(self):
        with tempfile.TemporaryDirectory() as directory:
            source = Path(directory) / 'input.wave'
            source.write_text('original')
            with patch.object(source_check, 'CASES_ROOT', Path(directory)), \
                 patch.object(source_check, 'load_case_manifest', side_effect=ValueError('bad manifest')), \
                 contextlib.redirect_stderr(io.StringIO()):
                self.assertEqual(source_check.main(['--wavec', 'missing', '--report-json', str(source)]), 1)
            self.assertEqual(source.read_text(), 'original')

    def test_checkout_std_path_with_spaces_is_passed_to_source_checker(self):
        with tempfile.TemporaryDirectory(prefix='wave root ') as directory:
            root = Path(directory)
            with patch.object(source_check, 'ROOT', root), \
                 patch.object(source_check, 'run_process', return_value=subprocess.CompletedProcess([], 0, '', '')) as run:
                self.assertEqual(source_check.check_sources(sys.executable, ['sample.wave'], root / 'report.json'), 0)
            command = run.call_args.args[0]
            self.assertEqual(command[command.index('--std-root') + 1], str(root / 'std'))


class CorpusFailureTests(unittest.TestCase):
    def test_caller_relative_compiler_and_environment_remain_absolute(self):
        with tempfile.TemporaryDirectory(prefix='caller space ') as directory:
            root = Path(directory)
            executable = root / 'compiler space'
            executable.touch(mode=0o700)
            old = Path.cwd()
            try:
                os.chdir(root)
                with patch.object(corpus.shutil, 'which', return_value='wrong-PATH-compiler'):
                    self.assertEqual(corpus.resolve_wavec(Path('./compiler space')), executable.resolve())
                    with patch.dict(os.environ, WAVEC='./compiler space'):
                        self.assertEqual(corpus.resolve_wavec(None), executable.resolve())
            finally:
                os.chdir(old)

    def test_launch_errors_are_reported_in_both_loops(self):
        for error in (OSError(errno.ENOEXEC, 'invalid format'), FileNotFoundError(errno.ENOENT, 'missing loader')):
            with self.subTest(error=error), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                source = root / 'examples/std/example.wave'
                source.parent.mkdir(parents=True)
                source.write_text('fun main() {}')
                out, err = io.StringIO(), io.StringIO()
                with patch.object(corpus, 'ROOT', root), patch.object(corpus, 'resolve_wavec', return_value=Path(sys.executable)), \
                     patch.object(corpus, 'run_process', side_effect=error) as run, \
                     contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
                    self.assertEqual(corpus.main(['--run-std-examples']), 1)
                    self.assertEqual(run.call_count, 2)
                self.assertIn(str(source.relative_to(root)), err.getvalue())
                self.assertIn('compiler launch failed', err.getvalue())
                self.assertNotIn('Traceback', err.getvalue())

    def test_empty_corpus_or_requested_examples_never_launches_compiler(self):
        for std_source in (False, True):
            with self.subTest(std_source=std_source), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                if std_source:
                    (root / 'std').mkdir()
                    (root / 'std/a.wave').touch()
                with patch.object(corpus, 'ROOT', root), patch.object(corpus, 'resolve_wavec', return_value=Path(sys.executable)), \
                     patch.object(corpus, 'run_process') as run, contextlib.redirect_stderr(io.StringIO()):
                    self.assertEqual(corpus.main(['--run-std-examples']), 2)
                    run.assert_not_called()

    def test_nonempty_success_still_passes(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'std').mkdir()
            (root / 'std/a.wave').touch()
            with patch.object(corpus, 'ROOT', root), patch.object(corpus, 'resolve_wavec', return_value=Path(sys.executable)), \
                 patch.object(corpus, 'run_process', return_value=subprocess.CompletedProcess([], 0, '', '')):
                self.assertEqual(corpus.main([]), 0)


class ArtifactFailureTests(unittest.TestCase):
    def test_compile_only_requires_object_for_single_and_multifile_cases(self):
        for source_name in ('test1.wave', 'test2/main.wave'):
            for status, create in ((0, False), (7, False), (0, True)):
                with self.subTest(source=source_name, status=status, create=create), tempfile.TemporaryDirectory() as directory:
                    root = Path(directory)
                    source = root / source_name
                    source.parent.mkdir(parents=True, exist_ok=True)
                    source.write_text('fun main() {}')
                    out = root / 'out'
                    (out / 'logical-case').mkdir(parents=True)
                    if create: (out / 'logical-case' / (source.stem + '.o')).write_bytes(b'object')
                    target = SimpleNamespace(target='aarch64-unknown-freebsd')
                    with patch.object(native, 'ROOT', root), patch.object(native, 'TEST_OUTPUT_DIR', out), \
                         patch.object(native, 'run_process', return_value=subprocess.CompletedProcess([], status, '', '')), \
                         contextlib.redirect_stdout(io.StringIO()):
                        result = native.classify_program('logical case', source_name, ['fake'], TestMetadata(), target)
                    self.assertEqual(result[0], 1 if status == 0 and create else 0)
                    if status == 0 and not create: self.assertIn('expected artifact', result[1]['reason'])

    def test_elf_header_contracts_cover_both_classes_and_byte_orders(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'object.o'
            for bits in (32, 64):
                for order in ('little', 'big'):
                    data = make_elf(bits=bits)
                    data[5] = 1 if order == 'little' else 2
                    for offset, width, value in [(16, 2, 1), (18, 2, 243), (20, 4, 1),
                                                  (52 if bits == 64 else 40, 2, len(data)),
                                                  (48 if bits == 64 else 36, 4, 4)]:
                        data[offset:offset + width] = value.to_bytes(width, order)
                    path.write_bytes(data)
                    parsed, error = read_elf_contract(path)
                    self.assertIsNone(error)
                    self.assertEqual(parsed, {'bits': bits, 'machine': 243, 'flags': 4})
                    invalid = [data[:51], data[:-1]]
                    for offset, width, value in [(6, 1, 0), (20, 4, 2), (16, 2, 2), (16, 2, 3),
                                                  (52 if bits == 64 else 40, 2, 0)]:
                        corrupt = data.copy()
                        corrupt[offset:offset + width] = value.to_bytes(width, order)
                        invalid.append(corrupt)
                    for corrupt in invalid:
                        path.write_bytes(corrupt)
                        self.assertIsNotNone(read_elf_contract(path)[1])


class NativeProgressTests(unittest.TestCase):
    def test_interruption_retains_finished_active_and_unstarted_cases(self):
        for before in (0, 1):
            with self.subTest(before=before), tempfile.TemporaryDirectory() as directory:
                report = Path(directory) / 'report.json'
                entries = [('one', 'one.wave'), ('two', 'two.wave'), ('three', 'three.wave')]
                outcomes = [(1, None)] * before + [KeyboardInterrupt()]
                with patch.object(native, 'resolve_wavec', return_value=Path(sys.executable)), \
                     patch.object(native, 'iter_test_entries', return_value=entries), \
                     patch.object(native, 'command_for_test', return_value=['fake']), \
                     patch.object(native, 'run_and_classify', side_effect=outcomes), \
                     patch.object(native, 'manifest_compile_target', return_value=None), \
                     patch.object(native, 'report_selection', return_value={}), patch.object(native.time, 'sleep'), \
                     contextlib.redirect_stdout(io.StringIO()), self.assertRaises(SystemExit) as error:
                    native.main(['--report-json', str(report)])
                self.assertEqual(error.exception.code, 130)
                rows = json.loads(report.read_text())['tests']
                self.assertEqual([row['status'] for row in rows], ['pass'] * before + ['interrupted'] + ['not_run'] * (2 - before))
                self.assertFalse(list(report.parent.glob('.*.tmp')))

    def test_report_failure_stops_before_running_unrecordable_tests(self):
        with patch.object(native, 'resolve_wavec', return_value=Path(sys.executable)), \
             patch.object(native, 'iter_test_entries', return_value=[('one', 'one.wave')]), \
             patch.object(native, 'manifest_compile_target', return_value=None), \
             patch.object(native, 'report_selection', return_value={}), \
             patch.object(native, 'write_report', side_effect=OSError('disk full')), \
             patch.object(native, 'run_and_classify') as run, contextlib.redirect_stderr(io.StringIO()) as err, \
             self.assertRaises(SystemExit) as error:
            native.main(['--report-json', '/tmp/wave-unused-report.json'])
        self.assertEqual(error.exception.code, 1)
        self.assertIn('failed to write test report', err.getvalue())
        run.assert_not_called()


class PolicyTests(unittest.TestCase):
    def test_layout_and_nested_comments_do_not_bypass_binding_rules(self):
        for source in ('extern(c) fun f();', 'extern ( c , "name" ) fun f();',
                       'extern\n(/* outer /* inner */ end */ c) fun f();',
                       'import ( "std::libc::io" );', 'import\n(/* comment */ "std::libc::io");',
                       r'import("std::\x6cibc::io");'):
            with self.subTest(source=source):
                self.assertTrue(list(policy.violations('std/math/test.wave', source)))
        self.assertFalse(list(policy.violations('std/math/test.wave',
            '// extern(c)\n/* import("std::libc::io") */\nvar x: str = "extern(c)";')))
        self.assertFalse(list(policy.violations('std/sys/linux/event.wave', 'extern /* x */ (c) fun f();')))
        self.assertTrue(list(policy.violations('tests/case.wave', 'fun main() { let /* x */ mut a: i32; }')))
        self.assertTrue(list(policy.violations('tests/case.wave', '#[target(os="linux")]\nlet a: i32;')))

    def test_removed_binding_directory_does_not_bypass_policy(self):
        for source in ('extern(c) fun f();', 'import("std::libc::stdio")::{puts};',
                       'import("std::libc");'):
            with self.subTest(source=source):
                self.assertTrue(list(policy.violations('std/libc/io.wave', source)))

    def test_os_c_abi_providers_retain_their_binding_permissions(self):
        for path in ('std/sys/linux/amd64/memory.wave', 'std/sys/linux/vector_io.wave',
                     'std/sys/macos/arm64/memory.wave', 'std/sys/macos/common/env.wave',
                     'std/sys/freebsd/event.wave',
                     'std/sys/wasm/io.wave', 'std/sys/wasm64/memory.wave'):
            with self.subTest(path=path):
                self.assertFalse(list(policy.violations(path, 'extern(c) fun os_api();')))
                self.assertTrue(list(policy.violations(path, 'import("std::libc::stdio");')))

    def test_search_failures_are_distinct_from_no_matches(self):
        for result, expected in [(FileNotFoundError('rg missing'), 1),
                                 (subprocess.CompletedProcess([], 2, '', 'search failed'), 1),
                                 (subprocess.CompletedProcess([], 1, '', ''), 0)]:
            with patch.object(policy.subprocess, 'run', side_effect=result if isinstance(result, Exception) else None,
                              return_value=None if isinstance(result, Exception) else result), \
                 contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
                self.assertEqual(policy.main(), expected)

    def test_active_binding_fails_and_missing_source_is_not_clean(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'std').mkdir()
            path = root / 'std/test.wave'
            path.write_text('extern (c) fun forbidden();')
            with patch.object(policy.subprocess, 'run', return_value=subprocess.CompletedProcess([], 0, 'std/test.wave\n', '')), \
                 contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
                self.assertEqual(policy.main(root), 1)
                path.write_text('// extern(c) fun example();')
                self.assertEqual(policy.main(root), 0)
                path.unlink()
                self.assertEqual(policy.main(root), 1)


class FreeBSDDiscoveryTests(unittest.TestCase):
    def test_mixed_case_layout_uses_numeric_order_and_logical_names(self):
        with tempfile.TemporaryDirectory() as directory:
            suite = Path(directory)
            (suite / 'test10.wave').touch()
            (suite / 'test2').mkdir()
            (suite / 'test2/main.wave').touch()
            (suite / 'test1.wave').touch()
            self.assertEqual([(name, path.relative_to(suite).as_posix()) for name, path in freebsd.discover_cases(suite)],
                             [('test1', 'test1.wave'), ('test2', 'test2/main.wave'), ('test10', 'test10.wave')])
            (suite / 'test2.wave').touch()
            with self.assertRaisesRegex(ValueError, 'ambiguous'): freebsd.discover_cases(suite)

    def test_empty_and_invalid_discovery_starts_no_external_command(self):
        for name in (None, 'testoops.wave', 'test1'):
            with tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                suite = root / 'tests/cases/freebsd/amd64'
                suite.mkdir(parents=True)
                if name == 'test1': (suite / name).mkdir()
                elif name: (suite / name).touch()
                with patch.object(freebsd, 'ROOT', root), patch.object(freebsd.Commands, 'run') as run, \
                     self.assertRaises(ValueError):
                    freebsd.execute(SimpleNamespace(arch='amd64'), {})
                run.assert_not_called()


if __name__ == '__main__':
    unittest.main()
