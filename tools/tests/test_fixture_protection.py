# SPDX-License-Identifier: MPL-2.0
"""Fixture I/O is UTF-8 and report aliases cannot replace validation inputs."""
import contextlib
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

from tools import run_runtime_cases, run_tests
from tools.process_tree import run_process
from tools.test_contracts import TestMetadata


class FixtureProtectionTests(unittest.TestCase):
    def test_both_runners_reject_every_fixture_report_alias_before_any_child(self):
        names = ("input.stdin", "output.stdout", "error.stderr", "environment.json")
        for runner in ("native", "runtime"):
            for name in names:
                for alias in ("direct", "normalized", "hardlink", "symlink"):
                    with self.subTest(runner=runner, fixture=name, alias=alias):
                        with tempfile.TemporaryDirectory() as directory:
                            root = Path(directory)
                            source = root / "tests/cases/shared/test1.wave"
                            source.parent.mkdir(parents=True)
                            source.write_text("fun main() {}\n", encoding="utf-8")
                            fixture = root / "tests/fixtures/wave-test" / name
                            fixture.parent.mkdir(parents=True)
                            original = "fixture input sentinel café\n".encode("utf-8")
                            fixture.write_bytes(original)
                            compiler = root / "fake-compiler"
                            compiler.write_bytes(b"must never execute")
                            report = fixture
                            if alias == "normalized":
                                report = fixture.parent / ".." / fixture.parent.name / fixture.name
                            elif alias == "hardlink":
                                report = root / "report.json"
                                try:
                                    os.link(fixture, report)
                                except OSError as error:
                                    self.skipTest(f"hard links unavailable: {error}")
                            elif alias == "symlink":
                                report = root / "report.json"
                                try:
                                    report.symlink_to(fixture)
                                except OSError as error:
                                    self.skipTest(f"symbolic links unavailable: {error}")
                            stdout, stderr = io.StringIO(), io.StringIO()
                            with (
                                contextlib.redirect_stdout(stdout),
                                contextlib.redirect_stderr(stderr),
                            ):
                                if runner == "native":
                                    with (
                                        patch.object(run_tests, "ROOT", root),
                                        patch.object(
                                            run_tests, "resolve_wavec", return_value=compiler,
                                        ),
                                        patch.object(run_tests, "iter_test_entries", return_value=[
                                            ("case", "tests/cases/shared/test1.wave"),
                                        ]),
                                        patch.object(
                                            run_tests, "run_and_classify",
                                            side_effect=AssertionError("compiler must not run"),
                                        ) as classify,
                                        patch.object(
                                            run_tests, "manifest_compile_target", return_value=None,
                                        ),
                                        patch.object(
                                            run_tests, "report_selection",
                                            return_value={"target": "test-target"},
                                        ),
                                        patch.object(run_tests, "run_process") as child,
                                        self.assertRaises(SystemExit) as failure,
                                    ):
                                        run_tests.main([
                                            "--wavec", str(compiler), "--suite", "shared",
                                            "--report-json", str(report),
                                        ])
                                    self.assertEqual(failure.exception.code, 2)
                                    classify.assert_not_called()
                                    child.assert_not_called()
                                else:
                                    options = SimpleNamespace(
                                        wavec=compiler, report_json=report,
                                        sources=["shared/test1.wave"], target_id="unused",
                                    )
                                    with (
                                        patch.object(run_runtime_cases, "ROOT", root),
                                        patch.object(
                                            run_runtime_cases, "load_case_manifest",
                                            side_effect=ValueError("manifest must not load"),
                                        ) as manifest,
                                        patch.object(run_runtime_cases, "run_process") as child,
                                    ):
                                        self.assertEqual(run_runtime_cases.execute(options), 2)
                                    manifest.assert_not_called()
                                    child.assert_not_called()
                            self.assertIn("report path aliases an input", stderr.getvalue())
                            self.assertEqual(fixture.read_bytes(), original)


class FixtureEncodingTests(unittest.TestCase):
    echo = (
        "import sys; sys.stdout.buffer.write(sys.stdin.buffer.read()); "
        "sys.stderr.buffer.write('검증 café'.encode('utf-8'))"
    )

    def fixtures(self, root):
        fixtures = root / "tests/fixtures"
        fixtures.mkdir(parents=True)
        for name in ("input.txt", "stdout.txt"):
            (fixtures / name).write_text("입력 café\n", encoding="utf-8")
        (fixtures / "stderr.txt").write_text("검증 café", encoding="utf-8")
        return TestMetadata(
            stdin_file="input.txt", stdout_file="stdout.txt", stderr_file="stderr.txt",
        )

    def test_native_fixture_streams_are_utf8_with_a_non_utf8_parent_locale(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            metadata = self.fixtures(root)
            with (
                patch.object(run_tests, "ROOT", root),
                patch("subprocess._text_encoding", return_value="cp1252"),
                contextlib.redirect_stdout(io.StringIO()),
                contextlib.redirect_stderr(io.StringIO()),
            ):
                status, detail = run_tests.classify_program(
                    "unicode", "test.wave", [sys.executable, "-c", self.echo], metadata, None,
                )
            self.assertEqual(status, 1, detail)
            self.assertIsNone(detail)

    def test_runtime_fixture_streams_are_utf8_with_a_non_utf8_parent_locale(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.fixtures(root)
            source = root / "tests/cases/shared/test1.wave"
            source.parent.mkdir(parents=True)
            source.write_text(
                "// wave-test: stdin-file=input.txt, stdout-file=stdout.txt, "
                "stderr-file=stderr.txt\n"
                "fun main() {}\n", encoding="utf-8",
            )
            compiler = root / "fake-compiler"
            compiler.write_bytes(b"not executed")
            options = SimpleNamespace(
                wavec=compiler, target_id="test-target", sources=["shared/test1.wave"],
                report_json=root / "report.json", qemu="fake-qemu", sysroot=root,
                timeout=10, build_timeout=10,
            )
            target = SimpleNamespace(
                enabled=True, executor="qemu", target="test-triple", suites=("shared",),
            )
            calls = []

            def child(command, **kwargs):
                calls.append(command)
                if command[0] == str(compiler.resolve()):
                    Path(command[command.index("-o") + 1]).touch()
                    return subprocess.CompletedProcess(command, 0, "", "")
                self.assertEqual(command[0], "fake-qemu")
                # Only target compilation/emulation is replaced. The actual
                # production process helper encodes stdin and decodes both
                # streams of this real short-lived Unicode echo process.
                return run_process([sys.executable, "-c", self.echo], **kwargs)

            with (
                patch.object(run_runtime_cases, "ROOT", root),
                patch.object(
                    run_runtime_cases, "load_case_manifest",
                    return_value=SimpleNamespace(target=lambda _: target),
                ),
                patch.object(run_runtime_cases, "run_process", side_effect=child),
                patch("subprocess._text_encoding", return_value="cp1252"),
                contextlib.redirect_stdout(io.StringIO()),
                contextlib.redirect_stderr(io.StringIO()),
            ):
                status = run_runtime_cases.execute(options)
            report = json.loads(options.report_json.read_text(encoding="utf-8"))
            self.assertEqual(status, 0, report)
            self.assertEqual(report["summary"]["pass"], 1)
            self.assertEqual(len(calls), 2)
            runtime = report["tests"][0]["commands"][-1]
            self.assertEqual(runtime["stdout"], "입력 café\n")
            self.assertEqual(runtime["stderr"], "검증 café")


if __name__ == "__main__":
    unittest.main()
