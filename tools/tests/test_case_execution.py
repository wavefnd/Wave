"""Program-exit contracts must not accept build or launch failures."""
import contextlib
import io
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from tools import run_tests as runner
from tools.test_contracts import TestMetadata


class CaseExecutionTests(unittest.TestCase):
    def classify(self, build_status=0, runtime_status=1, create=True, launch_error=None):
        calls = []
        def execute(cmd, **kwargs):
            calls.append((cmd, kwargs))
            if len(calls) == 1:
                self.assertEqual(cmd[1], "build")
                if create and build_status == 0:
                    Path(cmd[cmd.index("-o") + 1]).touch()
                return subprocess.CompletedProcess(cmd, build_status, "", "build diagnostic")
            if launch_error:
                raise launch_error
            return subprocess.CompletedProcess(cmd, runtime_status, "", "")

        with tempfile.TemporaryDirectory() as directory, contextlib.redirect_stdout(io.StringIO()):
            metadata = TestMetadata(expected_exit=1, stdin="input", target="x86_64-pc-windows-msvc")
            with patch.object(runner, "TEST_OUTPUT_DIR", Path(directory)), \
                 patch.object(runner, "parse_test_metadata", return_value=metadata), \
                 patch.object(runner, "manifest_compile_target", return_value=None), \
                 patch.object(runner, "run_process", side_effect=execute):
                result = runner.run_and_classify("case", "case.wave", ["wavec", "run", "case.wave"])
            self.assertEqual(list(Path(directory).iterdir()), [])
        return result, calls

    def test_compile_link_and_compiler_crash_cannot_match_program_status(self):
        for status in [1, 2, -11, 0xc0000005]:
            with self.subTest(status=status):
                result, calls = self.classify(build_status=status)
                self.assertEqual(result[0], 0)
                self.assertIn("build failed", result[1]["reason"])
                self.assertEqual(result[1]["actual_exit"], status)
                self.assertEqual(result[1]["expected_exit"], 0)
                self.assertEqual(result[1]["stderr"], "build diagnostic")
                self.assertEqual(len(calls), 1)

    def test_valid_nonzero_program_executes_after_build_with_target_and_stdin(self):
        result, calls = self.classify()
        self.assertEqual(result, (3, None))
        self.assertEqual(calls[0][0][-2:], ["--target", "x86_64-pc-windows-msvc"])
        self.assertNotIn("input", calls[0][1])
        self.assertEqual(calls[1][1]["input"], "input\n")
        self.assertEqual(len(calls[1][0]), 1)

    def test_missing_output_is_not_executed(self):
        result, calls = self.classify(create=False)
        self.assertEqual(result[0], 0)
        self.assertEqual(len(calls), 1)

    def test_launch_failure_does_not_satisfy_expected_exit(self):
        result, _ = self.classify(launch_error=OSError("cannot launch"))
        self.assertEqual(result[0], 0)
        self.assertIn("launch failed", result[1]["reason"])

    def test_runtime_crash_does_not_satisfy_expected_exit(self):
        for status in [-11, 0xc0000005]:
            with self.subTest(status=status):
                result, _ = self.classify(runtime_status=status)
                self.assertEqual(result[0], 0)
                self.assertEqual(result[1]["actual_exit"], status)
                self.assertEqual(result[1]["expected_exit"], 1)

    def test_build_timeout_remains_failure(self):
        with tempfile.TemporaryDirectory() as directory, contextlib.redirect_stdout(io.StringIO()), \
             patch.object(runner, "TEST_OUTPUT_DIR", Path(directory)), \
             patch.object(runner, "parse_test_metadata", return_value=TestMetadata(expected_exit=1)), \
             patch.object(runner, "manifest_compile_target", return_value=None), \
             patch.object(runner, "run_process", side_effect=subprocess.TimeoutExpired("wavec", 5)):
            result = runner.run_and_classify("case", "case.wave", ["wavec", "run", "case.wave"])
        self.assertEqual(result[0], -1)
        self.assertIn("build timed out", result[1]["reason"])


if __name__ == "__main__":
    unittest.main()
