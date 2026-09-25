import io
import sys
import json
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from tools.run_tests import parse_args, resolve_wavec, main
from tools import run_tests as runner
from tools.test_contracts import TestMetadata
from types import SimpleNamespace

class TestRunTestsCLI(unittest.TestCase):
    def test_report_retains_failure_exit_bits_diagnostics_and_artifact_reason(self):
        scenarios = [(7, None), (0xc0000005, None), (-11, None), (0, "wrong object machine")]
        for code, artifact_error in scenarios:
            with self.subTest(code=code, artifact=artifact_error), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                compiler = root / "wavec"
                compiler.touch()
                report = root / "report.json"
                metadata = TestMetadata(mode="build")
                with patch.object(runner, "iter_test_entries", return_value=[("case", "case.wave")]), \
                     patch.object(runner, "command_for_test", return_value=[str(compiler)]), \
                     patch.object(runner, "parse_test_metadata", return_value=metadata), \
                     patch.object(runner, "run_process", return_value=subprocess.CompletedProcess(
                         [str(compiler)], code, "output", "failure " + "x" * 5000)), \
                     patch.object(runner, "compiler_default_target", return_value="x86_64-unknown-linux-gnu"), \
                     patch.object(runner, "validate_compiled_artifact", return_value=artifact_error), \
                     patch.object(runner.time, "sleep"), patch("sys.stdout", io.StringIO()):
                    with self.assertRaises(SystemExit) as error:
                        main(["--wavec", str(compiler), "--suite", "shared", "--report-json", str(report)])
                    self.assertEqual(error.exception.code, 1)
                row = json.loads(report.read_text())["tests"][0]
                self.assertEqual(row["status"], "fail")
                self.assertEqual(row["actual_exit"], code)
                self.assertEqual(row["expected_exit"], 0)
                self.assertEqual(row["stdout"], "output")
                self.assertEqual(len(row["stderr"]), 4096)
                self.assertTrue(row["stderr_truncated"])
                self.assertIn(artifact_error or f"exit={code}", row["reason"])
                self.assertEqual(row["phase"], "artifact" if artifact_error else "compile")

    def test_serialized_report_identifies_selection_and_preserves_results(self):
        selections = [([], "auto", "linux-amd64", "native"),
                      (["--target-id", "windows-amd64"], "target", "windows-amd64", "native"),
                      (["--target-id", "linux-riscv64"], "target", "linux-riscv64", "qemu"),
                      (["--target-id", "wasm-unknown"], "target", "wasm-unknown", "wasm"),
                      (["--suite", "shared", "--suite", "shared"], "suites", None, "native")]
        for args, mode, target_id, executor in selections:
            with self.subTest(mode=mode, target=target_id), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                compiler = root / "wavec"
                compiler.touch()
                report = root / "result.json"
                with patch.object(runner, "HOST_OS", "linux"), patch.object(runner, "HOST_ARCH", "amd64"), \
                     patch.object(runner, "iter_test_entries", return_value=[("shared/test1.wave", "case.wave")]), \
                     patch.object(runner, "command_for_test", return_value=[str(compiler)]), \
                     patch.object(runner, "run_and_classify", return_value=(1, None)), \
                     patch.object(runner, "compiler_default_target", return_value="x86_64-unknown-linux-gnu"), \
                     patch.object(runner.time, "sleep"), patch("sys.stdout", io.StringIO()):
                    main(["--wavec", str(compiler), "--report-json", str(report), *args])
                data = json.loads(report.read_text())
                selection = data["selection"]
                self.assertEqual(data["schema_version"], 1)
                self.assertEqual(selection["mode"], mode)
                self.assertEqual(selection["id"], target_id)
                self.assertEqual(selection["executor"], executor)
                self.assertTrue(selection["target"])
                self.assertEqual(selection["suites"][0], "shared")
                if mode == "suites":
                    self.assertEqual(selection["suites"], ["shared"])
                else:
                    target = runner.load_case_manifest().target(target_id)
                    self.assertEqual(selection["suites"], list(target.suites))
                    if target.target:
                        self.assertEqual(selection["target"], target.target)
                self.assertEqual(data["summary"], {"pass": 1, "fail": 0, "skip": 0, "timeout": 0})
                self.assertEqual(data["tests"], [{"name": "shared/test1.wave", "status": "pass"}])

    def test_explicit_compiler_wins_over_stale_default_and_missing_is_fatal(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            stale = root / "target/release/wavec.exe"
            stale.parent.mkdir(parents=True)
            stale.touch()
            selected = root / "msvc-wavec.exe"
            selected.touch()
            self.assertEqual(resolve_wavec(root, selected), selected.resolve())
            with self.assertRaises(ValueError):
                resolve_wavec(root, root / "missing.exe")

    def test_explicit_native_manifest_target_still_executes_cases(self):
        for target_id, triple in [("windows-amd64", "x86_64-pc-windows-msvc"),
                                   ("windows-arm64", "aarch64-pc-windows-msvc")]:
            with self.subTest(target=target_id), \
                 patch.object(runner, "ARGS", SimpleNamespace(suite=[], target_id=target_id)), \
                 patch.object(runner, "WAVEC", "selected-wavec.exe"), \
                 patch.object(runner, "parse_test_metadata", return_value=TestMetadata()):
                runner.configured_target.cache_clear()
                try:
                    self.assertIsNone(runner.manifest_compile_target())
                    self.assertEqual(runner.command_for_test("case", "case.wave"),
                                     ["selected-wavec.exe", "run", "case.wave", "--target", triple])
                finally:
                    runner.configured_target.cache_clear()

    def test_help_shows_usage_and_creates_no_output_dir(self):
        tmp_dir = tempfile.gettempdir()
        before_dirs = set(Path(tmp_dir).glob("wave-test-output-*"))

        stdout = io.StringIO()
        with patch("sys.stdout", stdout):
            with self.assertRaises(SystemExit) as cm:
                parse_args(["--help"])

        self.assertEqual(cm.exception.code, 0)
        self.assertIn("Run Wave end-to-end tests", stdout.getvalue())

        after_dirs = set(Path(tmp_dir).glob("wave-test-output-*"))
        new_dirs = after_dirs - before_dirs
        self.assertEqual(len(new_dirs), 0, f"Unexpected temp dir created: {new_dirs}")

    def test_main_help_creates_no_output_dir(self):
        tmp_dir = tempfile.gettempdir()
        before_dirs = set(Path(tmp_dir).glob("wave-test-output-*"))

        stdout = io.StringIO()
        with patch("sys.stdout", stdout):
            with self.assertRaises(SystemExit) as cm:
                main(["--help"])

        self.assertEqual(cm.exception.code, 0)
        self.assertIn("Run Wave end-to-end tests", stdout.getvalue())

        after_dirs = set(Path(tmp_dir).glob("wave-test-output-*"))
        new_dirs = after_dirs - before_dirs
        self.assertEqual(len(new_dirs), 0, f"Unexpected temp dir created: {new_dirs}")

    def test_missing_compiler_retains_actionable_error(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            empty_root = Path(tmp_dir)
            stdout = io.StringIO()
            with patch("sys.stdout", stdout):
                with self.assertRaises(SystemExit) as cm:
                    resolve_wavec(empty_root)

            self.assertEqual(cm.exception.code, 1)
            self.assertIn(
                "wavec not found. Run `cargo build --release` or `cargo build` first.",
                stdout.getvalue(),
            )

    def test_binary_program_output_does_not_abort_classification(self):
        """Binary bytes on either stream must not raise or skip report data."""
        for stream in ("stdout", "stderr"):
            with self.subTest(stream=stream), patch("sys.stdout", io.StringIO()):
                command = [
                    sys.executable, "-c",
                    f"import sys; sys.{stream}.buffer.write(bytes([255]))",
                ]
                status, detail = runner.classify_program(
                    "binary-output", "case.wave", command, TestMetadata(), None,
                )
                self.assertEqual(status, 1, detail)
                self.assertIsNone(detail)

    def test_binary_program_output_preserves_nonzero_exit_and_diagnostics(self):
        with patch("sys.stdout", io.StringIO()):
            command = [
                sys.executable, "-c",
                "import sys; sys.stdout.buffer.write(b'ok' + bytes([255])); "
                "sys.stderr.buffer.write(bytes([255])); sys.exit(7)",
            ]
            status, detail = runner.classify_program(
                "binary-fail", "case.wave", command, TestMetadata(), None,
            )
        self.assertEqual(status, 0)
        self.assertEqual(detail["actual_exit"], 7)
        self.assertEqual(detail["expected_exit"], 0)
        self.assertEqual(detail["phase"], "run")
        self.assertIn("exit=7", detail["reason"])
        self.assertEqual(detail["stdout"], "ok\ufffd")
        self.assertEqual(detail["stderr"], "\ufffd")
        # Bounded JSON diagnostics remain UTF-8-safe for report generation.
        json.dumps(detail)

    def test_ordinary_utf8_program_output_is_preserved(self):
        with patch("sys.stdout", io.StringIO()):
            command = [
                sys.executable, "-c",
                "import sys; "
                "sys.stdout.buffer.write(('caf' + chr(0xe9) + chr(10)).encode()); "
                "sys.stderr.buffer.write(('na' + chr(0xef) + 've').encode()); "
                "sys.exit(9)",
            ]
            status, detail = runner.classify_program(
                "utf8-output", "case.wave", command, TestMetadata(), None,
            )
        self.assertEqual(status, 0, detail)
        self.assertEqual(detail["actual_exit"], 9)
        self.assertEqual(detail["stdout"], "caf\u00e9\n")
        self.assertEqual(detail["stderr"], "na\u00efve")

    def test_found_compiler(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            fake_root = Path(tmp_dir)
            fake_wavec = fake_root / "target" / "release" / "wavec"
            fake_wavec.parent.mkdir(parents=True, exist_ok=True)
            fake_wavec.touch()

            result = resolve_wavec(fake_root)
            self.assertEqual(result, fake_wavec)

if __name__ == "__main__":
    unittest.main()
