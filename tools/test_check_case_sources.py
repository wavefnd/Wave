import json
import os
import stat
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from tools import check_case_sources as checker


class SourceCheckTests(unittest.TestCase):
    def test_checks_after_multiple_failures_and_keeps_diagnostics(self):
        with tempfile.TemporaryDirectory() as directory:
            report = Path(directory) / "report.json"
            outcomes = [subprocess.CompletedProcess([], 1, "", "bad first"),
                        subprocess.CompletedProcess([], 2, "", "bad second"),
                        subprocess.CompletedProcess([], 0, "last ran", "")]
            with patch.object(checker, "run_process", side_effect=outcomes) as run:
                self.assertEqual(checker.check_sources("wavec", ["one", "two", "three"], report), 1)
            self.assertEqual(run.call_count, 3)
            records = json.loads(report.read_text())["results"]
            self.assertEqual([r["exit_code"] for r in records], [1, 2, 0])
            self.assertEqual([r["status"] for r in records], ["failed", "failed", "passed"])
            self.assertEqual(records[1]["stderr"], "bad second")
            self.assertEqual(records[2]["stdout"], "last ran")

    def test_launch_and_timeout_errors_do_not_hide_later_checks(self):
        with tempfile.TemporaryDirectory() as directory:
            report = Path(directory) / "report.json"
            outcomes = [OSError("missing compiler"), subprocess.TimeoutExpired("wavec", 15),
                        subprocess.CompletedProcess([], 0, "", "")]
            with patch.object(checker, "run_process", side_effect=outcomes):
                self.assertEqual(checker.check_sources("wavec", ["a", "b", "c"], report), 1)
            records = json.loads(report.read_text())["results"]
            self.assertEqual([r["status"] for r in records], ["failed", "timeout", "passed"])
            self.assertIn("missing compiler", records[0]["error"])

    def test_empty_checks_fail_and_all_success_passes(self):
        with tempfile.TemporaryDirectory() as directory:
            report = Path(directory) / "report.json"
            self.assertEqual(checker.check_sources("wavec", [], report), 1)
            with patch.object(checker, "run_process", return_value=subprocess.CompletedProcess([], 0, "", "")):
                self.assertEqual(checker.check_sources("wavec", ["valid"], report), 0)

    def test_atomic_report_replaces_previous_complete_json(self):
        with tempfile.TemporaryDirectory() as directory:
            report = Path(directory) / "report.json"
            report.write_text('{"old": true}\n')
            checker._write_report(report, {"phase": "source-check", "results": [{"status": "passed"}]})
            self.assertEqual(json.loads(report.read_text())["results"][0]["status"], "passed")
            self.assertEqual(list(report.parent.glob(f".{report.name}.*.tmp")), [])

    def test_atomic_report_write_failure_preserves_previous_report(self):
        with tempfile.TemporaryDirectory() as directory:
            report = Path(directory) / "report.json"
            report.write_text('{"old": true}\n')
            with patch.object(checker.os, "fdopen", side_effect=OSError("write failed")):
                with self.assertRaisesRegex(OSError, "write failed"):
                    checker._write_report(report, {"new": True})
            self.assertEqual(json.loads(report.read_text()), {"old": True})
            self.assertEqual(list(report.parent.glob(f".{report.name}.*.tmp")), [])

    def test_atomic_report_replace_failure_preserves_previous_report(self):
        with tempfile.TemporaryDirectory() as directory:
            report = Path(directory) / "report.json"
            report.write_text('{"old": true}\n')
            with patch.object(Path, "replace", side_effect=OSError("replace failed")):
                with self.assertRaisesRegex(OSError, "replace failed"):
                    checker._write_report(report, {"new": True})
            self.assertEqual(json.loads(report.read_text()), {"old": True})
            self.assertEqual(list(report.parent.glob(f".{report.name}.*.tmp")), [])

    def test_manifest_failure_leaves_a_failure_report(self):
        with tempfile.TemporaryDirectory() as directory:
            report = Path(directory) / "report.json"
            with patch.object(checker, "load_case_manifest", side_effect=ValueError("invalid manifest")):
                self.assertEqual(checker.main(["--wavec", "wavec", "--report-json", str(report)]), 1)
            self.assertEqual(json.loads(report.read_text())["error"], "invalid manifest")

    def test_missing_compiler_path_fails_early_and_records_error_in_report(self):
        with tempfile.TemporaryDirectory() as directory:
            report = Path(directory) / "report.json"
            missing_wavec = Path(directory) / "nonexistent_wavec_binary"
            with patch.object(checker, "run_process") as mock_run:
                exit_code = checker.main(["--wavec", str(missing_wavec), "--report-json", str(report)])
                self.assertEqual(exit_code, 1)
                mock_run.assert_not_called()
            report_data = json.loads(report.read_text())
            self.assertEqual(report_data["phase"], "source-check")
            self.assertEqual(report_data["results"], [])
            self.assertIn("executable not found", report_data["error"])

    def test_unusable_directory_compiler_path_fails_early_and_records_error(self):
        with tempfile.TemporaryDirectory() as directory:
            report = Path(directory) / "report.json"
            dir_compiler = Path(directory) / "dummy_dir"
            dir_compiler.mkdir()
            with patch.object(checker, "run_process") as mock_run:
                exit_code = checker.main(["--wavec", str(dir_compiler), "--report-json", str(report)])
                self.assertEqual(exit_code, 1)
                mock_run.assert_not_called()
            report_data = json.loads(report.read_text())
            self.assertEqual(report_data["phase"], "source-check")
            self.assertEqual(report_data["results"], [])
            self.assertIn("not a regular file", report_data["error"])

    def test_empty_compiler_path_fails_early(self):
        for empty_arg in ["", "   "]:
            with tempfile.TemporaryDirectory() as directory:
                report = Path(directory) / "report.json"
                with patch.object(checker, "run_process") as mock_run:
                    exit_code = checker.main(["--wavec", empty_arg, "--report-json", str(report)])
                    self.assertEqual(exit_code, 1)
                    mock_run.assert_not_called()
                report_data = json.loads(report.read_text())
                self.assertEqual(report_data["results"], [])
                self.assertIn("compiler path cannot be empty", report_data["error"])

    def test_non_executable_compiler_fails_early(self):
        with tempfile.TemporaryDirectory() as directory:
            report = Path(directory) / "report.json"
            fake_wavec = Path(directory) / "wavec"
            fake_wavec.touch()
            with patch.object(checker, "validate_compiler", side_effect=PermissionError("wavec executable is not launchable: wavec")):
                with patch.object(checker, "run_process") as mock_run:
                    exit_code = checker.main(["--wavec", str(fake_wavec), "--report-json", str(report)])
                    self.assertEqual(exit_code, 1)
                    mock_run.assert_not_called()
            report_data = json.loads(report.read_text())
            self.assertEqual(report_data["results"], [])
            self.assertIn("not launchable", report_data["error"])

    def test_validate_compiler_checks_launchable_on_posix(self):
        with tempfile.TemporaryDirectory() as directory:
            fake_wavec = Path(directory) / "wavec"
            fake_wavec.touch()
            with patch.object(checker.os, "name", "posix"), patch.object(checker.os, "access", return_value=False):
                with self.assertRaises(PermissionError) as cm:
                    checker.validate_compiler(fake_wavec)
                self.assertIn("not launchable", str(cm.exception))

    def test_validate_compiler_checks_launchable_on_windows(self):
        with tempfile.TemporaryDirectory() as directory:
            fake_wavec = Path(directory) / "wavec.txt"
            fake_wavec.touch()
            with patch.object(checker.os, "name", "nt"):
                with self.assertRaises(PermissionError) as cm:
                    checker.validate_compiler(fake_wavec)
                self.assertIn("not launchable", str(cm.exception))

    def test_valid_compiler_path_proceeds_to_check_sources(self):
        with tempfile.TemporaryDirectory() as directory:
            report = Path(directory) / "report.json"
            wavec_name = "wavec.exe" if checker.os.name == "nt" else "wavec"
            valid_wavec = Path(directory) / wavec_name
            valid_wavec.touch()
            if checker.os.name != "nt":
                valid_wavec.chmod(valid_wavec.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
            with patch.object(checker.os, "access", return_value=True):
                with patch.object(checker, "check_sources", return_value=0) as mock_check_sources:
                    with patch.object(checker, "load_case_manifest") as mock_manifest:
                        mock_manifest.return_value.sources.return_value = ["test.wave"]
                        exit_code = checker.main(["--wavec", str(valid_wavec), "--report-json", str(report)])
                        self.assertEqual(exit_code, 0)
                        mock_check_sources.assert_called_once()
                        called_wavec = mock_check_sources.call_args[0][0]
                        self.assertEqual(called_wavec, valid_wavec.resolve())


if __name__ == "__main__":
    unittest.main()
