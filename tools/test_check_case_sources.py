import json
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


if __name__ == "__main__":
    unittest.main()
