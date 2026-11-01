# SPDX-License-Identifier: MPL-2.0
"""Runtime reports distinguish execution from compilation without real SDKs."""
import argparse
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

from tools import run_runtime_cases as runner
from tools.process_tree import run_process


class RuntimeReportTests(unittest.TestCase):
    def test_reports_do_not_clobber_std_or_unselected_imports_before_selection(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for name in ("std/mem/layout.wave", "tests/cases/helper.wave", "tests/cases/cases.toml"):
                source = root / name
                source.parent.mkdir(parents=True, exist_ok=True)
                source.write_text("INPUT SENTINEL")
                for kind in ("direct", "normalized", "hardlink", "symlink"):
                    with self.subTest(input=name, alias=kind):
                        report = root / f"{source.name}-{kind}"
                        if kind == "direct":
                            report = source
                        elif kind == "normalized":
                            report = source.parent / ".." / source.parent.name / source.name
                        elif kind == "hardlink":
                            os.link(source, report)
                        else:
                            try:
                                report.symlink_to(source)
                            except OSError:
                                continue
                        options = SimpleNamespace(
                            wavec=Path(sys.executable), report_json=report,
                            sources=["main.wave"], target_id="not-a-target",
                        )
                        with patch.object(runner, "ROOT", root), \
                             patch.object(runner, "load_case_manifest", side_effect=ValueError("invalid manifest")) as manifest, \
                             patch.object(runner, "run_process") as run:
                            self.assertEqual(runner.execute(options), 2)
                            manifest.assert_not_called()
                            run.assert_not_called()
                        self.assertEqual(source.read_text(), "INPUT SENTINEL")

    def run_suite(self, executor="wasm", outcomes=(), missing_output=False, sources=None, alias_root=False, metadata="", plan_text=None):
        calls = []
        self.process_inputs = []
        with tempfile.TemporaryDirectory(prefix="runtime root ") as directory:
            root = Path(directory)
            if alias_root:
                real = root / "real checkout"
                real.mkdir()
                root = root / "alias checkout"
                try:
                    root.symlink_to(real, target_is_directory=True)
                except OSError as error:
                    self.skipTest(f"directory symlinks unavailable: {error}")
            selected = ["shared/test1.wave", "shared/test2/main.wave"]
            for name in selected:
                path = root / "tests/cases" / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text((f"// wave-test: {metadata}\n" if metadata else "") + "fun main() {}")
            options = argparse.Namespace(
                wavec=Path(sys.executable), target_id="test-target", sources=selected if sources is None else sources,
                report_json=root / "report.json", qemu="fake-qemu", sysroot=root,
                build_timeout=3, timeout=3)
            target = SimpleNamespace(enabled=True, executor=executor, target="test-triple", suites=("shared",))

            def process(command, **kwargs):
                if "--std-root" in command:
                    selected_std = Path(command[command.index("--std-root") + 1])
                    self.assertEqual(selected_std.resolve(), (root / "std").resolve())
                    self.assertTrue(Path(command[2]).is_relative_to(root.resolve()))
                index = len(calls)
                calls.append(command)
                self.process_inputs.append(kwargs.get("input"))
                outcome = outcomes[index] if index < len(outcomes) else 0
                if isinstance(outcome, BaseException):
                    raise outcome
                if "-o" in command and "--dry-run" not in command and outcome == 0 and not missing_output:
                    Path(command[command.index("-o") + 1]).touch()
                if "--dry-run" in command and outcome == 0:
                    output = command[command.index("-o") + 1]
                    payload = json.dumps({"link": {"output": output}, "execute": {
                        "program": "fake-node", "args": [output]}}) if plan_text is None else plan_text
                    return subprocess.CompletedProcess(command, 0, payload, "")
                # A real short-lived subprocess supplies stdout, stderr, and exit status.
                return run_process([sys.executable, "-c",
                    "import sys; print('runtime output'); "
                    "print('env.printf unavailable ' + 'x' * 5000, file=sys.stderr); "
                    f"sys.exit({outcome})"], **kwargs)

            with patch.object(runner, "ROOT", root), \
                 patch.object(runner, "load_case_manifest", return_value=SimpleNamespace(target=lambda _: target)), \
                 patch.object(runner, "run_process", side_effect=process), \
                 contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
                status = runner.execute(options)
            return status, json.loads(options.report_json.read_text()), calls

    def test_wasm_success_is_explicitly_runtime_and_preserves_selection(self):
        status, report, calls = self.run_suite()
        self.assertEqual(status, 0)
        self.assertEqual(report["phase"], "runtime")
        self.assertEqual(report["selection"], {
            "mode": "target", "id": "test-target", "target": "test-triple",
            "executor": "wasm", "suites": ["shared"]})
        self.assertEqual(report["summary"]["pass"], 2)
        self.assertEqual([c["phase"] for c in report["tests"][0]["commands"]], ["build", "plan", "run"])
        self.assertEqual(calls[0][1], "build")
        self.assertIn("--dry-run", calls[1])
        self.assertEqual(calls[1][1], "build")
        self.assertIn("--run", calls[1])
        self.assertEqual(calls[2][0], "fake-node")

    def test_compiler_commands_use_same_checkout_through_directory_alias(self):
        for executor in ("wasm", "qemu"):
            with self.subTest(executor=executor):
                status, _, calls = self.run_suite(executor=executor, alias_root=True)
                self.assertEqual(status, 0)
                self.assertEqual(sum("--std-root" in command for command in calls), 4 if executor == "wasm" else 2)

    def test_host_import_failure_is_reported_and_later_case_still_runs(self):
        status, report, _ = self.run_suite(outcomes=[0, 0, 7])
        self.assertEqual(status, 1)
        self.assertEqual([row["status"] for row in report["tests"]], ["fail", "pass"])
        command = report["tests"][0]["commands"][-1]
        self.assertEqual(command["actual_exit"], 7)
        self.assertEqual(command["expected_exit"], 0)
        self.assertIn("env.printf", command["stderr"])
        self.assertEqual(len(command["stderr"]), 4096)
        self.assertTrue(command["stderr_truncated"])

    def test_qemu_build_and_execution_have_separate_status_and_use_exact_output(self):
        status, report, calls = self.run_suite(executor="qemu")
        self.assertEqual(status, 0)
        for row, build, run in zip(report["tests"], calls[::2], calls[1::2]):
            self.assertEqual([cmd["phase"] for cmd in row["commands"]], ["build", "run"])
            self.assertEqual(build[build.index("-o") + 1], run[-1])
            self.assertEqual(Path(build[build.index("--std-root") + 1]).name, "std")
            self.assertIn("runtime root ", build[build.index("--std-root") + 1])
            self.assertEqual(run[0], "fake-qemu")
            self.assertEqual(Path(build[build.index("--sysroot") + 1]), Path(run[2]).resolve())
        self.assertTrue(any(arg.endswith("main.wave") for arg in calls[2]))

    def test_qemu_compile_failure_or_missing_artifact_never_counts_as_execution(self):
        for missing, outcomes in [(False, [7]), (True, [])]:
            with self.subTest(missing=missing):
                status, report, _ = self.run_suite(executor="qemu", outcomes=outcomes, missing_output=missing)
                self.assertEqual(status, 1)
                first = report["tests"][0]
                self.assertEqual(first["status"], "fail")
                self.assertEqual(len(first["commands"]), 1)
                self.assertEqual(first["commands"][0]["phase"], "build")

    def test_successful_qemu_build_cannot_hide_program_failure(self):
        status, report, _ = self.run_suite(executor="qemu", outcomes=[0, 7])
        self.assertEqual(status, 1)
        self.assertEqual(report["tests"][0]["status"], "fail")
        build, run = report["tests"][0]["commands"]
        self.assertEqual(build["actual_exit"], 0)
        self.assertEqual(run["actual_exit"], 7)
        self.assertEqual(run["phase"], "run")
        self.assertEqual(report["tests"][1]["status"], "pass")

    def test_timeout_and_launch_failure_preserve_reason_without_passing(self):
        for error, expected in [(subprocess.TimeoutExpired("host", 6, output=b"partial"), "timeout"),
                                (OSError("host unavailable"), "fail")]:
            with self.subTest(error=error):
                status, report, _ = self.run_suite(outcomes=[0, 0, error])
                self.assertEqual(status, 1)
                self.assertEqual(report["tests"][0]["status"], expected)
                self.assertEqual(report["tests"][1]["status"], "pass")
                self.assertIn("reason", report["tests"][0])
                if expected == "timeout":
                    self.assertEqual(report["tests"][0]["commands"][-1]["output"], "partial")

    def test_interruption_keeps_pending_cases_not_run(self):
        status, report, calls = self.run_suite(outcomes=[KeyboardInterrupt()])
        self.assertEqual(status, 130)
        self.assertEqual([row["status"] for row in report["tests"]], ["interrupted", "not_run"])
        self.assertEqual(report["summary"]["pass"], 0)
        self.assertEqual(len(calls), 1)

    def test_empty_unsafe_or_duplicate_selection_fails_before_execution(self):
        for sources in ([], ["../outside.wave"], ["shared/test1.wave"] * 2):
            with self.subTest(sources=sources):
                status, report, calls = self.run_suite(sources=sources)
                self.assertEqual(status, 2)
                self.assertIn("error", report)
                self.assertFalse(calls)

    def test_metadata_input_and_nonzero_exit_apply_only_to_execution(self):
        for executor, outcomes in (("qemu", [0, 7, 0, 7]), ("wasm", [0, 0, 7, 0, 0, 7])):
            with self.subTest(executor=executor):
                status, report, _ = self.run_suite(executor=executor, outcomes=outcomes,
                                                  metadata="stdin=3, expected-exit=7")
                self.assertEqual(status, 0)
                records = [c for row in report["tests"] for c in row["commands"]]
                for record, stdin in zip(records, self.process_inputs):
                    self.assertEqual(record["expected_exit"], 7 if record["phase"] == "run" else 0)
                    self.assertEqual(stdin, "3\n" if record["phase"] == "run" else None)

    def test_build_failure_matching_expected_program_exit_still_fails(self):
        for executor in ("qemu", "wasm"):
            with self.subTest(executor=executor):
                status, report, _ = self.run_suite(executor=executor, outcomes=[7], metadata="expected-exit=7")
                self.assertEqual(status, 1)
                row = report["tests"][0]
                self.assertEqual(row["status"], "fail")
                self.assertEqual([c["phase"] for c in row["commands"]], ["build"])
                self.assertEqual(row["commands"][0]["expected_exit"], 0)

    def test_invalid_or_unsupported_metadata_fails_before_any_command(self):
        for metadata in ("mode=build, runner=compile", "runner=server", "udp-input=true", "unknown=value"):
            with self.subTest(metadata=metadata):
                status, report, calls = self.run_suite(metadata=metadata)
                self.assertEqual(status, 2)
                self.assertIn("error", report)
                self.assertFalse(calls)

    def test_wasm_missing_output_or_failed_plan_never_counts_as_execution(self):
        for options in ({"missing_output": True}, {"outcomes": [0, 7]}, {"plan_text": "not json"},
                        {"plan_text": "{}"}):
            with self.subTest(options=options):
                status, report, _ = self.run_suite(**options)
                self.assertEqual(status, 1)
                self.assertNotIn("run", [c["phase"] for c in report["tests"][0]["commands"]])

    def test_wasm_plan_rejects_different_output_and_invalid_command_shapes(self):
        output = Path("/temporary/case.wasm")
        for plan in ({"link": {"output": "other"}},
                     {"link": {"output": str(output)}, "execute": {"program": "node", "args": [17]}},
                     {"link": {"output": str(output)}, "execute": {"program": "node", "args": ["other"]}}):
            with self.subTest(plan=plan), self.assertRaises(ValueError):
                runner.wasm_execute_plan(json.dumps(plan), output)

    def test_timeout_rejects_unbounded_values(self):
        for value in ("nan", "inf", "0", "-1"):
            with self.subTest(value=value), self.assertRaises(argparse.ArgumentTypeError):
                runner.positive_seconds(value)


if __name__ == "__main__":
    unittest.main()
