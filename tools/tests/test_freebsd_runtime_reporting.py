# SPDX-License-Identifier: MPL-2.0
"""Failure-path tests for the FreeBSD CI runner; no VM or network required."""
import argparse
import io
import json
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import Mock, patch

from tools import check_freebsd_sys as runner


class ReportingTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.image = self.root / "image.qcow2"
        self.compiler = self.root / "wavec"
        self.image.touch()
        self.compiler.touch()
        cases = self.root / "tests/cases/freebsd/amd64"
        cases.mkdir(parents=True)
        for name in ["test1.wave", "test2.wave"]:
            (cases / name).write_text("fun main() -> i32 { return 0; }")
        self.args = argparse.Namespace(arch="amd64", image=self.image, compiler=self.compiler,
            firmware=None, kernel=None, out_dir=self.root / "out", clang="clang", linker="ld.lld",
            command_timeout=3, boot_timeout=4, case_timeout=5)

    def execute(self, failures=None, missing=None, boot_error=False):
        report = {"commands": [], "cases": []}
        commands = []
        failures = failures or {}

        class Console:
            def __init__(self, *args):
                pass

            def send(self, *args, **kwargs):
                pass

            def loader_command(self, *args):
                pass

            def expect(self, pattern, timeout=None):
                if boot_error:
                    raise TimeoutError("boot timeout")
                if pattern.startswith("WAVE-RESULT"):
                    name = pattern.split()[1]
                    if name == missing:
                        raise TimeoutError("missing case result")
                    return re.search(pattern, f"WAVE-RESULT {name} {failures.get(name, 0)}\r")
                return re.search(".*", "OK ")

        process = Mock()
        process.wait.return_value = 0
        process.poll.return_value = None
        with patch.object(runner, "ROOT", self.root), patch.object(runner, "Console", Console), \
             patch.object(runner.Commands, "run", lambda _, *args: commands.append(args)), \
             patch.object(runner.subprocess, "Popen", return_value=process):
            try:
                runner.execute(self.args, report)
                error = None
            except Exception as exc:
                error = exc
        return report, commands, process, error

    def test_all_optimization_results_required_and_checkout_std_selected(self):
        report, commands, _, error = self.execute()
        self.assertIsNone(error)
        self.assertEqual([case["name"] for case in report["cases"]],
            ["test1-O0", "test1-O2", "test2-O0", "test2-O2"])
        self.assertTrue(all(case["status"] == "pass" for case in report["cases"]))
        builds = [cmd for cmd in commands if "build" in cmd]
        self.assertEqual(len(builds), 4)
        self.assertTrue(all(cmd[cmd.index("--std-root") + 1] == self.root / "std" for cmd in builds))

    def test_directory_case_builds_main_object_with_logical_result_name(self):
        suite = self.root / "tests/cases/freebsd/amd64"
        (suite / "test2.wave").unlink()
        (suite / "test2").mkdir()
        (suite / "test2/main.wave").write_text("fun main() -> i32 { return 0; }")
        report, commands, _, error = self.execute()
        self.assertIsNone(error)
        self.assertEqual([case["name"] for case in report["cases"]],
            ["test1-O0", "test1-O2", "test2-O0", "test2-O2"])
        builds = [cmd for cmd in commands if "build" in cmd and cmd[2].name == "main.wave"]
        self.assertEqual(len(builds), 2)
        links = [cmd for cmd in commands if cmd[0] == "ld.lld" and any(str(arg).endswith("main.o") for arg in cmd)]
        self.assertEqual(len(links), 2)
        self.assertEqual([Path(cmd[-1]).name for cmd in links], ["test2-O0", "test2-O2"])

    def test_nonzero_result_fails_but_retains_later_results(self):
        report, _, _, error = self.execute(failures={"test1-O2": 17})
        self.assertIsInstance(error, RuntimeError)
        self.assertEqual([c["status"] for c in report["cases"]], ["pass", "fail", "pass", "pass"])
        self.assertEqual(report["cases"][1]["exit_code"], 17)

    def test_missing_result_never_marks_pending_cases_as_pass(self):
        report, _, process, error = self.execute(missing="test1-O2")
        self.assertIsInstance(error, TimeoutError)
        self.assertEqual([c["status"] for c in report["cases"]], ["pass", "missing_result", "not_run", "not_run"])
        process.terminate.assert_called_once()

    def test_boot_failure_preserves_phase_and_stops_guest(self):
        report, _, process, error = self.execute(boot_error=True)
        self.assertIsInstance(error, TimeoutError)
        self.assertEqual(report["phase"], "guest_boot")
        self.assertTrue(all(c["status"] == "not_run" for c in report["cases"]))
        process.terminate.assert_called_once()

    def replay_serial_boot(self, prompt, shell_ready=True):
        # Exercise the real serial parser and boot sequence without a VM.
        chunks = [b"Autoboot in 10 seconds. ", b"OK ",
            *[bytes([ch]) for ch in b"set console=comconsole"], b"\r\nOK ",
            *[bytes([ch]) for ch in b"boot -s"],
            prompt[:12], prompt[12:37], prompt[37:]]
        if shell_ready:
            chunks += [b"root@:/ # ",
                b"WAVE-RESULT test1-O0 0\r\nWAVE-RESULT test1-O2 0\r\n"
                b"WAVE-RESULT test2-O0 0\r\nWAVE-RESULT test2-O2 0\r\n"]
        chunks = iter(chunks)
        process = Mock()
        process.stdin = io.BytesIO()
        process.wait.return_value = 0
        process.poll.return_value = None
        report = {"commands": [], "cases": []}
        with patch.object(runner, "ROOT", self.root), \
             patch.object(runner.Commands, "run"), \
             patch.object(runner.subprocess, "Popen", return_value=process), \
             patch.object(runner.select, "select", return_value=([process.stdout], [], [])), \
             patch.object(runner.os, "read", side_effect=lambda *_: next(chunks, b"")), \
             patch.object(runner.time, "sleep"):
            try:
                runner.execute(self.args, report)
                error = None
            except Exception as exc:
                error = exc
        return report, process, error

    def test_loader_waits_for_echo_before_sending_more_input(self):
        process = Mock()
        process.stdin = io.BytesIO()
        chunks = iter(b"boot -s")
        console = runner.Console(process, io.BytesIO())
        received = bytearray()

        def echo(*_):
            character = bytes([next(chunks)])
            # Only one unacknowledged byte may have been sent, regardless of
            # how long the guest takes to process it.
            self.assertEqual(process.stdin.getvalue(), received + character)
            received.extend(character)
            return character

        with patch.object(runner.select, "select", return_value=([process.stdout], [], [])), \
             patch.object(runner.os, "read", side_effect=echo):
            console.loader_command("boot -s")
        self.assertEqual(process.stdin.getvalue(), b"boot -s\r")

    def test_loader_does_not_submit_a_partially_echoed_command(self):
        process = Mock()
        process.stdin = io.BytesIO()
        log = io.BytesIO()
        log.name = "guest.log"
        console = runner.Console(process, log)
        with patch.object(runner.select, "select", return_value=([process.stdout], [], [])), \
             patch.object(runner.os, "read", side_effect=[b"b", b"o", b""]):
            with self.assertRaises(RuntimeError):
                console.loader_command("boot -s")
        self.assertEqual(process.stdin.getvalue(), b"boo")

    def test_serial_prompt_accepts_interleaved_device_output(self):
        # Captured from PR #802's FreeBSD job 108548040634: cd0 output
        # split the shell path and postponed the final colon to another line.
        interleaved = (
            b"Enter full pathname of shell or RETURN for c/bin/shd0 at ata1 bus 0 scbus1 target 0 lun 0\r\n"
            b"cd0: <QEMU QEMU DVD-ROM 2.5+> Removable CD-ROM SCSI device\r\n"
            b"cd0: Attempt to query device size failed: NOT READY, Medium not present\r\n: ")
        for prompt in [b"Enter full pathname of shell or RETURN for /bin/sh: ", interleaved]:
            with self.subTest(prompt=prompt):
                report, process, error = self.replay_serial_boot(prompt)
                self.assertIsNone(error)
                self.assertTrue(all(c["status"] == "pass" for c in report["cases"]))
                self.assertIn(b"boot -s\r\rmount -uw /\n", process.stdin.getvalue())
                self.assertIn(prompt, (Path(report["work_dir"]) / "guest.log").read_bytes())

    def test_serial_prompt_still_requires_root_shell_before_cases(self):
        report, process, error = self.replay_serial_boot(
            b"Enter full pathname of shell or RETURN for /bin/sh: ", shell_ready=False)
        self.assertIsInstance(error, RuntimeError)
        self.assertEqual(report["phase"], "guest_boot")
        self.assertTrue(all(c["status"] == "not_run" for c in report["cases"]))
        self.assertNotIn(b"sh /mnt/run.sh", process.stdin.getvalue())
        process.terminate.assert_called_once()

    def test_command_failure_and_timeout_are_recorded(self):
        report = {"commands": []}
        command = runner.Commands(report, self.root / "build.log", 3)
        with self.assertRaises(RuntimeError):
            command.run(sys.executable, "-c", "print('failure evidence'); raise SystemExit(7)")
        self.assertEqual(report["commands"][-1]["exit_code"], 7)
        self.assertIn("failure evidence", (self.root / "build.log").read_text())
        with patch.object(runner.subprocess, "run", side_effect=subprocess.TimeoutExpired("clang", 3)):
            with self.assertRaises(subprocess.TimeoutExpired):
                command.run("clang")
        self.assertEqual(report["commands"][-1]["status"], "timeout")

    def test_missing_image_still_writes_machine_readable_failure(self):
        result = self.root / "report.json"
        with patch.object(sys, "argv", ["runner", "--arch", "amd64", "--image", str(self.root / "missing"),
                "--compiler", str(self.compiler), "--report-json", str(result)]):
            self.assertEqual(runner.main(), 1)
        report = json.loads(result.read_text())
        self.assertEqual(report["status"], "fail")
        self.assertEqual(report["phase"], "validation")
        self.assertIn("File does not exist", report["error"])


    def call_main(self, report, *extra):
        argv = ["runner", "--arch", "amd64", "--image", str(self.image),
                "--compiler", str(self.compiler), "--out-dir", str(self.args.out_dir),
                "--report-json", str(report), *map(str, extra)]
        with patch.object(sys, "argv", argv), patch.object(runner, "ROOT", self.root):
            return runner.main()

    def test_reports_cannot_alias_inputs_even_when_validation_would_fail(self):
        paths = [self.image, self.compiler, self.root / "firmware", self.root / "kernel",
                 self.root / "clang", self.root / "linker",
                 self.root / "tests/cases/freebsd/amd64/test1.wave",
                 self.root / "tests/cases/freebsd/amd64/test3/helper.wave",
                 self.root / "tests/fixtures/freebsd_case_runtime/start.c",
                 self.root / "std/manifest.json", self.root / "std/io/fd.wave"]
        extra = ["--firmware", paths[2], "--kernel", paths[3], "--clang", paths[4], "--linker", paths[5]]
        for index, source in enumerate(paths):
            source.parent.mkdir(parents=True, exist_ok=True)
            sentinel = f"input {index}".encode()
            source.write_bytes(sentinel)
            for alias_kind in ("direct", "dot", "symlink", "hardlink"):
                with self.subTest(source=source.name, alias=alias_kind):
                    alias = source
                    if alias_kind == "dot":
                        (source.parent / "empty").mkdir(exist_ok=True)
                        alias = source.parent / "empty/.." / source.name
                    elif alias_kind in ("symlink", "hardlink"):
                        alias = self.root / f"{index}-{alias_kind}"
                        try:
                            if alias_kind == "symlink":
                                alias.symlink_to(source)
                            else:
                                alias.hardlink_to(source)
                        except OSError:
                            continue  # Some Windows runners do not permit links.
                    with patch.object(runner, "execute", side_effect=ValueError("preflight failed")) as execute:
                        self.assertEqual(self.call_main(alias, *extra), 1)
                        execute.assert_not_called()
                    self.assertEqual(source.read_bytes(), sentinel)
                    self.assertEqual(alias.read_bytes(), sentinel)

    def test_safe_reports_survive_failure_and_interruption(self):
        report = self.root / "reports/result.json"
        for error, expected in [(ValueError("preflight failed"), "fail"),
                                (KeyboardInterrupt(), "interrupted")]:
            with self.subTest(expected=expected), patch.object(runner, "execute", side_effect=error):
                self.assertEqual(self.call_main(report), 1)
            self.assertEqual(json.loads(report.read_text())["status"], expected)
            self.assertEqual(list(report.parent.glob("*.tmp")), [])

    def test_report_write_failure_returns_failure(self):
        report = self.root / "report-directory"
        report.mkdir()
        with patch.object(runner, "execute"):
            self.assertEqual(self.call_main(report), 1)


if __name__ == "__main__":
    unittest.main()
