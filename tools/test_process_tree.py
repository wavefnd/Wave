# This file is part of the Wave language project.
# Copyright (c) 2024–2026 Wave Foundation
# Copyright (c) 2024–2026 LunaStev and contributors
#
# This Source Code Form is subject to the terms of the
# Mozilla Public License, v. 2.0.
# If a copy of the MPL was not distributed with this file,
# You can obtain one at https://mozilla.org/MPL/2.0/.
#
# SPDX-License-Identifier: MPL-2.0
# AI TRAINING NOTICE: Prohibited without prior written permission. No use for machine learning or generative AI training, fine-tuning, distillation, embedding, or dataset creation.

import contextlib
import io
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
import unittest
from unittest.mock import MagicMock, patch

from tools import check_wave_corpus, run_tests
from tools.process_tree import ProcessTree, run_process
from tools.test_contracts import TestMetadata


class ProcessTreeTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="wave-process-tree-test-")
        self.root = Path(self.temporary.name)
        self.ready = self.root / "ready"
        self.release = self.root / "release"
        self.marker = self.root / "escaped"
        child = self.root / "child.py"
        child.write_text(
            "from pathlib import Path\nimport time\n"
            f"Path({str(self.ready)!r}).touch()\n"
            "deadline = time.monotonic() + 15\n"
            f"while not Path({str(self.release)!r}).exists():\n"
            "    if time.monotonic() > deadline: raise SystemExit(0)\n"
            "    time.sleep(0.01)\n"
            f"Path({str(self.marker)!r}).write_text('survived')\n"
        )
        self.compiler = self.root / "compiler.py"
        self.compiler.write_text(
            "from pathlib import Path\nimport subprocess,sys,time\n"
            f"subprocess.Popen([sys.executable, {str(child)!r}], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)\n"
            f"while not Path({str(self.ready)!r}).exists(): time.sleep(0.01)\n"
            "print('child ready', flush=True)\n"
            "print('compiler diagnostic', file=sys.stderr, flush=True)\n"
            "if '--exit-parent' not in sys.argv: time.sleep(15)\n"
        )
        self.command = [sys.executable, str(self.compiler)]

    def tearDown(self):
        self.temporary.cleanup()

    def assert_descendant_stopped(self):
        self.assertTrue(self.ready.exists(), "reproducer never launched its child")
        self.release.touch()
        time.sleep(0.4)
        self.assertFalse(self.marker.exists(), "child performed a side effect after cleanup")

    @unittest.skipUnless(os.name == "posix", "POSIX process-group ownership")
    def test_successful_timeout_cleanup_never_signals_the_reaped_group_again(self):
        process = MagicMock()
        process.pid = 424242
        process.communicate.side_effect = [
            subprocess.TimeoutExpired(["fixture"], 2),
            ("partial stdout", "partial stderr"),
            ("partial stdout", "partial stderr"),
        ]
        with patch("tools.process_tree.subprocess.Popen", return_value=process), \
             patch("tools.process_tree.os.killpg", side_effect=[None, PermissionError(1, "stale group")]) as kill:
            with self.assertRaises(subprocess.TimeoutExpired) as result:
                run_process(["fixture"], timeout=2, capture_output=True, text=True)
        self.assertEqual(result.exception.output, "partial stdout")
        self.assertEqual(result.exception.stderr, "partial stderr")
        kill.assert_called_once()

    @unittest.skipUnless(os.name == "posix", "POSIX process-group ownership")
    def test_failed_termination_is_not_marked_complete_or_silently_ignored(self):
        process = MagicMock()
        process.pid = 424242
        process.communicate.return_value = ("", "")
        with patch("tools.process_tree.subprocess.Popen", return_value=process), \
             patch("tools.process_tree.os.killpg", side_effect=[PermissionError(1, "denied"), None]) as kill:
            tree = ProcessTree(["fixture"])
            with self.assertRaises(PermissionError):
                tree.terminate()
            self.assertFalse(tree.terminated)
            tree.close()
            tree.close()
        self.assertTrue(tree.terminated)
        self.assertTrue(tree.closed)
        self.assertEqual(kill.call_count, 2)

    def test_timeout_stops_descendants_and_preserves_diagnostics(self):
        with self.assertRaises(subprocess.TimeoutExpired) as result:
            run_process(self.command, capture_output=True, text=True, timeout=2)
        self.assertIn("child ready", result.exception.output)
        self.assertIn("compiler diagnostic", result.exception.stderr)
        self.assert_descendant_stopped()

    def test_cleanup_still_owns_children_after_the_compiler_exits(self):
        result = run_process(self.command + ["--exit-parent"], capture_output=True, text=True, timeout=3)
        self.assertEqual(result.returncode, 0)
        self.assert_descendant_stopped()

    def test_keyboard_interrupt_cleans_up_without_touching_unrelated_processes(self):
        unrelated_marker = self.root / "unrelated"
        other = subprocess.Popen([sys.executable, "-c",
            f"import pathlib,time; time.sleep(1); pathlib.Path({str(unrelated_marker)!r}).touch()"])
        try:
            with self.assertRaises(KeyboardInterrupt):
                with ProcessTree(self.command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True) as tree:
                    self.assertEqual(tree.process.stdout.readline().strip(), "child ready")
                    raise KeyboardInterrupt()
            self.assert_descendant_stopped()
            self.assertEqual(other.wait(timeout=5), 0)
            self.assertTrue(unrelated_marker.is_file())
        finally:
            if other.poll() is None:
                other.kill()
            other.wait()

    def test_std_example_runner_cleans_the_tree_and_reports_captured_output(self):
        (self.root / "examples/std").mkdir(parents=True)
        (self.root / "examples/std/case.wave").write_text("fun main() {}")
        def python_compiler(args, **kwargs):
            return run_process([sys.executable, *args], **kwargs)
        with patch.object(check_wave_corpus, "ROOT", self.root), \
             patch.object(check_wave_corpus, "run_process", side_effect=python_compiler), \
             contextlib.redirect_stdout(io.StringIO()):
            failures = check_wave_corpus.run_std_examples(self.compiler, dict(os.environ), 2)
        self.assertEqual(len(failures), 1)
        self.assertIn("timed out", failures[0][1])
        self.assertIn("compiler diagnostic", failures[0][1])
        self.assert_descendant_stopped()

    def test_case_runner_keeps_timeout_classification_and_cleans_the_tree(self):
        output = io.StringIO()
        with patch.object(run_tests, "ROOT", self.root), \
             patch.object(run_tests, "TIMEOUT_SEC", 2), \
             patch.object(run_tests, "parse_test_metadata", return_value=TestMetadata()), \
             patch.object(run_tests, "manifest_compile_target", return_value=None), \
             contextlib.redirect_stdout(output):
            status, detail = run_tests.classify_program(
                "case", "case.wave", self.command, TestMetadata(), None
            )
        self.assertEqual(status, -1, f"{detail}\n{output.getvalue()}")
        self.assertIn("timed out", detail["reason"])
        self.assertEqual(detail["timeout_seconds"], 2)
        self.assertIn("compiler diagnostic", detail["stderr"])
        self.assertIn("compiler diagnostic", output.getvalue())
        self.assert_descendant_stopped()

    def test_server_cleanup_terminates_generated_server(self):
        class Socket:
            def settimeout(self, _): pass
            def connect(self, _): pass
            def sendall(self, _): pass
            def recv(self, _): return b"Welcome to the Wave HTTP Server!"
            def close(self): pass
        with patch.object(run_tests.socket, "socket", return_value=Socket()), \
             contextlib.redirect_stdout(io.StringIO()):
            status, _ = run_tests.run_server_test(self.command)
        self.assertEqual(status, 1)
        self.assert_descendant_stopped()

    def test_text_mode_replaces_invalid_utf8_on_both_streams(self):
        for stream in ("stdout", "stderr"):
            with self.subTest(stream=stream):
                result = run_process(
                    [sys.executable, "-c",
                     f"import sys; sys.{stream}.buffer.write(bytes([255]))"],
                    capture_output=True, text=True, timeout=3,
                )
                self.assertEqual(result.returncode, 0)
                decoded = result.stdout if stream == "stdout" else result.stderr
                self.assertEqual(decoded, "\ufffd")

    def test_text_mode_preserves_ordinary_utf8(self):
        result = run_process(
            [sys.executable, "-c", "import sys; print('caf\u00e9', end='')"],
            capture_output=True, text=True, timeout=3,
        )
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "caf\u00e9")

    def test_input_and_nonzero_status_are_preserved(self):
        result = run_process([sys.executable, "-c",
            "import sys; print(sys.stdin.read()); sys.exit(7)"],
            input="hello", capture_output=True, text=True, timeout=3)
        self.assertEqual(result.returncode, 7)
        self.assertEqual(result.stdout.strip(), "hello")

    @unittest.skipUnless(os.name == "nt", "native Windows exit-code contract")
    def test_windows_crash_exit_code_is_not_truncated_by_supervisor(self):
        result = run_process([sys.executable, "-c", "import os; os._exit(-1073741819)"], timeout=3)
        self.assertEqual(result.returncode, 0xC0000005)


if __name__ == "__main__":
    unittest.main()
