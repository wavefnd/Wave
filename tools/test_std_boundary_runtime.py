# SPDX-License-Identifier: MPL-2.0
"""Run std path and readiness boundaries with an external process deadline."""
import os
from pathlib import Path
import tempfile
import unittest

from tools.process_tree import run_process

ROOT = Path(__file__).resolve().parents[1]
COMPILER = os.environ.get("WAVE_TEST_COMPILER")


@unittest.skipUnless(COMPILER, "Cargo supplies WAVE_TEST_COMPILER")
class StandardBoundaryTests(unittest.TestCase):
    def run_case(self, name):
        target = os.environ["WAVE_TEST_TARGET"]
        with tempfile.TemporaryDirectory(prefix="wave-std-boundaries-") as temp:
            for opt in ["-O0", "-O2"]:
                binary = Path(temp) / ("case.exe" if os.name == "nt" else "case")
                command = [COMPILER, "--std-root", str(ROOT / "std"), "build",
                    str(ROOT / f"tests/fixtures/std_boundaries/{name}.wave"),
                    "--target", target, opt, "-o", str(binary)]
                result = run_process(command, timeout=60, capture_output=True, text=True)
                self.assertEqual(result.returncode, 0, f"{target} {opt} {name}: {result.stdout}\n{result.stderr}")
                # Infinite poll on a negative descriptor must fail the test
                # promptly even when it blocks inside the OS.
                result = run_process([str(binary)], timeout=10, capture_output=True, text=True)
                self.assertEqual(result.returncode, 0, f"{target} {opt} {name}: {result.stdout}\n{result.stderr}")

    def test_path_destinations_and_dot_components(self):
        self.run_case("paths")

    @unittest.skipIf(os.name == "nt", "Windows socket handles are not POSIX descriptors")
    def test_single_descriptor_waits_and_disabled_poll_entries(self):
        self.run_case("poll")
