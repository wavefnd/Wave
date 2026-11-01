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

    def test_zero_length_tcp_reads_preserve_connection_and_queued_data(self):
        target = os.environ["WAVE_TEST_TARGET"]
        with tempfile.TemporaryDirectory(prefix="wave-tcp-zero-") as temp:
            for opt in ["-O0", "-O2"]:
                binary = Path(temp) / "tcp.exe"
                result = run_process([COMPILER, "--std-root", str(ROOT / "std"), "build",
                    str(ROOT / "tests/fixtures/stabilization_17/tcp_zero.wave"), "--target", target,
                    opt, "-o", str(binary)], timeout=60, capture_output=True, text=True)
                self.assertEqual(result.returncode, 0, f"{opt}: {result.stdout} {result.stderr}")
                result = run_process([str(binary)], timeout=10, capture_output=True, text=True)
                self.assertEqual(result.returncode, 0, f"{target} {opt}: {result.stdout} {result.stderr}")

    @unittest.skipIf(os.name == "nt", "POSIX terminal ioctl providers")
    def test_invalid_terminal_actions_preserve_pty_settings(self):
        import pty
        import termios
        master, slave = pty.openpty()
        try:
            initial = termios.tcgetattr(slave)
            with tempfile.TemporaryDirectory(prefix="wave-tty-action-") as temp:
                source = Path(temp) / "tty.wave"
                source.write_text(f'''import("std::sys::tty")::{{Termios, tty_getattr, tty_setattr, TTY_ECHO, TTY_TCSANOW, TTY_TCSADRAIN, TTY_TCSAFLUSH}};
fun main() -> i32 {{
    var term: Termios;
    if (tty_getattr({slave}, &term) != 0) {{ return 1; }}
    term.c_lflag = term.c_lflag ^ TTY_ECHO;
    if (tty_setattr({slave}, -1, &term) != -22 || tty_setattr({slave}, 99, &term) != -22) {{ return 2; }}
    var status: i64 = tty_getattr({slave}, &term);
    if (status != 0) {{ println("second tty_getattr: {{}}", status); return 3; }}
    if (tty_setattr({slave}, TTY_TCSANOW, &term) != 0 || tty_setattr({slave}, TTY_TCSADRAIN, &term) != 0 || tty_setattr({slave}, TTY_TCSAFLUSH, &term) != 0) {{ return 4; }}
    return 0;
}}''')
                for opt in ["-O0", "-O2"]:
                    binary = Path(temp) / "tty"
                    result = run_process([COMPILER, "--std-root", str(ROOT / "std"), "build", str(source),
                        "--target", os.environ["WAVE_TEST_TARGET"], opt, "-o", str(binary)], timeout=60, capture_output=True, text=True)
                    self.assertEqual(result.returncode, 0, f"{opt}: {result.stdout} {result.stderr}")
                    result = run_process([str(binary)], pass_fds=(slave,), timeout=10, capture_output=True, text=True)
                    self.assertEqual(result.returncode, 0, f"{opt}: {result.stdout} {result.stderr}")
                    self.assertEqual(termios.tcgetattr(slave), initial)
        finally:
            os.close(slave)
            os.close(master)
