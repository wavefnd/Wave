# SPDX-License-Identifier: MPL-2.0
"""Descriptor representability and actual native child wait-status regressions."""
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import unittest

from tools.process_tree import run_process

ROOT = Path(__file__).resolve().parents[1]
COMPILER = os.environ.get("WAVE_TEST_COMPILER")


@unittest.skipUnless(COMPILER and os.name != "nt", "native POSIX fixture")
class NativeProcessBoundaries(unittest.TestCase):
    def run_source(self, source, descriptors=()):
        with tempfile.TemporaryDirectory(prefix="wave-process-boundary-") as temp:
            path = Path(temp) / "main.wave"
            path.write_text(source, encoding="utf-8")
            for opt in ["-O0", "-O2"]:
                binary = Path(temp) / "main"
                built = run_process([COMPILER, "--std-root", str(ROOT / "std"),
                    "build", str(path), "--target", os.environ["WAVE_TEST_TARGET"],
                    opt, "-o", str(binary)], timeout=60, capture_output=True, text=True)
                self.assertEqual(built.returncode, 0, f"{built.stdout}\n{built.stderr}")
                result = run_process([str(binary)], pass_fds=descriptors,
                    timeout=10, capture_output=True, text=True)
                self.assertEqual(result.returncode, 0, f"{opt}: {result.stdout}\n{result.stderr}")

    def test_wide_descriptors_cannot_read_write_close_seek_or_duplicate_aliases(self):
        read_fd, write_fd = os.pipe()
        try:
            os.write(write_fd, b"ZZ")
            source = '''import("std::io::fd")::{io_read, io_write, io_close, io_seek, io_dup, io_dup2, io_write_all};
fun main() -> i32 {
    var octet: u8 = 65;
    var descriptors: array<i64, 6> = [BAD_READ, BAD_WRITE, NEGATIVE_ALIAS, 2147483647, 2147483648, -2147483649];
    var i: i32 = 0;
    while (i < 6) {
        var fd: i64 = descriptors[i];
        if (io_read(fd, &octet, 1) != -9 || octet != 65) { return 1; }
        if (io_write(fd, &octet, 1) != -9) { return 2; }
        if (io_seek(fd, 0, 0) != -9 || io_close(fd) != -9 || io_dup(fd) != -9) { return 3; }
        if (io_dup2(fd, WRITE_FD) != -9 || io_dup2(READ_FD, fd) != -9) { return 4; }
        if (io_write_all(fd, &octet, 0) != 0) { return 5; }
        i += 1;
    }
    if (io_read(READ_FD, &octet, 1) != 1 || octet != 90) { return 6; }
    octet = 65;
    if (io_write(WRITE_FD, &octet, 1) != 1) { return 7; }
    return 0;
}'''
            for key, value in {"BAD_READ": 2**32 + read_fd, "BAD_WRITE": 2**32 + write_fd,
                "NEGATIVE_ALIAS": -2**32 + read_fd, "READ_FD": read_fd, "WRITE_FD": write_fd}.items():
                source = source.replace(key, str(value))
            self.run_source(source, (read_fd, write_fd))
            os.close(write_fd)
            write_fd = None
            self.assertEqual(os.read(read_fd, 32), b"AA")
        finally:
            os.close(read_fd)
            if write_fd is not None:
                os.close(write_fd)

    def test_actual_waitpid_stop_and_continue_words_match_portable_predicates(self):
        child = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(30)"])
        try:
            os.kill(child.pid, signal.SIGSTOP)
            _, stopped = os.waitpid(child.pid, os.WUNTRACED)
            os.kill(child.pid, signal.SIGCONT)
            _, continued = os.waitpid(child.pid, os.WCONTINUED)
            self.assertTrue(os.WIFSTOPPED(stopped))
            self.assertTrue(os.WIFCONTINUED(continued))
        finally:
            child.terminate()
            child.wait(timeout=5)
        source = '''import("std::process::wait")::{proc_status_continued, proc_status_stopped, proc_status_signaled, proc_status_exited, proc_status_exit_code, proc_status_term_signal};
fun main() -> i32 {
    if (!proc_status_continued(CONTINUED) || proc_status_stopped(CONTINUED) || proc_status_signaled(CONTINUED)) { return 1; }
    if (!proc_status_stopped(STOPPED) || proc_status_continued(STOPPED) || proc_status_signaled(STOPPED)) { return 2; }
    if (!proc_status_exited(1792) || proc_status_exit_code(1792) != 7) { return 3; }
    if (!proc_status_signaled(9) || proc_status_term_signal(9) != 9) { return 4; }
    return 0;
}'''.replace("CONTINUED", str(continued)).replace("STOPPED", str(stopped))
        self.run_source(source)
