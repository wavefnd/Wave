"""Native standard-library regressions, invoked by Cargo with its built wavec."""
import os
import errno
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import threading
import time
import unittest
from unittest.mock import patch

from tools.process_tree import run_process, timeout_output

ROOT = Path(__file__).resolve().parent.parent
COMPILER = os.environ.get("WAVE_TEST_COMPILER")


@unittest.skipUnless(COMPILER, "Cargo supplies WAVE_TEST_COMPILER for native fixtures")
class StandardIoRuntimeTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.workspace = tempfile.TemporaryDirectory(prefix="wave-std-io-")
        cls.addClassCleanup(cls.workspace.cleanup)
        cls.base = Path(cls.workspace.name)
        home = cls.base / "home"
        shutil.copytree(ROOT / "std", home / ".wave/lib/wave/std")
        cls.env = dict(os.environ, HOME=str(home), USERPROFILE=str(home))
        cls.target = os.environ.get("WAVE_TEST_TARGET")
        cls.executables = {}
        names = ["copy", "copy_self", "read_all", "read_zero", "tcp_timeout", "tcp_read", "invalid_buffers"]
        if os.name == "posix":
            names += ["capture", "capture_close", "fork_status"]
        if sys.platform.startswith("linux"):
            names += ["capture_failures", "capture_fork_failure"]
        for name in names:
            cls.build(name)
        if sys.platform.startswith("linux"):
            objdir = cls.base / "exit"
            cls.compile("exit_group", ["--emit=obj", "--out-dir", str(objdir)])
            cc = shutil.which("clang-21") or shutil.which("clang") or shutil.which("cc")
            if not cc:
                raise RuntimeError("a C compiler is required for the native thread fixture")
            executable = objdir / "exit-host"
            result = run_process(
                [cc, "-pthread", str(ROOT / "tests/fixtures/io/exit_group.c"),
                 str(objdir / "exit_group.o"), "-o", str(executable)],
                timeout=30, capture_output=True, text=True,
            )
            if result.returncode:
                raise RuntimeError(result.stdout + result.stderr)
            cls.executables["exit_group"] = executable
            cls.compile("tcp_interrupt", ["--emit=obj", "--out-dir", str(objdir)])
            executable = objdir / "tcp-interrupt-host"
            result = run_process(
                [cc, str(ROOT / "tests/fixtures/io/tcp_interrupt.c"),
                 str(objdir / "tcp_interrupt.o"), "-o", str(executable)],
                timeout=30, capture_output=True, text=True,
            )
            if result.returncode:
                raise RuntimeError(result.stdout + result.stderr)
            cls.executables["tcp_interrupt"] = executable


    @classmethod
    def compile(cls, name, options):
        command = [COMPILER, "build", str(ROOT / f"tests/fixtures/io/{name}.wave")]
        if cls.target:
            command += ["--target", cls.target]
        result = run_process(command + options, env=cls.env, timeout=60,
                             capture_output=True, text=True)
        if result.returncode:
            raise RuntimeError(f"{name}: {result.stdout}\n{result.stderr}")

    @classmethod
    def build(cls, name):
        executable = cls.base / (name + (".exe" if os.name == "nt" else ""))
        cls.compile(name, ["-o", str(executable)])
        cls.executables[name] = executable

    def setUp(self):
        directory = tempfile.TemporaryDirectory(prefix="case-", dir=self.base)
        self.addCleanup(directory.cleanup)
        self.directory = Path(directory.name)

    def run_fixture(self, name, *args, **kwargs):
        try:
            result = run_process([str(self.executables[name]), *args], cwd=self.directory,
                                 timeout=10, capture_output=True, text=True, **kwargs)
        except subprocess.TimeoutExpired as error:
            self.fail(f"{name} target={self.target}: timed out after {error.timeout}s\n"
                      + timeout_output(error)[-8192:])
        self.assertEqual(result.returncode, 0,
                         f"{name} target={self.target}: exit {result.returncode}\n"
                         + (result.stdout + result.stderr)[-8192:])
        return result.stdout.strip()

    def low_fd_limit(self):
        if not sys.platform.startswith("linux"):
            return {}
        def limit():
            import resource
            _, hard = resource.getrlimit(resource.RLIMIT_NOFILE)
            resource.setrlimit(resource.RLIMIT_NOFILE, (64, hard))
        return {"preexec_fn": limit}

    def test_invalid_buffers_do_not_open_or_truncate_files(self):
        destination = self.directory / "destination.bin"
        existing = self.directory / "existing.bin"
        destination.write_bytes(b"untouched")
        existing.write_bytes(b"preserve me")
        self.run_fixture("invalid_buffers")
        self.assertEqual(destination.read_bytes(), b"untouched")
        self.assertEqual(existing.read_bytes(), b"preserve me")
        self.assertFalse((self.directory / "missing-write.bin").exists())
        self.assertFalse((self.directory / "missing-append.bin").exists())
        # Zero-length null write is still allowed and truncates/creates.
        self.assertEqual((self.directory / "zero-truncate.bin").read_bytes(), b"")

    def test_copy_self_preserves_bytes_and_closes_error_handles(self):
        source = self.directory / "source.bin"
        data = bytes(range(97))
        source.write_bytes(data)
        self.assertEqual(self.run_fixture("copy_self", **self.low_fd_limit()), "-4096")
        self.assertEqual(source.read_bytes(), data)

    def test_copy_hardlink_preserves_bytes(self):
        source = self.directory / "source.bin"
        data = bytes(range(97))
        source.write_bytes(data)
        os.link(source, self.directory / "destination.bin")
        self.assertEqual(self.run_fixture("copy", **self.low_fd_limit()), "-4096")
        self.assertEqual(source.read_bytes(), data)

    @unittest.skipIf(os.name == "nt", "Windows symlink creation requires a host privilege")
    def test_copy_symlink_preserves_bytes(self):
        source = self.directory / "source.bin"
        source.write_bytes(b"original")
        (self.directory / "destination.bin").symlink_to(source)
        self.assertEqual(self.run_fixture("copy"), "-4096")
        self.assertEqual(source.read_bytes(), b"original")

    def test_copy_distinct_existing_destination_is_truncated_and_handles_close(self):
        data = bytes(range(97))
        (self.directory / "source.bin").write_bytes(data)
        target = self.directory / "destination.bin"
        target.write_bytes(b"old" * 100)
        self.assertEqual(self.run_fixture("copy", **self.low_fd_limit()), "97")
        self.assertEqual(target.read_bytes(), data)

    def test_copy_creates_destination(self):
        (self.directory / "source.bin").write_bytes(b"new content")
        self.assertEqual(self.run_fixture("copy"), "11")
        self.assertEqual((self.directory / "destination.bin").read_bytes(), b"new content")

    def test_missing_copy_source_preserves_destination(self):
        target = self.directory / "destination.bin"
        target.write_bytes(b"untouched")
        self.assertLess(int(self.run_fixture("copy")), 0)
        self.assertEqual(target.read_bytes(), b"untouched")

    def test_read_empty_exact_capacity_and_oversized_files(self):
        for length in (0, 1, 127, 128, 129):
            with self.subTest(length=length):
                data = bytes(range(length))
                (self.directory / "source.bin").write_bytes(data)
                count, checksum = map(int, self.run_fixture("read_all").split())
                if length > 128:
                    self.assertEqual(count, -4098)
                else:
                    self.assertEqual(count, length)
                    self.assertEqual(checksum, sum((i+1)*b for i, b in enumerate(data)))

    def test_read_zero_capacity_probes_eof(self):
        source = self.directory / "source.bin"
        source.write_bytes(b"")
        self.assertEqual(self.run_fixture("read_zero"), "0")
        source.write_bytes(b"x")
        self.assertEqual(self.run_fixture("read_zero"), "-4098")

    @unittest.skipUnless(sys.platform.startswith("linux"), "Linux virtual file")
    def test_read_nonempty_virtual_file_with_zero_stat_size(self):
        (self.directory / "source.bin").symlink_to("/proc/self/status")
        self.assertEqual(os.stat("/proc/self/status").st_size, 0)
        self.assertEqual(self.run_fixture("read_all").split()[0], "-4098")

    @unittest.skipUnless(sys.platform.startswith("linux"), "Linux virtual file")
    def test_read_small_virtual_file_returns_actual_content(self):
        (self.directory / "source.bin").symlink_to("/proc/self/comm")
        expected = b"read_all\n"
        self.assertEqual(self.run_fixture("read_all"),
                         f"{len(expected)} {sum((i+1)*b for i,b in enumerate(expected))}")

    @unittest.skipUnless(os.name == "posix", "native FIFO")
    def test_read_stream_in_multiple_chunks_until_eof(self):
        fifo = self.directory / "source.bin"
        os.mkfifo(fifo)
        errors = []
        stopped = threading.Event()
        def produce():
            writer = None
            try:
                deadline = time.monotonic() + 5
                while not stopped.is_set():
                    try:
                        writer = os.open(fifo, os.O_WRONLY | os.O_NONBLOCK)
                        break
                    except OSError as error:
                        if error.errno != errno.ENXIO or time.monotonic() >= deadline:
                            raise
                        stopped.wait(0.005)
                if writer is not None:
                    for piece in (b"abc", b"defgh", b"ijk"):
                        os.write(writer, piece)
                        stopped.wait(0.03)
            except BaseException as error:
                errors.append(error)
            finally:
                if writer is not None:
                    os.close(writer)
        thread = threading.Thread(target=produce)
        thread.start()
        try:
            result = self.run_fixture("read_all")
        finally:
            stopped.set()
            thread.join(timeout=2)
        self.assertFalse(thread.is_alive())
        self.assertFalse(errors, errors)
        self.assertEqual(result, f"11 {sum((i+1)*b for i,b in enumerate(b'abcdefghijk'))}")

    def test_tcp_backpressure_does_not_turn_readiness_into_an_unbounded_send(self):
        self.run_fixture("tcp_timeout")

    def test_tcp_read_timeout_cancellation_and_later_data(self):
        self.run_fixture("tcp_read")

    @unittest.skipUnless(sys.platform.startswith("linux"), "native Unix signal and alias probe")
    def test_tcp_interruptions_share_one_deadline_and_preserve_alias_mode(self):
        self.run_fixture("tcp_interrupt")

    @unittest.skipUnless(os.name == "posix", "fork/wait return conventions")
    def test_fork_distinguishes_child_and_parent_and_waits_for_exact_child(self):
        self.run_fixture("fork_status")

    def write_capture_child(self, body):
        child = self.directory / "capture-child"
        child.write_text(f"#!{sys.executable}\n" + body)
        child.chmod(0o700)

    @unittest.skipUnless(os.name == "posix", "fork/exec capture")
    def test_capture_has_no_inherited_pipe_reader_and_no_parent_leak(self):
        self.write_capture_child("""import os, fcntl, stat
out = os.fstat(1)
for fd in range(256):
    try:
        info = os.fstat(fd)
        flags = fcntl.fcntl(fd, fcntl.F_GETFL)
    except OSError:
        continue
    if (stat.S_ISFIFO(info.st_mode)
            and (info.st_dev, info.st_ino) == (out.st_dev, out.st_ino)
            and flags & os.O_ACCMODE == os.O_RDONLY):
        raise SystemExit(42)
os.write(1, b'OK')
""")
        self.run_fixture("capture", **self.low_fd_limit())
        def close_standard_slots():
            os.close(0)
            os.close(1)
        self.run_fixture("capture", preexec_fn=close_standard_slots)


    @unittest.skipUnless(os.name == "posix", "fork/exec capture")
    def test_capture_writer_observes_consumer_closure(self):
        self.write_capture_child("""import os
try:
    while True:
        os.write(1, b'x' * 65536)
except BrokenPipeError:
    pass
""")
        self.run_fixture("capture_close")

    @unittest.skipUnless(sys.platform.startswith("linux"), "bounded Linux descriptor limit")
    def test_capture_exec_failure_and_pipe_exhaustion_release_resources(self):
        self.run_fixture("capture_failures", **self.low_fd_limit())

    @unittest.skipUnless(sys.platform.startswith("linux") and os.geteuid() != 0,
                         "unprivileged Linux process limit")
    def test_capture_fork_failure_closes_both_pipe_ends(self):
        def forbid_fork():
            import resource
            _, hard = resource.getrlimit(resource.RLIMIT_NPROC)
            resource.setrlimit(resource.RLIMIT_NPROC, (0, hard))
        self.run_fixture("capture_fork_failure", preexec_fn=forbid_fork)

    @unittest.skipUnless(sys.platform.startswith("linux"), "Linux exit_group")
    def test_process_exit_terminates_other_native_threads(self):
        for args in ([], ["worker"]):
            with self.subTest(args=args):
                result = run_process([str(self.executables["exit_group"]), *args],
                                     timeout=5, capture_output=True, text=True)
                self.assertEqual(result.returncode, 37, result.stdout + result.stderr)


class FixtureDiagnosticTests(unittest.TestCase):
    def run_failing_fixture(self, *, result=None, error=None):
        runner = StandardIoRuntimeTests()
        runner.executables = {"tcp_timeout": Path("tcp_timeout")}
        runner.directory = ROOT
        runner.target = "aarch64-apple-darwin"
        with patch("tools.test_std_io_runtime.run_process", return_value=result, side_effect=error):
            with self.assertRaises(AssertionError) as failure:
                runner.run_fixture("tcp_timeout")
        return str(failure.exception)

    def test_timeout_includes_target_phase_and_partial_output(self):
        error = subprocess.TimeoutExpired(["tcp_timeout"], 10,
                                          output=b"partial output", stderr=b"tcp: drain and resume\n")
        message = self.run_failing_fixture(error=error)
        self.assertIn("tcp_timeout target=aarch64-apple-darwin: timed out after 10s", message)
        self.assertIn("partial output", message)
        self.assertIn("tcp: drain and resume", message)

    def test_failure_includes_exit_status_and_observed_values(self):
        result = subprocess.CompletedProcess(["tcp_timeout"], 17, "", "confirmed before drain=0\n")
        message = self.run_failing_fixture(result=result)
        self.assertIn("tcp_timeout target=aarch64-apple-darwin: exit 17", message)
        self.assertIn("confirmed before drain=0", message)


if __name__ == "__main__":
    unittest.main()
