# SPDX-License-Identifier: MPL-2.0
"""Actual local sockets/processes; every child runs under an external deadline."""

import os
import socket
import sys
import tempfile
import unittest
from pathlib import Path

from tools.process_tree import run_process

ROOT = Path(__file__).resolve().parents[1]
COMPILER = os.environ.get("WAVE_TEST_COMPILER")


@unittest.skipUnless(COMPILER, "Cargo supplies a matching compiler")
class SocketBoundaries(unittest.TestCase):
    def run_source(self, source, *, pass_fds=()):
        with tempfile.TemporaryDirectory(prefix="wave-socket-bounds-") as folder:
            path = Path(folder) / "main.wave"
            path.write_text(source, encoding="utf-8")
            for opt in ("-O0", "-O2"):
                binary = Path(folder) / ("case.exe" if os.name == "nt" else "case")
                result = run_process(
                    [
                        COMPILER,
                        "--std-root",
                        str(ROOT / "std"),
                        "build",
                        str(path),
                        "--target",
                        os.environ["WAVE_TEST_TARGET"],
                        opt,
                        "-o",
                        str(binary),
                    ],
                    timeout=60,
                    capture_output=True,
                    text=True,
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                extra = {"pass_fds": pass_fds} if os.name == "posix" else {}
                result = run_process(
                    [str(binary)], timeout=5, capture_output=True, text=True, **extra
                )
                self.assertEqual(
                    result.returncode,
                    0,
                    f"{opt}: {result.returncode} {result.stdout} {result.stderr}",
                )

    @unittest.skipIf(os.name == "nt", "inherited POSIX socketpair")
    def test_empty_reads_exact_peek_rejection_and_ordinary_peek_preserve_data(self):
        left, right = socket.socketpair()
        self.addCleanup(left.close)
        self.addCleanup(right.close)
        fd = left.fileno()
        self.run_source(
            f"""import("std::net::unix")::{{unix_read, UnixStream}};
import("std::net::error")::{{NetIoResult}};
fun main() -> i32 {{
    var result: NetIoResult = unix_read(UnixStream {{ fd: {fd} }}, null, 0);
    if (result.count != 0 || result.eof || result.error.kind != 0) {{ return 1; }}
    return 0;
}}""",
            pass_fds=(fd,),
        )
        right.sendall(b"ab")
        self.run_source(
            f"""import("std::net::socket_base")::{{net_recv_exact, net_recv_exact_io, net_recv_once}};
import("std::net::error")::{{NetIoResult}};
fun main() -> i32 {{
    var bytes: array<u8, 4> = [11, 22, 33, 44];
    if (net_recv_exact({fd}, &bytes[0], 4, 2) != -22) {{ return 1; }}
    var result: NetIoResult = net_recv_exact_io({fd}, &bytes[0], 4, 2);
    if (result.count != 0 || result.error.native_code != -22) {{ return 2; }}
    if (bytes[0] != 11 || bytes[1] != 22 || bytes[2] != 33 || bytes[3] != 44) {{ return 3; }}
    if (net_recv_once({fd}, &bytes[0], 2, 2) != 2 || bytes[0] != 97 || bytes[1] != 98) {{ return 4; }}
    return 0;
}}""",
            pass_fds=(fd,),
        )
        self.assertEqual(left.recv(2), b"ab")

    @unittest.skipIf(os.name == "nt", "Windows SOCKET is intentionally full width")
    def test_oversized_descriptors_cannot_mutate_write_or_close_the_real_socket(self):
        left, right = socket.socketpair()
        self.addCleanup(left.close)
        self.addCleanup(right.close)
        fd = left.fileno()
        self.run_source(
            f"""import("std::net::socket_base")::{{net_fd_valid, net_send_once, net_close, net_shutdown}};
import("std::net::socketopt")::{{net_set_nonblock}};
fun main() -> i32 {{
    var bad: i64 = {fd} + 4294967296;
    var payload: u8 = 7;
    if (net_fd_valid(bad) || net_set_nonblock(bad, 1) != -22) {{ return 1; }}
    if (net_send_once(bad, &payload, 1, 0) != -22 || net_close(bad) != -22 || net_shutdown(bad, 2) != -22) {{ return 2; }}
    return 0;
}}""",
            pass_fds=(fd,),
        )
        self.assertTrue(os.get_blocking(fd))
        right.settimeout(0.05)
        with self.assertRaises(TimeoutError):
            right.recv(1)

    def test_address_family_failure_preserves_address_and_native_errors(self):
        # Winsock requires a bound socket for getsockname. Use an explicit
        # loopback address so the fixture tests family validation on every OS.
        self.run_source("""import("std::net::socket_base")::{net_socket_udp_v4, net_bind_socket_v4, net_local_addr_v6, net_local_addr_v4, net_close};
import("std::net::address")::{SocketAddrV6, SocketAddrV4, socket_addr_v6_any, socket_addr_v4_any, socket_addr_v4_loopback};
fun main() -> i32 {
    var fd: i64 = net_socket_udp_v4();
    if (fd < 0) { return 1; }
    var local: SocketAddrV4 = socket_addr_v4_loopback(0);
    if (net_bind_socket_v4(fd, local) != 0) { net_close(fd); return 4; }
    var wrong: SocketAddrV6 = socket_addr_v6_any(12345);
    if (net_local_addr_v6(fd, &wrong) != -22 || wrong.port != 12345) { net_close(fd); return 2; }
    var valid: SocketAddrV4 = socket_addr_v4_any(0);
    if (net_local_addr_v4(fd, &valid) != 0 || valid.port == 0) { net_close(fd); return 3; }
    return net_close(fd) as i32;
}""")

    @unittest.skipIf(os.name == "nt", "POSIX pid_t and signal-0 probes")
    def test_invalid_pid_values_do_not_alias_processes_or_change_wait_status(self):
        self.run_source("""import("std::process::core")::{proc_getpid, proc_kill, proc_waitpid_raw};
fun main() -> i32 {
    var pid: i64 = proc_getpid();
    if (proc_kill(pid, 0) != 0 || proc_kill(pid + 4294967296, 0) != -22 || proc_kill(pid - 4294967296, 0) != -22) { return 1; }
    var status: i32 = 42;
    if (proc_waitpid_raw(4294967296, &status, 1) != -22 || status != 42) { return 2; }
    return 0;
}""")

    @unittest.skipIf(
        os.name == "nt", "readiness implementation and POSIX nonblocking flags"
    )
    def test_invalid_async_timeout_does_not_change_socket_mode(self):
        left, right = socket.socketpair()
        self.addCleanup(left.close)
        self.addCleanup(right.close)
        fd = left.fileno()
        self.run_source(
            f"""import("std::task" as task);
import("std::task::net" as net);
import("std::net::error")::{{NetIoResult}};
fun main() -> i32 {{
    var payload: u8 = 7;
    var result: NetIoResult = task::block_on(net::write_all({fd}, &payload, 1, -2));
    task::shutdown();
    if (result.error.native_code != -22) {{ return 1; }}
    return 0;
}}""",
            pass_fds=(fd,),
        )
        self.assertTrue(os.get_blocking(fd))

    def test_selected_error_domains_match_timeout_and_connect_classification(self):
        timeout = (
            10060
            if os.name == "nt"
            else 60
            if sys.platform.startswith(("darwin", "freebsd"))
            else 110
        )
        overflow = 84 if sys.platform.startswith(("darwin", "freebsd")) else 75
        pending = (
            -10036
            if os.name == "nt"
            else -36
            if sys.platform.startswith(("darwin", "freebsd"))
            else -115
        )
        not_pending = (
            -115
            if sys.platform.startswith(("darwin", "freebsd")) or os.name == "nt"
            else -36
        )
        self.run_source(f"""import("std::net::error")::{{NET_NATIVE_TIMEOUT, NET_ERROR_TIMED_OUT, NetError, net_error_from_native, net_connect_pending}};
import("std::sys::errors")::{{SYS_ERR_OVERFLOW}};
fun main() -> i32 {{
    if (NET_NATIVE_TIMEOUT != -{timeout} || SYS_ERR_OVERFLOW != -{overflow}) {{ return 1; }}
    var error: NetError = net_error_from_native(NET_NATIVE_TIMEOUT);
    if (error.kind != NET_ERROR_TIMED_OUT || !net_connect_pending({pending}) || net_connect_pending({not_pending})) {{ return 2; }}
    return 0;
}}""")
