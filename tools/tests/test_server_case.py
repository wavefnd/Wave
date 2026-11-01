# SPDX-License-Identifier: MPL-2.0
"""An unrelated responder cannot make a failed server fixture pass."""
import contextlib
import io
import socket
import sys
import unittest
from unittest.mock import patch

from tools import run_tests as runner

SERVER = r'''
import socket, sys, time
with socket.socket() as server:
    server.bind(('127.0.0.1', 0))
    server.listen()
    time.sleep(float(sys.argv[1]))
    print('WAVE-SERVER-READY', server.getsockname()[1], flush=True)
    conn, _ = server.accept()
    with conn:
        request = b''
        while not request.endswith(b'\r\n\r\n'):
            request += conn.recv(1024)
        body = b'Welcome to the Wave HTTP Server!\n'
        if sys.argv[2] == 'challenge':
            body += request
        conn.sendall(b'HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n' + body)
    time.sleep(30)
'''


class ServerTests(unittest.TestCase):
    def test_failed_process_cannot_pass_from_unrelated_response(self):
        with patch.object(runner.socket, 'create_connection') as connect, contextlib.redirect_stdout(io.StringIO()):
            status, detail = runner.run_server_test([sys.executable, '-c', 'raise SystemExit(7)'])
        self.assertEqual(status, 0)
        self.assertEqual(detail['actual_exit'], 7)
        connect.assert_not_called()

    def test_delayed_ready_owned_server_passes_but_fixed_response_fails(self):
        try:
            with socket.socket() as probe: probe.bind(('127.0.0.1', 0))
        except PermissionError:
            self.skipTest('local sockets unavailable')
        for delay, mode, expected in [('0', 'challenge', 1), ('1.1', 'challenge', 1), ('0', 'unrelated', 0)]:
            with self.subTest(delay=delay, mode=mode), contextlib.redirect_stdout(io.StringIO()):
                status, detail = runner.run_server_test([sys.executable, '-c', SERVER, delay, mode])
                self.assertEqual(status, expected, detail)

    def test_permission_unavailable_marker_is_a_skip(self):
        status, _ = runner.run_server_test([sys.executable, '-c', 'print("WAVE-SERVER-UNAVAILABLE -13", flush=True)'])
        self.assertEqual(status, 2)


    def test_output_cleanup_retries_only_transient_windows_sharing_errors(self):
        from unittest.mock import Mock
        denied = PermissionError("still releasing handle")
        denied.winerror = 32
        path = Mock()
        path.unlink.side_effect = [denied, None]
        with patch.object(runner.time, "sleep"):
            runner.remove_server_output([path])
        self.assertEqual(path.unlink.call_count, 2)
        path.unlink.side_effect = PermissionError("not a sharing violation")
        with self.assertRaises(PermissionError):
            runner.remove_server_output([path])


if __name__ == '__main__':
    unittest.main()
