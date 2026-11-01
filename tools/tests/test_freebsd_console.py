# SPDX-License-Identifier: MPL-2.0
"""Serial console parsing without a VM, image, network or compiler."""

import io
from types import SimpleNamespace
import unittest
from unittest.mock import Mock, patch

from tools.check_freebsd_sys import Console


class ConsoleTests(unittest.TestCase):
    def console(self):
        output = Mock()
        output.fileno.return_value = 7
        log = io.BytesIO()
        log.name = "guest.log"
        with patch("tools.check_freebsd_sys.time.monotonic", return_value=0):
            return Console(SimpleNamespace(stdout=output), log, timeout=10)

    def test_single_and_split_reads_log_and_consume_exactly_the_match(self):
        for chunks in (
            [b"boot ready> tail next>"],
            [b"bo", b"ot rea", b"dy> tail next>"],
        ):
            with self.subTest(chunks=chunks):
                console = self.console()
                with (
                    patch(
                        "tools.check_freebsd_sys.select.select",
                        return_value=([console.process.stdout], [], []),
                    ),
                    patch(
                        "tools.check_freebsd_sys.os.read", side_effect=chunks
                    ) as read,
                    patch("tools.check_freebsd_sys.time.monotonic", return_value=0),
                ):
                    match = console.expect(r"boot (ready)>")
                    self.assertEqual(match.group(1), "ready")
                    self.assertEqual(console.buffer, " tail next>")
                    self.assertEqual(console.expect("next>").group(), "next>")
                    self.assertEqual(console.buffer, "")
                self.assertEqual(read.call_count, len(chunks))
                self.assertEqual(console.log.getvalue(), b"".join(chunks))
                read.assert_called_with(7, 65536)

    def test_eof_before_match_retains_guest_log_and_names_expected_pattern(self):
        console = self.console()
        with (
            patch(
                "tools.check_freebsd_sys.select.select",
                return_value=([console.process.stdout], [], []),
            ),
            patch("tools.check_freebsd_sys.os.read", side_effect=[b"booting", b""]),
            patch("tools.check_freebsd_sys.time.monotonic", return_value=0),
        ):
            with self.assertRaisesRegex(
                RuntimeError, "Guest exited before 'login:'.*guest.log"
            ):
                console.expect("login:")
        self.assertEqual(console.log.getvalue(), b"booting")
        self.assertEqual(console.buffer, "booting")

    def test_timeout_uses_deadline_without_waiting_or_reading_from_guest(self):
        console = self.console()
        with (
            patch("tools.check_freebsd_sys.time.monotonic", side_effect=[0, 0, 0.2]),
            patch(
                "tools.check_freebsd_sys.select.select", return_value=([], [], [])
            ) as select,
            patch("tools.check_freebsd_sys.os.read") as read,
        ):
            with self.assertRaisesRegex(
                TimeoutError, "Guest did not reach 'login:'.*guest.log"
            ):
                console.expect("login:", timeout=0.1)
        read.assert_not_called()
        self.assertEqual(select.call_count, 1)
        self.assertAlmostEqual(select.call_args.args[3], 0.1)


if __name__ == "__main__":
    unittest.main()
