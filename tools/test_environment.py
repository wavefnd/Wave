# SPDX-License-Identifier: MPL-2.0
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from tools.ci import environment
from tools.ci.common import Runner


class EnvironmentTests(unittest.TestCase):
    def test_exact_roundtrip_and_no_injected_variables(self):
        for value in ("", " x = y ", "one\nSECOND=two", "a\r\nb\n\n", "\r", "한\n🙂"):
            with self.subTest(value=value):
                self.assertEqual(
                    environment.decode(environment.encode("AUDIT", value)),
                    {"AUDIT": value},
                )

    def test_collision_and_complete_input_validation(self):
        from types import SimpleNamespace

        with patch.object(
            environment.uuid,
            "uuid4",
            side_effect=[SimpleNamespace(hex="x"), SimpleNamespace(hex="y")],
        ):
            value = "wave_x\npayload"
            self.assertIn(b"<<wave_y\n", environment.encode("AUDIT", value))
        for data in (
            b"GOOD=one\nbad-name=two\n",
            b"A<<X\nunclosed",
            b"A<<bad delimiter\nx\n",
        ):
            with self.assertRaises(ValueError):
                environment.decode(data)
        for key in ("", "A\nB", "A=B", "A<<B", "1A"):
            with self.assertRaises(ValueError):
                environment.encode(key, "x")

    def test_refresh_is_atomic_and_writer_validates_before_changes(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "env"
            path.write_bytes(b"OK=changed\nbad-name=x\n")
            runner = object.__new__(Runner)
            runner.env = {
                "GITHUB_ENV": str(path),
                "GITHUB_PATH": str(Path(folder) / "path"),
                "OK": "old",
            }
            with self.assertRaises(ValueError):
                runner.refresh_environment()
            self.assertEqual(runner.env["OK"], "old")
            before = path.read_bytes()
            with self.assertRaises(ValueError):
                runner.setenv("BAD\nNAME", "x")
            self.assertEqual(path.read_bytes(), before)


class ExternalEnvironmentTests(unittest.TestCase):
    def test_actions_toolkit_delimiter_and_missing_final_newline(self):
        data = b"VALUE<<ghadelimiter_1234-5678\none\ntwo\nghadelimiter_1234-5678\n"
        self.assertEqual(environment.decode(data), {"VALUE": "one\ntwo"})
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "env"
            path.write_bytes(b"OLD=before")
            environment.append(path, environment.encode("NEW", "after"))
            self.assertEqual(
                environment.decode(path.read_bytes()), {"OLD": "before", "NEW": "after"}
            )

    def test_partial_write_failure_rolls_back_the_complete_record(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "env"
            path.write_bytes(b"OLD=before\n")
            actual = open

            class Failure:
                def __init__(self, stream):
                    self.stream = stream
                    self.count = 0

                def __enter__(self):
                    return self

                def __exit__(self, *args):
                    self.stream.close()

                def __getattr__(self, name):
                    return getattr(self.stream, name)

                def write(self, data):
                    self.count += 1
                    if self.count == 1:
                        return self.stream.write(data[:5])
                    raise OSError("injected write failure")

            with (
                patch.object(
                    environment,
                    "open",
                    side_effect=lambda *a, **k: Failure(actual(*a, **k)),
                    create=True,
                ),
                self.assertRaises(OSError),
            ):
                environment.append(path, environment.encode("NEW", "multi\nline"))
            self.assertEqual(path.read_bytes(), b"OLD=before\n")
