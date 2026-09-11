import io
import unittest
from unittest.mock import patch

from tools.check_wave_corpus import parse_args, main


class TestCheckWaveCorpusCLI(unittest.TestCase):
    def test_default_timeout(self):
        args = parse_args([])
        self.assertEqual(args.timeout, 15.0)

    def test_valid_positive_timeouts(self):
        cases = [
            ("0.25", 0.25),
            ("1", 1.0),
            ("15", 15.0),
            ("30.5", 30.5),
            ("1e2", 100.0),
        ]
        for arg_str, expected in cases:
            with self.subTest(arg=arg_str):
                args = parse_args(["--timeout", arg_str])
                self.assertEqual(args.timeout, expected)

                args_eq = parse_args([f"--timeout={arg_str}"])
                self.assertEqual(args_eq.timeout, expected)

    def test_invalid_timeouts_table_driven(self):
        invalid_values = [
            # Zero
            "0",
            "0.0",
            "-0.0",
            # Negative
            "-1",
            "-0.001",
            "-15",
            # NaN
            "nan",
            "NaN",
            "NAN",
            "-nan",
            # Infinities
            "inf",
            "-inf",
            "Infinity",
            "-Infinity",
            "+inf",
            # Non-numeric
            "abc",
            "",
            "15s",
            "None",
        ]
        for value in invalid_values:
            with self.subTest(value=value):
                stderr = io.StringIO()
                with patch("sys.stderr", stderr):
                    with self.assertRaises(SystemExit) as cm:
                        parse_args([f"--timeout={value}"])

                self.assertEqual(cm.exception.code, 2)
                err_output = stderr.getvalue()
                self.assertIn("--timeout", err_output)
                self.assertIn("timeout must be a finite positive number", err_output)
                self.assertNotIn("Traceback", err_output)

    def test_main_rejects_invalid_timeout_before_compiler_discovery(self):
        stderr = io.StringIO()
        with patch("sys.stderr", stderr):
            with patch("tools.check_wave_corpus.resolve_wavec") as mock_resolve:
                with self.assertRaises(SystemExit) as cm:
                    main(["--timeout=0"])

                self.assertEqual(cm.exception.code, 2)
                mock_resolve.assert_not_called()
                err_output = stderr.getvalue()
                self.assertIn("--timeout", err_output)
                self.assertIn("timeout must be a finite positive number", err_output)
                self.assertNotIn("Traceback", err_output)

    def test_help_flag(self):
        stdout = io.StringIO()
        with patch("sys.stdout", stdout):
            with self.assertRaises(SystemExit) as cm:
                parse_args(["--help"])

        self.assertEqual(cm.exception.code, 0)
        output = stdout.getvalue()
        self.assertIn("--timeout TIMEOUT", output)
        self.assertIn("per-file timeout in seconds (default: 15)", output)


if __name__ == "__main__":
    unittest.main()
