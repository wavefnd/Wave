import io
import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from tools.check_wave_corpus import parse_args, resolve_wavec, main
import tools.check_wave_corpus as check_wave_corpus


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


class TestResolveWavec(unittest.TestCase):
    def test_invalid_explicit_path_fails_without_selecting_existing_fallback(self):
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            fallback = root / "target" / "release" / "wavec"
            fallback.parent.mkdir(parents=True)
            fallback.touch()

            with patch.object(check_wave_corpus, "ROOT", root):
                with self.assertRaises(FileNotFoundError) as cm:
                    resolve_wavec(Path("missing/wavec"))

                self.assertIn("missing/wavec", str(cm.exception))

    def test_invalid_explicit_wavec_env_fails_without_selecting_fallback(self):
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            fallback = root / "target" / "release" / "wavec"
            fallback.parent.mkdir(parents=True)
            fallback.touch()

            with patch.object(check_wave_corpus, "ROOT", root):
                with patch.dict(os.environ, {"WAVEC": "missing_env_wavec"}, clear=False):
                    with self.assertRaises(FileNotFoundError) as cm:
                        resolve_wavec(None)

                    self.assertIn("missing_env_wavec", str(cm.exception))

    def test_valid_explicit_path_selected(self):
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            custom = root / "bin" / "custom_wavec"
            custom.parent.mkdir(parents=True)
            custom.touch(mode=0o700)

            fallback = root / "target" / "release" / "wavec"
            fallback.parent.mkdir(parents=True)
            fallback.touch()

            with patch.object(check_wave_corpus, "ROOT", root):
                self.assertEqual(resolve_wavec(custom), custom)
                self.assertEqual(
                    resolve_wavec(Path("bin/custom_wavec")),
                    custom,
                )

    def test_valid_wavec_env_selected(self):
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            custom = root / "bin" / "custom_wavec"
            custom.parent.mkdir(parents=True)
            custom.touch(mode=0o700)

            fallback = root / "target" / "release" / "wavec"
            fallback.parent.mkdir(parents=True)
            fallback.touch(mode=0o700)

            with patch.object(check_wave_corpus, "ROOT", root):
                with patch.dict(os.environ, {"WAVEC": str(custom)}, clear=False):
                    self.assertEqual(resolve_wavec(None), custom)

    def test_no_override_falls_back_to_discovery(self):
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            fallback = root / "target" / "release" / "wavec"
            fallback.parent.mkdir(parents=True)
            fallback.touch(mode=0o700)

            with patch.object(check_wave_corpus, "ROOT", root):
                env = os.environ.copy()
                env.pop("WAVEC", None)
                with patch.dict(os.environ, env, clear=True):
                    self.assertEqual(resolve_wavec(None), fallback)

    def test_empty_wavec_env_falls_back_to_discovery(self):
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            fallback = root / "target" / "release" / "wavec"
            fallback.parent.mkdir(parents=True)
            fallback.touch(mode=0o700)

            with patch.object(check_wave_corpus, "ROOT", root):
                with patch.dict(os.environ, {"WAVEC": "   "}, clear=False):
                    self.assertEqual(resolve_wavec(None), fallback)

    def test_no_override_and_no_binary_raises_file_not_found(self):
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            with patch.object(check_wave_corpus, "ROOT", root):
                env = os.environ.copy()
                env.pop("WAVEC", None)
                with patch.dict(os.environ, env, clear=True):
                    with self.assertRaises(FileNotFoundError) as cm:
                        resolve_wavec(None)

                    self.assertIn("wavec not found; build it or pass --wavec", str(cm.exception))

    @unittest.skipIf(os.name == "nt", "POSIX executable bits do not apply")
    def test_main_rejects_non_executable_compiler_without_traceback(self):
        with tempfile.TemporaryDirectory() as td:
            compiler = Path(td) / "wavec"
            compiler.touch(mode=0o600)
            stderr = io.StringIO()
            with patch("sys.stderr", stderr):
                self.assertEqual(main(["--wavec", str(compiler)]), 2)

        self.assertIn(str(compiler), stderr.getvalue())
        self.assertIn("not launchable", stderr.getvalue())
        self.assertNotIn("Traceback", stderr.getvalue())


if __name__ == "__main__":
    unittest.main()
