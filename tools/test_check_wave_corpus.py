import io
import os
import subprocess
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

                self.assertIn(str(Path("missing/wavec")), str(cm.exception))

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
                self.assertEqual(resolve_wavec(custom), custom.resolve())
                self.assertEqual(
                    resolve_wavec(Path("bin/custom_wavec")),
                    custom.resolve(),
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
                    self.assertEqual(resolve_wavec(None), custom.resolve())

    def test_explicit_and_environment_paths_resolve_symlinked_parent(self):
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            real = root / "real compiler dir"
            real.mkdir()
            compiler = real / "wavec"
            compiler.touch(mode=0o700)
            alias = root / "alias"
            try:
                alias.symlink_to(real, target_is_directory=True)
            except OSError as error:
                self.skipTest(f"directory symlinks unavailable: {error}")
            self.assertEqual(resolve_wavec(alias / "wavec"), compiler.resolve())
            with patch.dict(os.environ, {"WAVEC": str(alias / "wavec")}):
                self.assertEqual(resolve_wavec(None), compiler.resolve())

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


class TestCorpusTimeout(unittest.TestCase):
    def test_main_preserves_partial_timeout_output(self):
        error = subprocess.TimeoutExpired(
            ["wavec", "check"], 0.1, output="partial stdout", stderr="partial stderr"
        )
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            source = root / "std" / "sample.wave"
            source.parent.mkdir(parents=True)
            source.write_text("fun main() -> i32 { return 0; }")
            stdout = io.StringIO()
            stderr = io.StringIO()
            with patch.object(check_wave_corpus, "ROOT", root):
                with patch.object(check_wave_corpus, "resolve_wavec", return_value=Path("wavec")):
                    with patch.object(check_wave_corpus, "corpus_files", return_value=[source]):
                        with patch.object(check_wave_corpus, "run_process", side_effect=error):
                            with patch("sys.stdout", stdout), patch("sys.stderr", stderr):
                                self.assertEqual(main(["--timeout=0.1"]), 1)

        self.assertIn("timed out after 0.1s", stderr.getvalue())
        self.assertIn("partial stdout", stderr.getvalue())
        self.assertIn("partial stderr", stderr.getvalue())



class TestStdExampleContracts(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.example_path = self.root / "examples/std/example.wave"
        self.example_path.parent.mkdir(parents=True)
        self.fixture_root = self.root / "tests/fixtures"
        self.fixture_root.mkdir(parents=True)
        (self.root / "std").mkdir()

    def exercise(self, metadata="", *, exit_code=0, stdout="", stderr="",
                 fixtures=None, error=None):
        source = ("// wave-test: " + metadata + "\n" if metadata else "") + "fun main() {}\n"
        self.example_path.write_text(source, encoding="utf-8")
        for name, contents in (fixtures or {}).items():
            path = self.fixture_root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(contents, encoding="utf-8")
        calls = []

        def fake_process(command, **kwargs):
            calls.append((command, kwargs))
            if error is not None:
                raise error
            return subprocess.CompletedProcess(command, exit_code, stdout, stderr)

        records = []
        with (
            patch.object(check_wave_corpus, "ROOT", self.root),
            patch.object(check_wave_corpus, "run_process", side_effect=fake_process),
            patch("sys.stdout", io.StringIO()),
        ):
            failures = check_wave_corpus.run_std_examples(
                Path("wavec"), {"CUSTOM_ENV": "kept"}, 2.0,
                records=records, capability_probe=lambda: None,
            )
        return records[0], failures, calls

    def test_expected_exit_contract_accepts_nonzero_and_rejects_mismatch(self):
        row, failures, calls = self.exercise("expected-exit=7", exit_code=7)
        self.assertEqual((row["status"], row["expected_exit"], row["actual_exit"]),
                         ("pass", 7, 7))
        self.assertFalse(failures)
        self.assertEqual(len(calls), 1)

        row, failures, _ = self.exercise("expected-exit=7", exit_code=0)
        self.assertEqual(row["status"], "fail")
        self.assertIn("exit status 0, expected 7", row["reason"])
        self.assertEqual(len(failures), 1)

        row, failures, _ = self.exercise(exit_code=0)
        self.assertEqual(row["status"], "pass")
        self.assertFalse(failures)

    def test_output_contract_checks_both_streams_even_on_exit_zero(self):
        files = {"expected.out": "correct\n", "expected.err": "warning\n"}
        declaration = "stdout-file=expected.out, stderr-file=expected.err"
        row, failures, calls = self.exercise(
            declaration, stdout="wrong\n", stderr="warning\n", fixtures=files,
        )
        self.assertEqual(row["status"], "fail")
        self.assertIn("stdout fixture mismatch", row["reason"])
        self.assertIn("-correct", row["reason"])
        self.assertEqual(calls[0][1]["errors"], "strict")
        self.assertEqual(len(failures), 1)

        row, failures, _ = self.exercise(
            declaration, stdout="correct\n", stderr="wrong\n", fixtures=files,
        )
        self.assertIn("stderr fixture mismatch", row["reason"])
        self.assertEqual(row["status"], "fail")
        self.assertEqual(len(failures), 1)

        row, failures, _ = self.exercise(
            declaration, stdout="correct\n", stderr="warning\n", fixtures=files,
        )
        self.assertEqual(row["status"], "pass")
        self.assertFalse(failures)

    def test_fixture_stdin_and_environment_reach_the_process(self):
        row, failures, calls = self.exercise(
            "stdin-file=input.txt, env-file=env.json",
            fixtures={
                "input.txt": "first\nsecond\n",
                "env.json": '{"WAVE_CORPUS_FIXTURE": "enabled"}',
            },
        )
        self.assertFalse(failures)
        self.assertEqual(row["status"], "pass")
        command, arguments = calls[0]
        self.assertEqual(command[-2], "run")
        self.assertEqual(Path(command[-1]), Path("examples/std/example.wave"))
        self.assertEqual(arguments["input"], "first\nsecond\n")
        self.assertEqual(arguments["env"]["WAVE_CORPUS_FIXTURE"], "enabled")
        self.assertEqual(arguments["encoding"], "utf-8")

        row, failures, calls = self.exercise("stdin=hello")
        self.assertFalse(failures)
        self.assertEqual(calls[0][1]["input"], "hello\n")
        self.assertEqual(calls[0][1]["env"], {"CUSTOM_ENV": "kept"})

    def test_unsupported_modes_are_failures_not_false_passes(self):
        for metadata in (
            "mode=check", "runner=server", "udp-input=true", "runtime=compile",
        ):
            with self.subTest(metadata=metadata):
                row, failures, calls = self.exercise(metadata)
                self.assertEqual(row["status"], "fail")
                self.assertEqual(len(failures), 1)
                self.assertEqual(calls, [])
                self.assertIn("standard-library examples", row["reason"])

    def test_invalid_metadata_and_borrowed_fixtures_are_recorded(self):
        for metadata, fixtures, expected in (
            ("expected-exit=oops", {}, "expects an integer"),
            ("stdin-file=../escape", {}, "fixture path"),
            ("stdin-file=missing.txt", {}, "cannot read UTF-8 fixture"),
            ("env-file=env.json", {"env.json": "{bad"}, "invalid environment fixture"),
        ):
            with self.subTest(metadata=metadata):
                row, failures, calls = self.exercise(metadata, fixtures=fixtures)
                self.assertEqual(row["status"], "fail")
                self.assertEqual(len(failures), 1)
                self.assertIn(expected, row["reason"])
                self.assertFalse(calls)

        row, failures, calls = self.exercise("stdout-file=missing.txt")
        self.assertEqual(row["status"], "fail")
        self.assertIn("cannot read UTF-8 fixture", row["reason"])
        self.assertEqual(len(failures), 1)
        self.assertEqual(len(calls), 1)

    def test_undecodable_contract_output_is_a_per_example_failure(self):
        error = UnicodeDecodeError("utf-8", b"\xff", 0, 1, "invalid start byte")
        row, failures, calls = self.exercise(
            "stdout-file=expected.out",
            fixtures={"expected.out": ""},
            error=error,
        )
        self.assertEqual(row["status"], "fail")
        self.assertIn("example execution failed", row["reason"])
        self.assertEqual(len(failures), 1)
        self.assertEqual(len(calls), 1)

    def test_report_cannot_overwrite_borrowed_fixture(self):
        self.example_path.write_text("fun main() {}\n", encoding="utf-8")
        output_file = self.fixture_root / "expected.txt"
        output_file.write_text("do not overwrite\n", encoding="utf-8")
        errors = io.StringIO()
        with (
            patch.object(check_wave_corpus, "ROOT", self.root),
            patch.object(check_wave_corpus, "resolve_wavec", return_value=Path("wavec")),
            patch.object(check_wave_corpus, "corpus_files", return_value=[self.example_path]),
            patch.object(check_wave_corpus, "run_process", side_effect=AssertionError("ran compiler")),
            patch("sys.stderr", errors),
        ):
            result = main(["--run-std-examples", "--report-json", str(output_file)])
        self.assertEqual(result, 2)
        self.assertEqual(output_file.read_text(encoding="utf-8"), "do not overwrite\n")
        self.assertIn("report path aliases an input", errors.getvalue())


if __name__ == "__main__":
    unittest.main()
