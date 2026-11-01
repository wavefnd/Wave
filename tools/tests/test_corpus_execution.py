# SPDX-License-Identifier: MPL-2.0
"""Actual fake compiler processes and capability skips require no LLVM or network."""

import contextlib
import errno
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

from tools import check_wave_corpus as corpus
from tools import host_capabilities
from tools.process_tree import run_process


class CorpusProcessTests(unittest.TestCase):
    def fixture(self, root):
        for name in ("examples/z.wave", "examples/nested/a.wave", "std/stdlib.wave"):
            path = root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("corpus sentinel", encoding="utf-8")
        compiler = root / "fake-wavec"
        compiler.write_text("selected fake compiler")
        compiler.chmod(0o700)
        script = root / "compiler.py"
        script.write_text(
            """import json, os, pathlib, sys, time
args = sys.argv[1:]
std = pathlib.Path(args[args.index("--std-root") + 1])
assert (std / "stdlib.wave").read_text() == "corpus sentinel"
with open(os.environ["CORPUS_RECORD"], "a", encoding="utf-8") as stream:
    stream.write(json.dumps({"args":args, "home":os.environ["HOME"], "profile":os.environ["USERPROFILE"]}) + "\\n")
if "timeout" in args[-1]:
    print("partial timeout output", flush=True)
    time.sleep(5)
print("compiler stdout", flush=True)
print("compiler stderr", file=sys.stderr, flush=True)
sys.exit(7 if "failure" in args[-1] else 0)
""",
            encoding="utf-8",
        )
        return compiler, script

    def run_corpus(self, root, compiler, script, *args, selection="explicit"):
        home = root / "isolated-home"
        home.mkdir(exist_ok=True)
        record = root / "compiler-record.jsonl"
        environment = dict(
            os.environ, HOME=str(home), USERPROFILE=str(home), CORPUS_RECORD=str(record)
        )
        environment.pop("WAVEC", None)
        if selection == "env":
            environment["WAVEC"] = str(compiler)
        before_home = list(home.rglob("*"))
        stdout, stderr = io.StringIO(), io.StringIO()

        def launch(command, **kwargs):
            self.assertEqual(Path(command[0]).resolve(), compiler.resolve())
            return run_process([sys.executable, str(script), *command[1:]], **kwargs)

        with (
            patch.object(corpus, "ROOT", root),
            patch.object(corpus, "run_process", side_effect=launch),
            patch.dict(os.environ, environment, clear=True),
            contextlib.redirect_stdout(stdout),
            contextlib.redirect_stderr(stderr),
        ):
            status = corpus.main(
                [
                    *(["--wavec", str(compiler)] if selection == "explicit" else []),
                    *args,
                ]
            )
        self.assertEqual(list(home.rglob("*")), before_home)
        records = [json.loads(line) for line in record.read_text().splitlines()]
        self.assertTrue(
            all(
                row["home"] == str(home) and row["profile"] == str(home)
                for row in records
            )
        )
        return status, records, stdout.getvalue(), stderr.getvalue()

    def test_real_fake_compiler_checks_deterministic_sources_with_matching_std_and_private_home(
        self,
    ):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            compiler, script = self.fixture(root)
            status, records, stdout, stderr = self.run_corpus(root, compiler, script)
            self.assertEqual(status, 0, stderr)
            self.assertEqual(
                [Path(row["args"][-1]).as_posix() for row in records],
                ["examples/nested/a.wave", "examples/z.wave", "std/stdlib.wave"],
            )
            self.assertTrue(
                all(
                    row["args"][:3] == ["--std-root", str(root / "std"), "check"]
                    for row in records
                )
            )
            self.assertIn("3 passed, 0 failed", stdout)

    def test_real_fake_flow_accepts_environment_and_default_build_selection(self):
        for selection in ("env", "default"):
            with (
                self.subTest(selection=selection),
                tempfile.TemporaryDirectory() as directory,
            ):
                root = Path(directory)
                compiler, script = self.fixture(root)
                if selection == "default":
                    candidate = root / "target/release/wavec.exe"
                    candidate.parent.mkdir(parents=True)
                    compiler.rename(candidate)
                    compiler = candidate
                status, records, stdout, stderr = self.run_corpus(
                    root,
                    compiler,
                    script,
                    selection=selection,
                )
                self.assertEqual(status, 0, stderr)
                self.assertEqual(len(records), 3)
                selected = compiler.resolve() if selection == "env" else compiler
                self.assertIn(str(selected), stdout)

    def test_nonzero_and_per_file_timeout_keep_output_and_json_results(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            compiler, script = self.fixture(root)
            for name in ("failure", "timeout"):
                (root / f"examples/{name}.wave").write_text("failure fixture")
            report = root / "report.json"
            status, records, stdout, stderr = self.run_corpus(
                root, compiler, script, "--timeout=0.2", "--report-json", str(report)
            )
            self.assertEqual(status, 1)
            self.assertIn("compiler stdout", stderr)
            self.assertIn("compiler stderr", stderr)
            self.assertIn("timed out after 0.2s", stderr)
            rows = {row["name"]: row for row in json.loads(report.read_text())["tests"]}
            self.assertEqual(rows["examples/failure.wave"]["actual_exit"], 7)
            self.assertEqual(rows["examples/timeout.wave"]["status"], "timeout")
            self.assertEqual(len(records), 5)

    def test_recursive_discovery_excludes_non_wave_files(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.fixture(root)
            (root / "std/ignore.txt").write_text("not a source")
            with patch.object(corpus, "ROOT", root):
                files = corpus.corpus_files()
            self.assertEqual(
                [path.relative_to(root).as_posix() for path in files],
                ["examples/nested/a.wave", "examples/z.wave", "std/stdlib.wave"],
            )


class ExampleCapabilityTests(unittest.TestCase):
    def fixture(self, root):
        examples = root / "examples/std"
        examples.mkdir(parents=True)
        (examples / "network.wave").write_text(
            "// wave-test: runtime=native, requires=sockets\nfun main() {}\n"
        )
        (examples / "pure.wave").write_text("fun main() {}\n")
        return examples

    def test_denied_socket_examples_skip_and_pure_examples_still_execute(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.fixture(root)
            records = []
            with (
                patch.object(corpus, "ROOT", root),
                patch.object(
                    corpus,
                    "run_process",
                    return_value=subprocess.CompletedProcess([], 0, "ok", ""),
                ) as process,
                contextlib.redirect_stdout(io.StringIO()),
            ):
                failures = corpus.run_std_examples(
                    Path("wavec"),
                    {},
                    1,
                    records=records,
                    capability_probe=lambda: "local sockets unavailable: EPERM",
                )
            self.assertEqual(failures, [])
            self.assertEqual(process.call_count, 1)
            self.assertEqual([row["status"] for row in records], ["skip", "pass"])
            self.assertIn("EPERM", records[0]["reason"])

    def test_available_socket_environment_does_not_hide_real_program_failures(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.fixture(root)
            with (
                patch.object(corpus, "ROOT", root),
                patch.object(
                    corpus,
                    "run_process",
                    return_value=subprocess.CompletedProcess(
                        [], 9, "program output", "real failure"
                    ),
                ),
                contextlib.redirect_stdout(io.StringIO()),
            ):
                failures = corpus.run_std_examples(
                    Path("wavec"), {}, 1, capability_probe=lambda: None
                )
            self.assertEqual(len(failures), 2)
            self.assertTrue(all("real failure" in detail for _, detail in failures))

    def test_compile_failures_survive_network_skip_in_machine_readable_report(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.fixture(root)
            report = root / "report.json"

            def execute(command, **kwargs):
                failed = "check" in command and str(command[-1]).endswith(
                    "network.wave"
                )
                return subprocess.CompletedProcess(
                    command, 7 if failed else 0, "", "compile failure" if failed else ""
                )

            with (
                patch.object(corpus, "ROOT", root),
                patch.object(corpus, "resolve_wavec", return_value=root / "wavec"),
                patch.object(corpus, "run_process", side_effect=execute),
                patch.object(corpus, "local_network_unavailable", return_value="EPERM"),
                contextlib.redirect_stdout(io.StringIO()),
                contextlib.redirect_stderr(io.StringIO()),
            ):
                self.assertEqual(
                    corpus.main(["--run-std-examples", "--report-json", str(report)]), 1
                )
            data = json.loads(report.read_text())
            self.assertEqual(
                data["summary"], {"pass": 2, "fail": 1, "timeout": 0, "skip": 1}
            )
            failed = next(row for row in data["tests"] if row["status"] == "fail")
            self.assertEqual(failed["phase"], "check")
            self.assertEqual(failed["actual_exit"], 7)

    def test_probe_permission_errors_skip_but_unexpected_errors_fail(self):
        with patch.object(
            host_capabilities.socket,
            "socket",
            side_effect=PermissionError(errno.EPERM, "blocked"),
        ):
            self.assertIn("blocked", host_capabilities.local_network_unavailable())
        with patch.object(
            host_capabilities.socket,
            "socket",
            side_effect=OSError(errno.EIO, "broken host"),
        ):
            with self.assertRaises(OSError):
                host_capabilities.local_network_unavailable()


if __name__ == "__main__":
    unittest.main()
