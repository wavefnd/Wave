# SPDX-License-Identifier: MPL-2.0
"""Case contracts, stable partitions and real artifact failures use no SDKs."""

import contextlib
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import Mock, patch

from tools import case_manifest, run_tests
from tools.test_run_tests import fake_compiler
from tools.ci import test as ci_test
from tools.test_contracts import (
    TestMetadata,
    parse_test_metadata,
    read_fixture,
    read_wasm_contract,
    standard_input,
    test_environment,
    validate_output,
    validate_compiled_artifact,
)


def leb(value):
    result = bytearray()
    while True:
        byte = value & 127
        value >>= 7
        result.append(byte | (128 if value else 0))
        if not value:
            return bytes(result)


def section(kind, data):
    return bytes([kind]) + leb(len(data)) + data


def wasm(bits):
    memory = b"\x01" + (b"\x04" if bits == 64 else b"\x00") + b"\x00"
    return b"\x00asm\x01\x00\x00\x00" + section(5, memory)


class CaseFixtureTests(unittest.TestCase):
    def test_fixture_names_contents_and_escape_rejection(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            fixtures = root / "fixtures"
            fixtures.mkdir()
            (fixtures / "input.txt").write_bytes("café\r\n\r\n".encode())
            (fixtures / "empty.txt").write_text("")
            self.assertEqual(
                standard_input(TestMetadata(stdin_file="input.txt"), fixtures),
                "café\n\n",
            )
            self.assertIsNone(
                validate_output(TestMetadata(stdout_file="empty.txt"), "", "", fixtures)
            )
            difference = validate_output(
                TestMetadata(stdout_file="input.txt"), "wrong\n", "", fixtures
            )
            self.assertIn("--- expected stdout", difference)
            self.assertIn("+wrong", difference)
            for name in (
                "../input.txt",
                "/input.txt",
                "C:/input.txt",
                "C:input.txt",
                "..\\input.txt",
                "./input.txt",
                "missing.txt",
            ):
                with self.subTest(name=name), self.assertRaises(ValueError):
                    read_fixture(name, fixtures)
            (fixtures / "bad.txt").write_bytes(b"\xff")
            with self.assertRaisesRegex(ValueError, "UTF-8"):
                read_fixture("bad.txt", fixtures)
            escaped = root / "outside.txt"
            escaped.write_text("private sentinel")
            try:
                (fixtures / "link.txt").symlink_to(escaped)
            except OSError:
                pass
            else:
                with self.assertRaisesRegex(ValueError, "escapes"):
                    read_fixture("link.txt", fixtures)

    def test_non_utf8_process_output_fails_a_declared_output_contract(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            fixture = root / "tests/fixtures/empty.txt"
            fixture.parent.mkdir(parents=True)
            fixture.write_text("")
            command = [
                sys.executable,
                "-c",
                "import sys; sys.stdout.buffer.write(bytes([255]))",
            ]
            with (
                patch.object(run_tests, "ROOT", root),
                contextlib.redirect_stdout(io.StringIO()),
            ):
                status, detail = run_tests.classify_program(
                    "bad-output",
                    "case.wave",
                    command,
                    TestMetadata(stdout_file="empty.txt"),
                    None,
                )
            self.assertEqual(status, 0)
            self.assertEqual(detail["phase"], "output")
            self.assertIn("not UTF-8", detail["reason"])

    def test_environment_fixture_overlays_child_without_changing_parent(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            root.joinpath("env.json").write_text(
                '{"WAVE_TEST_ENV_TOKEN":"caf\u00e9", "WAVE_TEST_ENV_EMPTY":""}',
                encoding="utf-8",
            )
            metadata = TestMetadata(env_file="env.json")
            with patch.dict(
                os.environ, {"WAVE_TEST_ENV_AMBIENT": "preserved"}, clear=False
            ):
                before = dict(os.environ)
                environment = test_environment(metadata, root)
                self.assertEqual(dict(os.environ), before)
                self.assertEqual(environment["WAVE_TEST_ENV_AMBIENT"], "preserved")
                self.assertEqual(environment["WAVE_TEST_ENV_EMPTY"], "")
                result = subprocess.run(
                    [
                        sys.executable,
                        "-c",
                        "import os, sys; "
                        "sys.stdout.buffer.write(os.environ['WAVE_TEST_ENV_TOKEN'].encode('utf-8') + bytes([10]))",
                    ],
                    env=environment,
                    capture_output=True,
                    text=True,
                    encoding="utf-8",
                    check=True,
                )
                self.assertEqual(result.stdout, "café\n")
            for data in (
                "[]",
                "false",
                '{"a":2}',
                '{"a=b":"bad"}',
                '{"":"bad"}',
                '{"a":"\\u0000"}',
                "{not json}",
            ):
                root.joinpath("env.json").write_text(data)
                with self.subTest(data=data), self.assertRaises(ValueError):
                    test_environment(metadata, root)

    def test_fixture_parser_rejects_conflicts_and_compile_only_contracts(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "case.wave"
            for body in (
                "stdin=3, stdin-file=input.txt",
                "mode=check, stdout-file=empty.txt",
                "runner=server, stderr-file=empty.txt",
                "stdout-file=../bad.txt",
                "requires=unknown",
                "runtime=guess",
            ):
                with self.subTest(body=body), self.assertRaises(ValueError):
                    path.write_text(f"// wave-test: {body}\nfun main() {{}}\n")
                    parse_test_metadata(path)

    def test_output_failure_is_written_to_json_report_without_per_case_sleep(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            compiler = root / "wavec"
            fake_compiler(compiler)
            fixture = root / "tests/fixtures/empty.txt"
            fixture.parent.mkdir(parents=True)
            fixture.write_text("")
            report = root / "report.json"
            metadata = TestMetadata(mode="run", stdout_file="empty.txt")
            # Run the actual output validator/classifier; only compiler discovery
            # and subprocess launch are replaced so no Wave binary is required.
            with contextlib.ExitStack() as stack:
                stack.enter_context(patch.object(run_tests, "ROOT", root))
                stack.enter_context(
                    patch.object(
                        run_tests,
                        "iter_test_entries",
                        return_value=[("case", "case.wave")],
                    )
                )
                stack.enter_context(
                    patch.object(
                        run_tests, "command_for_test", return_value=[str(compiler)]
                    )
                )
                stack.enter_context(
                    patch.object(
                        run_tests, "parse_test_metadata", return_value=metadata
                    )
                )
                stack.enter_context(
                    patch.object(
                        run_tests, "manifest_compile_target", return_value=None
                    )
                )
                stack.enter_context(
                    patch.object(
                        run_tests, "report_selection", return_value={"target": "test"}
                    )
                )
                stack.enter_context(
                    patch.object(
                        run_tests,
                        "run_and_classify",
                        side_effect=lambda name, path, cmd: run_tests.classify_program(
                            name, path, cmd, metadata, None
                        ),
                    )
                )
                stack.enter_context(
                    patch.object(
                        run_tests,
                        "run_process",
                        return_value=subprocess.CompletedProcess(
                            [], 0, "unexpected\n", ""
                        ),
                    )
                )
                sleep = stack.enter_context(
                    patch.object(
                        run_tests.time,
                        "sleep",
                        side_effect=AssertionError("ordinary cases must not sleep"),
                    )
                )
                stack.enter_context(contextlib.redirect_stdout(io.StringIO()))
                with self.assertRaises(SystemExit) as error:
                    run_tests.main(
                        [
                            "--wavec",
                            str(compiler),
                            "--suite",
                            "shared",
                            "--report-json",
                            str(report),
                        ]
                    )
                self.assertEqual(error.exception.code, 1)
                sleep.assert_not_called()
            row = json.loads(report.read_text())["tests"][0]
            self.assertEqual(row["status"], "fail")
            self.assertEqual(row["phase"], "output")
            self.assertIn("fixture mismatch", row["reason"])
            self.assertEqual(row["stdout"], "unexpected\n")


class WasmArtifactTests(unittest.TestCase):
    def test_memory_width_magic_and_truncation_contracts(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            metadata = TestMetadata(mode="build", runner="compile")
            artifact = root / "case/case.o"
            artifact.parent.mkdir()
            for bits in (32, 64):
                artifact.write_bytes(wasm(bits))
                self.assertEqual(read_wasm_contract(artifact), ({"bits": bits}, None))
                for expected in (32, 64):
                    error = validate_compiled_artifact(
                        "case",
                        root / "case.wave",
                        root,
                        metadata,
                        target=f"wasm{expected}-unknown-unknown",
                    )
                    self.assertEqual(error is None, expected == bits, error)
                    if error:
                        self.assertIn("address width mismatch", error)
            malformed = [
                b"",
                b"\x7fELF" + bytes(64),
                b"\x00asm\x02\0\0\0",
                wasm(64)[:-1],
                wasm(64) + section(5, b"\x00"),
                b"\x00asm\x01\0\0\0" + section(5, b"\x01\x80\x80\x80\x80\x80\x00"),
            ]
            for body in malformed:
                artifact.write_bytes(body)
                contract, error = read_wasm_contract(artifact)
                self.assertIsNone(contract)
                self.assertIsNotNone(error)

    def test_imported_memory_and_memory64_feature_cannot_disagree(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "case.o"
            feature = b"\x0ftarget_features\x01+\x08memory64"
            imported = b"\x01\x03env\x06memory\x02\x04\x00"
            path.write_bytes(
                b"\x00asm\x01\0\0\0" + section(2, imported) + section(0, feature)
            )
            self.assertEqual(read_wasm_contract(path), ({"bits": 64}, None))
            path.write_bytes(wasm(32) + section(0, feature))
            self.assertIn("inconsistent", read_wasm_contract(path)[1])


class CaseShardTests(unittest.TestCase):
    def test_shards_partition_the_same_selection_once_with_original_paths(self):
        entries = [
            (f"shared/test{n}.wave", f"tests/cases/shared/test{n}.wave")
            for n in range(1, 18)
        ]
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            compiler = root / "wavec"
            fake_compiler(compiler)
            selections = []
            for index in range(3):
                report = root / f"shard-{index}.json"
                with (
                    patch.object(run_tests, "iter_test_entries", return_value=entries),
                    patch.object(
                        run_tests, "command_for_test", return_value=[str(compiler)]
                    ),
                    patch.object(run_tests, "run_and_classify", return_value=(1, None)),
                    patch.object(
                        run_tests,
                        "compiler_default_target",
                        return_value="x86_64-unknown-linux-gnu",
                    ),
                    contextlib.redirect_stdout(io.StringIO()),
                ):
                    run_tests.main(
                        [
                            "--wavec",
                            str(compiler),
                            "--suite",
                            "shared",
                            "--shard-index",
                            str(index),
                            "--shard-count",
                            "3",
                            "--report-json",
                            str(report),
                        ]
                    )
                data = json.loads(report.read_text())
                selected = [row["name"] for row in data["tests"]]
                self.assertEqual(selected, [name for name, _ in entries[index::3]])
                selections.extend(selected)
                self.assertEqual(data["selection"]["shard_index"], index)
                self.assertEqual(data["selection"]["shard_count"], 3)
            self.assertCountEqual(selections, [name for name, _ in entries])
            self.assertEqual(len(selections), len(set(selections)))

    def test_explicit_empty_shard_fails_without_running_or_overwriting_a_report(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            compiler = root / "wavec"
            fake_compiler(compiler)
            report = root / "report.json"
            report.write_text("previous report sentinel", encoding="utf-8")
            entry = ("shared/test1.wave", "tests/cases/shared/test1.wave")
            stderr = io.StringIO()
            with (
                patch.object(run_tests, "iter_test_entries", return_value=[entry]),
                patch.object(run_tests, "run_and_classify") as run,
                contextlib.redirect_stdout(io.StringIO()),
                contextlib.redirect_stderr(stderr),
                self.assertRaises(SystemExit) as error,
            ):
                run_tests.main(
                    [
                        "--wavec",
                        str(compiler),
                        "--suite",
                        "shared",
                        "--only",
                        entry[0],
                        "--shard-index",
                        "1",
                        "--shard-count",
                        "2",
                        "--report-json",
                        str(report),
                    ]
                )
            self.assertEqual(error.exception.code, 2)
            self.assertIn("no tests selected for shard 1/2", stderr.getvalue())
            run.assert_not_called()
            self.assertEqual(
                report.read_text(encoding="utf-8"), "previous report sentinel"
            )

    def test_every_ci_target_has_unique_shards_and_runtime_uses_matching_partition(
        self,
    ):
        manifest = case_manifest.load_case_manifest()
        for group, matrix in manifest.github_matrices().items():
            entries = matrix["include"]
            self.assertEqual(
                len({(row["id"], row["shard_index"]) for row in entries}), len(entries)
            )
            for row in entries:
                self.assertEqual(row["shard_count"], 2)
        runtime = ["shared/test1.wave", "shared/test2.wave", "shared/test3.wave"]
        runner = SimpleNamespace(
            target=SimpleNamespace(id="wasm-unknown", executor="wasm", host_os="linux"),
            context={"matrix.shard_index": 1, "matrix.shard_count": 2},
            run=Mock(side_effect=["\n".join(runtime), ""]),
        )
        ci_test.selected_runtime(runner, {})
        command = runner.run.call_args_list[-1].args[0]
        self.assertEqual(command[command.index("--sources") + 1 :], [runtime[1]])
        self.assertIn("wave-cases-wasm-unknown-shard-1-runtime.json", command)

    def test_capability_selection_is_metadata_driven_for_native_qemu_bare_and_wasi(
        self,
    ):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            headers = [
                "runtime=portable",
                "runtime=portable, requires=filesystem",
                "runtime=native",
                "runtime=compile",
            ]
            for number, header in enumerate(headers, 1):
                path = root / f"shared/test{number}.wave"
                path.parent.mkdir(exist_ok=True)
                path.write_text(f"// wave-test: {header}\nfun main() {{}}\n")
            for executor, os_name, caps, expected in [
                ("native", "linux", ("filesystem",), [1, 2, 3]),
                ("qemu", "linux", ("filesystem",), [1, 2, 3]),
                ("wasm", "wasm", (), [1]),
                ("wasm", "wasi", ("filesystem",), [1, 2]),
            ]:
                target = case_manifest.CaseTarget(
                    id="target",
                    os=os_name,
                    arch="amd64",
                    suite="shared",
                    status="supported",
                    enabled=True,
                    ci=True,
                    executor=executor,
                    suites=("shared",),
                    exclude=(),
                    capabilities=caps,
                )
                with patch.object(case_manifest, "CASES_ROOT", root):
                    selected = case_manifest.CaseManifest(2, (target,)).runtime_sources(
                        "target"
                    )
                self.assertEqual(
                    selected, tuple(f"shared/test{n}.wave" for n in expected)
                )


if __name__ == "__main__":
    unittest.main()
