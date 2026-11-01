# SPDX-License-Identifier: MPL-2.0
"""Fail-closed behavior of the native MSVC execution gate."""
import argparse
import json
from pathlib import Path
import struct
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

from tools.check_msvc_native import Audit, pe_machine


class NativeGateTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="wave-msvc-gate-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)

    def audit(self):
        audit = Audit(argparse.Namespace(
            output=self.root / "audit", wavec=self.root / "missing-wavec.exe",
            llvm_bin=self.root / "missing-llvm", target="aarch64-pc-windows-msvc"))
        self.addCleanup(audit.profile.cleanup)
        return audit

    def test_unavailable_native_environment_fails_and_preserves_sources(self):
        audit = self.audit()
        with self.assertRaisesRegex(RuntimeError, "native MSVC checks failed"):
            audit.run()
        result = json.loads((audit.output / "result.json").read_text())
        self.assertFalse(result["passed"])
        self.assertEqual(result["failures"][0]["case"], "prerequisites")
        self.assertTrue((audit.output / "sources/abi.c").is_file())
        self.assertTrue((audit.output / "sources/abi.wave").is_file())

    def test_isolated_profile_is_outside_the_uploadable_evidence(self):
        audit = self.audit()
        self.addCleanup(audit.profile.cleanup)
        home = Path(audit.env["USERPROFILE"])
        self.assertEqual(audit.env["HOME"], str(home))
        self.assertFalse(home.is_relative_to(audit.output))
        self.assertTrue((home / ".wave/lib/wave/std").is_dir())
        self.assertFalse((audit.output / "home").exists())

    def test_child_exit_and_diagnostic_are_preserved_on_failure(self):
        audit = self.audit()
        with self.assertRaisesRegex(AssertionError, "expected exit 0, got 37"):
            audit.command([sys.executable, "-c", "import sys; print('fixture failure'); sys.exit(37)"], self.root)
        log = json.loads((audit.output / "commands.json").read_text())[-1]
        self.assertEqual(log["exit"], 37)
        self.assertEqual(log["stdout"].strip(), "fixture failure")

    def test_prerequisite_timeouts_preserve_result_and_partial_output(self):
        for index in (0, 1):
            with self.subTest(probe=index):
                audit = Audit(argparse.Namespace(
                    output=self.root / f"timeout-{index}", wavec=self.root / "wavec.exe",
                    llvm_bin=self.root / "llvm", target="aarch64-pc-windows-msvc"))
                self.addCleanup(audit.profile.cleanup)
                commands = []

                def execute(args, **kwargs):
                    commands.append(args)
                    if len(commands) == index + 1:
                        raise subprocess.TimeoutExpired(args, 90, output=b"partial probe output")
                    return subprocess.CompletedProcess(args, 0, "version", "")

                with patch("tools.check_msvc_native.require_native"), \
                     patch("tools.check_msvc_native.run_process", side_effect=execute), \
                     patch.object(audit, "case") as scenario:
                    with self.assertRaisesRegex(RuntimeError, "native MSVC checks failed"):
                        audit.run()
                    scenario.assert_not_called()
                result = json.loads((audit.output / "result.json").read_text())
                self.assertFalse(result["passed"])
                self.assertEqual(result["target"], "aarch64-pc-windows-msvc")
                self.assertEqual(result["failures"][0]["case"], "prerequisites")
                self.assertIn("90", result["failures"][0]["error"])
                log = json.loads((audit.output / "commands.json").read_text())
                self.assertEqual(len(log), index + 1)
                self.assertEqual(log[-1]["command"], commands[-1])
                self.assertTrue(log[-1]["timeout"])
                self.assertIn("partial probe output", log[-1]["output"])

    def test_unrelated_negative_failure_does_not_count_as_expected_diagnostic(self):
        audit = self.audit()
        with self.assertRaisesRegex(AssertionError, "missing diagnostic"):
            audit.command([sys.executable, "-c", "raise SystemExit(1)"], self.root,
                          expected=None, diagnostic="wave_missing_runtime_helper")
        with self.assertRaisesRegex(AssertionError, "unexpectedly succeeded"):
            audit.command([sys.executable, "-c", "pass"], self.root, expected=None)

    def test_response_fixture_uses_windows_safe_unicode_paths_and_long_arguments(self):
        audit = self.audit()
        directory = audit.output / "response"
        directory.mkdir()
        commands = []

        class ReadyToLink(Exception):
            pass

        def compile_only(args, cwd, **kwargs):
            commands.append(list(map(str, args)))
            if len(commands) == 1:
                self.assertIn("/MD", args)
                self.assertNotIn("/MT", args)
                source = next(Path(arg) for arg in args if str(arg).endswith("empty.c"))
                self.assertTrue(source.is_file())
                self.assertIn("한글", str(source))
                for part in source.relative_to(directory).parts:
                    self.assertEqual(part, part.rstrip(" ."))
                output = next(str(arg)[3:] for arg in args if str(arg).startswith("/Fo"))
                Path(output).write_bytes(b"fixture object")
            else:
                raise ReadyToLink

        with patch.object(audit, "command", side_effect=compile_only):
            with self.assertRaises(ReadyToLink):
                audit.response(directory, audit.linker)
        objects = [Path(arg) for arg in commands[1] if arg.endswith(".obj")]
        self.assertEqual(len(objects), 64)
        self.assertTrue(all(path.is_file() for path in objects))
        argument_units = len(" ".join(commands[1]).encode("utf-16-le")) // 2
        self.assertGreater(argument_units, 8192)
        self.assertLess(argument_units, 32767)

    def test_pe_machine_requires_an_image_and_valid_header_offset(self):
        image = self.root / "arm64.exe"
        data = bytearray(134)
        data[:2] = b"MZ"
        struct.pack_into("<I", data, 60, 128)
        data[128:132] = b"PE\0\0"
        struct.pack_into("<H", data, 132, 0xAA64)
        image.write_bytes(data)
        self.assertEqual(pe_machine(image), 0xAA64)
        for bad in (data[:133], b"not an executable", data[:60] + b"\xff" * 4 + data[64:]):
            image.write_bytes(bad)
            with self.assertRaises(ValueError):
                pe_machine(image)


class NativeDecisionTests(unittest.TestCase):
    def native(
        self,
        *,
        target="x86_64-pc-windows-msvc",
        machines=None,
        tools=None,
        environment=None,
        api_error=None,
    ):
        from tools import check_msvc_native as native

        expected = native.TARGETS[target]
        variables = {
            "LIB": "sdk-lib",
            "INCLUDE": "sdk-include",
            "VCToolsInstallDir": "vc-tools",
        }
        if environment is not None:
            for key, value in environment.items():
                if value is None:
                    variables.pop(key, None)
                else:
                    variables[key] = value
        with (
            patch.object(
                native,
                "windows_process_machines",
                return_value=machines or (0, expected),
                side_effect=api_error,
            ),
            patch.object(
                native, "pe_machine", side_effect=tools or [expected, expected]
            ),
            patch.dict(native.os.environ, variables, clear=True),
        ):
            return native.require_native(
                target, Path("wavec.exe"), Path("clang-cl.exe")
            )

    def test_matching_native_x64_and_arm64_succeed_without_windows_api_or_sdk(self):
        for target in ("x86_64-pc-windows-msvc", "aarch64-pc-windows-msvc"):
            with self.subTest(target=target):
                self.assertIsNone(self.native(target=target))

    def test_emulation_and_host_mismatch_are_distinct_native_rejections(self):
        with self.assertRaisesRegex(RuntimeError, "process=0x14c, host=0x8664"):
            self.native(machines=(0x14C, 0x8664))
        with self.assertRaisesRegex(RuntimeError, "process=0x0, host=0xaa64"):
            self.native(machines=(0, 0xAA64))

    def test_each_tool_machine_is_checked_independently(self):
        for tools, name in (
            ([0xAA64], "wavec.exe"),
            ([0x8664, 0xAA64], "clang-cl.exe"),
        ):
            with (
                self.subTest(tool=name),
                self.assertRaisesRegex(RuntimeError, f"tool is not native.*{name}"),
            ):
                self.native(tools=tools)

    def test_missing_or_empty_sdk_fields_have_the_specific_cause(self):
        for field in ("LIB", "INCLUDE", "VCToolsInstallDir"):
            for value in (None, ""):
                with (
                    self.subTest(field=field, value=value),
                    self.assertRaisesRegex(RuntimeError, field),
                ):
                    self.native(environment={field: value})

    def test_windows_api_failure_propagates_its_useful_cause(self):
        with self.assertRaisesRegex(OSError, "IsWow64Process2: API unavailable"):
            self.native(api_error=OSError("IsWow64Process2: API unavailable"))


if __name__ == "__main__":
    unittest.main()
