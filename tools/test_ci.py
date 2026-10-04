# SPDX-License-Identifier: MPL-2.0
"""Failure-path and artifact-contract tests for local/CI shared procedures."""

import contextlib
import hashlib
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tarfile
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import Mock, patch

from tools.ci import common, targets, build, package, release, test as ci_test


class PlatformSmokeTests(unittest.TestCase):
    def test_windows_object_reports_compiler_failure_even_with_existing_artifact(self):
        with tempfile.TemporaryDirectory() as directory:
            artifact = Path(directory) / "default-object" / "test1.o"
            artifact.parent.mkdir()
            artifact.write_bytes(b"\x64\xaa")
            runner = SimpleNamespace(
                target=targets.resolve("windows-arm64"),
                temp=Path(directory),
                run=Mock(side_effect=RuntimeError("command exited 1, expected 0")),
            )
            with patch.object(ci_test, "compiler", return_value=Path("wavec.exe")):
                with self.assertRaisesRegex(RuntimeError, "command exited 1"):
                    ci_test.windows_object(runner, {"explicit": False})

    def test_cross_package_smoke_passes_linker_sysroot_in_isolated_environment(self):
        from tools.ci.llvm_bundle import MACHINES

        for target_id, machine in MACHINES.items():
            with self.subTest(target=target_id), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                payload = root / "payload"
                payload.mkdir()
                header = bytearray(20)
                header[:6] = b"\x7fELF\x02\x01"
                header[18:20] = machine.to_bytes(2, "little")
                (payload / "wavec").write_bytes(header)
                archive = root / "package.tar.gz"
                with tarfile.open(archive, "w:gz") as stream:
                    stream.add(payload, arcname="package")
                sysroot = str(root / "target sysroot")
                runner = SimpleNamespace(
                    target=targets.resolve(target_id),
                    env={"WAVE_CROSS_SYSROOT": sysroot, "QEMU_LD_PREFIX": sysroot,
                         "LLVM_SYS_211_PREFIX": "/build-only/llvm"},
                    run=Mock(side_effect=["[$ORIGIN/llvm/lib]", "0.2.1", "release smoke\n"]),
                )
                with patch.object(package, "archive_for", return_value=archive), \
                     patch.object(ci_test, "crt_check"), patch.object(package, "checksums"):
                    package.package_smoke(runner, {})
                invocation = runner.run.call_args_list[-1]
                command = invocation.args[0]
                self.assertEqual(command[command.index("--sysroot") + 1], sysroot)
                self.assertTrue(invocation.kwargs["clean_env"])
                self.assertEqual(invocation.kwargs["env"]["QEMU_LD_PREFIX"], sysroot)
                self.assertEqual(invocation.kwargs["env"]["PATH"], "/usr/bin:/bin")
                self.assertNotIn("LLVM_SYS_211_PREFIX", invocation.kwargs["env"])

    def test_windows_probe_requires_only_the_enabled_cargo_backends(self):
        for target_id, architecture, backends in [
            ("windows-amd64", "AMD64", "X86 AArch64 RISCV"),
            ("windows-arm64", "ARM64", "AArch64"),
        ]:
            with self.subTest(target=target_id):
                target = targets.resolve(target_id)
                runner = SimpleNamespace(
                    target=target,
                    env={"PROCESSOR_ARCHITECTURE": architecture},
                    run=Mock(
                        side_effect=[f"host: {target.triple}", "21.1.8", backends]
                    ),
                )
                build.windows_host(runner, {})
                runner.run.side_effect = [f"host: {target.triple}", "21.1.8", "X86"]
                with self.assertRaisesRegex(ValueError, "missing LLVM backends"):
                    build.windows_host(runner, {})

    def test_wasi_smoke_uses_the_same_controlled_filesystem_for_both_runners(self):
        with tempfile.TemporaryDirectory() as directory:
            runner = SimpleNamespace(
                temp=Path(directory), run=Mock(return_value="Format: WASM")
            )
            with patch.object(
                ci_test, "compiler", return_value=Path("/compiler/wavec")
            ):
                ci_test.wasm_smoke(runner, {})
            calls = runner.run.call_args_list
            host = next(
                c
                for c in calls
                if common.ROOT / "tools/run_wasi_smoke.mjs" in c.args[0]
            )
            command = next(
                c
                for c in calls
                if c.args[0][1] == "run" and "wasm32-wasip1" in c.args[0]
            )
            preopen = Path(host.args[0][-1])
            self.assertEqual(command.kwargs["cwd"], preopen)
            self.assertTrue(Path(command.args[0][2]).is_absolute())
            self.assertEqual((preopen / "README.md").read_bytes()[:2], b"# ")
            self.assertNotEqual(preopen, common.ROOT)

    @unittest.skipUnless(shutil.which("node"), "Node.js is required for the WASI host")
    def test_wasi_host_propagates_guest_failure_status(self):
        # (module (import "wasi_snapshot_preview1" "proc_exit" (func (param i32)))
        #   (memory (export "memory") 1)
        #   (func (export "_start") i32.const 7 call 0))
        module = bytes.fromhex(
            "0061736d0100000001080260017f0060000002240116"
            "776173695f736e617073686f745f707265766965773109"
            "70726f635f657869740000030201010503010001071302"
            "066d656d6f72790200065f737461727400010a08010600410710000b"
        )
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "exit.wasm"
            keepalive = Path(directory) / "keepalive.cjs"
            keepalive.write_text("setInterval(() => {}, 1000);", encoding="utf-8")
            cases = [
                ("proc_exit_failure", module, 7),
                ("proc_exit_success", module.replace(bytes.fromhex("410710000b"), bytes.fromhex("410010000b")), 0),
                ("start_returns", module.replace(bytes.fromhex("0a08010600410710000b"), bytes.fromhex("0a040102000b")), 0),
                ("start_traps", module.replace(bytes.fromhex("0a08010600410710000b"), bytes.fromhex("0a05010300000b")), 1),
                ("invalid_module", b"not a WebAssembly module", 1),
            ]
            for name, contents, expected in cases:
                with self.subTest(case=name):
                    path.write_bytes(contents)
                    result = subprocess.run(
                        [
                            "node",
                            "--no-warnings",
                            "--require", str(keepalive),
                            str(common.ROOT / "tools/run_wasi_smoke.mjs"),
                            str(path),
                            directory,
                        ],
                        stdin=subprocess.DEVNULL,
                        capture_output=True,
                        text=True,
                        timeout=30,
                    )
                    self.assertEqual(result.returncode, expected, result.stderr)



class TargetTests(unittest.TestCase):
    def test_every_manifest_target_has_a_compatible_plan(self):
        from tools.case_manifest import load_case_manifest

        plans, procedures = common.inventory()
        for case in load_case_manifest().targets:
            if not case.enabled:
                continue
            target = targets.resolve(case.id)
            if case.target is not None:
                self.assertEqual(target.triple, case.target)
            for lane in targets.lanes(target, "test"):
                targets.check_lane(target, lane)
                self.assertIn(lane, plans)
        self.assertEqual(
            targets.resolve("riscv64-linux").rust_target, "riscv64gc-unknown-linux-gnu"
        )
        self.assertEqual(targets.resolve("wasm64").triple, "wasm64-unknown-unknown")
        self.assertEqual(len(plans), 30)
        self.assertEqual(sum(len(p["stages"]) for p in plans.values()), 226)
        handlers = build.OPERATIONS | package.OPERATIONS | release.OPERATIONS
        from tools.ci.test import OPERATIONS

        handlers.update(OPERATIONS)
        for plan in plans.values():
            ids = [s["id"] for s in plan["stages"]]
            self.assertEqual(len(ids), len(set(ids)))
            for stage in plan["stages"]:
                operation = procedures[stage["procedure"]]
                self.assertTrue(stage["phase"])
                self.assertGreater(stage["timeout_seconds"], 0)
                if operation["operation"] != "commands":
                    self.assertIn(operation["operation"], handlers)

    def test_invalid_targets_lanes_and_publication_fail_before_execution(self):
        with (
            patch.object(common, "Runner") as runner,
            contextlib.redirect_stderr(io.StringIO()),
        ):
            for family, args in [
                ("test", ["--target", "missing"]),
                (
                    "test",
                    ["--target", "linux-amd64", "--lane", "rust/build-windows-arm64"],
                ),
                ("release", ["--target", "wasm64"]),
                (
                    "test",
                    [
                        "--target",
                        "linux-amd64",
                        "--lane",
                        "release/publish",
                        "--publish",
                    ],
                ),
            ]:
                self.assertEqual(common.main(family, args), 2)
            runner.assert_not_called()

    def test_plan_is_side_effect_free_and_complete(self):
        with (
            patch.object(common, "Runner") as runner,
            contextlib.redirect_stdout(io.StringIO()) as stream,
        ):
            self.assertEqual(
                common.main("test", ["--target", "linux-riscv64", "--plan"]), 0
            )
            data = json.loads(stream.getvalue())
            self.assertEqual(
                [lane["name"] for lane in data["lanes"]],
                ["rust/build-linux-riscv64", "cases/cases-riscv64"],
            )
            runner.assert_not_called()

    def test_expression_language_rejects_code_and_tracks_dependencies(self):
        context = {"steps.llvm_setup.outcome": "failure"}
        self.assertFalse(
            common.evaluate(
                "${{ !cancelled() && steps.llvm_setup.outcome == 'success' }}", context
            )
        )
        self.assertTrue(common.evaluate("${{ always() }}", context))
        for invalid in [
            "__import__('os').system('false')",
            "[x for x in []]",
            "open('/tmp/no')",
        ]:
            with self.assertRaises(ValueError):
                common.evaluate(invalid, {})


class RunnerTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="ci space ")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.runner = common.Runner(
            targets.resolve("linux-amd64"),
            self.root / "report.json",
            environment={
                "PATH": os.environ.get("PATH", ""),
                "RUNNER_TEMP": str(self.root),
            },
        )

    def plan(self, operations):
        steps = []
        procedures = {}
        for i, (op, condition) in enumerate(operations):
            steps.append(
                dict(
                    id=str(i),
                    name=op,
                    condition=condition,
                    procedure=str(i),
                    phase="validation",
                    timeout_seconds=10,
                )
            )
            procedures[str(i)] = {"operation": op}
        return {"fixture": {"env": {}, "stages": steps}}, procedures

    def test_failure_preserves_status_and_independent_evidence(self):
        operations = [("fail", ""), ("blocked", ""), ("independent", "${{ always() }}")]
        called = []

        def fail(*_):
            raise RuntimeError("compiler failed")

        with (
            patch.object(common, "inventory", return_value=self.plan(operations)),
            contextlib.redirect_stdout(io.StringIO()),
            contextlib.redirect_stderr(io.StringIO()),
        ):
            status = self.runner.execute(
                ["fixture"],
                operations={
                    "fail": fail,
                    "blocked": lambda *_: self.fail("dependent stage ran"),
                    "independent": lambda *_: called.append(True),
                },
            )
        self.assertEqual(status, 1)
        self.assertEqual(called, [True])
        self.assertEqual(
            [s["status"] for s in json.loads(self.runner.report.read_text())["stages"]],
            ["fail", "not_run", "pass"],
        )

    def test_cancellation_preserves_completed_and_unstarted_work(self):
        def stop(*_):
            raise KeyboardInterrupt()

        with (
            patch.object(
                common,
                "inventory",
                return_value=self.plan([("pass", ""), ("stop", ""), ("never", "")]),
            ),
            contextlib.redirect_stdout(io.StringIO()),
        ):
            status = self.runner.execute(
                ["fixture"], operations={"pass": lambda *_: None, "stop": stop}
            )
        self.assertEqual(status, 130)
        self.assertEqual(
            [s["status"] for s in json.loads(self.runner.report.read_text())["stages"]],
            ["pass", "interrupted", "not_run"],
        )

    def test_ci_cancellation_is_not_a_successful_or_keyboard_interrupted_stage(self):
        def cancel(*_):
            raise common.Cancelled()

        with (
            patch.object(
                common,
                "inventory",
                return_value=self.plan([("cancel", ""), ("never", "")]),
            ),
            contextlib.redirect_stdout(io.StringIO()),
        ):
            status = self.runner.execute(["fixture"], operations={"cancel": cancel})
        self.assertEqual(status, 143)
        data = json.loads(self.runner.report.read_text())
        self.assertEqual(
            [row["status"] for row in data["stages"]], ["cancelled", "not_run"]
        )
        self.assertEqual(data["summary"]["cancelled"], 1)

    def test_child_timeout_and_launch_failure_are_distinct(self):
        for failure, status in [
            (subprocess.TimeoutExpired(["tool"], 1, output="partial"), "timeout"),
            (FileNotFoundError("missing tool"), "unavailable"),
        ]:
            with self.subTest(status=status):
                plans, procedures = self.plan([("commands", "")])
                procedures["0"]["commands"] = [["tool"]]
                with (
                    patch.object(common, "inventory", return_value=(plans, procedures)),
                    patch.object(common, "run_process", side_effect=failure),
                    contextlib.redirect_stdout(io.StringIO()),
                    contextlib.redirect_stderr(io.StringIO()),
                ):
                    self.runner.data["stages"] = []
                    self.assertEqual(self.runner.execute(["fixture"]), 1)
                row = json.loads(self.runner.report.read_text())["stages"][0]
                self.assertEqual(row["status"], status)
                self.assertEqual(row["commands"][0]["status"], status)

    def test_provisioning_requires_explicit_flag_before_any_command(self):
        plans, procedures = self.plan([("commands", "")])
        procedures["0"].update(requires_provision=True, commands=[["install"]])
        with (
            patch.object(common, "inventory", return_value=(plans, procedures)),
            patch.object(self.runner, "run") as run,
            contextlib.redirect_stdout(io.StringIO()),
            contextlib.redirect_stderr(io.StringIO()),
        ):
            self.assertEqual(self.runner.execute(["fixture"]), 1)
            run.assert_not_called()

    def test_report_cannot_replace_source_even_through_alias(self):
        source = common.ROOT / "src/main.rs"
        for report in [source, self.root / "alias"]:
            if report != source:
                try:
                    report.symlink_to(source)
                except OSError:
                    continue
            with self.assertRaisesRegex(ValueError, "aliases an input"):
                common.Runner(targets.resolve("linux-amd64"), report)

    def test_std_setup_preserves_user_installation(self):
        home = self.root / "real home"
        installed = home / ".wave/lib/wave/std"
        installed.mkdir(parents=True)
        (installed / "sentinel").write_text("original")
        self.runner.env["HOME"] = str(home)
        build.stdlib(self.runner, {})
        self.assertEqual((installed / "sentinel").read_text(), "original")
        self.assertNotEqual(self.runner.env["HOME"], str(home))
        copied = Path(self.runner.env["HOME"]) / ".wave/lib/wave/std/manifest.json"
        self.assertEqual(
            copied.read_bytes(), (common.ROOT / "std/manifest.json").read_bytes()
        )


class MetadataTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.version = "0.2.1-pre-beta"
        self.source = "a" * 40
        self.names = []
        for target in release.ARCHIVE_TARGETS:
            name = f"wave-v{self.version}-{target}" + (
                ".zip" if "windows" in target else ".tar.gz"
            )
            self.names.append(name)
            (self.root / name).write_bytes(name.encode())
            digest = hashlib.sha256(name.encode()).hexdigest()
            (self.root / (name + ".sha256")).write_text(f"{digest}  {name}\n")
            metadata = {
                "schema_version": 1,
                "compiler_version": self.version,
                "source_sha": self.source,
                "target": target,
                "archive": name,
                "sha256": digest,
                "std_compatibility_revision": 4,
                **package.artifact_contract(target),
            }
            (self.root / (name + ".metadata.json")).write_text(json.dumps(metadata))

    def test_complete_and_mismatched_package_identities(self):
        release.verify_metadata(self.root, self.version, self.source)
        path = self.root / (self.names[0] + ".metadata.json")
        original = json.loads(path.read_text())
        for key, value in [
            ("schema_version", 2),
            ("source_sha", "b" * 40),
            ("compiler_version", "0.0.0"),
            ("target", "wrong"),
            ("std_compatibility_revision", 5),
            ("sha256", "0" * 64),
            ("archive", "bad"),
        ]:
            with self.subTest(key=key):
                path.write_text(json.dumps(dict(original, **{key: value})))
                with self.assertRaises(ValueError):
                    release.verify_metadata(self.root, self.version, self.source)
        path.write_text(json.dumps(original))
        path.unlink()
        with self.assertRaisesRegex(ValueError, "metadata"):
            release.verify_metadata(self.root, self.version, self.source)

    def test_no_publication_without_authorization_or_valid_identity(self):
        runner = SimpleNamespace(
            publish_authorized=False, run=Mock(), env={}, context={}
        )
        with self.assertRaises(ValueError):
            release.publish(runner, {})
        runner.run.assert_not_called()


class PackageTests(unittest.TestCase):
    def test_archive_triplet_rolls_back_each_failed_replace(self):
        for failure in range(1, 7):
            with (
                self.subTest(failure=failure),
                tempfile.TemporaryDirectory() as directory,
            ):
                root = Path(directory)
                candidates = []
                for index in range(3):
                    source = root / f"candidate{index}"
                    destination = root / f"published{index}"
                    source.write_text("new")
                    destination.write_text("old")
                    candidates.append((source, destination))
                original = os.replace
                count = 0

                def replace(source, destination):
                    nonlocal count
                    count += 1
                    if count == failure:
                        raise OSError("injected replacement failure")
                    return original(source, destination)

                with (
                    patch.object(package.os, "replace", side_effect=replace),
                    self.assertRaises(OSError),
                ):
                    package.replace_set(candidates)
                self.assertEqual([p.read_text() for _, p in candidates], ["old"] * 3)

    def test_all_distribution_layouts_include_matching_std_and_identity(self):
        import shutil, zipfile

        with tempfile.TemporaryDirectory(prefix="package layout ") as directory:
            root = Path(directory)
            (root / "std").mkdir()
            (root / "std/manifest.json").write_text('{"compatibility_revision":5}')
            (root / "std/sample.wave").write_text("fun sample() {}")
            (root / "std/LICENSE").write_text("std license")
            (root / "LICENSE").write_text("Wave license")
            selected = [
                t.rust_target for t in targets.TARGETS.values() if t.distribution
            ]
            for target in selected:
                binary = (
                    root
                    / "target"
                    / target
                    / "release"
                    / ("wavec.exe" if "windows" in target else "wavec")
                )
                binary.parent.mkdir(parents=True)
                binary.write_bytes(b"compiler fixture")

            def tools(stage, target):
                folder = stage / "llvm/bin"
                folder.mkdir(parents=True)
                (folder / "ld.lld").write_bytes(b"linker fixture")
                return []

            legacy = SimpleNamespace(
                ROOT=root,
                DIST_DIR=root / "dist",
                TARGET_DIR=root / "target",
                TARGETS=selected,
                NAME="wave",
                VERSION="0.2.1-pre-beta",
                release_target_name=lambda t: t.replace(
                    "riscv64gc-", "riscv64-"
                ).replace("-unknown-linux", "-linux"),
                copy_executable=shutil.copy2,
                copy_lld_tools=tools,
                write_linux_crt_objects=Mock(),
                copy_llvm_runtime_libs=Mock(return_value=["fixture-library"]),
                is_windows_target=lambda t: "windows" in t,
                copy_windows_msvc_resources=Mock(),
                patch_staged_runtime=Mock(),
                verify_packaged_runtime_arch=Mock(),
            )
            with (
                patch.object(
                    package.subprocess,
                    "run",
                    return_value=SimpleNamespace(stdout="a" * 40),
                ),
                contextlib.redirect_stdout(io.StringIO()),
            ):
                package.package_targets(legacy)
            self.assertEqual(legacy.verify_packaged_runtime_arch.call_count, 9)
            for target in selected:
                name = (
                    "wave-v" + legacy.VERSION + "-" + legacy.release_target_name(target)
                )
                archive = root / (name + (".zip" if "windows" in target else ".tar.gz"))
                if archive.suffix == ".zip":
                    with zipfile.ZipFile(archive) as opened:
                        names = opened.namelist()
                        std = opened.read(name + "/std/manifest.json")
                else:
                    with tarfile.open(archive) as opened:
                        names = opened.getnames()
                        std = opened.extractfile(name + "/std/manifest.json").read()
                        self.assertFalse(
                            any(p.issym() or p.islnk() for p in opened.getmembers())
                        )
                self.assertIn(name + "/std/sample.wave", names)
                self.assertEqual(json.loads(std)["compatibility_revision"], 5)
                metadata = json.loads(
                    archive.with_name(archive.name + ".metadata.json").read_text()
                )
                self.assertEqual(metadata["target"], target)
                self.assertEqual(metadata["source_sha"], "a" * 40)
                self.assertEqual(
                    metadata["sha256"], hashlib.sha256(archive.read_bytes()).hexdigest()
                )


class ReleaseIdentityTests(unittest.TestCase):
    def test_dev_mismatched_and_existing_versions_fail_before_build(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "Cargo.toml").write_text('[package]\nversion="0.2.1-pre-beta"\n')
            for version, tag in [
                ("0.2.1-pre-beta-dev", ""),
                ("0.2.2-pre-beta", ""),
                ("0.2.1-pre-beta", "tag exists"),
            ]:
                runner = SimpleNamespace(
                    env={"RELEASE_VERSION": version},
                    run=Mock(return_value=tag),
                    setenv=Mock(),
                )
                with patch.object(release, "ROOT", root), self.assertRaises(ValueError):
                    release.release_identity(runner, {})
                if not tag:
                    runner.run.assert_not_called()


if __name__ == "__main__":
    unittest.main()
