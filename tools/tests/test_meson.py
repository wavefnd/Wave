# SPDX-License-Identifier: MPL-2.0
"""Exercise the real Meson graph with a disposable, controlled Cargo executable."""

import importlib.util
import json
import os
import platform
import re
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from tools.build import cargo_env, install
from tools.ci.targets import PINS

ROOT = Path(__file__).resolve().parents[1]
MESON = [sys.executable, "-m", "mesonbuild.mesonmain"]
HAS_MESON = importlib.util.find_spec("mesonbuild") is not None

FIXTURE = """#!/usr/bin/env python3
import json, os, pathlib, sys
role = 'rustc' if sys.argv[1] == 'rustc' else 'cargo'
args = sys.argv[2:] if role == 'rustc' else sys.argv[1:]
if role == 'rustc':
    print('rustc ' + os.environ['FIXTURE_RUST_VERSION'])
    print('host: ' + os.environ['FIXTURE_RUST_HOST'])
    print('release: ' + os.environ['FIXTURE_RUST_VERSION'])
elif role == 'cargo':
    with open(os.environ['FIXTURE_COMMANDS'], 'a') as stream:
        stream.write(json.dumps({'args': args, 'rustflags': os.environ.get('RUSTFLAGS', '')}) + '\\n')
    if os.environ.get('FIXTURE_FAIL'):
        sys.exit(17)
    root = pathlib.Path(os.environ['CARGO_TARGET_DIR'])
    if '--target' in args:
        root /= args[args.index('--target') + 1]
    root /= 'release' if '--release' in args else 'debug'
    root.mkdir(parents=True, exist_ok=True)
    name = 'wavec.exe' if 'windows' in os.environ['FIXTURE_RUST_HOST'] else 'wavec'
    (root / name).write_bytes(b'controlled Cargo output')
"""


def host_triple():
    arch = platform.machine().lower()
    arch = {"amd64": "x86_64", "arm64": "aarch64"}.get(arch, arch)
    suffix = {
        "Linux": "unknown-linux-gnu",
        "Darwin": "apple-darwin",
        "Windows": "pc-windows-msvc",
        "FreeBSD": "unknown-freebsd",
    }[platform.system()]
    return arch + "-" + suffix


@unittest.skipUnless(
    HAS_MESON and shutil.which("ninja"), "Meson and Ninja are required"
)
class MesonGraphTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="wave meson graph ")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        fixture = self.root / "cargo fixture.py"
        fixture.write_text(FIXTURE, encoding="utf-8")
        fixture.chmod(0o755)
        native = self.root / "native.ini"
        native.write_text(
            "[binaries]\n"
            + "".join(
                name
                + " = "
                + repr(
                    [Path(sys.executable).as_posix(), fixture.as_posix(), name]
                    if name == "rustc"
                    else [fixture.as_posix()]
                )
                + "\n"
                for name in ["cargo", "rustc", "rustup"]
            ),
            encoding="utf-8",
        )
        self.native = native
        self.commands = self.root / "commands.jsonl"
        self.environment = dict(
            os.environ,
            FIXTURE_COMMANDS=str(self.commands),
            FIXTURE_RUST_VERSION=PINS["RUST_VERSION"],
            FIXTURE_RUST_HOST=host_triple(),
            LLVM_SYS_211_PREFIX=str(self.root),
            LLVM_CONFIG_PATH=str(self.root / "llvm-config"),
            WAVE_LLVM_HOME=str(self.root),
        )

    def run_meson(self, *arguments, expected=0, environment=None):
        result = subprocess.run(
            [*MESON, *map(str, arguments)],
            cwd=ROOT,
            env=environment or self.environment,
            text=True,
            capture_output=True,
            timeout=90,
            check=False,
        )
        self.assertEqual(result.returncode, expected, result.stdout + result.stderr)
        return result.stdout

    def setup(self, name="build", *options):
        directory = self.root / name
        self.run_meson(
            "setup",
            directory,
            ROOT,
            "--native-file",
            self.native,
            "-Dcargo_target_dir=" + str(self.root / "cargo output"),
            *options,
        )
        return directory

    @unittest.skipIf(os.name == "nt", "controlled Cargo fixture uses a POSIX shebang")
    def test_native_build_rechecks_cargo_and_failure_preserves_previous_output(self):
        directory = self.setup()
        self.run_meson("compile", "-C", directory, "wavec")
        output = directory / ("wavec.exe" if os.name == "nt" else "wavec")
        self.assertEqual(output.read_bytes(), b"controlled Cargo output")
        self.run_meson("compile", "-C", directory, "wavec")
        rows = [json.loads(row) for row in self.commands.read_text().splitlines()]
        self.assertEqual(len(rows), 2)
        for row in rows:
            self.assertEqual(
                row["args"][:3], ["+" + PINS["RUST_VERSION"], "build", "--locked"]
            )
            self.assertIn("--jobs", row["args"])
            self.assertEqual(row["args"][row["args"].index("--jobs") + 1], "2")
            if platform.system() in ("Linux", "FreeBSD"):
                self.assertIn("$ORIGIN/llvm/lib", row["rustflags"])
        result = subprocess.run(
            [*MESON, "compile", "-C", str(directory), "release"],
            cwd=ROOT,
            env=dict(self.environment, FIXTURE_FAIL="1"),
            text=True,
            capture_output=True,
            timeout=90,
            check=False,
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("[+] Packaged", result.stdout)
        self.assertEqual(output.read_bytes(), b"controlled Cargo output")

    def test_cross_and_feature_arguments_and_package_dependencies(self):
        for target, features in [
            ("aarch64-pc-windows-msvc", "llvm-target-aarch64"),
            ("x86_64-pc-windows-msvc", "llvm-target-core64"),
            ("riscv64gc-unknown-linux-gnu", None),
            ("loongarch64-unknown-linux-gnu", None),
            ("aarch64-apple-darwin", None),
            ("x86_64-unknown-freebsd", None),
        ]:
            with self.subTest(target=target):
                directory = self.setup(target, "-Dcompiler_target=" + target)
                targets = json.loads(
                    self.run_meson("introspect", directory, "--targets")
                )
                compiler = next(t for t in targets if t["name"] == "wavec")
                args = compiler["target_sources"][0]["compiler"]
                cargo = args[args.index("--") + 1 :]
                self.assertEqual(cargo[cargo.index("--target") + 1], target)
                if features:
                    self.assertIn("--no-default-features", cargo)
                    self.assertEqual(cargo[cargo.index("--features") + 1], features)
                else:
                    self.assertNotIn("--no-default-features", cargo)
                package = next(t for t in targets if t["name"] == "package")
                self.assertIn(compiler["id"], package["depends"])
                existing = next(t for t in targets if t["name"] == "package-existing")
                self.assertEqual(existing["depends"], [])

    def test_debug_profile_and_explicit_backend_selection(self):
        directory = self.setup(
            "debug", "-Dbuildtype=debug", "-Dllvm_features=llvm-target-wasm"
        )
        targets = json.loads(self.run_meson("introspect", directory, "--targets"))
        args = next(t for t in targets if t["name"] == "wavec")["target_sources"][0][
            "compiler"
        ]
        self.assertNotIn("--release", args)
        self.assertIn("llvm-target-wasm", args)
        self.assertNotIn("package", [t["name"] for t in targets])

    def test_invalid_target_and_wrong_rust_version_fail_at_setup(self):
        self.run_meson(
            "setup",
            self.root / "invalid",
            ROOT,
            "-Dcompiler_target=unrecognized-target",
            expected=1,
        )
        environment = dict(self.environment, FIXTURE_RUST_VERSION="1.90.0")
        self.run_meson(
            "setup",
            self.root / "wrong-version",
            ROOT,
            "--native-file",
            self.native,
            expected=1,
            environment=environment,
        )
        self.assertFalse(self.commands.exists())

    def test_ci_graph_can_configure_without_rust_or_llvm(self):
        directory = self.setup("ci-only", "-Dcompiler=false")
        targets = json.loads(self.run_meson("introspect", directory, "--targets"))
        self.assertEqual({t["name"] for t in targets}, {"ci", "distclean"})
        self.assertFalse(self.commands.exists())

    def test_unknown_task_fails_without_running_cargo(self):
        directory = self.setup("unknown-task", "-Dcompiler=false")
        self.run_meson(
            "compile", "-C", directory, "unrecognized-audit-command", expected=1
        )
        self.assertFalse(self.commands.exists())


class BuildEnvironmentTests(unittest.TestCase):
    def test_each_workflow_python_setup_provisions_build_tools(self):
        for workflow in ("ci", "release", "nightly"):
            path = ROOT / ".github/workflows" / (workflow + ".yml")
            # Workflows use four-space job-step indentation. Inspect whole
            # step blocks so metadata such as id: cannot hide a Python setup.
            steps = re.split(r"(?m)^    - ", path.read_text(encoding="utf-8"))[1:]
            setups = 0
            for index, step in enumerate(steps):
                if "uses: actions/setup-python@" not in step:
                    continue
                setups += 1
                with self.subTest(workflow=workflow, setup=step.splitlines()[0]):
                    self.assertLess(index + 1, len(steps))
                    provision = steps[index + 1]
                    self.assertIn("name: Install pinned Meson and Ninja", provision)
                    self.assertIn(
                        "python -m pip install --disable-pip-version-check "
                        "-r tools/ci/requirements-build.txt",
                        provision,
                    )
            self.assertGreater(setups, 0, workflow)

    def test_release_rpath_keeps_inherited_flags_and_literal_origin(self):
        environment = {"RUSTFLAGS": "-C debuginfo=1"}
        cargo_env.configure_linux_release_env(environment)
        self.assertTrue(environment["RUSTFLAGS"].startswith("-C debuginfo=1 "))
        self.assertIn("-Wl,--disable-new-dtags", environment["RUSTFLAGS"])
        self.assertIn("-Wl,-rpath,$ORIGIN/llvm/lib", environment["RUSTFLAGS"])

    def test_install_uses_destdir_and_preserves_unrelated_files(self):
        with tempfile.TemporaryDirectory(prefix="wave install ") as temporary:
            root = Path(temporary)
            stage = root / "payload"
            (stage / "std").mkdir(parents=True)
            (stage / "std/library.wave").write_bytes(b"std payload")
            (stage / "wavec").write_bytes(b"compiler payload")
            prefix = root / "destdir/prefix"
            destination = prefix / "bin"
            destination.mkdir(parents=True)
            (destination / "unrelated").write_bytes(b"keep")
            outside = root / "other-compiler"
            outside.write_bytes(b"keep linked compiler")
            try:
                os.link(outside, destination / "wavec")
            except OSError as error:
                self.skipTest(f"hard links unavailable: {error}")
            with (
                patch.dict(os.environ, MESON_INSTALL_DESTDIR_PREFIX=str(prefix)),
                patch.object(install, "stage_package", return_value=stage),
            ):
                install.install(root / "wavec", host_triple(), "bin")
            self.assertEqual((destination / "wavec").read_bytes(), b"compiler payload")
            self.assertEqual(
                (destination / "std/library.wave").read_bytes(), b"std payload"
            )
            self.assertEqual((destination / "unrelated").read_bytes(), b"keep")
            self.assertEqual(outside.read_bytes(), b"keep linked compiler")

    def test_install_dry_run_does_not_stage_or_modify_anything(self):
        with tempfile.TemporaryDirectory() as temporary:
            prefix = Path(temporary) / "absent"
            with (
                patch.dict(
                    os.environ,
                    MESON_INSTALL_DESTDIR_PREFIX=str(prefix),
                    MESON_INSTALL_DRY_RUN="1",
                ),
                patch.object(install, "stage_package") as stage,
            ):
                install.install(Path("unused"), host_triple(), "bin")
            stage.assert_not_called()
            self.assertFalse(prefix.exists())

    def test_ci_inventory_retains_all_lanes_and_uses_meson_builds(self):
        from tools.ci.common import inventory

        plans, procedures = inventory()
        self.assertEqual(len(plans), 31)
        self.assertEqual(sum(len(p["stages"]) for p in plans.values()), 228)
        commands = [c for p in procedures.values() for c in p.get("commands", [])]
        self.assertFalse(any(c[:2] == ["cargo", "build"] for c in commands))
        self.assertTrue(any("mesonbuild.mesonmain" in c for c in commands))
        requirements = (ROOT / "tools/ci/requirements-build.txt").read_text()
        self.assertIn("meson==" + PINS["MESON_VERSION"], requirements)
        self.assertIn("ninja==" + PINS["NINJA_VERSION"], requirements)


if __name__ == "__main__":
    unittest.main()
