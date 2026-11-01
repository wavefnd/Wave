# SPDX-License-Identifier: MPL-2.0
"""Exercise cache invalidation, standalone snapshots and mandatory acceptance."""

import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import MagicMock, Mock, patch

from tools.ci import freebsd_cache as cache, freebsd_package as package
from tools.ci.targets import PINS


class FreeBSDCacheTests(unittest.TestCase):
    def test_failed_snapshot_keeps_previous_cache(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            (root / "vm.qcow2").write_bytes(b"previous image")
            (root / "manifest.json").write_text("previous manifest")
            runner = SimpleNamespace(run=Mock(side_effect=RuntimeError("conversion failed")))
            with self.assertRaisesRegex(RuntimeError, "conversion failed"):
                cache.save_image(runner, root / "working", root, "new", "commit")
            self.assertEqual((root / "vm.qcow2").read_bytes(), b"previous image")
            self.assertEqual((root / "manifest.json").read_text(), "previous manifest")

    def test_identity_tracks_pins_dependencies_and_build_configuration(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            names = ["Cargo.lock", "Cargo.toml", "front/parser/Cargo.toml",
                     "llvm/Cargo.toml", "utils/Cargo.toml", "whale/Cargo.toml", ".cargo/config.toml",
                     "tools/ci/freebsd_cache.py", "tools/ci/freebsd_package.py", "meson.build", "meson.options", "tools/build/cargo_env.py", "tools/build/runtime.py"]
            for name in names:
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(name)
            original = cache.identity(root)
            (root / "source.rs").write_text("changed application source")
            self.assertEqual(original, cache.identity(root))
            for name in names:
                with self.subTest(file=name):
                    path = root / name
                    path.write_text(name + " changed")
                    self.assertNotEqual(original, cache.identity(root))
                    path.write_text(name)
            for name in ("FREEBSD_IMAGE_SHA256", "RUST_VERSION", "LLVM_SOURCE_VERSION", "MESON_VERSION"):
                self.assertNotEqual(original, cache.identity(root, dict(PINS, **{name: "changed"})))

    def test_bad_cache_is_discarded_before_boot(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            runner = SimpleNamespace(run=Mock())
            self.assertIsNone(cache.restored_image(runner, root, "expected"))
            image = root / "vm.qcow2"
            image.write_bytes(b"fixture")
            for metadata in ([], {"identity": "wrong"}, {"identity": "expected", "sha256": "bad"}):
                (root / "manifest.json").write_text(json.dumps(metadata))
                self.assertIsNone(cache.restored_image(runner, root, "expected"))
            runner.run.assert_not_called()
            (root / "manifest.json").write_text(json.dumps({"identity": "expected", "sha256": cache.digest(image)}))
            for extra in ({"backing-filename": "/old/runner/image"}, {"dirty-flag": True},
                          {"format-specific": {"data": {"data-file": "/outside"}}}):
                runner.run.return_value = json.dumps({"format": "qcow2", **extra})
                self.assertIsNone(cache.restored_image(runner, root, "expected"))

    @unittest.skipUnless(shutil.which("qemu-img"), "qemu-img snapshot integration")
    def test_saved_overlay_is_standalone_and_survives_backing_file_removal(self):
        def run(command, **kwargs):
            return subprocess.run(command, check=True, capture_output=True, text=True).stdout
        runner = SimpleNamespace(run=run)
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            base, overlay = root / "base.qcow2", root / "working.qcow2"
            run(["qemu-img", "create", "-f", "qcow2", base, "8M"])
            run(["qemu-img", "create", "-f", "qcow2", "-F", "qcow2", "-b", base, overlay])
            cache.save_image(runner, overlay, root / "cache", "environment", "commit")
            base.unlink()
            overlay.unlink()
            self.assertEqual(cache.restored_image(runner, root / "cache", "environment"), root / "cache/vm.qcow2")

    def test_cold_and_warm_runs_validate_and_only_success_saves(self):
        for warm, failure, kvm in (
            (False, None, False), (False, None, True), (True, None, False),
            (True, None, True), (True, "acceptance", True), (True, "shutdown", False),
        ):
            with self.subTest(warm=warm, failure=failure, kvm=kvm), tempfile.TemporaryDirectory() as folder:
                root = Path(folder)
                calls = []
                def run(command, **kwargs):
                    calls.append(command)
                    if command[0] == "ssh-keygen":
                        Path(str(command[-1]) + ".pub").write_text("public key")
                    if command[:2] == ["git", "rev-parse"]:
                        return "a" * 40
                    if command[0] == "ssh" and command[-1] == "sh /mnt/build.sh" and failure == "acceptance":
                        raise RuntimeError("package acceptance failed")
                    return ""
                runner = SimpleNamespace(provision=True, temp=root, run=run,
                    env=dict(PINS, RUNNER_TEMP=str(root), RELEASE_VERSION="0.2.1-pre-beta"))
                tree = MagicMock()
                tree.__enter__.return_value = tree
                tree.process.wait.return_value = 1 if failure == "shutdown" else 0
                sock = MagicMock()
                sock.__enter__.return_value.getsockname.return_value = ("127.0.0.1", 12345)
                with patch.object(cache, "restored_image", return_value=root / "cached.qcow2" if warm else None), \
                     patch.object(cache, "save_image") as save, \
                     patch.object(package, "freebsd_image") as provision, \
                     patch.object(package, "ProcessTree", return_value=tree), \
                     patch.object(package.os, "access", return_value=kvm), \
                     patch.object(package, "Console"), \
                     patch.object(package, "bootstrap_guest"), \
                     patch.object(package, "console_host_key", return_value="ssh-ed25519 fixture"), \
                     patch.object(package.socket, "socket", return_value=sock):
                    if failure:
                        with self.assertRaisesRegex(RuntimeError, "acceptance failed|shut down cleanly"):
                            package.freebsd_package(runner, {})
                        save.assert_not_called()
                        if failure == "acceptance":
                            tree.process.wait.assert_not_called()
                    else:
                        package.freebsd_package(runner, {})
                        save.assert_called_once()
                        tree.process.wait.assert_called_once()
                    self.assertEqual(provision.call_count, 0 if warm else 1)
                    command = package.ProcessTree.call_args.args[0]
                    self.assertEqual(command[command.index("-accel") + 1], "kvm" if kvm else "tcg")
                    self.assertEqual(command[command.index("-cpu") + 1], "host" if kvm else "Nehalem")
                script = (root / "freebsd-package/inputs/build.sh").read_text()
                if os.name == "posix":
                    subprocess.run(["sh", "-n"], input=script, text=True, check=True)
                self.assertIn("cargo test --locked --workspace --lib --jobs 2", script)
                self.assertIn("sysctl hw.model hw.ncpu", script)
                self.assertIn("cargo test --locked --test frontend_regressions --jobs 2", script)
                self.assertIn("cargo test --locked --test env_boundaries --jobs 2", script)
                self.assertIn("meson compile -C .tmp/meson-freebsd release", script)
                self.assertIn("python3 -m tools.ci.freebsd_package --guest-smoke", script)
                self.assertIn("checkout --detach --force " + "a" * 40, script)
                self.assertIn("git -C /root/Wave clean -ffdx -e target/", script)
                self.assertEqual(any(c[:2] == ["qemu-img", "resize"] for c in calls), not warm)
                if not failure:
                    cleanup = next(c[-1] for c in calls if c[0] == "ssh" and c[-1].startswith("set -e; rm"))
                    self.assertIn("/etc/ssh/ssh_host_*", cleanup)
                    self.assertIn("/root/.cargo/credentials", cleanup)


if __name__ == "__main__":
    unittest.main()
