# SPDX-License-Identifier: MPL-2.0
"""Exercise committed package membership with disposable real Git repositories."""

import io
from pathlib import Path
import shutil
import subprocess
import tarfile
import tempfile
from types import SimpleNamespace
import unittest
from unittest import mock
import zipfile

from tools.ci.package import installation_instructions, stage_package


class PackageInputsTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="wave-package-inputs-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.git("init", "--quiet")
        (self.root / "std").mkdir()
        (self.root / "std/LICENSE").write_bytes(b"std license\n")
        (self.root / "std/core.wave").write_bytes(b"pub fun identity(x: i32) -> i32 { return x; }\n")
        (self.root / "LICENSE").write_bytes(b"compiler license\n")
        (self.root / "wavec").write_bytes(b"fixture compiler, never executed")
        self.git("add", "std", "LICENSE")
        self.commit()
        self.legacy = SimpleNamespace(
            ROOT=self.root,
            DIST_DIR=self.root / "dist",
            copy_executable=shutil.copy2,
            copy_lld_tools=lambda *args: [],
            write_linux_crt_objects=lambda *args: None,
            copy_llvm_runtime_libs=lambda *args: ["fixture-runtime"],
            is_windows_target=lambda target: "windows" in target,
            copy_windows_msvc_resources=lambda *args: None,
            patch_staged_runtime=lambda *args: None,
            verify_packaged_runtime_arch=lambda *args: None,
        )

    def git(self, *args):
        return subprocess.run(
            ["git", "-c", "core.autocrlf=false", *args], cwd=self.root,
            check=True, capture_output=True,
        ).stdout

    def commit(self):
        self.git("-c", "user.name=Package Fixture", "-c", "user.email=fixture@example.invalid",
                 "-c", "commit.gpgsign=false", "commit", "--quiet", "-m", "fixture")

    def stage(self, target="x86_64-unknown-linux-gnu"):
        return stage_package(self.legacy, target, self.root / "wavec", "package")

    def test_untracked_ignored_and_outside_files_do_not_enter_tar_or_zip(self):
        (self.root / ".gitignore").write_text("std/.ignored\n", encoding="utf-8")
        for name in [".local", ".ignored"]:
            (self.root / "std" / name).write_bytes(b"local sentinel")
        (self.root / "outside").write_bytes(b"outside sentinel")
        for target in ["x86_64-unknown-linux-gnu", "aarch64-pc-windows-msvc"]:
            with self.subTest(target=target):
                stage = self.stage(target)
                self.assertEqual((stage / "std/core.wave").read_bytes(), self.git("show", "HEAD:std/core.wave"))
                self.assertFalse((stage / "std/.local").exists())
                self.assertFalse((stage / "std/.ignored").exists())
                self.assertFalse((stage / "outside").exists())
                data = io.BytesIO()
                if "windows" in target:
                    with zipfile.ZipFile(data, "w") as archive:
                        for path in stage.rglob("*"):
                            if path.is_file():
                                archive.write(path, path.relative_to(stage))
                    with zipfile.ZipFile(data) as archive:
                        names = archive.namelist()
                else:
                    with tarfile.open(fileobj=data, mode="w:gz") as archive:
                        archive.add(stage, arcname="package")
                    data.seek(0)
                    with tarfile.open(fileobj=data, mode="r:gz") as archive:
                        names = archive.getnames()
                self.assertTrue(any(name.endswith("INSTALL.txt") for name in names))
                self.assertFalse(any("sentinel" in name or name.endswith((".local", ".ignored")) for name in names))

    def test_dirty_inputs_fail_before_destination_or_dependency_helpers_change(self):
        destination = self.root / "dist/package"
        destination.mkdir(parents=True)
        (destination / "previous").write_bytes(b"preserve")
        (self.root / "std/core.wave").write_bytes(b"modified tracked source")
        with mock.patch.object(self.legacy, "copy_executable") as copy:
            with self.assertRaisesRegex(ValueError, "commit or restore"):
                self.stage()
            copy.assert_not_called()
        self.assertEqual((destination / "previous").read_bytes(), b"preserve")

    def test_every_tracked_symlink_is_rejected_including_internal_links(self):
        for target in ["core.wave", str(self.root / "LICENSE")]:
            with self.subTest(target=target):
                link = self.root / "std/link.wave"
                try:
                    link.symlink_to(target)
                except OSError as error:
                    self.skipTest(f"symlink creation unavailable: {error}")
                self.git("add", "std/link.wave")
                self.commit()
                with self.assertRaisesRegex(ValueError, "regular files"):
                    self.stage()
                self.git("rm", "--quiet", "std/link.wave")
                self.commit()

    def test_untracked_symlink_is_excluded_without_reading_its_target(self):
        link = self.root / "std/untracked.wave"
        try:
            link.symlink_to(self.root / "missing-outside")
        except OSError as error:
            self.skipTest(f"symlink creation unavailable: {error}")
        self.assertFalse((self.stage() / "std/untracked.wave").exists())

    def test_installation_instructions_cover_all_nine_targets(self):
        # Keep target membership authoritative in the existing package plans.
        from tools.ci.targets import TARGETS
        available = [target for target in TARGETS.values() if target.distribution]
        self.assertEqual(len(available), 9)
        for target in available:
            text = installation_instructions(target.triple, "wavec.exe" if target.host_os == "windows" else "wavec")
            self.assertIn(target.triple, text)
            self.assertIn("check hello.wave", text)
            self.assertIn("PATH", text)
            self.assertIn("adjacent std", text)
            self.assertIn("Windows SDK" if target.host_os == "windows" else "Apple SDK" if target.host_os == "macos" else "base system" if "freebsd" in target.triple else "glibc", text)


if __name__ == "__main__":
    unittest.main()
