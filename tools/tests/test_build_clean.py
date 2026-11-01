"""Tests for the repository build driver."""

import os
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


TARGETS = ["x86_64-unknown-linux-gnu", "aarch64-pc-windows-msvc"]


class CleanupTests(unittest.TestCase):
    def test_clean_uses_repository_paths_and_preserves_unrelated_files(self):
        from tools.build import clean as cleanup

        with tempfile.TemporaryDirectory(prefix="cleanup with spaces ") as directory:
            root = Path(directory) / "repository"
            other = Path(directory) / "unrelated"
            root.mkdir()
            other.mkdir()
            for folder in (
                root / "target",
                root / "dist",
                other / "target",
                root / ".tmp",
            ):
                folder.mkdir()
                (folder / "sentinel").write_text("preserve unless owned")
            owned = [
                "wave-v0.2.1-pre-beta-x86_64-linux-gnu.tar.gz",
                "wave-v0.2.1-pre-beta-aarch64-pc-windows-msvc.zip",
                "wave-v0.2.1-pre-beta-x86_64-linux-gnu.tar.gz.sha256",
            ]
            unowned = [
                "backup.zip",
                "source.tar.gz",
                "wave-custom.zip",
                "wave-v0.2.1-pre-beta-unknown-target.tar.gz",
            ]
            for name in owned + unowned:
                (root / name).write_text("repository")
                (other / name).write_text("unrelated")
            cwd = Path.cwd()
            try:
                os.chdir(other)
                cleanup.clean(root, TARGETS)
                cleanup.clean(root, TARGETS)  # Missing build outputs are harmless.
            finally:
                os.chdir(cwd)
            self.assertFalse((root / "target").exists())
            self.assertFalse((root / "dist").exists())
            self.assertTrue((other / "target/sentinel").exists())
            self.assertTrue((root / ".tmp/sentinel").exists())
            for name in owned:
                self.assertFalse((root / name).exists())
            for name in unowned:
                self.assertEqual((root / name).read_text(), "repository")
            for name in owned + unowned:
                self.assertEqual((other / name).read_text(), "unrelated")

    def test_clean_unlinks_build_directory_alias_without_deleting_its_target(self):
        from tools.build import clean as cleanup

        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory) / "repo"
            outside = Path(directory) / "outside"
            root.mkdir()
            outside.mkdir()
            sentinel = outside / "sentinel"
            sentinel.write_text("keep")
            try:
                (root / "target").symlink_to(outside, target_is_directory=True)
            except OSError as error:
                self.skipTest(f"directory symlinks unavailable: {error}")
            cleanup.clean(root, TARGETS)
            self.assertFalse((root / "target").is_symlink())
            self.assertEqual(sentinel.read_text(), "keep")


if __name__ == "__main__":
    unittest.main()
