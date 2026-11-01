# This file is part of the Wave language project.
# SPDX-License-Identifier: MPL-2.0
"""Execute the actual publish step with a fake gh; never contact GitHub."""

import hashlib
import sys
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import textwrap
import unittest


class ReleasePublishTests(unittest.TestCase):
    def run_publish(self, remote, status=0):
        from types import SimpleNamespace
        from unittest.mock import Mock, patch
        from tools.ci.release import publish

        calls = []

        def run(args):
            calls.append(args)
            if args[:2] == ["gh", "api"]:
                if status:
                    raise RuntimeError("GitHub API unavailable")
                return remote
            return ""

        runner = SimpleNamespace(
            publish_authorized=True,
            run=run,
            context={},
            env={
                "GITHUB_REPOSITORY": "wavefnd/Wave",
                "GITHUB_SHA": "a" * 40,
                "RELEASE_VERSION": "0.2.1-pre-beta",
            },
        )
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            runner.temp = root
            assets = root / "release-assets"
            assets.mkdir()
            from tools.check_release_assets import ARCHIVE_TARGETS
            for target in ARCHIVE_TARGETS:
                name = f"wave-v0.2.1-pre-beta-{target}" + (
                    ".zip" if "windows" in target else ".tar.gz"
                )
                for suffix in ("", ".sha256", ".metadata.json"):
                    (assets / (name + suffix)).touch()
            (assets / "SHA256SUMS").touch()
            (root / "std").mkdir()
            (root / "std/manifest.json").write_text('{"compatibility_revision":5}')
            with (
                patch("tools.ci.release.ROOT", root),
                patch("tools.ci.release.verify_metadata") as verify,
                patch("tools.ci.release_notes.generate", return_value="Fixture release notes") as notes,
            ):
                try:
                    publish(runner, {})
                except (ValueError, RuntimeError):
                    result = 1
                else:
                    result = 0
                verify.assert_called_once()
                notes.assert_called_once()
                self.assertEqual(notes.call_args.args[1], "a" * 40)
                self.assertEqual(
                    notes.call_args.kwargs, {"release_tag": "v0.2.1-pre-beta"}
                )
        return result, [call for call in calls if call[0] == "gh"]

    def test_matching_master_publishes_after_verification(self):
        result, calls = self.run_publish("a" * 40)
        self.assertEqual(result, 0)
        self.assertEqual(
            calls[0],
            [
                "gh",
                "api",
                "repos/wavefnd/Wave/git/ref/heads/master",
                "--jq",
                ".object.sha",
            ],
        )
        self.assertEqual(calls[1][:3], ["gh", "release", "create"])
        self.assertEqual(calls[1][calls[1].index("--target") + 1], "a" * 40)

    def test_public_release_uploads_only_the_nine_archives(self):
        from tools.check_release_assets import ARCHIVE_TARGETS

        result, calls = self.run_publish("a" * 40)
        self.assertEqual(result, 0)
        command = calls[-1]
        assets = command[4:command.index("--repo")]
        self.assertEqual(
            {Path(asset).name for asset in assets},
            {
                f"wave-v0.2.1-pre-beta-{target}" + (
                    ".zip" if "windows" in target else ".tar.gz"
                )
                for target in ARCHIVE_TARGETS
            },
        )
        self.assertEqual(len(assets), 9)

    def test_changed_missing_malformed_or_unavailable_master_never_publishes(self):
        for remote, status in [("b" * 40, 0), ("", 0), ("not-a-sha", 0), ("a" * 40, 1)]:
            with self.subTest(remote=remote, status=status):
                result, calls = self.run_publish(remote, status)
                self.assertNotEqual(result, 0)
                self.assertEqual(len(calls), 1)


class ReleaseAssetTests(unittest.TestCase):
    def setUp(self):
        from tools import check_release_assets

        self.validator = check_release_assets
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.version = "0.2.1-pre-beta"
        self.names = []
        for target in check_release_assets.ARCHIVE_TARGETS:
            name = f"wave-v{self.version}-{target}" + (
                ".zip" if "windows" in target else ".tar.gz"
            )
            self.names.append(name)
            (self.root / name).write_bytes(name.encode())
            digest = hashlib.sha256(name.encode()).hexdigest()
            (self.root / (name + ".sha256")).write_text(f"{digest}  {name}\n")

    def verify(self):
        return self.validator.verify(self.root, self.version)

    def test_complete_set_writes_exactly_one_verified_record_per_archive(self):
        # Accept the binary marker emitted by checksum utilities as well.
        sidecar = self.root / (self.names[0] + ".sha256")
        sidecar.write_text(sidecar.read_text().replace("  ", " *"))
        self.assertEqual(self.verify(), 9)
        lines = (self.root / "SHA256SUMS").read_text().splitlines()
        self.assertEqual([line[66:] for line in lines], sorted(self.names))
        for line in lines:
            self.assertEqual(
                line[:64],
                hashlib.sha256((self.root / line[66:]).read_bytes()).hexdigest(),
            )

    def test_empty_duplicate_malformed_wrong_name_and_unsafe_records_fail(self):
        name = self.names[0]
        sidecar = self.root / (name + ".sha256")
        valid = sidecar.read_text()
        for invalid in [
            "",
            "\n",
            valid + valid,
            valid + "\n",
            "not a checksum\n",
            valid.replace(name, self.names[1]),
            valid.replace(name, "../" + name),
            valid.replace(name, str((self.root / name).resolve())),
            "0" * 64 + f"  {name}\n",
        ]:
            with self.subTest(record=invalid):
                (self.root / "SHA256SUMS").write_text("previous manifest")
                sidecar.write_text(invalid)
                with self.assertRaises(ValueError):
                    self.verify()
                self.assertEqual(
                    (self.root / "SHA256SUMS").read_text(), "previous manifest"
                )
        sidecar.write_text(valid)
        (self.root / name).write_bytes(b"changed archive")
        with self.assertRaisesRegex(ValueError, "checksum mismatch"):
            self.verify()

    def test_missing_or_unexpected_archives_and_sidecars_fail(self):
        for name in (self.names[0], self.names[0] + ".sha256"):
            path = self.root / name
            contents = path.read_bytes()
            path.unlink()
            with self.assertRaisesRegex(ValueError, "missing="):
                self.verify()
            path.write_bytes(contents)
        for name in ("unexpected.zip", "unexpected.sha256"):
            path = self.root / name
            path.touch()
            with self.assertRaisesRegex(ValueError, "unexpected="):
                self.verify()
            path.unlink()

    def test_cli_failure_does_not_create_a_partial_manifest(self):
        (self.root / (self.names[0] + ".sha256")).write_text("")
        result = subprocess.run(
            [
                sys.executable,
                "-m",
                "tools.check_release_assets",
                "--directory",
                str(self.root),
                "--version",
                self.version,
            ],
            cwd=Path(__file__).resolve().parents[1],
            text=True,
            capture_output=True,
            timeout=10,
        )
        self.assertEqual(result.returncode, 1)
        self.assertIn("exactly one checksum record", result.stderr)
        self.assertFalse((self.root / "SHA256SUMS").exists())


if __name__ == "__main__":
    unittest.main()
