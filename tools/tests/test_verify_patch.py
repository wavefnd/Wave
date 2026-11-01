# SPDX-License-Identifier: MPL-2.0
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

SCRIPT = Path(__file__).resolve().parent / "verify_patch.sh"


@unittest.skipUnless(
    os.name == "posix" and shutil.which("bash"), "POSIX patch verifier"
)
class PatchVerificationTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="wave patch tests ")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.repo = self.root / "repo"
        self.repo.mkdir()
        self.git_env = dict(os.environ, GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL=os.devnull)
        self.git("init", "-b", "original")
        self.git("config", "user.name", "Fixture Author")
        self.git("config", "user.email", "fixture@example.invalid")
        self.git("config", "commit.gpgsign", "false")
        (self.repo / "file").write_text("base\n")
        self.git("add", ".")
        self.git("commit", "-m", "base")
        self.base = self.git("rev-parse", "HEAD").strip()
        self.bin = self.root / "bin"
        self.bin.mkdir()
        self.calls = self.root / "calls"
        self.env = dict(
            self.git_env,
            PATH=str(self.bin) + os.pathsep + os.environ["PATH"],
            CALLS=str(self.calls),
        )
        self.cargo('printf "%s\\n" "$*" >> "$CALLS"\n')

    def git(self, *args):
        return subprocess.run(
            ["git", *args], cwd=self.repo, env=self.git_env, check=True, capture_output=True, text=True
        ).stdout

    def cargo(self, body):
        script = self.bin / "cargo"
        script.write_text("#!/bin/sh\n" + body)
        script.chmod(0o755)

    def series(self, signed):
        for index, sign in enumerate(signed):
            (self.repo / "file").write_text(f"change {index}\n")
            self.git("add", ".")
            self.git("commit", *(["-s"] if sign else []), "-m", f"change {index}")
        patch = self.root / "series.patch"
        patch.write_text(self.git("format-patch", "--no-signoff", "--stdout", self.base + "..HEAD"))
        self.git("reset", "--hard", self.base)
        return patch

    def run_patch(self, patch):
        return subprocess.run(
            ["bash", str(SCRIPT), str(patch)],
            cwd=self.repo,
            env=self.env,
            capture_output=True,
            text=True,
            timeout=15,
        )

    def restored(self, detached=False):
        self.assertEqual(self.git("rev-parse", "HEAD").strip(), self.base)
        self.assertEqual(
            self.git("branch", "--show-current").strip(), "" if detached else "original"
        )
        self.assertEqual(self.git("status", "--porcelain"), "")
        self.assertNotIn("patch-verify-", self.git("branch", "--list"))
        self.assertFalse((self.repo / ".git/rebase-apply").exists())

    def test_signed_series_runs_full_checks_and_restores(self):
        result = self.run_patch(self.series([True, True]))
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.restored()
        self.assertEqual(
            self.calls.read_text().splitlines(),
            [
                "fmt --all --check",
                "build --locked --release --jobs 2",
                "test --locked --workspace --all-targets --jobs 2",
                "clippy --locked --workspace --all-targets --jobs 2 -- -D warnings",
            ],
        )

    def test_unsigned_earlier_commit_rejected_before_cargo(self):
        result = self.run_patch(self.series([False, True]))
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Missing DCO", result.stderr)
        self.assertFalse(self.calls.exists())
        self.restored()

    def test_detached_head_restored_after_failure(self):
        patch = self.series([True])
        self.git("checkout", "--detach", self.base)
        self.cargo("exit 7\n")
        self.assertEqual(self.run_patch(patch).returncode, 7)
        self.restored(detached=True)

    def test_failed_application_aborts_am(self):
        patch = self.series([True])
        patch.write_text(patch.read_text().replace("-base\n", "-absent\n"))
        self.assertNotEqual(self.run_patch(patch).returncode, 0)
        self.restored()

    def test_term_restores_original_branch(self):
        self.cargo('kill -TERM "$PPID"\n')
        self.assertEqual(self.run_patch(self.series([True])).returncode, 143)
        self.restored()

    def test_dirty_work_is_preserved(self):
        patch = self.series([True])
        (self.repo / "file").write_text("user work\n")
        self.assertNotEqual(self.run_patch(patch).returncode, 0)
        self.assertEqual((self.repo / "file").read_text(), "user work\n")
        self.assertEqual(self.git("branch", "--show-current").strip(), "original")

    def test_unsigned_commit_stays_unsigned_with_format_signoff_configured(self):
        self.git("config", "format.signOff", "true")
        result = self.run_patch(self.series([False, True]))
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("Missing DCO", result.stderr)
        self.restored()
