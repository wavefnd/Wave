import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
SCRIPT = ROOT / "tools" / "get_maintainer.py"


class TestGetMaintainer(unittest.TestCase):
    def test_invocation_outside_repository_root(self):
        with tempfile.TemporaryDirectory() as cwd:
            result = subprocess.run(
                [sys.executable, str(SCRIPT), "front/parser/ast.rs"],
                cwd=cwd,
                capture_output=True,
                text=True,
                check=False,
            )

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("Maintainers to CC:", result.stdout)
        self.assertNotIn("MAINTAINERS file not found", result.stdout)

    def run_script(self, path):
        return subprocess.run(
            [sys.executable, str(SCRIPT), path],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=False,
        )

    def test_windows_and_repository_paths_match_same_section(self):
        unix = self.run_script("front/parser/file.rs")
        windows = self.run_script(r"front\parser\file.rs")

        self.assertEqual(unix.returncode, 0, unix.stderr)
        self.assertEqual(windows.returncode, 0, windows.stderr)
        self.assertEqual(windows.stdout, unix.stdout)
        self.assertIn("Maintainers to CC:", windows.stdout)

        sibling = self.run_script(r"front\parser-other\file.rs")
        self.assertIn("Using default", sibling.stdout)


if __name__ == "__main__":
    unittest.main()
