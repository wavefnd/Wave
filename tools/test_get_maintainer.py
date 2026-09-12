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


if __name__ == "__main__":
    unittest.main()
