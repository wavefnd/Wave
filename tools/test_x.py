"""Tests for the repository build driver."""

import subprocess
import sys
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


class XPyTests(unittest.TestCase):
    def test_unknown_command_fails_without_running_a_build(self):
        result = subprocess.run(
            [sys.executable, str(ROOT / "x.py"), "unrecognized-audit-command"],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=False,
        )

        self.assertEqual(result.returncode, 1)
        self.assertIn("Unknown command: unrecognized-audit-command", result.stdout)
        self.assertIn("Usage: x.py", result.stdout)


if __name__ == "__main__":
    unittest.main()
