# SPDX-License-Identifier: MPL-2.0
"""MSVC linker tool inventory; native release staging."""
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

from tools.build import runtime as DRIVER

class MsvcToolTests(unittest.TestCase):
    def test_msvc_inventory_requires_named_coff_linker(self):
        for target in ("x86_64-pc-windows-msvc", "aarch64-pc-windows-msvc"):
            self.assertIn(("lld-link", True), DRIVER.llvm_tools_for_target(target))
        self.assertNotIn(("lld-link", True), DRIVER.llvm_tools_for_target("x86_64-pc-windows-gnu"))
        self.assertNotIn(("lld-link", True), DRIVER.llvm_tools_for_target("x86_64-unknown-linux-gnu"))

    def test_stages_exact_name_and_rejects_missing_or_foreign_tool(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "lld-link.exe"
            source.write_bytes(b"test tool")
            target = "aarch64-pc-windows-msvc"
            with patch.object(DRIVER, "llvm_tools_for_target", return_value=[("lld-link", True)]), \
                 patch.object(DRIVER, "find_release_tool", return_value=source), \
                 patch.object(DRIVER, "require_binary_for_target") as check:
                DRIVER.copy_lld_tools(root / "stage", target)
                self.assertEqual((root / "stage/llvm/bin/lld-link.exe").read_bytes(), b"test tool")
                check.assert_called_once_with(source, target, "LLVM tool")
                check.side_effect = SystemExit(1)
                with self.assertRaises(SystemExit):
                    DRIVER.copy_lld_tools(root / "foreign", target)
                self.assertFalse((root / "foreign/llvm/bin/lld-link.exe").exists())
            with patch.object(DRIVER, "llvm_tools_for_target", return_value=[("lld-link", True)]), \
                 patch.object(DRIVER, "find_release_tool", return_value=None):
                with self.assertRaises(SystemExit):
                    DRIVER.copy_lld_tools(root / "missing", target)

if __name__ == "__main__":
    unittest.main()
