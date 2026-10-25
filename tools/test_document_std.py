# SPDX-License-Identifier: MPL-2.0
import json
from pathlib import PurePosixPath
import subprocess
import unittest

from tools.document_std import ROOT, declarations


class PublicInventoryTests(unittest.TestCase):
    def test_public_exports_keep_signatures_guards_and_source_lines(self):
        source = '''// pub fun hidden() {}
import("private");
#[target(os="linux")]
pub async fun value<T>(x: T, marker: str = "{") -> T { return x; }
pub import("public")::{value};
pub struct Pair { left: i32; right: i32; }
/* pub const PRIVATE: i32 = 4; */
pub const COUNT: i32 = 3;
'''
        entries = list(declarations(source))
        self.assertEqual(len(entries), 4)
        self.assertEqual(entries[0][0], 4)
        self.assertEqual(entries[0][1], ['#[target(os="linux")]'])
        self.assertIn('marker: str = "{"', entries[0][2])
        self.assertNotIn('return x', entries[0][2])
        self.assertEqual(entries[1][2], 'pub import("public")::{value};')
        self.assertIn('right: i32;', entries[2][2])
        self.assertEqual(entries[3][0], 8)

    def test_checked_in_inventory_matches_public_sources(self):
        from tools.document_std import ROOT, render, render_systems
        for name, rendered in [("API.md", render()), ("SYSTEMS.md", render_systems())]:
            self.assertTrue(
                (ROOT / "std" / name).read_text(encoding="utf-8") == rendered,
                f"std/{name} is stale; run python3 tools/document_std.py",
            )

    def test_std_manifest_lists_git_tracked_module_roots(self):
        # HEAD's tracked sources, not filesystem discovery, define module membership:
        # untracked local or generated .wave files must never enter this inventory.
        tracked = subprocess.check_output(
            ["git", "ls-tree", "-r", "--name-only", "HEAD", "--", "std"],
            cwd=ROOT,
            text=True,
        ).splitlines()
        modules = sorted({
            path.parts[1].removesuffix(".wave")
            for name in tracked
            if name.endswith(".wave")
            for path in [PurePosixPath(name)]
        })
        self.assertTrue(modules, "expected tracked std source modules")
        manifest = json.loads((ROOT / "std/manifest.json").read_text(encoding="utf-8"))
        self.assertEqual(sorted(manifest["modules"]), modules)

    def test_private_and_commented_declarations_are_not_api(self):
        source = 'fun private() {}\n/*\npub fun fake() {}\n*/\n'
        self.assertEqual(list(declarations(source)), [])


if __name__ == '__main__':
    unittest.main()
