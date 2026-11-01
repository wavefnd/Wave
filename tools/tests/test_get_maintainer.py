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

class TestNormalizedMaintainers(unittest.TestCase):
    def test_normalized_paths_match_without_escaping_or_prefix_collisions(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            script = root / "tools/get_maintainer.py"
            script.parent.mkdir()
            script.write_text(SCRIPT.read_text())
            (root / "MAINTAINERS").write_text(
                "[Parser]\nM: parser@example.test\nF: front/parser/\n"
            )
            cases = {
                "front/parser/file.rs": True,
                "./front/parser/file.rs": True,
                "front/unused/../parser/file.rs": True,
                r".\front\unused\..\parser\file.rs": True,
                "front/parser-other/file.rs": False,
                "../front/parser/file.rs": False,
                "front/../../front/parser/file.rs": False,
                "/front/parser/file.rs": False,
                r"C:\front\parser\file.rs": False,
                r"C:front\parser\file.rs": False,
                r"\\server\front\parser\file.rs": False,
            }
            for path, matches in cases.items():
                with self.subTest(path=path):
                    result = subprocess.run([sys.executable, str(script), path],
                                            cwd=directory, capture_output=True, text=True)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual("parser@example.test" in result.stdout, matches)


class TestMaintainerCoverage(unittest.TestCase):
    def test_directory_exact_file_multiple_inputs_and_fallback_paths(self):
        from tools.get_maintainer import maintainers_for_paths, parse_maintainers

        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "MAINTAINERS"
            path.write_text(
                "[Compiler]\nM: compiler@example.test\nF: src/\n"
                "[Build]\nM: build@example.test\nF: Cargo.toml\n"
            )
            sections = parse_maintainers(path)
            self.assertEqual(
                maintainers_for_paths(["src", "src/deep/file.rs"], sections),
                ["compiler@example.test"],
            )
            self.assertEqual(
                maintainers_for_paths(["Cargo.toml", "src/main.rs"], sections),
                ["build@example.test", "compiler@example.test"],
            )
            self.assertEqual(
                maintainers_for_paths(
                    ["src-other/file", "Cargo.toml.backup", "unknown.txt"], sections
                ),
                [],
            )

    def test_each_published_pattern_matches_a_tracked_repository_path(self):
        from tools.get_maintainer import parse_maintainers, stale_patterns

        result = subprocess.run(
            ["git", "ls-files", "-z"], cwd=ROOT, capture_output=True, check=True
        )
        paths = result.stdout.decode("utf-8").split("\0")
        self.assertEqual(stale_patterns(parse_maintainers(), paths), [])
        sections = [{"files": ["src/", "llvm_temporary/", "Cargo.toml"]}]
        self.assertEqual(stale_patterns(sections, paths), ["llvm_temporary/"])


if __name__ == "__main__":
    unittest.main()
