# SPDX-License-Identifier: MPL-2.0
"""Generator guarantees operate on disposable cases, without compiling Wave."""

from pathlib import Path
import tempfile
import unittest

from tools.populate_case_matrix import refresh_cases, target_mix


class PopulateCaseMatrixTests(unittest.TestCase):
    def fixture(self, root):
        shared = root / "shared"
        shared.mkdir()
        for number in range(104, 114):
            (shared / f"test{number}.wave").write_text(
                f"// workload {number}\nfun main() -> i32 {{\n    return 0;\n}}\n"
            )
        manifest = root / "cases.toml"
        manifest.write_text("""version = 2
[supported.linux]
amd64 = true
shakti = false
[ci.linux]
amd64 = true
shakti = false
[[target]]
id = "linux-amd64"
os = "linux"
arch = "amd64"
executor = "native"
runner = "ubuntu-latest"
ci_group = "linux"
""")
        return manifest

    def test_first_creation_idempotence_numbering_and_handwritten_preservation(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            manifest = self.fixture(root)
            handwritten = root / "linux/amd64/test1.wave"
            handwritten.parent.mkdir(parents=True)
            content = b"// hand-written sentinel\nfun main() { }\n"
            handwritten.write_bytes(content)
            self.assertEqual(refresh_cases(root, manifest), 39)
            files = {
                path.relative_to(root): path.read_bytes()
                for path in root.rglob("*.wave")
            }
            self.assertEqual(refresh_cases(root, manifest), 0)
            self.assertEqual(
                files,
                {
                    path.relative_to(root): path.read_bytes()
                    for path in root.rglob("*.wave")
                },
            )
            self.assertEqual(handwritten.read_bytes(), content)
            for suite in (
                "shared/amd64",
                "shared/shakti",
                "linux/amd64",
                "linux/shakti",
            ):
                self.assertEqual(
                    sorted(int(p.stem[4:]) for p in (root / suite).glob("test*.wave")),
                    list(range(1, 11)),
                )
            generated = (root / "linux/shakti/test1.wave").read_text()
            self.assertIn('"slli t0, a0, 5"', generated)
            self.assertIn("// workload 104", generated)
            self.assertEqual(target_mix("shakti"), target_mix("riscv64"))

    def test_refuses_numbering_gaps_and_bad_manifest_shapes(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            manifest = self.fixture(root)
            case = root / "linux/amd64/test3.wave"
            case.parent.mkdir(parents=True)
            case.write_text("sentinel")
            with self.assertRaisesRegex(ValueError, "non-contiguous"):
                refresh_cases(root, manifest)
            case.unlink()
            for body in (
                "version=1\ntarget=[]\n",
                'version=2\ntarget=[]\n[supported.linux]\namd64="yes"\n[ci.linux]\namd64=true\n',
            ):
                manifest.write_text(body)
                with self.assertRaises(ValueError):
                    refresh_cases(root, manifest)

    def test_full_manifest_metadata_contract_is_checked_before_generation(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            manifest = self.fixture(root)
            valid = manifest.read_text()
            variants = [
                (
                    valid.replace("[[target]]", "[[target]]\nunknown = true"),
                    "unknown fields",
                ),
                (
                    valid.replace('executor = "native"', 'executor = "guess"'),
                    "unsupported executor",
                ),
                (
                    valid.replace('arch = "amd64"', 'arch = "arm64"'),
                    "missing from the support matrix",
                ),
                (valid.replace('runner = "ubuntu-latest"\n', ""), "needs runner"),
                (
                    valid.replace('id = "linux-amd64"', 'id = "-unsafe"'),
                    "invalid target id",
                ),
                (
                    valid + valid[valid.index("[[target]]") :],
                    "multiple execution tables",
                ),
                (
                    'version=2\ntarget=["not-a-table"]\n[supported.linux]\namd64=true\n[ci.linux]\namd64=false\n',
                    "entry must be a table",
                ),
                (
                    "version=2\ntarget=[]\n[supported.linux]\n[ci.linux]\n",
                    "invalid operating-system row",
                ),
                (
                    valid.replace("shakti = false", "shakti = true", 1),
                    "need execution tables",
                ),
                (
                    valid.replace(
                        'executor = "native"',
                        'executor = "native"\nexclude=["../private.wave"]',
                    ),
                    "unsafe excluded case",
                ),
            ]
            before = {
                path.relative_to(root): path.read_bytes()
                for path in root.rglob("*.wave")
            }
            for body, message in variants:
                with (
                    self.subTest(message=message),
                    self.assertRaisesRegex(ValueError, message),
                ):
                    manifest.write_text(body)
                    refresh_cases(root, manifest)
                self.assertEqual(
                    before,
                    {
                        path.relative_to(root): path.read_bytes()
                        for path in root.rglob("*.wave")
                    },
                )
            manifest.write_text(valid)
            self.assertEqual(refresh_cases(root, manifest), 40)
            self.assertEqual(refresh_cases(root, manifest), 0)

    def test_refresh_preserves_replaced_workloads_and_existing_metadata(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            manifest = self.fixture(root)
            refresh_cases(root, manifest)
            source = root / "shared/amd64/test1.wave"
            source.write_text("// hand-authored ABI workload\nfun main() {}\n")
            other = root / "linux/amd64/test2.wave"
            other.write_text("// wave-test: runtime=native\n" + other.read_text())
            self.assertEqual(refresh_cases(root, manifest), 0)
            self.assertTrue(source.read_text().startswith("// hand-authored"))
            self.assertEqual(other.read_text().count("// wave-test:"), 1)


if __name__ == "__main__":
    unittest.main()
