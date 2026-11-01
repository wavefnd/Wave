# SPDX-License-Identifier: MPL-2.0
"""Reject unusable or unpinned prebuilt SDKs without touching an installation."""

import hashlib
import json
import os
import tarfile
import tempfile
import unittest
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import Mock, patch

from tools.ci import llvm_bundle as bundle


class BundleTests(unittest.TestCase):
    def test_cmake_cross_tools_are_absolute_paths(self):
        from tools.ci.llvm_toolchain import options
        with patch("shutil.which", side_effect=lambda name: "/usr/bin/" + name):
            settings = options("linux-riscv64", "/host/bin", "riscv64-linux-gnu-")
        for name in ("C_COMPILER", "CXX_COMPILER", "AR", "RANLIB", "STRIP"):
            self.assertTrue(Path(settings["CMAKE_" + name]).is_absolute())

    def sdk(self, prefix, target="linux-riscv64"):
        for directory in ("bin", "lib", "include/llvm/Config", "include/llvm-c"):
            (prefix / directory).mkdir(parents=True, exist_ok=True)
        header = bytearray(20)
        header[:6] = b"\x7fELF\x02\x01"
        header[18:20] = bundle.MACHINES[target].to_bytes(2, "little")
        for name in bundle.TOOLS:
            path = prefix / "bin" / name
            path.write_bytes(header)
            path.chmod(0o755)
        for name in ("include/llvm/Config/llvm-config.h", "include/llvm-c/Core.h", "lib/libLLVM.so"):
            (prefix / name).touch()
        (prefix / "bundle.json").write_text(json.dumps(bundle.identity(target)))

    @unittest.skipUnless(os.name == "posix", "Linux SDK executable permissions")
    def test_identity_architecture_and_header_failures(self):
        with tempfile.TemporaryDirectory() as folder:
            prefix = Path(folder) / "llvm"
            self.sdk(prefix)
            bundle.check_layout(prefix, "linux-riscv64")
            with self.assertRaisesRegex(ValueError, "identity"):
                bundle.check_layout(prefix, "linux-loong64")
            (prefix / "bin/llc").write_bytes(b"not an ELF")
            with self.assertRaisesRegex(ValueError, "ELF"):
                bundle.check_layout(prefix, "linux-riscv64")
            self.sdk(prefix)
            (prefix / "include/llvm-c/Core.h").unlink()
            with self.assertRaisesRegex(ValueError, "header"):
                bundle.check_layout(prefix, "linux-riscv64")

    @unittest.skipUnless(os.name == "posix", "Linux SDK executable permissions")
    def test_rejects_build_tree_paths_and_missing_backends(self):
        with tempfile.TemporaryDirectory() as folder:
            prefix = Path(folder) / "llvm"
            self.sdk(prefix)
            for responses, message in (
                ([bundle.PINS["LLVM_SOURCE_VERSION"], "X86"], "backend"),
                ([bundle.PINS["LLVM_SOURCE_VERSION"], " ".join(bundle.BACKENDS), "/old/build"], "relocatable"),
            ):
                runner = SimpleNamespace(run=Mock(side_effect=responses))
                with self.assertRaisesRegex(ValueError, message):
                    bundle.validate(runner, prefix, "linux-riscv64", "/sysroot")

    def test_pin_validation_precedes_download(self):
        runner = SimpleNamespace(run=Mock())
        entry = dict(bundle.identity("linux-riscv64"), url="https://example.invalid/sdk.tar.xz", sha256="0" * 64)
        for changes in ({"sha256": "unknown"}, {"url": "http://example.invalid/sdk"}, {"llvm_version": "22.1.0"}):
            with self.subTest(changes=changes), tempfile.TemporaryDirectory() as folder:
                with self.assertRaises(ValueError):
                    bundle.fetch_bundle(runner, entry | changes, Path(folder) / "sdk", "linux-riscv64", "/sysroot")
        runner.run.assert_not_called()

    def test_checksum_failure_does_not_install_or_execute(self):
        with tempfile.TemporaryDirectory() as folder:
            destination = Path(folder) / "sdk"
            def run(command, **kwargs):
                self.assertEqual(command[0], "curl")
                Path(command[-1]).write_bytes(b"corrupt archive")
            entry = dict(bundle.identity("linux-riscv64"), url="https://example.invalid/sdk.tar.xz", sha256="0" * 64)
            with self.assertRaisesRegex(ValueError, "checksum"):
                bundle.fetch_bundle(SimpleNamespace(run=run), entry, destination, "linux-riscv64", "/sysroot")
            self.assertFalse(destination.exists())

    def test_archive_cannot_write_outside_staging(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            source = root / "bad.tar.xz"
            with tarfile.open(source, "w:xz") as stream:
                item = tarfile.TarInfo("../../escape")
                stream.addfile(item)
            payload = source.read_bytes()
            def run(command, **kwargs):
                self.assertEqual(command[0], "curl")
                Path(command[-1]).write_bytes(payload)
            entry = dict(bundle.identity("linux-riscv64"), url="https://example.invalid/sdk.tar.xz", sha256=hashlib.sha256(payload).hexdigest())
            with self.assertRaises(tarfile.FilterError):
                bundle.fetch_bundle(SimpleNamespace(run=run), entry, root / "sdk", "linux-riscv64", "/sysroot")
            self.assertFalse((root / "escape").exists())
            self.assertFalse((root / "sdk").exists())

    @unittest.skipUnless(os.name == "posix", "Linux SDK executable permissions")
    def test_verified_archive_installs_without_any_source_build(self):
        with tempfile.TemporaryDirectory() as folder:
            physical = Path(folder) / "physical"
            physical.mkdir()
            root = Path(folder) / "alias"
            root.symlink_to(physical, target_is_directory=True)
            sdk = root / "original"
            self.sdk(sdk)
            archive = root / "sdk.tar.xz"
            with tarfile.open(archive, "w:xz") as stream:
                stream.add(sdk, arcname="llvm")
            payload = archive.read_bytes()
            calls = []
            def run(command, **kwargs):
                calls.append(command)
                if command[0] == "curl":
                    Path(command[-1]).write_bytes(payload)
                    return ""
                self.assertEqual(command[0], "qemu-riscv64")
                prefix = Path(command[3]).parent.parent
                return {
                    "--version": bundle.PINS["LLVM_SOURCE_VERSION"],
                    "--targets-built": " ".join(bundle.BACKENDS),
                    "--prefix": str(prefix),
                    "--includedir": str(prefix / "include"),
                    "--libdir": str(prefix / "lib"),
                    "--libnames": "libLLVM.so",
                    "--system-libs": "-lm",
                }[command[-1]]
            entry = dict(bundle.identity("linux-riscv64"), url="https://example.invalid/sdk.tar.xz", sha256=hashlib.sha256(payload).hexdigest())
            result = bundle.fetch_bundle(SimpleNamespace(run=run), entry, root / "installed", "linux-riscv64", "/sysroot")
            self.assertEqual(result, (root / "installed").resolve())
            bundle.check_layout(result, "linux-riscv64")
            self.assertFalse(any("cmake" in command for command in calls))


class PublicationTests(unittest.TestCase):
    def artifacts(self, root):
        from tools.ci.publish_toolchains import identity
        for target in bundle.MACHINES:
            entry = dict(identity(target), revision="r1")
            name = f"wave-llvm-{entry['llvm_version']}-{target}-r1.tar.xz"
            archive = root / name
            archive.write_bytes(target.encode())
            digest = bundle.sha256(archive)
            archive.with_name(name + ".sha256").write_text(f"{digest}  {name}\n")
            entry.update(filename=name, sha256=digest, size_bytes=archive.stat().st_size,
                         url=f"https://wave-lang.dev/downloads/toolchains/llvm/{entry['llvm_version']}/r1/{name}")
            archive.with_suffix("").with_suffix(".json").write_text(json.dumps(entry))

    def test_atomic_publication_is_idempotent_and_immutable(self):
        from tools.ci.publish_toolchains import prepare_publication
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            inputs, website = root / "inputs", root / "website"
            inputs.mkdir()
            self.artifacts(inputs)
            pins = prepare_publication(inputs, website)
            original = (website / "index.json").read_bytes()
            self.assertEqual(set(pins["bundles"]), set(bundle.MACHINES))
            self.assertEqual(prepare_publication(inputs, website), pins)
            self.assertEqual((website / "index.json").read_bytes(), original)
            artifact = next(inputs.glob("*.tar.xz"))
            artifact.write_bytes(b"changed")
            with self.assertRaisesRegex(ValueError, "checksum"):
                prepare_publication(inputs, website)
            self.assertEqual((website / "index.json").read_bytes(), original)

    def test_missing_architecture_and_bad_url_publish_nothing(self):
        from tools.ci.publish_toolchains import prepare_publication
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            inputs, website = root / "inputs", root / "website"
            inputs.mkdir()
            self.artifacts(inputs)
            next(inputs.glob("*.json")).unlink()
            with self.assertRaisesRegex(ValueError, "both"):
                prepare_publication(inputs, website)
            self.assertFalse(website.exists())
            self.artifacts(inputs)
            meta = next(inputs.glob("*.json"))
            entry = json.loads(meta.read_text())
            entry["url"] = "https://outside.invalid/file"
            meta.write_text(json.dumps(entry))
            with self.assertRaisesRegex(ValueError, "URL"):
                prepare_publication(inputs, website)
            self.assertFalse(website.exists())





@unittest.skipUnless(os.name == "posix", "symlink fixture")
class LinkPolicyTests(unittest.TestCase):
    def test_full_tree_rejects_absolute_outside_dangling_cycles_and_directory_links(
        self,
    ):
        for target in ("/etc/passwd", "../../outside", "missing", "link", "."):
            with self.subTest(target=target), tempfile.TemporaryDirectory() as folder:
                root = Path(folder) / "sdk"
                root.mkdir()
                (root / "link").symlink_to(target)
                with self.assertRaises(ValueError):
                    bundle.check_tree(root)

    def test_relative_multilevel_links_and_outside_intermediate_hops(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder) / "sdk"
            (root / "lib").mkdir(parents=True)
            (root / "lib/real").write_bytes(b"library")
            (root / "lib/first").symlink_to("real")
            (root / "second").symlink_to("lib/first")
            bundle.check_tree(root)
            (root / "second").unlink()
            (root / "second").symlink_to("../sdk/lib/real")
            with self.assertRaises(ValueError):
                bundle.check_tree(root)

    def test_file_parent_components_are_not_treated_as_directories(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            (root / "file").write_bytes(b"library")
            (root / "link").symlink_to("file/../file")
            with self.assertRaises(ValueError):
                bundle.check_tree(root)


if __name__ == "__main__":
    unittest.main()
