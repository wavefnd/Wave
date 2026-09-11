# This file is part of the Wave language project.
# Copyright (c) 2024–2026 Wave Foundation
# Copyright (c) 2024–2026 LunaStev and contributors
#
# This Source Code Form is subject to the terms of the
# Mozilla Public License, v. 2.0.
# If a copy of the MPL was not distributed with this file,
# You can obtain one at https://mozilla.org/MPL/2.0/.
#
# SPDX-License-Identifier: MPL-2.0
# AI TRAINING NOTICE: Prohibited without prior written permission. No use for machine learning or generative AI training, fine-tuning, distillation, embedding, or dataset creation.

import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

from tools.test_contracts import parse_test_metadata, validate_compiled_artifact


def make_elf(machine=243, bits=64, flags=4):
    size = 64 if bits == 64 else 52
    data = bytearray(size)
    data[:4] = b"\x7fELF"
    data[4] = 2 if bits == 64 else 1
    data[5] = 1
    data[18:20] = machine.to_bytes(2, "little")
    flags_offset = 48 if bits == 64 else 36
    data[flags_offset:flags_offset + 4] = flags.to_bytes(4, "little")
    return data


class MetadataContractTests(unittest.TestCase):
    def parse(self, directory, body):
        source = Path(directory) / "case.wave"
        source.write_text(f"// wave-test: {body}\nfun main() {{}}\n")
        return parse_test_metadata(source, "case.wave")

    def test_rejects_malformed_duplicate_unknown_and_empty_metadata(self):
        invalid = [
            "mode:build",
            "mode=build, mode=build",
            "mode=build, unknown-contract=yes",
            "mode=build, runner=compile, asm-contains=",
            "mode=build, runner=compile, freestanding=maybe",
        ]
        with tempfile.TemporaryDirectory() as directory:
            for body in invalid:
                with self.subTest(body=body):
                    with self.assertRaises(ValueError):
                        self.parse(directory, body)

    def test_build_and_compile_runner_are_a_single_contract(self):
        invalid = [
            "mode=build",
            "runner=compile",
            "mode=run, runner=compile",
            "mode=build, runner=native",
            "mode=build, runner=compile, host-arch=riscv64",
            "mode=run, emit=asm",
            "mode=check, freestanding=false",
        ]
        with tempfile.TemporaryDirectory() as directory:
            for body in invalid:
                with self.subTest(body=body):
                    with self.assertRaises(ValueError):
                        self.parse(directory, body)

    def test_runtime_fixtures_are_explicit_metadata(self):
        with tempfile.TemporaryDirectory() as directory:
            stdin = self.parse(directory, "stdin=10")
            self.assertEqual(stdin.stdin, "10")
            self.assertEqual(stdin.runner, "native")

            server = self.parse(directory, "runner=server")
            self.assertEqual(server.runner, "server")

            for body in (
                "runner=server, stdin=10",
                "runner=server, udp-input=true",
                "mode=check, runner=server",
                "mode=check, stdin=10",
            ):
                with self.subTest(body=body):
                    with self.assertRaises(ValueError):
                        self.parse(directory, body)

    def test_artifact_expectations_require_matching_emit_and_architecture(self):
        invalid = [
            "mode=build, runner=compile, emit=asm, object-arch=riscv64",
            "mode=build, runner=compile, emit=obj, asm-contains=ecall",
            "mode=build, runner=compile, emit=obj, riscv-float-abi=lp64d",
            "mode=build, runner=compile, emit=obj, object-arch=x86_64, riscv-float-abi=lp64d",
            "mode=build, runner=compile, emit=obj, object-bits=128",
            "mode=build, runner=compile, emit=asm, expected-exit=1, asm-contains=ecall",
        ]
        with tempfile.TemporaryDirectory() as directory:
            for body in invalid:
                with self.subTest(body=body):
                    with self.assertRaises(ValueError):
                        self.parse(directory, body)

    def test_utf8_metadata_read_with_non_ascii_comments(self):
        with tempfile.TemporaryDirectory() as directory:
            source = Path(directory) / "case.wave"
            source.write_bytes(
                "// wave-test: mode=check\n// non-ascii comment: 測試 é\nfun main() {}\n".encode(
                    "utf-8"
                )
            )
            metadata = parse_test_metadata(source, "cases/shared/test1.wave")
            self.assertEqual(metadata.mode, "check")

    def test_utf8_metadata_read_independent_of_ascii_host_locale(self):
        with tempfile.TemporaryDirectory() as directory:
            source = Path(directory) / "case.wave"
            source.write_bytes(
                "// wave-test: mode=check\n// non-ascii comment: 測試 é\nfun main() {}\n".encode(
                    "utf-8"
                )
            )
            command = [
                sys.executable,
                "-c",
                "import sys; from pathlib import Path; "
                "from tools.test_contracts import parse_test_metadata; "
                "meta = parse_test_metadata(Path(sys.argv[1]), 'case.wave'); "
                "sys.stdout.write(meta.mode)",
                str(source),
            ]
            env = {**os.environ, "LC_ALL": "C", "PYTHONUTF8": "0"}
            process = subprocess.run(
                command,
                capture_output=True,
                text=True,
                env=env,
            )
            self.assertEqual(process.returncode, 0, process.stderr)
            self.assertEqual(process.stdout, "check")

    def test_invalid_utf8_bytes_reports_display_path_in_value_error(self):
        with tempfile.TemporaryDirectory() as directory:
            source = Path(directory) / "case.wave"
            source.write_bytes(b"// wave-test: mode=check\n// \xff\n")
            with self.assertRaises(ValueError) as context:
                parse_test_metadata(source, "shared/case.wave")
            self.assertIn("failed to read wave-test metadata from shared/case.wave:", str(context.exception))
            self.assertIsInstance(context.exception.__cause__, UnicodeDecodeError)

            # Also verify fallback to str(source) when display_path is omitted
            with self.assertRaises(ValueError) as context_default:
                parse_test_metadata(source)
            self.assertIn(f"failed to read wave-test metadata from {source}:", str(context_default.exception))
            self.assertIsInstance(context_default.exception.__cause__, UnicodeDecodeError)


class ArtifactContractTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        self.outputs = self.root / "outputs"

    def tearDown(self):
        self.temporary.cleanup()

    def prepare(self, body, suffix, contents):
        source = self.root / "case.wave"
        source.write_text(f"// wave-test: {body}\nfun main() {{}}\n")
        metadata = parse_test_metadata(source, "case.wave")
        artifact_dir = self.outputs / "case"
        artifact_dir.mkdir(parents=True, exist_ok=True)
        artifact = artifact_dir / f"case{suffix}"
        if isinstance(contents, str):
            artifact.write_text(contents)
        else:
            artifact.write_bytes(contents)
        return source, metadata

    def validate(self, source, metadata):
        return validate_compiled_artifact("case", source, self.outputs, metadata)

    def test_rejects_wrong_elf_machine_class_and_riscv_abi(self):
        body = (
            "mode=build, runner=compile, emit=obj, object-arch=riscv64, "
            "object-bits=64, riscv-float-abi=lp64d"
        )
        cases = [
            (make_elf(machine=62), "ELF machine mismatch"),
            (make_elf(bits=32), "ELF class mismatch"),
            (make_elf(flags=0), "RISC-V float ABI mismatch"),
        ]
        for contents, expected in cases:
            with self.subTest(expected=expected):
                source, metadata = self.prepare(body, ".o", contents)
                self.assertIn(expected, self.validate(source, metadata))

    def test_accepts_matching_riscv64_lp64d_object(self):
        body = (
            "mode=build, runner=compile, emit=obj, object-arch=riscv64, "
            "object-bits=64, riscv-float-abi=lp64d"
        )
        source, metadata = self.prepare(body, ".o", make_elf())
        self.assertIsNone(self.validate(source, metadata))

    def test_validates_loongarch64_lp64d_object_flags(self):
        body = (
            "mode=build, runner=compile, emit=obj, object-arch=loongarch64, "
            "object-bits=64, loongarch-float-abi=lp64d"
        )
        source, metadata = self.prepare(body, ".o", make_elf(machine=258, flags=0x43))
        self.assertIsNone(self.validate(source, metadata))

        artifact = self.outputs / "case" / "case.o"
        artifact.write_bytes(make_elf(machine=258, flags=0x42))
        self.assertIn("LoongArch float ABI mismatch", self.validate(source, metadata))

    def test_aarch64_immediates_survive_target_specific_comment_filtering(self):
        for target, comment in (
            ("aarch64-unknown-linux-gnu", "//"),
            ("aarch64-pc-windows-msvc", "//"),
            ("aarch64-apple-darwin", ";"),
        ):
            with self.subTest(target=target):
                body = (
                    f"mode=build, runner=compile, emit=asm, target={target}, "
                    "asm-contains=#16, asm-not-contains=brk|#32"
                )
                source, metadata = self.prepare(body, ".s",
                    f"{comment} brk #32\nlabel: {comment} brk\n"
                    f".word 16 {comment} brk\nadd x0, x0, #16 {comment} brk #32\n"
                    "/* brk #32\n continued comment */ ret\n")
                self.assertIsNone(self.validate(source, metadata))
                artifact = self.outputs / "case" / "case.s"
                artifact.write_text("add x0, x0, #32\n")
                self.assertIn("missing instruction token '#16'", self.validate(source, metadata))
                artifact.write_text("add x0, x0, #16\nbrk #32\n")
                self.assertIn("unexpectedly contains", self.validate(source, metadata))

    def test_hash_comments_stay_comments_for_other_supported_architectures(self):
        for target, instruction, immediate, forbidden in (
            ("x86_64-unknown-linux-gnu", "addq $16, %rax", "$16", "int3"),
            ("riscv64-unknown-linux-gnu", "addi a0, a0, 16", "16", "ebreak"),
            ("loongarch64-unknown-linux-gnu", "addi.d $a0, $a0, 16", "16", "break"),
        ):
            with self.subTest(target=target):
                body = (
                    f"mode=build, runner=compile, emit=asm, target={target}, "
                    f"asm-contains={immediate}, asm-not-contains={forbidden}"
                )
                source, metadata = self.prepare(body, ".s",
                    f"# {forbidden}\n{instruction} # {forbidden}\n")
                self.assertIsNone(self.validate(source, metadata))

    def test_assembly_dialect_comes_from_effective_target(self):
        source, metadata = self.prepare(
            "mode=build, runner=compile, emit=asm, asm-contains=#16", ".s",
            "add x0, x0, #16 // comment\n")
        self.assertIsNone(validate_compiled_artifact(
            "case", source, self.outputs, metadata, target="aarch64-unknown-linux-gnu"))
        self.assertIn("target", self.validate(source, metadata))

    def test_assembly_patterns_ignore_comments_directives_and_labels(self):
        body = (
            "mode=build, runner=compile, emit=asm, target=riscv64-unknown-linux-gnu, "
            "asm-contains=ecall|a7, asm-not-contains=ebreak"
        )
        source, metadata = self.prepare(
            body,
            ".s",
            "# ecall a7\n.attribute arch, \"rv64\"\na7:\n",
        )
        self.assertIn("missing instruction token 'ecall'", self.validate(source, metadata))

        artifact = self.outputs / "case" / "case.s"
        artifact.write_text("li a7, 64\necall\n")
        self.assertIsNone(self.validate(source, metadata))

        artifact.write_text("li a7, 64\necall\nebreak\n")
        self.assertIn("unexpectedly contains", self.validate(source, metadata))


if __name__ == "__main__":
    unittest.main()
