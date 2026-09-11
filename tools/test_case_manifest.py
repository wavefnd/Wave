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

import tempfile
import unittest
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch

from tools import case_manifest
from tools.case_manifest import (
    MIN_CASES_PER_SUITE,
    CaseManifestError,
    _validate_case_layout,
    load_case_manifest,
)


class CaseManifestTests(unittest.TestCase):
    def setUp(self):
        self.manifest = load_case_manifest()

    def test_native_hosts_select_shared_and_platform_suites(self):
        linux = self.manifest.native_target("linux", "amd64")
        self.assertEqual(
            linux.suites,
            ("shared", "shared/amd64", "linux/amd64"),
        )

        macos = self.manifest.native_target("macos", "arm64")
        self.assertEqual(
            macos.suites,
            ("shared", "shared/arm64", "macos/arm64"),
        )

        windows = self.manifest.native_target("windows", "arm64")
        self.assertEqual(
            windows.suites,
            ("shared", "shared/arm64", "windows/arm64"),
        )
        self.assertEqual(windows.ci_group, "windows_arm64")

    def test_disabled_roadmap_targets_do_not_enter_ci(self):
        matrices = self.manifest.github_matrices()
        ci_ids = {
            entry["id"]
            for matrix in matrices.values()
            for entry in matrix["include"]
        }
        for target_id in (
            "openbsd-amd64",
            "netbsd-arm64",
            "fuchsia-riscv64",
            "dragonfly-amd64",
            "wasi-wasm64",
        ):
            target = self.manifest.target(target_id)
            self.assertFalse(target.enabled)
            self.assertFalse(target.ci)
            self.assertNotIn(target_id, ci_ids)

    def test_planned_operating_systems_reserve_only_existing_or_announced_ports(self):
        planned = {
            "openbsd": {"amd64", "arm64", "riscv64"},
            "netbsd": {"amd64", "arm64", "riscv64"},
            "fuchsia": {"amd64", "arm64", "riscv64"},
            "android": {"amd64", "arm64", "riscv64"},
            "ios": {"amd64", "arm64"},  # amd64 is simulator-only.
            "dragonfly": {"amd64"},
        }
        for os_name, arches in planned.items():
            self.assertEqual(
                {target.arch for target in self.manifest.targets if target.os == os_name},
                arches,
            )
            for arch in arches:
                with self.subTest(os=os_name, arch=arch):
                    target = self.manifest.target(f"{os_name}-{arch}")
                    self.assertEqual(target.suite, f"{os_name}/{arch}")
                    self.assertEqual(target.status, "planned")
                    self.assertFalse(target.enabled)
                    self.assertFalse(target.ci)

    def test_unannounced_riscv_os_ports_are_not_reserved_or_discovered(self):
        cases_root = Path(__file__).resolve().parents[1] / "tests" / "cases"
        for os_name in ("macos", "windows", "ios"):
            with self.subTest(os=os_name):
                with self.assertRaises(CaseManifestError):
                    self.manifest.target(f"{os_name}-riscv64")
                self.assertFalse((cases_root / os_name / "riscv64").exists())
        self.assertTrue(self.manifest.target("linux-riscv64").enabled)
        self.assertTrue(self.manifest.target("freebsd-riscv64").enabled)

    def test_freebsd_lp64_targets_compile_platform_provider_cases(self):
        matrix = self.manifest.github_matrices()["cross"]["include"]
        for arch, isa in (("amd64", "x86_64"), ("arm64", "aarch64"), ("riscv64", "riscv64")):
            target = self.manifest.target(f"freebsd-{arch}")
            self.assertTrue(target.enabled)
            self.assertTrue(target.ci)
            self.assertEqual(target.executor, "compile")
            self.assertEqual(target.target, f"{isa}-unknown-freebsd")
            self.assertIn(f"freebsd/{arch}", target.suites)
            self.assertIn(target.id, {entry["id"] for entry in matrix})

    def test_wasm64_is_enabled_without_claiming_a_wasi64_abi(self):
        bare = self.manifest.target("wasm64-unknown")
        self.assertTrue(bare.enabled)
        self.assertTrue(bare.ci)
        self.assertEqual(bare.target, "wasm64-unknown-unknown")
        self.assertEqual(
            bare.suites,
            ("shared", "shared/wasm64", "wasm/wasm64"),
        )

        wasi = self.manifest.target("wasi-wasm64")
        self.assertFalse(wasi.enabled)
        self.assertFalse(wasi.ci)

    def test_named_processor_roadmap_cells_are_planned_only(self):
        cells = (
            ("linux", "rhea"),
            ("linux", "shakti"),
            ("linux", "xiangshan"),
            ("linux", "t-head"),
        )
        for os_name, arch in cells:
            with self.subTest(os=os_name, arch=arch):
                target = self.manifest.target(f"{os_name}-{arch}")
                self.assertEqual(target.status, "planned")
                self.assertFalse(target.enabled)
                self.assertFalse(target.ci)

    def test_linux_loong64_is_enabled_under_qemu(self):
        target = self.manifest.target("linux-loong64")
        self.assertEqual(target.status, "supported")
        self.assertTrue(target.enabled)
        self.assertTrue(target.ci)
        self.assertEqual(target.executor, "qemu")
        self.assertEqual(target.ci_group, "loongarch")
        self.assertEqual(target.target, "loongarch64-unknown-linux-gnu")
        self.assertEqual(target.smoke_case, "linux/loong64/test2.wave")
        self.assertEqual(target.smoke_stdout, "loongarch workload=pass")
        self.assertEqual(
            target.suites,
            ("shared", "shared/loong64", "linux/loong64"),
        )

    def test_unknown_target_is_rejected(self):
        with self.assertRaises(CaseManifestError):
            self.manifest.target("missing-target")

    def test_every_configured_suite_has_the_baseline_case_count(self):
        counts = {}
        for source in self.manifest.sources():
            suite = source.rsplit("/", 1)[0]
            if source.endswith("/main.wave"):
                suite = suite.rsplit("/", 1)[0]
            counts[suite] = counts.get(suite, 0) + 1

        for target in self.manifest.targets:
            self.assertGreaterEqual(counts[target.suite], MIN_CASES_PER_SUITE)

    def test_case_layout_accepts_platform_metadata_in_literals_and_comments(self):
        cases = (
            'fun main() { println("host-os=linux"); }\n',
            'fun main() { var arch = "host-arch=arm64"; }\n',
            '// Ordinary comment: host-arch=arm64\nfun main() {}\n',
            '// Note: host-os=windows is handled elsewhere\nfun main() {}\n',
        )
        for content in cases:
            with self.subTest(content=content):
                with tempfile.TemporaryDirectory() as temporary:
                    root = Path(temporary)
                    suite = root / "shared"
                    suite.mkdir()
                    for number in range(1, 11):
                        (suite / f"test{number}.wave").write_text(content, encoding="utf-8")
                    target = SimpleNamespace(suite="shared", suites=())
                    with patch.object(case_manifest, "CASES_ROOT", root):
                        _validate_case_layout((target,))

    def test_case_layout_rejects_legacy_platform_metadata_in_wave_test_directives(self):
        invalid_cases = (
            "// wave-test: host-os=linux\nfun main() {}\n",
            "// wave-test: mode=check, host-arch=x86_64\nfun main() {}\n",
            "// wave-test: host-os = linux\nfun main() {}\n",
            "// wave-test: mode=check, host-arch = arm64\nfun main() {}\n",
        )
        for content in invalid_cases:
            with self.subTest(content=content):
                with tempfile.TemporaryDirectory() as temporary:
                    root = Path(temporary)
                    suite = root / "shared"
                    suite.mkdir()
                    for number in range(1, 11):
                        (suite / f"test{number}.wave").write_text(
                            "fun main() {}\n",
                            encoding="utf-8",
                        )
                    (suite / "test1.wave").write_text(content, encoding="utf-8")
                    target = SimpleNamespace(suite="shared", suites=())
                    with patch.object(case_manifest, "CASES_ROOT", root):
                        with self.assertRaises(CaseManifestError) as context:
                            _validate_case_layout((target,))
                        self.assertEqual(
                            str(context.exception),
                            "case 'shared/test1.wave' uses legacy platform metadata; use its directory",
                        )


if __name__ == "__main__":
    unittest.main()
