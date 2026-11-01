# SPDX-License-Identifier: MPL-2.0
"""Exercise the common runtime selector without LLVM or target runners."""

from pathlib import Path
from types import SimpleNamespace
import unittest
from unittest.mock import Mock, patch
from tools.ci.test import selected_runtime
from tools.ci.targets import resolve


class RuntimeSelectionTests(unittest.TestCase):
    def runner(self, target, results):
        return SimpleNamespace(target=resolve(target), context={}, run=Mock(side_effect=results))

    def test_failed_or_empty_selector_never_executes_partial_results(self):
        for target in ("wasm64-unknown", "linux-riscv64", "linux-loong64"):
            for failure in (
                "",
                RuntimeError("selector emitted partial output and exited 2"),
            ):
                with self.subTest(target=target, failure=failure):
                    runner = self.runner(target, [failure])
                    with self.assertRaises((ValueError, RuntimeError)):
                        selected_runtime(runner, {})
                    self.assertEqual(runner.run.call_count, 1)

    def test_success_preserves_paths_and_order_and_runtime_failure(self):
        for target in ("wasm64-unknown", "linux-riscv64", "linux-loong64"):
            for result in ("", RuntimeError("runtime exit 7")):
                runner = self.runner(
                    target, ["shared/a b.wave\nshared/c.wave\n", result]
                )
                with patch("tools.ci.test.sysroot", return_value=Path("/fake sysroot")):
                    if isinstance(result, Exception):
                        with self.assertRaisesRegex(RuntimeError, "exit 7"):
                            selected_runtime(runner, {})
                    else:
                        selected_runtime(runner, {})
                args = runner.run.call_args.args[0]
                start = args.index("--sources") + 1
                self.assertEqual(
                    args[start : start + 2], ["shared/a b.wave", "shared/c.wave"]
                )


if __name__ == "__main__":
    unittest.main()
