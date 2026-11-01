# SPDX-License-Identifier: MPL-2.0
import contextlib
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

from tools import case_manifest
from tools.ci import release_notes
from tools.process_tree import run_process


class FinalReleaseTests(unittest.TestCase):
    def test_manifest_requires_case_ownership(self):
        raw = dict(
            id="linux-riscv64",
            os="linux",
            arch="riscv64",
            executor="qemu",
            target="riscv64-unknown-linux-gnu",
        )
        with tempfile.TemporaryDirectory() as folder, patch.object(
            case_manifest, "CASES_ROOT", Path(folder)
        ):
            root = Path(folder)
            valid = [
                "shared/test1.wave",
                "shared/riscv64/test2/main.wave",
                "linux/riscv64/test3.wave",
            ]
            invalid = [
                "shared/amd64/test4.wave",
                "macos/riscv64/test5.wave",
                "linux/riscv64/helper.wave",
            ]
            for name in valid + invalid:
                p = root / name
                p.parent.mkdir(parents=True, exist_ok=True)
                p.touch()
            for name in valid:
                case_manifest._parse_target(
                    dict(raw, smoke_case=name, smoke_stdout="ok", exclude=[name]),
                    True,
                    False,
                )
            for field in ("smoke_case", "exclude"):
                for name in invalid + ["linux/riscv64"]:
                    values = {field: [name] if field == "exclude" else name}
                    if field == "smoke_case":
                        values["smoke_stdout"] = "ok"
                    with self.subTest(field=field, path=name), self.assertRaises(
                        case_manifest.CaseManifestError
                    ):
                        case_manifest._parse_target(dict(raw, **values), True, False)

    def test_streaming_preserves_output_status_and_timeout(self):
        out, err = io.StringIO(), io.StringIO()
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
            result = run_process(
                [
                    sys.executable,
                    "-c",
                    "import sys;print('hello');print('error',file=sys.stderr);sys.exit(7)",
                ],
                capture_output=True,
                text=True,
                stream_output=True,
                timeout=10,
            )
        self.assertEqual(
            (result.returncode, result.stdout, result.stderr), (7, "hello\n", "error\n")
        )
        self.assertEqual(
            (out.getvalue(), err.getvalue()), (result.stdout, result.stderr)
        )
        with contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(
            io.StringIO()
        ):
            with self.assertRaises(subprocess.TimeoutExpired) as failure:
                run_process(
                    [
                        sys.executable,
                        "-c",
                        "import time;print('started',flush=True);time.sleep(10)",
                    ],
                    capture_output=True,
                    text=True,
                    stream_output=True,
                    timeout=1,
                )
        self.assertIn("started", failure.exception.output)

    @unittest.skipUnless(shutil.which("node"), "Node required")
    def test_smoke_runner_contracts(self):
        subprocess.run(
            ["node", "--test", str(Path(__file__).with_name("test_wasm_smoke.mjs"))],
            check=True,
            timeout=60,
        )

    def test_notes_credit_authors_and_coauthors_not_signers(self):
        commits = [
            {
                "author": {"login": "alice"},
                "commit": {
                    "author": {"name": "Alice", "email": "alice@example.invalid"},
                    "message": "Change\n\nCo-authored-by: Bob <bob@example.invalid>\nSigned-off-by: Reviewer <reviewer@example.invalid>\n",
                },
            },
            {
                "author": {"login": "bob"},
                "commit": {
                    "author": {"name": "Bob", "email": "bob@example.invalid"},
                    "message": "Change",
                },
            },
        ]
        self.assertEqual(
            release_notes.contributors({"user": {"login": "alice"}}, commits),
            ["@alice", "@bob"],
        )
        self.assertNotIn(
            "example.invalid", ",".join(release_notes.contributors({}, commits))
        )

    def test_notes_exact_range_ignores_nightly_drafts_and_unmerged_prs(self):
        sha = "a" * 40

        def api(endpoint):
            if endpoint.startswith("releases?"):
                return [
                    {"tag_name": "nightly", "published_at": "9"},
                    {"tag_name": "v9.0.0", "draft": True, "published_at": "8"},
                    {"tag_name": "v0.2.0-pre-beta", "published_at": "1"},
                ]
            if endpoint.startswith("compare/"):
                return {"status": "ahead", "commits": [{"sha": sha}]}
            if endpoint.startswith("commits/"):
                return [
                    {
                        "number": 1,
                        "merged_at": "today",
                        "base": {"ref": "master"},
                        "merge_commit_sha": sha,
                        "title": "Fix",
                        "user": {"login": "author"},
                    },
                    {"number": 2, "merged_at": None},
                ]
            if endpoint.startswith("pulls/"):
                return [
                    {
                        "author": {"login": "contributor"},
                        "commit": {"author": {}, "message": ""},
                    }
                ]
            raise AssertionError(endpoint)

        notes = release_notes.generate(api, sha)
        self.assertIn("by @author, @contributor", notes)
        self.assertIn("v0.2.0-pre-beta..." + sha, notes)
        self.assertNotIn("/pull/2", notes)

        def versioned_api(endpoint):
            # A not-yet-created tag must never be used for API comparisons.
            self.assertNotIn("v0.2.1-pre-beta", endpoint)
            return api(endpoint)

        versioned = release_notes.generate(
            versioned_api, sha, release_tag="v0.2.1-pre-beta"
        )
        self.assertEqual(
            versioned,
            notes.replace("..." + sha, "...v0.2.1-pre-beta"),
        )

    def test_notes_reject_invalid_versioned_tag(self):
        for tag in ("nightly", "master", "v1.2.3...master", "", "v1.2.3\n"):
            with self.subTest(tag=tag), self.assertRaises(ValueError):
                release_notes.generate(
                    lambda _: self.fail("invalid tag reached API"),
                    "a" * 40,
                    release_tag=tag,
                )

    def test_command_output_is_forwarded_before_process_exit(self):
        with tempfile.TemporaryDirectory() as directory:
            ack = Path(directory) / "ack"
            log = Path(directory) / "command.log"

            class AcknowledgingOutput(io.StringIO):
                def write(self, text):
                    if "ready" in text:
                        ack.touch()
                    return super().write(text)

            with contextlib.redirect_stdout(AcknowledgingOutput()):
                run_process(
                    [
                        sys.executable,
                        "-c",
                        "import pathlib,sys,time; print('ready',flush=True); "
                        "p=pathlib.Path(sys.argv[1]); "
                        "exec('while not p.exists(): time.sleep(0.01)')",
                        str(ack),
                    ],
                    capture_output=True,
                    text=True,
                    stream_output=True,
                    stream_log=log,
                    timeout=10,
                    check=True,
                )
            self.assertEqual(log.read_text(), "ready\n")

    def test_darwin_bundles_versioned_and_transitive_dependencies(self):
        from tools.test_msvc_package_tools import DRIVER

        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            stage = root / "package"
            stage.mkdir()
            compiler = stage / "wavec"
            compiler.write_bytes(b"compiler")
            llvm = root / "libLLVM.21.1.dylib"
            zstd = root / "libzstd.1.dylib"
            llvm.write_bytes(b"llvm")
            zstd.write_bytes(b"zstd")
            refs = {compiler: [str(llvm)], llvm: [str(zstd)], zstd: []}
            with patch.object(
                DRIVER, "dylib_references", side_effect=lambda p: refs[p]
            ), patch.object(DRIVER, "llvm_lib_dir", return_value=None), patch.object(
                DRIVER.subprocess, "run"
            ) as run:
                DRIVER.copy_darwin_dependency_closure(stage, [(compiler, compiler)])
            self.assertEqual((stage / "llvm/lib" / llvm.name).read_bytes(), b"llvm")
            self.assertEqual((stage / "llvm/lib" / zstd.name).read_bytes(), b"zstd")
            commands = [c.args[0] for c in run.call_args_list]
            self.assertIn(
                [
                    "install_name_tool",
                    "-change",
                    str(llvm),
                    "@executable_path/llvm/lib/" + llvm.name,
                    str(compiler),
                ],
                commands,
            )
            self.assertIn(
                [
                    "install_name_tool",
                    "-change",
                    str(zstd),
                    "@loader_path/" + zstd.name,
                    str(stage / "llvm/lib" / llvm.name),
                ],
                commands,
            )
            llvm.unlink()
            with patch.object(
                DRIVER, "dylib_references", return_value=[str(llvm)]
            ), patch.object(
                DRIVER, "llvm_lib_dir", return_value=None
            ), self.assertRaises(
                FileNotFoundError
            ):
                DRIVER.copy_darwin_dependency_closure(stage, [(compiler, compiler)])

    def test_darwin_resolves_loader_relative_and_rpath_libraries(self):
        from tools.test_msvc_package_tools import DRIVER

        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "bin").mkdir()
            (root / "lib").mkdir()
            lib = root / "lib/libLLVM.21.1.dylib"
            lib.touch()
            binary = root / "bin/ld64.lld"
            self.assertEqual(
                DRIVER.resolve_dylib_reference(
                    "@loader_path/../lib/" + lib.name, binary
                ).resolve(),
                lib.resolve(),
            )
            with patch.object(
                DRIVER.subprocess,
                "run",
                return_value=subprocess.CompletedProcess(
                    [],
                    0,
                    "cmd LC_RPATH\ncmdsize 48\npath @loader_path/../lib (offset 12)\n",
                ),
            ):
                self.assertEqual(
                    DRIVER.resolve_dylib_reference(
                        "@rpath/" + lib.name, binary
                    ).resolve(),
                    lib.resolve(),
                )

    def test_streaming_translates_split_crlf_like_captured_text(self):
        out = io.StringIO()
        with tempfile.TemporaryDirectory() as directory, contextlib.redirect_stdout(out):
            log = Path(directory) / "output.log"
            result = run_process(
                [sys.executable, "-c", "import os,time;os.write(1,b'a\\r');time.sleep(.2);os.write(1,b'\\nb\\rc\\n')"],
                capture_output=True, text=True, stream_output=True, stream_log=log,
                timeout=10, check=True,
            )
            self.assertEqual(result.stdout, "a\nb\nc\n")
            self.assertEqual(out.getvalue(), result.stdout)
            self.assertEqual(log.read_text(), result.stdout)

    def test_serial_host_key_waits_for_complete_record(self):
        import re
        from tools.ci.freebsd_package import console_host_key

        record = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBase64Fixture root@\r\n"
        owner = self
        class FragmentedConsole:
            def send(self, command):
                owner.assertIn("ssh_host_ed25519_key.pub", command)
            def expect(self, pattern, timeout):
                for index in range(1, len(record)):
                    owner.assertIsNone(re.search(pattern, record[:index]))
                return re.search(pattern, record)
        self.assertEqual(console_host_key(FragmentedConsole()), record.split(" root@")[0])

    def test_serial_bootstrap_uses_iso_script_and_checks_exit_status(self):
        import re
        from tools.ci.freebsd_package import bootstrap_guest

        owner = self
        class BootstrapConsole:
            def __init__(self, status):
                self.status = status
            def send(self, command):
                owner.assertLess(len(command.encode()), 160)
                owner.assertEqual(command.count("\n"), 1)
                owner.assertIn("sh /mnt/bootstrap.sh", command)
                self.echo = command.replace("\n", "\r\n")
            def expect(self, pattern, timeout):
                owner.assertIsNone(re.search(pattern, self.echo))
                return re.search(pattern, f"\r\nWAVE-SSH-STATUS {self.status}\r\n")

        bootstrap_guest(BootstrapConsole(0))
        with self.assertRaisesRegex(RuntimeError, "exit status 7"):
            bootstrap_guest(BootstrapConsole(7))

    @unittest.skipUnless(os.name == "posix", "POSIX guest bootstrap script")
    def test_freebsd_bootstrap_grows_mounted_root_and_stops_on_failure(self):
        from tools.ci.freebsd_package import BOOTSTRAP_SCRIPT

        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            commands = {
                "gpart": "[ \"$1\" != show ] || echo '1 2 vtbd0p4 freebsd-ufs'\nexit 0\n",
                "growfs": 'printf "%s\\n" "$*" > "$CALLS"\nexit 7\n',
            }
            for name, body in commands.items():
                path = root / name
                path.write_text("#!/bin/sh\n" + body)
                path.chmod(0o755)
            calls = root / "calls"
            result = subprocess.run(
                ["sh", "-c", BOOTSTRAP_SCRIPT],
                env=dict(os.environ, PATH=str(root) + os.pathsep + os.environ["PATH"], CALLS=str(calls)),
                capture_output=True, text=True, timeout=10,
            )
            self.assertEqual(result.returncode, 7, result.stderr)
            self.assertEqual(calls.read_text(), "-y /\n")

    @unittest.skipUnless(os.name == "posix", "POSIX cross LLVM wrapper")
    def test_cross_llvm_sys_uses_the_explicit_config_wrapper(self):
        from tools.build import cargo_env as DRIVER
        with tempfile.TemporaryDirectory(prefix="cross config ") as directory:
            root = Path(directory)
            library = root / "libLLVM.so.21.1"
            library.touch()
            config = root / "config"
            config.write_text("#!/bin/sh\ncase \"$1\" in\n--libnames) echo libLLVM.so.21.1;;\n--libdir) dirname \"$0\";;\n--system-libs) echo;;\nesac\n")
            config.chmod(0o755)
            env = dict(os.environ, LLVM_CONFIG_PATH=str(config), LLVM_SYS_211_PREFIX="wrong-host")
            with patch.object(DRIVER, "TARGET_DIR", root / "build"):
                DRIVER.configure_cross_llvm_env(env, "riscv64gc-unknown-linux-gnu")
            wrapper = Path(env["LLVM_SYS_211_PREFIX"]) / "bin/llvm-config"
            result = subprocess.run([wrapper, "--libnames", "--link-shared"], check=True, capture_output=True, text=True)
            self.assertEqual(result.stdout.strip(), library.name)
            library.unlink()
            with patch.object(DRIVER, "TARGET_DIR", root / "build"), self.assertRaises(RuntimeError):
                DRIVER.configure_cross_llvm_env(env, "riscv64gc-unknown-linux-gnu")
