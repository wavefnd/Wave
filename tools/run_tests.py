#!/usr/bin/env python3

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

import subprocess
import time
import argparse
from pathlib import Path
import threading
import socket
import sys
import platform
import shutil
import tempfile

ROOT = Path(__file__).resolve().parent.parent
TEST_DIR = ROOT / "test"

TIMEOUT_SEC = 5

GREEN = "\033[92m"
RED = "\033[91m"
YELLOW = "\033[93m"
BLUE = "\033[94m"
CYAN = "\033[96m"
MAGENTA = "\033[95m"
RESET = "\033[0m"

KNOWN_TIMEOUT = {
    # "test22.wave",
}

FAIL_PATTERNS = [
    "WaveError",
    "WaveErrorKind",
    "SyntaxError",
    "error[E",
    "failed to parse",
    "Failed to run",
    "llc failed:",
    "compiler internal error during code generation",
    "thread 'main' panicked",
    "panicked at",
    "LLVM ERROR",
    "Segmentation fault",
    "stack overflow",
]

def resolve_wavec() -> Path:
    candidates = [
        ROOT / "target" / "release" / "wavec",
        ROOT / "target" / "debug" / "wavec",
    ]
    for candidate in candidates:
        if candidate.exists():
            return candidate

    print("wavec not found. Run `cargo build --release` or `cargo build` first.")
    sys.exit(1)


WAVEC = resolve_wavec()

results = []

HOST_OS = platform.system().lower()
HOST_ARCH = platform.machine().lower()


def normalize_arch(arch: str) -> str:
    aliases = {
        "amd64": "x86_64",
        "arm64": "aarch64",
    }
    return aliases.get(arch.lower(), arch.lower())


HOST_ARCH = normalize_arch(HOST_ARCH)
TEST_OUTPUT_DIR = Path(tempfile.mkdtemp(prefix="wave-test-output-"))


def parse_args():
    parser = argparse.ArgumentParser(description="Run Wave end-to-end tests")
    parser.add_argument(
        "--only",
        action="append",
        default=[],
        metavar="NAME",
        help="run only the named test; may be repeated",
    )
    parser.add_argument(
        "--skip",
        action="append",
        default=[],
        metavar="NAME",
        help="skip the named test; may be repeated",
    )
    return parser.parse_args()


ARGS = parse_args()


def iter_test_entries():
    for path in sorted(TEST_DIR.glob("test*.wave")):
        if (not ARGS.only or path.name in ARGS.only) and path.name not in ARGS.skip:
            yield path.name, path.relative_to(ROOT).as_posix()

    for main_wave in sorted(TEST_DIR.glob("test*/main.wave")):
        name = f"{main_wave.parent.name} (dir)"
        if (not ARGS.only or name in ARGS.only) and name not in ARGS.skip:
            yield name, main_wave.relative_to(ROOT).as_posix()


def parse_test_metadata(rel_path: str):
    path = ROOT / rel_path
    meta = {
        "host_os": None,
        "host_arch": None,
        "mode": "run",
        "target": None,
        "emit": "obj",
        "freestanding": False,
        "expected_exit": 0,
    }

    try:
        for line in path.read_text().splitlines():
            stripped = line.strip()
            if not stripped.startswith("//"):
                if stripped:
                    break
                continue

            marker = "// wave-test:"
            if not stripped.startswith(marker):
                continue

            body = stripped[len(marker):].strip()
            for item in body.split(","):
                item = item.strip()
                if not item or "=" not in item:
                    continue
                key, value = item.split("=", 1)
                key = key.strip()
                value = value.strip()
                if key == "host-os":
                    meta["host_os"] = value.lower()
                elif key == "host-arch":
                    meta["host_arch"] = normalize_arch(value)
                elif key == "mode":
                    meta["mode"] = value.lower()
                elif key == "target":
                    meta["target"] = value
                elif key == "emit":
                    meta["emit"] = value.lower()
                elif key == "freestanding":
                    meta["freestanding"] = value.lower() in {"1", "true", "yes"}
                elif key == "expected-exit":
                    meta["expected_exit"] = int(value)
    except OSError:
        pass

    return meta


def skip_reason_for_metadata(name: str, rel_path: str):
    meta = parse_test_metadata(rel_path)
    host_os = meta["host_os"]
    host_arch = meta["host_arch"]

    if host_os and host_os != HOST_OS:
        return f"{name} requires host OS {host_os}, current host is {HOST_OS}"

    if host_arch and host_arch != HOST_ARCH:
        return f"{name} requires host arch {host_arch}, current host is {HOST_ARCH}"

    return None


def command_for_test(name: str, rel_path: str):
    meta = parse_test_metadata(rel_path)
    mode = meta["mode"]

    if mode == "run":
        return [str(WAVEC), "run", rel_path]

    if mode == "check":
        return [str(WAVEC), "check", rel_path]

    if mode == "build":
        output_dir = TEST_OUTPUT_DIR / name.replace(" ", "-")
        output_dir.mkdir(parents=True, exist_ok=True)

        cmd = [
            str(WAVEC),
            "build",
            rel_path,
            f"--emit={meta['emit']}",
            "--out-dir",
            str(output_dir),
        ]
        if meta["target"]:
            cmd.extend(["--target", meta["target"]])
        if meta["freestanding"]:
            cmd.append("--freestanding")
        return cmd

    raise ValueError(f"unsupported wave-test mode '{mode}' in {rel_path}")


def send_udp_for_test61():
    time.sleep(0.5)
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        sock.sendto(b"hello from python\n", ("127.0.0.1", 8080))
        sock.close()
    except OSError:
        # Some CI/sandbox environments block local sockets.
        pass

def run_test56_server(cmd):
    print(f"{BLUE}RUN test56.wave (server test){RESET}")

    proc = subprocess.Popen(
        cmd,
        cwd=str(ROOT),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )

    try:
        time.sleep(1.0)  # server boot wait

        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.settimeout(2)
        s.connect(("127.0.0.1", 8080))
        s.sendall(b"GET / HTTP/1.1\r\nHost: localhost\r\n\r\n")

        data = s.recv(4096)
        s.close()

        if b"Welcome to the Wave HTTP Server!" in data:
            print(f"{GREEN}→ PASS (server responded){RESET}\n")
            return 1
        else:
            print(f"{RED}→ FAIL (unexpected response){RESET}")
            print(data)
            return 0

    except Exception as e:
        print(f"{RED}→ FAIL (server not responding){RESET}")
        print(e)
        return 0

    finally:
        proc.terminate()
        try:
            proc.wait(timeout=1)
        except subprocess.TimeoutExpired:
            proc.kill()


def looks_like_fail(stderr: str) -> bool:
    if not stderr:
        return False
    s = stderr.strip()
    if not s:
        return False
    s_low = s.lower()
    for p in FAIL_PATTERNS:
        if p.lower() in s_low:
            return True
    return False

# Return Type:
# 1 = PASS (exit 0)
# 3 = PASS (explicit expected nonzero exit)
# 0 = FAIL
# 2 = SKIP
# -1 = TIMEOUT
def run_and_classify(name, rel_path, cmd):
    print(f"{BLUE}RUN {name}{RESET}")

    skip_reason = skip_reason_for_metadata(name, rel_path)
    if skip_reason is not None:
        print(f"{CYAN}→ SKIP ({skip_reason}){RESET}\n")
        return 2

    expected_exit = parse_test_metadata(rel_path)["expected_exit"]

    stdin_data = None
    if name == "test22.wave":
        stdin_data = "3\n"

    if name == "test74.wave":
        stdin_data = "10\n"

    if name == "test56.wave":
        return run_test56_server(cmd)

    try:
        if name == "test61.wave":
            threading.Thread(
                target=send_udp_for_test61,
                daemon=True
            ).start()

        if name == "test62.wave":
            threading.Thread(
                target=send_udp_for_test61,
                daemon=True
            ).start()

        result = subprocess.run(
            cmd,
            cwd=str(ROOT),
            input=stdin_data,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=TIMEOUT_SEC
        )

        if looks_like_fail(result.stderr):
            print(f"{RED}→ FAIL (exit={result.returncode}){RESET}")
            if result.stdout.strip():
                print(f"{BLUE}--- STDOUT ---{RESET}")
                print(result.stdout.rstrip())
            if result.stderr.strip():
                print(f"{YELLOW}--- STDERR ---{RESET}")
                print(result.stderr.rstrip())
            print()
            return 0

        if result.returncode == expected_exit:
            if expected_exit != 0:
                print(f"{MAGENTA}→ PASS (expected exit={expected_exit}){RESET}\n")
                return 3
            print(f"{GREEN}→ PASS{RESET}\n")
            return 1

        print(
            f"{RED}→ FAIL (exit={result.returncode}, expected={expected_exit}){RESET}"
        )
        if result.stdout.strip():
            print(f"{BLUE}--- STDOUT ---{RESET}")
            print(result.stdout.rstrip())
        if result.stderr.strip():
            print(f"{YELLOW}--- STDERR ---{RESET}")
            print(result.stderr.rstrip())
        print()
        return 0

    except subprocess.TimeoutExpired:
        if name in KNOWN_TIMEOUT:
            print(f"{CYAN}→ SKIP (expected blocking / unimplemented){RESET}\n")
            return 2
        else:
            print(f"{YELLOW}→ TIMEOUT ({TIMEOUT_SEC}s){RESET}\n")
            return -1

entries = list(iter_test_entries())
selected_names = {name for name, _ in entries}
missing_names = sorted(set(ARGS.only) - selected_names)

if missing_names:
    shutil.rmtree(TEST_OUTPUT_DIR, ignore_errors=True)
    print(f"unknown test name(s): {', '.join(missing_names)}", file=sys.stderr)
    sys.exit(2)

if not entries:
    shutil.rmtree(TEST_OUTPUT_DIR, ignore_errors=True)
    print("no tests selected", file=sys.stderr)
    sys.exit(2)

try:
    for name, rel_path in entries:
        result = run_and_classify(
            name,
            rel_path,
            command_for_test(name, rel_path)
        )
        results.append((name, result))

        time.sleep(0.3)
except KeyboardInterrupt:
    print(f"\n{YELLOW}Interrupted by user.{RESET}")
    sys.exit(130)
finally:
    shutil.rmtree(TEST_OUTPUT_DIR, ignore_errors=True)

pass_zero = [name for name, r in results if r == 1]
pass_nonzero = [name for name, r in results if r == 3]
fail_tests = [name for name, r in results if r == 0]
timeout_tests = [name for name, r in results if r == -1]
skip_tests = [name for name, r in results if r == 2]

print("\n=========================")
print("🎉 FINAL TEST RESULT")
print("=========================\n")

print(f"{GREEN}PASS (exit=0) ({len(pass_zero)}){RESET}")
for name in pass_zero:
    print(f"  - {name}")

print(f"\n{MAGENTA}PASS (expected non-zero exit) ({len(pass_nonzero)}){RESET}")
for name in pass_nonzero:
    print(f"  - {name}")

print(f"\n{CYAN}SKIP ({len(skip_tests)}){RESET}")
for name in skip_tests:
    print(f"  - {name}")

print(f"\n{RED}FAIL ({len(fail_tests)}){RESET}")
for name in fail_tests:
    print(f"  - {name}")

print(f"\n{YELLOW}TIMEOUT ({len(timeout_tests)}){RESET}")
for name in timeout_tests:
    print(f"  - {name}")

print("\n=========================")
print(f"{GREEN}PASS(0): {len(pass_zero)}{RESET}")
print(f"{MAGENTA}PASS(expected !0): {len(pass_nonzero)}{RESET}")
print(f"{CYAN}SKIP: {len(skip_tests)}{RESET}")
print(f"{RED}FAIL: {len(fail_tests)}{RESET}")
print(f"{YELLOW}TIMEOUT: {len(timeout_tests)}{RESET}")
print("=========================\n")

if fail_tests or timeout_tests:
    sys.exit(1)
