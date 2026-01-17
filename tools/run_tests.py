#!/usr/bin/env python3
import subprocess
import time
from pathlib import Path
import threading
import socket

ROOT = Path(__file__).resolve().parent.parent
TEST_DIR = ROOT / "test"
WAVEC = ROOT / "target" / "release" / "wavec"

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
    "failed to parse",
    "Failed to run",
    "thread 'main' panicked",
    "panicked at",
    "LLVM ERROR",
    "Segmentation fault",
    "stack overflow",
]

if not WAVEC.exists():
    print("wavec not found. Run `cargo build --release` first.")
    exit(1)

results = []

def send_udp_for_test61():
    time.sleep(0.5)
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.sendto(b"hello from python\n", ("127.0.0.1", 8080))
    sock.close()

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
# 3 = PASS (exit nonzero)
# 0 = FAIL
# 2 = SKIP
# -1 = TIMEOUT
def run_and_classify(name, cmd):
    print(f"{BLUE}RUN {name}{RESET}")

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

        if result.returncode == 0:
            print(f"{GREEN}→ PASS{RESET}\n")
            return 1

        print(f"{MAGENTA}→ PASS (non-zero exit={result.returncode}){RESET}\n")
        return 3

    except subprocess.TimeoutExpired:
        if name in KNOWN_TIMEOUT:
            print(f"{CYAN}→ SKIP (expected blocking / unimplemented){RESET}\n")
            return 2
        else:
            print(f"{YELLOW}→ TIMEOUT ({TIMEOUT_SEC}s){RESET}\n")
            return -1

for path in sorted(TEST_DIR.glob("test*.wave")):
    name = path.name
    cmd = [str(WAVEC), "run", f"test/{name}"]

    result = run_and_classify(name, cmd)
    results.append((name, result))

    time.sleep(0.3)

test28 = TEST_DIR / "test28" / "main.wave"
if test28.exists():
    result = run_and_classify(
        "test28 (dir)",
        [str(WAVEC), "run", "test/test28/main.wave"]
    )
    results.append(("test28 (dir)", result))

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

print(f"\n{MAGENTA}PASS (non-zero exit) ({len(pass_nonzero)}){RESET}")
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
print(f"{MAGENTA}PASS(!0): {len(pass_nonzero)}{RESET}")
print(f"{CYAN}SKIP: {len(skip_tests)}{RESET}")
print(f"{RED}FAIL: {len(fail_tests)}{RESET}")
print(f"{YELLOW}TIMEOUT: {len(timeout_tests)}{RESET}")
print("=========================\n")
