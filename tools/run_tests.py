#!/usr/bin/env python3
import subprocess
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
TEST_DIR = ROOT / "test"
WAVEC = ROOT / "target" / "release" / "wavec"

TIMEOUT_SEC = 5

GREEN = "\033[92m"
RED = "\033[91m"
YELLOW = "\033[93m"
BLUE = "\033[94m"
RESET = "\033[0m"

if not WAVEC.exists():
    print("wavec not found. Run `cargo build --release` first.")
    exit(1)

results = []

def run_and_classify(name, cmd):
    print(f"{BLUE}RUN {name}{RESET}")

    try:
        result = subprocess.run(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=TIMEOUT_SEC
        )

        if result.returncode == 0:
            print(f"{GREEN}→ PASS{RESET}\n")
            return 1
        else:
            print(f"{RED}→ FAIL (exit={result.returncode}){RESET}\n")
            return 0

    except subprocess.TimeoutExpired:
        print(f"{YELLOW}→ TIMEOUT ({TIMEOUT_SEC}s){RESET}\n")
        return -1


# test*.wave
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

# 집계
pass_tests = [name for name, r in results if r == 1]
fail_tests = [name for name, r in results if r == 0]
timeout_tests = [name for name, r in results if r == -1]

# 최종 출력
print("\n=========================")
print("🎉 FINAL TEST RESULT")
print("=========================\n")

print(f"{GREEN}PASS ({len(pass_tests)}){RESET}")
for name in pass_tests:
    print(f"  - {name}")

print(f"\n{RED}FAIL ({len(fail_tests)}){RESET}")
for name in fail_tests:
    print(f"  - {name}")

print(f"\n{YELLOW}TIMEOUT ({len(timeout_tests)}){RESET}")
for name in timeout_tests:
    print(f"  - {name}")

print("\n=========================")
print(f"{GREEN}PASS: {len(pass_tests)}{RESET}")
print(f"{RED}FAIL: {len(fail_tests)}{RESET}")
print(f"{YELLOW}TIMEOUT: {len(timeout_tests)}{RESET}")
print("=========================\n")
