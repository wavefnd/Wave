#!/usr/bin/env python3
import os
import sys
import subprocess
from pathlib import Path
import toml
import shutil
import platform

# ------------------------------------------------------
#  Basic Setting
# ------------------------------------------------------

ROOT = Path(__file__).resolve().parent
TARGET_DIR = ROOT / "target"
BINARY_NAME = "wavec"
NAME = "wave"

def detect_targets():
    os_name = platform.system()

    if os_name == "Darwin":
        return ["aarch64-apple-darwin"]

    if os_name == "Linux":
        return [
            "x86_64-unknown-linux-gnu",
            "x86_64-pc-windows-gnu",
        ]

    print("Unsupported build environment:", os_name)
    sys.exit(1)

TARGETS = detect_targets()

# Version Read
def get_version():
    cargo = toml.load(str(ROOT / "Cargo.toml"))
    return cargo["package"]["version"]

VERSION = get_version()

# ------------------------------------------------------
# rustup target add
# ------------------------------------------------------
def cmd_install():
    print("[*] Installing Rust targets...")
    for t in TARGETS:
        subprocess.run(["rustup", "target", "add", t], check=True)
    print("[+] Targets installed.\n")

# ------------------------------------------------------
# Cargo build
# ------------------------------------------------------
def cmd_build():
    print("[*] Building Wave compiler...")
    for t in TARGETS:
        print(f"  -> building for {t}")
        subprocess.run(["cargo", "build", "--target", t, "--release"], check=True)
    print("[+] Build finished.\n")

# ------------------------------------------------------
# Packaging
# ------------------------------------------------------
def cmd_package():
    print("[*] Packaging release binaries...")

    for target in TARGETS:
        target_dir = TARGET_DIR / target / "release"
        binary = target_dir / BINARY_NAME

        formatted_target = target.replace("-unknown", "")

        out_name = f"{NAME}-v{VERSION}-{formatted_target}"

        if "windows" in target:
            bin_path = binary.with_suffix(".exe")
            if not bin_path.exists():
                print(f"[!] Missing binary: {bin_path}")
                continue

            shutil.copy(bin_path, ROOT / f"{BINARY_NAME}.exe")
            zip_path = ROOT / f"{out_name}.zip"
            subprocess.run(["zip", "-q", zip_path, f"{BINARY_NAME}.exe"], check=True)
            os.remove(ROOT / f"{BINARY_NAME}.exe")

            print(f"[+] Windows packaged → {zip_path}")

        else:
            if not binary.exists():
                print(f"[!] Missing binary: {binary}")
                continue

            tar_path = ROOT / f"{out_name}.tar.gz"
            subprocess.run([
                "tar", "-czf", tar_path,
                "-C", str(target_dir),
                BINARY_NAME
            ], check=True)

            print(f"[+] Packaged → {tar_path}")

    print("[+] Packaging complete.\n")

# ------------------------------------------------------
# release = build + package
# ------------------------------------------------------
def cmd_release():
    cmd_build()
    cmd_package()

# ------------------------------------------------------
# clean
# ------------------------------------------------------
def cmd_clean():
    print("[*] Cleaning build artifacts...")
    shutil.rmtree("target", ignore_errors=True)

    for f in os.listdir(ROOT):
        if f.endswith(".tar.gz") or f.endswith(".zip"):
            os.remove(f)

    print("[+] Cleaned.\n")

# ------------------------------------------------------
# CLI
# ------------------------------------------------------
def main():
    if len(sys.argv) < 2:
        print("Usage: x.py [install | build | package | release | clean]")
        return

    cmd = sys.argv[1]

    if cmd == "install":
        cmd_install()
    elif cmd == "build":
        cmd_build()
    elif cmd == "package":
        cmd_package()
    elif cmd == "release":
        cmd_release()
    elif cmd == "clean":
        cmd_clean()
    else:
        print("Unknown command:", cmd)
        print("Usage: x.py [install | build | package | release | clean]")

if __name__ == "__main__":
    main()
