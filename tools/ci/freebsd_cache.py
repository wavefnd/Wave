# SPDX-License-Identifier: MPL-2.0
"""Cache a cleanly stopped FreeBSD build VM, never a release acceptance result."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile

from tools.ci.common import ROOT
from tools.ci.targets import PINS


def digest(path):
    with Path(path).open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def identity(root=ROOT, pins=None):
    pins = PINS if pins is None else pins
    # Include setup/link flags and the dependency graph, but not application
    # sources: Cargo checks those against the fresh checkout on every run.
    paths = ["Cargo.lock", ".cargo/config.toml", "tools/ci/freebsd_cache.py",
             "tools/ci/freebsd_package.py", "x.py"]
    manifests = [root / "Cargo.toml", root / "llvm/Cargo.toml", root / "utils/Cargo.toml", root / "whale/Cargo.toml",
                 *(root / "front").glob("*/Cargo.toml")]
    paths.extend(p.relative_to(root).as_posix() for p in manifests)
    inputs = {name: digest(root / name) for name in sorted(set(paths))}
    values = {name: pins[name] for name in ("FREEBSD_IMAGE", "FREEBSD_IMAGE_SHA256", "RUST_VERSION", "LLVM_SOURCE_VERSION")}
    return hashlib.sha256(json.dumps({"schema": 1, "pins": values, "inputs": inputs}, sort_keys=True).encode()).hexdigest()


def cache_path(environment):
    temporary = Path(environment.get("RUNNER_TEMP", tempfile.gettempdir()))
    return Path(environment.get("WAVE_FREEBSD_CACHE_DIR", temporary / "wave-freebsd-cache")).resolve()


def restored_image(r, directory, expected):
    image, manifest = directory / "vm.qcow2", directory / "manifest.json"
    if not image.is_file() or not manifest.is_file():
        print("FreeBSD VM cache miss; provisioning a fresh guest", flush=True)
        return None
    try:
        metadata = json.loads(manifest.read_text())
        if not isinstance(metadata, dict):
            raise ValueError("invalid cache manifest")
        if metadata.get("identity") != expected or metadata.get("sha256") != digest(image):
            raise ValueError("identity or checksum mismatch")
        # Restored disks must be standalone, not refer to another run's files.
        info = json.loads(r.run(["qemu-img", "info", "--output=json", image]))
        if info.get("format") != "qcow2" or info.get("backing-filename") or info.get("data-file"):
            raise ValueError("cache is not a standalone qcow2 image")
        specific = info.get("format-specific", {}).get("data", {})
        if specific.get("data-file") or specific.get("corrupt") or info.get("dirty-flag"):
            raise ValueError("cache image is dirty, corrupt or externally backed")
    except (ValueError, OSError, RuntimeError) as error:
        print(f"Discarding unusable FreeBSD VM cache: {error}", flush=True)
        return None
    print("FreeBSD VM cache hit; reusing installed tools and Cargo build outputs", flush=True)
    return image


def save_image(r, overlay, directory, expected, source):
    """Called only after acceptance, credential cleanup and orderly power-off."""
    directory.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix=".snapshot-", dir=directory) as temporary:
        stage = Path(temporary)
        image = stage / "vm.qcow2"
        # Flatten the overlay: the next runner has no access to our backing file.
        r.run(["qemu-img", "convert", "-O", "qcow2", overlay, image], timeout=900)
        r.run(["qemu-img", "check", image], timeout=120)
        metadata = {"identity": expected, "source": source, "sha256": digest(image)}
        (stage / "manifest.json").write_text(json.dumps(metadata, indent=2) + "\n")
        os.replace(image, directory / "vm.qcow2")
        os.replace(stage / "manifest.json", directory / "manifest.json")
    print(f"Saved FreeBSD VM and Cargo cache ({(directory / 'vm.qcow2').stat().st_size} bytes)", flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--github-output", type=Path, required=True)
    args = parser.parse_args()
    prefix = "wave-freebsd-v1-" + identity() + "-"
    source = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip()
    with args.github_output.open("a") as output:
        output.write(f"path={cache_path(os.environ)}\nkey={prefix}{source}\nrestore-prefix={prefix}\n")


if __name__ == "__main__":
    main()
