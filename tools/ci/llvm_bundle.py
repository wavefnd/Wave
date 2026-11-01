# SPDX-License-Identifier: MPL-2.0
"""Install, validate and consume relocatable, checksum-pinned LLVM 21 SDKs."""

import hashlib
import json
import os
import re
import shutil
import stat
import tarfile
import tempfile
from pathlib import Path
from urllib.parse import urlsplit

from tools.ci.build import download
from tools.ci.targets import PINS


BACKENDS = {"AArch64", "LoongArch", "RISCV", "WebAssembly", "X86"}
MACHINES = {"linux-riscv64": 243, "linux-loong64": 258}
TOOLS = ("llvm-config", "llc", "llvm-as", "llvm-mc", "lld", "ld.lld", "wasm-ld")
COMPONENTS = ("LLVM", "llvm-headers", "llvm-config", "llc", "llvm-as", "llvm-mc", "lld")


def sha256(path):
    with Path(path).open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def identity(target):
    if target not in MACHINES:
        raise ValueError(f"unsupported LLVM bundle target: {target}")
    return {
        "schema_version": 1,
        "target": target,
        "llvm_version": PINS["LLVM_SOURCE_VERSION"],
        "source_sha256": PINS["LLVM_SOURCE_SHA256"],
        "backends": sorted(BACKENDS),
    }


def check_tree(root):
    """Resolve every link component without allowing an outside intermediate hop."""
    root = Path(root).resolve(strict=True)

    def regular_target(relative):
        pending = list(relative.parts)
        current = root
        seen = set()
        while pending:
            part = pending.pop(0)
            if part in ("", "."):
                continue
            if part == "..":
                if current == root:
                    raise ValueError("LLVM link escapes bundle")
                current = current.parent
                continue
            current = current / part
            info = current.lstat()
            if stat.S_ISLNK(info.st_mode):
                if current in seen:
                    raise ValueError("cyclic LLVM bundle link")
                seen.add(current)
                target = Path(os.readlink(current))
                if target.is_absolute():
                    raise ValueError("absolute LLVM bundle link")
                pending = list(target.parts) + pending
                current = current.parent
            elif pending and not stat.S_ISDIR(info.st_mode):
                raise ValueError("LLVM link traverses a non-directory component")
        if not current.is_file():
            raise ValueError("LLVM link must target a regular file")

    def unreadable(error):
        raise ValueError(f"cannot inspect complete LLVM bundle: {error}") from error

    for directory, folders, files in os.walk(
        root, followlinks=False, onerror=unreadable
    ):
        for name in folders + files:
            path = Path(directory) / name
            mode = path.lstat().st_mode
            if stat.S_ISLNK(mode):
                try:
                    regular_target(path.relative_to(root))
                except (OSError, RuntimeError) as error:
                    raise ValueError(f"invalid LLVM bundle link: {path}") from error
            elif not (stat.S_ISDIR(mode) or stat.S_ISREG(mode)):
                raise ValueError(f"unsupported LLVM bundle entry: {path}")


def check_layout(prefix, target):
    """Reject wrong-architecture/incomplete SDKs before executing their tools."""
    prefix = Path(prefix).resolve()
    check_tree(prefix)
    metadata = json.loads((prefix / "bundle.json").read_text())
    if any(metadata.get(k) != v for k, v in identity(target).items()):
        raise ValueError("LLVM bundle identity does not match the pinned toolchain")
    for name in TOOLS:
        path = prefix / "bin" / name
        if not path.resolve().is_relative_to(prefix):
            raise ValueError(f"LLVM tool escapes bundle: {name}")
        with path.open("rb") as stream:
            header = stream.read(20)
        if (
            len(header) != 20
            or header[:6] != b"\x7fELF\x02\x01"
            or int.from_bytes(header[18:20], "little") != MACHINES[target]
            or not path.stat().st_mode & 0o111
        ):
            raise ValueError(f"invalid {target} ELF tool: {name}")
    for name in ("llvm/Config/llvm-config.h", "llvm-c/Core.h"):
        if not (prefix / "include" / name).is_file():
            raise ValueError(f"missing LLVM header: {name}")
    if not any((prefix / "lib").glob("libLLVM*.so*")):
        raise ValueError("missing shared LLVM library")
    return metadata


def validate(r, prefix, target, sysroot):
    """Check relocation and shared linking through the target's real llvm-config."""
    prefix = Path(prefix).resolve()
    check_layout(prefix, target)
    qemu = "qemu-riscv64" if target == "linux-riscv64" else "qemu-loongarch64"
    command = [qemu, "-L", str(sysroot)]
    config = command + [str(prefix / "bin/llvm-config")]
    if r.run(config + ["--version"]).strip() != PINS["LLVM_SOURCE_VERSION"]:
        raise ValueError("LLVM bundle version mismatch")
    if set(r.run(config + ["--targets-built"]).split()) != BACKENDS:
        raise ValueError("LLVM bundle backend set mismatch")
    for flag, relative in (("--prefix", "."), ("--includedir", "include"), ("--libdir", "lib")):
        if Path(r.run(config + [flag]).strip()).resolve() != (prefix / relative).resolve():
            raise ValueError(f"LLVM bundle is not relocatable: {flag}")
    libraries = r.run(config + ["--link-shared", "--libnames"]).split()
    if not libraries or any(
        Path(name).name != name or not (prefix / "lib" / name).is_file()
        for name in libraries
    ):
        raise ValueError("LLVM bundle shared library list is invalid")
    if any("ffi" in item for item in r.run(config + ["--link-shared", "--system-libs"]).split()):
        raise ValueError("LLVM bundle unexpectedly depends on libffi")
    for name in ("llc", "llvm-as", "llvm-mc", "ld.lld", "wasm-ld"):
        r.run(command + [str(prefix / "bin" / name), "--version"])


def install_bundle(r, build, source, destination, target, sysroot):
    """Use LLVM's install components; never ship a build tree as an SDK."""
    destination = Path(destination).resolve()
    if destination.exists():
        raise ValueError(f"bundle destination already exists: {destination}")
    for component in COMPONENTS:
        r.run(["cmake", "--install", str(build), "--prefix", str(destination),
               "--strip", "--component", component])
    for name in ("ld.lld", "wasm-ld"):
        alias = destination / "bin" / name
        if not alias.exists():
            alias.symlink_to("lld")
    shutil.copyfile(Path(source) / "llvm/LICENSE.TXT", destination / "LICENSE.TXT")
    (destination / "bundle.json").write_text(json.dumps(identity(target), indent=2) + "\n")
    validate(r, destination, target, sysroot)
    archive = destination.with_name(destination.name + ".tar.xz")
    if archive.exists():
        raise ValueError(f"bundle archive already exists: {archive}")
    with tarfile.open(archive, "w:xz") as stream:
        stream.add(destination, arcname="llvm")
    # Validate the actual extracted archive at a different prefix, not just staging.
    with tempfile.TemporaryDirectory(prefix="llvm-relocation-", dir=destination.parent) as folder:
        with tarfile.open(archive) as stream:
            stream.extractall(folder, filter="data")
        validate(r, Path(folder) / "llvm", target, sysroot)
    digest = sha256(archive)
    archive.with_name(archive.name + ".sha256").write_text(f"{digest}  {archive.name}\n")
    return {"url": None, "sha256": digest, **identity(target)}


def fetch_bundle(r, entry, destination, target, sysroot):
    """Only a manifest-pinned archive is accepted; no source-build fallback."""
    if any(entry.get(k) != v for k, v in identity(target).items()):
        raise ValueError("LLVM bundle pin does not match the required toolchain")
    url = entry.get("url", "")
    parsed = urlsplit(url)
    digest = entry.get("sha256", "")
    if parsed.scheme != "https" or not parsed.netloc or not re.fullmatch(r"[0-9a-f]{64}", digest):
        raise ValueError("LLVM bundle requires an HTTPS URL and a pinned SHA-256")
    destination = Path(destination).resolve()
    if destination.exists():
        raise ValueError(f"bundle destination already exists: {destination}")
    destination.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="llvm-download-", dir=destination.parent) as folder:
        folder = Path(folder)
        archive = folder / "sdk.tar.xz"
        download(r, url, archive, digest)
        with tarfile.open(archive) as stream:
            stream.extractall(folder / "unpacked", filter="data")
        sdk = folder / "unpacked/llvm"
        validate(r, sdk, target, sysroot)
        sdk.rename(destination)
    return destination
