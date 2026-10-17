# SPDX-License-Identifier: MPL-2.0
"""Atomic compiler/std staging and release archive identity shared with x.py."""

import hashlib
import json
import os
import re
from pathlib import Path
import shutil
import subprocess
import sys
import tarfile
import tempfile
import zipfile

from tools.ci.common import ROOT, main
from tools.validation_reports import write_report


def artifact_contract(target):
    paths = {
        "compiler": "wavec.exe" if "windows" in target else "wavec",
        "std": "std",
        "tools": "llvm/bin",
    }
    if "linux" in target:
        paths["crt"] = "crt"
    if "windows" in target:
        paths["builtins"] = "llvm/lib/clang/21/lib/windows"
    return {
        "abi": (
            "msvc"
            if "windows" in target
            else (
                "gnu"
                if "linux" in target
                else "freebsd" if "freebsd" in target else "darwin"
            )
        ),
        "paths": paths,
        "external_prerequisites": (
            ["Windows SDK", "MSVC/UCRT libraries", "Visual C++ runtime"]
            if "windows" in target
            else (
                ["Apple SDK and system libraries"]
                if "apple" in target
                else (
                    ["FreeBSD 14.4 or newer compatible base system"]
                    if "freebsd" in target
                    else ["compatible target glibc and system libraries"]
                )
            )
        ),
    }


def identity(archive, target, version, root=ROOT):
    source = subprocess.run(
        ["git", "rev-parse", "HEAD"],
        cwd=root,
        capture_output=True,
        text=True,
        check=True,
    ).stdout.strip()
    manifest = json.loads((root / "std/manifest.json").read_text())
    if not re.fullmatch("[0-9a-f]{40}", source):
        raise ValueError("invalid package source SHA")
    if (
        type(manifest.get("compatibility_revision")) is not int
        or manifest["compatibility_revision"] < 0
    ):
        raise ValueError("invalid std compatibility revision in package inputs")
    with Path(archive).open("rb") as stream:
        digest = hashlib.file_digest(stream, "sha256").hexdigest()
    return {
        "schema_version": 1,
        "compiler_version": version,
        "source_sha": source,
        "std_compatibility_revision": manifest["compatibility_revision"],
        "target": target,
        **artifact_contract(target),
        "archive": Path(archive).name,
        "sha256": digest,
    }


def replace_set(candidates):
    """Replace an archive and sidecars together, restoring previous files on errors."""
    candidates = list(candidates)
    for source, destination in candidates:
        if not source.is_file():
            raise FileNotFoundError(source)
        if destination.exists() and not destination.is_file():
            raise ValueError(f"not a file: {destination}")
    with tempfile.TemporaryDirectory(
        prefix=".previous-", dir=candidates[0][1].parent
    ) as directory:
        backups = []
        published = []
        try:
            for index, (source, destination) in enumerate(candidates):
                if destination.exists():
                    backup = Path(directory) / str(index)
                    os.replace(destination, backup)
                    backups.append((backup, destination))
                os.replace(source, destination)
                published.append(destination)
        except BaseException:
            for destination in reversed(published):
                destination.unlink(missing_ok=True)
            for backup, destination in reversed(backups):
                os.replace(backup, destination)
            raise


def sidecars(directory, archive, metadata):
    checksum = directory / (archive.name + ".sha256")
    checksum.write_text(f"{metadata['sha256']}  {archive.name}\n", encoding="ascii")
    descriptor = directory / (archive.name + ".metadata.json")
    write_report(descriptor, metadata)
    return [
        (checksum, archive.with_name(checksum.name)),
        (descriptor, archive.with_name(descriptor.name)),
    ]


def write_identity(archive, target, version, root=ROOT):
    data = identity(archive, target, version, root)
    archive = Path(archive)
    with tempfile.TemporaryDirectory(
        prefix=".identity-", dir=archive.parent
    ) as temporary:
        replace_set(sidecars(Path(temporary), archive, data))
    return data


def stage_package(legacy, target, binary, out_name):
    """Reuse authoritative native dependency helpers, but own staging and std policy."""
    legacy.DIST_DIR.mkdir(parents=True, exist_ok=True)
    destination = legacy.DIST_DIR / out_name
    with tempfile.TemporaryDirectory(
        prefix=".stage-", dir=legacy.DIST_DIR
    ) as temporary:
        stage = Path(temporary) / out_name
        stage.mkdir()
        staged = stage / binary.name
        legacy.copy_executable(binary, staged)
        tools = legacy.copy_lld_tools(stage, target)
        legacy.write_linux_crt_objects(stage, target)
        libraries = legacy.copy_llvm_runtime_libs(stage, target, tools, [staged])
        if not libraries and not legacy.is_windows_target(target):
            raise RuntimeError("LLVM runtime libraries missing from package inputs")
        if legacy.is_windows_target(target):
            legacy.copy_windows_msvc_resources(stage, target)
        shutil.copytree(legacy.ROOT / "std", stage / "std")
        licenses = stage / "licenses"
        licenses.mkdir(exist_ok=True)
        shutil.copy2(legacy.ROOT / "LICENSE", licenses / "Wave.txt")
        shutil.copy2(legacy.ROOT / "std/LICENSE", licenses / "std.txt")
        legacy.patch_staged_runtime(stage, target, staged, tools)
        legacy.verify_packaged_runtime_arch(stage, target)
        backup = Path(temporary) / "previous"
        if destination.exists():
            destination.rename(backup)
        try:
            stage.rename(destination)
        except BaseException:
            if backup.exists():
                backup.rename(destination)
            raise
    return destination


def package_targets(legacy):
    """x.py-compatible package entry point, with no destructive partial archive writes."""
    selected = []
    for target in legacy.TARGETS:
        binary = (
            legacy.TARGET_DIR
            / target
            / "release"
            / ("wavec.exe" if "windows" in target else "wavec")
        )
        if not binary.is_file():
            raise FileNotFoundError(f"Missing binary: {binary}")
        selected.append((target, binary))
    if not selected:
        raise ValueError("no release targets selected")
    for target, binary in selected:
        name = f"{legacy.NAME}-v{legacy.VERSION}-{legacy.release_target_name(target)}"
        stage = stage_package(legacy, target, binary, name)
        extension = ".zip" if "windows" in target else ".tar.gz"
        archive = legacy.ROOT / (name + extension)
        with tempfile.TemporaryDirectory(
            prefix=".archive-", dir=legacy.DIST_DIR
        ) as temporary:
            candidate = Path(temporary) / archive.name
            if extension == ".zip":
                with zipfile.ZipFile(
                    candidate, "w", compression=zipfile.ZIP_DEFLATED
                ) as output:
                    for path in sorted(stage.rglob("*")):
                        if path.is_file():
                            output.write(path, Path(name) / path.relative_to(stage))
            else:
                with tarfile.open(candidate, "w:gz", dereference=True) as output:
                    output.add(stage, arcname=name)
            metadata = identity(candidate, target, legacy.VERSION, legacy.ROOT)
            replace_set(
                [(candidate, archive), *sidecars(Path(temporary), archive, metadata)]
            )
        print(f"[+] Packaged {archive}")


def archive_for(r):
    extension = "zip" if r.target.host_os == "windows" else "tar.gz"
    version = r.env.get("RELEASE_VERSION")
    if version:
        return ROOT / f"wave-v{version}-{r.target.archive_target}.{extension}"
    found = list(ROOT.glob(f"wave-v*-{r.target.archive_target}.{extension}"))
    if len(found) != 1:
        raise ValueError("expected exactly one target archive")
    return found[0]


def checksums(r, _):
    archive = archive_for(r)
    version = r.env.get("RELEASE_VERSION")
    if not version:
        import tomllib

        version = tomllib.loads((ROOT / "Cargo.toml").read_text())["package"]["version"]
    write_identity(archive, r.target.triple, version)


def windows_package_smoke(r, _):
    r.run(
        [
            "@python",
            "tools/check_msvc_package.py",
            "--archive",
            archive_for(r),
            "--target",
            r.target.triple,
            "--output",
            Path(r.env["RUNNER_TEMP"])
            / (
                "wave-package-arm64"
                if r.target.host_arch == "arm64"
                else "wave-package-x64"
            ),
        ]
    )


def package_smoke(r, _):
    from tools.ci.test import crt_check

    archive = archive_for(r)
    with tempfile.TemporaryDirectory(prefix="wave extracted package ") as temporary:
        root = Path(temporary)
        with tarfile.open(archive) as package:
            package.extractall(root, filter="data")
        entries = list(root.iterdir())
        if len(entries) != 1 or not entries[0].is_dir():
            raise ValueError("expected one package root")
        package = entries[0]
        home = root / "isolated home"
        home.mkdir()
        compiler = package / "wavec"
        if r.target.id in ("linux-riscv64", "linux-loong64"):
            from tools.ci.llvm_bundle import MACHINES
            with compiler.open("rb") as stream:
                header = stream.read(20)
            if (len(header) != 20 or header[:6] != b"\x7fELF\x02\x01"
                    or int.from_bytes(header[18:20], "little") != MACHINES[r.target.id]):
                raise ValueError("packaged compiler has the wrong ELF architecture")
            dynamic = r.run(["readelf", "-d", compiler])
            if "[$ORIGIN/llvm/lib]" not in dynamic:
                raise ValueError("packaged compiler is missing its relative LLVM library path")
        env = {"HOME": str(home), "PATH": "/usr/bin:/bin", "NO_COLOR": "1"}
        # Passing a fresh environment prevents fallback to the checkout or installed std.
        for name in ("QEMU_LD_PREFIX", "WAVE_LOONGARCH64_SYSROOT"):
            if name in r.env:
                env[name] = r.env[name]
        r.run([compiler, "-V"], cwd=root, env=env, clean_env=True)
        if r.target.host_os == "linux":
            crt_check(r, package / "crt", "loongarch64")
        source = root / "std smoke.wave"
        source.write_text(
            'import("std::mem::layout")::{size_of};\nfun main() -> i32 { if (size_of<i64>() != 8) { return 7; } println("release smoke"); return 0; }\n'
        )
        # Exercise the same bundled-std discovery as an ordinary installation.
        # An explicit override would hide a broken default lookup.
        command = [compiler, "run", source]
        if r.target.id in ("linux-riscv64", "linux-loong64"):
            # QEMU_LD_PREFIX selects the emulator's loader, not Wave's linker
            # search paths. The isolated package test must select both.
            command.extend(["--sysroot", r.env["WAVE_CROSS_SYSROOT"]])
        output = r.run(
            command,
            cwd=root,
            env=env,
            clean_env=True,
        )
        if output.strip() != "release smoke":
            raise ValueError("extracted package std smoke failed")
    checksums(r, {})


from tools.ci.cross_package import riscv_package, loongarch_package
from tools.ci.freebsd_package import freebsd_package


OPERATIONS = {
    name: globals()[name]
    for name in (
        "checksums",
        "package_smoke",
        "windows_package_smoke",
        "riscv_package",
        "loongarch_package",
        "freebsd_package",
    )
}

if __name__ == "__main__":
    sys.exit(main("package"))
