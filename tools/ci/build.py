# SPDX-License-Identifier: MPL-2.0
"""Verify prerequisites or explicitly provision them, then run compiler builds."""

import hashlib
from pathlib import Path
import shutil
import sys
import tarfile
import tempfile

from tools.ci.common import ROOT, main
from tools.ci.targets import LLVM_MAJOR, RUST_VERSION


def llvm_config(r, _):
    if r.target.host_os == "macos":
        prefix = Path(r.run(["brew", "--prefix", "llvm@" + LLVM_MAJOR]).strip())
        lld = Path(r.run(["brew", "--prefix", "lld@" + LLVM_MAJOR]).strip())
        r.addpath(lld / "bin")
    else:
        prefix = Path(r.env.get("LLVM_SYS_211_PREFIX", r.env["LLVM_PREFIX_LINUX"]))
    config = (
        prefix
        / "bin"
        / ("llvm-config.exe" if r.target.host_os == "windows" else "llvm-config")
    )
    version = r.run([config, "--version"]).strip()
    if not version.startswith(LLVM_MAJOR + "."):
        raise ValueError(f"expected LLVM {LLVM_MAJOR}, found {version}")
    rust = r.run(["rustc", "--version"]).split()
    if len(rust) < 2 or rust[1] != RUST_VERSION:
        raise ValueError(f"expected Rust {RUST_VERSION}, found {rust}")
    r.setenv("LLVM_SYS_211_PREFIX", prefix)
    r.setenv("LLVM_CONFIG_PATH", config)
    r.addpath(prefix / "bin")
    # ld.lld selects the Mach-O driver when invoked as ld64.lld.
    for name in ("ld64.lld", "ld.lld", "lld"):
        candidate = shutil.which(name, path=r.env["PATH"])
        if candidate:
            directory = r.temp / "llvm-bin"
            directory.mkdir(exist_ok=True)
            link = directory / "ld64.lld"
            if not link.exists():
                link.symlink_to(candidate)
            r.setenv("WAVE_LD64_LLD", link)
            r.addpath(directory)
            break


def llvm_setup(r, operation):
    if not r.provision:
        llvm_config(r, operation)
        for tool in ("clang", "ld.lld"):
            if not shutil.which(tool, path=r.env["PATH"]):
                raise FileNotFoundError(
                    f"{tool} required; install it or use --provision"
                )
        return
    if r.target.host_os == "macos":
        # Keep the runner's formula metadata and valid LLVM 21 installation.
        # Refreshing Homebrew can replace available Intel bottles with source builds.
        r.setenv("HOMEBREW_NO_AUTO_UPDATE", "1")
        r.setenv("HOMEBREW_NO_INSTALL_UPGRADE", "1")
        if operation.get("macos_update"):
            r.run(["brew", "update"])
        r.run(["brew", "install", "llvm@" + LLVM_MAJOR, "lld@" + LLVM_MAJOR])
    else:
        packages = [r.expand(p) for p in operation.get("packages", [])]
        r.run(["sudo", "apt-get", "update"])
        r.run(
            [
                "sudo",
                "apt-get",
                "install",
                "-y",
                "wget",
                "curl",
                "software-properties-common",
                *[p for p in packages if not p.startswith(("clang-", "lld-", "llvm-"))],
            ]
        )
        installer = r.temp / "llvm.sh"
        r.run(
            [
                "wget",
                "--inet4-only",
                "--tries=3",
                "--timeout=30",
                "--retry-on-host-error",
                "--retry-connrefused",
                "https://apt.llvm.org/llvm.sh",
                "-O",
                installer,
            ]
        )
        r.run(["sudo", "bash", installer, LLVM_MAJOR])
        r.run(
            [
                "sudo",
                "apt-get",
                "install",
                "-y",
                *packages,
                f"clang-{LLVM_MAJOR}",
                f"lld-{LLVM_MAJOR}",
                f"llvm-{LLVM_MAJOR}-dev",
            ]
        )
    llvm_config(r, operation)


def stdlib(r, _):
    # Isolate std from the user's installed version without losing their Rust toolchain.
    old_home = Path(r.env.get("HOME", r.env.get("USERPROFILE", str(Path.home()))))
    r.env.setdefault("CARGO_HOME", str(old_home / ".cargo"))
    r.env.setdefault("RUSTUP_HOME", str(old_home / ".rustup"))
    home = r.temp / "home"
    destination = home / ".wave/lib/wave/std"
    if destination.exists():
        shutil.rmtree(destination)
    shutil.copytree(ROOT / "std", destination)
    r.setenv("HOME", home)


def download(r, url, path, digest, timeout=900):
    path = Path(path)
    with tempfile.TemporaryDirectory(prefix=f".{path.name}.", dir=path.parent) as directory:
        candidate = Path(directory) / path.name
        r.run(
            [
                "curl",
                "--ipv4",
                "--fail",
                "--location",
                "--retry",
                "3",
                "--connect-timeout",
                "30",
                "--max-time",
                str(timeout),
                url,
                "-o",
                candidate,
            ],
            timeout=timeout + 30,
        )
        with candidate.open("rb") as stream:
            actual = hashlib.file_digest(stream, "sha256").hexdigest()
        if actual != digest:
            raise ValueError(f"checksum mismatch for {path.name}")
        candidate.replace(path)


def loongarch_tools(r, _):
    if not r.provision:
        tool = shutil.which("loongarch64-unknown-linux-gnu-gcc", path=r.env["PATH"])
        sysroot = r.env.get("WAVE_LOONGARCH64_SYSROOT")
        if not tool or not sysroot or not Path(sysroot).is_dir():
            raise FileNotFoundError(
                "LoongArch toolchain and WAVE_LOONGARCH64_SYSROOT required; use --provision"
            )
        return
    name = r.env["LOONGARCH_TOOLCHAIN_ARCHIVE"]
    archive = r.temp / name
    download(
        r,
        f"https://github.com/loongson/build-tools/releases/download/{r.env['LOONGARCH_TOOLCHAIN_VERSION']}/{name}",
        archive,
        r.env["LOONGARCH_TOOLCHAIN_SHA256"],
    )
    directory = r.temp / "loongarch-toolchain"
    directory.mkdir(exist_ok=True)
    with tarfile.open(archive) as tar:
        tar.extractall(directory, filter="data")
    toolchain = directory / "cross-tools"
    if not (toolchain / "bin/loongarch64-unknown-linux-gnu-gcc").is_file():
        raise FileNotFoundError("LoongArch compiler missing from verified archive")
    r.addpath(toolchain / "bin")
    r.setenv("WAVE_LOONGARCH64_SYSROOT", toolchain / "target")


def windows_host(r, _):
    host = r.run(["rustc", "-vV"])
    if f"host: {r.target.triple}" not in host:
        raise ValueError("Rust host must match native MSVC architecture")
    if r.env.get("PROCESSOR_ARCHITECTURE", "").upper() != (
        "ARM64" if r.target.host_arch == "arm64" else "AMD64"
    ):
        raise ValueError("Windows runner architecture does not match target")
    version = r.run(["$LLVM_CONFIG_PATH", "--version"])
    if not version.startswith(LLVM_MAJOR + "."):
        raise ValueError("LLVM version mismatch")
    backends = r.run(["$LLVM_CONFIG_PATH", "--targets-built"]).split()
    required = (
        {"AArch64"}
        if r.target.host_arch == "arm64"
        else {"X86", "AArch64", "RISCV"}
    )
    if not required.issubset(backends):
        raise ValueError(f"missing LLVM backends: {required-set(backends)}")


def llvm_tools(r, _):
    r.run(["llvm-config", "--version"])
    r.run(["clang", "--version"])
    r.run(["ld.lld", "--version"])
    if not Path(
        r.env.get("WAVE_LD64_LLD", shutil.which("ld64.lld", path=r.env["PATH"]) or "")
    ).is_file():
        # Ordinary native CI does not need the Mach-O cross linker.
        if r.current["plan"].startswith("release/"):
            raise FileNotFoundError("Mach-O linker required for release validation")


def wasm_tools(r, _):
    for tool in ("llvm-config", "wasm-ld", "node"):
        r.run([tool, "--version"])
    if "WebAssembly" not in r.run(["llvm-config", "--targets-built"]).split():
        raise ValueError("LLVM WebAssembly backend missing")


def freebsd_image(r, _):
    image = Path(r.env["RUNNER_TEMP"]) / r.env["FREEBSD_IMAGE"]
    if r.provision:
        download(
            r,
            "https://download.freebsd.org/releases/VM-IMAGES/14.4-RELEASE/amd64/Latest/"
            + image.name,
            image,
            r.env["FREEBSD_IMAGE_SHA256"],
        )
    else:
        if not image.is_file():
            raise FileNotFoundError(
                f"provide pinned image archive {image} or use --provision"
            )
        with image.open("rb") as source:
            if (
                hashlib.file_digest(source, "sha256").hexdigest()
                != r.env["FREEBSD_IMAGE_SHA256"]
            ):
                raise ValueError("FreeBSD image checksum mismatch")
    r.run(["xz", "--decompress", "--keep", "--force", image], timeout=240)


def freebsd_build(r, _):
    r.run(
        [
            "cargo",
            "build",
            "--release",
            "--no-default-features",
            "--features",
            "llvm-target-x86",
        ]
    )
    r.run(["@python", "-m", "unittest", "tools.test_freebsd_runtime_reporting"])


def windows_prerequisites(r, operation):
    if r.provision:
        for command in operation["commands"]:
            r.run(command)
        return
    prefix = r.env.get("LLVM_SYS_211_PREFIX")
    if not prefix:
        raise FileNotFoundError(
            "set LLVM_SYS_211_PREFIX for the pinned native SDK or use --provision"
        )
    config = Path(prefix) / "bin/llvm-config.exe"
    r.setenv("LLVM_CONFIG_PATH", config)
    r.setenv("WAVE_WINDOWS_LLVM_BIN", config.parent)
    r.addpath(config.parent)
    if not r.run([config, "--version"]).strip().startswith(LLVM_MAJOR + "."):
        raise ValueError("prepared SDK has the wrong LLVM major version")
    if any("libxml2" in str(arg) for c in operation["commands"] for arg in c):
        search = [
            Path(prefix) / "lib",
            *[Path(p) for p in r.env.get("LIB", "").split(";") if p],
        ]
        for name in r.run([config, "--system-libs", "--link-static"]).split():
            if not any((p / name).is_file() for p in search):
                raise FileNotFoundError(f"prepared SDK library missing: {name}")
        if not r.env.get("VCToolsInstallDir") or not r.env.get("WindowsSdkDir"):
            raise FileNotFoundError(
                "use the matching MSVC Developer Prompt or --provision"
            )


OPERATIONS = {
    name: globals()[name]
    for name in (
        "windows_prerequisites",
        "llvm_setup",
        "llvm_config",
        "stdlib",
        "loongarch_tools",
        "windows_host",
        "llvm_tools",
        "wasm_tools",
        "freebsd_image",
        "freebsd_build",
    )
}

if __name__ == "__main__":
    sys.exit(main("build"))
