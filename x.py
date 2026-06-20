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


import os
import sys
import subprocess
from pathlib import Path
import shutil
import platform

try:
    import tomllib
except ModuleNotFoundError:
    tomllib = None

# ------------------------------------------------------
#  Basic Setting
# ------------------------------------------------------

ROOT = Path(__file__).resolve().parent
TARGET_DIR = ROOT / "target"
DIST_DIR = ROOT / "dist"
BINARY_NAME = "wavec"
NAME = "wave"
WINDOWS_GNU_TARGET = "x86_64-pc-windows-gnu"
WINDOWS_LLVM_PREFIX = ROOT / "tools" / "llvm-win-prefix"
WINDOWS_LLVM_CONFIG_EXE = Path(os.environ.get(
    "LLVM_CONFIG_EXE",
    "/opt/llvm-win/bin/llvm-config.exe",
))
WINDOWS_RUST_TOOLCHAIN = os.environ.get(
    "WAVE_WINDOWS_RUST_TOOLCHAIN",
    "stable-x86_64-pc-windows-gnu",
)
MINGW_CC = os.environ.get(
    "WAVE_MINGW_CC",
    "gcc" if platform.system() == "Windows" else "x86_64-w64-mingw32-gcc",
)
MINGW_CXX = os.environ.get(
    "WAVE_MINGW_CXX",
    "g++" if platform.system() == "Windows" else "x86_64-w64-mingw32-g++",
)

TARGET_MATRIX = {
    "x86_64-unknown-linux-gnu":     ["Linux"],
    #"aarch64-unknown-linux-gnu":     ["Linux"],
    "x86_64-unknown-freebsd":       ["FreeBSD"],
    "x86_64-unknown-openbsd":       ["OpenBSD"],
    "x86_64-unknown-netbsd":        ["NetBSD"],
    "x86_64-unknown-dragonfly":     ["DragonFly"],
    "x86_64-unknown-redox":         ["Redox"],
    "x86_64-unknown-fuchsia":       ["Fuchsia"],
    "x86_64-unknown-haiku":         ["Haiku"],
    WINDOWS_GNU_TARGET:             ["Linux", "Windows"],
    "aarch64-apple-darwin":         ["Darwin"],
    "x86_64-apple-darwin":          ["Darwin"],
}

ALL_TARGETS = list(TARGET_MATRIX.keys())

def target_darwin_arch(target):
    if target.startswith("aarch64-"):
        return "arm64"
    if target.startswith("x86_64-"):
        return "x86_64"
    return None

def early_llvm_prefix():
    for env_name in ["WAVE_LLVM_HOME", "LLVM_SYS_211_PREFIX"]:
        value = os.environ.get(env_name)
        if value:
            path = Path(value)
            if path.exists():
                return path

    llvm_config = os.environ.get("LLVM_CONFIG_PATH") or shutil.which("llvm-config")
    if llvm_config:
        result = subprocess.run(
            [llvm_config, "--prefix"],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            check=False,
        )
        if result.returncode == 0:
            path = Path(result.stdout.strip())
            if path.exists():
                return path
    return None

def llvm_dylib_arches():
    prefix = early_llvm_prefix()
    if prefix is None:
        return set()

    lib_dir = prefix / "lib"
    candidates = [
        *lib_dir.glob("libLLVM-*.dylib"),
        lib_dir / "libLLVM.dylib",
        lib_dir / "libLLVM-C.dylib",
    ]

    for candidate in candidates:
        if not candidate.exists():
            continue
        result = subprocess.run(
            ["lipo", "-info", str(candidate)],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            check=False,
        )
        if result.returncode != 0:
            continue

        text = result.stdout.strip()
        if "are:" in text:
            return set(text.split("are:", 1)[1].strip().split())
        if "architecture:" in text:
            return {text.rsplit("architecture:", 1)[1].strip()}
    return set()

def is_target_buildable_with_current_llvm(target):
    if not target.endswith("apple-darwin"):
        return True

    target_arch = target_darwin_arch(target)
    if target_arch is None:
        return True

    arches = llvm_dylib_arches()
    if not arches:
        return platform.machine() == target_arch
    return target_arch in arches

def detect_targets():
    os_name = platform.system()

    native = [
        target
        for target, hosts in TARGET_MATRIX.items()
        if os_name in hosts and is_target_buildable_with_current_llvm(target)
    ]

    if not native:
        print("Unsupported build environment:", os_name)
        sys.exit(1)

    return native

TARGETS = detect_targets()

# Version Read
def get_version():
    if tomllib is None:
        print("[!] Python 3.11+ is required, or install the 'tomli' package for older Python.")
        sys.exit(1)

    with open(ROOT / "Cargo.toml", "rb") as f:
        cargo = tomllib.load(f)
    return cargo["package"]["version"]

VERSION = get_version()

def is_windows_gnu_target(target):
    return target == WINDOWS_GNU_TARGET

def is_darwin_target(target):
    return target.endswith("apple-darwin")

def is_linux_target(target):
    return "linux" in target

def require_tool(tool):
    if shutil.which(tool) is None:
        print(f"[!] Missing required tool: {tool}")
        sys.exit(1)

def configure_windows_gnu_env(env):
    if platform.system() == "Windows":
        prefix = early_llvm_prefix()
        llvm_config = env.get("LLVM_CONFIG_PATH") or shutil.which("llvm-config")

        if prefix is None or llvm_config is None:
            print("[!] Native Windows release builds require LLVM 21 and llvm-config.exe.")
            print("    Set WAVE_LLVM_HOME and LLVM_CONFIG_PATH to the LLVM 21 prefix.")
            sys.exit(1)

        require_tool(MINGW_CC)
        require_tool(MINGW_CXX)

        env["LLVM_SYS_211_PREFIX"] = str(prefix)
        env["LLVM_CONFIG_PATH"] = str(llvm_config)
        env["CARGO_TARGET_X86_64_PC_WINDOWS_GNU_LINKER"] = MINGW_CC
        env["CC_x86_64_pc_windows_gnu"] = MINGW_CC
        env["CXX_x86_64_pc_windows_gnu"] = MINGW_CXX
        return

    if not WINDOWS_LLVM_PREFIX.exists():
        print(f"[!] Missing Windows LLVM prefix wrapper: {WINDOWS_LLVM_PREFIX}")
        print("    Expected tools/llvm-win-prefix/bin/llvm-config to exist.")
        sys.exit(1)

    if not WINDOWS_LLVM_CONFIG_EXE.exists():
        print(f"[!] Missing Windows llvm-config.exe: {WINDOWS_LLVM_CONFIG_EXE}")
        print("    Set LLVM_CONFIG_EXE=/path/to/llvm-config.exe if it is installed elsewhere.")
        sys.exit(1)

    require_tool(MINGW_CC)
    require_tool(MINGW_CXX)
    require_tool("wine")

    env["LLVM_SYS_211_PREFIX"] = str(WINDOWS_LLVM_PREFIX)
    env["LLVM_CONFIG_EXE"] = str(WINDOWS_LLVM_CONFIG_EXE)
    env["CARGO_TARGET_X86_64_PC_WINDOWS_GNU_LINKER"] = MINGW_CC
    env["CC_x86_64_pc_windows_gnu"] = MINGW_CC
    env["CXX_x86_64_pc_windows_gnu"] = MINGW_CXX

def append_env_words(env, name, words):
    current = env.get(name, "").strip()
    addition = " ".join(words)
    env[name] = f"{current} {addition}" if current else addition

def configure_linux_release_env(env):
    # Use legacy DT_RPATH intentionally: unlike DT_RUNPATH, it applies to
    # transitive dependencies such as libLLVM -> libffi in clean containers.
    append_env_words(env, "RUSTFLAGS", [
        "-C", "link-arg=-Wl,--disable-new-dtags",
        "-C", "link-arg=-Wl,-z,origin",
        "-C", "link-arg=-Wl,-rpath,$ORIGIN/llvm/lib",
    ])

def cargo_build_args(target):
    args = ["cargo", "build", "--locked", "--target", target, "--release"]
    if is_windows_gnu_target(target):
        args.extend(["--no-default-features", "--features", "llvm-target-x86"])
    return args

def mingw_print_file_name(name):
    fallback = Path("/usr/x86_64-w64-mingw32/sys-root/mingw/bin") / name
    if shutil.which(MINGW_CC) is None:
        return fallback if fallback.exists() else None

    result = subprocess.run(
        [MINGW_CC, f"-print-file-name={name}"],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        check=False,
    )
    if result.returncode != 0:
        return None

    path = Path(result.stdout.strip())
    if path.exists() and path.name.lower() == name.lower():
        return path
    if fallback.exists():
        return fallback
    return None

def windows_package_inputs(exe_path):
    files = [exe_path]
    for dll in ["libgcc_s_seh-1.dll", "libstdc++-6.dll", "libwinpthread-1.dll"]:
        path = mingw_print_file_name(dll)
        if path is not None:
            files.append(path)
    return files

def llvm_config_path():
    return os.environ.get("LLVM_CONFIG_PATH") or shutil.which("llvm-config")

def llvm_config_dir(flag):
    llvm_config = llvm_config_path()
    if not llvm_config:
        return None

    result = subprocess.run(
        [llvm_config, flag],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        check=False,
    )
    if result.returncode != 0:
        return None

    path = Path(result.stdout.strip())
    return path if path.exists() else None

def llvm_prefix():
    for env_name in ["WAVE_LLVM_HOME", "LLVM_SYS_211_PREFIX"]:
        value = os.environ.get(env_name)
        if value:
            path = Path(value)
            if path.exists():
                return path

    return llvm_config_dir("--prefix")

def llvm_bin_dir():
    value = os.environ.get("WAVE_LLVM_BIN")
    if value:
        path = Path(value)
        if path.exists():
            return path

    path = llvm_config_dir("--bindir")
    if path is not None:
        return path

    prefix = llvm_prefix()
    if prefix is not None:
        path = prefix / "bin"
        if path.exists():
            return path
    return None

def llvm_lib_dir():
    value = os.environ.get("WAVE_LLVM_LIB")
    if value:
        path = Path(value)
        if path.exists():
            return path

    path = llvm_config_dir("--libdir")
    if path is not None:
        return path

    prefix = llvm_prefix()
    if prefix is not None:
        for name in ["lib64", "lib"]:
            path = prefix / name
            if path.exists():
                return path
    return None

def unique_existing_dirs(dirs):
    out = []
    seen = set()
    for directory in dirs:
        if directory is None:
            continue
        path = Path(directory)
        if not path.exists() or not path.is_dir():
            continue
        resolved = path.resolve()
        if resolved in seen:
            continue
        seen.add(resolved)
        out.append(resolved)
    return out

def windows_llvm_bin_dirs():
    dirs = []
    value = os.environ.get("WAVE_WINDOWS_LLVM_BIN")
    if value:
        dirs.append(Path(value))
    if WINDOWS_LLVM_CONFIG_EXE.exists():
        dirs.append(WINDOWS_LLVM_CONFIG_EXE.parent)
    return unique_existing_dirs(dirs)

def windows_rust_toolchain_root():
    value = os.environ.get("WAVE_WINDOWS_RUST_ROOT")
    if value:
        path = Path(value)
        if path.exists():
            return path

    rustup = shutil.which("rustup")
    if rustup is None:
        return None

    result = subprocess.run(
        [rustup, "which", "--toolchain", WINDOWS_RUST_TOOLCHAIN, "rustc.exe"],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        check=False,
    )
    if result.returncode != 0:
        return None

    rustc = Path(result.stdout.strip())
    if rustc.exists():
        return rustc.parent.parent
    return None

def windows_rustlib_root():
    root = windows_rust_toolchain_root()
    if root is None:
        return None
    path = root / "lib" / "rustlib" / WINDOWS_GNU_TARGET
    return path if path.exists() else None

def windows_rust_lld_path():
    root = windows_rustlib_root()
    if root is None:
        return None

    path = root / "bin" / "rust-lld.exe"
    return path if path.exists() else None

def windows_mingw_self_contained_lib_dir():
    value = os.environ.get("WAVE_WINDOWS_MINGW_LIB")
    if value:
        path = Path(value)
        if path.exists():
            return path

    root = windows_rustlib_root()
    if root is not None:
        path = root / "lib" / "self-contained"
        if path.exists():
            return path
    return None

def file_description(path):
    if shutil.which("file") is None:
        return ""
    result = subprocess.run(
        ["file", "-L", str(path)],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        check=False,
    )
    if result.returncode != 0:
        return ""
    return result.stdout.strip()

def is_windows_x86_64_binary(path):
    desc = file_description(path)
    if not desc:
        return True
    return "PE32+" in desc and "x86-64" in desc

def expected_binary_arch_token(target):
    arch = target.split("-", 1)[0]
    if arch == "x86_64":
        return "x86-64"
    if arch == "aarch64":
        return "ARM aarch64"
    if arch == "riscv64":
        return "RISC-V"
    if arch in {"i386", "i586", "i686"}:
        return "Intel 80386"
    return None

def is_binary_for_target(path, target):
    desc = file_description(path)
    if not desc:
        return True

    if is_windows_gnu_target(target):
        return "PE32+" in desc and "x86-64" in desc

    if is_linux_target(target) and "ELF" in desc:
        expected = expected_binary_arch_token(target)
        return expected is None or expected in desc

    return True

def require_binary_for_target(path, target, label):
    if is_binary_for_target(path, target):
        return

    print(f"[!] {label} has the wrong architecture for {target}: {path}")
    print(f"    {file_description(path)}")
    sys.exit(1)

def windows_tool_names(tool):
    if tool.lower().endswith(".exe"):
        names = [tool]
    else:
        names = [f"{tool}.exe"]
    if tool == "ld.lld":
        names.append("lld.exe")
    return names

def packaged_tool_name(tool, target):
    if is_windows_gnu_target(target) and not tool.lower().endswith(".exe"):
        return f"{tool}.exe"
    return tool

def find_release_tool(tool, target):
    if is_windows_gnu_target(target):
        for directory in windows_llvm_bin_dirs():
            for name in windows_tool_names(tool):
                candidate = directory / name
                if candidate.exists():
                    if not is_windows_x86_64_binary(candidate):
                        print(f"[!] Windows LLVM tool is not 64-bit PE: {candidate}")
                        print(f"    {file_description(candidate)}")
                        sys.exit(1)
                    return candidate.resolve()
        if tool == "ld.lld":
            rust_lld = windows_rust_lld_path()
            if rust_lld is not None:
                if not is_windows_x86_64_binary(rust_lld):
                    print(f"[!] Windows rust-lld is not 64-bit PE: {rust_lld}")
                    print(f"    {file_description(rust_lld)}")
                    sys.exit(1)
                return rust_lld.resolve()
        return None

    names = [tool]
    if platform.system() == "Windows" and not tool.endswith(".exe"):
        names.insert(0, f"{tool}.exe")

    dirs = []
    bin_dir = llvm_bin_dir()
    if bin_dir is not None:
        dirs.append(bin_dir)
    path_hit = shutil.which(tool)
    if path_hit:
        dirs.append(Path(path_hit).parent)

    for directory in dirs:
        for name in names:
            candidate = directory / name
            if candidate.exists():
                require_binary_for_target(candidate, target, "LLVM tool")
                return candidate.resolve()
    return None

def copy_executable(src, dst):
    dst.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(src, dst)
    mode = dst.stat().st_mode
    dst.chmod(mode | 0o755)

def copy_optional(src, dst):
    if src is None or not Path(src).exists():
        return None
    dst.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(src, dst)
    return dst

def copy_globbed_files(patterns, dst_dir, target=None):
    copied = []
    dst_dir.mkdir(parents=True, exist_ok=True)
    seen = set()
    for pattern in patterns:
        for src in pattern.parent.glob(pattern.name):
            if src.name in seen or not src.is_file():
                continue
            if target is not None and not is_binary_for_target(src, target):
                print(f"[!] Skipping runtime with wrong architecture for {target}: {src}")
                print(f"    {file_description(src)}")
                continue
            seen.add(src.name)
            dst = dst_dir / src.name
            shutil.copy2(src, dst)
            copied.append(dst)
    return copied

def copy_named_runtime(src, dst_dir, dst_name=None):
    if src is None:
        return None

    src = Path(src)
    if not src.exists() or not src.is_file():
        return None

    dst_dir.mkdir(parents=True, exist_ok=True)
    dst = dst_dir / (dst_name or src.name)
    if dst.exists():
        return dst
    shutil.copy2(src, dst)
    return dst

def llvm_tools_for_target(target):
    common = ["llc", "llvm-as", "llvm-mc"]
    if is_darwin_target(target):
        return [(tool, True) for tool in ["ld64.lld", "ld.lld", *common]]
    if is_windows_gnu_target(target):
        return [
            ("ld.lld", True),
            *[(tool, True) for tool in common],
        ]
    return [(tool, True) for tool in ["ld.lld", *common]]

def copy_lld_tools(stage_dir, target):
    copied = []
    tool_dir = stage_dir / "llvm" / "bin"
    missing_optional = []
    for tool, required in llvm_tools_for_target(target):
        src = find_release_tool(tool, target)
        if src is None:
            if not required:
                missing_optional.append(tool)
                continue
            print(f"[!] Missing LLVM tool for package: {tool}")
            if is_windows_gnu_target(target):
                print(f"    Expected a 64-bit Windows executable in: {', '.join(str(p) for p in windows_llvm_bin_dirs())}")
                print(f"    Install one with:")
                print(f"      rustup toolchain install {WINDOWS_RUST_TOOLCHAIN} --profile minimal --force-non-host")
                print("    Or set WAVE_WINDOWS_LLVM_BIN to a complete x86_64 Windows LLVM/LLD bin directory.")
            sys.exit(1)
        require_binary_for_target(src, target, "LLVM tool")
        dst = tool_dir / packaged_tool_name(tool, target)
        copy_executable(src, dst)
        copied.append((src, dst))

    if missing_optional and is_windows_gnu_target(target):
        print("[!] Windows package is missing optional LLD tools:")
        for tool in missing_optional:
            print(f"    missing optional tool: {tool}.exe")
    return copied

def copy_windows_mingw_self_contained_libs(stage_dir, target):
    if not is_windows_gnu_target(target):
        return []

    src_dir = windows_mingw_self_contained_lib_dir()
    if src_dir is None:
        print("[!] Missing Windows MinGW self-contained runtime libraries")
        print(f"    Install them with:")
        print(f"      rustup toolchain install {WINDOWS_RUST_TOOLCHAIN} --profile minimal --force-non-host")
        print("    Or set WAVE_WINDOWS_MINGW_LIB to a directory containing crt2.o and lib*.a.")
        sys.exit(1)

    required = ["crt2.o", "libmingw32.a", "libgcc.a", "libmingwex.a", "libmsvcrt.a", "libkernel32.a"]
    missing = [name for name in required if not (src_dir / name).exists()]
    if missing:
        print(f"[!] Incomplete Windows MinGW runtime library directory: {src_dir}")
        for name in missing:
            print(f"    missing: {name}")
        sys.exit(1)

    dst_dir = stage_dir / "mingw" / "lib"
    dst_dir.mkdir(parents=True, exist_ok=True)

    copied = []
    for src in sorted(src_dir.glob("*.a")) + sorted(src_dir.glob("*.o")):
        dst = dst_dir / src.name
        shutil.copy2(src, dst)
        copied.append(dst)
    return copied

def llvm_tool_run_env(tool):
    env = os.environ.copy()
    tool = Path(tool)
    if tool.parent.name == "bin":
        lib_dir = tool.parent.parent / "lib"
        if lib_dir.is_dir():
            current = env.get("LD_LIBRARY_PATH", "")
            env["LD_LIBRARY_PATH"] = f"{lib_dir}:{current}" if current else str(lib_dir)
    return env

def linux_crt_source_for_target(target):
    if target == "x86_64-unknown-linux-gnu":
        return ROOT / "llvm" / "crt" / "linux" / "x86_64" / "crt1.s"
    return None

def write_linux_crt_objects(stage_dir, target):
    if not is_linux_target(target):
        return []

    source = linux_crt_source_for_target(target)
    if source is None:
        return []
    if not source.exists():
        print(f"[!] Missing Linux CRT source for {target}: {source}")
        sys.exit(1)

    llvm_mc = find_release_tool("llvm-mc", target)
    if llvm_mc is None:
        print("[!] Missing LLVM tool for Linux CRT object: llvm-mc")
        sys.exit(1)

    dst_dir = stage_dir / "crt" / target
    dst_dir.mkdir(parents=True, exist_ok=True)
    dst = dst_dir / "crt1.o"

    subprocess.run(
        [
            str(llvm_mc),
            f"-triple={target}",
            "-filetype=obj",
            str(source),
            "-o",
            str(dst),
        ],
        env=llvm_tool_run_env(llvm_mc),
        check=True,
    )
    return [dst]

def ldd_shared_libs(binary):
    if shutil.which("ldd") is None:
        return []

    result = subprocess.run(
        ["ldd", str(binary)],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        check=False,
    )
    if result.returncode != 0:
        return []

    libs = []
    for line in result.stdout.splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("linux-vdso"):
            continue

        path_text = None
        if "=>" in stripped:
            rhs = stripped.split("=>", 1)[1].strip()
            if rhs.startswith("/"):
                path_text = rhs.split(" ", 1)[0]
        elif stripped.startswith("/"):
            path_text = stripped.split(" ", 1)[0]

        if path_text:
            path = Path(path_text)
            if path.exists():
                libs.append(path)

    return libs

def is_glibc_core_runtime(path):
    name = path.name
    return (
        name == "libc.so.6"
        or name.startswith("ld-linux")
        or name in {
            "libdl.so.2",
            "libm.so.6",
            "libpthread.so.0",
            "librt.so.1",
            "libresolv.so.2",
            "libutil.so.1",
        }
    )

def copy_linux_runtime_deps(stage_dir, binaries):
    root_lib_dir = stage_dir / "llvm" / "lib"
    copied = []
    queue = [Path(p) for p in binaries if Path(p).exists()]
    seen = set()

    while queue:
        current = queue.pop(0)
        for dep in ldd_shared_libs(current):
            resolved = dep.resolve()
            if resolved in seen or is_glibc_core_runtime(dep):
                continue
            seen.add(resolved)

            dst = copy_named_runtime(dep, root_lib_dir)
            if dst is not None:
                copied.append(dst)
                queue.append(dst)

    return copied

WINDOWS_SYSTEM_DLLS = {
    "advapi32.dll",
    "bcryptprimitives.dll",
    "comdlg32.dll",
    "crypt32.dll",
    "gdi32.dll",
    "kernel32.dll",
    "msvcrt.dll",
    "ntdll.dll",
    "ole32.dll",
    "oleaut32.dll",
    "shell32.dll",
    "user32.dll",
    "userenv.dll",
    "ws2_32.dll",
}

def is_windows_system_dll(name):
    lower = name.lower()
    return lower in WINDOWS_SYSTEM_DLLS or lower.startswith("api-ms-win-") or lower.startswith("ext-ms-win-")

def pe_imported_dlls(binary):
    objdump = shutil.which("x86_64-w64-mingw32-objdump") or shutil.which("objdump")
    if objdump is None:
        return []

    result = subprocess.run(
        [objdump, "-p", str(binary)],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        check=False,
    )
    if result.returncode != 0:
        return []

    dlls = []
    for line in result.stdout.splitlines():
        line = line.strip()
        if line.startswith("DLL Name:"):
            name = line.split(":", 1)[1].strip()
            if name and not is_windows_system_dll(name):
                dlls.append(name)
    return dlls

def windows_dll_search_dirs():
    dirs = []
    dirs.extend(windows_llvm_bin_dirs())
    target_runtime = TARGET_DIR / WINDOWS_GNU_TARGET / "release"
    dirs.append(target_runtime)
    for dll in ["libgcc_s_seh-1.dll", "libstdc++-6.dll", "libwinpthread-1.dll"]:
        path = mingw_print_file_name(dll)
        if path is not None:
            dirs.append(path.parent)
    return unique_existing_dirs(dirs)

def find_windows_dll(name):
    lower = name.lower()
    for directory in windows_dll_search_dirs():
        direct = directory / name
        if direct.exists():
            return direct
        for candidate in directory.glob("*.dll"):
            if candidate.name.lower() == lower:
                return candidate
    return None

def copy_windows_runtime_deps(stage_dir, binaries):
    copied = []
    queue = [Path(p) for p in binaries if Path(p).exists()]
    seen = set()
    root_dir = stage_dir
    tool_dir = stage_dir / "llvm" / "bin"

    for directory in windows_llvm_bin_dirs():
        for pattern in ["LLVM*.dll", "libLLVM*.dll", "liblld*.dll"]:
            for dll in directory.glob(pattern):
                queue.append(dll)

    while queue:
        current = queue.pop(0)
        resolved = current.resolve()
        if resolved in seen:
            continue
        seen.add(resolved)

        if current.suffix.lower() == ".dll":
            root_copy = copy_optional(current, root_dir / current.name)
            tool_copy = copy_optional(current, tool_dir / current.name)
            copied.extend(p for p in [root_copy, tool_copy] if p is not None)

        for dll_name in pe_imported_dlls(current):
            dll = find_windows_dll(dll_name)
            if dll is None:
                print(f"[!] Missing Windows runtime DLL for package: {dll_name}")
                print(f"    referenced by: {current}")
                sys.exit(1)
            queue.append(dll)

    return copied

def verify_packaged_runtime_arch(stage_dir, target):
    paths = []
    for directory in [stage_dir, stage_dir / "llvm" / "bin", stage_dir / "llvm" / "lib"]:
        if directory.exists():
            paths.extend(path for path in directory.iterdir() if path.is_file())

    for path in paths:
        desc = file_description(path)
        if "ELF" not in desc and "PE32" not in desc:
            continue
        require_binary_for_target(path, target, "Packaged runtime")

def resolve_dylib_reference(ref, binary, extra_dirs=None):
    path = Path(ref)
    if path.is_absolute():
        return path if path.exists() else None

    name = path.name
    search_dirs = []
    if ref.startswith("@loader_path/"):
        search_dirs.append(Path(binary).parent)
    if ref.startswith("@executable_path/"):
        search_dirs.append(Path(binary).parent)
    if extra_dirs:
        search_dirs.extend(extra_dirs)

    for directory in search_dirs:
        candidate = directory / name
        if candidate.exists():
            return candidate
    return None

def copy_darwin_lld_runtime_refs(root_lib_dir, compiler_lib_dir, binaries):
    copied = []
    compiler_llvm = None
    if compiler_lib_dir is not None:
        compiler_llvm = compiler_lib_dir / "libLLVM.dylib"

    for binary in binaries:
        extra_dirs = [Path(binary).parent, Path(binary).parent.parent / "lib"]
        for ref in dylib_references(binary):
            name = Path(ref).name
            src = resolve_dylib_reference(ref, binary, extra_dirs)
            if src is None:
                continue

            if name.startswith("liblld"):
                copied.append(copy_named_runtime(src, root_lib_dir))
            elif name == "libLLVM.dylib":
                if compiler_llvm is not None and compiler_llvm.exists() and src.resolve() != compiler_llvm.resolve():
                    copied.append(copy_named_runtime(src, root_lib_dir, "libLLVM-lld.dylib"))

    return [p for p in copied if p is not None]

def copy_llvm_runtime_libs(stage_dir, target, lld_tool_paths, runtime_roots=None):
    copied = []
    root_lib_dir = stage_dir / "llvm" / "lib"
    lib_dir = llvm_lib_dir()

    if is_windows_gnu_target(target):
        dep_roots = list(runtime_roots or [])
        dep_roots.extend(staged for _, staged in lld_tool_paths)
        copied.extend(copy_windows_runtime_deps(stage_dir, dep_roots))
        return [p for p in copied if p is not None]

    patterns = []
    if lib_dir is not None:
        if is_darwin_target(target):
            patterns.extend([lib_dir / "libLLVM*.dylib", lib_dir / "liblld*.dylib"])
        elif is_linux_target(target):
            patterns.extend([lib_dir / "libLLVM*.so*", lib_dir / "liblld*.so*"])

    for tool_src, _ in lld_tool_paths:
        tool_lib_dir = tool_src.parent.parent / "lib"
        if tool_lib_dir.exists():
            if is_darwin_target(target):
                patterns.extend([tool_lib_dir / "liblld*.dylib"])
            elif is_linux_target(target):
                patterns.extend([tool_lib_dir / "libLLVM*.so*", tool_lib_dir / "liblld*.so*"])

    copied.extend(copy_globbed_files(patterns, root_lib_dir, target))
    if is_darwin_target(target):
        lld_sources = [tool_src for tool_src, _ in lld_tool_paths]
        lld_sources.extend(root_lib_dir.glob("liblld*.dylib"))
        copied.extend(copy_darwin_lld_runtime_refs(root_lib_dir, lib_dir, lld_sources))
    elif is_linux_target(target):
        dep_roots = list(runtime_roots or [])
        dep_roots.extend(staged for _, staged in lld_tool_paths)
        dep_roots.extend(root_lib_dir.glob("*.so*"))
        copied.extend(copy_linux_runtime_deps(stage_dir, dep_roots))
    return copied

def dylib_references(binary):
    if shutil.which("otool") is None:
        return []
    result = subprocess.run(
        ["otool", "-L", str(binary)],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        check=False,
    )
    if result.returncode != 0:
        return []

    refs = []
    for line in result.stdout.splitlines()[1:]:
        ref = line.strip().split(" ", 1)[0]
        if ref.startswith("/usr/lib/") or ref.startswith("/System/"):
            continue
        if "libLLVM" in ref or "liblld" in ref:
            refs.append(ref)
    return refs

def patch_macos_binary(binary, loader_prefix):
    if shutil.which("install_name_tool") is None:
        print(f"[!] install_name_tool not found; {binary.name} may require host LLVM paths")
        return

    subprocess.run(
        ["install_name_tool", "-add_rpath", loader_prefix, str(binary)],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        check=False,
    )
    for ref in dylib_references(binary):
        name = Path(ref).name
        subprocess.run(
            ["install_name_tool", "-change", ref, f"{loader_prefix}/{name}", str(binary)],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            check=False,
        )

def patch_macos_binary_with_lld_llvm(binary, loader_prefix):
    if shutil.which("install_name_tool") is None:
        print(f"[!] install_name_tool not found; {binary.name} may require host LLVM paths")
        return

    subprocess.run(
        ["install_name_tool", "-add_rpath", loader_prefix, str(binary)],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        check=False,
    )
    for ref in dylib_references(binary):
        name = Path(ref).name
        if name == "libLLVM.dylib":
            name = "libLLVM-lld.dylib"
        subprocess.run(
            ["install_name_tool", "-change", ref, f"{loader_prefix}/{name}", str(binary)],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            check=False,
        )

def linux_binary_has_runpath(binary, expected):
    if shutil.which("readelf") is None:
        return True

    result = subprocess.run(
        ["readelf", "-d", str(binary)],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        check=False,
    )
    if result.returncode != 0:
        return False
    return expected in result.stdout

def patch_linux_binary(binary, rpath):
    if shutil.which("patchelf") is None:
        return
    subprocess.run(["patchelf", "--force-rpath", "--set-rpath", rpath, str(binary)], check=True)

def codesign_macos_binary(binary):
    if shutil.which("codesign") is None:
        print(f"[!] codesign not found; {binary.name} may not run after install_name_tool")
        return

    subprocess.run(
        ["codesign", "--force", "--sign", "-", str(binary)],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        check=False,
    )

def patch_staged_runtime(stage_dir, target, binary_path, lld_tool_paths):
    if is_darwin_target(target):
        patch_macos_binary(binary_path, "@executable_path/llvm/lib")
        has_lld_llvm = (stage_dir / "llvm" / "lib" / "libLLVM-lld.dylib").exists()
        for _, staged in lld_tool_paths:
            if staged.exists():
                if has_lld_llvm:
                    patch_macos_binary_with_lld_llvm(staged, "@executable_path/../lib")
                else:
                    patch_macos_binary(staged, "@executable_path/../lib")
        for dylib in (stage_dir / "llvm" / "lib").glob("liblld*.dylib"):
            if has_lld_llvm:
                patch_macos_binary_with_lld_llvm(dylib, "@loader_path")
            else:
                patch_macos_binary(dylib, "@loader_path")
        codesign_macos_binary(binary_path)
        for _, staged in lld_tool_paths:
            if staged.exists():
                codesign_macos_binary(staged)
        for dylib in (stage_dir / "llvm" / "lib").glob("*.dylib"):
            codesign_macos_binary(dylib)
    elif is_linux_target(target):
        patch_linux_binary(binary_path, "$ORIGIN/llvm/lib")
        if not linux_binary_has_runpath(binary_path, "$ORIGIN/llvm/lib"):
            print(f"[!] Missing RPATH/RUNPATH in {binary_path.name}")
            print("    Linux release packages must keep wavec as an ELF binary and")
            print("    resolve bundled LLVM from $ORIGIN/llvm/lib.")
            print("    Rebuild with x.py build/release so Cargo embeds the release runtime path.")
            sys.exit(1)
        for _, staged in lld_tool_paths:
            if staged.exists():
                patch_linux_binary(staged, "$ORIGIN/../lib")

def stage_release_package(target, binary, out_name):
    stage_dir = DIST_DIR / out_name
    if stage_dir.exists():
        shutil.rmtree(stage_dir)
    stage_dir.mkdir(parents=True)

    staged_binary = stage_dir / binary.name
    copy_executable(binary, staged_binary)

    lld_tools = copy_lld_tools(stage_dir, target)
    write_linux_crt_objects(stage_dir, target)
    runtime_libs = copy_llvm_runtime_libs(stage_dir, target, lld_tools, [staged_binary])
    if not runtime_libs:
        print("[!] Missing LLVM runtime libraries for package")
        print("    Set WAVE_LLVM_HOME or LLVM_SYS_211_PREFIX to the LLVM release prefix.")
        sys.exit(1)
    copy_windows_mingw_self_contained_libs(stage_dir, target)
    patch_staged_runtime(stage_dir, target, staged_binary, lld_tools)

    if is_windows_gnu_target(target):
        for src in windows_package_inputs(binary):
            copy_optional(src, stage_dir / src.name)

    verify_packaged_runtime_arch(stage_dir, target)

    return stage_dir

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
        if not is_target_buildable_with_current_llvm(t):
            arches = ", ".join(sorted(llvm_dylib_arches())) or "unknown"
            print(f"[!] Cannot build {t} with the current LLVM runtime architecture(s): {arches}")
            print("    Use a matching/universal LLVM prefix, or select a compatible target explicitly.")
            sys.exit(1)

        env = os.environ.copy()

        if is_windows_gnu_target(t):
            print("     [*] Applying MinGW + Windows LLVM environment")
            configure_windows_gnu_env(env)
        elif is_linux_target(t):
            configure_linux_release_env(env)

        subprocess.run(
            cargo_build_args(t),
            check=True,
            env=env
        )

    print("[+] Build finished.\n")

# ------------------------------------------------------
# Packaging
# ------------------------------------------------------
def cmd_package():
    print("[*] Packaging release binaries...")
    DIST_DIR.mkdir(exist_ok=True)
    packaged = 0
    missing = []

    for target in TARGETS:
        target_dir = TARGET_DIR / target / "release"
        binary = target_dir / BINARY_NAME

        formatted_target = target.replace("-unknown", "")

        out_name = f"{NAME}-v{VERSION}-{formatted_target}"

        if "windows" in target:
            bin_path = binary.with_suffix(".exe")
            if not bin_path.exists():
                print(f"[!] Missing binary: {bin_path}")
                missing.append(str(bin_path))
                continue

            stage_dir = stage_release_package(target, bin_path, out_name)
            zip_path = ROOT / f"{out_name}.zip"
            if zip_path.exists():
                zip_path.unlink()
            subprocess.run(
                ["zip", "-r", "-q", str(zip_path), stage_dir.name],
                cwd=DIST_DIR,
                check=True,
            )

            print(f"[+] Windows packaged → {zip_path}")
            packaged += 1

        else:
            if not binary.exists():
                print(f"[!] Missing binary: {binary}")
                missing.append(str(binary))
                continue

            stage_dir = stage_release_package(target, binary, out_name)
            tar_path = ROOT / f"{out_name}.tar.gz"
            subprocess.run([
                "tar", "-czf", tar_path,
                "-C", str(DIST_DIR),
                stage_dir.name
            ], check=True)

            print(f"[+] Packaged → {tar_path}")
            packaged += 1

    if missing:
        print("[!] Packaging failed because required release binaries are missing:")
        for path in missing:
            print(f"    {path}")
        print("    Run x.py build for the selected target(s) before packaging.")
        sys.exit(1)

    if packaged == 0:
        print("[!] No release packages were produced.")
        sys.exit(1)

    print("[+] Packaging complete.\n")

def cmd_gui():
    global TARGETS

    try:
        import tkinter as tk
        from tkinter import ttk, messagebox
    except ModuleNotFoundError:
        print("[!] Python tkinter support is not available in this interpreter.")
        print("    Non-GUI commands do not require tkinter. Use: x.py install|build|package|release|clean")
        print("    To use x.py gui, install a Python build with Tk support.")
        sys.exit(1)

    root = tk.Tk()
    root.title("Wave Build Manager")
    root.geometry("700x720")
    root.configure(bg="#1e1e1e")
    root.resizable(False, False)

    style = ttk.Style()
    style.theme_use("clam")

    # ---- Colors ----
    BG = "#1e1e1e"
    CARD = "#2b2b2b"
    ACCENT = "#4e8cff"
    TEXT = "#e6e6e6"
    MUTED = "#9aa0a6"

    style.configure("TFrame", background=BG)
    style.configure("Card.TFrame", background=CARD)
    style.configure("TLabel", background=BG, foreground=TEXT)
    style.configure("Header.TLabel",
                    background=BG,
                    foreground=TEXT,
                    font=("Segoe UI", 18, "bold"))
    style.configure("Sub.TLabel",
                    background=BG,
                    foreground=MUTED,
                    font=("Segoe UI", 10))
    style.configure("TCheckbutton",
                    background=CARD,
                    foreground=TEXT)
    style.configure("Accent.TButton",
                    background=ACCENT,
                    foreground="white",
                    font=("Segoe UI", 10, "bold"))
    style.map("Accent.TButton",
              background=[("active", "#6fa4ff")])

    host_os = platform.system()

    header = ttk.Frame(root)
    header.pack(fill="x", pady=20)

    ttk.Label(header,
              text="Wave Toolchain Manager",
              style="Header.TLabel").pack()

    ttk.Label(header,
              text=f"Host OS: {host_os}",
              style="Sub.TLabel").pack()

    # ----------------------------------
    # Card Container
    # ----------------------------------
    card = ttk.Frame(root, style="Card.TFrame")
    card.pack(fill="both", expand=True, padx=30, pady=10)

    canvas = tk.Canvas(card,
                       bg=CARD,
                       highlightthickness=0)

    scrollbar = ttk.Scrollbar(card,
                              orient="vertical",
                              command=canvas.yview)

    scroll_frame = ttk.Frame(canvas, style="Card.TFrame")

    scroll_frame.bind(
        "<Configure>",
        lambda e: canvas.configure(
            scrollregion=canvas.bbox("all")
        )
    )

    canvas.create_window((0, 0),
                         window=scroll_frame,
                         anchor="nw")

    canvas.configure(yscrollcommand=scrollbar.set)

    canvas.pack(side="left", fill="both", expand=True, padx=15, pady=15)
    scrollbar.pack(side="right", fill="y")

    vars_dict = {}
    native_targets = detect_targets()

    ttk.Label(scroll_frame,
              text="Available Targets",
              font=("Segoe UI", 12, "bold"),
              background=CARD,
              foreground=TEXT).pack(anchor="w", pady=10)

    for target in ALL_TARGETS:
        var = tk.BooleanVar()

        if target in native_targets:
            var.set(True)

        chk = ttk.Checkbutton(scroll_frame,
                              text=target,
                              variable=var,
                              style="TCheckbutton")
        chk.pack(anchor="w", pady=4)

        vars_dict[target] = var

    # ----------------------------------
    # Bottom Buttons
    # ----------------------------------
    button_frame = ttk.Frame(root)
    button_frame.pack(pady=20)

    status_var = tk.StringVar()
    status_var.set("Ready")

    def run_action(action):
        global TARGETS

        selected = [t for t, v in vars_dict.items() if v.get()]
        if not selected:
            messagebox.showerror("Error", "No target selected.")
            return

        TARGETS = selected
        status_var.set(f"{action.capitalize()} in progress...")
        root.update()

        root.destroy()

        if action == "install":
            cmd_install()
        elif action == "build":
            cmd_build()
        elif action == "release":
            cmd_release()

    ttk.Button(button_frame,
               text="Install",
               style="Accent.TButton",
               width=12,
               command=lambda: run_action("install")).grid(row=0, column=0, padx=10)

    ttk.Button(button_frame,
               text="Build",
               style="Accent.TButton",
               width=12,
               command=lambda: run_action("build")).grid(row=0, column=1, padx=10)

    ttk.Button(button_frame,
               text="Release",
               style="Accent.TButton",
               width=12,
               command=lambda: run_action("release")).grid(row=0, column=2, padx=10)

    ttk.Button(button_frame,
               text="Cancel",
               width=10,
               command=root.destroy).grid(row=0, column=3, padx=10)

    # ----------------------------------
    # Status Bar
    # ----------------------------------
    status_bar = ttk.Label(root,
                           textvariable=status_var,
                           relief="flat",
                           background=BG,
                           foreground=MUTED,
                           anchor="w")
    status_bar.pack(fill="x", side="bottom", pady=10, padx=20)

    root.mainloop()

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
    shutil.rmtree(DIST_DIR, ignore_errors=True)

    for f in os.listdir(ROOT):
        if f.endswith(".tar.gz") or f.endswith(".zip"):
            os.remove(f)

    print("[+] Cleaned.\n")

# ------------------------------------------------------
# CLI
# ------------------------------------------------------
def main():
    if len(sys.argv) < 2:
        print("Usage: x.py [install | build | package | release | clean | gui] [target...]")

        return

    cmd = sys.argv[1]
    selected_targets = sys.argv[2:]

    global TARGETS
    if selected_targets:
        unknown = [t for t in selected_targets if t not in ALL_TARGETS]
        if unknown:
            print("Unknown target(s):", ", ".join(unknown))
            print("Known targets:")
            for target in ALL_TARGETS:
                print("  ", target)
            sys.exit(1)
        TARGETS = selected_targets

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
    elif cmd == "gui":
        cmd_gui()
    else:
        print("Unknown command:", cmd)
        print("Usage: x.py [install | build | package | release | clean | gui] [target...]")

if __name__ == "__main__":
    main()
