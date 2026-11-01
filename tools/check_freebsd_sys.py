#!/usr/bin/env python3
# SPDX-License-Identifier: MPL-2.0
"""Build libc-independent sys cases and run them in a supplied FreeBSD VM.

Requires clang/lld, qemu-img, genisoimage and the selected qemu-system binary.
The input qcow2 image is read through a fresh writable overlay. No downloads,
network device, host filesystem sharing or third-party Python modules are used.
"""

import argparse
import os
from pathlib import Path
import re
import select
import shutil
import subprocess
import sys
import tempfile
import time

if __package__ in (None, ""):
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from tools.validation_reports import validate_report_path, write_report

ROOT = Path(__file__).resolve().parents[1]
ARCHES = {"amd64": "x86_64", "arm64": "aarch64", "riscv64": "riscv64"}
# Kernel device messages can interleave even inside 'pathname' or '/bin/sh:'.
# Accept the short leading prefix, then require the actual root shell prompt.
SINGLE_USER_PROMPT = r"Enter full "


class Commands:
    def __init__(self, report, log, timeout):
        self.report, self.log, self.timeout = report, log, timeout

    def run(self, *args):
        command = [str(arg) for arg in args]
        entry = {"command": command, "status": "running"}
        self.report["commands"].append(entry)
        with self.log.open("ab") as log:
            log.write((repr(command) + "\n").encode())
            log.flush()
            try:
                result = subprocess.run(command, stdout=log, stderr=subprocess.STDOUT,
                    timeout=self.timeout, check=False)
                entry["exit_code"] = result.returncode
                entry["status"] = "pass" if result.returncode == 0 else "fail"
                if result.returncode != 0:
                    raise RuntimeError(f"Command exited {result.returncode}: {command}; see {self.log}")
            except subprocess.TimeoutExpired:
                entry["status"] = "timeout"
                raise
            except OSError:
                entry["status"] = "launch_error"
                raise


class Console:
    def __init__(self, process, log, timeout=240):
        self.boot_deadline = time.monotonic() + timeout
        self.process = process
        self.log = log
        self.buffer = ""

    def expect(self, pattern, timeout=None):
        deadline = self.boot_deadline if timeout is None else time.monotonic() + timeout
        while True:
            match = re.search(pattern, self.buffer)
            if match:
                self.buffer = self.buffer[match.end():]
                return match
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise TimeoutError(f"Guest did not reach {pattern!r}; see {self.log.name}")
            ready, _, _ = select.select([self.process.stdout], [], [], min(remaining, 1))
            if ready:
                chunk = os.read(self.process.stdout.fileno(), 65536)
                if not chunk:
                    raise RuntimeError(f"Guest exited before {pattern!r}; see {self.log.name}")
                self.log.write(chunk)
                self.log.flush()
                self.buffer += chunk.decode("utf-8", errors="replace")

    def send(self, command, paced=False):
        for part in command if paced else [command]:
            self.process.stdin.write(part.encode())
            self.process.stdin.flush()
            if paced:
                # The BIOS loader's serial input buffer is much smaller than sh's.
                time.sleep(0.3)

    def loader_command(self, command):
        # Fixed delays do not bound how far a TCG guest can lag behind the
        # sender. Wait for each loader echo so its small input buffer never
        # accumulates a whole command (or loses the final carriage return).
        for character in command:
            self.send(character)
            self.expect(re.escape(character))
        self.send("\r")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--arch", choices=ARCHES, required=True)
    parser.add_argument("--image", type=Path, required=True, help="Verified official FreeBSD UFS qcow2 image")
    parser.add_argument("--firmware", type=Path, help="AArch64 QEMU_EFI.fd")
    parser.add_argument("--kernel", type=Path, help="FreeBSD RISC-V ELF kernel from the matching release")
    parser.add_argument("--compiler", type=Path, default=ROOT / "target/debug/wavec")
    parser.add_argument("--clang", default="clang-21")
    parser.add_argument("--linker", default="ld.lld")
    parser.add_argument("--out-dir", type=Path, default=ROOT / ".tmp/freebsd-sys")
    parser.add_argument("--report-json", type=Path, help="Result file retained on success and failure")
    parser.add_argument("--command-timeout", type=int, default=120)
    parser.add_argument("--boot-timeout", type=int, default=240)
    parser.add_argument("--case-timeout", type=int, default=60)
    args = parser.parse_args()
    report_path = args.report_json or args.out_dir / "report.json"
    # Guard failure reports too, before creating directories or launching tools.
    try:
        protected = report_inputs(args)
        validate_report_path(report_path, protected)
    except (OSError, ValueError) as error:
        print(f"Unsafe report destination: {error}", file=sys.stderr)
        return 1
    report = {"schema_version": 1, "arch": args.arch, "status": "running",
        "phase": "validation", "commands": [], "cases": []}
    status = 0
    try:
        if min(args.command_timeout, args.boot_timeout, args.case_timeout) <= 0:
            raise ValueError("timeouts must be positive")
        execute(args, report)
        report["status"] = "pass"
    except (Exception, KeyboardInterrupt) as error:
        report["status"] = "interrupted" if isinstance(error, KeyboardInterrupt) else "fail"
        report["error"] = f"{type(error).__name__}: {error}"
        print(report["error"], flush=True)
        status = 1
    try:
        validate_report_path(report_path, protected)
        write_report(report_path, report)
    except (OSError, ValueError) as error:
        print(f"Failed to write report: {error}", file=sys.stderr)
        return 1
    return status


def report_inputs(args):
    inputs = [p for p in (args.image, args.compiler, args.firmware, args.kernel) if p is not None]
    # Include imported helper modules and std metadata, not just case entry points.
    inputs.extend((ROOT / "tests/cases").rglob("*.wave"))
    for directory in (ROOT / "std", ROOT / "tests/fixtures/freebsd_case_runtime"):
        inputs.extend(p for p in directory.rglob("*") if p.is_file())
    for tool in (args.clang, args.linker, "genisoimage", "qemu-img", f"qemu-system-{ARCHES[args.arch]}"):
        inputs.append(Path(shutil.which(tool) or tool))
    return inputs


def discover_cases(suite):
    cases = {}
    for entry in suite.glob("test*"):
        name = entry.stem if entry.is_file() and entry.suffix == ".wave" else entry.name
        if not re.fullmatch(r"test[0-9]+", name):
            raise ValueError(f"invalid FreeBSD case name: {entry}")
        source = entry / "main.wave" if entry.is_dir() else entry
        if not source.is_file() or source.suffix != ".wave":
            raise ValueError(f"missing FreeBSD case source: {source}")
        number = int(name[4:])
        if number in cases:
            raise ValueError(f"ambiguous FreeBSD case number: {entry}")
        cases[number] = (name, source)
    if not cases:
        raise ValueError("No FreeBSD provider cases selected")
    return [cases[number] for number in sorted(cases)]


def execute(args, report):
    cases = discover_cases(ROOT / f"tests/cases/freebsd/{args.arch}")
    for path in [args.image, args.compiler]:
        if not path.is_file():
            raise ValueError(f"File does not exist: {path}")
    if args.arch == "arm64" and (args.firmware is None or not args.firmware.is_file()):
        raise ValueError("--firmware must name the matching UEFI firmware")
    if args.arch == "riscv64" and (args.kernel is None or not args.kernel.is_file()):
        raise ValueError("RISC-V requires --kernel from the matching FreeBSD release")
    args.out_dir.mkdir(parents=True, exist_ok=True)
    work = Path(tempfile.mkdtemp(prefix=f"{args.arch}-", dir=args.out_dir.resolve()))
    print(f"Artifacts and guest log: {work}", flush=True)
    report["work_dir"] = str(work)
    report["phase"] = "build"
    runner = Commands(report, work / "build.log", args.command_timeout)
    iso_root = work / "cases"
    iso_root.mkdir()
    triple = f"{ARCHES[args.arch]}-unknown-freebsd"
    runtime = work / "start.o"
    runner.run(args.clang, f"--target={triple}", "-O2", "-fno-builtin", "-ffreestanding",
        "-fno-stack-protector", "-c", ROOT / "tests/fixtures/freebsd_case_runtime/start.c", "-o", runtime)
    report["cases"] = [{"name": f"{name}-{opt}", "status": "not_run"}
        for name, source in cases for opt in ("O0", "O2")]
    for name, source in cases:
        for opt in ("O0", "O2"):
            objects = work / f"{name}-{opt}"
            runner.run(args.compiler.resolve(), "build", source,
                "--std-root", ROOT / "std", "--target", triple, "--emit=obj", f"-{opt}", "--out-dir", objects)
            runner.run(args.linker, "-static", "-e", "_start", runtime, objects / f"{source.stem}.o",
                "-o", iso_root / f"{name}-{opt}")
    (iso_root / "run.sh").write_text(
        "for name in " + " ".join(name for name, source in cases) + "; do\n  for opt in O0 O2; do\n"
        "    /mnt/$name-$opt\n    echo \"WAVE-RESULT $name-$opt $?\"\n"
        "  done\ndone\n"
    )
    iso = work / "cases.iso"
    runner.run("genisoimage", "-quiet", "-R", "-o", iso, iso_root)
    overlay = work / "guest.qcow2"
    runner.run("qemu-img", "create", "-f", "qcow2", "-F", "qcow2", "-b", args.image.resolve(), overlay)
    block_device = "virtio-blk-device" if args.arch == "riscv64" else "virtio-blk-pci"
    command = [f"qemu-system-{ARCHES[args.arch]}", "-m", "768", "-smp", "2", "-accel", "tcg",
        "-drive", f"file={overlay},format=qcow2,if=none,id=system",
        "-device", f"{block_device},drive=system",
        "-drive", f"file={iso},format=raw,if=none,id=cases,readonly=on",
        "-device", f"{block_device},drive=cases", "-nographic", "-monitor", "none", "-nic", "none"]
    if args.arch == "amd64":
        command += ["-object", "rng-random,filename=/dev/urandom,id=rng0",
            "-device", "virtio-rng-pci,rng=rng0"]
    if args.arch == "arm64":
        command += ["-machine", "virt", "-cpu", "cortex-a72", "-bios", str(args.firmware.resolve())]
    elif args.arch == "riscv64":
        # FreeBSD 14.3 boots with the SBI timer on QEMU 10; its Sstc path stalls.
        command += ["-machine", "virt,acpi=off", "-cpu", "rv64,sstc=false", "-bios", "default",
            "-kernel", str(args.kernel.resolve()), "-append", "-s"]
    report["phase"] = "guest_boot"
    report["guest_command"] = command
    with (work / "guest.log").open("wb") as log:
        process = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        console = Console(process, log, args.boot_timeout)
        try:
            if args.arch == "riscv64":
                # Direct boot preserves QEMU's FDT for FreeBSD 14.3. A kernel
                # without loader-provided root metadata asks for its root disk.
                prompt = console.expect(r"mountroot> |" + SINGLE_USER_PROMPT)
                if prompt.group(0).startswith("mountroot>"):
                    console.send("ufs:/dev/vtbd0p3\n")
                    console.expect(SINGLE_USER_PROMPT)
            else:
                console.expect("Autoboot in")
                console.send("3", paced=True)
                console.expect(r"OK ")
                if args.arch == "amd64":
                    console.loader_command("set console=comconsole")
                    console.expect(r"OK ")
                console.loader_command("boot -s")
                console.expect(SINGLE_USER_PROMPT)
            console.send("\r")
            console.expect(r"root@[^\r\n]*# ")
            console.send("mount -uw /\nmount -t cd9660 /dev/vtbd1 /mnt\nsh /mnt/run.sh\n")
            report["phase"] = "guest_cases"
            for entry in report["cases"]:
                entry["status"] = "running"
                try:
                    result = console.expect(rf"WAVE-RESULT {entry['name']} (\d+)\r", timeout=args.case_timeout)
                except (Exception, KeyboardInterrupt):
                    entry["status"] = "missing_result"
                    raise
                status = int(result.group(1))
                entry["exit_code"] = status
                entry["status"] = "pass" if status == 0 else "fail"
                print(f"{entry['status'].upper()} {args.arch} {entry['name']} exit={status}", flush=True)
            report["phase"] = "guest_shutdown"
            console.send("poweroff\n")
            if process.wait(timeout=60) != 0:
                raise RuntimeError(f"QEMU failed during shutdown; see {log.name}")
        finally:
            if process.poll() is None:
                process.terminate()
                try:
                    process.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait()
    if any(entry["status"] != "pass" for entry in report["cases"]):
        raise RuntimeError("FreeBSD provider cases failed; see guest.log and report")


if __name__ == "__main__":
    raise SystemExit(main())
