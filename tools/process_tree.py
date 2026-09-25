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

"""Own compiler/program descendants for bounded test and example execution."""

import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time


class _WindowsJob:
    def __init__(self):
        import ctypes
        from ctypes import wintypes

        self.ctypes = ctypes
        self.api = ctypes.WinDLL("kernel32", use_last_error=True)

        class BasicLimits(ctypes.Structure):
            _fields_ = [
                ("process_time", ctypes.c_int64), ("job_time", ctypes.c_int64),
                ("flags", wintypes.DWORD), ("minimum", ctypes.c_size_t),
                ("maximum", ctypes.c_size_t), ("active_limit", wintypes.DWORD),
                ("affinity", ctypes.c_size_t), ("priority", wintypes.DWORD),
                ("scheduling", wintypes.DWORD),
            ]

        class ExtendedLimits(ctypes.Structure):
            _fields_ = [
                ("basic", BasicLimits), ("io_counters", ctypes.c_uint64 * 6),
                ("process_memory", ctypes.c_size_t), ("job_memory", ctypes.c_size_t),
                ("peak_process_memory", ctypes.c_size_t), ("peak_job_memory", ctypes.c_size_t),
            ]

        class Accounting(ctypes.Structure):
            _fields_ = [
                ("times", ctypes.c_int64 * 4), ("page_faults", wintypes.DWORD),
                ("total_processes", wintypes.DWORD), ("active_processes", wintypes.DWORD),
                ("terminated_processes", wintypes.DWORD),
            ]

        self.accounting_type = Accounting
        declarations = {
            "CreateJobObjectW": ([ctypes.c_void_p, wintypes.LPCWSTR], wintypes.HANDLE),
            "SetInformationJobObject": ([wintypes.HANDLE, ctypes.c_int, ctypes.c_void_p, wintypes.DWORD], wintypes.BOOL),
            "QueryInformationJobObject": ([wintypes.HANDLE, ctypes.c_int, ctypes.c_void_p, wintypes.DWORD, ctypes.c_void_p], wintypes.BOOL),
            "AssignProcessToJobObject": ([wintypes.HANDLE, wintypes.HANDLE], wintypes.BOOL),
            "OpenProcess": ([wintypes.DWORD, wintypes.BOOL, wintypes.DWORD], wintypes.HANDLE),
            "TerminateJobObject": ([wintypes.HANDLE, wintypes.UINT], wintypes.BOOL),
            "CloseHandle": ([wintypes.HANDLE], wintypes.BOOL),
        }
        for name, (arguments, result) in declarations.items():
            function = getattr(self.api, name)
            function.argtypes = arguments
            function.restype = result
        self.handle = self.api.CreateJobObjectW(None, None)
        if not self.handle:
            raise ctypes.WinError(ctypes.get_last_error())
        limits = ExtendedLimits()
        limits.basic.flags = 0x2000  # JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE
        if not self.api.SetInformationJobObject(self.handle, 9, ctypes.byref(limits), ctypes.sizeof(limits)):
            error = ctypes.WinError(ctypes.get_last_error())
            self.close()
            raise error

    def assign(self, pid):
        handle = self.api.OpenProcess(0x0101, False, pid)  # SET_QUOTA | TERMINATE
        if not handle:
            raise self.ctypes.WinError(self.ctypes.get_last_error())
        try:
            if not self.api.AssignProcessToJobObject(self.handle, handle):
                raise self.ctypes.WinError(self.ctypes.get_last_error())
        finally:
            self.api.CloseHandle(handle)

    def terminate(self):
        if not self.api.TerminateJobObject(self.handle, 1):
            raise self.ctypes.WinError(self.ctypes.get_last_error())
        deadline = time.monotonic() + 5
        while True:
            accounting = self.accounting_type()
            if not self.api.QueryInformationJobObject(self.handle, 1, self.ctypes.byref(accounting), self.ctypes.sizeof(accounting), None):
                raise self.ctypes.WinError(self.ctypes.get_last_error())
            if accounting.active_processes == 0:
                return
            if time.monotonic() >= deadline:
                raise TimeoutError("Windows test job did not finish terminating")
            time.sleep(0.01)

    def close(self):
        if self.handle:
            self.api.CloseHandle(self.handle)
            self.handle = None


class ProcessTree:
    """A POSIX session or Windows job, confined to this test's processes."""

    def __init__(self, args, **kwargs):
        self.args = args
        self.job = None
        self.bootstrap = None
        self.process = None
        self.closed = False
        self.terminated = False
        # Captured stdout/stderr use replacement decoding by default so a
        # program that writes non-UTF-8 bytes cannot raise UnicodeDecodeError
        # and abort a suite before its report is written. Callers that need
        # strict decoding can pass errors= explicitly. Source and manifest
        # readers stay strict and do not go through this helper.
        if kwargs.get("text") and "errors" not in kwargs:
            kwargs["errors"] = "replace"
        try:
            if os.name == "nt":
                self.job = _WindowsJob()
                self.bootstrap = tempfile.TemporaryDirectory(prefix="wave-process-job-")
                ready = Path(self.bootstrap.name) / "ready"
                # The supervisor must not launch the compiler until assignment.
                # This removes the Popen/AssignProcessToJobObject child-spawn race.
                command = [sys.executable, str(Path(__file__).resolve()), "--supervise", str(ready), *args]
                self.process = subprocess.Popen(command, **kwargs)
                self.job.assign(self.process.pid)
                ready.touch()
            else:
                self.process = subprocess.Popen(args, start_new_session=True, **kwargs)
        except BaseException:
            if self.process is not None:
                self.process.kill()
                self.process.communicate()
            if self.job is not None:
                self.job.close()
            if self.bootstrap is not None:
                self.bootstrap.cleanup()
            raise

    def terminate(self):
        if self.terminated:
            return
        if self.job is not None:
            self.job.terminate()
        else:
            try:
                os.killpg(self.process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
        # Do not signal the group again after communicate() has reaped its
        # leader: the numeric process-group ID may no longer belong to us.
        # Failed termination remains an error and may be retried by cleanup.
        self.terminated = True

    def close(self):
        if self.closed:
            return
        try:
            self.terminate()
        finally:
            if self.job is not None:
                self.job.close()
            try:
                self.process.communicate()
            finally:
                if self.bootstrap is not None:
                    self.bootstrap.cleanup()
                self.closed = True

    def __enter__(self):
        return self

    def __exit__(self, *_):
        self.close()


def run_process(args, *, input=None, timeout=None, check=False, **kwargs):
    if kwargs.pop("capture_output", False):
        if "stdout" in kwargs or "stderr" in kwargs:
            raise ValueError("capture_output cannot be combined with stdout/stderr")
        kwargs.update(stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if input is not None:
        if "stdin" in kwargs:
            raise ValueError("stdin and input cannot be used together")
        kwargs["stdin"] = subprocess.PIPE
    with ProcessTree(args, **kwargs) as tree:
        try:
            stdout, stderr = tree.process.communicate(input=input, timeout=timeout)
        except subprocess.TimeoutExpired:
            tree.terminate()
            stdout, stderr = tree.process.communicate()
            raise subprocess.TimeoutExpired(args, timeout, output=stdout, stderr=stderr) from None
        result = subprocess.CompletedProcess(args, tree.process.returncode, stdout, stderr)
        if check:
            result.check_returncode()
        return result


def timeout_output(error):
    def text(value):
        return value.decode(errors="replace") if isinstance(value, bytes) else value or ""
    return "\n".join(part.rstrip() for part in (text(error.output), text(error.stderr)) if part.strip())


def _supervise(ready, command):
    deadline = time.monotonic() + 30
    while not Path(ready).exists():
        if time.monotonic() >= deadline:
            raise TimeoutError("test supervisor was not assigned to its Windows job")
        time.sleep(0.01)
    status = subprocess.call(command)
    # Preserve all native Windows exit bits, including access-violation codes.
    os._exit(status if status < 0x80000000 else status - 0x100000000)


if __name__ == "__main__":
    if len(sys.argv) < 4 or sys.argv[1] != "--supervise":
        raise SystemExit("process_tree.py is an internal test-runner helper")
    _supervise(sys.argv[2], sys.argv[3:])
