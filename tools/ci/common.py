# SPDX-License-Identifier: MPL-2.0
"""Bounded process execution and failure-preserving shared procedure runner."""

import argparse
import ast
import json
import os
import re
import signal
import subprocess
import sys
import tempfile
import time
from pathlib import Path

import tomllib

from tools.ci import environment
from tools.ci.targets import PINS, check_lane, lanes, resolve
from tools.process_tree import run_process, timeout_output
from tools.validation_reports import validate_report_path, write_report

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent


class Cancelled(KeyboardInterrupt):
    """CI termination is distinct from a user's keyboard interruption."""


def cancel_from_signal(signum, frame):
    raise Cancelled()


def inventory():
    return json.loads((HERE / "plans.json").read_text()), json.loads(
        (HERE / "procedures.json").read_text()
    )


def evaluate(expression, context):
    """Evaluate the small, checked expression subset used by the migrated plans."""
    expression = expression.strip().removeprefix("${{").removesuffix("}}").strip()
    # Rewrite only tokens outside string literals. Substituting into quoted
    # text also corrupts comparisons and can turn literal context names into
    # Python source, so tokenize strings and operators in a single pass.
    tokens = re.compile(
        r"""'(?:\\.|[^'\\])*'|"(?:\\.|[^"\\])*"|&&|\|\||!(?!=)|"""
        r"""\b(?:always|cancelled)\(\)|"""
        r"""\b(?:matrix|steps|runner|inputs|github)\.[A-Za-z0-9_.-]+"""
    )

    def replace_token(match):
        token = match[0]
        if token.startswith(("'", '"')):
            return token
        if token == "&&":
            return " and "
        if token == "||":
            return " or "
        if token == "!":
            return " not "
        if token == "always()":
            return "True"
        if token == "cancelled()":
            return repr(context.get("cancelled", False))
        return repr(context.get(token, ""))

    expression = tokens.sub(replace_token, expression)

    def visit(node):
        if isinstance(node, ast.Constant):
            return node.value
        if isinstance(node, ast.Name) and node.id in ("True", "False"):
            return node.id == "True"
        if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.Not):
            return not visit(node.operand)
        if isinstance(node, ast.BoolOp):
            value = visit(node.values[0])
            for child in node.values[1:]:
                if isinstance(node.op, ast.And):
                    value = visit(child) if value else value
                else:
                    value = value if value else visit(child)
            return value
        if isinstance(node, ast.Compare) and len(node.ops) == 1:
            a, b = visit(node.left), visit(node.comparators[0])
            if isinstance(node.ops[0], ast.Eq):
                return a == b
            if isinstance(node.ops[0], ast.NotEq):
                return a != b
        raise ValueError(f"unsupported plan expression: {expression}")

    return visit(ast.parse(expression.strip(), mode="eval").body)


class Runner:
    def __init__(
        self, target, report, *, provision=False, context=None, environment=None
    ):
        self.target = target
        self.report = Path(report).resolve()
        inputs = [
            ROOT / "Cargo.toml",
            ROOT / "Cargo.lock",
            ROOT / "meson.build",
            ROOT / "meson.options",
            *(
                p
                for folder in (
                    "src",
                    "front",
                    "llvm",
                    "whale",
                    "utils",
                    "tests",
                    "std",
                    "tools",
                    ".github",
                )
                for p in (ROOT / folder).rglob("*")
                if p.is_file()
            ),
            *ROOT.glob("target/**/wavec"),
            *ROOT.glob("target/**/wavec.exe"),
        ]
        validate_report_path(self.report, inputs)
        self.provision = provision
        self.env = dict(os.environ if environment is None else environment)
        for key, value in PINS.items():
            self.env[key] = value
        self.env["FREEBSD_UNPACKED_IMAGE"] = self.env["FREEBSD_IMAGE"].removesuffix(
            ".xz"
        )
        self.env.setdefault(
            "RELEASE_VERSION",
            tomllib.loads((ROOT / "Cargo.toml").read_text())["package"]["version"],
        )
        self.env.update(
            CARGO_BUILD_JOBS="2",
            PYTHONUTF8="1",
            PYTHONUNBUFFERED="1",
            PYTHONIOENCODING="utf-8",
        )
        self.temp = Path(
            tempfile.mkdtemp(
                prefix=f"wave-ci-{target.id}-",
                dir=self.env.get("RUNNER_TEMP", tempfile.gettempdir()),
            )
        )
        self.env.setdefault("RUNNER_TEMP", str(self.temp))
        self.env.setdefault("GITHUB_WORKSPACE", str(ROOT))
        for key in ("GITHUB_ENV", "GITHUB_PATH", "GITHUB_OUTPUT"):
            self.env.setdefault(key, str(self.temp / key.lower()))
        self.context = {
            "runner.temp": self.env["RUNNER_TEMP"],
            "matrix.id": target.id,
            "matrix.target": target.rust_target,
            "matrix.archive_target": target.archive_target,
            "matrix.arch": target.triple.split("-")[0],
            "github.token": self.env.get("GH_TOKEN", ""),
            "steps.python_setup.outcome": "success",
        }
        if context:
            self.context.update({f"matrix.{k}": v for k, v in context.items()})
        self.data = {"schema_version": 1, "target": target.describe(), "stages": []}
        self.current = None
        self.cancelled = False

    def expand(self, text):
        text = str(text)
        if text == "@python":
            return sys.executable
        text = re.sub(
            r"\$\{\{\s*(.*?)\s*\}\}|@\{(.*?)\}",
            lambda m: str(evaluate(m[1] or m[2], self.context)),
            text,
        )
        pattern = r"\$env:([A-Za-z_][A-Za-z0-9_]*)|\$\{([A-Za-z_][A-Za-z0-9_]*)\}|\$([A-Za-z_][A-Za-z0-9_]*)"

        def variable(match):
            key = next(v for v in match.groups() if v is not None)
            if key not in self.env:
                raise ValueError(f"missing environment input: {key}")
            return self.env[key]

        return re.sub(pattern, variable, text)

    def save(self):
        self.data["summary"] = {
            status: sum(s["status"] == status for s in self.data["stages"])
            for status in (
                "pass",
                "fail",
                "timeout",
                "interrupted",
                "cancelled",
                "not_run",
                "skipped",
                "unavailable",
            )
        }
        write_report(self.report, self.data)

    def summarize(self):
        destination = self.env.get("GITHUB_STEP_SUMMARY")
        if not destination:
            return

        def cell(value):
            return str(value).replace("&", "&amp;").replace("<", "&lt;").replace(
                ">", "&gt;"
            ).replace("|", "&#124;").replace("\n", " ").replace("\r", " ")

        lines = [
            f"### {cell(self.target.id)}", "",
            "| Phase | Stage | Result | Seconds |",
            "| --- | --- | --- | ---: |",
        ]
        for row in self.data["stages"]:
            duration = row.get("duration_seconds")
            elapsed = f"{duration:.1f}" if duration is not None else "—"
            lines.append(
                f"| {cell(row['phase'])} | {cell(row['name'])} | {cell(row['status'])} | {elapsed} |"
            )
        try:
            with Path(destination).open("a", encoding="utf-8") as stream:
                stream.write("\n".join(lines) + "\n\n")
        except OSError as error:
            # Presentation must not change the actual validation result.
            print(f"Could not write Actions summary: {error}", file=sys.stderr)

    def setenv(self, key, value):
        record = environment.encode(key, value)
        environment.append(self.env["GITHUB_ENV"], record)
        self.env[key] = str(value)

    def addpath(self, path):
        self.env["PATH"] = str(path) + os.pathsep + self.env.get("PATH", "")
        with Path(self.env["GITHUB_PATH"]).open("a", encoding="utf-8") as f:
            f.write(str(path) + "\n")

    def refresh_environment(self):
        path = Path(self.env["GITHUB_ENV"])
        if path.exists():
            self.env.update(environment.decode(path.read_bytes()))
        path = Path(self.env["GITHUB_PATH"])
        if path.exists():
            for line in path.read_text(encoding="utf-8-sig").splitlines():
                if line and line not in self.env.get("PATH", "").split(os.pathsep):
                    self.env["PATH"] = line + os.pathsep + self.env.get("PATH", "")

    def run(
        self,
        args,
        *,
        timeout=None,
        cwd=None,
        input=None,
        expected=0,
        env=None,
        clean_env=False,
    ):
        if timeout is None:
            timeout = (
                self.current.get("timeout_seconds", 1800) if self.current else 1800
            )
        command = [self.expand(a) for a in args]
        record = {
            "command": command,
            "cwd": str(cwd or ROOT),
            "expected_exit": expected,
            "status": "running",
        }
        if self.current is not None:
            self.current["commands"].append(record)
        self.save()
        started = time.monotonic()
        log = (
            self.temp
            / f"command-{sum(len(s.get('commands', [])) for s in self.data['stages'])}.log"
        )
        record["log"] = str(log)
        try:
            if not command:
                raise ValueError("command vector cannot be empty")
            if command[0] == "cargo":
                if len(command) < 2:
                    raise ValueError("cargo command requires a subcommand")
                if command[1] in ("build", "test", "check", "clippy", "doc"):
                    if "--jobs" not in command:
                        command[2:2] = ["--jobs", "2"]
                    if "--locked" not in command:
                        command.insert(2, "--locked")
            result = run_process(
                command,
                cwd=cwd or ROOT,
                env=dict(env or {}) if clean_env else dict(self.env, **(env or {})),
                input=input,
                timeout=timeout,
                capture_output=True,
                stream_output=True,
                stream_log=log,
                text=True,
                errors="replace",
            )
            record.update(
                actual_exit=result.returncode,
                status="pass" if result.returncode == expected else "fail",
            )
            log = (
                self.temp
                / f"command-{sum(len(s.get('commands',[])) for s in self.data['stages'])}.log"
            )
            log.write_text(result.stdout + result.stderr, encoding="utf-8")
            record["log"] = str(log)
            self.refresh_environment()
            if result.returncode != expected:
                raise RuntimeError(
                    f"command exited {result.returncode}, expected {expected}: {command}\n{(result.stdout+result.stderr)[-6000:]}"
                )
            return result.stdout
        except ValueError as error:
            record.update(status="fail", error=str(error))
            raise
        except subprocess.TimeoutExpired as error:
            log = (
                self.temp
                / f"timeout-{len(self.data['stages'])}-{len(self.current['commands']) if self.current else 0}.log"
            )
            log.write_text(timeout_output(error), encoding="utf-8")
            record.update(
                status="timeout", log=str(log), output=timeout_output(error)[-6000:]
            )
            raise
        except KeyboardInterrupt as error:
            record["status"] = (
                "cancelled" if isinstance(error, Cancelled) else "interrupted"
            )
            raise
        except OSError as error:
            record.update(status="unavailable", error=str(error))
            raise
        finally:
            record["elapsed_seconds"] = round(time.monotonic() - started, 3)
            cmd_display = command[0] if command else "<empty>"
            print(
                f"Command {record['status']} after {record['elapsed_seconds']:.1f}s: {cmd_display}",
                flush=True,
            )
            self.save()

    def execute(self, selected, *, operations=None, phases=None):
        from tools.ci.build import OPERATIONS
        from tools.ci.test import OPERATIONS as TEST_OPERATIONS
        from tools.ci.package import OPERATIONS as PACKAGE_OPERATIONS
        from tools.ci.release import OPERATIONS as RELEASE_OPERATIONS

        handlers = (
            OPERATIONS
            | TEST_OPERATIONS
            | PACKAGE_OPERATIONS
            | RELEASE_OPERATIONS
            | (operations or {})
        )
        plans, procedures = inventory()
        for plan_name in selected:
            if plan_name not in plans:
                raise ValueError(f"unknown lane: {plan_name}")
        failed = False
        for plan_name in selected:
            self.data["stages"].extend(
                dict(step, plan=plan_name, status="not_run", commands=[])
                for step in plans[plan_name]["stages"]
                if phases is None or step["phase"] in phases
            )
        self.save()
        for plan_name in selected:
            plan = plans[plan_name]
            self.env.update({k: self.expand(v) for k, v in plan["env"].items()})
            rows = [row for row in self.data["stages"] if row["plan"] == plan_name]
            lane_failed = failed if plan_name.startswith("release/package-") else False
            for row in rows:
                self.current = row
                condition = row["condition"]
                allowed = (
                    bool(evaluate(condition, self.context))
                    if condition
                    else not lane_failed
                )
                if not allowed:
                    row.update(
                        status="not_run" if lane_failed else "skipped",
                        reason="stage prerequisite not satisfied",
                    )
                    self.context[f"steps.{row['id']}.outcome"] = "skipped"
                    self.save()
                    continue
                operation = procedures[row["procedure"]]
                name = operation["operation"]
                previous = self.env.copy()
                started = time.monotonic()
                grouped = self.env.get("GITHUB_ACTIONS") == "true"
                if grouped:
                    print(f"::group::{row['phase']} / {row['name']}", flush=True)
                try:
                    self.env.update(
                        {
                            k: self.expand(v)
                            for k, v in operation.get("env", {}).items()
                            if k != "STEP_OUTCOMES"
                        }
                    )
                    print(f"RUN {self.target.id}: {row['name']}", flush=True)
                    if name == "commands":
                        if operation.get("requires_provision") and not self.provision:
                            raise ValueError(
                                "this provisioning procedure requires --provision"
                            )
                        from tools.ci.validate import commands

                        commands(self, operation)
                    else:
                        handlers[name](self, operation)
                    row["status"] = "pass"
                except KeyboardInterrupt as error:
                    status = (
                        "cancelled" if isinstance(error, Cancelled) else "interrupted"
                    )
                    row.update(status=status, reason=status)
                    self.cancelled = True
                    self.save()
                    return 143 if isinstance(error, Cancelled) else 130
                except (
                    OSError,
                    ValueError,
                    RuntimeError,
                    subprocess.TimeoutExpired,
                ) as error:
                    row.update(
                        status=(
                            "timeout"
                            if isinstance(error, subprocess.TimeoutExpired)
                            else (
                                "unavailable"
                                if isinstance(error, FileNotFoundError)
                                else "fail"
                            )
                        ),
                        reason=str(error),
                    )
                    failed = lane_failed = True
                    print(f"FAIL {row['name']}: {error}", file=sys.stderr, flush=True)
                finally:
                    row["duration_seconds"] = time.monotonic() - started
                    if grouped:
                        print("::endgroup::", flush=True)
                    for key in operation.get("env", {}):
                        if key in previous:
                            self.env[key] = previous[key]
                        else:
                            self.env.pop(key, None)
                    self.context[f"steps.{row['id']}.outcome"] = (
                        "success" if row["status"] == "pass" else "failure"
                    )
                    output = Path(self.env["GITHUB_OUTPUT"])
                    with output.open("a", encoding="utf-8") as stream:
                        stream.write(
                            row["id"]
                            + "="
                            + self.context["steps." + row["id"] + ".outcome"]
                            + "\n"
                        )
                    self.save()
        return int(failed)


def main(family, argv=None):
    parser = argparse.ArgumentParser(
        description=f"Wave {family} procedures shared by local runs and CI"
    )
    parser.add_argument("--target", required=True)
    parser.add_argument(
        "--lane",
        action="append",
        help="Explicit inventoried lane; defaults to the target plan",
    )
    parser.add_argument(
        "--publish",
        action="store_true",
        help="Explicitly publish a fully validated release; release entry point only",
    )
    parser.add_argument(
        "--plan",
        action="store_true",
        help="Print commands and stages without running or provisioning",
    )
    parser.add_argument(
        "--provision",
        action="store_true",
        help="Explicitly allow platform package/tool installation",
    )
    parser.add_argument("--report-json", type=Path)
    args = parser.parse_args(argv)
    try:
        target = resolve(args.target)
        selected = args.lane or lanes(target, family)
        plans, procedures = inventory()
        for lane in selected:
            if lane not in plans:
                raise ValueError(f"unknown lane: {lane}")
            check_lane(target, lane)
        if any(lane == "release/publish" for lane in selected) and not (
            family == "release" and args.publish
        ):
            raise ValueError(
                "release/publish requires the release entry point and --publish"
            )
        if args.publish and selected != ["release/publish"]:
            raise ValueError("--publish requires only the release/publish lane")
        from tools.ci.validate import PHASES

        phases = (
            {"prerequisites", "build"}
            if family == "build"
            else PHASES if family == "validate" else None
        )
        if phases is not None:
            plans = {
                name: dict(
                    plan,
                    stages=[
                        stage for stage in plan["stages"] if stage["phase"] in phases
                    ],
                )
                for name, plan in plans.items()
            }
            if not all(plans[lane]["stages"] for lane in selected):
                raise ValueError(
                    "selected lane contains no stages for this entry point"
                )
        if args.plan:
            print(
                json.dumps(
                    {
                        "target": target.describe(),
                        "lanes": [{"name": lane, **plans[lane]} for lane in selected],
                        "procedures": {
                            s["procedure"]: procedures[s["procedure"]]
                            for lane in selected
                            for s in plans[lane]["stages"]
                        },
                    },
                    indent=2,
                )
            )
            return 0
        target.check_host()
        context = json.loads(os.environ.get("WAVE_CI_MATRIX", "{}"))
        inputs = json.loads(os.environ.get("WAVE_RELEASE_INPUTS", "{}"))
        for name, value in (("WAVE_CI_MATRIX", context), ("WAVE_RELEASE_INPUTS", inputs)):
            if not isinstance(value, dict):
                raise ValueError(f"{name} must be an object")
        runner = Runner(
            target,
            args.report_json or ROOT / ".tmp/ci" / f"{family}-{target.id}.json",
            provision=args.provision,
            context=context,
        )
        runner.publish_authorized = args.publish
        runner.context.update({f"inputs.{k}": v for k, v in inputs.items()})
        if inputs.get("version"):
            runner.env["RELEASE_VERSION"] = str(inputs["version"])
        previous = signal.signal(signal.SIGTERM, cancel_from_signal)
        try:
            from tools.ci.validate import PHASES

            phases = (
                {"prerequisites", "build"}
                if family == "build"
                else PHASES if family == "validate" else None
            )
            if (
                family == "release"
                and not args.publish
                and not {"release/validate", "release/gate"}.intersection(selected)
            ):
                from tools.ci.release import release_identity

                stage = dict(
                    id="identity",
                    name="Validate release identity",
                    plan="release/preflight",
                    phase="validation",
                    status="not_run",
                    commands=[],
                )
                runner.current = stage
                runner.data["stages"].append(stage)
                runner.save()
                try:
                    release_identity(runner, {})
                except (
                    OSError,
                    ValueError,
                    RuntimeError,
                    subprocess.TimeoutExpired,
                ) as error:
                    stage.update(status="fail", reason=str(error))
                    runner.save()
                    return 1
                except KeyboardInterrupt as error:
                    stage.update(
                        status=(
                            "cancelled"
                            if isinstance(error, Cancelled)
                            else "interrupted"
                        )
                    )
                    runner.save()
                    return 143 if isinstance(error, Cancelled) else 130
                stage["status"] = "pass"
                runner.save()
            return runner.execute(selected, phases=phases)
        finally:
            signal.signal(signal.SIGTERM, previous)
            runner.summarize()
    except (OSError, ValueError) as error:
        print(f"CI configuration error: {error}", file=sys.stderr)
        return 2
