"""Check all manifest sources and preserve every result, including failures."""
import argparse
import json
import os
from pathlib import Path
import subprocess
import tempfile
import sys

try:
    from tools.case_manifest import CASES_ROOT, ROOT, load_case_manifest
    from tools.process_tree import run_process, timeout_output
except ModuleNotFoundError:
    from case_manifest import CASES_ROOT, ROOT, load_case_manifest
    from process_tree import run_process, timeout_output


def _write_report(report, payload):
    report = Path(report)
    report.parent.mkdir(parents=True, exist_ok=True)
    fd, temp_name = tempfile.mkstemp(prefix=f".{report.name}.", suffix=".tmp", dir=report.parent)
    temp_path = Path(temp_name)
    try:
        try:
            stream = os.fdopen(fd, "w", encoding="utf-8")
        except Exception:
            os.close(fd)
            raise
        with stream:
            stream.write(json.dumps(payload, indent=2) + "\n")
        temp_path.replace(report)
    finally:
        temp_path.unlink(missing_ok=True)


def check_sources(wavec, sources, report, timeout=15):
    records = []
    report = Path(report)
    def save():
        _write_report(report, {"phase": "source-check", "results": records})
    save()
    for source in sources:
        record = {"source": source, "status": "failed", "exit_code": None}
        try:
            result = run_process(
                [str(wavec), "check", str(CASES_ROOT / source)],
                cwd=ROOT, capture_output=True, text=True, timeout=timeout,
            )
            record.update(exit_code=result.returncode, stdout=result.stdout, stderr=result.stderr)
            if result.returncode == 0:
                record["status"] = "passed"
        except subprocess.TimeoutExpired as error:
            record.update(status="timeout", error=timeout_output(error))
        except OSError as error:
            record["error"] = str(error)
        records.append(record)
        save()  # Retain completed checks even if a later invocation is interrupted.
        print(f"[{record['status']}] {source}", flush=True)
        if record["status"] != "passed":
            print(record.get("stderr", "") or record.get("error", ""), flush=True)
    return 0 if records and all(r["status"] == "passed" for r in records) else 1


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--wavec", type=Path, required=True)
    parser.add_argument("--report-json", type=Path, required=True)
    args = parser.parse_args(argv)
    try:
        sources = load_case_manifest().sources()
    except (ValueError, OSError) as error:
        _write_report(args.report_json, {"phase": "source-check", "error": str(error), "results": []})
        print(error, file=sys.stderr)
        return 1
    return check_sources(args.wavec.resolve(), sources, args.report_json)


if __name__ == "__main__":
    raise SystemExit(main())
