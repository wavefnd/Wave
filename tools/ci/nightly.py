# SPDX-License-Identifier: MPL-2.0
"""Rolling Nightly identity, staging and recoverable GitHub publication.

Each generation is uploaded to a private draft before the old release is deleted.
The draft retains the publication number across interrupted replacements; a retry
finishes publishing it under the single public nightly tag.
"""

import argparse
import base64
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
from datetime import datetime, timedelta, timezone
from pathlib import Path
from urllib.request import Request, urlopen

import tomllib

from tools.ci.common import ROOT
from tools.ci.job_contract import required_names
from tools.ci.release import verify_metadata

REPO = "wavefnd/Wave"
SHA = r"[0-9a-f]{40}"
MARKER = r"<!-- wave-nightly-state:(.*?) -->"
STAGING_TAG = "nightly-staging"
INSTALLERS_FILE = Path(__file__).with_name("nightly_installers.json")


def sha(value):
    if not isinstance(value, str) or not re.fullmatch(SHA, value):
        raise ValueError("expected a full source SHA")
    return value


class GitHub:
    def api(self, endpoint, *, method="GET", data=None, optional=False):
        args = ["gh", "api", f"repos/{REPO}/{endpoint}", "--method", method]
        if data is not None:
            args += ["--input", "-"]
        result = subprocess.run(
            args,
            input=json.dumps(data) if data is not None else None,
            text=True,
            capture_output=True,
            timeout=120,
        )
        if result.returncode:
            if optional and "HTTP 404" in result.stderr:
                return None
            raise RuntimeError(f"GitHub {method} {endpoint}: {result.stderr.strip()}")
        return json.loads(result.stdout) if result.stdout.strip() else None

    def assets(self, release_id):
        result = []
        page = 1
        while True:
            batch = self.api(f"releases/{release_id}/assets?per_page=100&page={page}")
            result.extend(batch)
            if len(batch) < 100:
                return result
            page += 1

    def upload(self, path, tag="nightly"):
        # Never --clobber: an existing usable asset must not be deleted first.
        subprocess.run(
            ["gh", "release", "upload", tag, str(path), "--repo", REPO],
            check=True,
            timeout=1800,
        )


def ancestor(github, older, newer):
    sha(older)
    sha(newer)
    if older == newer:
        return True
    comparison = github.api(f"compare/{older}...{newer}")
    return (
        comparison["status"] == "ahead"
        and comparison["merge_base_commit"]["sha"] == older
    )


def eligible(github, run_id):
    if not str(run_id).isdigit() or int(run_id) <= 0:
        raise ValueError("a successful canonical master CI run ID is required")
    run = github.api(f"actions/runs/{run_id}")
    if (
        run.get("repository", {}).get("full_name") != REPO
        or run.get("head_repository", {}).get("full_name") != REPO
        or run.get("event") != "push"
        or run.get("head_branch") != "master"
        or run.get("path") != ".github/workflows/ci.yml"
    ):
        raise ValueError("Nightly requires successful canonical master push CI")
    source = sha(run["head_sha"])

    def source_text(path):
        record = github.api(f"contents/{path}?ref={source}")
        return base64.b64decode(record["content"]).decode("utf-8")

    required = required_names(
        source_text(".github/workflows/ci.yml"), source_text("tests/cases/cases.toml")
    )
    identity = (int(run_id), run["run_attempt"], source)

    def check_run(current):
        if (
            current.get("id", int(run_id)),
            current.get("run_attempt"),
            current.get("head_sha"),
        ) != identity or any(
            current.get(key) != run.get(key)
            for key in ("repository", "head_repository", "event", "head_branch", "path")
        ):
            raise ValueError("CI run identity changed during eligibility validation")
        if current.get("status") not in ("queued", "in_progress", "completed"):
            raise ValueError("CI run has an invalid validation status")
        if current.get("conclusion") not in (None, "success") or (
            current.get("status") == "completed"
            and current.get("conclusion") != "success"
        ):
            raise ValueError("CI run no longer completed successfully")
        return (
            current.get("status") == "completed"
            and current.get("conclusion") == "success"
        )

    check_run(run)
    current = run
    started = time.monotonic()
    deadline = started + 60
    for attempt in range(7):
        if attempt:
            remaining = deadline - time.monotonic()
            if remaining < 0:
                raise ValueError("CI job snapshots did not settle within 60 seconds")
            time.sleep(max(0, started + attempt * 10 - time.monotonic()))
            current = github.api(f"actions/runs/{run_id}")
            check_run(current)
        jobs = []
        page = 1
        total = None
        while True:
            snapshot = github.api(
                f"actions/runs/{run_id}/attempts/{run['run_attempt']}/jobs?per_page=100&page={page}"
            )
            batch = snapshot["jobs"]
            total = snapshot.get("total_count", total)
            jobs.extend(batch)
            if len(batch) < 100:
                break
            page += 1
        if any(j.get("conclusion") not in (None, "success") for j in jobs):
            raise ValueError(
                "CI jobs must all complete successfully; skipped jobs are not validation"
            )
        if (
            jobs
            and (total is None or len(jobs) == total)
            and {j.get("name") for j in jobs} == required
            and len(jobs) == len(required)
            and all(
                j.get("status") == "completed" and j.get("conclusion") == "success"
                for j in jobs
            )
        ):
            settled = github.api(f"actions/runs/{run_id}")
            if check_run(settled):
                break
        if attempt == 6:
            raise ValueError("CI job snapshots did not settle within 60 seconds")
    source = sha(run["head_sha"])
    master = github.api("git/ref/heads/master")["object"]["sha"]
    if not ancestor(github, source, master):
        raise ValueError("CI source is not in canonical master history")
    content = github.api(f"contents/Cargo.toml?ref={source}")
    version = tomllib.loads(base64.b64decode(content["content"]).decode())["package"][
        "version"
    ]
    if not re.fullmatch(r"\d+\.\d+\.\d+(?:-[0-9A-Za-z.-]+)?", version):
        raise ValueError("invalid compiler version")
    return {"source_sha": source, "compiler_version": version, "ci_run_id": int(run_id)}


def checkout_identity(identity):
    source = subprocess.run(
        ["git", "rev-parse", "HEAD"],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    ).stdout.strip()
    version = tomllib.loads((ROOT / "Cargo.toml").read_text())["package"]["version"]
    if source != identity["source_sha"] or version != identity["compiler_version"]:
        raise ValueError("checkout differs from the exact CI-validated source/version")


def installers_ready():
    # The installer change lives in wavefnd/wave-platform. Do not publish until
    # those reviewed bytes are actually deployed, regardless of merge order.
    pins = json.loads(INSTALLERS_FILE.read_text())
    required = {"https://wave-lang.dev/install.sh", "https://wave-lang.dev/install.ps1"}
    if set(pins) != required or any(
        not re.fullmatch(r"[0-9a-f]{64}", value) for value in pins.values()
    ):
        raise ValueError("both reviewed installer deployment hashes are required")
    for url, expected in pins.items():
        # Identify this client: the installer CDN rejects Python's default UA.
        request = Request(url, headers={"User-Agent": "Wave-Nightly/1.0"})
        try:
            with urlopen(request, timeout=30) as response:
                data = response.read(1024 * 1024 + 1)
        except OSError as error:
            raise RuntimeError(
                f"cannot verify deployed installer {url}: {error}"
            ) from error
        actual = hashlib.sha256(data).hexdigest()
        if len(data) > 1024 * 1024 or actual != expected:
            raise ValueError(
                f"reviewed installer is not deployed: {url} "
                f"(expected SHA-256 {expected}, received {actual})"
            )


def file_record(path):
    with path.open("rb") as stream:
        digest = hashlib.file_digest(stream, "sha256").hexdigest()
    return {"sha256": digest, "size": path.stat().st_size}


def stage(directory, output, identity, run_id, attempt, revision):
    if (
        not str(run_id).isdigit()
        or not str(attempt).isdigit()
        or min(int(run_id), int(attempt)) < 1
    ):
        raise ValueError("invalid Nightly workflow run identity")
    source, version = sha(identity["source_sha"]), identity["compiler_version"]
    verify_metadata(directory, version, source, revision)
    generation = f"{source}-{run_id}-{attempt}"
    output.mkdir()
    records = {}
    checksums = []
    for descriptor in sorted(directory.glob("*.metadata.json")):
        metadata = json.loads(descriptor.read_text())
        original = metadata["archive"]
        name = original.replace(f"wave-v{version}-", f"wave-nightly-{generation}-", 1)
        archive = output / name
        shutil.copyfile(directory / original, archive)
        if file_record(archive)["sha256"] != metadata["sha256"]:
            raise ValueError("package changed while staging Nightly")
        metadata.update(archive=name, channel="nightly", generation=generation)
        (output / (name + ".metadata.json")).write_text(
            json.dumps(metadata, indent=2) + "\n"
        )
        line = f"{metadata['sha256']}  {name}\n"
        (output / (name + ".sha256")).write_text(line)
        checksums.append(line)
    (output / f"nightly-{generation}-SHA256SUMS").write_text("".join(checksums))
    for path in sorted(output.iterdir()):
        records[path.name] = file_record(path)
    state = dict(
        identity,
        schema_version=1,
        generation=generation,
        run_id=int(run_id),
        built_at=datetime.now(timezone.utc).isoformat(),
        assets=records,
    )
    manifest = output / f"nightly-{generation}.json"
    manifest.write_text(json.dumps(state, indent=2) + "\n")
    state["assets"][manifest.name] = file_record(manifest)
    return state


def active_state(release, tag="nightly"):
    if release is None:
        return None
    if (
        release.get("tag_name") != tag
        or not release.get("prerelease")
    ):
        raise ValueError("refusing to replace an unmanaged Nightly release")
    match = re.search(MARKER, release.get("body") or "")
    if match is None:
        if (
            release.get("draft")
            and not release.get("body")
            and release.get("name") == "Wave Nightly"
        ):
            return None  # Interrupted first publication, before promotion.
        raise ValueError("Nightly release has no recovery state")
    state = json.loads(match[1])
    sha(state["source_sha"])
    if state.get("schema_version") != 1 or not state.get("assets"):
        raise ValueError("invalid Nightly recovery state")
    if release.get("name") != title(state):
        raise ValueError("Nightly title differs from its recovery state")
    return state


def verify_remote(github, release_id, state):
    assets = {a["name"]: a for a in github.assets(release_id)}
    for name, record in state["assets"].items():
        asset = assets.get(name, {})
        if (
            asset.get("state") != "uploaded"
            or asset.get("size") != record["size"]
            or asset.get("digest") != "sha256:" + record["sha256"]
        ):
            raise ValueError(f"unverified Nightly asset: {name}")


def set_tag(github, source):
    sha(source)
    tag = github.api("git/ref/tags/nightly", optional=True)
    if tag is None:
        github.api(
            "git/refs", method="POST", data={"ref": "refs/tags/nightly", "sha": source}
        )
    elif tag["object"]["sha"] != source:
        if tag["object"]["type"] != "commit":
            raise ValueError("Nightly must use a lightweight commit tag")
        github.api(
            "git/refs/tags/nightly", method="PATCH", data={"sha": source, "force": True}
        )


def title(state):
    # Existing unnumbered releases remain readable during migration.
    if "publication_number" not in state:
        return "Wave Nightly"
    number = state["publication_number"]
    if type(number) is not int or number < 1:
        raise ValueError("invalid Nightly publication number")
    built = datetime.fromisoformat(state["built_at"])
    if built.tzinfo is None:
        raise ValueError("Nightly build timestamp requires a timezone")
    date = built.astimezone(timezone(timedelta(hours=9))).date()
    return f"Wave {date.isoformat()}-{number:02d}-nightly"


def notes(state):
    source = state["source_sha"]
    lines = [
        title(state),
        "",
        f"Commit: [{source}](https://github.com/{REPO}/commit/{source})",
        "Branch: master",
        f"Built: {state['built_at']}",
        f"Compiler version: {state['compiler_version']}",
        f"Build: https://github.com/{REPO}/actions/runs/{state['run_id']}",
        f"Validated CI: https://github.com/{REPO}/actions/runs/{state['ci_run_id']}",
        "",
        "This release tracks the latest successfully validated master build.",
        "",
        "Download manually below. Nightly is not available through install.sh or install.ps1.",
        "",
    ]
    if state.get("changelog"):
        lines += [state["changelog"], ""]
    for name in sorted(state["assets"]):
        lines.append(
            f"- [{name}](https://github.com/{REPO}/releases/download/nightly/{name})"
        )
    lines += [
        "",
        "<!-- wave-nightly-state:" + json.dumps(state, separators=(",", ":")) + " -->",
    ]
    return "\n".join(lines)


def find_release(github, tag):
    release = github.api(f"releases/tags/{tag}", optional=True)
    if release is not None:
        return release
    # The tag endpoint may omit drafts. Search all pages for a pending upload.
    page = 1
    while True:
        batch = github.api(f"releases?per_page=100&page={page}")
        for release in batch:
            if release["tag_name"] == tag:
                return release
        if len(batch) < 100:
            return None
        page += 1


def promote(github, directory, state):
    release = find_release(github, "nightly")
    previous = active_state(release)
    source = sha(state["source_sha"])
    # A draft from the old publisher can be resumed instead of orphaned.
    legacy_draft = release is not None and release.get("draft")
    if previous and not legacy_draft:
        verify_remote(github, release["id"], previous)
        if source != previous["source_sha"] and not ancestor(
            github, previous["source_sha"], source
        ):
            print("Skipping stale or unrelated Nightly generation")
            return "stale"
        set_tag(github, previous["source_sha"])
        if source == previous["source_sha"]:
            return "unchanged"

    draft = find_release(github, STAGING_TAG)
    if legacy_draft:
        if draft is not None:
            raise ValueError("multiple pending Nightly drafts require reconciliation")
        draft, release, previous = release, None, None
    pending = active_state(draft, draft["tag_name"]) if draft else None
    if draft and not draft.get("draft"):
        raise ValueError("Nightly staging release must be private")
    if pending and source != pending["source_sha"] and not ancestor(
        github, pending["source_sha"], source
    ):
        print("Skipping stale or unrelated pending Nightly generation")
        return "stale"
    if previous:
        state["publication_number"] = previous.get("publication_number", 1) + 1
    else:
        state["publication_number"] = pending.get("publication_number", 1) if pending else 1
    name = title(state)
    # Check local inputs before creating a draft or touching the public release.
    for filename, record in state["assets"].items():
        if file_record(directory / filename) != record:
            raise ValueError(f"Nightly asset changed before upload: {filename}")
    payload = dict(
        tag_name=draft["tag_name"] if draft else STAGING_TAG,
        target_commitish=source,
        name=name,
        body=notes(state),
        draft=True,
        prerelease=True,
        make_latest="false",
    )
    if draft is None:
        draft = github.api("releases", method="POST", data=payload)
    elif pending != state:
        draft = github.api(f"releases/{draft['id']}", method="PATCH", data=payload)

    existing = {a["name"]: a for a in github.assets(draft["id"])}
    for filename, record in state["assets"].items():
        if filename in existing:
            asset = existing[filename]
            if (
                asset.get("digest") != "sha256:" + record["sha256"]
                or asset.get("size") != record["size"]
            ):
                raise ValueError(f"conflicting immutable Nightly asset: {filename}")
        else:
            github.upload(directory / filename, draft["tag_name"])
    verify_remote(github, draft["id"], state)
    # Remove partial uploads from older attempts while the new release is private.
    for filename, asset in existing.items():
        if filename not in state["assets"]:
            github.api(f"releases/assets/{asset['id']}", method="DELETE")
    master = github.api("git/ref/heads/master")["object"]["sha"]
    if not ancestor(github, source, master):
        raise ValueError("source left canonical master history during upload")

    # This is an actual replacement: the old release ID and all of its assets
    # are deleted. The verified draft survives any interruption after deletion.
    if release is not None:
        github.api(f"releases/{release['id']}", method="DELETE")
    try:
        set_tag(github, source)
        github.api(
            f"releases/{draft['id']}",
            method="PATCH",
            data=dict(payload, tag_name="nightly", draft=False),
        )
    except BaseException:
        # A lost response must not cause a second release or consume a number.
        observed = github.api(f"releases/{draft['id']}")
        if observed.get("draft") or active_state(observed) != state:
            raise
    observed = github.api(f"releases/{draft['id']}")
    if active_state(observed) != state or observed.get("draft"):
        raise ValueError("Nightly release promotion could not be verified")
    verify_remote(github, draft["id"], state)
    if github.api("git/ref/tags/nightly")["object"]["sha"] != source:
        raise ValueError("Nightly tag differs from the active generation")
    return "published"


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--stage", choices=("gate", "package", "publish"), required=True
    )
    parser.add_argument("--ci-run-id", default=os.environ.get("WAVE_NIGHTLY_CI_RUN"))
    parser.add_argument("--target", default="linux-amd64")
    parser.add_argument("--provision", action="store_true")
    parser.add_argument("--report-json", type=Path)
    parser.add_argument("--publish", action="store_true")
    parser.add_argument("--directory", type=Path, default=ROOT / "release-assets")
    options = parser.parse_args(argv)
    try:
        if options.publish and options.stage != "publish":
            raise ValueError("--publish is only valid for the publish stage")
        if os.environ.get("GITHUB_REPOSITORY") != REPO:
            raise ValueError("Nightly requires the canonical repository")
        github = GitHub()
        identity = eligible(github, options.ci_run_id)
        if options.stage == "gate":
            installers_ready()
            with open(os.environ["GITHUB_OUTPUT"], "a", encoding="utf-8") as output:
                for key, value in identity.items():
                    output.write(f"{key}={value}\n")
            return 0
        checkout_identity(identity)
        if options.stage == "package":
            from tools.ci.common import main as run_plan

            os.environ["RELEASE_VERSION"] = identity["compiler_version"]
            args = ["--target", options.target]
            if options.provision:
                args += ["--provision"]
            if options.report_json:
                args += ["--report-json", str(options.report_json)]
            return run_plan("package", args)
        if (
            not options.publish
            or os.environ.get("GITHUB_ACTIONS") != "true"
            or os.environ.get("GITHUB_REF") != "refs/heads/master"
        ):
            raise ValueError(
                "Nightly publication requires --publish in the canonical master workflow"
            )
        run_id = os.environ["GITHUB_RUN_ID"]
        own_run = github.api(f"actions/runs/{run_id}")
        if (
            own_run.get("path") != ".github/workflows/nightly.yml"
            or own_run.get("repository", {}).get("full_name") != REPO
            or own_run.get("event") not in ("workflow_run", "workflow_dispatch")
        ):
            raise ValueError("publication must run in the canonical Nightly workflow")
        installers_ready()
        revision = json.loads((ROOT / "std/manifest.json").read_text())[
            "compatibility_revision"
        ]
        with tempfile.TemporaryDirectory(prefix="wave-nightly-") as temporary:
            output = Path(temporary) / "assets"
            state = stage(
                options.directory,
                output,
                identity,
                run_id,
                os.environ["GITHUB_RUN_ATTEMPT"],
                revision,
            )
            from tools.ci.release_notes import generate
            state["changelog"] = generate(github.api, identity["source_sha"])
            print(promote(github, output, state))
        return 0
    except (
        OSError,
        ValueError,
        KeyError,
        RuntimeError,
        subprocess.SubprocessError,
    ) as error:
        print(f"Nightly failed: {error}", file=sys.stderr)
        return 1
