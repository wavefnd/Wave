# SPDX-License-Identifier: MPL-2.0
"""Nightly tests use an in-memory GitHub; no live release writes."""
import base64
import copy
import hashlib
import io
import json
import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch
from urllib.error import HTTPError, URLError

from tools.check_release_assets import ARCHIVE_TARGETS
from tools.ci import nightly, package, release, targets

A, B, C, OTHER = (c * 40 for c in "abcd")


class FakeGitHub:
    def __init__(self):
        self.releases = {}
        self.release_files = {}
        self.next_id = 1
        self.next_asset_id = 1
        self.tag = None
        self.calls = []
        self.fail = None
        self.upload_failure = False
        self.corrupt = False
        self.run = dict(
            repository={"full_name": nightly.REPO},
            head_repository={"full_name": nightly.REPO},
            event="push",
            head_branch="master",
            path=".github/workflows/ci.yml",
            status="completed",
            conclusion="success",
            head_sha=A,
            run_attempt=1,
        )
        required = nightly.required_names(
            (nightly.ROOT / ".github/workflows/ci.yml").read_text(),
            (nightly.ROOT / "tests/cases/cases.toml").read_text(),
        )
        self.jobs = [
            {"name": name, "status": "completed", "conclusion": "success"}
            for name in sorted(required)
        ]

    def by_tag(self, tag):
        return next((r for r in self.releases.values() if r["tag_name"] == tag), None)

    @property
    def release(self):
        return self.by_tag("nightly") or self.by_tag(nightly.STAGING_TAG)

    @property
    def files(self):
        return self.release_files[self.release["id"]]

    def api(self, endpoint, *, method="GET", data=None, optional=False):
        self.calls.append((method, endpoint, copy.deepcopy(data)))
        fault = self.fail and self.fail[:2] == (method, endpoint)
        if fault and self.fail[2] == "before":
            self.fail = None
            raise RuntimeError("injected API failure")
        if endpoint == "git/ref/heads/master":
            value = {"object": {"sha": C}}
        elif endpoint.startswith("compare/"):
            before, after = endpoint.removeprefix("compare/").split("...")
            ordered = [A, B, C]
            ahead = (
                before in ordered
                and after in ordered
                and ordered.index(before) < ordered.index(after)
            )
            value = {
                "status": "ahead" if ahead else "behind",
                "merge_base_commit": {"sha": before},
            }
        elif "/jobs?" in endpoint:
            value = {"jobs": self.jobs}
        elif endpoint.startswith("actions/runs/"):
            value = self.run
        elif endpoint.startswith(
            ("contents/.github/workflows/ci.yml", "contents/tests/cases/cases.toml")
        ):
            path = endpoint.removeprefix("contents/").split("?")[0]
            value = {
                "content": base64.b64encode((nightly.ROOT / path).read_bytes()).decode()
            }
        elif endpoint.startswith("contents/Cargo.toml"):
            value = {
                "content": base64.b64encode(
                    b'[package]\nversion="0.2.1-pre-beta-dev"'
                ).decode()
            }
        elif endpoint == "git/ref/tags/nightly":
            value = (
                {"object": {"sha": self.tag, "type": "commit"}} if self.tag else None
            )
        elif endpoint in ("git/refs", "git/refs/tags/nightly"):
            self.tag = data["sha"]
            value = {}
        elif endpoint.startswith("releases/tags/"):
            value = self.by_tag(endpoint.removeprefix("releases/tags/"))
            if value and value["draft"]:
                value = None  # Exercise the draft-list fallback.
        elif endpoint.startswith("releases?"):
            page = int(endpoint.split("page=")[-1])
            value = list(self.releases.values())[(page - 1) * 100 : page * 100]
        elif endpoint == "releases" and method == "POST":
            assert self.by_tag(data["tag_name"]) is None
            value = dict(data, id=self.next_id, published_at=None)
            self.releases[self.next_id] = value
            self.release_files[self.next_id] = {}
            self.next_id += 1
        elif endpoint.startswith("releases/assets/") and method == "DELETE":
            asset_id = int(endpoint.split("/")[-1])
            for files in self.release_files.values():
                for name, asset in list(files.items()):
                    if asset["id"] == asset_id:
                        del files[name]
            value = None
        elif endpoint.startswith("releases/"):
            release_id = int(endpoint.split("/")[-1])
            value = self.releases[release_id]
            if method == "PATCH":
                other = self.by_tag(data["tag_name"])
                assert other is None or other["id"] == release_id
                value.update(data)
                if not value["draft"] and value["published_at"] is None:
                    value["published_at"] = f"publication-{release_id}"
            elif method == "DELETE":
                del self.releases[release_id]
                del self.release_files[release_id]
                value = None
            else:
                assert method == "GET"
        else:
            raise AssertionError((method, endpoint))
        if fault:
            mode = self.fail[2]
            self.fail = None
            if mode == "kill":
                raise SystemExit("simulated process death")
            raise RuntimeError("response lost after commit")
        return copy.deepcopy(value)

    def assets(self, release_id):
        return copy.deepcopy(list(self.release_files[release_id].values()))

    def upload(self, path, tag="nightly"):
        self.calls.append(("UPLOAD", path.name, tag))
        if self.upload_failure:
            raise RuntimeError("upload failed")
        record = nightly.file_record(path)
        files = self.release_files[self.by_tag(tag)["id"]]
        files[path.name] = dict(
            id=self.next_asset_id,
            name=path.name,
            size=record["size"],
            state="uploaded",
            digest="sha256:" + ("0" * 64 if self.corrupt else record["sha256"]),
        )

        self.next_asset_id += 1


class NightlyTests(unittest.TestCase):
    def setUp(self):
        sleeper = patch.object(nightly.time, "sleep")
        sleeper.start()
        self.addCleanup(sleeper.stop)
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.github = FakeGitHub()
        self.version = "0.2.1-pre-beta-dev"
        self.counter = 0

    def generation(self, source=A):
        self.counter += 1
        folder = self.root / str(self.counter)
        folder.mkdir()
        for target in ARCHIVE_TARGETS:
            name = f"wave-v{self.version}-{target}" + (
                ".zip" if "windows" in target else ".tar.gz"
            )
            path = folder / name
            path.write_bytes((target + source).encode())
            digest = hashlib.sha256(path.read_bytes()).hexdigest()
            (folder / (name + ".sha256")).write_text(f"{digest}  {name}\n")
            metadata = dict(
                schema_version=1,
                compiler_version=self.version,
                source_sha=source,
                std_compatibility_revision=4,
                target=target,
                archive=name,
                sha256=digest,
                **package.artifact_contract(target),
            )
            (folder / (name + ".metadata.json")).write_text(json.dumps(metadata))
        output = folder / "staged"
        state = nightly.stage(
            folder,
            output,
            dict(source_sha=source, compiler_version=self.version, ci_run_id=10),
            20 + self.counter,
            1,
            4,
        )
        return output, state

    def seed(self):
        directory, state = self.generation()
        self.assertEqual(nightly.promote(self.github, directory, state), "published")
        self.github.calls.clear()
        return state

    def test_eligibility_checks_repository_event_branch_run_and_ancestry(self):
        self.assertEqual(nightly.eligible(self.github, 10)["source_sha"], A)
        for key, value in [
            ("repository", {"full_name": "fork/Wave"}),
            ("head_repository", {"full_name": "fork/Wave"}),
            ("event", "pull_request"),
            ("head_branch", "feature"),
            ("path", ".github/workflows/other.yml"),
            ("status", "in_progress"),
            ("conclusion", "failure"),
            ("conclusion", "cancelled"),
            ("head_sha", OTHER),
        ]:
            with self.subTest(key=key, value=value):
                original = self.github.run[key]
                self.github.run[key] = value
                with self.assertRaises(ValueError):
                    nightly.eligible(self.github, 10)
                self.github.run[key] = original
        for jobs in [
            [],
            [{"status": "completed", "conclusion": "skipped"}],
            [{"status": "completed", "conclusion": "failure"}],
        ]:
            self.github.jobs = jobs
            with self.assertRaises(ValueError):
                nightly.eligible(self.github, 10)
        for value in [None, "../10", "0", "-1"]:
            with self.assertRaises(ValueError):
                nightly.eligible(self.github, value)

    def test_all_nine_packages_have_generation_specific_matching_sidecars(self):
        directory, state = self.generation()
        archives = list(directory.glob("*.zip")) + list(directory.glob("*.tar.gz"))
        self.assertEqual(len(archives), 9)
        self.assertEqual(len(state["assets"]), 29)
        for path in archives:
            self.assertIn(A, path.name)
            metadata = json.loads(
                (directory / (path.name + ".metadata.json")).read_text()
            )
            self.assertEqual(metadata["archive"], path.name)
            self.assertEqual(metadata["compiler_version"], self.version)
            self.assertEqual(metadata["source_sha"], A)
            self.assertIn(path.name, (directory / (path.name + ".sha256")).read_text())
        self.assertEqual(
            {t.archive_target for t in targets.TARGETS.values() if t.distribution},
            set(ARCHIVE_TARGETS),
        )

    def test_bad_package_sets_fail_before_any_remote_write(self):
        for bad in ("missing", "checksum", "metadata", "revision"):
            with self.subTest(bad=bad):
                directory, _ = self.generation()
                source = directory.parent
                archive = next(source.glob("*.tar.gz"))
                if bad == "missing":
                    archive.unlink()
                elif bad == "checksum":
                    archive.write_bytes(b"changed")
                elif bad == "metadata":
                    (source / (archive.name + ".metadata.json")).write_text("{}")
                with self.assertRaises(ValueError):
                    nightly.stage(
                        source,
                        source / "bad",
                        dict(source_sha=A, compiler_version=self.version),
                        1,
                        1,
                        999 if bad == "revision" else 4,
                    )
        self.assertEqual(self.github.calls, [])

    def test_first_publication_and_replacement_preserve_previous_until_commit(self):
        previous = self.seed()
        self.assertEqual(previous["publication_number"], 1)
        old = copy.deepcopy(self.github.release)
        directory, state = self.generation(B)
        self.assertEqual(nightly.promote(self.github, directory, state), "published")
        self.assertNotEqual(self.github.release["id"], old["id"])
        self.assertNotEqual(self.github.release["published_at"], old["published_at"])
        self.assertEqual(len(self.github.releases), 1)
        self.assertEqual(state["publication_number"], 2)
        self.assertEqual(self.github.release["name"], nightly.title(state))
        self.assertTrue(self.github.release["body"].startswith(nightly.title(state)))
        self.assertEqual(self.github.tag, B)
        self.assertEqual(nightly.active_state(self.github.release), state)
        self.assertFalse(self.github.release["draft"])
        self.assertTrue(self.github.release["prerelease"])
        self.assertEqual(self.github.release["make_latest"], "false")
        calls = self.github.calls
        commit = next(
            i for i, c in enumerate(calls) if c[:2] == ("PATCH", "releases/2")
        )
        deletion = next(i for i, c in enumerate(calls) if c[:2] == ("DELETE", "releases/1"))
        self.assertLess(deletion, commit)
        self.assertTrue(all(i < deletion for i, c in enumerate(calls) if c[0] == "UPLOAD"))
        self.assertTrue(all(c[2] == nightly.STAGING_TAG for c in calls if c[0] == "UPLOAD"))
        self.assertTrue(set(previous["assets"]).isdisjoint(self.github.files))
        self.assertEqual(set(state["assets"]), set(self.github.files))

    def test_title_uses_korean_date_and_counter_continues_across_dates(self):
        directory, first = self.generation(A)
        first["built_at"] = "2026-10-03T14:59:59+00:00"
        nightly.promote(self.github, directory, first)
        self.assertEqual(self.github.release["name"], "Wave 2026-10-03-01-nightly")
        directory, second = self.generation(B)
        second["built_at"] = "2026-10-03T15:00:00+00:00"
        nightly.promote(self.github, directory, second)
        self.assertEqual(self.github.release["name"], "Wave 2026-10-04-02-nightly")
        self.assertEqual(self.github.release["id"], 2)
        self.assertEqual(self.github.release["tag_name"], "nightly")
        self.assertEqual(sum(c[:2] == ("POST", "releases") for c in self.github.calls), 2)
        second["publication_number"] = 100
        self.assertEqual(nightly.title(second), "Wave 2026-10-04-100-nightly")

    def test_existing_unnumbered_release_migrates_without_resetting_history(self):
        previous = self.seed()
        del previous["publication_number"]
        self.github.release.update(name="Wave Nightly", body=nightly.notes(previous))
        self.assertEqual(nightly.active_state(self.github.release), previous)
        directory, state = self.generation(B)
        nightly.promote(self.github, directory, state)
        self.assertEqual(state["publication_number"], 2)
        self.assertEqual(self.github.release["name"], nightly.title(state))

    def test_invalid_number_or_title_cannot_replace_managed_release(self):
        state = self.seed()
        for number in (0, -1, True, "2", None):
            with self.subTest(number=number), self.assertRaises(ValueError):
                nightly.title(dict(state, publication_number=number))
        self.github.release["name"] = "Wave 2026-10-03-999-nightly"
        directory, next_state = self.generation(B)
        with self.assertRaisesRegex(ValueError, "title differs"):
            nightly.promote(self.github, directory, next_state)
        self.assertEqual(self.github.tag, A)

    def test_failed_or_corrupt_upload_preserves_current_generation_and_tag(self):
        for mode in ("upload_failure", "corrupt"):
            with self.subTest(mode=mode):
                self.github = FakeGitHub()
                previous = self.seed()
                directory, state = self.generation(B)
                setattr(self.github, mode, True)
                with self.assertRaises((ValueError, RuntimeError)):
                    nightly.promote(self.github, directory, state)
                self.assertEqual(nightly.active_state(self.github.release), previous)
                self.assertEqual(self.github.tag, A)
                self.assertTrue(set(previous["assets"]).issubset(self.github.files))
                self.assertFalse(any(c[0] == "DELETE" for c in self.github.calls))

    def test_out_of_order_runs_and_duplicate_publication(self):
        self.seed()
        directory, state = self.generation(C)
        nightly.promote(self.github, directory, state)
        before = copy.deepcopy(self.github.release)
        for source, expected in [(B, "stale"), (OTHER, "stale"), (C, "unchanged")]:
            directory, state = self.generation(source)
            self.assertEqual(nightly.promote(self.github, directory, state), expected)
            self.assertEqual(self.github.tag, C)
            self.assertEqual(self.github.release, before)

    def test_publication_failure_after_deletion_resumes_draft_with_same_number(self):
        self.seed()
        directory, state = self.generation(B)
        self.github.fail = ("PATCH", "releases/2", "before")
        with self.assertRaises(RuntimeError):
            nightly.promote(self.github, directory, state)
        self.assertIsNone(self.github.by_tag("nightly"))
        self.assertTrue(self.github.release["draft"])
        self.assertEqual(state["publication_number"], 2)
        self.assertEqual(nightly.promote(self.github, directory, state), "published")
        self.assertEqual(state["publication_number"], 2)
        self.assertEqual(self.github.release["id"], 2)
        self.assertEqual(len(self.github.releases), 1)

    def test_lost_promotion_response_is_read_back_without_another_release(self):
        self.seed()
        directory, state = self.generation(B)
        self.github.fail = ("PATCH", "releases/2", "after")
        self.assertEqual(nightly.promote(self.github, directory, state), "published")
        self.assertEqual(self.github.tag, B)
        self.assertEqual(self.github.release["id"], 2)
        self.assertEqual(len(self.github.releases), 1)

    def test_retry_recovers_after_process_death_following_deletion(self):
        self.seed()
        directory, state = self.generation(B)
        self.github.fail = ("DELETE", "releases/1", "kill")
        with self.assertRaises(SystemExit):
            nightly.promote(self.github, directory, state)
        self.assertIsNone(self.github.by_tag("nightly"))
        self.assertTrue(self.github.release["draft"])
        self.assertEqual(nightly.promote(self.github, directory, state), "published")
        self.assertEqual(state["publication_number"], 2)
        self.assertEqual(self.github.tag, B)

    def test_creation_or_deletion_failure_preserves_old_release_and_can_retry(self):
        for fault in [("POST", "releases", "before"), ("POST", "releases", "after"),
                      ("DELETE", "releases/1", "before")]:
            with self.subTest(fault=fault):
                self.github = FakeGitHub()
                previous = self.seed()
                directory, state = self.generation(B)
                self.github.fail = fault
                with self.assertRaises(RuntimeError):
                    nightly.promote(self.github, directory, state)
                self.assertEqual(nightly.active_state(self.github.release), previous)
                self.assertEqual(self.github.tag, A)
                self.assertEqual(nightly.promote(self.github, directory, state), "published")
                self.assertEqual(state["publication_number"], 2)
                self.assertEqual(len(self.github.releases), 1)

    def test_pending_replacement_rejects_stale_source_and_accepts_newer_source(self):
        self.seed()
        directory, state = self.generation(B)
        self.github.fail = ("DELETE", "releases/1", "kill")
        with self.assertRaises(SystemExit):
            nightly.promote(self.github, directory, state)
        stale_dir, stale = self.generation(A)
        self.assertEqual(nightly.promote(self.github, stale_dir, stale), "stale")
        newer_dir, newer = self.generation(C)
        self.assertEqual(nightly.promote(self.github, newer_dir, newer), "published")
        self.assertEqual(newer["publication_number"], 2)
        self.assertEqual(self.github.tag, C)
        self.assertEqual(set(self.github.files), set(newer["assets"]))

    def test_interrupted_first_upload_keeps_draft_and_can_retry(self):
        directory, state = self.generation()
        self.github.upload_failure = True
        with self.assertRaises(RuntimeError):
            nightly.promote(self.github, directory, state)
        self.assertTrue(self.github.release["draft"])
        self.github.upload_failure = False
        self.assertEqual(nightly.promote(self.github, directory, state), "published")

    def test_unmanaged_release_and_conflicting_assets_are_not_overwritten(self):
        self.seed()
        self.github.release["body"] = "manually owned release"
        directory, state = self.generation(B)
        with self.assertRaises(ValueError):
            nightly.promote(self.github, directory, state)
        self.assertEqual(self.github.release["body"], "manually owned release")

    def pending_upload(self):
        directory, state = self.generation(B)
        self.github.upload_failure = True
        with self.assertRaises(RuntimeError):
            nightly.promote(self.github, directory, state)
        self.github.upload_failure = False
        name = next(iter(state["assets"]))
        self.github.upload(directory / name, nightly.STAGING_TAG)
        draft = self.github.by_tag(nightly.STAGING_TAG)
        return directory, state, self.github.release_files[draft["id"]][name]

    def test_pending_cleanup_failure_preserves_old_release_and_can_retry(self):
        previous = self.seed()
        _, _, old_asset = self.pending_upload()
        directory, state = self.generation(C)
        self.github.fail = ("DELETE", f"releases/assets/{old_asset['id']}", "before")
        with self.assertRaises(RuntimeError):
            nightly.promote(self.github, directory, state)
        self.assertEqual(nightly.active_state(self.github.release), previous)
        self.assertEqual(self.github.tag, A)
        self.assertEqual(nightly.promote(self.github, directory, state), "published")
        self.assertEqual(state["publication_number"], 2)
        self.assertEqual(set(self.github.files), set(state["assets"]))

    def test_conflicting_immutable_upload_is_never_clobbered(self):
        previous = self.seed()
        directory, state, asset = self.pending_upload()
        asset["digest"] = "sha256:" + "0" * 64
        with self.assertRaisesRegex(ValueError, "conflicting immutable"):
            nightly.promote(self.github, directory, state)
        self.assertEqual(nightly.active_state(self.github.release), previous)
        self.assertEqual(self.github.tag, A)
        self.assertTrue(set(previous["assets"]).issubset(self.github.files))

    def test_changed_local_asset_fails_before_creating_replacement(self):
        previous = self.seed()
        directory, state = self.generation(B)
        (directory / next(iter(state["assets"]))).write_bytes(b"changed")
        with self.assertRaisesRegex(ValueError, "changed before upload"):
            nightly.promote(self.github, directory, state)
        self.assertEqual(nightly.active_state(self.github.release), previous)
        self.assertIsNone(self.github.by_tag(nightly.STAGING_TAG))
        self.assertFalse(any(c[0] != "GET" for c in self.github.calls))

    def test_legacy_empty_draft_is_resumed(self):
        self.github.api("releases", method="POST", data=dict(
            tag_name="nightly", name="Wave Nightly", body="",
            draft=True, prerelease=True,
        ))
        directory, state = self.generation(A)
        self.assertEqual(nightly.promote(self.github, directory, state), "published")
        self.assertEqual(state["publication_number"], 1)
        self.assertEqual(len(self.github.releases), 1)
        self.assertEqual(self.github.release["id"], 1)

    def test_upload_uses_pending_tag_without_clobber(self):
        path = self.root / "archive.tar.gz"
        with patch.object(nightly.subprocess, "run") as run:
            nightly.GitHub().upload(path, nightly.STAGING_TAG)
        run.assert_called_once_with(
            ["gh", "release", "upload", nightly.STAGING_TAG, str(path),
             "--repo", nightly.REPO],
            check=True, timeout=1800,
        )

    def test_publication_authorization_precedes_any_release_write(self):
        identity = dict(source_sha=A, compiler_version=self.version, ci_run_id=10)
        base = dict(
            GITHUB_REPOSITORY=nightly.REPO,
            GITHUB_ACTIONS="true",
            GITHUB_REF="refs/heads/master",
            GITHUB_RUN_ID="30",
            GITHUB_RUN_ATTEMPT="1",
        )
        for changes, flag in [
            ({}, False),
            ({"GITHUB_REF": "refs/heads/topic"}, True),
            ({"GITHUB_ACTIONS": "false"}, True),
        ]:
            with self.subTest(changes=changes, flag=flag), patch.dict(
                os.environ, dict(base, **changes)
            ), patch.object(nightly, "eligible", return_value=identity), patch.object(
                nightly, "checkout_identity"
            ), patch.object(
                nightly, "promote"
            ) as promote:
                args = ["--stage", "publish"] + (["--publish"] if flag else [])
                self.assertEqual(nightly.main(args), 1)
                promote.assert_not_called()

    def test_gate_exports_the_validated_sha_and_version(self):
        identity = dict(source_sha=A, compiler_version=self.version, ci_run_id=10)
        output = self.root / "outputs"
        with patch.dict(
            os.environ,
            {"GITHUB_REPOSITORY": nightly.REPO, "GITHUB_OUTPUT": str(output)},
        ), patch.object(nightly, "eligible", return_value=identity), patch.object(
            nightly, "installers_ready"
        ), patch.object(
            nightly, "promote"
        ) as promote:
            self.assertEqual(nightly.main(["--stage", "gate", "--ci-run-id", "10"]), 0)
            self.assertIn("source_sha=" + A, output.read_text())
            self.assertIn("compiler_version=" + self.version, output.read_text())
            promote.assert_not_called()

    def test_package_uses_the_existing_target_package_plan(self):
        identity = dict(source_sha=A, compiler_version=self.version, ci_run_id=10)
        with patch.dict(os.environ, {"GITHUB_REPOSITORY": nightly.REPO}), patch.object(
            nightly, "eligible", return_value=identity
        ), patch.object(nightly, "checkout_identity"), patch(
            "tools.ci.common.main", return_value=0
        ) as run:
            self.assertEqual(
                nightly.main(
                    ["--stage", "package", "--target", "linux-riscv64", "--provision"]
                ),
                0,
            )
            run.assert_called_once_with(
                "package", ["--target", "linux-riscv64", "--provision"]
            )
            self.assertEqual(os.environ["RELEASE_VERSION"], self.version)

    def test_installers_must_match_reviewed_deployed_bytes(self):
        data = b"fixture"
        pins = self.root / "pins.json"
        pins.write_text(
            json.dumps(
                {
                    url: hashlib.sha256(data).hexdigest()
                    for url in [
                        "https://wave-lang.dev/install.sh",
                        "https://wave-lang.dev/install.ps1",
                    ]
                }
            )
        )
        requests = []

        def deployed_installer(request, *, timeout):
            # Model the CDN's rejection of Python's default User-Agent.
            if request.get_header("User-agent") != "Wave-Nightly/1.0":
                raise HTTPError(request.full_url, 403, "Forbidden", {}, None)
            self.assertEqual(timeout, 30)
            requests.append(request.full_url)
            return io.BytesIO(data)

        with patch.object(nightly, "INSTALLERS_FILE", pins), patch.object(
            nightly, "urlopen", side_effect=deployed_installer
        ):
            nightly.installers_ready()
        self.assertEqual(set(requests), set(json.loads(pins.read_text())))
        with patch.object(nightly, "INSTALLERS_FILE", pins), patch.object(
            nightly, "urlopen", return_value=io.BytesIO(b"old")
        ):
            with self.assertRaises(ValueError):
                nightly.installers_ready()

    def test_gate_reports_installer_url_and_exports_nothing_on_network_failure(self):
        identity = dict(source_sha=A, compiler_version=self.version, ci_run_id=10)
        urls = list(json.loads(nightly.INSTALLERS_FILE.read_text()))
        pins = self.root / "pins.json"
        pins.write_text(json.dumps({
            url: hashlib.sha256(b"fixture").hexdigest() for url in urls
        }))
        for index, url in enumerate(urls):
            for error in (
                HTTPError(url, 403, "Forbidden", {}, None),
                URLError("connection failed"),
                TimeoutError("timed out"),
            ):
                if isinstance(error, HTTPError):
                    self.addCleanup(error.close)
                with self.subTest(url=url, error=type(error).__name__):
                    output = self.root / "failed-outputs"
                    stderr = io.StringIO()
                    responses = [io.BytesIO(b"fixture") for _ in range(index)] + [error]
                    with patch.dict(
                        os.environ,
                        {
                            "GITHUB_REPOSITORY": nightly.REPO,
                            "GITHUB_OUTPUT": str(output),
                        },
                    ), patch.object(
                        nightly, "eligible", return_value=identity
                    ), patch.object(
                        nightly, "urlopen", side_effect=responses
                    ), patch.object(nightly, "INSTALLERS_FILE", pins), patch(
                        "sys.stderr", stderr
                    ):
                        self.assertEqual(nightly.main(["--stage", "gate"]), 1)
                    self.assertIn(
                        f"cannot verify deployed installer {url}", stderr.getvalue()
                    )
                    self.assertIn(str(error), stderr.getvalue())
                    self.assertFalse(output.exists())

    def test_gate_blocks_each_unapproved_installer_without_exporting_identity(self):
        identity = dict(source_sha=A, compiler_version=self.version, ci_run_id=10)
        approved = b"reviewed pinned installer"
        urls = list(json.loads(nightly.INSTALLERS_FILE.read_text()))
        expected = hashlib.sha256(approved).hexdigest()
        pins = self.root / "pins.json"
        pins.write_text(json.dumps({url: expected for url in urls}))
        for index, url in enumerate(urls):
            for unapproved in (b"previous latest-only installer", b"changed pinned installer"):
                with self.subTest(url=url, data=unapproved):
                    output = self.root / "mismatched-outputs"
                    stderr = io.StringIO()
                    responses = [io.BytesIO(approved) for _ in range(index)]
                    responses.append(io.BytesIO(unapproved))
                    with patch.dict(
                        os.environ,
                        {
                            "GITHUB_REPOSITORY": nightly.REPO,
                            "GITHUB_OUTPUT": str(output),
                        },
                    ), patch.object(
                        nightly, "eligible", return_value=identity
                    ), patch.object(
                        nightly, "urlopen", side_effect=responses
                    ), patch.object(nightly, "INSTALLERS_FILE", pins), patch(
                        "sys.stderr", stderr
                    ):
                        self.assertEqual(nightly.main(["--stage", "gate"]), 1)
                    self.assertIn(
                        f"reviewed installer is not deployed: {url}", stderr.getvalue()
                    )
                    self.assertIn(f"expected SHA-256 {expected}", stderr.getvalue())
                    self.assertIn(hashlib.sha256(unapproved).hexdigest(), stderr.getvalue())
                    self.assertFalse(output.exists())

    def test_publication_is_opt_in_and_versioned_rules_remain_strict(self):
        with patch.dict(os.environ, {"GITHUB_REPOSITORY": "fork/Wave"}), patch.object(
            nightly, "GitHub"
        ) as github:
            self.assertEqual(nightly.main(["--stage", "publish"]), 1)
            github.assert_not_called()


class EligibilityPropagationTests(unittest.TestCase):
    def exercise(
        self, *, settle_at=None, terminal=None, change_identity=False, missing=False
    ):
        github = FakeGitHub()
        original = github.api
        clock = [0]
        snapshots = [0]

        def api(endpoint, **kwargs):
            value = original(endpoint, **kwargs)
            if "/jobs?" in endpoint:
                snapshots[0] += 1
                if change_identity:
                    github.run["run_attempt"] += 1
                jobs = copy.deepcopy(value["jobs"])
                if missing:
                    jobs.pop()
                elif terminal:
                    jobs[0]["conclusion"] = terminal
                elif settle_at is None or clock[0] < settle_at:
                    jobs[0].update(status="in_progress", conclusion=None)
                return {"jobs": jobs, "total_count": len(github.jobs)}
            return value

        def sleep(seconds):
            clock[0] += seconds

        with (
            patch.object(nightly.time, "monotonic", side_effect=lambda: clock[0]),
            patch.object(nightly.time, "sleep", side_effect=sleep),
        ):
            github.api = api
            if settle_at is not None and not (terminal or change_identity or missing):
                self.assertEqual(nightly.eligible(github, 10)["source_sha"], A)
            else:
                with self.assertRaises(ValueError):
                    nightly.eligible(github, 10)
        return clock[0], snapshots[0]

    def test_run_state_propagates_without_accepting_pending_validation(self):
        github = FakeGitHub()
        github.run.update(status="in_progress", conclusion=None)
        original = github.api
        clock = [0]

        def api(endpoint, **kwargs):
            if endpoint == "actions/runs/10" and clock[0] >= 20:
                github.run.update(status="completed", conclusion="success")
            return original(endpoint, **kwargs)

        def sleep(seconds):
            clock[0] += seconds

        with (
            patch.object(nightly.time, "monotonic", side_effect=lambda: clock[0]),
            patch.object(nightly.time, "sleep", side_effect=sleep),
        ):
            github.api = api
            self.assertEqual(nightly.eligible(github, 10)["source_sha"], A)
        self.assertEqual(clock[0], 20)

    def test_exact_schedule_and_last_allowed_snapshot(self):
        self.assertEqual(self.exercise(settle_at=60), (60, 7))
        self.assertEqual(self.exercise(), (60, 7))
        self.assertEqual(self.exercise(missing=True), (60, 7))

    def test_terminal_failures_never_wait_or_pass(self):
        for state in ("failure", "cancelled", "skipped", "timed_out"):
            self.assertEqual(self.exercise(terminal=state), (0, 1))

    def test_changed_attempt_is_rejected(self):
        self.assertEqual(self.exercise(change_identity=True), (10, 1))


class WorkflowLineEndingTests(unittest.TestCase):
    def setUp(self):
        self.workflow = (nightly.ROOT / ".github/workflows/ci.yml").read_text(
            encoding="utf-8"
        )
        self.manifest = (nightly.ROOT / "tests/cases/cases.toml").read_text(
            encoding="utf-8"
        )

    def test_required_names_preserve_all_jobs_across_line_endings(self):
        expected = nightly.required_names(self.workflow, self.manifest)
        self.assertEqual(len(expected), 50)
        crlf = self.workflow.replace("\n", "\r\n")
        mixed = "".join(
            line + ("\r\n" if index % 2 else "\n")
            for index, line in enumerate(self.workflow.splitlines())
        )
        for workflow in (crlf, mixed, crlf.rstrip("\r\n")):
            with self.subTest(ending=repr(workflow[-2:])):
                self.assertEqual(
                    nightly.required_names(workflow, self.manifest), expected
                )

    def test_crlf_api_content_keeps_strict_eligibility(self):
        github = FakeGitHub()
        original = github.api

        def api(endpoint, **kwargs):
            value = original(endpoint, **kwargs)
            if endpoint.startswith("contents/.github/workflows/ci.yml?"):
                value = {
                    "content": base64.b64encode(
                        self.workflow.replace("\n", "\r\n").encode("utf-8")
                    ).decode("ascii")
                }
            return value

        github.api = api
        self.assertEqual(nightly.eligible(github, 10)["source_sha"], A)
        github.jobs[0]["conclusion"] = "skipped"
        with self.assertRaisesRegex(ValueError, "skipped jobs are not validation"):
            nightly.eligible(github, 10)

    def test_missing_jobs_contract_has_a_controlled_error(self):
        for workflow in ("name: CI\n", "name: CI\r\n"):
            with self.assertRaisesRegex(ValueError, "jobs contract"):
                nightly.required_names(workflow, self.manifest)


if __name__ == "__main__":
    unittest.main()
