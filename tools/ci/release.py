# SPDX-License-Identifier: MPL-2.0
"""Release identity and publication gates; local builds never implicitly publish."""

import json
from pathlib import Path
import re
import sys
import tomllib

from tools.ci.common import ROOT, main
from tools.check_release_assets import verify, ARCHIVE_TARGETS


def release_identity(r, _):
    version = (
        r.env.get("RELEASE_VERSION")
        or tomllib.loads((ROOT / "Cargo.toml").read_text())["package"]["version"]
    )
    if not re.fullmatch(
        r"\d+\.\d+\.\d+(?:-[0-9A-Za-z.-]+)?", version
    ) or version.endswith("-dev"):
        raise ValueError(f"invalid release version: {version}")
    actual = tomllib.loads((ROOT / "Cargo.toml").read_text())["package"]["version"]
    if actual != version:
        raise ValueError(
            f"Cargo version {actual} does not match release version {version}"
        )
    r.setenv("RELEASE_VERSION", version)
    if (
        r.env.get("GITHUB_ACTIONS") == "true"
        and r.env.get("GITHUB_REF") != "refs/heads/master"
    ):
        raise ValueError("release workflow must run on canonical master")
    remote = r.run(
        [
            "git",
            "ls-remote",
            "--tags",
            "https://github.com/wavefnd/Wave.git",
            f"refs/tags/v{version}",
        ]
    )
    if remote.strip():
        raise ValueError(f"release tag already exists: v{version}")


def verify_metadata(directory, version, source, expected_revision=None):
    directory = Path(directory)
    verify(directory, version)
    expected = {
        f"wave-v{version}-{target}" + (".zip" if "windows" in target else ".tar.gz")
        for target in ARCHIVE_TARGETS
    }
    actual = {p.name for p in directory.glob("*.metadata.json")}
    if actual != {name + ".metadata.json" for name in expected}:
        raise ValueError("incomplete or unexpected artifact metadata")
    revision = None
    for name in expected:
        metadata_path = directory / (name + ".metadata.json")
        try:
            metadata = json.loads(metadata_path.read_text())
        except json.JSONDecodeError as error:
            raise ValueError(f"{metadata_path.name}: invalid JSON metadata: {error.msg}") from error
        if not isinstance(metadata, dict):
            raise ValueError(f"{metadata_path.name}: metadata must be a JSON object")
        field_types = {
            "schema_version": int,
            "compiler_version": str,
            "source_sha": str,
            "archive": str,
            "sha256": str,
            "target": str,
            "std_compatibility_revision": int,
            "abi": str,
            "paths": dict,
            "external_prerequisites": list,
        }
        for field, expected_type in field_types.items():
            if type(metadata.get(field)) is not expected_type:
                raise ValueError(
                    f"{metadata_path.name}: invalid {field}: expected {expected_type.__name__}"
                )
        if any(type(value) is not str for value in metadata["paths"].values()):
            raise ValueError(f"{metadata_path.name}: paths entries must be strings")
        if any(type(value) is not str for value in metadata["external_prerequisites"]):
            raise ValueError(
                f"{metadata_path.name}: external_prerequisites entries must be strings"
            )
        digest = (directory / (name + ".sha256")).read_text().split()[0]
        if (
            metadata.get("schema_version") != 1
            or metadata.get("compiler_version") != version
            or metadata.get("source_sha") != source
        ):
            raise ValueError(f"package version/source identity mismatch: {name}")
        if metadata.get("archive") != name or metadata.get("sha256") != digest:
            raise ValueError(f"package digest identity mismatch: {name}")
        target = (
            metadata.get("target", "")
            .replace("riscv64gc-", "riscv64-")
            .replace("-unknown-linux", "-linux")
        )
        if name != f"wave-v{version}-{target}" + (
            ".zip" if "windows" in target else ".tar.gz"
        ):
            raise ValueError(f"package target identity mismatch: {name}")
        from tools.ci.package import artifact_contract

        for key, value in artifact_contract(target).items():
            if metadata.get(key) != value:
                raise ValueError(f"package {key} contract mismatch: {name}")
        std = metadata.get("std_compatibility_revision")
        if type(std) is not int or std < 0:
            raise ValueError("invalid std compatibility identity")
        if revision is not None and revision != std:
            raise ValueError("packages contain different std compatibility revisions")
        if expected_revision is not None and std != expected_revision:
            raise ValueError("package std revision differs from publication checkout")
        revision = std


def publish(r, _):
    if not getattr(r, "publish_authorized", False):
        raise ValueError("publication requires explicit --publish")
    if r.env.get("GITHUB_REPOSITORY") != "wavefnd/Wave":
        raise ValueError("publication requires the canonical repository")
    source = r.env.get("GITHUB_SHA", "")
    version = r.env.get("RELEASE_VERSION", "")
    if not re.fullmatch("[0-9a-f]{40}", source):
        raise ValueError("missing exact publication source SHA")
    if not re.fullmatch(
        r"\d+\.\d+\.\d+(?:-[0-9A-Za-z.-]+)?", version
    ) or version.endswith("-dev"):
        raise ValueError("invalid publication version")
    directory = ROOT / "release-assets"
    expected_revision = json.loads((ROOT / "std/manifest.json").read_text())[
        "compatibility_revision"
    ]
    verify_metadata(directory, version, source, expected_revision)
    # Refuse a tag created after the earlier release gate as well.
    if r.run(
        [
            "git",
            "ls-remote",
            "--tags",
            "https://github.com/wavefnd/Wave.git",
            f"refs/tags/v{version}",
        ]
    ).strip():
        raise ValueError("release tag already exists")
    from tools.ci.release_notes import generate

    notes_path = r.temp / "release-notes.md"
    notes_path.write_text(
        generate(
            lambda endpoint: json.loads(
                r.run(["gh", "api", "repos/wavefnd/Wave/" + endpoint])
            ),
            source,
            release_tag="v" + version,
        ),
        encoding="utf-8",
    )
    # This is deliberately the final remote read before release creation.
    current = r.run(
        ["gh", "api", "repos/wavefnd/Wave/git/ref/heads/master", "--jq", ".object.sha"]
    ).strip()
    if not re.fullmatch("[0-9a-f]{40}", current) or current != source:
        raise ValueError(
            "canonical master changed or could not be verified; refusing publication"
        )
    assets = sorted(
        str(p)
        for p in directory.iterdir()
        # Checksums and metadata are publication inputs, not download assets.
        if p.name.endswith((".tar.gz", ".zip"))
    )
    args = [
        "gh",
        "release",
        "create",
        "v" + version,
        *assets,
        "--repo",
        "wavefnd/Wave",
        "--target",
        source,
        "--title",
        "Wave v" + version,
        "--notes-file",
        str(notes_path),
    ]
    if r.context.get("inputs.draft", True):
        args += ["--draft"]
    if r.context.get("inputs.prerelease", True):
        args += ["--prerelease"]
    r.run(args)


OPERATIONS = {"release_identity": release_identity, "publish": publish}

if __name__ == "__main__":
    if "--channel" in sys.argv:
        args = sys.argv[1:]
        index = args.index("--channel")
        if args[index : index + 2] != ["--channel", "nightly"]:
            raise SystemExit("only --channel nightly is supported")
        del args[index : index + 2]
        from tools.ci.nightly import main as nightly_main

        sys.exit(nightly_main(args))
    sys.exit(main("release"))
