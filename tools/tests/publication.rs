// SPDX-License-Identifier: MPL-2.0
//! Stateful external API fixtures exercise policy without any live mutations.
use base64::Engine;
use std::collections::BTreeMap;
use std::path::Path;
use std::time::Duration;
use utils::wson::Value;
use wave_tools::{
    cases, config, error, package,
    publication::{self, Api, Identity},
    storage, Result,
};

fn source(letter: char) -> String {
    letter.to_string().repeat(40)
}

fn obj(fields: impl IntoIterator<Item = (impl Into<String>, Value)>) -> Value {
    Value::object(fields)
}
struct Remote {
    run: Value,
    jobs: Vec<Value>,
    releases: BTreeMap<u64, Value>,
    assets: BTreeMap<u64, Vec<Value>>,
    tag: Option<String>,
    calls: Vec<String>,
    next: u64,
    next_asset: u64,
    fault: Option<(String, String, bool)>,
    fault_skip: usize,
    upload_failure: bool,
    upload_error_after: Option<(usize, bool)>,
    corrupt: bool,
    lag: usize,
    change_attempt: bool,
}
impl Remote {
    fn new() -> Self {
        let run = obj([
            ("repository", obj([("full_name", Value::string(publication::REPO))])),
            ("head_repository", obj([("full_name", Value::string(publication::REPO))])),
            ("event", Value::string("push")),
            ("head_branch", Value::string("master")),
            ("path", Value::string(".github/workflows/ci.yml")),
            ("status", Value::string("completed")),
            ("conclusion", Value::string("success")),
            ("head_sha", Value::string(source('a'))),
            ("run_attempt", Value::integer(1)),
        ]);
        let root = wave_tools::root();
        let names = publication::required_names(
            &std::fs::read_to_string(root.join(".github/workflows/ci.yml")).unwrap(),
            &std::fs::read_to_string(root.join("tests/cases/cases.toml")).unwrap(),
        )
        .unwrap();
        let jobs = names
            .into_iter()
            .map(|n| {
                obj([
                    ("name", Value::string(n)),
                    ("status", Value::string("completed")),
                    ("conclusion", Value::string("success")),
                ])
            })
            .collect();
        Self {
            run,
            jobs,
            releases: BTreeMap::new(),
            assets: BTreeMap::new(),
            tag: None,
            calls: vec![],
            next: 1,
            next_asset: 1000,
            fault: None,
            fault_skip: 0,
            upload_failure: false,
            upload_error_after: None,
            corrupt: false,
            lag: 0,
            change_attempt: false,
        }
    }
}
impl Api for Remote {
    fn request(
        &mut self,
        method: &str,
        path: &str,
        data: Option<&Value>,
        _optional: bool,
    ) -> Result<Option<Value>> {
        self.calls.push(format!("{method} {path}"));
        let mut fault = self.fault.as_ref().is_some_and(|(m, p, _)| m == method && p == path);
        if fault && self.fault_skip > 0 {
            self.fault_skip -= 1;
            fault = false;
        }
        if fault && !self.fault.as_ref().unwrap().2 {
            self.fault = None;
            return Err(error("injected API failure"));
        }
        let value = if path == "git/ref/heads/master" {
            Some(obj([("object", obj([("sha", Value::string(source('c')))]))]))
        } else if let Some(comparison) = path.strip_prefix("compare/") {
            let (before, after) = comparison.split_once("...").unwrap();
            let ahead = [source('a'), source('b'), source('c')]
                .iter()
                .position(|v| v == before)
                .zip([source('a'), source('b'), source('c')].iter().position(|v| v == after))
                .is_some_and(|(a, b)| a < b);
            Some(obj([
                ("status", Value::string(if ahead { "ahead" } else { "behind" })),
                ("merge_base_commit", obj([("sha", Value::string(before))])),
            ]))
        } else if path.contains("/jobs?") {
            let mut jobs = self.jobs.clone();
            if self.lag > 0 {
                self.lag -= 1;
                config::field(&mut jobs[0], "status", Value::string("in_progress"))?;
                config::field(&mut jobs[0], "conclusion", Value::Null)?;
            }
            Some(obj([
                ("jobs", Value::Array(jobs)),
                ("total_count", Value::integer(self.jobs.len() as u64)),
            ]))
        } else if path.starts_with("actions/runs/") {
            let run = self.run.clone();
            if self.change_attempt {
                config::field(&mut self.run, "run_attempt", Value::integer(2))?;
            }
            Some(run)
        } else if path.starts_with("contents/") {
            let path = path.strip_prefix("contents/").unwrap().split('?').next().unwrap();
            let bytes = if path == "Cargo.toml" {
                b"[package]\nversion=\"0.0.1-alpha-dev\"\n".to_vec()
            } else {
                std::fs::read(wave_tools::root().join(path))?
            };
            Some(obj([(
                "content",
                Value::string(base64::engine::general_purpose::STANDARD.encode(bytes)),
            )]))
        } else if path == "git/ref/tags/nightly" {
            self.tag.as_ref().map(|tag| {
                obj([(
                    "object",
                    obj([("sha", Value::string(tag)), ("type", Value::string("commit"))]),
                )])
            })
        } else if path == "git/refs" || path == "git/refs/tags/nightly" {
            self.tag = Some(config::text(data.unwrap(), "sha")?.into());
            Some(obj(Vec::<(String, Value)>::new()))
        } else if let Some(tag) = path.strip_prefix("releases/tags/") {
            self.releases
                .values()
                .find(|r| {
                    r.get_str("tag_name") == Some(tag) && r.get("draft") != Some(&Value::Bool(true))
                })
                .cloned()
        } else if path.starts_with("releases?") {
            Some(Value::Array(self.releases.values().cloned().collect()))
        } else if path == "releases" && method == "POST" {
            let id = self.next;
            self.next += 1;
            let mut value = data.unwrap().clone();
            config::field(&mut value, "id", Value::integer(id))?;
            self.releases.insert(id, value.clone());
            self.assets.insert(id, vec![]);
            Some(value)
        } else if let Some(id) = path.strip_prefix("releases/assets/") {
            let id = id.parse::<u64>()?;
            for assets in self.assets.values_mut() {
                assets.retain(|a| a.get_u64("id") != Some(id));
            }
            None
        } else if let Some(rest) = path.strip_prefix("releases/") {
            if let Some(id) = rest.split('/').next() {
                let id = id.parse::<u64>()?;
                if rest.contains("/assets?") {
                    Some(Value::Array(self.assets.get(&id).unwrap().clone()))
                } else if method == "DELETE" {
                    self.releases.remove(&id);
                    self.assets.remove(&id);
                    None
                } else if method == "PATCH" {
                    let mut value = self.releases[&id].clone();
                    let Value::Object(fields) = data.unwrap() else { panic!() };
                    for (key, item) in fields {
                        config::field(&mut value, key, item.clone())?;
                    }
                    self.releases.insert(id, value.clone());
                    Some(value)
                } else {
                    Some(self.releases[&id].clone())
                }
            } else {
                return Err(error("invalid fake release ID"));
            }
        } else {
            return Err(error(format!("unexpected fake request: {method} {path}")));
        };
        if fault {
            self.fault = None;
            return Err(error("response lost after commit"));
        }
        Ok(value)
    }

    fn upload(&mut self, path: &Path, tag: &str) -> Result<()> {
        if self.upload_failure {
            return Err(error("upload failed"));
        }
        let fault = match self.upload_error_after.as_mut() {
            Some((remaining, after)) if *remaining == 0 => {
                let after = *after;
                self.upload_error_after = None;
                Some(after)
            },
            Some((remaining, _)) => {
                *remaining -= 1;
                None
            },
            None => None,
        };
        if fault == Some(false) {
            return Err(error("injected partial upload failure"));
        }
        let id = self.releases.iter().find(|(_, r)| r.get_str("tag_name") == Some(tag)).unwrap().0;
        let name = path.file_name().unwrap().to_string_lossy();
        self.calls.push(format!("UPLOAD {name}"));
        let digest = if self.corrupt { "0".repeat(64) } else { storage::digest(path)? };
        let asset_id = self.next_asset;
        self.next_asset += 1;
        let assets = self.assets.get_mut(id).unwrap();
        assets.push(obj([
            ("id", Value::integer(asset_id)),
            ("name", Value::string(name)),
            ("state", Value::string("uploaded")),
            ("size", Value::integer(std::fs::metadata(path)?.len())),
            ("digest", Value::string(format!("sha256:{digest}"))),
        ]));
        if fault == Some(true) {
            return Err(error("lost successful upload response"));
        }
        Ok(())
    }
}

fn generation(root: &Path, letter: char, count: u64) -> (std::path::PathBuf, Value) {
    let input = root.join(count.to_string());
    std::fs::create_dir(&input).unwrap();
    let version = "0.0.1-alpha-dev";
    let source = source(letter);
    let manifest = cases::Manifest::load(&wave_tools::root(), false).unwrap();
    for target in manifest.targets.iter().filter(|t| {
        t.enabled
            && (t.os == "linux"
                || t.os == "macos"
                || t.os == "windows"
                || t.os == "freebsd" && t.arch == "amd64")
    }) {
        let triple = target.archive_target().unwrap();
        let name = format!(
            "wave-v{version}-{triple}{}",
            if target.os == "windows" { ".zip" } else { ".tar.gz" }
        );
        let path = input.join(&name);
        std::fs::write(&path, format!("{triple}{source}")).unwrap();
        let digest = storage::digest(&path).unwrap();
        std::fs::write(input.join(format!("{name}.sha256")), format!("{digest}  {name}\n"))
            .unwrap();
        let mut metadata = package::contract(&triple);
        for (key, value) in [
            ("schema_version", Value::integer(1)),
            ("compiler_version", Value::string(version)),
            ("source_sha", Value::string(&source)),
            ("std_compatibility_revision", Value::integer(4)),
            ("target", Value::string(&triple)),
            ("archive", Value::string(&name)),
            ("sha256", Value::string(digest)),
        ] {
            config::field(&mut metadata, key, value).unwrap();
        }
        std::fs::write(
            input.join(format!("{name}.metadata.json")),
            config::json(&metadata).unwrap(),
        )
        .unwrap();
    }
    let output = input.join("staged");
    let state = publication::nightly_stage(
        &input,
        &output,
        &Identity { source, version: version.into(), ci_run_id: 10 },
        20 + count,
        1,
        4,
    )
    .unwrap();
    (output, state)
}

#[test]
fn eligibility_requires_exact_identity_and_all_jobs_and_tolerates_only_pending_snapshots() {
    let mut remote = Remote::new();
    assert_eq!(
        publication::eligible_with(&mut remote, 10, Duration::from_secs(10), |_| {})
            .unwrap()
            .source,
        source('a')
    );
    for (key, value) in [
        ("event", Value::string("pull_request")),
        ("head_branch", Value::string("feature")),
        ("conclusion", Value::string("failure")),
        ("run_attempt", Value::integer(0)),
    ] {
        let mut remote = Remote::new();
        config::field(&mut remote.run, key, value).unwrap();
        assert!(
            publication::eligible_with(&mut remote, 10, Duration::from_secs(10), |_| {}).is_err()
        );
    }
    for state in ["skipped", "failure", "cancelled", "timed_out"] {
        let mut remote = Remote::new();
        config::field(&mut remote.jobs[0], "conclusion", Value::string(state)).unwrap();
        assert!(
            publication::eligible_with(&mut remote, 10, Duration::from_secs(10), |_| {}).is_err()
        );
    }
    let mut remote = Remote::new();
    remote.lag = 3;
    let mut waits = 0;
    assert!(publication::eligible_with(&mut remote, 10, Duration::from_secs(10), |_| waits += 1)
        .is_ok());
    assert_eq!(waits, 3);
    let mut remote = Remote::new();
    remote.lag = 20;
    assert!(publication::eligible_with(&mut remote, 10, Duration::from_secs(10), |_| {}).is_err());
    let mut remote = Remote::new();
    remote.lag = 1;
    remote.change_attempt = true;
    assert!(publication::eligible_with(&mut remote, 10, Duration::from_secs(10), |_| {})
        .unwrap_err()
        .to_string()
        .contains("identity changed"));
}

#[test]
fn generation_has_nine_archives_and_twenty_nine_bound_assets() {
    let temp = tempfile::tempdir().unwrap();
    let (output, state) = generation(temp.path(), 'a', 1);
    let Value::Object(assets) = state.get("assets").unwrap() else { panic!() };
    assert_eq!(assets.len(), 29);
    assert_eq!(storage::files(&output).unwrap().len(), 29);
    for (name, record) in assets {
        assert_eq!(
            storage::digest(&output.join(name)).unwrap(),
            config::text(record, "sha256").unwrap()
        );
    }
}

#[test]
fn recoverable_promotion_retains_the_old_release_until_verified_uploads() {
    for fault in ["upload", "digest", "lost-response", "none"] {
        let temp = tempfile::tempdir().unwrap();
        let mut remote = Remote::new();
        let (dir, mut state) = generation(temp.path(), 'a', 1);
        assert_eq!(publication::promote(&mut remote, &dir, &mut state).unwrap(), "published");
        let old = remote
            .releases
            .values()
            .find(|r| r.get_str("tag_name") == Some("nightly"))
            .unwrap()
            .clone();
        let old_id = old.get_u64("id").unwrap();
        remote.calls.clear();
        let (directory, mut next) = generation(temp.path(), 'b', 2);
        match fault {
            "upload" => remote.upload_failure = true,
            "digest" => remote.corrupt = true,
            "lost-response" => {
                remote.fault = Some(("PATCH".into(), format!("releases/{old_id}"), true))
            },
            _ => {},
        }
        let result = publication::promote(&mut remote, &directory, &mut next);
        if ["upload", "digest"].contains(&fault) {
            assert!(result.is_err());
            assert_eq!(
                publication::active_state(Some(&remote.releases[&old_id]), "nightly").unwrap(),
                publication::active_state(Some(&old), "nightly").unwrap()
            );
            assert_eq!(remote.tag.as_deref(), Some(source('a').as_str()));
            remote.upload_failure = false;
            remote.corrupt = false;
            if fault == "digest" {
                remote
                    .assets
                    .get_mut(&old_id)
                    .unwrap()
                    .retain(|asset| !asset.get_str("name").unwrap().contains(&source('b')));
            }
            assert_eq!(
                publication::promote(&mut remote, &directory, &mut next).unwrap(),
                "published"
            );
        } else {
            assert_eq!(result.unwrap(), "published");
        }
        assert_eq!(next.get_u64("publication_number"), Some(2));
        assert_eq!(remote.releases.len(), 1);
        assert!(remote.releases.contains_key(&old_id));
        assert_eq!(remote.tag.as_deref(), Some(source('b').as_str()));
        assert_eq!(publication::promote(&mut remote, &directory, &mut next).unwrap(), "unchanged");
    }
}

#[test]
fn stale_generations_cannot_replace_a_newer_published_release() {
    let temp = tempfile::tempdir().unwrap();
    let mut remote = Remote::new();
    let (dir, mut state) = generation(temp.path(), 'b', 1);
    publication::promote(&mut remote, &dir, &mut state).unwrap();
    let (dir, mut state) = generation(temp.path(), 'a', 2);
    assert_eq!(publication::promote(&mut remote, &dir, &mut state).unwrap(), "stale");
    assert_eq!(remote.tag.as_deref(), Some(source('b').as_str()));
    remote.tag = Some(source('c'));
    let releases = remote.releases.clone();
    let assets = remote.assets.clone();
    let (dir, mut state) = generation(temp.path(), 'b', 3);
    assert!(publication::promote(&mut remote, &dir, &mut state).is_err());
    assert_eq!(remote.releases, releases);
    assert_eq!(remote.assets, assets);
    assert_eq!(remote.tag.as_deref(), Some(source('c').as_str()));
}

#[test]
fn interrupted_cutover_and_cleanup_keep_the_public_id_and_reconcile_observed_mutations() {
    for (method, endpoint, skip) in [
        ("PATCH", "releases/1", 0),
        ("PATCH", "git/refs/tags/nightly", 0),
        ("PATCH", "releases/1", 1),
        ("DELETE", "releases/assets/1000", 0),
        ("PATCH", "releases/1", 2),
    ] {
        for after in [false, true] {
            let temp = tempfile::tempdir().unwrap();
            let mut remote = Remote::new();
            let (first_dir, mut first) = generation(temp.path(), 'a', 1);
            publication::promote(&mut remote, &first_dir, &mut first).unwrap();
            let id = remote
                .releases
                .values()
                .find(|r| r.get_str("tag_name") == Some("nightly"))
                .unwrap()
                .get_u64("id")
                .unwrap();
            let old_names: Vec<_> =
                remote.assets[&id].iter().map(|a| a.get_str("name").unwrap().to_string()).collect();
            let (directory, mut next) = generation(temp.path(), 'b', 2);
            remote.calls.clear();
            remote.fault = Some((method.into(), endpoint.into(), after));
            remote.fault_skip = skip;
            let result = publication::promote(&mut remote, &directory, &mut next);
            assert!(remote.releases.contains_key(&id), "{method} {endpoint} {skip} after={after}");
            assert!(!remote.calls.iter().any(|call| call == &format!("DELETE releases/{id}")));
            let active =
                publication::active_state(Some(&remote.releases[&id]), "nightly").unwrap().unwrap();
            publication::verify_remote(&mut remote, id, &active).unwrap();
            if active.get_str("source_sha") == Some(source('a').as_str()) {
                assert!(old_names.iter().all(|name| remote.assets[&id]
                    .iter()
                    .any(|a| a.get_str("name") == Some(name.as_str()))));
            }
            if result.is_err() {
                assert_eq!(
                    publication::promote(&mut remote, &directory, &mut next).unwrap(),
                    "published"
                );
            }
            let active =
                publication::active_state(Some(&remote.releases[&id]), "nightly").unwrap().unwrap();
            assert_eq!(active.get_str("source_sha"), Some(source('b').as_str()));
            assert_eq!(remote.tag.as_deref(), Some(source('b').as_str()));
            assert_eq!(remote.assets[&id].len(), 29);
            publication::verify_remote(&mut remote, id, &active).unwrap();
            assert!(!remote.releases[&id]
                .get_str("body")
                .unwrap()
                .contains("wave-nightly-transition:"));
            assert_eq!(
                publication::promote(&mut remote, &directory, &mut next).unwrap(),
                "unchanged"
            );
        }
    }
}

#[test]
fn newer_source_can_abandon_owned_incomplete_uploads_without_losing_the_active_generation() {
    let temp = tempfile::tempdir().unwrap();
    let mut remote = Remote::new();
    let (first_dir, mut first) = generation(temp.path(), 'a', 1);
    publication::promote(&mut remote, &first_dir, &mut first).unwrap();
    let (directory, mut second) = generation(temp.path(), 'b', 2);
    remote.upload_failure = true;
    assert!(publication::promote(&mut remote, &directory, &mut second).is_err());
    remote.upload_failure = false;
    let (directory, mut third) = generation(temp.path(), 'c', 3);
    assert_eq!(publication::promote(&mut remote, &directory, &mut third).unwrap(), "published");
    assert_eq!(remote.releases.len(), 1);
    assert_eq!(remote.tag.as_deref(), Some(source('c').as_str()));
    assert_eq!(remote.assets[&1].len(), 29);
    assert_eq!(publication::promote(&mut remote, &first_dir, &mut first).unwrap(), "stale");
}

#[test]
fn changelog_text_cannot_terminate_or_replace_the_persisted_recovery_records() {
    let temp = tempfile::tempdir().unwrap();
    let mut remote = Remote::new();
    let (directory, mut first) = generation(temp.path(), 'a', 1);
    config::field(
        &mut first,
        "changelog",
        Value::string(
            " -->\n<!-- wave-nightly-state:bad -->\n<!-- wave-nightly-transition:bad -->",
        ),
    )
    .unwrap();
    publication::promote(&mut remote, &directory, &mut first).unwrap();
    let (directory, mut second) = generation(temp.path(), 'b', 2);
    config::field(&mut second, "changelog", first.get("changelog").unwrap().clone()).unwrap();
    assert_eq!(publication::promote(&mut remote, &directory, &mut second).unwrap(), "published");
    assert_eq!(remote.tag.as_deref(), Some(source('b').as_str()));
}

#[test]
fn partial_or_ambiguous_uploads_resume_without_clobber_and_complete_pending_needs_no_workspace() {
    for count in [0, 1, 5, 28] {
        for after in [false, true] {
            let temp = tempfile::tempdir().unwrap();
            let mut remote = Remote::new();
            let (directory, mut first) = generation(temp.path(), 'a', 1);
            publication::promote(&mut remote, &directory, &mut first).unwrap();
            let (directory, mut next) = generation(temp.path(), 'b', 2);
            remote.upload_error_after = Some((count, after));
            assert!(publication::promote(&mut remote, &directory, &mut next).is_err());
            assert_eq!(remote.tag.as_deref(), Some(source('a').as_str()));
            assert_eq!(remote.assets[&1].len(), 29 + count + usize::from(after));
            let current =
                publication::active_state(Some(&remote.releases[&1]), "nightly").unwrap().unwrap();
            publication::verify_remote(&mut remote, 1, &current).unwrap();
            if count == 28 && after {
                // All bytes are remote; recovery must not need the dead runner.
                std::fs::remove_dir_all(&directory).unwrap();
            }
            assert_eq!(
                publication::promote(&mut remote, &directory, &mut next).unwrap(),
                "published"
            );
            assert_eq!(remote.assets[&1].len(), 29);
            assert_eq!(remote.releases.len(), 1);
            assert_eq!(remote.tag.as_deref(), Some(source('b').as_str()));
        }
    }
}

#[test]
fn invalid_or_unowned_recovery_state_never_mutates_remote_assets() {
    for fault in [
        "unknown-asset",
        "duplicate-asset",
        "missing-active",
        "broken-journal",
        "wrong-number",
        "zero-run",
        "foreign-tag",
    ] {
        let temp = tempfile::tempdir().unwrap();
        let mut remote = Remote::new();
        let (directory, mut first) = generation(temp.path(), 'a', 1);
        publication::promote(&mut remote, &directory, &mut first).unwrap();
        let (directory, mut next) = generation(temp.path(), 'b', 2);
        remote.upload_failure = true;
        assert!(publication::promote(&mut remote, &directory, &mut next).is_err());
        remote.upload_failure = false;
        match fault {
            "unknown-asset" => {
                let mut asset = remote.assets[&1][0].clone();
                config::field(&mut asset, "name", Value::string("unowned.txt")).unwrap();
                config::field(&mut asset, "id", Value::integer(9000)).unwrap();
                remote.assets.get_mut(&1).unwrap().push(asset);
            },
            "duplicate-asset" => {
                let asset = remote.assets[&1][0].clone();
                remote.assets.get_mut(&1).unwrap().push(asset);
            },
            "missing-active" => {
                remote.assets.get_mut(&1).unwrap().pop();
            },
            "foreign-tag" => {
                remote.tag = Some(source('c'));
            },
            _ => {
                let body = config::text(&remote.releases[&1], "body").unwrap();
                let (prefix, journal) =
                    body.rsplit_once("\n<!-- wave-nightly-transition:").unwrap();
                let mut journal =
                    config::parse_json(journal.strip_suffix(" -->").unwrap()).unwrap();
                let prefix = prefix.to_owned();
                if fault == "broken-journal" {
                    config::field(
                        remote.releases.get_mut(&1).unwrap(),
                        "body",
                        Value::string(format!("{prefix}\n<!-- wave-nightly-transition:{{ -->")),
                    )
                    .unwrap();
                } else {
                    let mut to = journal.get("to").unwrap().clone();
                    let (key, value) = if fault == "wrong-number" {
                        ("publication_number", 1)
                    } else {
                        ("run_id", 0)
                    };
                    config::field(&mut to, key, Value::integer(value)).unwrap();
                    config::field(&mut journal, "to", to).unwrap();
                    let encoded =
                        utils::wson::dumps(&journal, utils::wson::Format::Json, false).unwrap();
                    let body = format!("{prefix}\n<!-- wave-nightly-transition:{encoded} -->");
                    config::field(
                        remote.releases.get_mut(&1).unwrap(),
                        "body",
                        Value::string(body),
                    )
                    .unwrap();
                }
            },
        }
        remote.calls.clear();
        let releases = remote.releases.clone();
        let assets = remote.assets.clone();
        let tag = remote.tag.clone();
        assert!(publication::promote(&mut remote, &directory, &mut next).is_err(), "{fault}");
        assert_eq!(remote.releases, releases, "{fault}");
        assert_eq!(remote.assets, assets, "{fault}");
        assert_eq!(remote.tag, tag, "{fault}");
        assert!(
            remote.calls.iter().all(|call| call.starts_with("GET ")),
            "{}: {:?}",
            fault,
            remote.calls
        );
    }
}
