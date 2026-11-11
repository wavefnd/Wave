// SPDX-License-Identifier: MPL-2.0
//! Strict, testable remote identity gates and recoverable release publication.
use crate::{cases, checks, cli::Args, config, error, package, process::Spec, storage, Result};
use base64::Engine;
use std::collections::{BTreeMap, BTreeSet};
use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};
use utils::wson::Value;
use yaml_rust2::{Yaml, YamlLoader};

pub const REPO: &str = "wavefnd/Wave";

pub trait Api {
    fn request(
        &mut self,
        method: &str,
        endpoint: &str,
        data: Option<&Value>,
        optional: bool,
    ) -> Result<Option<Value>>;
    fn upload(&mut self, path: &Path, tag: &str) -> Result<()>;
    fn get(&mut self, endpoint: &str) -> Result<Value> {
        self.request("GET", endpoint, None, false)?
            .ok_or_else(|| error("empty GitHub API response"))
    }

    fn assets(&mut self, id: u64) -> Result<Vec<Value>> {
        let mut result = Vec::new();
        for page in 1..=10000 {
            let value = self.get(&format!("releases/{id}/assets?per_page=100&page={page}"))?;
            let Value::Array(values) = value else {
                return Err(error("asset response must be an array"));
            };
            let last = values.len() < 100;
            result.extend(values);
            if last {
                return Ok(result);
            }
        }
        Err(error("GitHub asset pagination limit exceeded"))
    }
}

pub struct GitHub;
impl Api for GitHub {
    fn request(
        &mut self,
        method: &str,
        endpoint: &str,
        data: Option<&Value>,
        optional: bool,
    ) -> Result<Option<Value>> {
        let mut args = vec![
            OsString::from("gh"),
            "api".into(),
            "--hostname".into(),
            "github.com".into(),
            format!("repos/{REPO}/{endpoint}").into(),
            "--method".into(),
            method.into(),
        ];
        if data.is_some() {
            args.extend(["--input".into(), "-".into()]);
        }
        let mut spec = Spec::new(args);
        spec.timeout = Duration::from_secs(120);
        spec.input = data.map(config::json).transpose()?.map(String::into_bytes);
        let output = spec.run()?;
        if output.failure.is_some() || output.status != 0 {
            if optional && output.text_stderr().contains("HTTP 404") {
                return Ok(None);
            }
            return Err(error(format!(
                "GitHub {method} {endpoint}: {} {}",
                output.failure.as_deref().unwrap_or_default(),
                output.text_stderr()
            )));
        }
        let text = output.strict_stdout()?;
        if text.trim().is_empty() {
            Ok(None)
        } else {
            Ok(Some(config::parse_json(&text)?))
        }
    }

    fn upload(&mut self, path: &Path, tag: &str) -> Result<()> {
        Spec::new([
            OsString::from("gh"),
            "release".into(),
            "upload".into(),
            tag.into(),
            path.as_os_str().into(),
            "--repo".into(),
            REPO.into(),
        ])
        .checked()?;
        Ok(())
    }
}

pub fn sha(value: &str) -> Result<&str> {
    if value.len() == 40 && value.bytes().all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
    {
        Ok(value)
    } else {
        Err(error("expected full lowercase source SHA"))
    }
}

fn yaml_text(value: &Yaml) -> Result<&str> {
    value.as_str().ok_or_else(|| error("workflow value must be a string"))
}

fn display(value: &Value) -> Result<String> {
    match value {
        Value::String(value) => Ok(value.clone()),
        Value::Number(value) => Ok(value.as_str().into()),
        Value::Bool(value) => Ok(value.to_string()),
        _ => Err(error("matrix field must be scalar")),
    }
}

fn matrix_name(template: &str, entry: &Value) -> Result<String> {
    let simple = regex_lite::Regex::new(r"^matrix\.([A-Za-z_][A-Za-z0-9_]*)$")?;
    let conditional = regex_lite::Regex::new(
        r"^matrix\.([A-Za-z_][A-Za-z0-9_]*)\s*==\s*'([^']*)'\s*&&\s*'([^']*)'\s*\|\|\s*'([^']*)'$",
    )?;
    let expression = regex_lite::Regex::new(r"\$\{\{(.*?)\}\}")?;
    let mut result = String::new();
    let mut previous = 0;
    for captures in expression.captures_iter(template) {
        let entire = captures.get(0).unwrap();
        result += &template[previous..entire.start()];
        let source = captures[1].trim();
        if let Some(capture) = simple.captures(source) {
            result +=
                &display(entry.get(&capture[1]).ok_or_else(|| error("missing matrix field"))?)?;
        } else if let Some(capture) = conditional.captures(source) {
            let actual = display(
                entry.get(&capture[1]).ok_or_else(|| error("missing matrix conditional field"))?,
            )?;
            result += if actual == capture[2] { &capture[3] } else { &capture[4] };
        } else {
            return Err(error(
                "unsupported workflow name expression; required jobs cannot be guessed",
            ));
        }
        previous = entire.end();
    }
    result += &template[previous..];
    if result.contains("${{") {
        return Err(error("unterminated workflow name expression"));
    }
    Ok(result)
}

pub fn required_names(workflow: &str, manifest: &str) -> Result<BTreeSet<String>> {
    let documents = YamlLoader::load_from_str(workflow)?;
    if documents.len() != 1 {
        return Err(error("workflow must contain one document"));
    }
    let jobs = documents[0]["jobs"].as_hash().ok_or_else(|| error("missing workflow jobs"))?;
    let manifest = cases::Manifest::parse(manifest, PathBuf::new(), false)?;
    let matrices = manifest.workflow_matrices()?;
    let dynamic = regex_lite::Regex::new(
        r"^\$\{\{\s*fromJSON\(needs\.case-matrix\.outputs\.([A-Za-z_0-9]+)\)\s*\}\}$",
    )?;
    let mut required = BTreeSet::new();
    for (_, job) in jobs {
        let name = yaml_text(&job["name"])?;
        let entries = if name.contains("${{") {
            let matrix = &job["strategy"]["matrix"];
            if let Some(text) = matrix.as_str() {
                let captures =
                    dynamic.captures(text).ok_or_else(|| error("unsupported dynamic CI matrix"))?;
                config::array(
                    matrices.get(&captures[1]).ok_or_else(|| error("unknown dynamic matrix"))?,
                    "include",
                )?
                .to_vec()
            } else {
                let mut entries = Vec::new();
                for entry in matrix["include"]
                    .as_vec()
                    .ok_or_else(|| error("unsupported static CI matrix"))?
                {
                    let mut fields = Vec::new();
                    for (key, value) in
                        entry.as_hash().ok_or_else(|| error("matrix entry must be an object"))?
                    {
                        let value = match value {
                            Yaml::String(v) => Value::string(v),
                            Yaml::Integer(v) => config::number(*v),
                            Yaml::Boolean(v) => Value::Bool(*v),
                            _ => return Err(error("unsupported matrix value")),
                        };
                        fields.push((yaml_text(key)?.to_string(), value));
                    }
                    entries.push(Value::object(fields));
                }
                entries
            }
        } else {
            vec![Value::object(Vec::<(String, Value)>::new())]
        };
        if entries.is_empty() {
            return Err(error("empty required job matrix"));
        }
        for entry in entries {
            let name = matrix_name(name, &entry)?;
            if !required.insert(name) {
                return Err(error("duplicate required CI job name"));
            }
        }
    }
    if required.is_empty() {
        return Err(error("empty CI job contract"));
    }
    Ok(required)
}

pub fn ancestor(api: &mut impl Api, older: &str, newer: &str) -> Result<bool> {
    sha(older)?;
    sha(newer)?;
    if older == newer {
        return Ok(true);
    }
    let comparison = api.get(&format!("compare/{older}...{newer}"))?;
    Ok(comparison.get_str("status") == Some("ahead")
        && comparison.get("merge_base_commit").and_then(|v| v.get_str("sha")) == Some(older))
}

fn source_text(api: &mut impl Api, source: &str, path: &str) -> Result<String> {
    let content = api.get(&format!("contents/{path}?ref={source}"))?;
    let content = config::text(&content, "content")?.split_whitespace().collect::<String>();
    Ok(String::from_utf8(base64::engine::general_purpose::STANDARD.decode(content)?)?)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identity {
    pub source: String,
    pub version: String,
    pub ci_run_id: u64,
}
impl Identity {
    pub fn value(&self) -> Value {
        Value::object([
            ("source_sha", Value::string(&self.source)),
            ("compiler_version", Value::string(&self.version)),
            ("ci_run_id", Value::integer(self.ci_run_id)),
        ])
    }
}

fn check_run(
    original: &Value,
    current: &Value,
    id: u64,
    attempt: u64,
    source: &str,
) -> Result<bool> {
    if current.get_u64("id").unwrap_or(id) != id
        || current.get_u64("run_attempt") != Some(attempt)
        || current.get_str("head_sha") != Some(source)
        || ["repository", "head_repository", "event", "head_branch", "path"].iter().any(|key| {
            current.get(key).zip(original.get(key)).is_none_or(|(a, b)| !config::equivalent(a, b))
        })
    {
        return Err(error("CI run identity changed during eligibility validation"));
    }
    let status = config::text(current, "status")?;
    if !["queued", "in_progress", "completed"].contains(&status) {
        return Err(error("invalid CI status"));
    }
    let conclusion = current.get("conclusion");
    if !matches!(conclusion, None | Some(Value::Null))
        && current.get_str("conclusion") != Some("success")
        || status == "completed" && current.get_str("conclusion") != Some("success")
    {
        return Err(error("canonical CI did not complete successfully"));
    }
    Ok(status == "completed" && current.get_str("conclusion") == Some("success"))
}

pub fn eligible(api: &mut impl Api, id: u64) -> Result<Identity> {
    eligible_with(api, id, Duration::from_secs(10), std::thread::sleep)
}

pub fn eligible_with(
    api: &mut impl Api,
    id: u64,
    interval: Duration,
    mut sleep: impl FnMut(Duration),
) -> Result<Identity> {
    if id == 0 {
        return Err(error("positive canonical CI run ID is required"));
    }
    let run = api.get(&format!("actions/runs/{id}"))?;
    if run.get("repository").and_then(|v| v.get_str("full_name")) != Some(REPO)
        || run.get("head_repository").and_then(|v| v.get_str("full_name")) != Some(REPO)
        || run.get_str("event") != Some("push")
        || run.get_str("head_branch") != Some("master")
        || run.get_str("path") != Some(".github/workflows/ci.yml")
    {
        return Err(error("Nightly requires canonical master push CI"));
    }
    let source = sha(config::text(&run, "head_sha")?)?.to_string();
    let attempt = run
        .get_u64("run_attempt")
        .filter(|n| *n > 0)
        .ok_or_else(|| error("invalid CI run attempt"))?;
    let required = required_names(
        &source_text(api, &source, ".github/workflows/ci.yml")?,
        &source_text(api, &source, "tests/cases/cases.toml")?,
    )?;
    let start = Instant::now();
    let mut current = run.clone();
    let mut success = false;
    for probe in 0..=6 {
        if probe > 0 {
            let due = interval * probe;
            if start.elapsed() > interval * 6 {
                return Err(error("CI snapshots did not settle within 60 seconds"));
            }
            sleep(due.saturating_sub(start.elapsed()));
            current = api.get(&format!("actions/runs/{id}"))?;
        }
        check_run(&run, &current, id, attempt, &source)?;
        let mut jobs = Vec::new();
        let mut total = None;
        for page in 1..=10000 {
            let value = api.get(&format!(
                "actions/runs/{id}/attempts/{attempt}/jobs?per_page=100&page={page}"
            ))?;
            if let Some(count) = value.get_u64("total_count") {
                total = Some(count);
            }
            let batch = config::array(&value, "jobs")?;
            jobs.extend_from_slice(batch);
            if batch.len() < 100 {
                break;
            }
        }
        if jobs.iter().any(|j| {
            !matches!(j.get("conclusion"), None | Some(Value::Null))
                && j.get_str("conclusion") != Some("success")
        }) {
            return Err(error(
                "CI jobs must all complete successfully; skipped jobs are not validation",
            ));
        }
        let names: std::result::Result<BTreeSet<_>, _> =
            jobs.iter().map(|j| config::text(j, "name").map(String::from)).collect();
        if !jobs.is_empty()
            && total.is_none_or(|n| n == jobs.len() as u64)
            && names? == required
            && jobs.len() == required.len()
            && jobs.iter().all(|j| {
                j.get_str("status") == Some("completed")
                    && j.get_str("conclusion") == Some("success")
            })
        {
            let settled = api.get(&format!("actions/runs/{id}"))?;
            if check_run(&run, &settled, id, attempt, &source)? {
                success = true;
                break;
            }
        }
    }
    if !success {
        return Err(error("CI snapshots did not settle within 60 seconds"));
    }
    let master = api.get("git/ref/heads/master")?;
    let master =
        config::text(master.get("object").ok_or_else(|| error("missing master object"))?, "sha")?;
    if !ancestor(api, &source, master)? {
        return Err(error("CI source is not in canonical master history"));
    }
    let manifest: toml::Value = source_text(api, &source, "Cargo.toml")?.parse()?;
    let version = manifest["package"]["version"]
        .as_str()
        .ok_or_else(|| error("missing compiler version"))?
        .to_string();
    validate_version(&version, false)?;
    Ok(Identity { source, version, ci_run_id: id })
}

pub fn validate_version(version: &str, release: bool) -> Result<()> {
    if !regex_lite::Regex::new(r"^[0-9]+\.[0-9]+\.[0-9]+(?:-[0-9A-Za-z.-]+)?$")?.is_match(version)
        || release && version.ends_with("-dev")
    {
        return Err(error("invalid publication version"));
    }
    Ok(())
}

pub fn installers_ready() -> Result<()> {
    let pins = config::read_json(&crate::root().join("tools/ci/nightly_installers.json"))?;
    let Value::Object(pins) = pins else {
        return Err(error("installer approvals must be an object"));
    };
    let required =
        BTreeSet::from(["https://wave-lang.dev/install.sh", "https://wave-lang.dev/install.ps1"]);
    if pins.iter().map(|(url, _)| url.as_str()).collect::<BTreeSet<_>>() != required {
        return Err(error("both reviewed installer endpoint approvals are required"));
    }
    for (url, expected) in pins {
        let Value::String(expected) = expected else {
            return Err(error("installer digest must be a string"));
        };
        if expected.len() != 64
            || !expected.bytes().all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
        {
            return Err(error("invalid approved installer digest"));
        }
        let mut request = Spec::new([
            "curl",
            "--fail",
            "--location",
            "--max-time",
            "30",
            "--user-agent",
            "Wave-Nightly/1.0",
            &url,
        ]);
        request.timeout = Duration::from_secs(35);
        request.output_limit = 1024 * 1024;
        let bytes = request.checked()?.stdout;
        let actual = storage::bytes_digest(&bytes);
        if actual != expected {
            return Err(error(format!("reviewed installer is not deployed: {url} (expected SHA-256 {expected}, received {actual})")));
        }
    }
    Ok(())
}

fn file_record(path: &Path) -> Result<Value> {
    Ok(Value::object([
        ("sha256", Value::string(storage::digest(path)?)),
        ("size", Value::integer(std::fs::metadata(path)?.len())),
    ]))
}

pub fn nightly_stage(
    directory: &Path,
    output: &Path,
    identity: &Identity,
    run: u64,
    attempt: u64,
    revision: u64,
) -> Result<Value> {
    if run == 0 || attempt == 0 {
        return Err(error("invalid Nightly workflow run identity"));
    }
    checks::release_assets(directory, &identity.version, Some(&identity.source), Some(revision))?;
    let generation = format!("{}-{run}-{attempt}", identity.source);
    std::fs::create_dir(output)?;
    let mut sums = String::new();
    let mut descriptors: Vec<_> = std::fs::read_dir(directory)?
        .map(|e| e.map(|e| e.path()))
        .collect::<std::io::Result<_>>()?;
    descriptors.sort();
    for descriptor in
        descriptors.into_iter().filter(|p| p.to_string_lossy().ends_with(".metadata.json"))
    {
        let mut metadata = config::read_json(&descriptor)?;
        let original = config::text(&metadata, "archive")?.to_string();
        let name = original.replacen(
            &format!("wave-v{}-", identity.version),
            &format!("wave-nightly-{generation}-"),
            1,
        );
        std::fs::copy(directory.join(&original), output.join(&name))?;
        if storage::digest(&output.join(&name))? != config::text(&metadata, "sha256")? {
            return Err(error("package changed during Nightly staging"));
        }
        config::field(&mut metadata, "archive", Value::string(&name))?;
        config::field(&mut metadata, "channel", Value::string("nightly"))?;
        config::field(&mut metadata, "generation", Value::string(&generation))?;
        std::fs::write(output.join(format!("{name}.metadata.json")), config::json(&metadata)?)?;
        let line = format!("{}  {name}\n", config::text(&metadata, "sha256")?);
        std::fs::write(output.join(format!("{name}.sha256")), &line)?;
        sums += &line;
    }
    std::fs::write(output.join(format!("nightly-{generation}-SHA256SUMS")), sums)?;
    let records = storage::files(output)?
        .into_iter()
        .map(|p| Ok((p.file_name().unwrap().to_string_lossy().to_string(), file_record(&p)?)))
        .collect::<Result<Vec<_>>>()?;
    let mut state = identity.value();
    for (key, value) in [
        ("schema_version", Value::integer(1)),
        ("generation", Value::string(&generation)),
        ("run_id", Value::integer(run)),
        (
            "built_at",
            Value::string(
                time::OffsetDateTime::now_utc()
                    .format(&time::format_description::well_known::Rfc3339)?,
            ),
        ),
        ("assets", Value::object(records)),
    ] {
        config::field(&mut state, key, value)?;
    }
    let manifest = output.join(format!("nightly-{generation}.json"));
    std::fs::write(&manifest, config::json(&state)?)?;
    let mut assets = state.get("assets").unwrap().clone();
    config::field(
        &mut assets,
        &manifest.file_name().unwrap().to_string_lossy(),
        file_record(&manifest)?,
    )?;
    config::field(&mut state, "assets", assets)?;
    Ok(state)
}

pub fn title(state: &Value) -> Result<String> {
    if state.get("publication_number").is_none() {
        return Ok("Wave Nightly".into());
    }
    let number = state
        .get_u64("publication_number")
        .filter(|n| *n > 0)
        .ok_or_else(|| error("invalid Nightly publication number"))?;
    let built = time::OffsetDateTime::parse(
        config::text(state, "built_at")?,
        &time::format_description::well_known::Rfc3339,
    )?;
    let date = built.to_offset(time::UtcOffset::from_hms(9, 0, 0)?).date();
    Ok(format!("Wave {date}-{number:02}-nightly"))
}

pub fn active_state(release: Option<&Value>, tag: &str) -> Result<Option<Value>> {
    let Some(release) = release else {
        return Ok(None);
    };
    if release.get_str("tag_name") != Some(tag)
        || release.get("prerelease") != Some(&Value::Bool(true))
    {
        return Err(error("refusing to replace unmanaged Nightly release"));
    }
    let body = release.get_str("body").unwrap_or("");
    let marker = "<!-- wave-nightly-state:";
    let state = if let Some((_, tail)) = body.rsplit_once(marker) {
        let (state, _) =
            tail.split_once(" -->").ok_or_else(|| error("unterminated Nightly recovery state"))?;
        config::parse_json(state)?
    } else {
        if release.get("draft") == Some(&Value::Bool(true))
            && body.is_empty()
            && release.get_str("name") == Some("Wave Nightly")
        {
            return Ok(None);
        }
        return Err(error("Nightly release has no recovery state"));
    };
    sha(config::text(&state, "source_sha")?)?;
    if state.get_u64("schema_version") != Some(1)
        || !matches!(state.get("assets"),Some(Value::Object(fields)) if !fields.is_empty())
        || release.get_str("name") != Some(title(&state)?.as_str())
    {
        return Err(error("invalid Nightly recovery identity/title"));
    }
    validate_asset_records(&state)?;
    Ok(Some(state))
}

fn validate_asset_records(state: &Value) -> Result<()> {
    let Value::Object(records) = state.get("assets").ok_or_else(|| error("missing assets"))? else {
        return Err(error("assets must be an object"));
    };
    let mut names = BTreeSet::new();
    for (name, record) in records {
        if cases::relative(name)?.components().count() != 1 || !names.insert(name) {
            return Err(error("invalid or duplicate Nightly asset name"));
        }
        let hash = config::text(record, "sha256")?;
        if hash.len() != 64
            || !hash.bytes().all(|c| c.is_ascii_digit() || (b'a'..=b'f').contains(&c))
            || record.get_u64("size").is_none()
        {
            return Err(error("invalid Nightly asset checksum or size"));
        }
    }
    Ok(())
}

pub fn verify_remote(api: &mut impl Api, id: u64, state: &Value) -> Result<()> {
    validate_asset_records(state)?;
    let release = api.get(&format!("releases/{id}"))?;
    if let Some(journal) = transition(&release)? {
        let active = active_state(Some(&release), "nightly")?
            .ok_or_else(|| error("missing active Nightly state"))?;
        if !config::equivalent(state, &active) {
            return Err(error("requested verification is not the active Nightly generation"));
        }
        let from = journal.get("from").unwrap();
        let to = journal.get("to").unwrap();
        verify_owned(
            api,
            id,
            state,
            Some(if config::equivalent(state, from) { to } else { from }),
        )?;
        return Ok(());
    }
    let assets = api.assets(id)?;
    let Value::Object(expected) =
        state.get("assets").ok_or_else(|| error("missing Nightly assets"))?
    else {
        return Err(error("assets must be an object"));
    };
    let mut names = BTreeSet::new();
    for asset in &assets {
        if !names.insert(config::text(asset, "name")?) {
            return Err(error("duplicate remote Nightly asset"));
        }
    }
    if assets.len() != expected.len() {
        return Err(error("remote Nightly asset set differs from the verified generation"));
    }
    for (name, record) in expected {
        let asset = assets
            .iter()
            .find(|v| v.get_str("name") == Some(name))
            .ok_or_else(|| error(format!("missing Nightly asset: {name}")))?;
        if asset.get_str("state") != Some("uploaded")
            || asset.get_u64("size") != record.get_u64("size")
            || asset.get_str("digest")
                != Some(format!("sha256:{}", config::text(record, "sha256")?).as_str())
        {
            return Err(error(format!("unverified Nightly asset: {name}")));
        }
    }
    Ok(())
}

fn release_id(value: &Value) -> Result<u64> {
    value.get_u64("id").filter(|id| *id > 0).ok_or_else(|| error("invalid GitHub release ID"))
}

fn find_release(api: &mut impl Api, tag: &str) -> Result<Option<Value>> {
    if let Some(release) = api.request("GET", &format!("releases/tags/{tag}"), None, true)? {
        return Ok(Some(release));
    }
    for page in 1..=10000 {
        let value = api.get(&format!("releases?per_page=100&page={page}"))?;
        let Value::Array(batch) = value else {
            return Err(error("release list must be an array"));
        };
        if let Some(release) = batch.iter().find(|r| r.get_str("tag_name") == Some(tag)) {
            return Ok(Some(release.clone()));
        }
        if batch.len() < 100 {
            return Ok(None);
        }
    }
    Err(error("release pagination exceeded limit"))
}

fn set_tag(api: &mut impl Api, source: &str) -> Result<()> {
    sha(source)?;
    let tag = api.request("GET", "git/ref/tags/nightly", None, true)?;
    if let Some(tag) = tag {
        let object = tag.get("object").ok_or_else(|| error("invalid tag object"))?;
        if object.get_str("sha") != Some(source) {
            if object.get_str("type") != Some("commit") {
                return Err(error("Nightly requires a lightweight commit tag"));
            }
            api.request(
                "PATCH",
                "git/refs/tags/nightly",
                Some(&Value::object([
                    ("sha", Value::string(source)),
                    ("force", Value::Bool(true)),
                ])),
                false,
            )?;
        }
    } else {
        api.request(
            "POST",
            "git/refs",
            Some(&Value::object([
                ("ref", Value::string("refs/tags/nightly")),
                ("sha", Value::string(source)),
            ])),
            false,
        )?;
    }
    Ok(())
}

fn comment_state(state: &Value) -> Result<String> {
    Ok(utils::wson::dumps(state, utils::wson::Format::Json, false)?
        .replace('<', "\\u003c")
        .replace('>', "\\u003e"))
}

pub fn notes(state: &Value) -> Result<String> {
    let source = config::text(state, "source_sha")?;
    let mut text=format!("{}\n\nCommit: [{source}](https://github.com/{REPO}/commit/{source})\nBranch: master\nBuilt: {}\nCompiler version: {}\nBuild: https://github.com/{REPO}/actions/runs/{}\nValidated CI: https://github.com/{REPO}/actions/runs/{}\n\nThis release tracks the latest successfully validated master build.\n\nDownload manually below. Nightly is not available through install.sh or install.ps1.\n\n",title(state)?,config::text(state,"built_at")?,config::text(state,"compiler_version")?,state.get_u64("run_id").ok_or_else(||error("missing workflow run ID"))?,state.get_u64("ci_run_id").ok_or_else(||error("missing canonical CI ID"))?);
    if let Some(changelog) = state.get_str("changelog") {
        text += changelog;
        text += "\n\n";
    }
    let Value::Object(assets) =
        state.get("assets").ok_or_else(|| error("missing asset records"))?
    else {
        return Err(error("assets must be an object"));
    };
    for (name, _) in assets {
        text +=
            &format!("- [{name}](https://github.com/{REPO}/releases/download/nightly/{name})\n");
    }
    text += &format!("\n<!-- wave-nightly-state:{} -->", comment_state(state)?);
    Ok(text)
}

// A durable journal lives beside the active state in the same public release.
// Asset generations overlap only while this validated journal owns both sets.
const TRANSITION_MARKER: &str = "<!-- wave-nightly-transition:";

fn generation_records(state: &Value) -> Result<BTreeMap<String, Value>> {
    validate_asset_records(state)?;
    sha(config::text(state, "source_sha")?)?;
    let generation = config::text(state, "generation")?;
    let suffix = generation
        .strip_prefix(&format!("{}-", config::text(state, "source_sha")?))
        .ok_or_else(|| error("generation source mismatch"))?;
    let (run, attempt) =
        suffix.split_once('-').ok_or_else(|| error("missing generation attempt"))?;
    let run_id = state.get_u64("run_id").filter(|id| *id > 0);
    let attempt_id = attempt.parse::<u64>().ok().filter(|id| *id > 0);
    if run_id.is_none()
        || run_id.map(|id| id.to_string()).as_deref() != Some(run)
        || attempt_id.map(|id| id.to_string()).as_deref() != Some(attempt)
        || state.get_u64("publication_number").is_none_or(|v| v == 0)
        || state.get_u64("ci_run_id").is_none_or(|v| v == 0)
        || state.get_u64("schema_version") != Some(1)
    {
        return Err(error("invalid generation identity"));
    }
    let manifest = cases::Manifest::load(&crate::root(), false)?;
    let mut names = BTreeSet::new();
    let mut targets = 0;
    for target in manifest.targets.iter().filter(|t| {
        t.enabled
            && (matches!(t.os.as_str(), "linux" | "macos" | "windows")
                || t.os == "freebsd" && t.arch == "amd64")
    }) {
        targets += 1;
        let target = target.archive_target()?;
        let archive = format!(
            "wave-nightly-{generation}-{target}{}",
            if target.contains("windows") { ".zip" } else { ".tar.gz" }
        );
        for suffix in ["", ".sha256", ".metadata.json"] {
            names.insert(format!("{archive}{suffix}"));
        }
    }
    names.insert(format!("nightly-{generation}-SHA256SUMS"));
    names.insert(format!("nightly-{generation}.json"));
    let Value::Object(records) = state.get("assets").unwrap() else { unreachable!() };
    let records: BTreeMap<_, _> = records.iter().cloned().collect();
    if targets != 9 || records.len() != 29 || names != records.keys().cloned().collect() {
        return Err(error("Nightly generation must own exactly nine targets and 29 named assets"));
    }
    Ok(records)
}

fn transition(release: &Value) -> Result<Option<Value>> {
    let body = release.get_str("body").unwrap_or("").trim_end();
    let Some(line) = body.lines().last().filter(|line| line.starts_with(TRANSITION_MARKER)) else {
        return Ok(None);
    };
    let text = line
        .strip_prefix(TRANSITION_MARKER)
        .unwrap()
        .strip_suffix(" -->")
        .ok_or_else(|| error("unterminated Nightly transition"))?;
    let journal = config::parse_json(text)?;
    let from = journal.get("from").ok_or_else(|| error("transition missing prior generation"))?;
    let to = journal.get("to").ok_or_else(|| error("transition missing pending generation"))?;
    let left = generation_records(from)?;
    let right = generation_records(to)?;
    if left.keys().any(|name| right.contains_key(name))
        || journal.get_u64("schema_version") != Some(1)
        || config::text(&journal, "previous_tag")? != config::text(from, "source_sha")?
        || to.get_u64("publication_number")
            != from.get_u64("publication_number").and_then(|v| v.checked_add(1))
    {
        return Err(error("invalid or overlapping Nightly transition identities"));
    }
    let active = active_state(Some(release), "nightly")?
        .ok_or_else(|| error("transition has no active generation"))?;
    if !config::equivalent(&active, from) && !config::equivalent(&active, to) {
        return Err(error("active generation is outside its transition"));
    }
    Ok(Some(journal))
}

fn verify_owned(
    api: &mut impl Api,
    id: u64,
    required: &Value,
    optional: Option<&Value>,
) -> Result<bool> {
    let required = generation_records(required)?;
    let optional = optional.map(generation_records).transpose()?.unwrap_or_default();
    let assets = api.assets(id)?;
    let mut present = BTreeSet::new();
    for asset in &assets {
        let name = config::text(asset, "name")?;
        if !present.insert(name.to_string()) {
            return Err(error("duplicate remote Nightly asset"));
        }
        let record = required
            .get(name)
            .or_else(|| optional.get(name))
            .ok_or_else(|| error(format!("unowned Nightly asset: {name}")))?;
        if asset.get_str("state") != Some("uploaded")
            || asset.get_u64("size") != record.get_u64("size")
            || asset.get_str("digest")
                != Some(format!("sha256:{}", config::text(record, "sha256")?).as_str())
        {
            return Err(error(format!("conflicting immutable Nightly asset: {name}")));
        }
    }
    if required.keys().any(|name| !present.contains(name)) {
        return Err(error("active Nightly generation is incomplete"));
    }
    Ok(optional.keys().all(|name| present.contains(name)))
}

fn tag_source(api: &mut impl Api) -> Result<String> {
    let tag = api.get("git/ref/tags/nightly")?;
    let object = tag.get("object").ok_or_else(|| error("missing Nightly tag object"))?;
    if object.get_str("type") != Some("commit") {
        return Err(error("Nightly tag must remain a lightweight commit tag"));
    }
    Ok(sha(config::text(object, "sha")?)?.into())
}

fn tag_reconciled(api: &mut impl Api, source: &str) -> Result<()> {
    if let Err(failure) = set_tag(api, source) {
        if tag_source(api)? != source {
            return Err(failure);
        }
    }
    if tag_source(api)? != source {
        return Err(error("Nightly tag mutation was not observed"));
    }
    Ok(())
}

fn write_active(
    api: &mut impl Api,
    id: u64,
    state: &Value,
    journal: Option<&Value>,
) -> Result<Value> {
    let mut body = notes(state)?;
    if let Some(journal) = journal {
        body += &format!("\n{TRANSITION_MARKER}{} -->", comment_state(journal)?);
    }
    let name = title(state)?;
    let payload = Value::object([
        ("body", Value::string(&body)),
        ("name", Value::string(&name)),
        ("make_latest", Value::string("false")),
    ]);
    let mutation = api.request("PATCH", &format!("releases/{id}"), Some(&payload), false);
    let observed = api.get(&format!("releases/{id}"))?;
    if observed.get_str("body") != Some(body.as_str())
        || observed.get_str("name") != Some(name.as_str())
        || observed.get("draft") != Some(&Value::Bool(false))
    {
        mutation?;
        return Err(error("Nightly release mutation was not observed"));
    }
    Ok(observed)
}

fn delete_generation(api: &mut impl Api, id: u64, state: &Value) -> Result<()> {
    let records = generation_records(state)?;
    for asset in api.assets(id)? {
        if records.contains_key(config::text(&asset, "name")?) {
            let asset_id = release_id(&asset)?;
            if let Err(failure) =
                api.request("DELETE", &format!("releases/assets/{asset_id}"), None, false)
            {
                if api.assets(id)?.iter().any(|a| a.get_u64("id") == Some(asset_id)) {
                    return Err(failure);
                }
            }
        }
    }
    Ok(())
}

fn transition_to_active(api: &mut impl Api, id: u64, journal: &Value) -> Result<Value> {
    let from = journal.get("from").unwrap();
    let to = journal.get("to").unwrap();
    if config::text(from, "source_sha")? == config::text(to, "source_sha")?
        || !ancestor(api, config::text(from, "source_sha")?, config::text(to, "source_sha")?)?
    {
        return Err(error("pending generation is not newer than the active source"));
    }
    // The new generation must be complete before either tag or active links move.
    verify_owned(api, id, to, Some(from))?;
    let tag = tag_source(api)?;
    if tag != config::text(from, "source_sha")? && tag != config::text(to, "source_sha")? {
        return Err(error("Nightly tag left the recorded transition"));
    }
    let master = api.get("git/ref/heads/master")?;
    let master = config::text(
        master.get("object").ok_or_else(|| error("missing canonical master"))?,
        "sha",
    )?;
    if !ancestor(api, config::text(to, "source_sha")?, master)? {
        return Err(error("pending source left canonical master history"));
    }
    tag_reconciled(api, config::text(to, "source_sha")?)?;
    write_active(api, id, to, Some(journal))?;
    // Verify active identity and tag independently before deleting an old byte.
    if tag_source(api)? != config::text(to, "source_sha")? {
        return Err(error("Nightly tag disagrees with activated generation"));
    }
    verify_owned(api, id, to, Some(from))?;
    delete_generation(api, id, from)?;
    verify_owned(api, id, to, None)?;
    write_active(api, id, to, None)
}

fn promote_public(
    api: &mut impl Api,
    directory: &Path,
    requested: &mut Value,
    mut release: Value,
) -> Result<String> {
    let id = release_id(&release)?;
    let mut completed = false;
    if let Some(journal) = transition(&release)? {
        let from = journal.get("from").unwrap();
        let to = journal.get("to").unwrap();
        let active = active_state(Some(&release), "nightly")?.unwrap();
        let pending_complete = verify_owned(
            api,
            id,
            &active,
            Some(if config::equivalent(&active, from) { to } else { from }),
        )?;
        if config::equivalent(&active, to) || pending_complete {
            release = transition_to_active(api, id, &journal)?;
            completed = true;
        } else {
            if tag_source(api)? != config::text(from, "source_sha")? {
                return Err(error("incomplete pending generation cannot own the Nightly tag"));
            }
            if requested.get_str("generation") != to.get_str("generation") {
                let source = config::text(requested, "source_sha")?;
                if !ancestor(api, config::text(to, "source_sha")?, source)? {
                    return Ok("stale".into());
                }
                // Abandon only verified, owned incomplete uploads. Keep the old
                // active body and journal until their removal is confirmed.
                delete_generation(api, id, to)?;
                verify_owned(api, id, from, None)?;
                release = write_active(api, id, from, None)?;
            } else {
                config::field(
                    requested,
                    "publication_number",
                    to.get("publication_number").unwrap().clone(),
                )?;
                if !config::equivalent(requested, to) {
                    return Err(error(
                        "retry identity differs from its recorded pending generation",
                    ));
                }
                return upload_public(api, directory, requested, release, journal);
            }
        }
    }
    let active = active_state(Some(&release), "nightly")?.unwrap();
    verify_owned(api, id, &active, None)?;
    let source = config::text(requested, "source_sha")?;
    let old = config::text(&active, "source_sha")?.to_string();
    if tag_source(api)? != old {
        return Err(error("active Nightly tag differs from its recorded source"));
    }
    if source == old {
        *requested = active;
        return Ok(if completed { "published" } else { "unchanged" }.into());
    }
    if !ancestor(api, &old, source)? {
        return Ok("stale".into());
    }
    let number = active
        .get_u64("publication_number")
        .unwrap_or(1)
        .checked_add(1)
        .ok_or_else(|| error("Nightly publication number overflow"))?;
    config::field(requested, "publication_number", Value::integer(number))?;
    generation_records(requested)?;
    let journal = Value::object([
        ("schema_version", Value::integer(1)),
        ("from", active),
        ("to", requested.clone()),
        ("previous_tag", Value::string(old)),
    ]);
    release = write_active(api, id, journal.get("from").unwrap(), Some(&journal))?;
    upload_public(api, directory, requested, release, journal)
}

fn upload_public(
    api: &mut impl Api,
    directory: &Path,
    requested: &Value,
    release: Value,
    journal: Value,
) -> Result<String> {
    let id = release_id(&release)?;
    let from = journal.get("from").unwrap();
    let records = generation_records(requested)?;
    // Validate every local candidate before the first upload; never clobber a name.
    for (name, record) in &records {
        if !config::equivalent(&file_record(&directory.join(name))?, record) {
            return Err(error(format!("Nightly asset changed before upload: {name}")));
        }
    }
    verify_owned(api, id, from, Some(requested))?;
    let existing = api.assets(id)?;
    for name in records.keys() {
        if !existing.iter().any(|asset| asset.get_str("name") == Some(name.as_str())) {
            // An ambiguous upload is reconciled on retry by digest, never clobber.
            api.upload(&directory.join(name), "nightly")?;
        }
    }
    transition_to_active(api, id, &journal)?;
    Ok("published".into())
}

pub fn promote(api: &mut impl Api, directory: &Path, state: &mut Value) -> Result<String> {
    let source = sha(config::text(state, "source_sha")?)?.to_string();
    let mut release = find_release(api, "nightly")?;
    if let Some(public) = release.as_ref().filter(|r| r.get("draft") == Some(&Value::Bool(false))) {
        return promote_public(api, directory, state, public.clone());
    }
    let mut previous = active_state(release.as_ref(), "nightly")?;
    let legacy = release.as_ref().is_some_and(|r| r.get("draft") == Some(&Value::Bool(true)));
    if let Some(old) = previous.as_ref().filter(|_| !legacy) {
        verify_remote(api, release_id(release.as_ref().unwrap())?, old)?;
        let old_source = config::text(old, "source_sha")?;
        if source != old_source && !ancestor(api, old_source, &source)? {
            return Ok("stale".into());
        }
        set_tag(api, old_source)?;
        if source == old_source {
            return Ok("unchanged".into());
        }
    }
    let mut draft = find_release(api, "nightly-staging")?;
    if legacy {
        if draft.is_some() {
            return Err(error("multiple pending Nightly drafts require reconciliation"));
        }
        draft = release.take();
        previous = None;
    }
    let tag =
        draft.as_ref().and_then(|d| d.get_str("tag_name")).unwrap_or("nightly-staging").to_string();
    let pending = active_state(draft.as_ref(), &tag)?;
    if draft.as_ref().is_some_and(|d| d.get("draft") != Some(&Value::Bool(true))) {
        return Err(error("Nightly staging release must be private"));
    }
    if let Some(pending) = &pending {
        let old = config::text(pending, "source_sha")?;
        if source != old && !ancestor(api, old, &source)? {
            return Ok("stale".into());
        }
    }
    let number = if let Some(previous) = &previous {
        previous.get_u64("publication_number").unwrap_or(1) + 1
    } else {
        pending.as_ref().and_then(|p| p.get_u64("publication_number")).unwrap_or(1)
    };
    config::field(state, "publication_number", Value::integer(number))?;
    let Value::Object(assets) = state.get("assets").ok_or_else(|| error("missing local assets"))?
    else {
        return Err(error("assets must be object"));
    };
    let assets = assets.clone();
    for (name, record) in &assets {
        cases::relative(name)?;
        if !config::equivalent(&file_record(&directory.join(name))?, record) {
            return Err(error(format!("Nightly asset changed before upload: {name}")));
        }
    }
    let mut payload = Value::object([
        ("tag_name", Value::string(&tag)),
        ("target_commitish", Value::string(&source)),
        ("name", Value::string(title(state)?)),
        ("body", Value::string(notes(state)?)),
        ("draft", Value::Bool(true)),
        ("prerelease", Value::Bool(true)),
        ("make_latest", Value::string("false")),
    ]);
    if draft.is_none() {
        draft = api.request("POST", "releases", Some(&payload), false)?;
    } else if pending.as_ref().is_none_or(|pending| !config::equivalent(pending, state)) {
        draft = api.request(
            "PATCH",
            &format!("releases/{}", release_id(draft.as_ref().unwrap())?),
            Some(&payload),
            false,
        )?;
    }
    let id =
        release_id(draft.as_ref().ok_or_else(|| error("missing staged release after mutation"))?)?;
    let existing = api.assets(id)?;
    for (name, record) in &assets {
        if let Some(asset) = existing.iter().find(|a| a.get_str("name") == Some(name)) {
            if asset.get_str("digest")
                != Some(format!("sha256:{}", config::text(record, "sha256")?).as_str())
                || asset.get_u64("size") != record.get_u64("size")
            {
                return Err(error(format!("conflicting immutable Nightly asset: {name}")));
            }
        } else {
            api.upload(&directory.join(name), &tag)?;
        }
    }
    verify_remote(api, id, state)?;
    for asset in existing {
        let name = config::text(&asset, "name")?;
        if !assets.iter().any(|(n, _)| n == name) {
            api.request(
                "DELETE",
                &format!("releases/assets/{}", release_id(&asset)?),
                None,
                false,
            )?;
        }
    }
    let master = api.get("git/ref/heads/master")?;
    let master =
        config::text(master.get("object").ok_or_else(|| error("missing master object"))?, "sha")?;
    if !ancestor(api, &source, master)? {
        return Err(error("source left canonical master history during upload"));
    }
    if let Some(release) = release {
        api.request("DELETE", &format!("releases/{}", release_id(&release)?), None, false)?;
    }
    config::field(&mut payload, "tag_name", Value::string("nightly"))?;
    config::field(&mut payload, "draft", Value::Bool(false))?;
    let mutation = (|| -> Result<()> {
        set_tag(api, &source)?;
        api.request("PATCH", &format!("releases/{id}"), Some(&payload), false)?;
        Ok(())
    })();
    if let Err(error) = mutation {
        let observed = api.get(&format!("releases/{id}"))?;
        if observed.get("draft") == Some(&Value::Bool(true))
            || active_state(Some(&observed), "nightly")?
                .as_ref()
                .is_none_or(|observed| !config::equivalent(observed, state))
        {
            return Err(error);
        }
    }
    let observed = api.get(&format!("releases/{id}"))?;
    if observed.get("draft") != Some(&Value::Bool(false))
        || active_state(Some(&observed), "nightly")?
            .as_ref()
            .is_none_or(|observed| !config::equivalent(observed, state))
    {
        return Err(error("Nightly promotion could not be verified"));
    }
    verify_remote(api, id, state)?;
    let tag = api.get("git/ref/tags/nightly")?;
    if tag.get("object").and_then(|o| o.get_str("sha")) != Some(source.as_str()) {
        return Err(error("Nightly tag differs from active generation"));
    }
    Ok("published".into())
}

fn pages(api: &mut impl Api, endpoint: &str, field: Option<&str>) -> Result<Vec<Value>> {
    let mut result = Vec::new();
    for page in 1..=10000 {
        let separator = if endpoint.contains('?') { '&' } else { '?' };
        let response = api.get(&format!("{endpoint}{separator}per_page=100&page={page}"))?;
        let response = field
            .map_or(Some(&response), |field| response.get(field))
            .ok_or_else(|| error("missing paginated API field"))?;
        let Value::Array(batch) = response else {
            return Err(error("paginated API field must be an array"));
        };
        result.extend_from_slice(batch);
        if batch.len() < 100 {
            return Ok(result);
        }
    }
    Err(error("API pagination limit exceeded"))
}

fn safe_text(text: &str) -> String {
    text.replace(['\r', '\n'], " ").replace('<', "&lt;").replace('>', "&gt;")
}

pub fn contributors(pr: &Value, commits: &[Value]) -> Result<Vec<String>> {
    let mut people = Vec::new();
    let mut seen = BTreeSet::new();
    let mut add = |label: String| {
        if seen.insert(label.trim_start_matches('@').to_lowercase()) {
            people.push(label);
        }
    };
    let mut emails = BTreeMap::new();
    if let Some(login) = pr.get("user").and_then(|u| u.get_str("login")) {
        add(format!("@{login}"));
    }
    for commit in commits {
        let author = commit.get("commit").and_then(|v| v.get("author"));
        let login = commit.get("author").and_then(|a| a.get_str("login"));
        let fallback = author.and_then(|a| a.get_str("name")).unwrap_or("");
        let label = if let Some(login) = login { format!("@{login}") } else { safe_text(fallback) };
        if !label.is_empty() {
            add(label.clone());
            if let Some(email) = author.and_then(|a| a.get_str("email")) {
                emails.insert(email.to_lowercase(), label);
            }
        }
    }
    let coauthor = regex_lite::Regex::new(r"(?mi)^Co-authored-by:\s*(.*?)\s*<([^<>]+)>\s*$")?;
    for commit in commits {
        let message = commit.get("commit").and_then(|c| c.get_str("message")).unwrap_or("");
        for captures in coauthor.captures_iter(message) {
            let label = emails
                .get(&captures[2].to_lowercase())
                .cloned()
                .unwrap_or_else(|| safe_text(&captures[1]));
            if !label.is_empty() {
                add(label);
            }
        }
    }
    Ok(people)
}

pub fn release_notes(
    api: &mut impl Api,
    source: &str,
    release_tag: Option<&str>,
) -> Result<String> {
    sha(source)?;
    if let Some(tag) = release_tag {
        validate_version(
            tag.strip_prefix('v').ok_or_else(|| error("release tag must start with v"))?,
            true,
        )?;
    }
    let mut releases = pages(api, "releases", None)?;
    releases.retain(|r| {
        r.get("draft") != Some(&Value::Bool(true))
            && r.get_str("tag_name").is_some_and(|t| {
                t.starts_with('v')
                    && validate_version(&t[1..], true).is_ok()
                    && Some(t) != release_tag
            })
    });
    releases.sort_by(|a, b| {
        b.get_str("published_at").unwrap_or("").cmp(a.get_str("published_at").unwrap_or(""))
    });
    let mut base = None;
    for release in releases {
        let tag = config::text(&release, "tag_name")?;
        let compare = api.get(&format!("compare/{tag}...{source}?per_page=1"))?;
        if matches!(compare.get_str("status"), Some("ahead" | "identical")) {
            base = Some(tag.to_string());
            break;
        }
    }
    let base = base.ok_or_else(|| error("no published release is an ancestor of this source"))?;
    let commits = pages(api, &format!("compare/{base}...{source}"), Some("commits"))?;
    let included: std::result::Result<BTreeSet<_>, _> =
        commits.iter().map(|c| config::text(c, "sha").map(String::from)).collect();
    let included = included?;
    let mut pulls = BTreeMap::new();
    for commit in commits {
        let sha = config::text(&commit, "sha")?;
        for pr in pages(api, &format!("commits/{sha}/pulls"), None)? {
            if pr.get_str("merged_at").is_some()
                && pr.get("base").and_then(|b| b.get_str("ref")) == Some("master")
                && pr.get_str("merge_commit_sha").is_some_and(|sha| included.contains(sha))
            {
                let number =
                    pr.get_u64("number").ok_or_else(|| error("missing pull request number"))?;
                pulls.insert(number, pr);
            }
        }
    }
    let mut lines = vec!["## What's Changed".to_string(), String::new()];
    for (number, pr) in &pulls {
        let commits = pages(api, &format!("pulls/{number}/commits"), None)?;
        let authors = contributors(pr, &commits)?;
        let credit =
            if authors.is_empty() { String::new() } else { format!(" by {}", authors.join(", ")) };
        lines.push(format!(
            "* {}{credit} in https://github.com/{REPO}/pull/{number}",
            safe_text(config::text(pr, "title")?)
        ));
    }
    if pulls.is_empty() {
        lines.push("* No merged pull requests in this comparison range.".into());
    }
    lines.extend([
        String::new(),
        format!(
            "**Full Changelog**: https://github.com/{REPO}/compare/{base}...{}",
            release_tag.unwrap_or(source)
        ),
        String::new(),
    ]);
    Ok(lines.join("\n"))
}

pub fn release_identity(api: &mut impl Api, version: &str) -> Result<()> {
    validate_version(version, true)?;
    if config::compiler_version()? != version {
        return Err(error("Cargo version differs from requested release version"));
    }
    if std::env::var("GITHUB_ACTIONS").ok().as_deref() == Some("true")
        && std::env::var("GITHUB_REF").ok().as_deref() != Some("refs/heads/master")
    {
        return Err(error("release workflow must run on canonical master"));
    }
    if api.request("GET", &format!("git/ref/tags/v{version}"), None, true)?.is_some() {
        return Err(error("release tag already exists"));
    }
    Ok(())
}

fn canonical_publication() -> Result<()> {
    if std::env::var("GITHUB_REPOSITORY").ok().as_deref() != Some(REPO)
        || std::env::var("GITHUB_ACTIONS").ok().as_deref() != Some("true")
        || std::env::var("GITHUB_REF").ok().as_deref() != Some("refs/heads/master")
    {
        return Err(error("publication requires the canonical master Actions workflow"));
    }
    Ok(())
}

pub fn main(arguments: &[OsString], nightly: bool) -> Result<i32> {
    let mut args = Args::new(arguments);
    let stage = args.text("--stage")?.unwrap_or_else(|| "verify".into());
    let publish = args.flag("--publish");
    let id = args
        .text("--ci-run-id")?
        .or_else(|| std::env::var("WAVE_NIGHTLY_CI_RUN").ok())
        .map(|v| v.parse::<u64>())
        .transpose()?;
    let directory =
        args.path("--directory")?.unwrap_or_else(|| crate::root().join("release-assets"));
    let version = args
        .text("--version")?
        .or_else(|| std::env::var("RELEASE_VERSION").ok())
        .map(Ok)
        .unwrap_or_else(config::compiler_version)?;
    args.finish()?;
    if publish && stage != "publish" {
        return Err(error("--publish requires publication stage"));
    }
    let mut api = GitHub;
    if nightly {
        if std::env::var("GITHUB_REPOSITORY").ok().as_deref() != Some(REPO) {
            return Err(error("Nightly requires canonical repository"));
        }
        let identity = eligible(&mut api, id.ok_or_else(|| error("missing canonical CI run ID"))?)?;
        if stage == "gate" {
            installers_ready()?;
            let output = PathBuf::from(
                std::env::var_os("GITHUB_OUTPUT").ok_or_else(|| error("missing GITHUB_OUTPUT"))?,
            );
            for (name, value) in [
                ("source_sha", identity.source.clone()),
                ("compiler_version", identity.version.clone()),
                ("ci_run_id", identity.ci_run_id.to_string()),
            ] {
                config::append_output(&output, name, &value)?;
            }
            return Ok(0);
        }
        let source = package::run(["git", "rev-parse", "HEAD"])?;
        if source.trim() != identity.source || config::compiler_version()? != identity.version {
            return Err(error("checkout differs from exact CI-validated source/version"));
        }
        if stage == "verify-source" {
            return Ok(0);
        }
        if stage != "publish" || !publish {
            return Err(error("Nightly supports gate, verify-source and explicit publish stages"));
        }
        canonical_publication()?;
        let run = std::env::var("GITHUB_RUN_ID")?.parse::<u64>()?;
        let attempt = std::env::var("GITHUB_RUN_ATTEMPT")?.parse::<u64>()?;
        let own = api.get(&format!("actions/runs/{run}"))?;
        if own.get_str("path") != Some(".github/workflows/nightly.yml")
            || own.get("repository").and_then(|r| r.get_str("full_name")) != Some(REPO)
            || !matches!(own.get_str("event"), Some("workflow_run" | "workflow_dispatch"))
        {
            return Err(error("publication must run in canonical Nightly workflow"));
        }
        installers_ready()?;
        let revision = config::read_json(&crate::root().join("std/manifest.json"))?
            .get_u64("compatibility_revision")
            .ok_or_else(|| error("invalid std revision"))?;
        let temporary = tempfile::tempdir()?;
        let output = temporary.path().join("assets");
        let mut state = nightly_stage(&directory, &output, &identity, run, attempt, revision)?;
        config::field(
            &mut state,
            "changelog",
            Value::string(release_notes(&mut api, &identity.source, None)?),
        )?;
        println!("{}", promote(&mut api, &output, &mut state)?);
        return Ok(0);
    }
    if stage == "verify" || stage == "gate" {
        release_identity(&mut api, &version)?;
        return Ok(0);
    }
    if stage != "publish" || !publish {
        return Err(error("release supports verify/gate and explicit publish stages"));
    }
    canonical_publication()?;
    release_identity(&mut api, &version)?;
    let source = std::env::var("GITHUB_SHA")?;
    sha(&source)?;
    let revision = config::read_json(&crate::root().join("std/manifest.json"))?
        .get_u64("compatibility_revision")
        .ok_or_else(|| error("invalid std revision"))?;
    checks::release_assets(&directory, &version, Some(&source), Some(revision))?;
    let tag = format!("v{version}");
    let notes = release_notes(&mut api, &source, Some(&tag))?;
    let pending = tempfile::tempdir()?;
    let notes_path = pending.path().join("notes.md");
    std::fs::write(&notes_path, notes)?;
    let current = api.get("git/ref/heads/master")?;
    if current.get("object").and_then(|o| o.get_str("sha")) != Some(source.as_str()) {
        return Err(error("canonical master changed before publication"));
    }
    let mut archives: Vec<_> = std::fs::read_dir(&directory)?
        .map(|e| e.map(|e| e.path()))
        .collect::<std::io::Result<_>>()?;
    archives.retain(|p| {
        p.to_string_lossy().ends_with(".zip") || p.to_string_lossy().ends_with(".tar.gz")
    });
    archives.sort();
    let mut command = vec![OsString::from("gh"), "release".into(), "create".into(), tag.into()];
    command.extend(archives.into_iter().map(PathBuf::into_os_string));
    command.extend([
        "--repo".into(),
        REPO.into(),
        "--target".into(),
        source.into(),
        "--title".into(),
        format!("Wave v{version}").into(),
        "--notes-file".into(),
        notes_path.into_os_string(),
    ]);
    let inputs = std::env::var("WAVE_RELEASE_INPUTS").unwrap_or_else(|_| "{}".into());
    let inputs = config::parse_json(&inputs)?;
    for (key, flag) in [("draft", "--draft"), ("prerelease", "--prerelease")] {
        match inputs.get(key) {
            None | Some(Value::Bool(true)) => command.push(flag.into()),
            Some(Value::Bool(false)) => {},
            _ => return Err(error("release draft/prerelease inputs must be booleans")),
        }
    }
    Spec::new(command).checked()?;
    Ok(0)
}
