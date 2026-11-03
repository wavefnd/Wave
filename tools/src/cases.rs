// SPDX-License-Identifier: MPL-2.0
//! One validated case/target inventory for local selection and Actions matrices.
use crate::{config, error, storage, Result};
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use utils::wson::Value;

const GROUPS: &[&str] =
    &["linux", "macos", "windows", "windows_arm64", "cross", "qemu", "loongarch", "wasm"];
const CAPABILITIES: &[&str] =
    &["stdio", "filesystem", "environment", "clock", "sockets", "process"];

#[derive(Debug, Clone)]
pub struct Target {
    pub id: String,
    pub os: String,
    /// Base ISA, independent of the optional processor profile.
    pub arch: String,
    pub profile: Option<String>,
    pub executor: String,
    pub runner: Option<String>,
    pub group: Option<String>,
    pub triple: Option<String>,
    pub smoke_case: Option<String>,
    pub smoke_stdout: Option<String>,
    pub exclude: Vec<String>,
    pub capabilities: Vec<String>,
    pub enabled: bool,
    pub ci: bool,
}

impl Target {
    pub fn suites(&self) -> Vec<String> {
        let mut suites = vec!["shared".into(), format!("shared/{}", self.arch)];
        if let Some(profile) = &self.profile {
            suites.push(format!("shared/{profile}"));
        }
        suites.push(format!("{}/{}", self.os, self.arch));
        if let Some(profile) = &self.profile {
            suites.push(format!("{}/{profile}", self.os));
        }
        suites
    }

    pub fn rust_target(&self) -> Result<String> {
        if let Some(triple) = &self.triple {
            return Ok(triple.replace("riscv64-unknown-linux", "riscv64gc-unknown-linux"));
        }
        if !self.enabled {
            return Err(error("planned target has no compiler triple"));
        }
        let arch = crate::binary::target_arch(&self.arch);
        let suffix = match self.os.as_str() {
            "linux" => "unknown-linux-gnu",
            "macos" => "apple-darwin",
            "windows" => "pc-windows-msvc",
            "freebsd" => "unknown-freebsd",
            _ => return Err(error("missing explicit target triple")),
        };
        Ok(format!("{arch}-{suffix}"))
    }

    pub fn archive_target(&self) -> Result<String> {
        Ok(self
            .rust_target()?
            .replace("riscv64gc-", "riscv64-")
            .replace("-unknown-linux", "-linux"))
    }

    pub fn value(&self) -> Value {
        Value::object([
            ("id", Value::string(&self.id)),
            ("os", Value::string(&self.os)),
            ("arch", Value::string(&self.arch)),
            ("profile", Value::optional_string(self.profile.as_deref())),
            ("executor", Value::string(&self.executor)),
            ("suite", Value::string(format!(
                "{}/{}",
                self.os,
                self.profile.as_deref().unwrap_or(&self.arch)
            ))),
            ("suites", Value::strings(self.suites())),
            (
                "status",
                Value::string(if !self.enabled {
                    "planned"
                } else if self.executor == "compile" {
                    "compile-only"
                } else {
                    "supported"
                }),
            ),
            ("enabled", Value::Bool(self.enabled)),
            ("ci", Value::Bool(self.ci)),
            ("runner", Value::optional_string(self.runner.as_deref())),
            ("target", Value::optional_string(self.triple.as_deref())),
            ("ci_group", Value::optional_string(self.group.as_deref())),
            ("smoke_case", Value::optional_string(self.smoke_case.as_deref())),
            ("smoke_stdout", Value::optional_string(self.smoke_stdout.as_deref())),
            ("exclude", Value::strings(&self.exclude)),
            ("capabilities", Value::strings(&self.capabilities)),
        ])
    }
}

#[derive(Debug)]
pub struct Manifest {
    pub targets: Vec<Target>,
    pub root: PathBuf,
}

#[derive(Debug)]
struct Profile {
    os: String,
    arch: String,
    enabled: bool,
    ci: bool,
}

fn name(value: &str) -> bool {
    !value.is_empty()
        && value.bytes().enumerate().all(|(i, b)| {
            b.is_ascii_lowercase() || b.is_ascii_digit() || i > 0 && [b'_', b'-'].contains(&b)
        })
}

fn required(table: &toml::map::Map<String, toml::Value>, key: &str) -> Result<String> {
    table
        .get(key)
        .and_then(toml::Value::as_str)
        .filter(|s| !s.is_empty())
        .map(String::from)
        .ok_or_else(|| error(format!("missing nonempty string {key}")))
}

fn optional(table: &toml::map::Map<String, toml::Value>, key: &str) -> Result<Option<String>> {
    if table.contains_key(key) {
        Ok(Some(required(table, key)?))
    } else {
        Ok(None)
    }
}

fn strings(table: &toml::map::Map<String, toml::Value>, key: &str) -> Result<Vec<String>> {
    let Some(value) = table.get(key) else {
        return Ok(Vec::new());
    };
    let mut result = Vec::new();
    for item in value.as_array().ok_or_else(|| error(format!("{key} must be an array")))? {
        let value = item.as_str().ok_or_else(|| error(format!("{key} must contain strings")))?;
        if result.iter().any(|v| v == value) {
            return Err(error(format!("duplicate {key}")));
        }
        result.push(value.to_owned());
    }
    Ok(result)
}

pub fn relative(value: &str) -> Result<PathBuf> {
    let path = Path::new(value);
    if value.is_empty()
        || value.contains(['\\', ':', '\0'])
        || path.is_absolute()
        || path.components().any(|c| !matches!(c, std::path::Component::Normal(_)))
        || path.to_string_lossy() != value
    {
        return Err(error(format!("unsafe relative path: {value:?}")));
    }
    Ok(path.into())
}

fn matrix(value: &toml::Value, key: &str) -> Result<BTreeMap<(String, String), bool>> {
    let mut result = BTreeMap::new();
    let rows = value
        .get(key)
        .and_then(toml::Value::as_table)
        .filter(|t| !t.is_empty())
        .ok_or_else(|| error(format!("missing nonempty [{key}]")))?;
    for (os, arches) in rows {
        if !name(os) {
            return Err(error("invalid OS name"));
        }
        let arches = arches
            .as_table()
            .filter(|t| !t.is_empty())
            .ok_or_else(|| error("empty architecture row"))?;
        for (arch, enabled) in arches {
            if !name(arch)
                || !["amd64", "arm64", "riscv64", "loong64", "wasm32", "wasm64"]
                    .contains(&arch.as_str())
            {
                return Err(error("invalid base architecture name"));
            }
            result.insert(
                (os.clone(), arch.clone()),
                enabled.as_bool().ok_or_else(|| error("support/CI cells must be booleans"))?,
            );
        }
    }
    Ok(result)
}


fn profiles(
    data: &toml::Value,
    supported: &BTreeMap<(String, String), bool>,
) -> Result<BTreeMap<String, Profile>> {
    let table = data
        .get("profiles")
        .and_then(toml::Value::as_table)
        .ok_or_else(|| error("missing [profiles] table"))?;
    let mut result = BTreeMap::new();
    for (profile, fields) in table {
        let base_arch = match profile.as_str() {
            "rhea" => "arm64",
            "shakti" | "xiangshan" | "t-head" => "riscv64",
            _ => return Err(error(format!("unknown CPU profile: {profile}"))),
        };
        let fields =
            fields.as_table().ok_or_else(|| error(format!("invalid CPU profile: {profile}")))?;
        if fields.keys().any(|key| {
            !matches!(key.as_str(), "os" | "arch" | "supported" | "ci")
        }) {
            return Err(error(format!("unknown field in CPU profile: {profile}")));
        }
        let os = required(fields, "os")?;
        let arch = required(fields, "arch")?;
        let enabled = fields
            .get("supported")
            .and_then(toml::Value::as_bool)
            .ok_or_else(|| error(format!("missing boolean support for profile: {profile}")))?;
        let ci = fields
            .get("ci")
            .and_then(toml::Value::as_bool)
            .ok_or_else(|| error(format!("missing boolean CI for profile: {profile}")))?;
        if os != "linux"
            || arch != base_arch
            || !supported.contains_key(&(os.clone(), arch.clone()))
            || enabled && !supported[&(os.clone(), arch.clone())]
            || ci && !enabled
        {
            return Err(error(format!(
                "invalid base architecture or status for profile: {profile}"
            )));
        }
        result.insert(profile.clone(), Profile { os, arch, enabled, ci });
    }
    Ok(result)
}

impl Manifest {
    pub fn load(root: &Path, validate_layout: bool) -> Result<Self> {
        Self::parse(
            &std::fs::read_to_string(root.join("tests/cases/cases.toml"))?,
            root.join("tests/cases"),
            validate_layout,
        )
    }

    pub fn parse(text: &str, root: PathBuf, validate_layout: bool) -> Result<Self> {
        let data: toml::Value = text.parse()?;
        let table = data.as_table().ok_or_else(|| error("manifest must be a table"))?;
        if table
            .keys()
            .any(|key| {
                !matches!(key.as_str(), "version" | "supported" | "ci" | "profiles" | "target")
            })
            || data.get("version").and_then(toml::Value::as_integer) != Some(3)
        {
            return Err(error("unknown manifest fields or unsupported version"));
        }
        let supported = matrix(&data, "supported")?;
        let ci = matrix(&data, "ci")?;
        if supported.keys().ne(ci.keys()) || supported.iter().any(|(k, v)| ci[k] && !*v) {
            return Err(error("support/CI matrices disagree"));
        }
        let profiles = profiles(&data, &supported)?;
        let mut targets = Vec::new();
        let mut configured = BTreeSet::new();
        let mut ids = BTreeSet::new();
        for raw in data
            .get("target")
            .and_then(toml::Value::as_array)
            .ok_or_else(|| error("missing target tables"))?
        {
            let raw = raw.as_table().ok_or_else(|| error("target must be a table"))?;
            if raw.keys().any(|k| {
                !matches!(
                    k.as_str(),
                    "id" | "os"
                        | "arch"
                        | "profile"
                        | "executor"
                        | "runner"
                        | "ci_group"
                        | "target"
                        | "smoke_case"
                        | "smoke_stdout"
                        | "exclude"
                        | "capabilities"
                )
            }) {
                return Err(error("unknown target field"));
            }
            let id = required(raw, "id")?;
            let os = required(raw, "os")?;
            let arch = required(raw, "arch")?;
            let profile = optional(raw, "profile")?;
            let pair = (os.clone(), arch.clone());
            let (enabled, target_ci) = if let Some(profile_name) = &profile {
                let defined = profiles.get(profile_name).ok_or_else(|| {
                    error(format!("unknown CPU profile: {profile_name}"))
                })?;
                if defined.os != os || defined.arch != arch {
                    return Err(error(format!(
                        "profile {profile_name} does not match target {id} base architecture"
                    )));
                }
                (defined.enabled, defined.ci)
            } else {
                (supported.get(&pair) == Some(&true), ci.get(&pair).copied().unwrap_or(false))
            };
            if !name(&id)
                || !ids.insert(id.clone())
                || !configured.insert((os.clone(), arch.clone(), profile.clone()))
                || !enabled
            {
                return Err(error(format!("invalid/duplicate/unsupported target {id}")));
            }
            let executor = required(raw, "executor")?;
            let target = Target {
                id,
                os,
                arch,
                profile,
                executor,
                runner: optional(raw, "runner")?,
                group: optional(raw, "ci_group")?,
                triple: optional(raw, "target")?,
                smoke_case: optional(raw, "smoke_case")?,
                smoke_stdout: optional(raw, "smoke_stdout")?,
                exclude: strings(raw, "exclude")?,
                capabilities: strings(raw, "capabilities")?,
                enabled: true,
                ci: target_ci,
            };
            if !["native", "compile", "qemu", "wasm"].contains(&target.executor.as_str())
                || target.executor != "native" && target.triple.is_none()
            {
                return Err(error("invalid executor or missing triple"));
            }
            if target.ci
                && (target.runner.is_none()
                    || !GROUPS.contains(&target.group.as_deref().unwrap_or("")))
                || !target.ci && (target.runner.is_some() || target.group.is_some())
            {
                return Err(error("invalid CI fields"));
            }
            if target.smoke_case.is_some() != target.smoke_stdout.is_some()
                || target.smoke_case.is_some() && target.executor != "qemu"
            {
                return Err(error("invalid smoke contract"));
            }
            if target.capabilities.iter().any(|c| !CAPABILITIES.contains(&c.as_str())) {
                return Err(error("unknown capability"));
            }
            for selected in target.smoke_case.iter().chain(&target.exclude) {
                let path = relative(selected)?;
                let mut owner = path.parent().unwrap_or(Path::new(""));
                let filename = path.file_name().unwrap().to_str().unwrap();
                let numbered = if filename == "main.wave" {
                    let valid =
                        case_number(owner.file_name().and_then(|v| v.to_str()).unwrap_or(""))
                            .is_some();
                    owner = owner.parent().unwrap_or(Path::new(""));
                    valid
                } else {
                    case_number(filename.trim_end_matches(".wave")).is_some()
                        && filename.ends_with(".wave")
                };
                if !numbered
                    || !target.suites().iter().any(|s| Path::new(s) == owner)
                    || validate_layout && !root.join(&path).is_file()
                {
                    return Err(error(format!(
                        "selected case is outside target suites: {selected}"
                    )));
                }
            }
            targets.push(target);
        }
        for ((os, arch), enabled) in &supported {
            if *enabled && !configured.contains(&(os.clone(), arch.clone(), None)) {
                return Err(error("supported target has no execution table"));
            }
            if !enabled {
                let id = format!("{os}-{arch}");
                if !ids.insert(id.clone()) {
                    return Err(error("duplicate planned target ID"));
                }
                targets.push(Target {
                    id,
                    os: os.clone(),
                    arch: arch.clone(),
                    profile: None,
                    executor: "none".into(),
                    runner: None,
                    group: None,
                    triple: None,
                    smoke_case: None,
                    smoke_stdout: None,
                    exclude: vec![],
                    capabilities: vec![],
                    enabled: false,
                    ci: false,
                });
            }
        }
        for (profile_name, profile) in &profiles {
            if profile.enabled
                && !configured.contains(&(
                    profile.os.clone(),
                    profile.arch.clone(),
                    Some(profile_name.clone()),
                ))
            {
                return Err(error(format!(
                    "supported CPU profile has no execution table: {profile_name}"
                )));
            }
            if !profile.enabled {
                let id = format!("{}-{profile_name}", profile.os);
                if !ids.insert(id.clone()) {
                    return Err(error("duplicate planned CPU profile ID"));
                }
                targets.push(Target {
                    id,
                    os: profile.os.clone(),
                    arch: profile.arch.clone(),
                    profile: Some(profile_name.clone()),
                    executor: "none".into(),
                    runner: None,
                    group: None,
                    triple: None,
                    smoke_case: None,
                    smoke_stdout: None,
                    exclude: vec![],
                    capabilities: vec![],
                    enabled: false,
                    ci: false,
                });
            }
        }
        let manifest = Self { targets, root };
        if validate_layout {
            manifest.validate_layout()?;
        }
        Ok(manifest)
    }

    pub fn target(&self, id: &str) -> Result<&Target> {
        self.targets
            .iter()
            .find(|t| t.id == id)
            .ok_or_else(|| error(format!("unknown case target: {id}")))
    }

    pub fn native(&self) -> Result<&Target> {
        let os = std::env::consts::OS;
        let os = if os == "macos" { "macos" } else { os };
        let arch = normalize_arch(std::env::consts::ARCH);
        let matches: Vec<_> = self
            .targets
            .iter()
            .filter(|t| t.enabled
                    && t.profile.is_none()
                    && t.executor == "native"
                    && t.os == os
                    && t.arch == arch)
            .collect();
        if matches.len() != 1 {
            return Err(error("expected one native target for this host"));
        }
        Ok(matches[0])
    }

    pub fn resolve(&self, id: &str) -> Result<&Target> {
        if let Some(target) = self.targets.iter().find(|target| target.id == id) {
            return Ok(target);
        }
        let targets = self
            .targets
            .iter()
            .filter(|target| {
                target.enabled
                    && (target.triple.as_deref() == Some(id)
                        || target.rust_target().is_ok_and(|triple| triple == id))
            })
            .collect::<Vec<_>>();
        if targets.len() != 1 {
            return Err(error(format!("unknown or ambiguous target: {id}")));
        }
        Ok(targets[0])
    }

    pub fn sources(&self, target: Option<&Target>) -> Result<Vec<PathBuf>> {
        let suites: Vec<_> = match target {
            Some(t) => t.suites(),
            None => self
                .targets
                .iter()
                .flat_map(Target::suites)
                .collect::<BTreeSet<_>>()
                .into_iter()
                .collect(),
        };
        let mut paths = Vec::new();
        for suite in suites {
            let directory = self.root.join(&suite);
            if !directory.is_dir() {
                continue;
            }
            for entry in std::fs::read_dir(&directory)? {
                let path = entry?.path();
                if path.is_file()
                    && path.extension().is_some_and(|v| v == "wave")
                    && case_number(path.file_stem().and_then(|v| v.to_str()).unwrap_or(""))
                        .is_some()
                {
                    paths.push(path);
                } else if path.is_dir()
                    && case_number(path.file_name().and_then(|v| v.to_str()).unwrap_or(""))
                        .is_some()
                    && path.join("main.wave").is_file()
                {
                    paths.push(path.join("main.wave"));
                }
            }
        }
        let suite_order = match target {
            Some(t) => t.suites(),
            None => self
                .targets
                .iter()
                .flat_map(Target::suites)
                .collect::<BTreeSet<_>>()
                .into_iter()
                .collect(),
        };
        paths.sort_by_key(|path| {
            let unit = if path.file_name().unwrap() == "main.wave" {
                path.parent().unwrap()
            } else {
                path.as_path()
            };
            let owner = unit.parent().unwrap().strip_prefix(&self.root).unwrap();
            (
                suite_order.iter().position(|s| owner == Path::new(s)).unwrap_or(usize::MAX),
                case_number(unit.file_stem().unwrap().to_str().unwrap_or("")).unwrap_or(0),
            )
        });
        if let Some(target) = target {
            paths.retain(|p| {
                !target.exclude.iter().any(|v| {
                    self.root.join(v) == *p
                        || p.file_name().is_some_and(|name| name == "main.wave")
                            && self.root.join(v) == p.parent().unwrap()
                })
            });
        }
        Ok(paths)
    }

    pub fn validate_layout(&self) -> Result<()> {
        let suites: BTreeSet<_> = self.targets.iter().flat_map(Target::suites).collect();
        for suite in &suites {
            let directory = self.root.join(suite);
            if !directory.is_dir() {
                return Err(error(format!("configured suite missing: {suite}")));
            }
            let mut numbers = Vec::new();
            for entry in std::fs::read_dir(&directory)? {
                let path = entry?.path();
                if path.is_file() && path.extension().is_some_and(|v| v == "wave") {
                    if let Some(n) = case_number(path.file_stem().unwrap().to_str().unwrap_or("")) {
                        numbers.push(n);
                    }
                } else if path.is_dir() && path.join("main.wave").is_file() {
                    if let Some(n) = case_number(path.file_name().unwrap().to_str().unwrap_or("")) {
                        numbers.push(n);
                    }
                }
            }
            numbers.sort();
            if numbers.len() < 10 || numbers.iter().copied().ne(1..=numbers.len()) {
                return Err(error(format!(
                    "suite must have at least ten contiguous numbered cases: {suite}"
                )));
            }
        }
        for path in storage::files(&self.root)? {
            if path.extension().is_some_and(|v| v == "wave") {
                let relative = path.strip_prefix(&self.root)?;
                let mut owner = relative.parent().unwrap_or(Path::new(""));
                let accepted = suites.iter().any(|s| owner == Path::new(s)) || {
                    let mut found = false;
                    while let Some(parent) = owner.parent() {
                        if case_number(owner.file_name().and_then(|v| v.to_str()).unwrap_or(""))
                            .is_some()
                        {
                            let full = self.root.join(owner).join("main.wave");
                            if full.is_file() && suites.iter().any(|s| parent == Path::new(s)) {
                                found = true;
                                break;
                            }
                        }
                        owner = parent;
                    }
                    found
                };
                if !accepted {
                    return Err(error(format!(
                        "unconfigured case directory: {}",
                        relative.display()
                    )));
                }
                for line in std::fs::read_to_string(&path)?.lines() {
                    let line = line.trim();
                    if !line.is_empty() && !line.starts_with("//") {
                        break;
                    }
                    if line.starts_with("// wave-test:")
                        && (line.contains("host-os=") || line.contains("host-arch="))
                    {
                        return Err(error("legacy platform metadata"));
                    }
                }
            }
        }
        Ok(())
    }

    pub fn workflow_matrices(&self) -> Result<Value> {
        let mut matrices = self.matrices()?;
        let cross = config::array(
            matrices.get("cross").ok_or_else(|| error("missing cross matrix"))?,
            "include",
        )?
        .to_vec();
        for group in ["freebsd", "freestanding"] {
            config::field(
                &mut matrices,
                group,
                Value::object([(
                    "include",
                    Value::Array(
                        cross
                            .iter()
                            .filter(|entry| entry.get_str("os") == Some(group))
                            .cloned()
                            .collect(),
                    ),
                )]),
            )?;
        }
        Ok(matrices)
    }

    pub fn matrices(&self) -> Result<Value> {
        let mut groups: BTreeMap<&str, Vec<Value>> =
            GROUPS.iter().map(|g| (*g, Vec::new())).collect();
        for target in self.targets.iter().filter(|t| t.ci) {
            for index in 0..2 {
                let mut value = Value::object([
                    ("id", Value::string(&target.id)),
                    ("os", Value::string(&target.os)),
                    ("arch", Value::string(&target.arch)),
                    ("runner", Value::string(target.runner.as_deref().unwrap())),
                    ("shard_index", Value::integer(index)),
                    ("shard_count", Value::integer(2)),
                ]);
                if let Some(profile) = &target.profile {
                    config::field(&mut value, "profile", Value::string(profile))?;
                }
                for (key, item) in [
                    ("target", &target.triple),
                    ("smoke_case", &target.smoke_case),
                    ("smoke_stdout", &target.smoke_stdout),
                ] {
                    if let Some(item) = item {
                        config::field(&mut value, key, Value::string(item))?;
                    }
                }
                groups.get_mut(target.group.as_deref().unwrap()).unwrap().push(value);
            }
        }
        Ok(Value::object(
            groups
                .into_iter()
                .map(|(key, items)| (key, Value::object([("include", Value::Array(items))]))),
        ))
    }
}

pub fn normalize_arch(arch: &str) -> String {
    match arch.to_lowercase().as_str() {
        "x86_64" | "amd64" => "amd64".into(),
        "aarch64" | "arm64" => "arm64".into(),
        "loongarch64" | "loong64" => "loong64".into(),
        value => value.into(),
    }
}

fn case_number(stem: &str) -> Option<usize> {
    let digits = stem.strip_prefix("test")?;
    if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
        None
    } else {
        digits.parse().ok()
    }
}

pub fn main(arguments: &[std::ffi::OsString]) -> Result<i32> {
    let mut args = crate::cli::Args::new(arguments);
    let output = args.path("--github-output")?;
    let json = args.flag("--json");
    let validate = !args.flag("--no-layout");
    args.finish()?;
    let manifest = Manifest::load(&crate::root(), validate)?;
    let matrices = manifest.matrices()?;
    if let Some(output) = output {
        let Value::Object(groups) = manifest.workflow_matrices()? else { unreachable!() };
        for (key, value) in groups {
            config::append_output(
                &output,
                &key,
                &utils::wson::dumps(&value, utils::wson::Format::Json, false)?,
            )?;
        }
    }
    if json {
        print!("{}", config::json(&matrices)?);
    } else {
        println!("Validated {} target contracts", manifest.targets.len());
    }
    Ok(0)
}
