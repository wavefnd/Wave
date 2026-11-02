// SPDX-License-Identifier: MPL-2.0
//! Bounded package identities and logical path names for build-system callers.
use crate::wson::{self, Value};
use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

#[derive(Clone, Debug)]
pub struct Package {
    pub name: String,
    pub root: PathBuf,
    pub dependencies: BTreeMap<String, String>,
}

#[derive(Clone, Debug)]
pub struct PackageGraph {
    pub root: String,
    pub packages: BTreeMap<String, Package>,
    document: Value,
}

fn fields(value: &Value, allowed: &[&str]) -> Result<(), String> {
    let Value::Object(entries) = value else {
        return Err("expected an object".into());
    };
    let mut seen = BTreeSet::new();
    if entries.iter().any(|(key, _)| !allowed.contains(&key.as_str()) || !seen.insert(key)) {
        return Err("unknown build-contract field".into());
    }
    Ok(())
}

fn identifier(value: &str) -> bool {
    let mut bytes = value.bytes();
    bytes.next().is_some_and(|b| b.is_ascii_alphabetic() || b == b'_')
        && bytes.all(|b| b.is_ascii_alphanumeric() || b == b'_')
}

impl PackageGraph {
    pub fn parse(raw: &str) -> Result<Self, String> {
        if raw.len() > 8 * 1024 * 1024 {
            return Err("package graph exceeds 8 MiB".into());
        }
        let value = wson::parse_json(raw).map_err(|e| e.to_string())?;
        fields(&value, &["schema_version", "root", "packages"])?;
        if value.get_u64("schema_version") != Some(1) {
            return Err("unsupported package graph schema".into());
        }
        let root = value.get_str("root").ok_or("missing root package ID")?.to_owned();
        let entries = value.get_arr("packages").ok_or("missing packages array")?;
        if entries.is_empty() || entries.len() > 4096 {
            return Err("package graph requires 1..4096 packages".into());
        }
        let mut packages = BTreeMap::new();
        let mut physical = BTreeSet::new();
        let mut edge_count = 0;
        for entry in entries {
            fields(entry, &["id", "name", "root", "dependencies"])?;
            let name = entry
                .get_str("name")
                .filter(|name| identifier(name))
                .ok_or("invalid declared package name")?
                .to_owned();
            let id = entry.get_str("id").ok_or("missing package ID")?;
            if id.len() != 64
                || !id.bytes().all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
            {
                return Err("package ID must be lowercase SHA-256".into());
            }
            let requested = Path::new(entry.get_str("root").ok_or("missing package root")?);
            if !requested.is_absolute() {
                return Err("package root must be absolute".into());
            }
            let directory = requested
                .canonicalize()
                .map_err(|e| format!("package root is unavailable: {e}"))?;
            if !directory.is_dir() || !physical.insert(directory.clone()) {
                return Err("duplicate or invalid physical package root".into());
            }
            let edges = entry.get("dependencies").ok_or("missing dependencies")?;
            let Value::Object(edges) = edges else {
                return Err("dependencies must be an alias-to-ID object".into());
            };
            let mut dependencies = BTreeMap::new();
            for (alias, target) in edges {
                if !identifier(alias) || alias == "std" {
                    return Err("invalid or reserved import alias".into());
                }
                let Value::String(target) = target else {
                    return Err("dependency ID must be a string".into());
                };
                if dependencies.insert(alias.clone(), target.clone()).is_some() {
                    return Err("duplicate import alias in package graph".into());
                }
                edge_count += 1;
                if edge_count > 65536 {
                    return Err("package graph edge limit exceeded".into());
                }
            }
            if packages
                .insert(id.to_owned(), Package { name, root: directory, dependencies })
                .is_some()
            {
                return Err("duplicate package ID".into());
            }
        }
        if !packages.contains_key(&root) {
            return Err("missing root package node".into());
        }
        for package in packages.values() {
            if package.dependencies.values().any(|id| !packages.contains_key(id)) {
                return Err("dangling package graph edge".into());
            }
        }
        let mut complete = BTreeSet::new();
        for start in packages.keys() {
            if complete.contains(start) {
                continue;
            }
            let mut active = BTreeSet::new();
            let mut pending = vec![(start.clone(), false, 1usize)];
            while let Some((id, exit, depth)) = pending.pop() {
                if exit {
                    active.remove(&id);
                    complete.insert(id);
                    continue;
                }
                if complete.contains(&id) {
                    continue;
                }
                if depth > 512 || !active.insert(id.clone()) {
                    return Err("cyclic or excessively deep package graph".into());
                }
                pending.push((id.clone(), true, depth));
                for child in packages[&id].dependencies.values().rev() {
                    pending.push((child.clone(), false, depth + 1));
                }
            }
        }
        Ok(Self { root, packages, document: value })
    }

    pub fn validate_entry(&self, path: &Path) -> Result<(), String> {
        let path = path.canonicalize().map_err(|e| format!("package input is unavailable: {e}"))?;
        let owner = self
            .packages
            .iter()
            .filter(|(_, p)| path.starts_with(&p.root))
            .max_by_key(|(_, p)| p.root.components().count());
        if owner.is_none_or(|(id, _)| id != &self.root) {
            return Err("Wave source input does not belong to the graph root package".into());
        }
        Ok(())
    }

    pub fn resolve(&self, base: &Path, alias: &str) -> Option<PathBuf> {
        let base = base.canonicalize().ok()?;
        let owner = self
            .packages
            .values()
            .filter(|p| base.starts_with(&p.root))
            .max_by_key(|p| p.root.components().count())?;
        if let Some(target) = owner.dependencies.get(alias) {
            return Some(self.packages.get(target)?.root.clone());
        }
        (alias == owner.name).then(|| owner.root.clone())
    }

    pub fn value(&self) -> Value {
        self.document.clone()
    }

    pub fn logical_roots(&self) -> Vec<(PathBuf, String)> {
        self.packages
            .iter()
            .map(|(id, node)| (node.root.clone(), format!("/vex/packages/{id}")))
            .collect()
    }
}

thread_local! {
    static PATHS: RefCell<Vec<(PathBuf, String)>> = const { RefCell::new(Vec::new()) };
}

pub fn configure_paths(mut roots: Vec<(PathBuf, String)>) -> Result<(), String> {
    roots.sort_by(|a, b| {
        b.0.components().count().cmp(&a.0.components().count()).then(a.0.cmp(&b.0))
    });
    let mut seen = BTreeMap::new();
    for (real, logical) in &roots {
        if !real.is_absolute()
            || !logical.starts_with('/')
            || logical.contains(['\\', '\n', '\r'])
            || logical.split('/').any(|part| matches!(part, "." | ".."))
        {
            return Err("invalid logical path prefix".into());
        }
        if seen.insert(real, logical).is_some_and(|old| old != logical) {
            return Err("conflicting path prefixes".into());
        }
    }
    PATHS.with(|paths| *paths.borrow_mut() = roots);
    Ok(())
}

pub fn logical_path(path: &Path) -> String {
    PATHS.with(|paths| {
        for (real, logical) in paths.borrow().iter() {
            if let Ok(relative) = path.strip_prefix(real) {
                return format!("{logical}/{}", relative.to_string_lossy().replace('\\', "/"));
            }
        }
        path.to_string_lossy().into_owned()
    })
}
