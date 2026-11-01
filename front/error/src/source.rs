// SPDX-License-Identifier: MPL-2.0
//! Immutable, compilation-owned source snapshots. Rendering never reads files.
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(usize);

#[derive(Debug, Default)]
struct Sources {
    files: Vec<(String, Arc<str>)>,
    ids: HashMap<String, FileId>,
    aliases: HashMap<String, String>,
    inputs: HashMap<String, Arc<str>>,
}
#[derive(Debug, Default)]
pub struct SourceManager(RwLock<Sources>);
thread_local! { static CURRENT: RefCell<Option<Arc<SourceManager>>> = const { RefCell::new(None) }; }

pub struct CompilationSources(Option<Arc<SourceManager>>);
impl CompilationSources {
    pub fn begin() -> Self {
        Self(CURRENT.with(|slot| slot.replace(Some(Arc::new(SourceManager::default())))))
    }
}
impl Drop for CompilationSources {
    fn drop(&mut self) {
        CURRENT.with(|slot| {
            slot.replace(self.0.take());
        });
    }
}
impl SourceManager {
    pub fn current() -> Option<Arc<Self>> {
        CURRENT.with(|slot| slot.borrow().clone())
    }

    pub fn insert(&self, path: String, source: String) -> FileId {
        let mut sources = self.0.write().expect("source manager lock");
        let path = sources.aliases.get(&path).cloned().unwrap_or(path);
        if let Some(id) = sources.ids.get(&path) {
            return *id;
        }
        let id = FileId(sources.files.len());
        sources.ids.insert(path.clone(), id);
        sources.files.push((path, Arc::from(source)));
        id
    }

    pub fn id(&self, path: &str) -> Option<FileId> {
        let sources = self.0.read().expect("source manager lock");
        sources.ids.get(sources.aliases.get(path).map_or(path, String::as_str)).copied()
    }

    pub fn get(&self, id: FileId) -> Option<Arc<str>> {
        self.0.read().expect("source manager lock").files.get(id.0).map(|(_, s)| s.clone())
    }

    /// Called only by source loaders. Cache raw bytes before preprocessing;
    /// renderers use get()/id() and never invoke filesystem operations.
    pub fn read_input(path: &std::path::Path) -> std::io::Result<String> {
        let Some(manager) = Self::current() else {
            return std::fs::read_to_string(path);
        };
        {
            let sources = manager.0.read().expect("source manager lock");
            let display = path.display().to_string();
            let key = sources.aliases.get(&display).unwrap_or(&display);
            if let Some(source) = sources.inputs.get(key) {
                return Ok(source.to_string());
            }
        }
        let canonical =
            path.canonicalize().unwrap_or_else(|_| path.to_path_buf()).display().to_string();
        {
            let mut sources = manager.0.write().expect("source manager lock");
            sources.aliases.insert(path.display().to_string(), canonical.clone());
            if let Some(source) = sources.inputs.get(&canonical) {
                return Ok(source.to_string());
            }
        }
        let source = std::fs::read_to_string(path)?;
        manager
            .0
            .write()
            .expect("source manager lock")
            .inputs
            .insert(canonical, Arc::from(source.clone()));
        Ok(source)
    }

    pub fn record(path: impl Into<String>, source: impl Into<String>) {
        if let Some(manager) = Self::current() {
            manager.insert(path.into(), source.into());
        }
    }

    pub fn snapshot(path: &str) -> Option<String> {
        let manager = Self::current()?;
        manager.get(manager.id(path)?).map(|source| source.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn snapshots_are_immutable_and_owned_by_one_compilation() {
        let weak;
        {
            let _scope = CompilationSources::begin();
            let manager = SourceManager::current().unwrap();
            weak = Arc::downgrade(&manager);
            let first = manager.insert("deleted.wave".into(), "original".into());
            assert_eq!(first, manager.insert("deleted.wave".into(), "changed".into()));
            assert_eq!(&*manager.get(first).unwrap(), "original");
            assert!(manager.id("missing.wave").is_none());
            let second = manager.insert("other.wave".into(), "other".into());
            assert_ne!(first, second);
        }
        assert!(weak.upgrade().is_none());
        assert!(SourceManager::current().is_none());
    }
}

#[cfg(test)]
mod file_tests {
    use super::*;
    #[test]
    fn changed_deleted_and_aliased_files_keep_the_original_snapshot() {
        let _scope = CompilationSources::begin();
        let root = std::env::temp_dir().join(format!(
            "wave-source-{}-{:?}",
            std::process::id(),
            std::thread::current().id()
        ));
        std::fs::create_dir_all(&root).unwrap();
        let path = root.join("input.wave");
        std::fs::write(&path, "original").unwrap();
        let raw = SourceManager::read_input(&path).unwrap();
        SourceManager::record(path.display().to_string(), raw);
        let alias = root.join(".").join("input.wave");
        assert_eq!(SourceManager::read_input(&alias).unwrap(), "original");
        let manager = SourceManager::current().unwrap();
        assert_eq!(
            manager.id(&path.display().to_string()),
            manager.id(&alias.display().to_string())
        );
        std::fs::write(&path, "changed").unwrap();
        assert_eq!(SourceManager::read_input(&path).unwrap(), "original");
        std::fs::remove_file(&path).unwrap();
        assert_eq!(SourceManager::read_input(&alias).unwrap(), "original");
        std::fs::remove_dir(root).unwrap();
    }
}
