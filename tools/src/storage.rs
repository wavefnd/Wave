// SPDX-License-Identifier: MPL-2.0
use crate::{error, Result};
use sha2::{Digest, Sha256};
use std::fs::{self, File};
use std::io::{Read, Write};
use std::path::{Component, Path, PathBuf};

pub fn digest(path: &Path) -> Result<String> {
    let mut file = File::open(path)?;
    let mut digest = Sha256::new();
    let mut buffer = [0; 64 * 1024];
    loop {
        let count = file.read(&mut buffer)?;
        if count == 0 {
            break;
        }
        digest.update(&buffer[..count]);
    }
    Ok(format!("{:x}", digest.finalize()))
}

pub fn bytes_digest(bytes: &[u8]) -> String {
    format!("{:x}", Sha256::digest(bytes))
}

pub fn exists(path: &Path) -> bool {
    fs::symlink_metadata(path).is_ok()
}

pub fn remove(path: &Path) -> Result<()> {
    if let Ok(metadata) = fs::symlink_metadata(path) {
        if metadata.is_dir() && !metadata.is_symlink() {
            fs::remove_dir_all(path)?;
        } else {
            fs::remove_file(path)?;
        }
    }
    Ok(())
}

/// Resolve existing components but preserve the spelling of a missing final output.
pub fn resolved(path: &Path) -> Result<PathBuf> {
    let path =
        if path.is_absolute() { path.to_owned() } else { std::env::current_dir()?.join(path) };
    let mut result = PathBuf::new();
    for part in path.components() {
        match part {
            Component::CurDir => {},
            Component::ParentDir => {
                result.pop();
            },
            _ => {
                result.push(part.as_os_str());
            },
        }
        if exists(&result) {
            result = fs::canonicalize(&result)?;
        }
    }
    Ok(result)
}

pub fn validate_output(output: &Path, inputs: &[PathBuf]) -> Result<()> {
    let destination = resolved(output)?;
    for input in inputs {
        if destination == resolved(input)? || same_file(output, input)? {
            return Err(error(format!(
                "output aliases an input: {} ({})",
                output.display(),
                input.display()
            )));
        }
    }
    Ok(())
}

fn same_file(a: &Path, b: &Path) -> Result<bool> {
    let (Ok(first), Ok(second)) = (fs::metadata(a), fs::metadata(b)) else {
        return Ok(false);
    };
    #[cfg(unix)]
    {
        use std::os::unix::fs::MetadataExt;
        Ok(first.dev() == second.dev() && first.ino() == second.ino())
    }
    #[cfg(windows)]
    {
        use std::os::windows::io::AsRawHandle;
        use windows_sys::Win32::Storage::FileSystem::{
            GetFileInformationByHandle, BY_HANDLE_FILE_INFORMATION,
        };
        let _ = (first, second);
        let identity = |path: &Path| -> Result<(u32, u32, u32)> {
            let file = File::open(path)?;
            let mut info: BY_HANDLE_FILE_INFORMATION = unsafe { std::mem::zeroed() };
            if unsafe { GetFileInformationByHandle(file.as_raw_handle(), &mut info) } == 0 {
                return Err(std::io::Error::last_os_error().into());
            }
            Ok((info.dwVolumeSerialNumber, info.nFileIndexHigh, info.nFileIndexLow))
        };
        Ok(identity(a)? == identity(b)?)
    }
    #[cfg(not(any(unix, windows)))]
    {
        let _ = (first, second);
        Ok(false)
    }
}

pub fn atomic_write(path: &Path, bytes: &[u8]) -> Result<()> {
    let parent = path.parent().filter(|p| !p.as_os_str().is_empty()).unwrap_or(Path::new("."));
    fs::create_dir_all(parent)?;
    let mut file = tempfile::NamedTempFile::new_in(parent)?;
    file.write_all(bytes)?;
    file.as_file().sync_all()?;
    file.persist(path).map_err(|e| e.error)?;
    Ok(())
}

pub fn files(root: &Path) -> Result<Vec<PathBuf>> {
    fn visit(root: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
        for entry in fs::read_dir(root)? {
            let entry = entry?;
            let kind = entry.file_type()?;
            if kind.is_symlink() {
                out.push(entry.path());
            } else if kind.is_dir() {
                visit(&entry.path(), out)?;
            } else if kind.is_file() {
                out.push(entry.path());
            } else {
                return Err(error(format!(
                    "unsupported filesystem entry: {}",
                    entry.path().display()
                )));
            }
        }
        Ok(())
    }
    let mut result = Vec::new();
    if root.is_dir() {
        visit(root, &mut result)?;
    }
    result.sort();
    Ok(result)
}

pub fn copy_tree(source: &Path, destination: &Path) -> Result<()> {
    let meta = fs::symlink_metadata(source)?;
    if meta.is_symlink() {
        return Err(error(format!("symlink not allowed in staged runtime: {}", source.display())));
    }
    if meta.is_dir() {
        fs::create_dir_all(destination)?;
        for entry in fs::read_dir(source)? {
            let entry = entry?;
            copy_tree(&entry.path(), &destination.join(entry.file_name()))?;
        }
    } else if meta.is_file() {
        if let Some(parent) = destination.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::copy(source, destination)?;
    } else {
        return Err(error(format!("not a regular file: {}", source.display())));
    }
    Ok(())
}

/// Backups are deliberately not a TempDir: failed recovery must retain the only old bytes.
pub fn replace_set(entries: &[(PathBuf, PathBuf)]) -> Result<()> {
    replace_set_with(entries, |a, b| fs::rename(a, b))
}

pub fn replace_set_with<F>(entries: &[(PathBuf, PathBuf)], mut rename: F) -> Result<()>
where
    F: FnMut(&Path, &Path) -> std::io::Result<()>,
{
    let first = entries.first().ok_or_else(|| error("empty replacement transaction"))?;
    let mut names = std::collections::BTreeSet::new();
    let mut sources = std::collections::BTreeSet::new();
    for (source, destination) in entries {
        if !exists(source)
            || !sources.insert(resolved(source)?)
            || !names.insert(resolved(destination)?)
        {
            return Err(error("missing candidate or duplicate transaction path"));
        }
    }
    if sources.iter().any(|source| names.contains(source)) {
        return Err(error("transaction input aliases a destination"));
    }
    let parent = first.1.parent().unwrap_or(Path::new("."));
    let backup = tempfile::Builder::new().prefix(".wave-recovery-").tempdir_in(parent)?.keep();
    let mut previous = Vec::new();
    let mut published = Vec::new();
    let activation = (|| -> Result<()> {
        for (index, (source, destination)) in entries.iter().enumerate() {
            if exists(destination) {
                let saved = backup.join(index.to_string());
                rename(destination, &saved)?;
                previous.push((saved, destination.clone()));
            }
            rename(source, destination)?;
            published.push(destination.clone());
        }
        Ok(())
    })();
    if let Err(error) = activation {
        let mut failures = Vec::new();
        for destination in published.iter().rev() {
            if let Err(e) = remove(destination) {
                failures.push(format!("remove {}: {e}", destination.display()));
            }
        }
        for (saved, destination) in previous.iter().rev() {
            if let Err(e) = rename(saved, destination) {
                failures.push(format!("restore {}: {e}", destination.display()));
            }
        }
        if failures.is_empty() {
            fs::remove_dir_all(&backup)?;
            return Err(error);
        }
        return Err(crate::error(format!(
            "activation failed: {error}; recovery failed: {}; retained backups: {}",
            failures.join("; "),
            backup.display()
        )));
    }
    if let Err(e) = fs::remove_dir_all(&backup) {
        eprintln!(
            "warning: replacement succeeded; previous backup retained at {}: {e}",
            backup.display()
        );
    }
    Ok(())
}
