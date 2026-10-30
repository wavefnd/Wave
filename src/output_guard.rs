// This file is part of the Wave language project.
// SPDX-License-Identifier: MPL-2.0

//! Protect compiler inputs and distinct artifact destinations before any write.
use crate::errors::CliError;
use std::fs;
use std::path::{Component, Path, PathBuf};

#[derive(Clone, Copy, PartialEq, Eq)]
enum Publication {
    Direct,
    Atomic,
    StagedLink,
}

pub(crate) struct PlannedOutput<'a> {
    path: PathBuf,
    passthrough: Option<&'a Path>,
    publication: Publication,
}

impl<'a> PlannedOutput<'a> {
    pub(crate) fn atomic(path: PathBuf) -> Self {
        Self { path, passthrough: None, publication: Publication::Atomic }
    }

    pub(crate) fn direct(path: PathBuf, passthrough: Option<&'a Path>) -> Self {
        Self { path, passthrough, publication: Publication::Direct }
    }

    pub(crate) fn staged_link(path: PathBuf) -> Self {
        Self { path, passthrough: None, publication: Publication::StagedLink }
    }
}

pub(crate) fn validate_plan(
    inputs: &[PathBuf],
    outputs: &[PlannedOutput<'_>],
) -> Result<(), CliError> {
    let mut destinations: Vec<(&PlannedOutput<'_>, PathBuf, PathBuf)> = Vec::new();
    for output in outputs {
        if let Some(source) = output.passthrough {
            if same_file(source, &output.path)? {
                continue;
            }
        }
        // MSVC staging canonicalizes only the parent and retains the original
        // basename, including literal trailing dots/spaces in a verbatim path.
        // Other writers pass the complete destination through Win32 normalization.
        let destination = match output.publication {
            Publication::StagedLink => output.path.clone(),
            _ => std::path::absolute(&output.path)?,
        };
        let entry = destination_entry(&destination)?;
        let path = match output.publication {
            Publication::Direct => resolve_destination(&std::path::absolute(&output.path)?, 64)?,
            Publication::Atomic | Publication::StagedLink => entry.clone(),
        };
        for (previous, previous_path, previous_entry) in &destinations {
            // Atomic publication replaces a directory entry, separating distinct
            // hard links. Direct writers follow symlinks and retain inode aliases.
            let direct_alias = output.publication == Publication::Direct
                && previous.publication == Publication::Direct
                && path.exists()
                && previous_path.exists()
                && same_file(previous_path, &path)?;
            if same_destination_path(&entry, previous_entry)?
                || same_destination_path(&path, previous_path)?
                || direct_alias
            {
                return Err(CliError::usage(format!(
                    "output '{}' overlaps another planned artifact '{}'; choose separate output paths",
                    output.path.display(),
                    previous.path.display()
                )));
            }
        }
        destinations.push((output, path, entry));
    }
    let outputs = outputs.iter().map(|out| (out.path.clone(), out.passthrough)).collect::<Vec<_>>();
    validate(inputs, &outputs)
}

#[cfg(not(windows))]
fn same_destination_path(left: &Path, right: &Path) -> Result<bool, CliError> {
    Ok(left == right)
}

#[cfg(windows)]
fn same_destination_path(left: &Path, right: &Path) -> Result<bool, CliError> {
    let mut parent = PathBuf::new();
    let mut right = right.components();
    for component in left.components() {
        let Some(other) = right.next() else {
            return Ok(false);
        };
        let same = match (component, other) {
            (Component::Normal(left), Component::Normal(right)) if left != right => {
                windows_ordinal_equal(left, right)? && !windows_case_sensitive(&parent)?
            },
            (Component::Prefix(left), Component::Prefix(right)) => {
                windows_ordinal_equal(left.as_os_str(), right.as_os_str())?
            },
            _ => component == other,
        };
        if !same {
            return Ok(false);
        }
        parent.push(component.as_os_str());
    }
    Ok(right.next().is_none())
}

#[cfg(windows)]
fn windows_ordinal_equal(left: &std::ffi::OsStr, right: &std::ffi::OsStr) -> std::io::Result<bool> {
    use std::os::windows::ffi::OsStrExt;
    #[link(name = "kernel32")]
    extern "system" {
        fn CompareStringOrdinal(
            first: *const u16,
            first_len: i32,
            second: *const u16,
            second_len: i32,
            ignore_case: i32,
        ) -> i32;
    }
    let left = left.encode_wide().collect::<Vec<_>>();
    let right = right.encode_wide().collect::<Vec<_>>();
    let length = |text: &[u16]| {
        i32::try_from(text.len()).map_err(|_| {
            std::io::Error::new(std::io::ErrorKind::InvalidInput, "output component is too long")
        })
    };
    // CompareStringOrdinal uses Windows' Unicode uppercase table, not Rust's
    // Unicode case folding. Its documented TRUE value is exactly 1.
    let comparison = unsafe {
        CompareStringOrdinal(left.as_ptr(), length(&left)?, right.as_ptr(), length(&right)?, 1)
    };
    if comparison == 0 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(comparison == 2)
    }
}

#[cfg(windows)]
fn windows_case_sensitive(directory: &Path) -> std::io::Result<bool> {
    use std::os::windows::fs::OpenOptionsExt;
    use std::os::windows::io::AsRawHandle;
    #[link(name = "kernel32")]
    extern "system" {
        fn GetFileInformationByHandleEx(
            handle: *mut std::ffi::c_void,
            information_class: i32,
            information: *mut std::ffi::c_void,
            size: u32,
        ) -> i32;
    }
    let mut parent = directory;
    let handle = loop {
        match fs::OpenOptions::new()
            .read(true)
            .access_mode(0x80) // FILE_READ_ATTRIBUTES
            .share_mode(7) // FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE
            .custom_flags(0x02000000) // FILE_FLAG_BACKUP_SEMANTICS permits a directory handle.
            .open(parent)
        {
            Ok(handle) => break handle,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                parent = parent.parent().ok_or(error)?;
            },
            Err(error) => return Err(error),
        }
    };
    // FileCaseSensitiveInfo is class 23 and returns one ULONG Flags field.
    // New directories inherit this setting from the nearest existing parent.
    let mut flags = 0u32;
    let success = unsafe {
        GetFileInformationByHandleEx(
            handle.as_raw_handle(),
            23,
            std::ptr::addr_of_mut!(flags).cast(),
            std::mem::size_of_val(&flags) as u32,
        )
    };
    if success != 0 {
        return Ok(flags & 1 != 0); // FILE_CS_FLAG_CASE_SENSITIVE_DIR
    }
    let error = std::io::Error::last_os_error();
    match error.raw_os_error() {
        // Older Windows/filesystem drivers cannot expose the per-directory
        // flag and use the historical case-insensitive behavior.
        Some(1 | 50 | 87) => Ok(false),
        _ => Err(error),
    }
}

fn destination_entry(output: &Path) -> Result<PathBuf, CliError> {
    let filename = output.file_name().ok_or_else(|| {
        CliError::usage(format!("output '{}' must name a file", output.display()))
    })?;
    let parent =
        output.parent().filter(|path| !path.as_os_str().is_empty()).unwrap_or(Path::new("."));
    let path = resolve_destination(&std::path::absolute(parent)?, 64)?.join(filename);
    match fs::symlink_metadata(&path) {
        // Canonical spelling also handles existing case aliases on Windows,
        // while a final symlink is replaced rather than followed.
        Ok(metadata) if !metadata.file_type().is_symlink() => Ok(fs::canonicalize(path)?),
        Ok(_) => Ok(path),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(path),
        Err(error) => Err(error.into()),
    }
}

fn resolve_destination(path: &Path, remaining_links: usize) -> Result<PathBuf, CliError> {
    let mut resolved = PathBuf::new();
    for component in path.components() {
        match component {
            Component::Prefix(_) => resolved.push(component.as_os_str()),
            Component::RootDir => {
                resolved.push(component.as_os_str());
                resolved = fs::canonicalize(resolved)?;
            },
            Component::CurDir => {},
            Component::ParentDir => {
                resolved.pop();
            },
            Component::Normal(name) => {
                resolved.push(name);
                match fs::canonicalize(&resolved) {
                    Ok(path) => resolved = path,
                    Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                        // A dangling final symlink still redirects a direct write.
                        // Resolve it before appending nonexistent path components.
                        match fs::symlink_metadata(&resolved) {
                            Ok(metadata) if metadata.file_type().is_symlink() => {
                                if remaining_links == 0 {
                                    return Err(CliError::usage(format!(
                                        "too many symbolic links in output '{}'",
                                        path.display()
                                    )));
                                }
                                let target = fs::read_link(&resolved)?;
                                let target = if target.is_absolute() {
                                    target
                                } else {
                                    resolved.parent().unwrap().join(target)
                                };
                                resolved = resolve_destination(&target, remaining_links - 1)?;
                            },
                            Ok(_) => {},
                            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {},
                            Err(error) => return Err(error.into()),
                        }
                    },
                    Err(error) => return Err(error.into()),
                }
            },
        }
    }
    Ok(resolved)
}

pub(crate) fn same_file(source: &Path, output: &Path) -> Result<bool, CliError> {
    let source_path = fs::canonicalize(source)?;
    let output_path = match fs::canonicalize(output) {
        Ok(path) => path,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(false),
        Err(error) => return Err(error.into()),
    };
    if source_path == output_path {
        return Ok(true);
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::MetadataExt;
        let a = fs::metadata(source_path)?;
        let b = fs::metadata(output_path)?;
        Ok(a.dev() == b.dev() && a.ino() == b.ino())
    }
    #[cfg(windows)]
    {
        Ok(windows_identity(&source_path)? == windows_identity(&output_path)?)
    }
    #[cfg(not(any(unix, windows)))]
    {
        Ok(false)
    }
}

pub(crate) fn validate(
    inputs: &[PathBuf],
    outputs: &[(PathBuf, Option<&Path>)],
) -> Result<(), CliError> {
    for (output, passthrough) in outputs {
        // An identity copy is a no-op, including symlink and hard-link aliases.
        if let Some(source) = passthrough {
            if same_file(source, output)? {
                continue;
            }
        }
        for input in inputs {
            if same_file(input, output)? {
                return Err(CliError::usage(format!(
                    "output '{}' aliases compiler input '{}'; choose a separate output path",
                    output.display(),
                    input.display()
                )));
            }
        }
    }
    Ok(())
}

#[cfg(windows)]
fn windows_identity(path: &Path) -> std::io::Result<(u32, [u32; 2])> {
    let info = crate::windows_fs::information(&fs::File::open(path)?)?;
    Ok((info.volume, info.index))
}
