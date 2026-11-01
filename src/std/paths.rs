// SPDX-License-Identifier: MPL-2.0
//! Directory replacement detection without following an activation-time alias.
#[cfg(unix)]
pub(super) fn identity(path: &std::path::Path) -> std::io::Result<(u64, u64)> {
    use std::os::unix::fs::MetadataExt;
    let metadata = std::fs::symlink_metadata(path)?;
    if !metadata.is_dir() || metadata.file_type().is_symlink() {
        return Err(std::io::Error::other("installation parent is not a regular directory"));
    }
    Ok((metadata.dev(), metadata.ino()))
}

#[cfg(windows)]
pub(super) fn identity(path: &std::path::Path) -> std::io::Result<(u64, u64)> {
    use std::os::windows::fs::OpenOptionsExt;
    let file =
        std::fs::OpenOptions::new().read(true).custom_flags(0x02000000 | 0x00200000).open(path)?;
    let info = crate::windows_fs::information(&file)?;
    if info.attributes & 0x10 == 0 || info.attributes & 0x400 != 0 {
        return Err(std::io::Error::other(
            "installation parent is a reparse point or not a directory",
        ));
    }
    Ok((u64::from(info.volume), (u64::from(info.index[0]) << 32) | u64::from(info.index[1])))
}

#[cfg(not(any(unix, windows)))]
pub(super) fn identity(_path: &std::path::Path) -> std::io::Result<(u64, u64)> {
    Err(std::io::Error::new(
        std::io::ErrorKind::Unsupported,
        "std installation is unsupported on this compiler host",
    ))
}
