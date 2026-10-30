// SPDX-License-Identifier: MPL-2.0
//! Shared Win32 file information for output and installation identity checks.

use std::fs::File;
use std::os::windows::io::AsRawHandle;

#[repr(C)]
pub(crate) struct FileInformation {
    pub(crate) attributes: u32,
    times: [u32; 6],
    pub(crate) volume: u32,
    size: [u32; 2],
    links: u32,
    pub(crate) index: [u32; 2],
}

#[link(name = "kernel32")]
unsafe extern "system" {
    fn GetFileInformationByHandle(
        handle: *mut std::ffi::c_void,
        information: *mut FileInformation,
    ) -> i32;
}

pub(crate) fn information(file: &File) -> std::io::Result<FileInformation> {
    let mut information = std::mem::MaybeUninit::<FileInformation>::uninit();
    // SAFETY: file owns a live handle, and FileInformation has the documented
    // BY_HANDLE_FILE_INFORMATION layout. The API initializes it on success.
    if unsafe { GetFileInformationByHandle(file.as_raw_handle(), information.as_mut_ptr()) } == 0 {
        return Err(std::io::Error::last_os_error());
    }
    // SAFETY: a successful call initialized every field in the structure.
    Ok(unsafe { information.assume_init() })
}
