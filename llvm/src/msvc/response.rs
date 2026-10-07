// SPDX-License-Identifier: MPL-2.0
//! Shell-free link.exe/lld-link argument transport, including Unicode paths.
use crate::diagnostic::{CodegenError, CodegenPhase, PendingOutput};
use std::path::Path;

const COMMAND_BUDGET: usize = 8192;

/// Keep the response file alive until the linker has exited.
pub struct LinkArguments {
    pub arguments: Vec<String>,
    _response: Option<PendingOutput>,
}

fn quote(argument: &str) -> String {
    let mut quoted = String::from("\"");
    let mut slashes = 0;
    for ch in argument.chars() {
        if ch == '\\' {
            slashes += 1;
            continue;
        }
        quoted.extend(std::iter::repeat_n('\\', if ch == '"' { slashes * 2 + 1 } else { slashes }));
        quoted.push(ch);
        slashes = 0;
    }
    quoted.extend(std::iter::repeat_n('\\', slashes * 2));
    quoted.push('"');
    quoted
}

impl LinkArguments {
    /// Serialize after temporary output paths have been substituted. The
    /// logical argument list remains available to the driver's dry-run path.
    pub fn prepare(
        program: &str,
        arguments: &[String],
        output: &Path,
    ) -> Result<Self, CodegenError> {
        let lines: Vec<_> = arguments.iter().map(|arg| quote(arg)).collect();
        let length = program.encode_utf16().count()
            + 3
            + lines.iter().map(|arg| arg.encode_utf16().count() + 1).sum::<usize>();
        if length < COMMAND_BUDGET {
            return Ok(Self { arguments: arguments.to_vec(), _response: None });
        }
        let response = PendingOutput::new(output)?;
        // Both MSVC LINK and LLVM's response-file reader accept UTF-16LE BOM.
        let bytes: Vec<u8> = std::iter::once(0xfeff_u16)
            .chain(lines.join("\n").encode_utf16())
            .flat_map(u16::to_le_bytes)
            .collect();
        std::fs::write(response.path(), bytes)
            .map_err(|e| CodegenError::new(CodegenPhase::Link, "write linker response file", e))?;
        Ok(Self {
            arguments: vec![format!("@{}", response.path().display())],
            _response: Some(response),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn windows_quoting_preserves_empty_unicode_quotes_and_trailing_slashes() {
        assert_eq!(quote(""), "\"\"");
        assert_eq!(quote("한글 directory\\"), "\"한글 directory\\\\\"");
        assert_eq!(quote("a\\\"b"), "\"a\\\\\\\"b\"");
        assert_eq!(quote("a\\b"), "\"a\\b\"");
    }

    #[cfg(windows)]
    #[test]
    fn windows_argument_parser_round_trips_serialized_arguments() {
        #[link(name = "shell32")]
        unsafe extern "system" {
            fn CommandLineToArgvW(command: *const u16, count: *mut i32) -> *mut *mut u16;
        }
        #[link(name = "kernel32")]
        unsafe extern "system" {
            fn LocalFree(memory: *mut std::ffi::c_void) -> *mut std::ffi::c_void;
        }
        let original =
            ["link.exe", "", "한글 object.obj", "a\\\"b", "C:\\space here\\", "literal\"quote"];
        let command: Vec<u16> = original
            .iter()
            .map(|arg| quote(arg))
            .collect::<Vec<_>>()
            .join(" ")
            .encode_utf16()
            .chain(Some(0))
            .collect();
        let mut count = 0;
        // SAFETY: input is NUL-terminated UTF-16; Windows owns the returned
        // argument allocation until LocalFree. Copy strings before releasing it.
        let actual = unsafe {
            let argv = CommandLineToArgvW(command.as_ptr(), &mut count);
            assert!(!argv.is_null());
            let mut values = Vec::new();
            for index in 0..count as usize {
                let value = *argv.add(index);
                let mut length = 0;
                while *value.add(length) != 0 {
                    length += 1;
                }
                values.push(String::from_utf16_lossy(std::slice::from_raw_parts(value, length)));
            }
            LocalFree(argv.cast());
            values
        };
        assert_eq!(actual, original);
    }

    #[test]
    fn response_lifetime_and_encoding_cover_success_and_early_failure() {
        let output = std::env::temp_dir().join("wave-response-test.exe");
        let args = vec!["/LIBPATH:한글 directory\\".repeat(1000), "".into()];
        let response = LinkArguments::prepare("lld-link", &args, &output).unwrap();
        let path = response._response.as_ref().unwrap().path().to_owned();
        let bytes = std::fs::read(&path).unwrap();
        assert_eq!(&bytes[..2], &[0xff, 0xfe]);
        let decoded = String::from_utf16(
            &bytes[2..]
                .chunks_exact(2)
                .map(|b| u16::from_le_bytes([b[0], b[1]]))
                .collect::<Vec<_>>(),
        )
        .unwrap();
        assert_eq!(decoded, args.iter().map(|arg| quote(arg)).collect::<Vec<_>>().join("\n"));
        drop(response);
        assert!(!path.exists());
        let short = LinkArguments::prepare("lld-link", &["/NOLOGO".into()], &output).unwrap();
        assert!(short._response.is_none());
        assert_eq!(short.arguments, ["/NOLOGO"]);
    }
}
