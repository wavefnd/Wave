// SPDX-License-Identifier: MPL-2.0
use std::{
    fs,
    path::{Path, PathBuf},
    process::{Command, Output},
    sync::atomic::{AtomicU64, Ordering},
};
static NEXT: AtomicU64 = AtomicU64::new(0);
struct Case(PathBuf);
impl Case {
    fn new() -> Self {
        let path = std::env::temp_dir().join(format!(
            "wave-native-boundaries-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir_all(&path).unwrap();
        Self(path)
    }

    fn command(&self) -> Command {
        let mut c = Command::new(env!("CARGO_BIN_EXE_wavec"));
        c.current_dir(&self.0).arg("--std-root").arg(repo("std"));
        c
    }
}
impl Drop for Case {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}
fn repo(path: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join(path)
}

fn success(out: Output) {
    assert!(
        out.status.success(),
        "{}\n{}\n{}",
        out.status,
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
}
#[test]
fn windows_file_and_memory_boundaries() {
    let case = Case::new();
    for target in ["x86_64-pc-windows-msvc", "aarch64-pc-windows-msvc"] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        for opt in ["-O0", "-O2"] {
            success(
                case.command()
                    .arg("build")
                    .arg(repo("tests/fixtures/native_boundaries/windows.wave"))
                    .args(["--target", target, "--emit=ir,obj", opt, "--out-dir"])
                    .arg(case.0.join(format!("{target}-{opt}")))
                    .output()
                    .unwrap(),
            );
        }
    }
    #[cfg(target_os = "windows")]
    for opt in ["-O0", "-O2"] {
        success(
            case.command()
                .arg("build")
                .arg(repo("tests/fixtures/native_boundaries/windows.wave"))
                .args([opt, "-o", "case.exe"])
                .output()
                .unwrap(),
        );
        success(Command::new(case.0.join("case.exe")).current_dir(&case.0).output().unwrap());
    }
}
#[cfg(target_os = "linux")]
#[test]
fn windows_boundary_native_api_mocks() {
    let case = Case::new();
    let mut provider =
        fs::read_to_string(repo("std/sys/windows/fs.wave")).unwrap().replace("\r\n", "\n");
    let path_import = "import(\"std::sys::windows::path_encoding\")::{WidePath, wide_path, release_path, wide_path_to_utf8};";
    assert!(provider.contains(path_import));
    provider = provider.replace(path_import, r#"
struct WidePath { data: ptr<u16>; error: i64; }
fun wide_path(path: str) -> WidePath { var result: WidePath; result.data = null; result.error = 0; return result; }
fun release_path(path: WidePath, status: i64) -> i64 { return status; }
fun wide_path_to_utf8(path: ptr<u16>, n: i32, dst: ptr<u8>, cap: i64) -> i64 { return -38; }
"#).replace("import(\"std::sys::windows::memory\")", "import(\"./memory\")").replace("extern(system,", "extern(c,");
    fs::write(case.0.join("provider.wave"), provider).unwrap();
    fs::write(
        case.0.join("memory.wave"),
        fs::read_to_string(repo("std/sys/windows/memory.wave"))
            .unwrap()
            .replace("extern(system,", "extern(c,"),
    )
    .unwrap();
    fs::copy(repo("tests/fixtures/native_boundaries/windows_mock.wave"), case.0.join("case.wave"))
        .unwrap();
    success(
        Command::new("cc")
            .args(["-c", "-O2"])
            .arg(repo("tests/fixtures/native_boundaries/windows_mock.c"))
            .arg("-o")
            .arg(case.0.join("mock.o"))
            .output()
            .unwrap(),
    );
    for opt in ["-O0", "-O2"] {
        success(
            case.command()
                .args(["build", "case.wave", "mock.o", opt, "-o", "case.exe"])
                .output()
                .unwrap(),
        );
        success(Command::new(case.0.join("case.exe")).output().unwrap());
    }
}
#[cfg(unix)]
#[test]
fn non_utf8_cli_paths_report_errors_without_colliding_outputs() {
    use std::os::unix::ffi::OsStringExt;
    let case = Case::new();
    // Validate argv before filesystem access: macOS CI rejects file creation
    // with these byte sequences. A valid file at their shared lossy rendering
    // also catches accidental compilation of a different source via decoding.
    fs::write(case.0.join("x\u{fffd}.wave"), "fun main() -> i32 { return 0; }").unwrap();
    for byte in [0xfe, 0xff] {
        let name = std::ffi::OsString::from_vec(vec![b'x', byte, b'.', b'w', b'a', b'v', b'e']);
        for args in [
            vec!["check"],
            vec!["build", "--emit=obj", "-o", "output.o"],
            vec!["build", "--out-dir", "out"],
        ] {
            let out =
                case.command().args(args).arg(&name).arg("--error-format=json").output().unwrap();
            assert_eq!(out.status.code(), Some(2), "{out:?}");
            let error =
                utils::wson::parse_json(String::from_utf8_lossy(&out.stderr).trim()).unwrap();
            assert!(error
                .get("error")
                .unwrap()
                .get_str("message")
                .unwrap()
                .contains("not valid UTF-8"));
            assert!(!case.0.join("output.o").exists());
            assert!(!case.0.join("out").exists());
        }
    }
    fs::write(case.0.join("normal.wave"), "fun main() -> i32 { return 0; }").unwrap();
    success(
        case.command()
            .args(["build", "normal.wave", "--emit=obj", "-o", "explicit.o"])
            .output()
            .unwrap(),
    );
    assert!(case.0.join("explicit.o").is_file());
}

#[cfg(target_os = "linux")]
#[test]
fn linux_environment_reads_complete_large_and_exact_boundary_sources() {
    let case = Case::new();
    for opt in ["-O0", "-O2"] {
        success(
            case.command()
                .arg("build")
                .arg(repo("tests/fixtures/native_boundaries/environment.wave"))
                .args([opt, "-o", "env.exe"])
                .output()
                .unwrap(),
        );
        // WAVE_VALUE= (11 bytes), terminating NUL, and EMPTY= plus NUL.
        for length in [1, 32768 - 19, 32768 - 18, 40000, 65536 - 19, 69000] {
            success(
                Command::new(case.0.join("env.exe"))
                    .env_clear()
                    .env("WAVE_VALUE", "x".repeat(length))
                    .env("EMPTY", "")
                    .output()
                    .unwrap(),
            );
        }
        // The queried key occurs after a different entry beyond the initial read.
        success(
            Command::new(case.0.join("env.exe"))
                .env_clear()
                .env("AAA_PADDING", "y".repeat(40000))
                .env("WAVE_VALUE", "xxx")
                .env("EMPTY", "")
                .output()
                .unwrap(),
        );
    }
}

#[cfg(target_os = "linux")]
#[test]
fn environment_short_reads_interruptions_and_failure_cleanup() {
    let case = Case::new();
    success(
        Command::new("cc")
            .args(["-c", "-O2"])
            .arg(repo("tests/fixtures/native_boundaries/environment_mock.c"))
            .arg("-o")
            .arg(case.0.join("mock.o"))
            .output()
            .unwrap(),
    );
    let high = fs::read_to_string(repo("std/env/environ.wave")).unwrap().replace("\r\n", "\n");
    let high = high.replace("import(\"std::sys::env\")::{\n    env_read,\n};", "extern(c, \"mock_env_read\") fun env_read(buf: ptr<u8>, cap: i64) -> i64;")
        .replace("import(\"std::sys::memory\")::{sys_alloc, sys_free};", "extern(c, \"mock_alloc\") fun sys_alloc(size: i64) -> ptr<u8>; extern(c, \"mock_free\") fun sys_free(p: ptr<u8>, size: i64) -> i64;");
    assert!(!high.contains("std::sys::env") && !high.contains("std::sys::memory"));
    let low = fs::read_to_string(repo("std/sys/linux/amd64/env.wave")).unwrap();
    let low = format!(
        r#"
extern(c, "mock_open") fun open(path: str, flags: i32, mode: i32) -> i64;
extern(c, "mock_read") fun read(fd: i64, buf: ptr<u8>, count: i64) -> i64;
extern(c, "mock_close") fun close(fd: i64) -> i64;
{}"#,
        &low[low.find("pub fun env_read(").unwrap()..]
    );
    for (provider, fixture) in [(high, "environment_mock"), (low, "linux_env_read_mock")] {
        fs::write(
            case.0.join("case.wave"),
            format!(
                "{provider}\n{}",
                fs::read_to_string(repo(&format!(
                    "tests/fixtures/native_boundaries/{fixture}.wave"
                )))
                .unwrap()
            ),
        )
        .unwrap();
        for opt in ["-O0", "-O2"] {
            success(
                case.command()
                    .args(["build", "case.wave", "mock.o", opt, "-o", "case.exe"])
                    .output()
                    .unwrap(),
            );
            success(Command::new(case.0.join("case.exe")).output().unwrap());
        }
    }
}

#[test]
fn environment_provider_cross_target_objects() {
    let case = Case::new();
    for target in [
        "x86_64-unknown-linux-gnu",
        "aarch64-unknown-linux-gnu",
        "riscv64-unknown-linux-gnu",
        "loongarch64-unknown-linux-gnu",
        "x86_64-apple-darwin",
        "aarch64-apple-darwin",
        "x86_64-pc-windows-msvc",
        "aarch64-pc-windows-msvc",
        "x86_64-unknown-freebsd",
        "wasm32-wasip1",
    ] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        for opt in ["-O0", "-O2"] {
            success(
                case.command()
                    .arg("build")
                    .arg(repo("tests/fixtures/native_boundaries/environment.wave"))
                    .args(["--target", target, "--emit=obj", opt, "--out-dir"])
                    .arg(case.0.join(format!("{target}-{opt}")))
                    .output()
                    .unwrap(),
            );
        }
    }
}

#[test]
fn windows_unicode_environment() {
    let case = Case::new();
    for target in ["x86_64-pc-windows-msvc", "aarch64-pc-windows-msvc"] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        for opt in ["-O0", "-O2"] {
            success(
                case.command()
                    .arg("build")
                    .arg(repo("tests/fixtures/release_constants/windows_env.wave"))
                    .args(["--target", target, "--emit=obj", opt, "--out-dir"])
                    .arg(case.0.join(format!("{target}-{opt}")))
                    .output()
                    .unwrap(),
            );
        }
    }
    #[cfg(target_os = "windows")]
    for opt in ["-O0", "-O2"] {
        success(
            case.command()
                .arg("build")
                .arg(repo("tests/fixtures/release_constants/windows_env.wave"))
                .args([opt, "-o", "env.exe"])
                .output()
                .unwrap(),
        );
        success(
            Command::new(case.0.join("env.exe"))
                .env("WAVE_한😀", "값😀")
                .env("WAVE_EMPTY", "")
                .output()
                .unwrap(),
        );
    }
}

#[cfg(target_os = "linux")]
#[test]
fn windows_environment_conversion_capacity_and_cleanup() {
    let case = Case::new();
    let provider = fs::read_to_string(repo("std/sys/windows/env.wave"))
        .unwrap()
        .replace("extern(system,", "extern(c,");
    let fixture =
        fs::read_to_string(repo("tests/fixtures/release_constants/windows_env_mock.wave")).unwrap();
    fs::write(case.0.join("case.wave"), format!("{provider}\n{fixture}")).unwrap();
    success(
        Command::new("cc")
            .args(["-c", "-O2"])
            .arg(repo("tests/fixtures/release_constants/windows_env_mock.c"))
            .arg("-o")
            .arg(case.0.join("mock.o"))
            .output()
            .unwrap(),
    );
    for opt in ["-O0", "-O2"] {
        success(
            case.command()
                .args(["build", "case.wave", "mock.o", opt, "-o", "case.exe"])
                .output()
                .unwrap(),
        );
        success(Command::new(case.0.join("case.exe")).output().unwrap());
    }
}
