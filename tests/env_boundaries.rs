//! Portable environment boundaries and the Darwin CRT adapter contract.
#![cfg(any(
    target_os = "linux",
    target_os = "macos",
    target_os = "windows",
    target_os = "freebsd"
))]

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};

static NEXT: AtomicU64 = AtomicU64::new(0);
struct Case(PathBuf);
impl Drop for Case {
    fn drop(&mut self) {
        if !std::thread::panicking() {
            let _ = fs::remove_dir_all(&self.0);
        }
    }
}

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn copy_tree(source: &Path, destination: &Path) {
    fs::create_dir_all(destination).unwrap();
    for entry in fs::read_dir(source).unwrap() {
        let entry = entry.unwrap();
        let target = destination.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_tree(&entry.path(), &target);
        } else {
            fs::copy(entry.path(), target).unwrap();
        }
    }
}

fn case() -> Case {
    let case = Case(std::env::temp_dir().join(format!(
        "wave-env-boundaries-{}-{}",
        std::process::id(),
        NEXT.fetch_add(1, Ordering::Relaxed)
    )));
    copy_tree(&root().join("std"), &case.0.join("std"));
    case
}

fn native_target() -> Option<String> {
    let target = Command::new(env!("CARGO_BIN_EXE_wavec"))
        .args(["print", "default-target"])
        .output()
        .unwrap();
    assert!(target.status.success());
    let target = String::from_utf8(target.stdout).unwrap().trim().to_owned();
    if llvm::codegen::target::target_spec_for_triple(&target).is_none() {
        eprintln!("environment native regression skipped: {target} disabled");
        None
    } else {
        Some(target)
    }
}

fn build(case: &Case, fixture: &Path, opt: &str, target: &str, object: bool) -> PathBuf {
    let output = case.0.join(if object {
        "fixture.o"
    } else if cfg!(windows) {
        "fixture.exe"
    } else {
        "fixture"
    });
    let result = Command::new(env!("CARGO_BIN_EXE_wavec"))
        .arg("--std-root")
        .arg(case.0.join("std"))
        .arg("build")
        .arg(fixture)
        .args(["--target", target, opt, "--emit", if object { "obj" } else { "bin" }, "-o"])
        .arg(&output)
        .output()
        .unwrap();
    assert!(result.status.success(), "{target} {opt}: {}", String::from_utf8_lossy(&result.stderr));
    output
}

fn run(executable: &Path, variables: bool) {
    let mut command = wave_tools::build::test_command().unwrap();
    command.current_dir(root()).env_remove("WAVE_TEST_ENV_ABSENT_427_428");
    if variables {
        let values = utils::wson::parse_json(
            &fs::read_to_string(root().join("tests/fixtures/env_boundaries.json")).unwrap(),
        )
        .unwrap();
        let utils::wson::Value::Object(fields) = values else {
            panic!("environment fixture must be an object");
        };
        for (name, value) in fields {
            let utils::wson::Value::String(value) = value else {
                panic!("fixture must contain strings");
            };
            command.env(name, value);
        }
    }
    let result = command.args(["exec", "--timeout", "30", "--"]).arg(executable).output().unwrap();
    assert!(result.status.success(), "{result:?}");
}

#[test]
fn portable_environment_boundaries_use_injected_variables() {
    let Some(target) = native_target() else { return };
    let case = case();
    for opt in ["-O0", "-O2"] {
        let binary =
            build(&case, &root().join("tests/cases/shared/test142.wave"), opt, &target, false);
        run(&binary, true);
    }
}

#[test]
fn darwin_environment_adapter_handles_pointer_and_capacity_boundaries() {
    let Some(target) = native_target() else { return };
    let case = case();
    let path = case.0.join("std/sys/macos/common/env.wave");
    let mut source = fs::read_to_string(&path).unwrap();
    source =
        source.lines().filter(|line| !line.starts_with("extern(c,")).collect::<Vec<_>>().join("\n");
    source.push('\n');
    source.push_str(include_str!("fixtures/env_boundaries_mock_helpers.wave"));
    fs::write(path, source).unwrap();
    for opt in ["-O0", "-O2"] {
        let binary = build(
            &case,
            &root().join("tests/fixtures/env_boundaries_mock.wave"),
            opt,
            &target,
            false,
        );
        run(&binary, false);
    }
}

#[cfg(not(target_os = "freebsd"))]
#[test]
fn environment_lookup_retries_only_the_source_capacity_status() {
    let Some(target) = native_target() else { return };
    let case = case();
    fs::write(
        case.0.join("std/sys/env.wave"),
        include_str!("fixtures/env_boundaries_retry_provider.wave"),
    )
    .unwrap();
    for opt in ["-O0", "-O2"] {
        let binary = build(
            &case,
            &root().join("tests/fixtures/env_boundaries_retry.wave"),
            opt,
            &target,
            false,
        );
        run(&binary, false);
    }
}

#[test]
fn environment_boundaries_emit_for_both_darwin_architectures() {
    let case = case();
    for target in ["x86_64-apple-darwin", "aarch64-apple-darwin"] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        for opt in ["-O0", "-O2"] {
            let object =
                build(&case, &root().join("tests/cases/shared/test142.wave"), opt, target, true);
            assert!(object.metadata().unwrap().len() > 0);
        }
    }
}

// Exercise the FreeBSD facade's retry budget with deterministic provider races.
// These are host executions of the selected policy, not native sysctl tests.
#[cfg(target_os = "linux")]
#[test]
fn freebsd_environment_lookup_bounds_capacity_and_read_attempts() {
    let Some(target) = native_target() else { return };
    let case = case();
    let path = case.0.join("std/env/environ.wave");
    let source = fs::read_to_string(&path)
        .unwrap()
        .replace("target(os=\"linux\")", "target(os=\"disabled\")")
        .replace("target(os=\"freebsd\")", "target(os=\"linux\")");
    fs::write(path, source).unwrap();
    fs::write(
        case.0.join("std/sys/env.wave"),
        r#"
static mode: i32 = 0;
static reads: i32 = 0;
pub fun configure(next: i32) { mode = next; reads = 0; }
pub fun read_count() -> i32 { return reads; }
pub fun env_required_capacity() -> i64 {
    if (mode == 3) { return 16777217; }
    if (mode == 5) { return -5; }
    if (mode == 6) { return 16777216; }
    if (mode == 7 && reads >= 2) { return 131072; }
    return 65536;
}
pub fun env_read(buf: ptr<u8>, cap: i64) -> i64 {
    reads += 1;
    if (mode == 4) { return -12; }
    if (mode == 2 || mode == 5 || cap < env_required_capacity()) { return -28; }
    var entry: str = "WAVE_BOUNDED_ENV=ok";
    var i: i64 = 0;
    while (entry[i] != 0) { buf[i] = entry[i]; i += 1; }
    buf[i] = 0;
    return i + 1;
}
"#,
    )
    .unwrap();
    let fixture = case.0.join("bounded.wave");
    fs::write(&fixture, r#"
import("std::env::environ")::{env_get};
import("std::env::consts")::{ENV_ERR_SOURCE_INCOMPLETE, ENV_ERR_READ};
import("std::sys::env")::{configure, read_count};
fun main() -> i32 {
    var output: array<u8, 8>;
    configure(1);
    if (env_get("WAVE_BOUNDED_ENV", &output[0], 8) != 2 || read_count() != 2 || output[0] != 111 || output[1] != 107 || output[2] != 0) { return 1; }
    output[0] = 91;
    configure(2);
    if (env_get("WAVE_BOUNDED_ENV", &output[0], 8) != ENV_ERR_SOURCE_INCOMPLETE || read_count() != 3 || output[0] != 91) { return 2; }
    configure(3);
    if (env_get("WAVE_BOUNDED_ENV", &output[0], 8) != ENV_ERR_SOURCE_INCOMPLETE || read_count() != 1 || output[0] != 91) { return 3; }
    configure(4);
    if (env_get("WAVE_BOUNDED_ENV", &output[0], 8) != ENV_ERR_READ || read_count() != 1 || output[0] != 91) { return 4; }
    configure(5);
    if (env_get("WAVE_BOUNDED_ENV", &output[0], 8) != ENV_ERR_READ || read_count() != 1 || output[0] != 91) { return 5; }
    configure(6);
    if (env_get("WAVE_BOUNDED_ENV", &output[0], 8) != 2 || read_count() != 2) { return 6; }
    configure(7);
    if (env_get("WAVE_BOUNDED_ENV", &output[0], 8) != 2 || read_count() != 3) { return 7; }
    return 0;
}
"#).unwrap();
    for opt in ["-O0", "-O2"] {
        run(&build(&case, &fixture, opt, &target, false), false);
    }
}

fn environment_key_fixture(windows: bool) -> String {
    let expected = if windows { "2" } else { "ENV_ERR_NOT_FOUND" };
    let native_probe = if windows {
        let long_key =
            "WAVE_LONG_S".encode_utf16().chain([0]).map(|u| u.to_string()).collect::<Vec<_>>();
        let key =
            "WAVE_KELVIN_k".encode_utf16().chain([0]).map(|u| u.to_string()).collect::<Vec<_>>();
        format!(
            r#"
extern(system, "GetEnvironmentVariableW") fun native_get(name: ptr<u16>, destination: ptr<u16>, cap: u32) -> u32;
fun kelvin_expected() -> i64 {{
    var key: array<u16, {}> = [{}];
    if (native_get(&key[0], null, 0) > 0) {{ return 2; }}
    return ENV_ERR_NOT_FOUND;
}}
fun long_expected() -> i64 {{
    var key: array<u16, {}> = [{}];
    if (native_get(&key[0], null, 0) > 0) {{ return 2; }}
    return ENV_ERR_NOT_FOUND;
}}
"#,
            key.len(),
            key.join(", "),
            long_key.len(),
            long_key.join(", ")
        )
    } else {
        "fun kelvin_expected() -> i64 { return ENV_ERR_NOT_FOUND; } fun long_expected() -> i64 { return ENV_ERR_NOT_FOUND; }".to_owned()
    };
    format!(
        r#"
import("std::env::environ")::{{env_get}};
import("std::env::consts")::{{ENV_ERR_NOT_FOUND}};
fun main() -> i32 {{
    var output: array<u8, 8>;
    if (env_get("Wave_Mixed_Key", &output[0], 8) != 2) {{ return 1; }}
    if (env_get("wave_mixed_key", &output[0], 8) != {expected}) {{ return 2; }}
    if (env_get("WAVE_UNICODE_ä", &output[0], 8) != {expected}) {{ return 3; }}
    // Kelvin sign is three UTF-8 bytes, K is one. Never prefilter by UTF-8 length.
    if (env_get("WAVE_KELVIN_k", &output[0], 8) != kelvin_expected()) {{ return 4; }}
    if (env_get("WAVE_UNICODE_Ä", &output[0], 8) != 2) {{ return 5; }}
    if (env_get("WAVE_LONG_S", &output[0], 8) != long_expected()) {{ return 6; }}
    return 0;
}}
{native_probe}
"#
    )
}

#[test]
fn environment_key_matching_follows_the_native_platform() {
    let Some(target) = native_target() else { return };
    let case = case();
    let fixture = case.0.join("keys.wave");
    fs::write(&fixture, environment_key_fixture(cfg!(windows))).unwrap();
    for opt in ["-O0", "-O2"] {
        let binary = build(&case, &fixture, opt, &target, false);
        let mut command = wave_tools::build::test_command().unwrap();
        command.args(["exec", "--timeout", "30", "--"]).arg(binary);
        if !cfg!(windows) {
            command
                .env_remove("wave_mixed_key")
                .env_remove("WAVE_UNICODE_ä")
                .env_remove("WAVE_KELVIN_k")
                .env_remove("WAVE_LONG_S");
        }
        let result = command
            .env("Wave_Mixed_Key", "ok")
            .env("WAVE_UNICODE_Ä", "ok")
            .env("WAVE_KELVIN_K", "ok")
            .env("WAVE_LONG_ſ", "ok")
            .output()
            .unwrap();
        assert!(result.status.success(), "{result:?}");
    }
}

#[test]
fn environment_boundaries_emit_for_windows_and_freebsd_targets() {
    let case = case();
    for target in ["x86_64-pc-windows-msvc", "aarch64-pc-windows-msvc", "x86_64-unknown-freebsd"] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        for opt in ["-O0", "-O2"] {
            let object =
                build(&case, &root().join("tests/cases/shared/test142.wave"), opt, target, true);
            assert!(object.metadata().unwrap().len() > 0);
            if target.contains("windows") {
                let fixture = case.0.join("native-keys.wave");
                fs::write(&fixture, environment_key_fixture(true)).unwrap();
                assert!(build(&case, &fixture, opt, target, true).metadata().unwrap().len() > 0);
            }
        }
    }
}

#[cfg(target_os = "linux")]
#[test]
fn freebsd_environment_provider_confirms_capacity_before_retrying_enomem() {
    let Some(target) = native_target() else { return };
    let case = case();
    let path = case.0.join("std/sys/freebsd/common/env.wave");
    let mut source = fs::read_to_string(&path)
        .unwrap()
        .replace("import(\"std::sys::freebsd::syscall\")::{syscall0, syscall6};", "");
    source.push_str(r#"
static mode: i32 = 0;
static actual_reads: i32 = 0;
pub fun configure(next: i32) { mode = next; actual_reads = 0; }
pub fun count_reads() -> i32 { return actual_reads; }
fun syscall0(number: i64) -> i64 { return 42; }
fun syscall6(number: i64, name: i64, count: i64, data: i64, size: i64, unused: i64, zero: i64) -> i64 {
    var length: ptr<u64> = size as ptr<u64>;
    if (data == 0) {
        if (mode == 4) { return -5; }
        deref length = 8;
        if (mode == 1 && actual_reads > 0 || mode == 3) { deref length = 64; }
        return 0;
    }
    actual_reads += 1;
    if (mode == 1 || mode == 2) { return -12; }
    var raw: ptr<u8> = data as ptr<u8>;
    raw[0] = 0;
    deref length = 1;
    return 0;
}
"#);
    fs::write(path, source).unwrap();
    let fixture = case.0.join("provider.wave");
    fs::write(
        &fixture,
        r#"
import("std::sys::freebsd::common::env")::{configure, count_reads, env_read};
fun main() -> i32 {
    var output: array<u8, 16>;
    output[0] = 91;
    configure(1);
    if (env_read(&output[0], 16) != -28 || count_reads() != 1 || output[0] != 91) { return 1; }
    configure(2);
    if (env_read(&output[0], 16) != -12 || count_reads() != 1 || output[0] != 91) { return 2; }
    configure(3);
    if (env_read(&output[0], 16) != -28 || count_reads() != 0 || output[0] != 91) { return 3; }
    configure(4);
    if (env_read(&output[0], 16) != -5 || count_reads() != 0 || output[0] != 91) { return 4; }
    configure(0);
    if (env_read(&output[0], 16) != 1 || count_reads() != 1 || output[0] != 0) { return 5; }
    return 0;
}
"#,
    )
    .unwrap();
    for opt in ["-O0", "-O2"] {
        run(&build(&case, &fixture, opt, &target, false), false);
    }
}

#[test]
fn hidden_drive_entries_are_not_ordinary_environment_keys() {
    let Some(target) = native_target() else { return };
    let case = case();
    fs::write(case.0.join("std/sys/env.wave"), r#"
pub fun env_read(buf: ptr<u8>, cap: i64) -> i64 {
    var entry: array<u8, 20> = [61, 67, 58, 61, 67, 58, 92, 0, 87, 65, 86, 69, 95, 75, 69, 89, 61, 120, 0, 0];
    if (cap < 20) { return -28; }
    var i: i64 = 0;
    while (i < 20) { buf[i] = entry[i]; i += 1; }
    return 20;
}
#[target(os="freebsd")]
pub fun env_required_capacity() -> i64 { return 20; }
"#).unwrap();
    let fixture = case.0.join("hidden.wave");
    fs::write(
        &fixture,
        r#"
import("std::env::environ")::{env_get};
import("std::env::consts")::{ENV_ERR_INVALID_KEY, ENV_ERR_NOT_FOUND};
fun main() -> i32 {
    var output: array<u8, 4>;
    if (env_get("WAVE_KEY", &output[0], 4) != 1 || output[0] != 120 || output[1] != 0) { return 1; }
    if (env_get("=C:", &output[0], 4) != ENV_ERR_INVALID_KEY) { return 2; }
    if (env_get("C:", &output[0], 4) != ENV_ERR_NOT_FOUND) { return 3; }
    return 0;
}
"#,
    )
    .unwrap();
    for opt in ["-O0", "-O2"] {
        run(&build(&case, &fixture, opt, &target, false), false);
    }
}
