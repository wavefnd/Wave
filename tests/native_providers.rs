// SPDX-License-Identifier: MPL-2.0
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant};

static NEXT: AtomicU64 = AtomicU64::new(0);
struct Case(PathBuf);
impl Case {
    fn new() -> Self {
        let path = std::env::temp_dir().join(format!(
            "wave-native-providers-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir_all(&path).unwrap();
        Self(path)
    }

    fn compiler(&self) -> Command {
        let mut cmd = Command::new(env!("CARGO_BIN_EXE_wavec"));
        cmd.current_dir(&self.0).arg("--std-root").arg(root().join("std"));
        cmd
    }
}
impl Drop for Case {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}
fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn fixture(name: &str) -> PathBuf {
    root().join("tests/fixtures/native_providers").join(name)
}

fn supported(target: &str) -> bool {
    llvm::codegen::target::target_spec_for_triple(target).is_some()
}

fn checked(cmd: &mut Command, context: &str, dir: &Path) {
    let stdout = dir.join("stdout.log");
    let stderr = dir.join("stderr.log");
    let mut child = cmd
        .stdout(Stdio::from(fs::File::create(&stdout).unwrap()))
        .stderr(Stdio::from(fs::File::create(&stderr).unwrap()))
        .spawn()
        .unwrap_or_else(|e| panic!("{context}: {cmd:?}: {e}"));
    let deadline = Instant::now() + Duration::from_secs(60);
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break status;
        }
        if Instant::now() >= deadline {
            let _ = child.kill();
            let _ = child.wait();
            panic!(
                "{context}: timed out: {cmd:?}\n{}\n{}",
                fs::read_to_string(stdout).unwrap(),
                fs::read_to_string(stderr).unwrap()
            );
        }
        std::thread::sleep(Duration::from_millis(10));
    };
    assert!(
        status.success(),
        "{context}: {cmd:?}: {status}\n{}\n{}",
        fs::read_to_string(stdout).unwrap(),
        fs::read_to_string(stderr).unwrap()
    );
}

fn build(case: &Case, source: &Path, target: &str, opt: &str, output: &Path, object: bool) {
    checked(
        case.compiler()
            .arg("build")
            .arg(source)
            .args(["--target", target, opt, "--emit", if object { "obj" } else { "bin" }, "-o"])
            .arg(output),
        &format!("{target} {opt} {} build", source.display()),
        &case.0,
    );
}

fn host_target() -> String {
    let os = match std::env::consts::OS {
        "macos" => "apple-darwin",
        "windows" => "pc-windows-msvc",
        "freebsd" => "unknown-freebsd",
        _ => "unknown-linux-gnu",
    };
    format!("{}-{os}", std::env::consts::ARCH)
}

#[test]
fn windows_pathname_streams_and_empty_events() {
    let case = Case::new();
    for target in ["x86_64-pc-windows-msvc", "aarch64-pc-windows-msvc"] {
        if !supported(target) {
            continue;
        }
        for name in [
            "windows_local_addr.wave",
            "windows_unix.wave",
            "windows_event.wave",
            "windows_event_infinite.wave",
        ] {
            for opt in ["-O0", "-O2"] {
                let native = target == host_target();
                let output = case.0.join(if native { "ipc.exe" } else { "ipc.o" });
                build(&case, &fixture(name), target, opt, &output, !native);
                if native && name != "windows_event_infinite.wave" {
                    let run_dir = case.0.join(format!("{name}-{opt}"));
                    fs::create_dir(&run_dir).unwrap();
                    checked(Command::new(&output).current_dir(&run_dir), name, &case.0);
                }
            }
        }
    }
}

#[cfg(windows)]
#[test]
fn windows_empty_infinite_wait_blocks_until_process_is_stopped() {
    let case = Case::new();
    // A marker proves the child reached the wait, rather than merely starting slowly.
    let source = fixture("windows_event_infinite.wave");
    for opt in ["-O0", "-O2"] {
        let binary = case.0.join("infinite.exe");
        build(&case, &source, &host_target(), opt, &binary, false);
        let mut child = Command::new(binary).current_dir(&case.0).spawn().unwrap();
        let deadline = Instant::now() + Duration::from_secs(10);
        let marker = case.0.join("ready");
        while !marker.exists() && Instant::now() < deadline {
            if let Some(status) = child.try_wait().unwrap() {
                panic!("infinite wait exited before readiness: {status}");
            }
            std::thread::sleep(Duration::from_millis(10));
        }
        let ready = marker.exists();
        std::thread::sleep(Duration::from_millis(250));
        let premature = child.try_wait().unwrap();
        let _ = child.kill();
        let _ = child.wait();
        assert!(ready, "child did not reach the wait");
        assert!(premature.is_none(), "infinite wait returned: {premature:?}");
        fs::remove_file(marker).unwrap();
    }
}

#[test]
fn loongarch_attribute_aliases_select_identical_declarations() {
    let target = "loongarch64-unknown-linux-gnu";
    if !supported(target) {
        return;
    }
    let case = Case::new();
    let source = case.0.join("alias.wave");
    fs::write(&source, "#[target(arch=\" LoOnG64 \")]\nfun alias() -> i32 { return 7; }\n#[target(arch=\"loongarch64\")]\nfun canonical() -> i32 { return alias(); }\nfun main() -> i32 { return canonical(); }\n").unwrap();
    checked(
        case.compiler().arg("build").arg(source).args(["--target", target, "--emit=check"]),
        target,
        &case.0,
    );
}

#[test]
fn native_provider_fixtures_compile_and_run_on_their_host() {
    let case = Case::new();
    for (source, targets) in [
        (
            "cwd.wave",
            vec![
                "x86_64-unknown-linux-gnu",
                "aarch64-unknown-linux-gnu",
                "riscv64-unknown-linux-gnu",
                "loongarch64-unknown-linux-gnu",
                "x86_64-apple-darwin",
                "aarch64-apple-darwin",
                "x86_64-pc-windows-msvc",
                "aarch64-pc-windows-msvc",
                "x86_64-unknown-freebsd",
            ],
        ),
        (
            "posix_poll_range.wave",
            vec![
                "x86_64-unknown-linux-gnu",
                "aarch64-unknown-linux-gnu",
                "riscv64-unknown-linux-gnu",
                "loongarch64-unknown-linux-gnu",
                "x86_64-apple-darwin",
                "aarch64-apple-darwin",
                "x86_64-unknown-freebsd",
            ],
        ),
        (
            "dup2.wave",
            vec![
                "x86_64-unknown-linux-gnu",
                "aarch64-unknown-linux-gnu",
                "riscv64-unknown-linux-gnu",
                "loongarch64-unknown-linux-gnu",
            ],
        ),
        ("macos_memory.wave", vec!["x86_64-apple-darwin", "aarch64-apple-darwin"]),
        ("macos_event.wave", vec!["x86_64-apple-darwin", "aarch64-apple-darwin"]),
        (
            "random.wave",
            vec!["x86_64-pc-windows-msvc", "aarch64-pc-windows-msvc", "x86_64-unknown-freebsd"],
        ),
    ] {
        for target in targets {
            if !supported(target) {
                continue;
            }
            for opt in ["-O0", "-O2"] {
                let native = target == host_target();
                let output = case.0.join(if native { "probe.exe" } else { "probe.o" });
                build(&case, &fixture(source), target, opt, &output, !native);
                if native {
                    checked(
                        &mut Command::new(&output),
                        &format!("{target} {opt} {source} run"),
                        &case.0,
                    );
                }
            }
        }
    }
}

// Exact OS failures and historical clock values are injected at the OS ABI
// boundary. The production Wave bodies are copied unchanged except imports
// and the system calling-convention spelling on the Linux test host.
#[cfg(all(target_os = "linux", target_arch = "x86_64", feature = "llvm-target-x86"))]
#[test]
fn native_provider_os_boundary_failures() {
    let case = Case::new();
    for (name, provider) in [
        ("windows_time", "std/sys/windows/time.wave"),
        ("windows_local_addr", "std/sys/windows/local_addr.wave"),
        ("windows_unix", "std/sys/windows/local_addr.wave"),
        ("windows_event", "std/sys/windows/event.wave"),
        ("windows_random", "std/sys/windows/random.wave"),
        ("freebsd_random", "std/sys/freebsd/amd64/random.wave"),
        ("macos_event", "std/sys/macos/event.wave"),
    ] {
        let mut text = fs::read_to_string(root().join(provider))
            .unwrap()
            .replace("extern(system,", "extern(c,");
        if name == "windows_unix" {
            let unix = fs::read_to_string(root().join("std/net/unix.wave")).unwrap();
            text += "\nimport(\"std::net::error\")::{NetResult, NetError, net_error_from_native, net_result_err, net_result_ok};\n";
            let types = unix.find("pub struct UnixListener").unwrap();
            let end_types = unix[types..].find("#[target").unwrap() + types;
            text += &unix[types..end_types];
            let windows = unix.find("#[target(os=\"windows\")]\nfun _unix_socket").unwrap();
            let start = windows + "#[target(os=\"windows\")]\n".len();
            let end = unix[start..].find('\n').unwrap() + start;
            text += &unix[start..end];
            let start = unix.find("fun _unix_address").unwrap();
            let end = unix[start..].find("#[target").unwrap() + start;
            text += &unix[start..end];
            text += r#"
const SOCK_STREAM: i32 = 1;
extern(c) fun socket(domain: i32, ty: i32, protocol: i32) -> i64;
extern(c) fun bind(fd: i64, address: ptr<i8>, length: i32) -> i64;
extern(c) fun listen(fd: i64, backlog: i32) -> i64;
extern(c) fun connect(fd: i64, address: ptr<i8>, length: i32) -> i64;
extern(c) fun net_close(fd: i64) -> i64;
"#;
        }
        if name == "windows_event" {
            text = text.replace(
                "import(\"std::sys::windows::socket\")::{\n    POLLIN, POLLOUT, POLLERR, POLLHUP, POLLNVAL, PollFd, poll,\n};",
                "pub const POLLIN: i16 = 256; pub const POLLOUT: i16 = 16; pub const POLLERR: i16 = 1; pub const POLLHUP: i16 = 2; pub const POLLNVAL: i16 = 4; struct PollFd { fd: i64; events: i16; revents: i16; } extern(c) fun poll(fds: ptr<PollFd>, count: i64, timeout: i32) -> i64;",
            ).replace(
                "import(\"std::sys::windows::memory\")::{sys_alloc, sys_free};",
                "extern(c) fun sys_alloc(size: i64) -> ptr<u8>; extern(c) fun sys_free(p: ptr<u8>, size: i64) -> i64;",
            );
        }
        if name == "freebsd_random" {
            text = text.replace(
                "import(\"std::sys::freebsd::amd64::syscall\")::{syscall3};",
                "extern(c) fun syscall3(id: i64, buffer: i64, size: i64, flags: i64) -> i64;",
            );
        }
        if name.ends_with("random") {
            text += &fs::read_to_string(root().join("std/random/fill.wave")).unwrap().replace(
                "import(\"std::sys::random\")::{sys_random_available, sys_random_read};",
                "",
            );
        }
        text += &fs::read_to_string(fixture(&format!("{name}_mock.wave"))).unwrap();
        let source = case.0.join(format!("{name}.wave"));
        fs::write(&source, text).unwrap();
        let c_object = case.0.join("host.o");
        checked(
            Command::new("clang")
                .arg("-c")
                .arg(fixture(&format!("{name}_mock.c")))
                .args(["-O2", "-o"])
                .arg(&c_object),
            name,
            &case.0,
        );
        for opt in ["-O0", "-O2"] {
            let object = case.0.join("probe.o");
            build(&case, &source, "x86_64-unknown-linux-gnu", opt, &object, true);
            let binary = case.0.join("probe");
            checked(
                Command::new("clang")
                    // Wave emits static objects; Ubuntu's clang defaults to PIE.
                    .arg("-no-pie")
                    .arg(&object)
                    .arg(&c_object)
                    .arg("-o")
                    .arg(&binary),
                &format!("{name} {opt} link"),
                &case.0,
            );
            checked(&mut Command::new(binary), &format!("{name} {opt} run"), &case.0);
        }
    }
}

#[test]
fn darwin_bidirectional_c_abi_fixtures() {
    let case = Case::new();
    let required = std::env::var_os("WAVE_RUN_DARWIN_INTEROP_TESTS").is_some();
    if required {
        assert_eq!(
            std::env::consts::OS,
            "macos",
            "Darwin ABI execution requires a native macOS runner"
        );
        assert!(supported(&host_target()), "native LLVM target is required");
    }
    for target in ["x86_64-apple-darwin", "aarch64-apple-darwin"] {
        if !supported(target) {
            continue;
        }
        let mut fixtures = vec![
            "c_abi_edges/interop",
            "native_providers/darwin_variadic",
            "aggregate_registers/pointer_float",
            "aggregate_registers/float_pressure",
            "aggregate_registers/mixed_gp_pressure",
            "aggregate_registers/aligned_i128",
            "aggregate_registers/triple_float",
            "aggregate_registers/hfa_stack",
            "aggregate_registers/aligned_pressure",
        ];
        if target.starts_with("x86_64") {
            fixtures.extend(["x86_64_sysv/interop", "x86_64_sysv_pressure/interop"]);
        } else {
            fixtures.push("aarch64_aapcs64/interop");
        }
        for name in fixtures {
            for opt in ["-O0", "-O2"] {
                let context = format!("{target} {opt} {name}");
                let source = root().join("tests/fixtures").join(name);
                let c_object = case.0.join("c.o");
                checked(
                    Command::new("clang")
                        .arg(format!("--target={target}"))
                        .args(["-ffreestanding", "-fno-builtin", "-fno-stack-protector", opt, "-c"])
                        .arg(source.with_extension("c"))
                        .arg("-o")
                        .arg(&c_object),
                    &context,
                    &case.0,
                );
                let object = case.0.join("wave.o");
                build(&case, &source.with_extension("wave"), target, opt, &object, true);
                if required && target == host_target() {
                    let binary = case.0.join("abi");
                    checked(
                        Command::new("clang").arg(&c_object).arg(&object).arg("-o").arg(&binary),
                        &format!("{context} link"),
                        &case.0,
                    );
                    checked(&mut Command::new(binary), &format!("{context} run"), &case.0);
                }
            }
        }
    }
}

#[test]
fn network_error_tables_and_async_sleep_results_run_on_native_hosts() {
    let target = host_target();
    if !supported(&target) {
        return;
    }
    let case = Case::new();
    for name in ["network_errors.wave", "async_sleep.wave"] {
        for opt in ["-O0", "-O2"] {
            let output = case.0.join("probe.exe");
            build(&case, &fixture(name), &target, opt, &output, false);
            checked(&mut Command::new(output), name, &case.0);
        }
    }
}

#[test]
fn full_range_trigonometry_matches_high_precision_references() {
    let target = host_target();
    if !supported(&target) {
        return;
    }
    let case = Case::new();
    let rows: Vec<Vec<u64>> = include_str!("fixtures/native_providers/trig_reference.txt")
        .lines()
        .filter(|s| !s.starts_with('#'))
        .map(|s| s.split_whitespace().map(|n| u64::from_str_radix(n, 16).unwrap()).collect())
        .collect();
    let mut source = String::from("import(\"std::math::trig\")::{SinCosF64, SinCosF32, sin_cos_f64, sin_cos_f32, sin_f64, cos_f64, tan_f64, sin_f32, cos_f32, tan_f32, wrap_angle_pi_f64, MATH_PI_F64};\nimport(\"std::math::float\")::{abs_f64, float_from_bits_f64, float_from_bits_f32, float_to_bits_f64, float_to_bits_f32, nan_f64, infinity_f64, is_nan_f64};\n");
    for (column, name) in ["inputs", "sines", "cosines", "tangents"].iter().enumerate() {
        source += &format!(
            "static {name}: array<u64, {}> = [{}];\n",
            rows.len(),
            rows.iter().map(|r| r[column].to_string()).collect::<Vec<_>>().join(",")
        );
    }
    source += &format!("const CASES: i32 = {};\n", rows.len());
    let single_rows: Vec<Vec<u64>> =
        include_str!("fixtures/native_providers/trig_reference_f32.txt")
            .lines()
            .filter(|s| !s.starts_with('#'))
            .map(|s| s.split_whitespace().map(|n| u64::from_str_radix(n, 16).unwrap()).collect())
            .collect();
    for (column, name) in ["inputs32", "sines32", "cosines32", "tangents32"].iter().enumerate() {
        source += &format!(
            "static {name}: array<u64, {}> = [{}];\n",
            single_rows.len(),
            single_rows.iter().map(|r| r[column].to_string()).collect::<Vec<_>>().join(",")
        );
    }
    source += &format!("const SINGLE_CASES: i32 = {};\n", single_rows.len());
    source += include_str!("fixtures/native_providers/trig_check.wave");
    let path = case.0.join("trig.wave");
    fs::write(&path, source).unwrap();
    for opt in ["-O0", "-O2"] {
        let output = case.0.join("probe.exe");
        build(&case, &path, &target, opt, &output, false);
        checked(&mut Command::new(output), "trig reference vectors", &case.0);
    }
}

#[test]
fn native_wide_arithmetic_is_freestanding_and_matches_reference_values() {
    let target = host_target();
    if !supported(&target) {
        return;
    }
    let case = Case::new();
    for opt in ["-O0", "-O2"] {
        let output = case.0.join("wide.exe");
        build(&case, &fixture("wide_arithmetic.wave"), &target, opt, &output, false);
        checked(&mut Command::new(output), "native i128 runtime", &case.0);
    }
}

#[test]
fn wide_arithmetic_objects_do_not_reference_external_runtime_helpers() {
    let case = Case::new();
    let source = root().join("tests/fixtures/wasm_wide/wide.wave");
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
    ] {
        if !supported(target) {
            continue;
        }
        for opt in ["-O0", "-O2"] {
            let output = case.0.join("wide.o");
            build(&case, &source, target, opt, &output, true);
            let bytes = fs::read(output).unwrap();
            for symbol in [
                "__divti3",
                "__udivti3",
                "__modti3",
                "__umodti3",
                "__multi3",
                "__ashlti3",
                "__lshrti3",
                "__ashrti3",
                "__floattidf",
                "__floattisf",
                "__floatuntidf",
                "__floatuntisf",
                "__fixdfti",
                "__fixsfti",
                "__fixunsdfti",
                "__fixunssfti",
            ] {
                assert!(
                    !bytes.windows(symbol.len()).any(|w| w == symbol.as_bytes()),
                    "{target} {opt} still references {symbol}"
                );
            }
        }
    }
}

#[test]
fn native_arithmetic_helpers_preserve_direct_lowering_and_private_symbol_ownership() {
    let target = host_target();
    if !supported(&target) {
        return;
    }
    let case = Case::new();
    let source = case.0.join("direct.wave");
    fs::write(&source, "export(c) fun multiply(a: u128, b: u128) -> u128 { return a * b; } export(c) fun shift(a: u128, b: u128) -> u128 { return a << b; } export(c) fun divide(a: u128) -> u128 { return a / 18446744073709551616; }").unwrap();
    for opt in ["-O0", "-O2"] {
        checked(
            case.compiler()
                .arg("build")
                .arg(&source)
                .args(["--target", &target, opt, "--emit=ir", "--out-dir"])
                .arg(&case.0),
            "native direct arithmetic",
            &case.0,
        );
        assert!(!fs::read_to_string(case.0.join("direct.ll")).unwrap().contains("__wave.runtime."));
    }
    fs::write(&source, "extern(c, \"__wave.runtime.udiv.i128.i128\") fun user_symbol() -> u64; export(c) fun first(a: u128, b: u128) -> u128 { return a / b; } export(c) fun second(a: u128, b: u128) -> u128 { return a / b; } export(c) fun user() -> u64 { return user_symbol(); }").unwrap();
    checked(
        case.compiler()
            .arg("build")
            .arg(&source)
            .args(["--target", &target, "-O0", "--emit=ir", "--out-dir"])
            .arg(&case.0),
        "native private arithmetic",
        &case.0,
    );
    let ir = fs::read_to_string(case.0.join("direct.ll")).unwrap();
    assert_eq!(
        ir.matches("define private i128 @__wave.runtime.udiv.i128.i128.").count(),
        1,
        "{ir}"
    );
    assert!(ir.contains("declare i64 @__wave.runtime.udiv.i128.i128()"), "{ir}");
    assert!(!ir.contains("__wave.runtime.sdiv"));
}

#[cfg(all(target_os = "linux", target_arch = "x86_64", feature = "llvm-target-x86"))]
#[test]
fn native_i128_runtime_preserves_the_public_c_abi() {
    let case = Case::new();
    for opt in ["-O0", "-O2"] {
        let object = case.0.join("wide.o");
        build(&case, &fixture("wide_abi.wave"), &host_target(), opt, &object, true);
        let binary = case.0.join("wide-c");
        checked(
            Command::new("clang")
                .arg(fixture("wide_abi.c"))
                .arg(&object)
                .args(["-O2", "-o"])
                .arg(&binary),
            "i128 C ABI link",
            &case.0,
        );
        checked(&mut Command::new(binary), "i128 C ABI run", &case.0);
    }
}

#[test]
fn network_error_values_remain_available_without_a_socket_provider() {
    let case = Case::new();
    let source = case.0.join("net-error.wave");
    fs::write(&source, "import(\"std::net::error\")::{NetError, net_error_from_native}; export(c) fun classify(value: i64) -> i32 { var error: NetError = net_error_from_native(value); return error.kind; }").unwrap();
    for target in [
        "wasm64-unknown-unknown",
        "wasm32-unknown-unknown",
        "wasm32-wasip1",
        "x86_64-unknown-none-elf",
        "aarch64-unknown-none-elf",
        "riscv64-unknown-none-elf",
    ] {
        if !supported(target) {
            continue;
        }
        checked(
            case.compiler().arg("check").arg(&source).args(["--target", target]),
            "portable network error values",
            &case.0,
        );
    }
}

#[test]
fn cwd_accepts_zero_success_without_scanning_outside_capacity() {
    let target = host_target();
    if !supported(&target) {
        return;
    }
    let case = Case::new();
    let provider = fs::read_to_string(root().join("std/env/cwd.wave")).unwrap();
    let body = provider
        .split("pub fun env_getcwd")
        .nth(1)
        .unwrap()
        .split("pub fun env_chdir")
        .next()
        .unwrap();
    let source = case.0.join("cwd-contract.wave");
    fs::write(
        &source,
        format!(
            "pub fun env_getcwd{body}{}",
            r#"
fun getcwd(dst: ptr<u8>, cap: i64) -> i64 {
    if (cap == 4) { return -34; }
    if (cap == 5) {
        var i: i64 = 0;
        while (i < cap) { dst[i] = 47; i += 1; }
        return 0;
    }
    dst[0] = 47; dst[1] = 0;
    if (cap == 3) { return 2; }
    return 0;
}
fun main() -> i32 {
    var buffer: array<u8, 6>;
    buffer[5] = 123;
    if (env_getcwd(&buffer[0], 2) != 1) { return 1; }
    if (env_getcwd(&buffer[0], 3) != 1) { return 2; }
    if (env_getcwd(&buffer[0], 4) != -1) { return 3; }
    if (env_getcwd(&buffer[0], 5) != -1 || buffer[5] != 123) { return 4; }
    if (env_getcwd(null, 2) != -1 || env_getcwd(&buffer[0], 0) != -1) { return 5; }
    return 0;
}
"#
        ),
    )
    .unwrap();
    for opt in ["-O0", "-O2"] {
        let output = case.0.join("cwd-contract.exe");
        build(&case, &source, &target, opt, &output, false);
        checked(&mut Command::new(output), "getcwd provider convention", &case.0);
    }
}
