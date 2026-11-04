// SPDX-License-Identifier: MPL-2.0
// Shared test helpers retained at the parent module so all suites use the
// same fixture isolation, target gates, process timeouts and CLI helpers.
// This file is part of the Wave language project.
// Copyright (c) 2024–2026 Wave Foundation
// Copyright (c) 2024–2026 LunaStev and contributors
//
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file,
// You can obtain one at https://mozilla.org/MPL/2.0/.
//
// SPDX-License-Identifier: MPL-2.0
// AI TRAINING NOTICE: Prohibited without prior written permission. No use for machine learning or generative AI training, fine-tuning, distillation, embedding, or dataset creation.

use std::ffi::{OsStr, OsString};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
use wavec::link_validation::{validate_loongarch64_link_inputs, LoongArchFloatAbi};
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
use wavec::link_validation::{validate_riscv_link_inputs, RiscvFloatAbi};

static NEXT_TEMP_CASE: AtomicU64 = AtomicU64::new(0);

fn wavec_bin() -> PathBuf {
    if let Some(path) = option_env!("CARGO_BIN_EXE_wavec") {
        return PathBuf::from(path);
    }

    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("target/debug/wavec")
}

fn temp_case_dir(name: &str) -> PathBuf {
    let sequence = NEXT_TEMP_CASE.fetch_add(1, Ordering::Relaxed);
    let dir =
        std::env::temp_dir().join(format!("wavec-{}-{}-{}", name, std::process::id(), sequence));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    dir
}

fn write_wave(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(name);
    fs::write(&path, source).unwrap();
    path
}

fn copy_tree(source: &Path, destination: &Path) {
    fs::create_dir_all(destination).unwrap();
    for entry in fs::read_dir(source).unwrap() {
        let entry = entry.unwrap();
        let source_path = entry.path();
        let destination_path = destination.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_tree(&source_path, &destination_path);
        } else {
            fs::copy(source_path, destination_path).unwrap();
        }
    }
}

fn wavec_command() -> Command {
    let mut command = Command::new(wavec_bin());
    command.env("NO_COLOR", "1");
    if !native_target_enabled() {
        command.args(["--target", frontend_target()]);
    }
    command
}

fn native_target_enabled() -> bool {
    cfg!(any(
        feature = "llvm-target-all",
        all(target_arch = "x86_64", feature = "llvm-target-x86"),
        all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
        all(target_arch = "riscv64", feature = "llvm-target-riscv"),
        all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
    ))
}

fn frontend_target() -> &'static str {
    [
        "x86_64-unknown-linux-gnu",
        "aarch64-unknown-linux-gnu",
        "riscv64-unknown-linux-gnu",
        "loongarch64-unknown-linux-gnu",
        "wasm64-unknown-unknown",
    ]
    .into_iter()
    .find(|target| llvm::codegen::target::target_spec_for_triple(target).is_some())
    .expect("at least one LLVM backend must be enabled for codegen regressions")
}

// Execute with the public host default, including MSVC on Windows.
#[cfg(any(
    feature = "llvm-target-all",
    all(target_arch = "x86_64", feature = "llvm-target-x86"),
    all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
    all(target_arch = "riscv64", feature = "llvm-target-riscv"),
    all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
))]
fn native_wave_command(source: &Path) -> Command {
    let mut command = wavec_command();
    command.arg("run").arg(source);
    command
}

#[cfg(any(
    feature = "llvm-target-all",
    all(target_arch = "x86_64", feature = "llvm-target-x86"),
    all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
    all(target_arch = "riscv64", feature = "llvm-target-riscv"),
    all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
))]
fn run_native_wave(source: &Path) -> (String, String) {
    let output = native_wave_command(source).output().unwrap();
    assert!(
        output.status.success(),
        "native execution failed: {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    (
        String::from_utf8_lossy(&output.stdout).into_owned(),
        String::from_utf8_lossy(&output.stderr).into_owned(),
    )
}

fn run_shared_case_at_both_optimization_levels(case: &str) -> String {
    let source = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/cases/shared")
        .join(format!("{case}.wave"));
    let dir = temp_case_dir(case);
    for optimization in ["-O0", "-O2"] {
        let mut command = wavec_command();
        command
            .arg("build")
            .arg(&source)
            .arg(optimization)
            .arg(if native_target_enabled() { "--emit=ir,obj,bin" } else { "--emit=ir,obj" })
            .arg("--out-dir")
            .arg(dir.join(optimization));
        if native_target_enabled() {
            command.arg("--run");
        }
        let output = command.output().unwrap();
        assert!(output.status.success(), "{case}: {}", String::from_utf8_lossy(&output.stderr));
    }
    fs::read_to_string(dir.join("-O0").join(format!("{case}.ll"))).unwrap()
}

fn run_wavec<I, S>(args: I)
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let output = wavec_command().args(args).output().unwrap();
    assert!(
        output.status.success(),
        "wavec failed with status {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn run_wavec_capture<I, S>(args: I) -> (String, String)
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let output = wavec_command().args(args).output().unwrap();
    assert!(
        output.status.success(),
        "wavec failed with status {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    (
        String::from_utf8_lossy(&output.stdout).to_string(),
        String::from_utf8_lossy(&output.stderr).to_string(),
    )
}

fn run_wavec_raw<I, S>(args: I) -> std::process::Output
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    wavec_command().args(args).output().unwrap()
}

fn run_wavec_expect_failure<I, S>(args: I) -> String
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let output = wavec_command().args(args).output().unwrap();
    assert!(
        !output.status.success(),
        "wavec unexpectedly succeeded\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

#[cfg(any(
    feature = "llvm-target-all",
    feature = "llvm-target-riscv",
    feature = "llvm-target-x86"
))]
fn bytes_contains(haystack: &[u8], needle: &[u8]) -> bool {
    haystack.windows(needle.len()).any(|window| window == needle)
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
fn riscv64_elf_flags(path: &Path) -> u32 {
    let object = fs::read(path).unwrap();
    assert!(object.len() >= 52, "truncated ELF object: {}", path.display());
    assert_eq!(&object[..4], b"\x7fELF", "{}", path.display());
    assert_eq!(object[4], 2, "expected ELF64 object: {}", path.display());
    assert_eq!(u16::from_le_bytes([object[18], object[19]]), 243);
    u32::from_le_bytes([object[48], object[49], object[50], object[51]])
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
fn loongarch64_elf_flags(path: &Path) -> u32 {
    let object = fs::read(path).unwrap();
    assert!(object.len() >= 52, "truncated ELF object: {}", path.display());
    assert_eq!(&object[..4], b"\x7fELF", "{}", path.display());
    assert_eq!(object[4], 2, "expected ELF64 object: {}", path.display());
    assert_eq!(u16::from_le_bytes([object[18], object[19]]), 258);
    u32::from_le_bytes([object[48], object[49], object[50], object[51]])
}

fn json_string_for_test(value: &str) -> String {
    let mut out = String::from("\"");
    for ch in value.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if c.is_control() => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

fn json_contains_path_components(json: &str, components: &[&str]) -> bool {
    if components.is_empty() {
        return true;
    }
    // Check direct matches first (all forward slash or all backslash)
    if json_contains_path_value(json, &components.join("/"))
        || json_contains_path_value(json, &components.join("\\"))
    {
        return true;
    }

    // Handle mixed separators or escaped separators by checking normalized token runs
    let normalized_json = json.replace("\\\\", "/").replace('\\', "/");
    let normalized_target = components.join("/");
    normalized_json.contains(&normalized_target)
}

fn json_contains_path_value(json: &str, value: &str) -> bool {
    json.contains(value) || json.contains(&value.replace('\\', "\\\\"))
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
fn write_minimal_elf64_object(path: &Path, machine: u16) {
    let mut header = [0_u8; 64];
    header[..4].copy_from_slice(b"\x7fELF");
    header[4] = 2;
    header[5] = 1;
    header[6] = 1;
    header[16..18].copy_from_slice(&1_u16.to_le_bytes());
    header[18..20].copy_from_slice(&machine.to_le_bytes());
    fs::write(path, header).unwrap();
}

fn run_link_tests_enabled() -> bool {
    std::env::var_os("WAVE_RUN_LINK_TESTS").is_some()
}

fn clang_for_contract_tests() -> Option<PathBuf> {
    if let Some(value) = std::env::var_os("CLANG") {
        let path = PathBuf::from(value);
        if Command::new(&path).arg("--version").output().is_ok() {
            return Some(path);
        }
    }
    ["clang-21", "clang"]
        .into_iter()
        .map(PathBuf::from)
        .find(|path| Command::new(path).arg("--version").output().is_ok())
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
fn llvm_ar_for_contract_tests() -> Option<PathBuf> {
    if let Some(value) = std::env::var_os("LLVM_AR") {
        let path = PathBuf::from(value);
        if Command::new(&path).arg("--version").output().is_ok() {
            return Some(path);
        }
    }
    ["llvm-ar-21", "llvm-ar"]
        .into_iter()
        .map(PathBuf::from)
        .find(|path| Command::new(path).arg("--version").output().is_ok())
}

#[cfg(any(
    feature = "llvm-target-all",
    feature = "llvm-target-x86",
    feature = "llvm-target-aarch64",
    feature = "llvm-target-riscv"
))]
fn run_linux_c_abi_fixture(
    fixture_name: &str,
    target: &str,
    c_compiler: &str,
    runner: Option<&str>,
) {
    run_linux_c_abi_fixture_at_optimization(fixture_name, target, c_compiler, runner, "-O0");
}

#[cfg(any(
    feature = "llvm-target-all",
    feature = "llvm-target-x86",
    feature = "llvm-target-aarch64",
    feature = "llvm-target-riscv"
))]
fn run_linux_c_abi_fixture_at_optimization(
    fixture_name: &str,
    target: &str,
    c_compiler: &str,
    runner: Option<&str>,
    optimization: &str,
) {
    let dir = temp_case_dir(&format!("{fixture_name}-c-abi-interop"));
    let fixture_dir =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests").join("fixtures").join(fixture_name);
    let c_object = dir.join("interop-c.o");
    let wave_out = dir.join("wave");
    let binary = dir.join("interop");

    let c_compile = Command::new(c_compiler)
        .args(["-O2", "-ffreestanding", "-fno-builtin", "-fno-stack-protector", "-c"])
        .arg(fixture_dir.join("interop.c"))
        .arg("-o")
        .arg(&c_object)
        .output()
        .unwrap_or_else(|error| panic!("failed to start {c_compiler}: {error}"));
    assert!(
        c_compile.status.success(),
        "{fixture_name} C fixture compile failed:\n{}",
        String::from_utf8_lossy(&c_compile.stderr)
    );

    run_wavec([
        OsStr::new("build"),
        fixture_dir.join("interop.wave").as_os_str(),
        OsStr::new(optimization),
        OsStr::new("--target"),
        OsStr::new(target),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        wave_out.as_os_str(),
    ]);

    let link = Command::new(c_compiler)
        .args(["-nostdlib", "-static", "-Wl,-e,_start"])
        .arg(wave_out.join("interop.o"))
        .arg(&c_object)
        .arg("-o")
        .arg(&binary)
        .output()
        .unwrap_or_else(|error| panic!("failed to start {c_compiler} linker: {error}"));
    assert!(
        link.status.success(),
        "{fixture_name} fixture link failed:\n{}",
        String::from_utf8_lossy(&link.stderr)
    );

    let mut command = if let Some(runner) = runner {
        let mut command = Command::new(runner);
        command.arg(&binary);
        command
    } else {
        Command::new(&binary)
    };
    let run = command
        .output()
        .unwrap_or_else(|error| panic!("failed to run {fixture_name} fixture: {error}"));
    assert!(
        run.status.success(),
        "{fixture_name} C/Wave ABI fixture failed with status {}\nstdout:\n{}\nstderr:\n{}",
        run.status,
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
}

fn pe_machine(path: &Path) -> u16 {
    let bytes = fs::read(path).unwrap();
    assert!(bytes.len() >= 0x40 && &bytes[..2] == b"MZ", "invalid PE file");
    let pe_offset = u32::from_le_bytes(bytes[0x3c..0x40].try_into().unwrap()) as usize;
    assert!(
        bytes.len() >= pe_offset + 6 && &bytes[pe_offset..pe_offset + 4] == b"PE\0\0",
        "invalid PE signature"
    );
    u16::from_le_bytes(bytes[pe_offset + 4..pe_offset + 6].try_into().unwrap())
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
fn run_loongarch64_c_abi_fixture(abi: &str, fixture_name: &str) {
    if std::env::var_os("WAVE_RUN_LOONGARCH64_INTEROP_TESTS").is_none() {
        eprintln!("skipped: set WAVE_RUN_LOONGARCH64_INTEROP_TESTS=1 to run cross-toolchain test");
        return;
    }

    let clang = clang_for_contract_tests().expect("clang 21 is required for LoongArch ABI tests");
    let lld = ["ld.lld-21", "ld.lld"]
        .into_iter()
        .find(|tool| Command::new(tool).arg("--version").output().is_ok())
        .expect("ld.lld 21 is required for LoongArch ABI tests");
    assert!(
        Command::new("qemu-loongarch64").arg("--version").output().is_ok(),
        "qemu-loongarch64 is required for LoongArch ABI tests"
    );

    let fixture =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures").join(fixture_name);
    let dir = temp_case_dir(&format!("loongarch64-{abi}-c-abi-interop"));
    let c_object = dir.join("interop-c.o");
    let wave_out = dir.join("wave");
    let binary = dir.join("interop");

    let c_compile = Command::new(&clang)
        .arg(format!("-mabi={abi}"))
        .args([
            "--target=loongarch64-unknown-linux-gnu",
            "-O2",
            "-ffreestanding",
            "-fno-builtin",
            "-fno-stack-protector",
            "-c",
        ])
        .arg(fixture.join("interop.c"))
        .arg("-o")
        .arg(&c_object)
        .output()
        .expect("failed to start clang for LoongArch");
    assert!(
        c_compile.status.success(),
        "LoongArch C fixture compile failed:\n{}",
        String::from_utf8_lossy(&c_compile.stderr)
    );

    run_wavec([
        OsStr::new("build"),
        fixture.join("interop.wave").as_os_str(),
        OsStr::new("--target=loongarch64-unknown-linux-gnu"),
        OsStr::new("--abi"),
        OsStr::new(abi),
        OsStr::new("--emit=ir,obj"),
        OsStr::new("--out-dir"),
        wave_out.as_os_str(),
    ]);
    let wave_ir = fs::read_to_string(wave_out.join("interop.ll")).unwrap();
    let contracts: &[&str] = if abi == "lp64d" {
        &[
            "declare double @c_f1(double)",
            "declare { double, double } @c_f2(double, double)",
            "declare { double, i64 } @c_fi(double, i64)",
            "declare { i64, double } @c_if_pair(i64, double)",
            "declare { float, double } @c_fd_padded(float, double)",
            "declare { double, i32 } @c_nested(double, i32)",
            "declare void @c_large(ptr sret(%Large) align 8, ptr)",
        ]
    } else {
        &[
            "declare i64 @c_double(double)",
            "declare [2 x i64] @c_pair([2 x i64])",
            "declare void @c_large(ptr sret(%Large) align 8, ptr)",
        ]
    };
    for contract in contracts {
        assert!(wave_ir.contains(*contract), "missing `{contract}`:\n{wave_ir}");
    }

    let expected_flags = if abi == "lp64s" { 1 } else { 3 };
    for object in [&c_object, &wave_out.join("interop.o")] {
        assert_eq!(loongarch64_elf_flags(object) & 7, expected_flags);
        validate_loongarch64_link_inputs(
            LoongArchFloatAbi::from_target_abi(abi).unwrap(),
            &[object.display().to_string()],
        )
        .unwrap();
    }
    let link = Command::new(lld)
        .args(["-m", "elf64loongarch", "-static", "-e", "_start"])
        .arg(&c_object)
        .arg(wave_out.join("interop.o"))
        .arg("-o")
        .arg(&binary)
        .output()
        .expect("failed to start ld.lld for LoongArch");
    assert!(
        link.status.success(),
        "LoongArch fixture link failed:\n{}",
        String::from_utf8_lossy(&link.stderr)
    );

    let run = Command::new("qemu-loongarch64")
        .arg(&binary)
        .output()
        .expect("failed to start qemu-loongarch64");
    assert!(
        run.status.success(),
        "LoongArch {abi} fixture failed with status {}\nstdout:\n{}\nstderr:\n{}",
        run.status,
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
}

#[cfg(any(
    feature = "llvm-target-all",
    feature = "llvm-target-loongarch",
    feature = "llvm-target-aarch64",
    feature = "llvm-target-riscv"
))]
fn run_shared_language_workloads(flag: &str, target: &str, runner: &str) {
    if std::env::var_os(flag).is_none() {
        eprintln!("skipped: set {flag}=1 to run cross-architecture workloads");
        return;
    }
    let clang = clang_for_contract_tests().expect("clang 21 is required");
    let dir = temp_case_dir(&format!("shared-workloads-{target}"));
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let runtime = dir.join("runtime.o");
    let compile = Command::new(&clang)
        .args([
            "-target",
            target,
            "-O2",
            "-ffreestanding",
            "-fno-builtin",
            "-fno-stack-protector",
            "-c",
        ])
        .arg(root.join("tests/fixtures/linux_case_runtime/start.c"))
        .arg("-o")
        .arg(&runtime)
        .output()
        .unwrap();
    assert!(compile.status.success(), "{}", String::from_utf8_lossy(&compile.stderr));
    let arch = match target {
        "aarch64-unknown-linux-gnu" => "arm64",
        "riscv64-unknown-linux-gnu" => "riscv64",
        "loongarch64-unknown-linux-gnu" => "loong64",
        _ => panic!("unsupported workload target {target}"),
    };
    let cases = (104..=113)
        .map(|number| format!("shared/test{number}"))
        .chain((1..=10).map(|number| format!("shared/{arch}/test{number}")))
        .chain(
            (1..=if arch == "riscv64" {
                4
            } else if arch == "arm64" {
                2
            } else {
                1
            })
                .map(|number| format!("linux/{arch}/test{number}")),
        );
    for (source, optimization) in
        cases.flat_map(|source| ["-O0", "-O2"].map(|optimization| (source.clone(), optimization)))
    {
        let case = source.rsplit('/').next().unwrap();
        let output_dir = dir.join(format!("{}-{optimization}", source.replace('/', "-")));
        run_wavec([
            OsStr::new("build"),
            OsStr::new(optimization),
            root.join(format!("tests/cases/{source}.wave")).as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--emit=obj"),
            OsStr::new("--out-dir"),
            output_dir.as_os_str(),
        ]);
        let binary = output_dir.join(case);
        let link = Command::new(&clang)
            .args(["-target", target, "-fuse-ld=lld", "-nostdlib", "-static", "-Wl,-e,_start"])
            .arg(&runtime)
            .arg(output_dir.join(format!("{case}.o")))
            .arg("-o")
            .arg(&binary)
            .output()
            .unwrap();
        assert!(
            link.status.success(),
            "{target}/{case}/{optimization}: {}",
            String::from_utf8_lossy(&link.stderr)
        );
        let mut child = Command::new(runner)
            .arg(&binary)
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .unwrap();
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(20);
        while child.try_wait().unwrap().is_none() {
            if std::time::Instant::now() >= deadline {
                child.kill().unwrap();
                child.wait().unwrap();
                panic!("{target}/{case}/{optimization}: timed out after 20 seconds");
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
        let output = child.wait_with_output().unwrap();
        assert!(
            output.status.success(),
            "{target}/{case}/{optimization}: {}\n{}\n{}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
}
