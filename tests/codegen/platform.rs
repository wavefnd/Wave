// SPDX-License-Identifier: MPL-2.0
//! Targets, OS providers, and runtime regression cases.
//! These tests share the parent integration suite's fixtures and target gates.
use super::*;

#[test]
#[cfg(any(
    feature = "llvm-target-all",
    all(target_arch = "x86_64", feature = "llvm-target-x86"),
    all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
    all(target_arch = "riscv64", feature = "llvm-target-riscv"),
    all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
))]
fn os_randomness_preserves_bounds_and_partial_failure_counts() {
    let dir = temp_case_dir("os-randomness");
    let home = dir.join("home");
    let std = home.join(".wave/lib/wave/std");
    copy_tree(&Path::new(env!("CARGO_MANIFEST_DIR")).join("std"), &std);
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/cases/shared/test127.wave");
    let output = wavec_command()
        .env("HOME", &home)
        .arg("build")
        .arg(source)
        .arg("--run")
        .arg("--out-dir")
        .arg(dir.join("native"))
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    fs::write(
        std.join("sys/random.wave"),
        r#"
static step: i32 = 0;
pub fun sys_random_available() -> bool { return true; }
pub fun sys_random_read(buffer: ptr<u8>, size: i64) -> i64 {
    step += 1;
    if (step == 1) { buffer[0] = 31; buffer[1] = 32; return 2; }
    if (step == 2) { return -4; }
    if (step == 3) { buffer[0] = 33; return 1; }
    return -5;
}
"#,
    )
    .unwrap();
    let source = write_wave(
        &dir,
        "partial.wave",
        r#"
import("std::random::fill")::{RandomFillResult, random_fill};
fun main() -> i32 {
    var data: array<u8, 5> = [85, 85, 85, 85, 85];
    var result: RandomFillResult = random_fill(&data[0], 5);
    if (result.ok || result.written != 3 || result.error != -5) { return 1; }
    if (data[0] != 31 || data[1] != 32 || data[2] != 33 || data[3] != 85 || data[4] != 85) { return 2; }
    return 0;
}
"#,
    );
    let output = wavec_command()
        .env("HOME", &home)
        .arg("build")
        .arg(source)
        .arg("--run")
        .arg("--out-dir")
        .arg(dir.join("partial"))
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
}

#[test]
fn linux_numeric_and_explicit_table_resolvers_are_libc_independent() {
    let dir = temp_case_dir("linux-numeric-resolver");
    let home = dir.join("home");
    copy_tree(&Path::new(env!("CARGO_MANIFEST_DIR")).join("std"), &home.join(".wave/lib/wave/std"));
    let fixture = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/linux_resolver");
    for target in [
        "x86_64-unknown-linux-gnu",
        "aarch64-unknown-linux-gnu",
        "riscv64-unknown-linux-gnu",
        "loongarch64-unknown-linux-gnu",
    ] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        let output = wavec_command()
            .env("HOME", &home)
            .arg("build")
            .arg(fixture.join("numeric_and_table.wave"))
            .arg("--target")
            .arg(target)
            .arg("--emit=ir,obj")
            .arg("-O2")
            .arg("--out-dir")
            .arg(dir.join(target))
            .output()
            .unwrap();
        assert!(output.status.success(), "{target}: {}", String::from_utf8_lossy(&output.stderr));
        let ir = fs::read_to_string(dir.join(target).join("numeric_and_table.ll")).unwrap();
        for symbol in ["getaddrinfo", "freeaddrinfo", "getpagesize"] {
            assert!(!ir.contains(&format!("@{symbol}(")), "unexpected libc import {symbol}");
        }
    }
    if cfg!(all(target_os = "linux", target_arch = "x86_64")) && native_target_enabled() {
        let runtime = dir.join("start.o");
        let compile = Command::new("cc")
            .args(["-ffreestanding", "-fno-builtin", "-fno-stack-protector", "-c"])
            .arg(fixture.join("start.c"))
            .arg("-o")
            .arg(&runtime)
            .output()
            .unwrap();
        assert!(compile.status.success(), "{}", String::from_utf8_lossy(&compile.stderr));
        let executable = dir.join("resolver");
        let link = Command::new("cc")
            .args(["-nostdlib", "-static", "-Wl,-e,_start"])
            .arg(runtime)
            .arg(dir.join("x86_64-unknown-linux-gnu/numeric_and_table.o"))
            .arg("-o")
            .arg(&executable)
            .output()
            .unwrap();
        assert!(link.status.success(), "{}", String::from_utf8_lossy(&link.stderr));
        assert!(Command::new(executable).status().unwrap().success());
    }
}

#[test]
fn windows_filesystem_errors_and_unicode_paths_use_native_apis() {
    let dir = temp_case_dir("windows-filesystem");
    let home = dir.join("home");
    copy_tree(&Path::new(env!("CARGO_MANIFEST_DIR")).join("std"), &home.join(".wave/lib/wave/std"));
    for fixture in ["errors", "unicode"] {
        let source = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join(format!("tests/fixtures/windows_fs/{fixture}.wave"));
        for target in ["x86_64-pc-windows-msvc", "aarch64-pc-windows-msvc"] {
            if llvm::codegen::target::target_spec_for_triple(target).is_none() {
                continue;
            }
            let destination = dir.join(format!("{fixture}-{target}"));
            let output = wavec_command()
                .env("HOME", &home)
                .arg("build")
                .arg(&source)
                .arg("--target")
                .arg(target)
                .arg("--emit=ir,obj")
                .arg("--out-dir")
                .arg(&destination)
                .output()
                .unwrap();
            assert!(
                output.status.success(),
                "{target}: {}",
                String::from_utf8_lossy(&output.stderr)
            );
            let ir = fs::read_to_string(destination.join(format!("{fixture}.ll"))).unwrap();
            for symbol in
                ["CreateFileW", "MultiByteToWideChar", "WideCharToMultiByte", "GetLastError"]
            {
                assert!(ir.contains(&format!("@{symbol}(")), "missing {symbol}");
            }
            assert!(!ir.contains("@CreateFileA("));
        }
        if cfg!(target_os = "windows") {
            let output = dir.join(format!("{fixture}.exe"));
            let compile = wavec_command()
                .env("HOME", &home)
                .arg("build")
                .arg(&source)
                .arg("-o")
                .arg(&output)
                .output()
                .unwrap();
            assert!(compile.status.success(), "{}", String::from_utf8_lossy(&compile.stderr));
            let run = Command::new(output).current_dir(&dir).output().unwrap();
            assert!(
                run.status.success(),
                "{fixture}: {:?}\n{}",
                run.status,
                String::from_utf8_lossy(&run.stderr)
            );
        }
    }
}

#[cfg(any(feature = "llvm-target-wasm", feature = "llvm-target-all"))]
#[test]
fn wasm_reclaiming_allocators_emit_for_both_pointer_widths() {
    let dir = temp_case_dir("wasm-reclaiming-allocators");
    let home = dir.join("home");
    copy_tree(&Path::new(env!("CARGO_MANIFEST_DIR")).join("std"), &home.join(".wave/lib/wave/std"));
    for (target, case, intrinsic) in [
        ("wasm32-unknown-unknown", "wasm/wasm32/test11.wave", "llvm.wasm.memory.grow.i32"),
        ("wasm32-wasip1", "wasi/wasm32/test11.wave", "llvm.wasm.memory.grow.i32"),
        ("wasm64-unknown-unknown", "wasm/wasm64/test12.wave", "llvm.wasm.memory.grow.i64"),
    ] {
        let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/cases").join(case);
        let output = wavec_command()
            .env("HOME", &home)
            .arg("build")
            .arg(source)
            .arg("--target")
            .arg(target)
            .arg("--emit=ir,obj")
            .arg("--out-dir")
            .arg(dir.join(target))
            .output()
            .unwrap();
        assert!(output.status.success(), "{target}: {}", String::from_utf8_lossy(&output.stderr));
        let ir = fs::read_to_string(
            dir.join(target).join(Path::new(case).file_stem().unwrap()).with_extension("ll"),
        )
        .unwrap();
        assert!(ir.contains(intrinsic), "{target}: {ir}");
    }
}

#[test]
fn freebsd_lp64_providers_emit_direct_syscalls_and_kernel_layouts() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let dir = temp_case_dir("freebsd-raw-providers");
    let home = dir.join("home");
    copy_tree(&root.join("std"), &home.join(".wave/lib/wave/std"));
    for (arch, target, instruction, registers) in [
        ("amd64", "x86_64-unknown-freebsd", "syscall", ["={rax}", "={rdx}"]),
        ("arm64", "aarch64-unknown-freebsd", "svc $3", ["={x0}", "={x1}"]),
        ("riscv64", "riscv64-unknown-freebsd", "ecall", ["={x10}", "={x11}"]),
    ] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        for number in [11, 12] {
            let source = root.join(format!("tests/cases/freebsd/{arch}/test{number}.wave"));
            for optimization in ["-O0", "-O2"] {
                let output_dir = dir.join(arch).join(number.to_string()).join(optimization);
                for emit in ["--emit=ir", "--emit=obj"] {
                    let output = wavec_command()
                        .env("HOME", &home)
                        .arg("build")
                        .arg(&source)
                        .args(["--target", target, emit, optimization])
                        .arg("--out-dir")
                        .arg(&output_dir)
                        .output()
                        .unwrap();
                    assert!(
                        output.status.success(),
                        "{target} test{number} {optimization} {emit}:\n{}\n{}",
                        String::from_utf8_lossy(&output.stdout),
                        String::from_utf8_lossy(&output.stderr)
                    );
                    assert!(
                        output.stderr.is_empty(),
                        "{target} must compile without backend diagnostics: {}",
                        String::from_utf8_lossy(&output.stderr)
                    );
                }
                let ir = fs::read_to_string(output_dir.join(format!("test{number}.ll"))).unwrap();
                assert!(ir.contains(instruction), "{target} must issue a kernel trap");
                for register in registers {
                    assert!(
                        ir.contains(register),
                        "{target} must declare kernel return register {register}"
                    );
                }
                for line in ir.lines().filter(|line| line.starts_with("declare ")) {
                    assert!(
                        line.contains("@llvm."),
                        "{target} raw sys provider acquired an external dependency: {line}"
                    );
                }
                if optimization == "-O0" && number == 12 {
                    assert!(
                        ir.contains(
                            "NativeKevent = type { i64, i16, i16, i32, i64, ptr, [4 x i64] }"
                        ),
                        "FreeBSD kevent requires ext[4] and a 64-byte stride"
                    );
                    assert!(
                        ir.contains("NativeMessage = type { ptr, i32, ptr, i32, ptr, i32, i32 }"),
                        "FreeBSD msghdr uses 32-bit socklen_t and iovlen"
                    );
                }
            }
        }
    }
}

#[test]
#[cfg(feature = "llvm-target-wasm")]
fn webassembly_targets_plan_modules_with_their_pointer_width() {
    let dir = temp_case_dir("wasm32-module-plan");
    let source = write_wave(
        &dir,
        "module.wave",
        r#"
fun add(a: i32, b: i32) -> i32 {
    return a + b;
}

fun main() -> i32 {
    return add(20, 22);
}
"#,
    );
    let out_dir = dir.join("out");
    let (plan, stderr) = run_wavec_capture([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm32-unknown-unknown"),
        OsStr::new("--dry-run"),
        OsStr::new("--error-format=json"),
        OsStr::new("--out-dir"),
        out_dir.as_os_str(),
    ]);
    assert!(stderr.trim().is_empty(), "unexpected stderr:\n{stderr}");
    assert!(plan.contains("\"target\":\"wasm32-unknown-unknown\""), "{plan}");
    assert!(plan.contains("\"program\":") && plan.contains("wasm-ld"), "{plan}");
    assert!(plan.contains("--no-entry"), "{plan}");
    assert!(plan.contains("--allow-undefined"), "{plan}");
    assert!(plan.contains("--export-if-defined=main"), "{plan}");
    assert!(plan.contains("--export-memory"), "{plan}");
    assert!(
        plan.contains(&json_string_for_test(&out_dir.join("module.wasm").to_string_lossy())),
        "{plan}"
    );

    let (run_plan, stderr) = run_wavec_capture([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm32-unknown-unknown"),
        OsStr::new("--run"),
        OsStr::new("--dry-run"),
        OsStr::new("--error-format=json"),
    ]);
    assert!(stderr.trim().is_empty(), "{stderr}");
    assert!(run_plan.contains("\"program\":\"node\""), "{run_plan}");
    assert!(run_plan.contains("--input-type=module"), "{run_plan}");

    let wasm64_out = dir.join("wasm64-out");
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm64-unknown-unknown"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        wasm64_out.as_os_str(),
    ]);
    let wasm64_ir = fs::read_to_string(wasm64_out.join("module.ll")).unwrap();
    assert!(
        wasm64_ir.contains("target triple = \"wasm64-unknown-unknown\"")
            && wasm64_ir.contains("p:64:64"),
        "{wasm64_ir}"
    );

    let (wasm64_plan, stderr) = run_wavec_capture([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm64-unknown-unknown"),
        OsStr::new("--run"),
        OsStr::new("--dry-run"),
        OsStr::new("--error-format=json"),
    ]);
    assert!(stderr.trim().is_empty(), "{stderr}");
    assert!(wasm64_plan.contains("-mwasm64"), "{wasm64_plan}");
    let parsed = utils::wson::parse_json(&wasm64_plan).unwrap();
    let Some(utils::wson::Value::Array(args)) = parsed.get("execute").unwrap().get("args") else {
        panic!("missing WebAssembly host arguments: {wasm64_plan}");
    };
    assert!(
        !args.contains(&utils::wson::Value::string("--experimental-wasm-memory64")),
        "memory64 host flags must be selected at execution time: {wasm64_plan}"
    );

    let asm_source =
        write_wave(&dir, "inline_asm.wave", "fun main() {\n    asm {\n        \"nop\"\n    }\n}\n");
    let asm_error = run_wavec_expect_failure([
        OsStr::new("build"),
        asm_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm32-unknown-unknown"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        out_dir.as_os_str(),
    ]);
    assert!(
        asm_error.contains("inline assembly is not supported for webassembly wasm32 unknown"),
        "{asm_error}"
    );
}

#[test]
#[cfg(feature = "llvm-target-wasm")]
fn wasm_host_output_preserves_bytes_numeric_formatting_and_exit_status() {
    for tool in ["node", "wasm-ld"] {
        if !Command::new(tool).arg("--version").output().is_ok_and(|output| output.status.success())
        {
            eprintln!("skipping WebAssembly output execution: {tool} unavailable");
            return;
        }
    }
    let host_tests = Command::new("node")
        .arg("--test")
        .arg(Path::new(env!("CARGO_MANIFEST_DIR")).join("tools/test_wasm_host.mjs"))
        .output()
        .unwrap();
    assert!(
        host_tests.status.success(),
        "{}\n{}",
        String::from_utf8_lossy(&host_tests.stdout),
        String::from_utf8_lossy(&host_tests.stderr)
    );
    let dir = temp_case_dir("wasm-host-output");
    let source = write_wave(
        &dir,
        "output.wave",
        r#"
extern(c) fun puts(s: ptr<i8>) -> i32;
fun main() -> i32 {
    var zero: f64 = -0.0;
    var large: u128 = 340282366920938463463374607431768211455;
    print("한글 {} {c} {} {} {} {} 100%", "text", 65, -42, large, zero, 0.0078125 as f64);
    puts(" host puts");
    return 7;
}
"#,
    );
    let expected = "한글 text A -42 340282366920938463463374607431768211455 -0.000000 0.007812 100% host puts\n";
    for target in ["wasm32-unknown-unknown", "wasm32-wasip1", "wasm64-unknown-unknown"] {
        for optimization in ["-O0", "-O2"] {
            let output = wavec_command()
                .arg("build")
                .arg(&source)
                .args(["--target", target, optimization, "--run", "--std-root"])
                .arg(Path::new(env!("CARGO_MANIFEST_DIR")).join("std"))
                .arg("--out-dir")
                .arg(dir.join(target))
                .output()
                .unwrap();
            assert_eq!(
                output.status.code(),
                Some(7),
                "{target} {optimization}: {}",
                String::from_utf8_lossy(&output.stderr)
            );
            assert_eq!(output.stdout, expected.as_bytes(), "{target} {optimization}");
        }
    }
    fs::remove_dir_all(dir).unwrap();
}

#[test]
#[cfg(feature = "llvm-target-wasm")]
fn wasi_runner_preserves_explicit_process_exit_status() {
    for tool in ["node", "wasm-ld"] {
        if !Command::new(tool).arg("--version").output().is_ok_and(|output| output.status.success())
        {
            eprintln!("skipping WASI execution: {tool} unavailable");
            return;
        }
    }
    let dir = temp_case_dir("wasi-exit-status");
    for status in [0, 7] {
        let source = write_wave(
            &dir,
            "exit.wave",
            &format!(
                "extern(c, \"proc_exit\") fun exit(code: u32);\n\
                 fun main() -> i32 {{ exit({status}); return 0; }}\n"
            ),
        );
        let output = wavec_command()
            .arg("build")
            .arg(&source)
            .args(["--target", "wasm32-wasip1", "--run", "--out-dir"])
            .arg(dir.join("out"))
            .output()
            .unwrap();
        assert_eq!(
            output.status.code(),
            Some(status),
            "stdout: {}\nstderr: {}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
    fs::remove_dir_all(dir).unwrap();
}

#[test]
fn target_attribute_supports_arch_os_env_and_abi_conditions() {
    let dir = temp_case_dir("target-attr-conditions");
    let src = write_wave(
        &dir,
        "select.wave",
        r#"
#[target(arch="x86_64", os="none", env="none")]
fun selected() -> i32 { return 64; }
#[target(arch="aarch64", os="none", env="none")]
fun selected() -> i32 { return 128; }
#[target(arch="riscv64", os="none", env="none", abi="lp64d")]
fun selected() -> i32 { return 255; }
#[target(arch="loongarch64", os="linux")]
fun selected() -> i32 { return 512; }
#[target(arch="wasm64")]
fun selected() -> i32 { return 1024; }
fun main() -> i32 { return selected(); }
"#,
    );
    for (target, abi, value) in [
        ("x86_64-unknown-none-elf", None, 64),
        ("aarch64-unknown-none-elf", None, 128),
        ("riscv64-unknown-none-elf", Some("lp64d"), 255),
        ("loongarch64-unknown-linux-gnu", None, 512),
        ("wasm64-unknown-unknown", None, 1024),
    ] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        let out = dir.join(target);
        let mut command = wavec_command();
        command
            .arg("build")
            .arg(&src)
            .args(["--target", target, "--emit=ir"])
            .arg("--out-dir")
            .arg(&out);
        if let Some(abi) = abi {
            command.args(["--abi", abi]);
        }
        let result = command.output().unwrap();
        assert!(result.status.success(), "{target}: {}", String::from_utf8_lossy(&result.stderr));
        let ir = fs::read_to_string(out.join("select.ll")).unwrap();
        for other in [64, 128, 255, 512, 1024] {
            assert_eq!(ir.contains(&format!("ret i32 {other}")), other == value, "{target}: {ir}");
        }
    }
}

#[test]
fn freebsd_link_plans_use_target_sysroot_and_elf_loader() {
    let dir = temp_case_dir("freebsd-link-plans");
    let source = write_wave(&dir, "main.wave", "fun main() -> i32 { return 0; }\n");
    let sysroot = dir.join("sysroot");
    fs::create_dir_all(sysroot.join("usr/lib")).unwrap();
    for file in ["crt1.o", "Scrt1.o", "crti.o", "crtn.o"] {
        fs::write(sysroot.join("usr/lib").join(file), []).unwrap();
    }
    for (target, emulation) in [
        ("x86_64-unknown-freebsd", "elf_x86_64"),
        ("aarch64-unknown-freebsd", "aarch64elf"),
        ("riscv64-unknown-freebsd", "elf64lriscv"),
    ] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        for (mode, start) in [("--no-pie", "crt1.o"), ("--pie", "Scrt1.o")] {
            let (plan, stderr) = run_wavec_capture([
                OsStr::new("--error-format=json"),
                OsStr::new("build"),
                source.as_os_str(),
                OsStr::new("--target"),
                OsStr::new(target),
                OsStr::new("--sysroot"),
                sysroot.as_os_str(),
                OsStr::new(mode),
                OsStr::new("--emit=bin"),
                OsStr::new("--dry-run"),
            ]);
            assert!(stderr.is_empty(), "{stderr}");
            assert!(plan.contains(&format!("\"-m\",\"{emulation}\"")), "{plan}");
            assert!(plan.contains("--dynamic-linker=/libexec/ld-elf.so.1"), "{plan}");
            for file in [start, "crti.o", "crtn.o"] {
                assert!(
                    json_contains_path_components(&plan, &["sysroot", "usr", "lib", file]),
                    "{plan}"
                );
            }
            assert!(!plan.contains("ld-linux") && !plan.contains("unknown-linux-gnu"), "{plan}");
        }
    }
}

#[test]
fn hosted_linux_link_plans_use_wave_crt_for_every_architecture_and_mode() {
    let dir = temp_case_dir("bundled-linux-crt-matrix");
    let source = write_wave(&dir, "main.wave", "fun main() -> i32 { return 0; }\n");

    for (target, abi) in [
        ("x86_64-unknown-linux-gnu", None),
        ("aarch64-unknown-linux-gnu", None),
        ("riscv64-unknown-linux-gnu", Some("lp64")),
        ("riscv64-unknown-linux-gnu", Some("lp64f")),
        ("riscv64-unknown-linux-gnu", Some("lp64d")),
        #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
        ("loongarch64-unknown-linux-gnu", Some("lp64s")),
        #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
        ("loongarch64-unknown-linux-gnu", Some("lp64d")),
    ] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        for (options, object_name) in [
            (Vec::<&str>::new(), "crt1.o"),
            (vec!["--pie"], "Scrt1.o"),
            (vec!["--static", "--pie"], "rcrt1.o"),
        ] {
            let mut args = vec![
                OsString::from("--error-format=json"),
                OsString::from("build"),
                source.as_os_str().to_os_string(),
                OsString::from("--target"),
                OsString::from(target),
            ];
            if let Some(abi) = abi {
                args.push(OsString::from(format!("--abi={abi}")));
            }
            args.extend(options.into_iter().map(OsString::from));
            args.push(OsString::from("--emit=bin"));
            args.push(OsString::from("--dry-run"));

            let output = run_wavec_raw(args);
            assert!(
                output.status.success(),
                "{target} {object_name} dry-run failed:\nstdout:\n{}\nstderr:\n{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            );
            let stdout = String::from_utf8_lossy(&output.stdout);
            let mut components = vec!["crt", target];
            if let Some(abi) = abi {
                components.push(abi);
            }
            components.push(object_name);
            assert!(
                json_contains_path_components(&stdout, &components),
                "{target} must select Wave's {object_name}:\n{stdout}"
            );
            let mut crti_components = vec!["crt", target];
            let mut crtn_components = vec!["crt", target];
            if let Some(abi) = abi {
                crti_components.push(abi);
                crtn_components.push(abi);
            }
            crti_components.push("crti.o");
            crtn_components.push("crtn.o");
            assert!(
                json_contains_path_components(&stdout, &crti_components)
                    && json_contains_path_components(&stdout, &crtn_components),
                "{target} must select Wave's crti.o and crtn.o:\n{stdout}"
            );
        }
    }
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
fn riscv64_debian_cross_prefix_does_not_double_apply_linker_sysroot() {
    let dir = temp_case_dir("riscv64-debian-cross-prefix");
    let source = write_wave(&dir, "main.wave", "fun main() -> i32 { return 0; }\n");
    let sysroot = dir.join("usr").join("riscv64-linux-gnu");
    let runtime = sysroot.join("lib");
    fs::create_dir_all(&runtime).unwrap();

    for crt in ["crt1.o", "crti.o", "crtn.o"] {
        write_minimal_elf64_object(&runtime.join(crt), 243);
    }
    let runtime_prefix = runtime.to_string_lossy();
    fs::write(
        runtime.join("libc.so"),
        format!(
            "GROUP ( {runtime_prefix}/libc.so.6 {runtime_prefix}/libc_nonshared.a \
             AS_NEEDED ( {runtime_prefix}/ld-linux-riscv64-lp64d.so.1 ) )\n"
        ),
    )
    .unwrap();
    fs::write(runtime.join("libm.so"), format!("GROUP ( {runtime_prefix}/libm.so.6 )\n")).unwrap();
    for runtime_file in
        ["libc.so.6", "libc_nonshared.a", "libm.so.6", "ld-linux-riscv64-lp64d.so.1"]
    {
        fs::write(runtime.join(runtime_file), []).unwrap();
    }

    let (target_spec, stderr) = run_wavec_capture([
        OsStr::new("--sysroot"),
        sysroot.as_os_str(),
        OsStr::new("print"),
        OsStr::new("target-spec"),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--format=json"),
    ]);
    assert!(stderr.trim().is_empty(), "{}", stderr);
    assert!(target_spec.contains("\"sysroot_source\":\"explicit\""), "{}", target_spec);
    assert!(json_contains_path_value(&target_spec, &sysroot.to_string_lossy()), "{}", target_spec);

    let (stdout, stderr) = run_wavec_capture([
        OsStr::new("--error-format=json"),
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--sysroot"),
        sysroot.as_os_str(),
        OsStr::new("--emit=bin"),
        OsStr::new("--dry-run"),
    ]);
    assert!(stderr.trim().is_empty(), "{}", stderr);
    assert!(stdout.contains("\"sysroot_source\":\"explicit\""), "{}", stdout);
    assert!(stdout.contains("--sysroot=/"), "{}", stdout);
    assert!(
        !stdout.contains(&format!("--sysroot={}", sysroot.display())),
        "cross prefix must not be applied twice by ld.lld:\n{}",
        stdout
    );
    assert!(
        json_contains_path_value(&stdout, &format!("-L{}", runtime.display())),
        "target runtime search path must remain isolated to the cross prefix:\n{}",
        stdout
    );
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-x86"))]
fn freestanding_codegen_marks_functions_no_red_zone() {
    let dir = temp_case_dir("freestanding-noredzone");
    let src = write_wave(
        &dir,
        "leaf.wave",
        r#"
fun leaf(a: i64, b: i64, c: i64, d: i64, e: i64) -> i64 {
    var x: i64 = a + b;
    var y: i64 = c + d;
    return x + y + e;
}
"#,
    );

    let explicit_dir = dir.join("explicit");
    run_wavec([
        OsStr::new("build"),
        src.as_os_str(),
        OsStr::new("--freestanding"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        explicit_dir.as_os_str(),
    ]);
    let explicit_out = explicit_dir.join("leaf.ll");
    let explicit_ir = fs::read_to_string(&explicit_out).unwrap();
    assert!(
        explicit_ir.contains("noredzone"),
        "--freestanding IR must carry the LLVM noredzone function attribute:\n{}",
        explicit_ir
    );
    assert!(
        explicit_ir.contains("nounwind"),
        "--freestanding IR must mark Wave functions nounwind:\n{}",
        explicit_ir
    );

    let bare_dir = dir.join("bare");
    run_wavec([
        OsStr::new("build"),
        src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-none-elf"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        bare_dir.as_os_str(),
    ]);
    let bare_out = bare_dir.join("leaf.ll");
    let bare_ir = fs::read_to_string(&bare_out).unwrap();
    assert!(
        bare_ir.contains("noredzone"),
        "bare-metal target IR must carry the LLVM noredzone function attribute:\n{}",
        bare_ir
    );
    assert!(
        bare_ir.contains("nounwind"),
        "bare-metal target IR must mark Wave functions nounwind:\n{}",
        bare_ir
    );
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-x86"))]
fn arithmetic_helpers_keep_target_and_freestanding_function_attributes() {
    let dir = temp_case_dir("runtime-function-attributes");
    let source = write_wave(
        &dir,
        "runtime.wave",
        r#"
fun divide(a: u128, b: u128) -> u128 { return a / b; }
fun remainder(a: u128, b: u128) -> u128 { return a % b; }
fun convert(a: u128) -> f64 { return a as f64; }
"#,
    );
    for (mode, target, freestanding) in [
        ("bare", "x86_64-unknown-none-elf", false),
        ("explicit", "x86_64-unknown-linux-gnu", true),
        ("hosted", "x86_64-unknown-linux-gnu", false),
    ] {
        for optimization in ["-O0", "-O2"] {
            let out = dir.join(mode).join(optimization);
            let mut command = wavec_command();
            command
                .arg("build")
                .arg(&source)
                .arg("--target")
                .arg(target)
                .args(["--cpu=x86-64-v3", "--features=+avx2", optimization, "--emit=ir,obj"])
                .arg("--out-dir")
                .arg(&out);
            if freestanding {
                command.arg("--freestanding");
            }
            let output = command.output().unwrap();
            assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
            let ir = fs::read_to_string(out.join("runtime.ll")).unwrap();
            let helpers: Vec<_> =
                ir.lines().filter(|line| line.starts_with("define private")).collect();
            assert_eq!(helpers.len(), 3, "{ir}");
            for helper in helpers {
                let group = helper.split_whitespace().find(|word| word.starts_with('#')).unwrap();
                let prefix = format!("attributes {group} =");
                let attributes = ir.lines().find(|line| line.starts_with(&prefix)).unwrap();
                assert!(
                    attributes.contains("\"target-cpu\"=\"x86-64-v3\""),
                    "{helper}: {attributes}"
                );
                assert!(
                    attributes.contains("\"target-features\"=\"+avx2\""),
                    "{helper}: {attributes}"
                );
                assert_eq!(attributes.contains("noredzone"), mode != "hosted", "{attributes}");
                if mode != "hosted" {
                    assert!(attributes.contains("nounwind"), "{attributes}");
                }
            }
        }
    }
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
fn riscv64_inline_asm_enforces_reserved_registers_fprs_and_indirect_jumps() {
    let dir = temp_case_dir("riscv64-asm-contract");

    for reserved in ["sp", "zero"] {
        let source = write_wave(
            &dir,
            &format!("reserved_{}.wave", reserved),
            &format!(
                r#"
fun bind(value: u64) {{
    asm {{
        "nop"
        in("{reserved}") value
    }}
}}

fun main() {{}}
"#
            ),
        );
        let reserved_out = dir.join(format!("reserved-{}", reserved));
        let error = run_wavec_expect_failure([
            OsStr::new("--error-format=json"),
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-linux-gnu"),
            OsStr::new("--emit=obj"),
            OsStr::new("--out-dir"),
            reserved_out.as_os_str(),
        ]);
        assert!(error.contains("\"code\":\"E3401\""), "{}", error);
        assert!(!error.contains("compiler internal error"), "{}", error);
    }

    let float_source = write_wave(
        &dir,
        "float_register.wave",
        r#"
fun consume(value: f64) {
    asm {
        "fmv.d fa0, fa0"
        in("fa0") value
    }
}

fun main() {}
"#,
    );
    let float_out = dir.join("float-register");
    run_wavec([
        OsStr::new("build"),
        float_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--abi=lp64d"),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        float_out.as_os_str(),
    ]);
    assert!(float_out.join("float_register.o").is_file());

    let jump_source = write_wave(
        &dir,
        "jalr.wave",
        r#"
fun jump(addr: u64) {
    asm {
        "jalr x0, 0(a0)"
        in("a0") addr
        clobber("stack")
    }
}

fun main() {}
"#,
    );
    let jump_out = dir.join("jalr-missing-noreturn");
    let error = run_wavec_expect_failure([
        OsStr::new("build"),
        jump_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-none-elf"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        jump_out.as_os_str(),
    ]);
    assert!(error.contains("clobber(\"noreturn\")"), "{}", error);

    let noreturn_source = write_wave(
        &dir,
        "jalr_noreturn.wave",
        r#"
fun jump(addr: u64) {
    asm {
        "jalr x0, 0(a0)"
        in("a0") addr
        clobber("stack")
        clobber("noreturn")
    }
}

fun main() {}
"#,
    );
    let noreturn_out = dir.join("jalr-noreturn");
    run_wavec([
        OsStr::new("build"),
        noreturn_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-none-elf"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        noreturn_out.as_os_str(),
    ]);
    let ir = fs::read_to_string(noreturn_out.join("jalr_noreturn.ll")).unwrap();
    assert!(ir.contains("unreachable"), "{}", ir);
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
fn riscv64_ir_and_bitcode_preserve_target_contract_when_recompiled() {
    let dir = temp_case_dir("riscv64-artifact-contract");
    let source = write_wave(&dir, "main.wave", "fun main() -> i32 { return 0; }\n");
    let original = dir.join("original");
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--abi=lp64f"),
        OsStr::new("--emit=ir,bc,obj"),
        OsStr::new("--out-dir"),
        original.as_os_str(),
    ]);

    let ir = fs::read_to_string(original.join("main.ll")).unwrap();
    assert!(ir.contains("!\"target-abi\", !\"lp64f\""), "{}", ir);
    assert!(ir.contains("!\"riscv-isa\""), "{}", ir);
    assert!(ir.contains("\"target-cpu\"=\"generic-rv64\""), "{}", ir);
    assert!(ir.contains("\"target-features\"=\"+m,+a,+f,-d,+c,+zicsr,+zifencei\""), "{}", ir);

    let from_ir = dir.join("from-ir");
    let from_bc = dir.join("from-bc");
    for (input, destination) in
        [(original.join("main.ll"), &from_ir), (original.join("main.bc"), &from_bc)]
    {
        let mut command = wavec_command();
        command.arg("build").arg(input).arg("--emit=obj").arg("--out-dir").arg(destination);
        if !native_target_enabled() {
            // The CLI validates its default host before loading module metadata.
            // Isolated cross-backend builds therefore need matching explicit
            // options; native-enabled builds retain automatic module inference.
            command.args([
                "--target=riscv64-unknown-linux-gnu",
                "--abi=lp64f",
                "--features=+m,+a,+f,-d,+c,+zicsr,+zifencei",
            ]);
        }
        let output = command.output().unwrap();
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    }

    for object in [original.join("main.o"), from_ir.join("main.o"), from_bc.join("main.o")] {
        assert_eq!(riscv64_elf_flags(&object) & 0x7, 0x3, "{}", object.display());
        let bytes = fs::read(&object).unwrap();
        assert!(bytes_contains(&bytes, b"zicsr"), "{}", object.display());
        assert!(bytes_contains(&bytes, b"zifencei"), "{}", object.display());
    }
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
fn riscv64_relocation_atomic_and_compressed_feature_contracts() {
    if std::env::var_os("WAVE_RUN_RISCV64_INTEROP_TESTS").is_none() {
        eprintln!("skipped: set WAVE_RUN_RISCV64_INTEROP_TESTS=1 to run toolchain test");
        return;
    }

    let dir = temp_case_dir("riscv64-object-contracts");
    let relocation_source = write_wave(
        &dir,
        "relocation.ll",
        r#"
@local_value = global i64 7, align 8
@external_value = external global i64

define ptr @local_address() {
entry:
  ret ptr @local_value
}

define ptr @external_address() {
entry:
  ret ptr @external_value
}
"#,
    );
    let pic_out = dir.join("pic");
    let static_out = dir.join("static");
    run_wavec([
        OsStr::new("build"),
        relocation_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("-Crelocation-model=pic"),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        pic_out.as_os_str(),
    ]);
    run_wavec([
        OsStr::new("build"),
        relocation_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("-Crelocation-model=static"),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        static_out.as_os_str(),
    ]);

    let read_relocations = |path: &Path| {
        let output = Command::new("llvm-readobj")
            .arg("--relocations")
            .arg(path)
            .output()
            .expect("failed to start llvm-readobj");
        assert!(
            output.status.success(),
            "llvm-readobj failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
        String::from_utf8_lossy(&output.stdout).into_owned()
    };
    let pic_relocations = read_relocations(&pic_out.join("relocation.o"));
    assert!(pic_relocations.contains("R_RISCV_GOT_HI20 local_value"));
    assert!(pic_relocations.contains("R_RISCV_GOT_HI20 external_value"));
    assert!(pic_relocations.contains("R_RISCV_PCREL_LO12_I"));
    assert!(!pic_relocations.contains("R_RISCV_HI20 local_value"));

    let static_relocations = read_relocations(&static_out.join("relocation.o"));
    assert!(static_relocations.contains("R_RISCV_HI20 local_value"));
    assert!(static_relocations.contains("R_RISCV_LO12_I local_value"));
    assert!(static_relocations.contains("R_RISCV_HI20 external_value"));
    assert!(!static_relocations.contains("R_RISCV_GOT_HI20"));

    let atomic_source = write_wave(
        &dir,
        "atomic.ll",
        r#"
define i64 @atomic_add(ptr %address) {
entry:
  %previous = atomicrmw add ptr %address, i64 1 seq_cst
  ret i64 %previous
}
"#,
    );
    let atomic_a_out = dir.join("atomic-a");
    let atomic_no_a_out = dir.join("atomic-no-a");
    let compressed_off_out = dir.join("compressed-off");
    for (features, out) in [
        ("+m,+a,-f,-d,+c", &atomic_a_out),
        ("+m,-a,-f,-d,+c", &atomic_no_a_out),
        ("+m,+a,-f,-d,-c", &compressed_off_out),
    ] {
        run_wavec([
            OsStr::new("build"),
            atomic_source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-linux-gnu"),
            OsStr::new("--features"),
            OsStr::new(features),
            OsStr::new("--abi=lp64"),
            OsStr::new("--emit=obj,asm"),
            OsStr::new("--out-dir"),
            out.as_os_str(),
        ]);
    }

    let atomic_a_assembly = fs::read_to_string(atomic_a_out.join("atomic.s")).unwrap();
    assert!(atomic_a_assembly.contains("amoadd.d.aqrl"));
    assert!(!atomic_a_assembly.contains("__atomic_fetch_add_8"));
    let atomic_no_a_assembly = fs::read_to_string(atomic_no_a_out.join("atomic.s")).unwrap();
    assert!(!atomic_no_a_assembly.contains("amoadd"));
    assert!(atomic_no_a_assembly.contains("__atomic_fetch_add_8"));

    let compressed_object = atomic_a_out.join("atomic.o");
    let uncompressed_object = compressed_off_out.join("atomic.o");
    assert_eq!(riscv64_elf_flags(&compressed_object) & 1, 1);
    assert_eq!(riscv64_elf_flags(&uncompressed_object) & 1, 0);
    let compressed_bytes = fs::read(&compressed_object).unwrap();
    let uncompressed_bytes = fs::read(&uncompressed_object).unwrap();
    assert!(bytes_contains(&compressed_bytes, b"c2p0"));
    assert!(!bytes_contains(&uncompressed_bytes, b"c2p0"));

    let disassemble = |path: &Path| {
        let output = Command::new("llvm-objdump")
            .arg("--disassemble")
            .arg(path)
            .output()
            .expect("failed to start llvm-objdump");
        assert!(
            output.status.success(),
            "llvm-objdump failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
        String::from_utf8_lossy(&output.stdout).into_owned()
    };
    let compressed_disassembly = disassemble(&compressed_object);
    let uncompressed_disassembly = disassemble(&uncompressed_object);
    assert!(compressed_disassembly.contains("8082"));
    assert!(uncompressed_disassembly.contains("00008067"));
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-x86"))]
fn waveos_boot_smoke_builds_windows_freestanding_coff_object() {
    let dir = temp_case_dir("waveos-boot-smoke-coff");
    let source =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/cases/windows/amd64/test1.wave");
    let object = dir.join("waveos_boot_smoke.obj");

    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-pc-windows-msvc"),
        OsStr::new("--freestanding"),
        OsStr::new("--emit=obj"),
        OsStr::new("-o"),
        object.as_os_str(),
    ]);

    let bytes = fs::read(&object).unwrap();
    assert!(
        bytes_contains(&bytes, &[0xB0, 0x45, 0xE6, 0xE9]),
        "COFF object must keep the embedded kernel byte array in .data"
    );
    assert!(
        bytes_contains(&bytes, b"embedded_kernel"),
        "COFF object must keep a relocatable embedded_kernel symbol"
    );
    assert!(
        bytes_contains(&bytes, &[0x41, 0xFF, 0xE3]),
        "jump_to_kernel must lower to an indirect jmp through r11"
    );
    assert!(
        !bytes_contains(&bytes, &[0x49, 0xC7, 0xC3, 0x00, 0x00, 0x20, 0x00]),
        "jump_to_kernel must not hard-code mov r11, 0x200000"
    );
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
#[test]
fn loongarch64_shared_language_workloads_run_under_qemu() {
    run_shared_language_workloads(
        "WAVE_RUN_LOONGARCH64_INTEROP_TESTS",
        "loongarch64-unknown-linux-gnu",
        "qemu-loongarch64",
    );
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-aarch64"))]
#[test]
fn aarch64_shared_language_workloads_run_under_qemu() {
    run_shared_language_workloads(
        "WAVE_RUN_AARCH64_INTEROP_TESTS",
        "aarch64-unknown-linux-gnu",
        "qemu-aarch64",
    );
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
#[test]
fn riscv64_shared_language_workloads_run_under_qemu() {
    run_shared_language_workloads(
        "WAVE_RUN_RISCV64_INTEROP_TESTS",
        "riscv64-unknown-linux-gnu",
        "qemu-riscv64",
    );
}

#[test]
fn msvc_link_companions_keep_final_names_and_survive_failed_replacement() {
    let dir = temp_case_dir("msvc-link-companions").join("한글 library directory");
    fs::create_dir_all(&dir).unwrap();
    let source = write_wave(
        &dir,
        "library.wave",
        "export(c) fun answer() -> i32 { return 42; }\nfun main() -> i32 { return 0; }\n",
    );
    for target in ["x86_64-pc-windows-msvc", "aarch64-pc-windows-msvc"] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        let output_dir = dir.join(target);
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--emit=obj"),
            OsStr::new("--out-dir"),
            output_dir.as_os_str(),
        ]);
        let object = output_dir.join("library.o");
        let dll = output_dir.join("answer.dll");
        let mut command = wavec_command();
        command.args([
            OsStr::new("build"),
            object.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--shared"),
            OsStr::new("-Cno-default-libs"),
            OsStr::new("-Clink-arg=/NOENTRY"),
            OsStr::new("-Clink-arg=/EXPORT:answer"),
            OsStr::new("-Clink-arg=/DEBUG"),
            OsStr::new("-o"),
            dll.as_os_str(),
        ]);
        // Force real response-file decoding by lld-link while preserving
        // executable/import-library/PDB publication and failure checks below.
        for index in 0..100 {
            command.arg(format!(
                "-Clink-arg=/LIBPATH:{} unused library path {index} {}",
                output_dir.display(),
                "search directory ".repeat(3)
            ));
        }
        let output = command.output().unwrap();
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        for extension in ["dll", "lib", "pdb"] {
            assert!(
                output_dir.join(format!("answer.{extension}")).is_file(),
                "missing final {extension} for {target}"
            );
        }
        assert_eq!(pe_machine(&dll), if target.starts_with("aarch64-") { 0xaa64 } else { 0x8664 });
        let library = fs::read(output_dir.join("answer.lib")).unwrap();
        assert!(library.windows(b"answer.dll".len()).any(|bytes| bytes == b"answer.dll"));
        assert!(!library.windows(b".wave-output-".len()).any(|bytes| bytes == b".wave-output-"));
        let custom_lib = output_dir.join("imports").join("public.lib");
        let custom_pdb = output_dir.join("symbols").join("private.pdb");
        command.arg(format!("-Clink-arg=/IMPLIB:{}", custom_lib.display()));
        command.arg(format!("-Clink-arg=/PDB:{}", custom_pdb.display()));
        let output = command.output().unwrap();
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        assert_eq!(fs::read(&custom_lib).unwrap(), library);
        assert!(custom_pdb.is_file());
        let consumer = write_wave(
            &dir,
            "consumer.wave",
            r#"
extern(c) fun answer() -> i32;
export(c) fun entry() -> i32 { return answer(); }
fun main() -> i32 { return 0; }
"#,
        );
        let executable = output_dir.join("consumer.exe");
        run_wavec([
            OsStr::new("--link"),
            custom_lib.as_os_str(),
            OsStr::new("build"),
            consumer.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--entry=entry"),
            OsStr::new("-Cno-default-libs"),
            OsStr::new("-o"),
            executable.as_os_str(),
        ]);
        let image = fs::read(&executable).unwrap();
        assert!(image.windows(b"answer.dll".len()).any(|bytes| bytes == b"answer.dll"));
        let original_dll = fs::read(&dll).unwrap();
        let original_pdb = fs::read(&custom_pdb).unwrap();
        command.arg("-Clink-arg=/EXPORT:missing_symbol");
        assert!(!command.output().unwrap().status.success());
        assert_eq!(fs::read(&dll).unwrap(), original_dll);
        assert_eq!(fs::read(output_dir.join("answer.lib")).unwrap(), library);
        assert_eq!(fs::read(&custom_lib).unwrap(), library);
        assert_eq!(fs::read(&custom_pdb).unwrap(), original_pdb);
        assert!(!fs::read_dir(&output_dir).unwrap().any(|entry| entry
            .unwrap()
            .file_name()
            .to_string_lossy()
            .starts_with(".wave-")));
    }
}

#[test]
fn explicit_entry_uses_the_selected_linker_dialect_once() {
    let dir = temp_case_dir("entry-linker-dialect");
    let source = write_wave(&dir, "entry.wave", "fun main() -> i32 { return 0; }");
    let cases = [
        ("x86_64-pc-windows-msvc", None, "\"/ENTRY:wave_start\""),
        ("aarch64-pc-windows-msvc", None, "\"/ENTRY:wave_start\""),
        ("x86_64-unknown-linux-gnu", None, "\"-e\",\"wave_start\""),
        ("x86_64-unknown-linux-gnu", Some("ld.lld"), "\"-e\",\"wave_start\""),
        ("x86_64-unknown-linux-gnu", Some("clang"), "\"-Wl,-e,wave_start\""),
        ("x86_64-pc-windows-msvc", Some("link.exe"), "\"/ENTRY:wave_start\""),
        ("aarch64-apple-darwin", None, "\"-e\",\"wave_start\""),
        ("wasm32-unknown-unknown", None, "\"--entry=wave_start\""),
    ];
    for (target, linker, expected) in cases {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        let mut command = wavec_command();
        command.args([
            "--error-format=json",
            "build",
            source.to_str().unwrap(),
            "--target",
            target,
            "--entry=wave_start",
            "-Cno-default-libs",
            "--dry-run",
        ]);
        if let Some(linker) = linker {
            command.arg(format!("-Clinker={linker}"));
        }
        let output = command.output().unwrap();
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        let plan = String::from_utf8(output.stdout).unwrap();
        let link_args =
            plan.split("\"args\":[").nth(1).unwrap().split("],\"command\":").next().unwrap();
        assert!(link_args.contains(expected), "{target}/{linker:?}: {plan}");
        assert_eq!(link_args.matches("wave_start").count(), 1, "{plan}");
    }
}

#[test]
fn msvc_native_fixtures_compile_and_arm64_hfas_match_clang() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/msvc_native");
    let dir = temp_case_dir("msvc-native-fixtures");
    let home = dir.join("home");
    copy_tree(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("std"),
        &home.join(".wave/lib/wave/std"),
    );
    let clang = clang_for_contract_tests().expect("Clang is required for MSVC ABI contracts");
    for target in ["x86_64-pc-windows-msvc", "aarch64-pc-windows-msvc"] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        for opt in ["-O0", "-O2"] {
            let output = dir.join(target).join(opt);
            fs::create_dir_all(&output).unwrap();
            for fixture in [
                "implicit",
                "exit",
                "helpers",
                "wide_numeric",
                "abi",
                "custom_entry",
                "missing_helper",
                "stack",
                "dll",
                "dll_consumer",
                "package_std",
            ] {
                let result = wavec_command()
                    .env("HOME", &home)
                    .env("USERPROFILE", &home)
                    .args([
                        OsStr::new("build"),
                        root.join(format!("{fixture}.wave")).as_os_str(),
                        OsStr::new("--target"),
                        OsStr::new(target),
                        OsStr::new(opt),
                        OsStr::new("--emit=ir,obj"),
                        OsStr::new("--out-dir"),
                        output.as_os_str(),
                    ])
                    .output()
                    .unwrap();
                assert!(
                    result.status.success(),
                    "{target}/{opt}/{fixture}:\n{}\n{}",
                    String::from_utf8_lossy(&result.stdout),
                    String::from_utf8_lossy(&result.stderr)
                );
                llvm::msvc::coff::validate_file(&output.join(format!("{fixture}.o")), target)
                    .unwrap();
            }
            let unwind = Command::new("llvm-readobj")
                .args(["--unwind", "--symbols"])
                .arg(output.join("stack.o"))
                .output()
                .expect("llvm-readobj is required for stack contracts");
            assert!(unwind.status.success(), "{}", String::from_utf8_lossy(&unwind.stderr));
            let unwind = String::from_utf8_lossy(&unwind.stdout);
            assert!(
                unwind.contains("RuntimeFunction") && unwind.contains("__chkstk"),
                "{target}/{opt}: {unwind}"
            );
            if target.starts_with("aarch64") {
                let ir = fs::read_to_string(output.join("stack.ll")).unwrap();
                for name in ["stack_inner", "stack_middle", "stack_outer", "main"] {
                    let definition = ir
                        .lines()
                        .find(|line| {
                            line.starts_with("define ") && line.contains(&format!("@{name}("))
                        })
                        .unwrap_or_else(|| panic!("missing {name}: {ir}"));
                    let group =
                        definition.split('#').nth(1).unwrap().split_whitespace().next().unwrap();
                    let attributes = ir
                        .lines()
                        .find(|line| line.starts_with(&format!("attributes #{group} =")))
                        .unwrap();
                    assert!(
                        attributes.contains("\"frame-pointer\"=\"non-leaf\""),
                        "{target}/{opt}/{name}: {attributes}"
                    );
                }
                assert!(
                    unwind.contains("add fp, sp") || unwind.contains("mov fp, sp"),
                    "ARM64 unwind records must describe the frame chain: {unwind}"
                );
            }
            let c_ir = output.join("abi-c.ll");
            let result = Command::new(&clang)
                .args(["-target", target, "-S", "-emit-llvm", opt])
                .arg(root.join("abi.c"))
                .arg("-o")
                .arg(&c_ir)
                .output()
                .unwrap();
            assert!(result.status.success(), "{}", String::from_utf8_lossy(&result.stderr));
            if target.starts_with("aarch64") {
                let c = fs::read_to_string(c_ir).unwrap();
                let wave = fs::read_to_string(output.join("abi.ll")).unwrap();
                assert!(c.contains("@c_hda([4 x double]"), "{c}");
                assert!(c.contains("@c_aggregate_variadic([2 x i64]"), "{c}");
                assert!(
                    wave.contains("declare i32 @c_aggregate_variadic([2 x i64], ptr, i32, ...)"),
                    "{wave}"
                );
                assert!(wave.contains("declare %Hda @c_hda([4 x double])"), "{wave}");
                assert!(wave.contains("define %Hda @wave_hda([4 x double]"), "{wave}");
                // HFA exhaustion must move the aggregate together onto the stack.
                assert!(wave.contains("float, [4 x float], float)"), "{wave}");
            }
        }
    }
    // This is compile/contract evidence. the native MSVC acceptance tool separately requires
    // actual Windows execution; cross-compilation cannot satisfy that gate.
}

#[test]
fn explicit_msvc_targets_emit_coff_and_link_without_mingw() {
    let dir = temp_case_dir("msvc-foundation");
    let source = write_wave(
        &dir,
        "entry.wave",
        r#"
export(c) fun entry() -> i32 { return 7; }
extern(system) fun native_api(value: i32) -> i32;
fun main() -> i32 { return 0; }
"#,
    );
    for (target, machine) in
        [("x86_64-pc-windows-msvc", 0x8664u16), ("aarch64-pc-windows-msvc", 0xaa64u16)]
    {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        let output_dir = dir.join(target);
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--emit=obj"),
            OsStr::new("--out-dir"),
            output_dir.as_os_str(),
        ]);
        let object = output_dir.join("entry.o");
        let data = fs::read(&object).unwrap();
        assert_eq!(u16::from_le_bytes([data[0], data[1]]), machine);
        let executable = output_dir.join("entry.exe");
        let (plan, _) = run_wavec_capture([
            OsStr::new("build"),
            object.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--dry-run"),
            OsStr::new("--entry=entry"),
            OsStr::new("-Cno-default-libs"),
            OsStr::new("-o"),
            executable.as_os_str(),
        ]);
        assert!(
            plan.contains("lld-link")
                && plan.contains("/NODEFAULTLIB")
                && plan.contains("/ENTRY:entry"),
            "{plan}"
        );
        assert!(
            !plan.contains("-Wl,")
                && !plan.contains("-lmingw")
                && !plan.contains("-lgcc")
                && !plan.contains("-lmsvcrt"),
            "{plan}"
        );
        // This verifies a PE image, not execution on a Windows host.
        if Command::new("lld-link").arg("--version").output().is_ok() {
            run_wavec([
                OsStr::new("build"),
                object.as_os_str(),
                OsStr::new("--target"),
                OsStr::new(target),
                OsStr::new("--entry=entry"),
                OsStr::new("-Cno-default-libs"),
                OsStr::new("-o"),
                executable.as_os_str(),
            ]);
            let image = fs::read(&executable).unwrap();
            assert_eq!(&image[..2], b"MZ");
            let pe = u32::from_le_bytes(image[0x3c..0x40].try_into().unwrap()) as usize;
            assert_eq!(&image[pe..pe + 4], b"PE\0\0");
            assert_eq!(u16::from_le_bytes(image[pe + 4..pe + 6].try_into().unwrap()), machine);
        }
    }
}

#[cfg(target_os = "linux")]
#[test]
#[cfg(any(
    feature = "llvm-target-all",
    all(target_arch = "x86_64", feature = "llvm-target-x86"),
    all(target_arch = "aarch64", feature = "llvm-target-aarch64"),
    all(target_arch = "riscv64", feature = "llvm-target-riscv"),
    all(target_arch = "loongarch64", feature = "llvm-target-loongarch")
))]
fn async_tasks_suspend_resume_cancel_and_exchange_tcp_data() {
    let available = run_wavec_capture(["print", "target-list"]).0;
    let arch = std::env::consts::ARCH;
    if !available.lines().any(|t| t.starts_with(arch)) {
        return;
    }
    let dir = temp_case_dir("async-runtime");
    let home = dir.join("home");
    copy_tree(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("std"),
        &home.join(".wave/lib/wave/std"),
    );
    for name in ["lazy_nested", "control_flow", "timer_fairness", "tcp_roundtrip"] {
        let source = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join(format!("tests/fixtures/async/{name}.wave"));
        let output = dir.join(name);
        let compiled = wavec_command()
            .env("HOME", &home)
            .arg("build")
            .arg(source)
            .arg("-o")
            .arg(&output)
            .output()
            .unwrap();
        assert!(compiled.status.success(), "{name}: {}", String::from_utf8_lossy(&compiled.stderr));
        let mut child = Command::new(&output).spawn().unwrap();
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(15);
        loop {
            if let Some(status) = child.try_wait().unwrap() {
                assert!(status.success(), "{name}: {status}");
                break;
            }
            if std::time::Instant::now() >= deadline {
                child.kill().unwrap();
                child.wait().unwrap();
                panic!("{name} did not complete");
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
    }
    fs::remove_dir_all(dir).unwrap();
}

#[test]
fn async_frames_emit_for_enabled_native_targets() {
    let dir = temp_case_dir("async-targets");
    let home = dir.join("home");
    copy_tree(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("std"),
        &home.join(".wave/lib/wave/std"),
    );
    let source =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/async/control_flow.wave");
    let available = run_wavec_capture(["print", "target-list"]).0;
    for target in [
        "x86_64-unknown-linux-gnu",
        "aarch64-unknown-linux-gnu",
        "riscv64-unknown-linux-gnu",
        "loongarch64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-pc-windows-msvc",
        "x86_64-apple-darwin",
        "aarch64-apple-darwin",
        "x86_64-unknown-freebsd",
    ] {
        if !available.lines().any(|t| t == target) {
            continue;
        }
        let output = dir.join(format!("{target}.o"));
        let compiled = wavec_command()
            .env("HOME", &home)
            .arg("build")
            .arg(&source)
            .args(["--target", target, "--emit=obj", "-o"])
            .arg(&output)
            .output()
            .unwrap();
        assert!(
            compiled.status.success(),
            "{target}: {}",
            String::from_utf8_lossy(&compiled.stderr)
        );
        assert!(output.metadata().unwrap().len() > 0);
    }
    fs::remove_dir_all(dir).unwrap();
}

#[test]
fn darwin_pipe_captures_both_kernel_return_registers() {
    let dir = temp_case_dir("darwin-pipe");
    let home = dir.join("home");
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    copy_tree(&root.join("std"), &home.join(".wave/lib/wave/std"));
    let source = root.join("tests/cases/macos/arm64/test6.wave");
    let available = run_wavec_capture(["print", "target-list"]).0;
    for (target, first, second, instruction) in [
        ("aarch64-apple-darwin", "{x0}", "{x1}", "svc"),
        ("x86_64-apple-darwin", "{rax}", "{rdx}", "syscall"),
    ] {
        if !available.lines().any(|t| t == target) {
            continue;
        }
        for (emit, extension) in [("ir", "ll"), ("obj", "o")] {
            let output_dir = dir.join(format!("{target}-{emit}"));
            let output = output_dir.join(format!("test6.{extension}"));
            let compiled = wavec_command()
                .env("HOME", &home)
                .env("USERPROFILE", &home)
                .arg("build")
                .arg(&source)
                .args(["--target", target, "--emit", emit, "--out-dir"])
                .arg(&output_dir)
                .output()
                .unwrap();
            assert!(
                compiled.status.success(),
                "{target}: {}",
                String::from_utf8_lossy(&compiled.stderr)
            );
            if emit == "ir" {
                let ir = fs::read_to_string(output).unwrap();
                assert!(
                    ir.lines().any(|line| line.contains("call { i64, i64 }")
                        && line.contains("asm sideeffect")
                        && line.contains(instruction)
                        && line.contains(first)
                        && line.contains(second)),
                    "{target}: pipe must capture both returned descriptors"
                );
            }
        }
        if cfg!(target_os = "macos") && target.starts_with(std::env::consts::ARCH) {
            let result = wavec_command()
                .env("HOME", &home)
                .env("USERPROFILE", &home)
                .arg("run")
                .arg(&source)
                .output()
                .unwrap();
            assert!(
                result.status.success(),
                "pipe/kqueue streaming test: {}\n{}\n{}",
                result.status,
                String::from_utf8_lossy(&result.stdout),
                String::from_utf8_lossy(&result.stderr)
            );
        }
    }
    fs::remove_dir_all(dir).unwrap();
}

#[test]
fn executor_restart_fixture_emits_for_windows_msvc() {
    let dir = temp_case_dir("executor-restart-targets");
    let home = dir.join("home");
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    copy_tree(&root.join("std"), &home.join(".wave/lib/wave/std"));
    for target in ["x86_64-pc-windows-msvc", "aarch64-pc-windows-msvc"] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        let output = dir.join(format!("{target}.o"));
        let compiled = wavec_command()
            .env("HOME", &home)
            .env("USERPROFILE", &home)
            .arg("build")
            .arg(root.join("tests/fixtures/async/executor_restart.wave"))
            .args(["--target", target, "--emit=obj", "-o"])
            .arg(&output)
            .output()
            .unwrap();
        assert!(
            compiled.status.success(),
            "{target}: {}",
            String::from_utf8_lossy(&compiled.stderr)
        );
        let object = fs::read(output).unwrap();
        let expected = if target.starts_with("aarch64") { 0xaa64 } else { 0x8664 };
        assert_eq!(u16::from_le_bytes([object[0], object[1]]), expected);
    }
    fs::remove_dir_all(dir).unwrap();
}

#[test]
fn timeout_modules_respect_enabled_windows_and_posix_targets() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let dir = temp_case_dir("timeout-module-targets");
    let home = dir.join("home");
    copy_tree(&root.join("std"), &home.join(".wave/lib/wave/std"));
    for target in [
        "x86_64-pc-windows-msvc",
        "aarch64-pc-windows-msvc",
        "x86_64-unknown-linux-gnu",
        "aarch64-unknown-linux-gnu",
        "x86_64-apple-darwin",
        "aarch64-apple-darwin",
        "x86_64-unknown-freebsd",
    ] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        let output = wavec_command()
            .env("HOME", &home)
            .env("USERPROFILE", &home)
            .arg("check")
            .arg(root.join("std/net/posix_timeout.wave"))
            .args(["--target", target])
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "standalone timeout module for {target}: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
    fs::remove_dir_all(dir).unwrap();
}

#[test]
fn darwin_process_and_timed_socket_fixtures_preserve_raw_syscall_contracts() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let dir = temp_case_dir("darwin-process-socket");
    let home = dir.join("home");
    copy_tree(&root.join("std"), &home.join(".wave/lib/wave/std"));
    for (target, result_registers) in
        [("x86_64-apple-darwin", "={rax},={rdx}"), ("aarch64-apple-darwin", "={x0},={x1}")]
    {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        for source in ["fork_status", "tcp_timeout"] {
            for opt in ["-O0", "-O2"] {
                let out = dir.join(target).join(source).join(opt);
                let output = wavec_command()
                    .env("HOME", &home)
                    .env("USERPROFILE", &home)
                    .arg("build")
                    .arg(root.join(format!("tests/fixtures/io/{source}.wave")))
                    .args(["--target", target, "--emit=ir,obj", opt])
                    .arg("--out-dir")
                    .arg(&out)
                    .output()
                    .unwrap();
                assert!(
                    output.status.success(),
                    "{target} {source} {opt}: {}",
                    String::from_utf8_lossy(&output.stderr)
                );
                assert!(out.join(format!("{source}.o")).is_file());
                if opt == "-O0" {
                    let ir = fs::read_to_string(out.join(format!("{source}.ll"))).unwrap();
                    let suffix = if source == "fork_status" { "_fork(" } else { "_sendto(" };
                    let function = ir
                        .split("\ndefine ")
                        .find(|body| body.lines().next().unwrap_or("").contains(suffix))
                        .unwrap()
                        .split("\n}")
                        .next()
                        .unwrap();
                    if source == "fork_status" {
                        assert!(function.contains(result_registers), "{function}");
                        assert!(function.contains("icmp slt i64"), "{function}");
                        assert!(function.contains("icmp ne i64"), "{function}");
                        assert!(function.contains("ret i64 0"), "{function}");
                    } else {
                        assert!(
                            function.contains("and i32") && function.contains(", 128"),
                            "{function}"
                        );
                        assert!(
                            function.contains("or i32") && function.contains(", 131072"),
                            "{function}"
                        );
                    }
                }
            }
        }
    }
    fs::remove_dir_all(dir).unwrap();
}

#[test]
fn main_parameter_contract_is_shared_by_hosted_freestanding_and_wasi_targets() {
    let dir = temp_case_dir("main-parameter-contract");
    let mut targets = vec![frontend_target()];
    if llvm::codegen::target::target_spec_for_triple("x86_64-unknown-none-elf").is_some() {
        targets.push("x86_64-unknown-none-elf");
    }
    if cfg!(any(feature = "llvm-target-wasm", feature = "llvm-target-all")) {
        targets.push("wasm32-wasip1");
    }
    for target in targets {
        for (name, body, valid) in [
            ("void_entry", "fun main() {}", true),
            ("int_entry", "fun main() -> i32 { return 0; }", true),
            ("parameter_entry", "fun main(x: i32) -> i32 { return x; }", false),
            ("default_entry", "fun main(x: i32 = 42) -> i32 { return x; }", false),
        ] {
            let source = write_wave(&dir, &format!("{name}.wave"), body);
            let out = dir.join(target).join(name);
            let mut command = wavec_command();
            command
                .arg("build")
                .arg(source)
                .args(["--target", target, "--emit=ir"])
                .arg("--out-dir")
                .arg(&out);
            if target.contains("none") {
                command.arg("--freestanding");
            }
            let output = command.output().unwrap();
            let error = String::from_utf8_lossy(&output.stderr);
            assert_eq!(output.status.success(), valid, "{target}: {body}: {error}");
            assert_eq!(out.join(format!("{name}.ll")).is_file(), valid);
            if !valid {
                assert!(output.stdout.is_empty());
                assert!(
                    error.contains("E3001") && error.contains("must have zero parameters"),
                    "{error}"
                );
            }
        }
    }
}

#[test]
#[cfg(feature = "llvm-target-wasm")]
fn wasi_access_checks_minimal_rights_and_preserves_host_errors() {
    for tool in ["node", "wasm-ld"] {
        if !Command::new(tool).arg("--version").output().is_ok_and(|out| out.status.success()) {
            eprintln!("skipping WASI access execution: {tool} unavailable");
            return;
        }
    }
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let dir = temp_case_dir("wasi-access");
    for optimization in ["-O0", "-O2"] {
        let output = wavec_command()
            .arg("build")
            .arg(root.join("tests/fixtures/wasi_access/access.wave"))
            .args([
                "--target",
                "wasm32-wasip1",
                "--emit=obj",
                "--freestanding",
                optimization,
                "--std-root",
            ])
            .arg(root.join("std"))
            .arg("--out-dir")
            .arg(&dir)
            .output()
            .unwrap();
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        let module = dir.join("access.wasm");
        let link = Command::new("wasm-ld")
            .args(["--no-entry", "--export=check_access", "--export-memory"])
            .arg(dir.join("access.o"))
            .arg("-o")
            .arg(&module)
            .output()
            .unwrap();
        assert!(link.status.success(), "{}", String::from_utf8_lossy(&link.stderr));
        let result = Command::new("node")
            .arg(root.join("tests/fixtures/wasi_access/host.cjs"))
            .arg(&module)
            .current_dir(&dir)
            .output()
            .unwrap();
        assert!(
            result.status.success(),
            "{optimization}: {}\n{}",
            String::from_utf8_lossy(&result.stdout),
            String::from_utf8_lossy(&result.stderr)
        );
    }
    fs::remove_dir_all(dir).unwrap();
}
