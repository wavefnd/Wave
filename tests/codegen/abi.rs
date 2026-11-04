// SPDX-License-Identifier: MPL-2.0
//! Calling conventions and ABI regression cases.
//! These tests share the parent integration suite's fixtures and target gates.
use super::*;

#[test]
fn type_layout_queries_follow_the_target_and_generic_storage() {
    let dir = temp_case_dir("type-layout-queries");
    let home = dir.join("home");
    copy_tree(&Path::new(env!("CARGO_MANIFEST_DIR")).join("std"), &home.join(".wave/lib/wave/std"));
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/cases/shared/test124.wave");
    if native_target_enabled() {
        for optimization in ["-O0", "-O2"] {
            let output = wavec_command()
                .env("HOME", &home)
                .arg("build")
                .arg(&source)
                .arg(optimization)
                .arg("--run")
                .arg("--out-dir")
                .arg(dir.join(optimization))
                .output()
                .unwrap();
            assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        }
    }
    let source = write_wave(
        &dir,
        "layout.wave",
        r#"
struct Record { tag: u8; value: u64; }
fun pointer_size() -> u64 { return __wave_size_of<ptr<u8>>(); }
fun pointer_alignment() -> u64 { return __wave_align_of<ptr<u8>>(); }
fun record_size() -> u64 { return __wave_size_of<Record>(); }
fun array_size() -> u64 { return __wave_size_of<array<Record, 3>>(); }
fun main() -> i32 { return 0; }
"#,
    );
    for (target, pointer_bytes) in [
        ("x86_64-unknown-linux-gnu", 8),
        ("aarch64-unknown-linux-gnu", 8),
        ("riscv64-unknown-linux-gnu", 8),
        ("wasm32-unknown-unknown", 4),
        ("wasm64-unknown-unknown", 8),
    ] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        let output = dir.join(target);
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--emit=ir"),
            OsStr::new("--out-dir"),
            output.as_os_str(),
        ]);
        let ir = fs::read_to_string(output.join("layout.ll")).unwrap();
        for (function, value) in [
            ("pointer_size", pointer_bytes),
            ("pointer_alignment", pointer_bytes),
            ("record_size", 16),
            ("array_size", 48),
        ] {
            let body =
                ir.split(&format!("@{function}(")).nth(1).unwrap().split('}').next().unwrap();
            assert!(body.contains(&format!("ret i64 {value}")), "{target}: {function}: {body}");
        }
    }
    for query in [
        "__wave_size_of<void>()",
        "__wave_align_of<Missing>()",
        "__wave_size_of<i32>(1)",
        "__wave_align_of()",
    ] {
        let source = write_wave(&dir, "invalid.wave", &format!("fun main() {{ {query}; }}"));
        let output = wavec_command().arg("check").arg(source).output().unwrap();
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success() && !stderr.contains("panicked"), "{query}: {stderr}");
    }
}

#[test]
fn windows_system_abi_is_target_scoped() {
    let dir = temp_case_dir("windows-system-abi");
    let source = write_wave(
        &dir,
        "system.wave",
        r#"
extern(system, "GetCurrentProcessId") fun get_current_process_id() -> u32;
fun main() -> i32 { return get_current_process_id() as i32; }
"#,
    );
    let available = run_wavec_capture(["print", "target-list"]).0;
    for target in ["x86_64-pc-windows-msvc", "aarch64-pc-windows-msvc"] {
        if !available
            .lines()
            .any(|t| t == target || (target.contains("w64") && t == target.replace("w64", "pc")))
        {
            continue;
        }
        eprintln!("system ABI target: {target}");
        let windows_out = dir.join(target);
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--emit=ir"),
            OsStr::new("--out-dir"),
            windows_out.as_os_str(),
        ]);
        let ir = fs::read_to_string(windows_out.join("system.ll")).unwrap();
        assert!(ir.contains("@GetCurrentProcessId"), "{target}: {ir}");
    }

    if cfg!(any(feature = "llvm-target-all", feature = "llvm-target-aarch64")) {
        let arm64_bin = dir.join("system-arm64.exe");
        let (link_plan, stderr) = run_wavec_capture([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target=aarch64-pc-windows-msvc"),
            OsStr::new("--emit=bin"),
            OsStr::new("--dry-run"),
            OsStr::new("-o"),
            arm64_bin.as_os_str(),
        ]);
        assert!(stderr.trim().is_empty(), "{stderr}");
        assert!(link_plan.contains("lld-link"), "{link_plan}");
    }
    if !cfg!(any(feature = "llvm-target-all", feature = "llvm-target-x86")) {
        return;
    }
    let linux_out = dir.join("linux");
    let error = run_wavec_expect_failure([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target=x86_64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        linux_out.as_os_str(),
    ]);
    assert!(error.contains("Windows 'system'"), "{error}");
}

#[test]
fn std_net_compiles_for_every_supported_socket_abi() {
    let dir = temp_case_dir("std-net-target-matrix");
    let home = dir.join("home");
    let std_destination = home.join(".wave/lib/wave/std");
    copy_tree(&PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("std"), &std_destination);

    let sources = [
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/std/net_tcp.wave"),
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/std/net_udp.wave"),
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/std/net_ipv6.wave"),
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/std/net_dns.wave"),
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/std/net_unix.wave"),
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/std/net_interfaces.wave"),
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/std/net_vectored.wave"),
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/std/net_event.wave"),
    ];
    let targets = [
        "x86_64-unknown-linux-gnu",
        "aarch64-unknown-linux-gnu",
        "riscv64-unknown-linux-gnu",
        #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
        "loongarch64-unknown-linux-gnu",
        "x86_64-apple-darwin",
        "aarch64-apple-darwin",
        "x86_64-pc-windows-msvc",
        "x86_64-pc-windows-msvc",
        "aarch64-pc-windows-msvc",
        "aarch64-pc-windows-msvc",
        "x86_64-unknown-freebsd",
        "aarch64-unknown-freebsd",
        "riscv64-unknown-freebsd",
    ];

    let available = run_wavec_capture(["print", "target-list"]).0;
    for target in targets {
        if !available
            .lines()
            .any(|t| t == target || (target.contains("w64") && t == target.replace("w64", "pc")))
        {
            continue;
        }
        eprintln!("std net ABI target: {target}");
        for source in &sources {
            let output_dir = dir.join(target).join(source.file_stem().unwrap());
            let output = wavec_command()
                .env("HOME", &home)
                .arg("build")
                .arg(source)
                .arg("--target")
                .arg(target)
                .arg("--emit=ir")
                .arg("--out-dir")
                .arg(&output_dir)
                .output()
                .unwrap();
            assert!(
                output.status.success(),
                "{target} {} failed\nstdout:\n{}\nstderr:\n{}",
                source.display(),
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            );

            if source.file_stem().unwrap() == "net_dns" {
                let ir_path = output_dir.join("net_dns.ll");
                let ir = fs::read_to_string(&ir_path).unwrap();
                let bsd_sockaddr = target.contains("apple") || target.contains("freebsd");
                if bsd_sockaddr {
                    assert!(
                        ir.contains("NativeSockAddrV4 = type { i8, i8, i16, i32, [8 x i8] }"),
                        "{target} must use the BSD sockaddr_in field order"
                    );
                    assert!(
                        ir.contains("NativeSockAddrV6 = type { i8, i8, i16, i32, [16 x i8], i32 }"),
                        "{target} must use the BSD sockaddr_in6 field order"
                    );
                } else {
                    assert!(
                        ir.contains("NativeSockAddrV4 = type { i16, i16, i32, [8 x i8] }"),
                        "{target} must use the Linux/Windows sockaddr_in field order"
                    );
                    assert!(
                        ir.contains("NativeSockAddrV6 = type { i16, i16, i32, [16 x i8], i32 }"),
                        "{target} must use the Linux/Windows sockaddr_in6 field order"
                    );
                }

                if target.contains("linux") {
                    assert!(!ir.contains("@getaddrinfo("));
                    assert!(!ir.contains("@freeaddrinfo("));
                    continue;
                }
                let addrinfo_length = if target.contains("windows") {
                    "NativeAddrInfo = type { i32, i32, i32, i32, i64, ptr, ptr, ptr }"
                } else {
                    "NativeAddrInfo = type { i32, i32, i32, i32, i32, ptr, ptr, ptr }"
                };
                assert!(
                    ir.contains(addrinfo_length),
                    "{target} must use the target addrinfo address-length width"
                );
            }

            if source.file_stem().unwrap() == "net_event" {
                let ir = fs::read_to_string(output_dir.join("net_event.ll")).unwrap();
                let backend_symbol = if target.contains("linux") {
                    "_event_create("
                } else if target.contains("freebsd") {
                    "_native_kevent("
                } else if target.contains("apple") {
                    "@kqueue"
                } else {
                    "@WSAPoll"
                };
                assert!(
                    ir.contains(backend_symbol),
                    "{target} must select readiness backend {backend_symbol}"
                );

                if target.contains("linux") {
                    for obsolete in ["@epoll_create1(", "@epoll_ctl(", "@epoll_pwait("] {
                        assert!(
                            !ir.contains(obsolete),
                            "{target} must use kernel syscalls, not {obsolete}"
                        );
                    }
                    let create_number = if target.starts_with("x86_64-") { 291 } else { 20 };
                    assert!(
                        ir.contains(&format!("_syscall1(i64 {create_number}, i64 0)")),
                        "{target} must call epoll_create1 through its native syscall number"
                    );
                    let (stride, data_offset) =
                        if target.starts_with("x86_64-") { (12, 4) } else { (16, 8) };
                    assert!(
                        ir.contains(&format!(
                            "__epoll_event_stride() #0 {{\nentry:\n  ret i32 {stride}"
                        )),
                        "{target} must use the Linux epoll_event stride {stride}"
                    );
                    assert!(
                        ir.contains(&format!(
                            "__epoll_data_offset() #0 {{\nentry:\n  ret i32 {data_offset}"
                        )),
                        "{target} must use the Linux epoll data offset {data_offset}"
                    );
                }
            }
        }
    }
}

#[test]
#[cfg(feature = "llvm-target-wasm")]
fn webassembly_c_abi_and_wasi_import_contracts_are_explicit() {
    let dir = temp_case_dir("wasm32-abi-contracts");
    let host_source = write_wave(
        &dir,
        "host.wave",
        r#"
struct Pair {
    x: i32;
    y: i32;
}

extern(c, "host_transform") fun transform(value: Pair) -> Pair;

export(c, "wave_transform") fun apply(value: Pair) -> Pair {
    return transform(value);
}

fun main() -> i32 { return 0; }
"#,
    );
    let host_out = dir.join("host-out");
    run_wavec([
        OsStr::new("build"),
        host_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm32-unknown-unknown"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        host_out.as_os_str(),
    ]);
    let host_ir = fs::read_to_string(host_out.join("host.ll")).unwrap();
    assert!(
        host_ir.contains("ptr sret(%Pair) align 4") && host_ir.contains("ptr byval(%Pair) align 4"),
        "{host_ir}"
    );
    assert!(
        host_ir.contains("\"wasm-import-module\"=\"env\"")
            && host_ir.contains("\"wasm-import-name\"=\"host_transform\""),
        "{host_ir}"
    );
    assert!(host_ir.contains("\"wasm-export-name\"=\"wave_transform\""), "{host_ir}");

    let host64_out = dir.join("host64-out");
    run_wavec([
        OsStr::new("build"),
        host_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm64-unknown-unknown"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        host64_out.as_os_str(),
    ]);
    let host64_ir = fs::read_to_string(host64_out.join("host.ll")).unwrap();
    assert!(
        host64_ir.contains("target triple = \"wasm64-unknown-unknown\"")
            && host64_ir.contains("p:64:64")
            && host64_ir.contains("ptr sret(%Pair) align 4")
            && host64_ir.contains("ptr byval(%Pair) align 4")
            && host64_ir.contains("\"wasm-import-module\"=\"env\""),
        "{host64_ir}"
    );

    let wasi_source = write_wave(
        &dir,
        "wasi.wave",
        r#"
extern(c, "fd_write") fun fd_write(fd: u32, vectors: ptr<u8>, count: u32, written: ptr<u32>) -> u16;
fun main() -> i32 { return 0; }
"#,
    );
    let wasi_out = dir.join("wasi-out");
    run_wavec([
        OsStr::new("build"),
        wasi_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm32-wasip1"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        wasi_out.as_os_str(),
    ]);
    let wasi_ir = fs::read_to_string(wasi_out.join("wasi.ll")).unwrap();
    assert!(wasi_ir.contains("target triple = \"wasm32-wasip1\""), "{wasi_ir}");
    assert!(
        wasi_ir.contains("\"wasm-import-module\"=\"wasi_snapshot_preview1\"")
            && wasi_ir.contains("\"wasm-import-name\"=\"fd_write\""),
        "{wasi_ir}"
    );
    assert!(wasi_ir.contains("define void @_start()"), "{wasi_ir}");

    let (spec, stderr) = run_wavec_capture([
        OsStr::new("print"),
        OsStr::new("target-spec"),
        OsStr::new("--target"),
        OsStr::new("wasm32-wasip1"),
        OsStr::new("--format=json"),
    ]);
    assert!(stderr.trim().is_empty(), "{stderr}");
    assert!(spec.contains("\"os\":\"wasi\""), "{spec}");
    assert!(spec.contains("\"env\":\"p1\""), "{spec}");
    assert!(spec.contains("\"hosted\":true"), "{spec}");

    let (run_plan, stderr) = run_wavec_capture([
        OsStr::new("build"),
        wasi_source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("wasm32-wasip1"),
        OsStr::new("--run"),
        OsStr::new("--dry-run"),
        OsStr::new("--error-format=json"),
    ]);
    assert!(stderr.trim().is_empty(), "{stderr}");
    assert!(run_plan.contains("\"program\":\"node\""), "{run_plan}");
    assert!(run_plan.contains("node:wasi"), "{run_plan}");
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
fn riscv64_export_c_uses_indirect_aggregate_parameters_and_sret() {
    let dir = temp_case_dir("riscv64-export-c-aggregate");
    let source = write_wave(
        &dir,
        "aggregate.wave",
        r#"
struct Triple {
    a: u64;
    b: u64;
    c: u64;
}

export(c) fun wave_take(value: Triple) -> u64 {
    return value.c;
}

export(c) fun wave_make(a: u64, b: u64, c: u64) -> Triple {
    return Triple { a: a, b: b, c: c };
}

fun main() -> i32 {
    var value: Triple = wave_make(1, 2, 3);
    if (wave_take(value) != 3) {
        return 1;
    }
    return 0;
}
"#,
    );
    let out = dir.join("out");
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        out.as_os_str(),
    ]);
    let ir = fs::read_to_string(out.join("aggregate.ll")).unwrap();
    assert!(
        ir.lines()
            .any(|line| line.starts_with("define i64 @wave_take(ptr ") && !line.contains("byval")),
        "{}",
        ir
    );
    assert!(ir.contains("define void @wave_make(ptr sret(%Triple) align 8"), "{}", ir);
    assert!(ir.contains("@__wave_export_impl_wave_take"), "{}", ir);
    assert!(ir.contains("@__wave_export_impl_wave_make"), "{}", ir);
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
fn riscv64_c_abi_marks_required_integer_extensions() {
    let dir = temp_case_dir("riscv64-c-abi-integer-extensions");
    let source = write_wave(
        &dir,
        "integer_extensions.wave",
        r#"
extern(c) fun c_i8(value: i8) -> i8;
extern(c) fun c_u8(value: u8) -> u8;
extern(c) fun c_u32(value: u32) -> u32;
extern(c) fun c_variadic(count: i32, ...) -> i64;

export(c) fun wave_i8(value: i8) -> i8 { return value; }
export(c) fun wave_u8(value: u8) -> u8 { return value; }
export(c) fun wave_u32(value: u32) -> u32 { return value; }

fun main() -> i32 {
    if (c_i8(-1) != -1) { return 1; }
    if (c_u8(255) != 255) { return 2; }
    if (c_u32(4294967295) != 4294967295) { return 3; }
    var narrow: i8 = -1;
    var single: f32 = 1.5;
    if (c_variadic(2, narrow, single) != 0) { return 4; }
    return 0;
}
"#,
    );
    let out = dir.join("out");
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        out.as_os_str(),
    ]);
    let ir = fs::read_to_string(out.join("integer_extensions.ll")).unwrap();

    for expected in [
        "define signext i8 @wave_i8(i8 signext",
        "define zeroext i8 @wave_u8(i8 zeroext",
        "define signext i32 @wave_u32(i32 signext",
        "call signext i8 @c_i8(i8 signext",
        "call zeroext i8 @c_u8(i8 zeroext",
        "call signext i32 @c_u32(i32 signext",
        "declare signext i8 @c_i8(i8 signext)",
        "declare zeroext i8 @c_u8(i8 zeroext)",
        "declare signext i32 @c_u32(i32 signext)",
        "call i64 (i32, ...) @c_variadic(i32 signext 2, i32 signext %vararg1_sext, double %vararg2_f64)",
        "declare i64 @c_variadic(i32 signext, ...)",
    ] {
        assert!(ir.contains(expected), "missing `{expected}` in:\n{ir}");
    }
}

#[test]
fn odd_sized_aggregate_transport_matches_clang_ir_contracts() {
    let Some(clang) = clang_for_contract_tests() else {
        eprintln!("skipped: clang is unavailable for ABI IR comparison");
        return;
    };
    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/c_abi_edges");
    let dir = temp_case_dir("odd-aggregate-clang-contract");

    for target in [
        "x86_64-unknown-linux-gnu",
        "x86_64-unknown-freebsd",
        "aarch64-unknown-freebsd",
        "riscv64-unknown-freebsd",
        "x86_64-apple-darwin",
        "aarch64-unknown-linux-gnu",
        "aarch64-apple-darwin",
        "riscv64-unknown-linux-gnu",
        "aarch64-pc-windows-msvc",
        "x86_64-pc-windows-msvc",
        "x86_64-pc-windows-msvc",
        "aarch64-pc-windows-msvc",
        #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
        "loongarch64-unknown-linux-gnu",
    ] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        let tag = target.split('-').next().unwrap();
        let target_label = target.replace('-', "_");
        let clang_ir_path = dir.join(format!("{target_label}-clang.ll"));
        let clang_output = Command::new(&clang)
            .args(["-target", target, "-S", "-emit-llvm", "-ffreestanding"])
            .arg(fixture.join("interop.c"))
            .arg("-o")
            .arg(&clang_ir_path)
            .output()
            .unwrap();
        assert!(
            clang_output.status.success(),
            "clang ABI probe failed for {target}:\n{}",
            String::from_utf8_lossy(&clang_output.stderr)
        );
        let wave_dir = dir.join(format!("{target_label}-wave"));
        run_wavec([
            OsStr::new("build"),
            fixture.join("interop.wave").as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--emit=ir"),
            OsStr::new("--out-dir"),
            wave_dir.as_os_str(),
        ]);
        let clang_ir = fs::read_to_string(clang_ir_path).unwrap();
        let wave_ir = fs::read_to_string(wave_dir.join("interop.ll")).unwrap();
        if matches!(target, "x86_64-pc-windows-msvc") {
            // Win64 passes only 1/2/4/8-byte aggregates as integers. All other
            // sizes use an sret pointer and a caller-owned indirect argument.
            for (name, size) in [
                ("bytes1", 1),
                ("bytes2", 2),
                ("bytes3", 3),
                ("bytes4", 4),
                ("bytes5", 5),
                ("bytes6", 6),
                ("bytes7", 7),
                ("bytes8", 8),
                ("bytes9", 9),
                ("bytes12", 12),
                ("bytes16", 16),
                ("nested", 8),
                ("array_member", 6),
                ("pointer_member", 8),
            ] {
                for (ir, prefix) in [(&clang_ir, "c"), (&wave_ir, "wave")] {
                    let line = ir
                        .lines()
                        .find(|line| {
                            line.starts_with("define ")
                                && line.contains(&format!("@{prefix}_{name}("))
                        })
                        .unwrap();
                    if matches!(size, 1 | 2 | 4 | 8) {
                        assert!(
                            line.contains(&format!("i{} @{prefix}_{name}(i{}", size * 8, size * 8)),
                            "{target}: {line}"
                        );
                    } else {
                        assert!(
                            line.contains(&format!("void @{prefix}_{name}(ptr"))
                                && line.contains("sret(")
                                && line.contains(", ptr")
                                && !line.contains("byval"),
                            "{target}: {line}"
                        );
                    }
                }
            }
            assert!(wave_ir.contains("alloca %Bytes3, align 16"), "{wave_ir}");
            continue;
        }
        for size in [1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 16] {
            let object_integer = format!("i{}", size * 8);
            let (result, argument) = match (tag, size <= 8) {
                ("x86_64", true) => (object_integer.clone(), object_integer),
                ("x86_64", false) => {
                    let remainder = (size - 8) * 8;
                    (format!("{{ i64, i{remainder} }}"), format!("i64, i{remainder}"))
                },
                ("aarch64", true) => (object_integer, "i64".to_string()),
                ("aarch64", false) => ("[2 x i64]".to_string(), "[2 x i64]".to_string()),
                ("riscv64" | "loongarch64", true) => ("i64".to_string(), "i64".to_string()),
                ("riscv64" | "loongarch64", false) => {
                    ("[2 x i64]".to_string(), "[2 x i64]".to_string())
                },
                _ => unreachable!(),
            };
            let c_contract = format!("{result} @c_bytes{size}({argument}");
            let wave_contract = format!("{result} @wave_bytes{size}({argument}");
            let clang_c_definition = clang_ir
                .lines()
                .find(|line| line.contains(&format!("@c_bytes{size}(")))
                .unwrap_or_else(|| panic!("missing c_bytes{size} definition:\n{clang_ir}"))
                .replace(" %0", "")
                .replace(" %1", "");
            assert!(
                clang_c_definition.contains(&c_contract),
                "missing `{c_contract}` in `{clang_c_definition}`"
            );
            assert!(wave_ir.contains(&format!("declare {c_contract}")), "{wave_ir}");
            assert!(clang_ir.contains(&wave_contract), "missing `{wave_contract}`:\n{clang_ir}");
            let wave_definition = wave_ir
                .lines()
                .find(|line| line.contains(&format!("@wave_bytes{size}(")))
                .unwrap_or_else(|| panic!("missing wave_bytes{size} definition:\n{wave_ir}"))
                .replace(" %0", "")
                .replace(" %1", "");
            assert!(
                wave_definition.contains(&format!("define {wave_contract}")),
                "missing `{wave_contract}` in `{wave_definition}`"
            );
        }
        for contract in ["void @c_empty()", "void @wave_empty()"] {
            assert!(clang_ir.contains(contract), "missing `{contract}`:\n{clang_ir}");
        }
        assert!(wave_ir.contains("declare void @c_empty()"), "{wave_ir}");
        assert!(wave_ir.contains("define void @wave_empty()"), "{wave_ir}");

        let aggregate_contracts: &[(&str, &str, &str)] = match tag {
            "x86_64" => &[
                ("nested", "i64", "i64"),
                ("array_member", "i48", "i48"),
                ("pointer_member", "ptr", "ptr"),
            ],
            "aarch64" => &[
                ("nested", "i64", "i64"),
                ("array_member", "i48", "i64"),
                ("pointer_member", "i64", "ptr"),
            ],
            "riscv64" | "loongarch64" => &[
                ("nested", "i64", "i64"),
                ("array_member", "i64", "i64"),
                ("pointer_member", "i64", "i64"),
            ],
            _ => unreachable!(),
        };
        for (name, result, argument) in aggregate_contracts {
            let c_contract = format!("{result} @c_{name}({argument}");
            let wave_contract = format!("{result} @wave_{name}({argument}");
            // Some Clang builds spell a one-pointer aggregate transported in
            // one x86_64 or AArch64 GPR as `i64`, while others preserve the
            // opaque `ptr` in IR. Both spellings describe the same ABI slot.
            let mut clang_contracts = vec![c_contract.clone()];
            if matches!(tag, "x86_64" | "aarch64") && *name == "pointer_member" {
                clang_contracts.push("i64 @c_pointer_member(i64".to_string());
            }
            let clang_definition = clang_ir
                .lines()
                .find(|line| line.contains(&format!("@c_{name}(")))
                .unwrap_or_else(|| panic!("missing c_{name} definition:\n{clang_ir}"))
                .replace(" %0", "");
            assert!(
                clang_contracts.iter().any(|contract| clang_definition.contains(contract)),
                "missing one of {clang_contracts:?} for {target} in `{clang_definition}`"
            );
            assert!(wave_ir.contains(&format!("declare {c_contract}")), "{wave_ir}");
            let mut clang_wave_contracts = vec![wave_contract.clone()];
            if matches!(tag, "x86_64" | "aarch64") && *name == "pointer_member" {
                clang_wave_contracts.push("i64 @wave_pointer_member(i64".to_string());
            }
            assert!(
                clang_wave_contracts.iter().any(|contract| clang_ir.contains(contract)),
                "missing one of {clang_wave_contracts:?} for {target}:\n{clang_ir}"
            );
            let wave_definition = wave_ir
                .lines()
                .find(|line| line.contains(&format!("@wave_{name}(")))
                .unwrap_or_else(|| panic!("missing wave_{name} definition:\n{wave_ir}"))
                .replace(" %0", "");
            assert!(
                wave_definition.contains(&format!("define {wave_contract}")),
                "{wave_definition}"
            );
        }
    }
}

#[test]
fn narrow_integer_c_abi_attributes_match_clang_targets() {
    let Some(clang) = clang_for_contract_tests() else {
        eprintln!("skipped: clang is unavailable for ABI IR comparison");
        return;
    };
    let dir = temp_case_dir("narrow-integer-c-abi-contract");
    let fixture =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/c_abi_edges/narrow.c");
    let source = write_wave(
        &dir,
        "narrow.wave",
        r#"
extern(c) fun c_i8(value: i8) -> i8;
extern(c) fun c_u8(value: u8) -> u8;
extern(c) fun c_i16(value: i16) -> i16;
extern(c) fun c_u16(value: u16) -> u16;
extern(c) fun c_i32(value: i32) -> i32;
extern(c) fun c_u32(value: u32) -> u32;
export(c) fun wave_i8(value: i8) -> i8 { return value; }
export(c) fun wave_u8(value: u8) -> u8 { return value; }
export(c) fun wave_i16(value: i16) -> i16 { return value; }
export(c) fun wave_u16(value: u16) -> u16 { return value; }
export(c) fun wave_i32(value: i32) -> i32 { return value; }
export(c) fun wave_u32(value: u32) -> u32 { return value; }
fun main() -> i32 { return c_i8(-1) as i32 + c_u8(1) as i32 + c_i16(-1) as i32 + c_u16(1) as i32 + c_i32(-1) + c_u32(1) as i32; }
"#,
    );
    for target in [
        "x86_64-unknown-linux-gnu",
        "x86_64-unknown-freebsd",
        "aarch64-unknown-freebsd",
        "riscv64-unknown-freebsd",
        "x86_64-apple-darwin",
        "aarch64-apple-darwin",
        "aarch64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "riscv64-unknown-linux-gnu",
        "aarch64-pc-windows-msvc",
        "x86_64-pc-windows-msvc",
        "aarch64-pc-windows-msvc",
        #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
        "loongarch64-unknown-linux-gnu",
    ] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        let out = dir.join(target);
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--emit=ir"),
            OsStr::new("--out-dir"),
            out.as_os_str(),
        ]);
        let ir = fs::read_to_string(out.join("narrow.ll")).unwrap();
        let clang_ir_path = dir.join(format!("{}-clang.ll", target));
        let clang_output = Command::new(&clang)
            .args(["-target", target, "-S", "-emit-llvm", "-ffreestanding"])
            .arg(&fixture)
            .arg("-o")
            .arg(&clang_ir_path)
            .output()
            .unwrap();
        assert!(
            clang_output.status.success(),
            "clang ABI probe failed for {target}:\n{}",
            String::from_utf8_lossy(&clang_output.stderr)
        );
        let clang_ir = fs::read_to_string(clang_ir_path).unwrap();

        for name in ["i8", "u8", "i16", "u16", "i32", "u32"] {
            let clang_line = clang_ir
                .lines()
                .find(|line| line.starts_with("define") && line.contains(&format!("@c_{name}(")))
                .unwrap_or_else(|| panic!("missing Clang definition for {name}:\n{clang_ir}"));
            let wave_definition = ir
                .lines()
                .find(|line| line.starts_with("define") && line.contains(&format!("@wave_{name}(")))
                .unwrap_or_else(|| panic!("missing Wave definition for {name}:\n{ir}"));
            let wave_declaration = ir
                .lines()
                .find(|line| line.starts_with("declare") && line.contains(&format!("@c_{name}(")))
                .unwrap_or_else(|| panic!("missing Wave declaration for {name}:\n{ir}"));
            let extension = if clang_line.contains("signext") {
                Some("signext")
            } else if clang_line.contains("zeroext") {
                Some("zeroext")
            } else {
                None
            };
            let clang_count = extension.map_or(0, |value| clang_line.matches(value).count());
            let wave_definition_count =
                extension.map_or(0, |value| wave_definition.matches(value).count());
            let wave_declaration_count =
                extension.map_or(0, |value| wave_declaration.matches(value).count());
            assert_eq!(
                wave_definition_count, clang_count,
                "target {target}, type {name}: Clang `{clang_line}`, Wave `{wave_definition}`"
            );
            assert_eq!(
                wave_declaration_count, clang_count,
                "target {target}, type {name}: Clang `{clang_line}`, Wave `{wave_declaration}`"
            );
            if extension.is_none() {
                assert!(!wave_definition.contains("signext"));
                assert!(!wave_definition.contains("zeroext"));
                assert!(!wave_declaration.contains("signext"));
                assert!(!wave_declaration.contains("zeroext"));
            }
        }
    }
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
fn c_variadic_promotions_use_semantic_expression_types() {
    let dir = temp_case_dir("c-variadic-semantic-promotions");
    let source = write_wave(
        &dir,
        "promotions.wave",
        r#"
extern(c) fun consume(count: i32, ...) -> i64;
fun signed_result() -> i8 { return -1; }
fun main() -> i32 {
    var signed: i8 = -128;
    var unsigned: u8 = 255;
    var zero: i8 = 0;
    var one: i8 = 1;
    consume(10, signed, unsigned, signed + 127, zero - one, signed * zero - one, signed < zero, !zero, (signed + 127) * one, signed_result(), 255 as u8);
    consume(2, null as ptr<byte>, null as ptr<i32>);
    return 0;
}
"#,
    );
    let out = dir.join("out");
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        out.as_os_str(),
    ]);
    let ir = fs::read_to_string(out.join("promotions.ll")).unwrap();
    for expected in [
        "%vararg1_sext = sext i8",
        "%vararg2_zext = zext i8",
        "%vararg3_sext = sext i8",
        "%vararg4_sext = sext i8",
        "%vararg5_sext = sext i8",
        "%vararg6_zext = zext i1",
        "%vararg7_zext = zext i1",
        "%vararg8_sext = sext i8",
        "%vararg9_sext = sext i8",
    ] {
        assert!(ir.contains(expected), "missing `{expected}`:\n{ir}");
    }
    assert!(ir.contains("i32 signext 255"), "{ir}");
    assert!(ir.contains("ptr null, ptr null"), "{ir}");

    let invalid = write_wave(
        &dir,
        "untyped_null.wave",
        "extern(c) fun consume(count: i32, ...) -> i64; fun main() { consume(1, null); }\n",
    );
    let error = run_wavec_expect_failure([OsStr::new("check"), invalid.as_os_str()]);
    assert!(error.contains("variadic argument 2") && error.contains("no scalar type"), "{error}");
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
fn discarded_c_return_values_do_not_require_an_expected_type() {
    let dir = temp_case_dir("discarded-c-return-values");
    let source = write_wave(
        &dir,
        "discard.wave",
        r#"
struct Empty {}
struct Pair { first: i64; second: i64; }
extern(c) fun c_empty(value: Empty) -> Empty;
extern(c) fun c_pair(value: Pair) -> Pair;
extern(c) fun c_integer() -> i64;
extern(c) fun c_float() -> f64;
extern(c) fun c_pointer() -> ptr<byte>;
fun main() {
    c_empty(Empty {});
    c_pair(Pair { first: 1, second: 2 });
    c_integer();
    c_float();
    c_pointer();
}
"#,
    );
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        dir.join("out").as_os_str(),
    ]);
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
fn riscv_link_input_abi_is_validated_before_linking() {
    let dir = temp_case_dir("riscv-pre-link-abi");
    let source = write_wave(&dir, "main.wave", "fun main() -> i32 { return 0; }\n");
    let mut objects = Vec::new();
    for abi in ["lp64", "lp64f", "lp64d"] {
        let out = dir.join(abi);
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-linux-gnu"),
            OsStr::new("--abi"),
            OsStr::new(abi),
            OsStr::new("--emit=obj"),
            OsStr::new("--out-dir"),
            out.as_os_str(),
        ]);
        objects.push((abi, out.join("main.o")));
    }
    for (abi, object) in &objects {
        let expected = RiscvFloatAbi::from_target_abi(abi).unwrap();
        validate_riscv_link_inputs(expected, &[object.display().to_string()]).unwrap();
    }
    let error =
        validate_riscv_link_inputs(RiscvFloatAbi::Lp64d, &[objects[0].1.display().to_string()])
            .unwrap_err()
            .to_string();
    assert!(error.contains("target ABI: LP64D"), "{error}");
    assert!(error.contains("input ABI: LP64"), "{error}");

    let llvm_ar = llvm_ar_for_contract_tests()
        .expect("llvm-ar is required to build cross-target archive fixtures");
    let archive = dir.join("libmixed.a");
    let archive_output = Command::new(&llvm_ar)
        .arg("--format=darwin")
        .arg("rcs")
        .arg(&archive)
        .arg(&objects[1].1)
        .output()
        .expect("failed to start llvm-ar");
    assert!(
        archive_output.status.success(),
        "{} failed to build the archive fixture:\n{}",
        llvm_ar.display(),
        String::from_utf8_lossy(&archive_output.stderr)
    );
    let archive_error =
        validate_riscv_link_inputs(RiscvFloatAbi::Lp64d, &[archive.display().to_string()])
            .unwrap_err()
            .to_string();
    assert!(archive_error.contains("libmixed.a(main.o)"), "{archive_error}");
    assert!(archive_error.contains("input ABI: LP64F"), "{archive_error}");

    for (input, expected_input_abi) in [(&objects[0].1, "LP64"), (&archive, "LP64F")] {
        let output = run_wavec_raw([
            OsStr::new("build"),
            source.as_os_str(),
            input.as_os_str(),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-linux-gnu"),
            OsStr::new("--abi=lp64d"),
            OsStr::new("--no-start-files"),
            OsStr::new("-Cno-default-libs"),
            OsStr::new("-Clinker=/bin/false"),
            OsStr::new("--out-dir"),
            dir.join("link-attempt").as_os_str(),
        ]);
        assert!(!output.status.success());
        let error = String::from_utf8_lossy(&output.stderr);
        assert!(error.contains("RISC-V floating-point ABI mismatch before linking"), "{error}");
        assert!(error.contains("target ABI: LP64D"), "{error}");
        assert!(error.contains(&format!("input ABI: {expected_input_abi}")), "{error}");
        assert!(
            !error.contains("link failed"),
            "external linker ran before ABI validation: {error}"
        );
    }
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
#[test]
fn loongarch64_link_inputs_require_matching_abi_before_linking() {
    let dir = temp_case_dir("loongarch64-pre-link-abi");
    let source = write_wave(&dir, "main.wave", "fun main() -> i32 { return 0; }\n");
    let out = dir.join("out");
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target=loongarch64-unknown-linux-gnu"),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        out.as_os_str(),
    ]);
    let compatible = out.join("main.o");
    validate_loongarch64_link_inputs(LoongArchFloatAbi::Lp64d, &[compatible.display().to_string()])
        .unwrap();

    let incompatible = dir.join("soft-float.o");
    let mut bytes = fs::read(&compatible).unwrap();
    bytes[48..52].copy_from_slice(&0x41_u32.to_le_bytes());
    fs::write(&incompatible, bytes).unwrap();
    let error = validate_loongarch64_link_inputs(
        LoongArchFloatAbi::Lp64d,
        &[incompatible.display().to_string()],
    )
    .unwrap_err()
    .to_string();
    assert!(error.contains("target ABI: LP64D"), "{error}");
    assert!(error.contains("input ABI: LP64S"), "{error}");

    let output = run_wavec_raw([
        OsStr::new("build"),
        source.as_os_str(),
        incompatible.as_os_str(),
        OsStr::new("--target=loongarch64-unknown-linux-gnu"),
        OsStr::new("--no-start-files"),
        OsStr::new("-Cno-default-libs"),
        OsStr::new("-Clinker=/bin/false"),
        OsStr::new("--out-dir"),
        dir.join("link-attempt").as_os_str(),
    ]);
    assert!(!output.status.success());
    let error = String::from_utf8_lossy(&output.stderr);
    assert!(error.contains("LoongArch floating-point ABI mismatch before linking"), "{error}");
    assert!(!error.contains("link failed"), "external linker ran before ABI validation: {error}");
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
#[test]
fn loongarch64_float_abi_modes_match_clang_contracts() {
    let dir = temp_case_dir("loongarch64-float-abi-modes");
    let source = write_wave(
        &dir,
        "modes.wave",
        r#"
struct F1 { x: f32; }
struct D1 { x: f64; }
struct FF { x: f32; y: f32; }
struct FD { x: f32; y: f64; }
extern(c) fun c_f1(x: F1) -> F1;
extern(c) fun c_d1(x: D1) -> D1;
extern(c) fun c_ff(x: FF) -> FF;
extern(c) fun c_fd(x: FD) -> FD;
fun main() -> i32 { return 0; }
"#,
    );

    let contracts = [
        (
            "lp64s",
            0x1,
            [
                "declare i64 @c_f1(i64)",
                "declare i64 @c_d1(i64)",
                "declare i64 @c_ff(i64)",
                "declare [2 x i64] @c_fd([2 x i64])",
            ],
        ),
        (
            "lp64f",
            0x2,
            [
                "declare float @c_f1(float)",
                "declare i64 @c_d1(i64)",
                "declare { float, float } @c_ff(float, float)",
                "declare [2 x i64] @c_fd([2 x i64])",
            ],
        ),
        (
            "lp64d",
            0x3,
            [
                "declare float @c_f1(float)",
                "declare double @c_d1(double)",
                "declare { float, float } @c_ff(float, float)",
                "declare { float, double } @c_fd(float, double)",
            ],
        ),
    ];

    for (abi, expected_flags, declarations) in contracts {
        let output = dir.join(abi);
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target=loongarch64-unknown-linux-gnu"),
            OsStr::new("--abi"),
            OsStr::new(abi),
            OsStr::new("--emit=ir,obj"),
            OsStr::new("--out-dir"),
            output.as_os_str(),
        ]);
        let ir = fs::read_to_string(output.join("modes.ll")).unwrap();
        for declaration in declarations {
            assert!(ir.contains(declaration), "{abi}: missing `{declaration}`\n{ir}");
        }
        let object = output.join("modes.o");
        assert_eq!(loongarch64_elf_flags(&object) & 0x7, expected_flags);
        let target_abi = LoongArchFloatAbi::from_target_abi(abi).unwrap();
        validate_loongarch64_link_inputs(target_abi, &[object.display().to_string()]).unwrap();

        let assembly = dir.join(format!("raw-{abi}.s"));
        fs::write(&assembly, "ret\n").unwrap();
        let assembly_output = dir.join(format!("asm-{abi}"));
        run_wavec([
            OsStr::new("build"),
            assembly.as_os_str(),
            OsStr::new("--target=loongarch64-unknown-linux-gnu"),
            OsStr::new("--abi"),
            OsStr::new(abi),
            OsStr::new("--emit=obj"),
            OsStr::new("--out-dir"),
            assembly_output.as_os_str(),
        ]);
        assert_eq!(
            loongarch64_elf_flags(&assembly_output.join(format!("raw-{abi}.o"))) & 0x7,
            expected_flags
        );
    }
}

#[test]
fn windows_arm64_c_abi_emits_msvc_objects() {
    if llvm::codegen::target::target_spec_for_triple("aarch64-pc-windows-msvc").is_none() {
        return;
    }
    let dir = temp_case_dir("windows-arm64-c-abi-interop");
    let fixture_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/c_abi_edges");
    let c_object = dir.join("interop-c.obj");
    let wave_object = dir.join("interop.o");
    let clang = std::env::var("WAVE_CLANG").unwrap_or_else(|_| "clang".to_string());
    let compiled = Command::new(&clang)
        .args([
            "--target=aarch64-pc-windows-msvc",
            "-O2",
            "-fno-builtin",
            "-fno-stack-protector",
            "-c",
        ])
        .arg(fixture_dir.join("interop.c"))
        .arg("-o")
        .arg(&c_object)
        .output()
        .unwrap();
    assert!(compiled.status.success(), "{}", String::from_utf8_lossy(&compiled.stderr));
    run_wavec([
        OsStr::new("build"),
        fixture_dir.join("interop.wave").as_os_str(),
        OsStr::new("--target=aarch64-pc-windows-msvc"),
        OsStr::new("--emit=obj"),
        OsStr::new("-o"),
        wave_object.as_os_str(),
    ]);
    for object in [&c_object, &wave_object] {
        let bytes = fs::read(object).unwrap();
        assert_eq!(u16::from_le_bytes(bytes[..2].try_into().unwrap()), 0xaa64);
    }
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-x86"))]
fn x86_64_c_abi_interoperates_with_c() {
    if std::env::var_os("WAVE_RUN_X86_64_INTEROP_TESTS").is_none() {
        eprintln!("skipped: set WAVE_RUN_X86_64_INTEROP_TESTS=1 to run native ABI test");
        return;
    }

    assert_eq!(std::env::consts::ARCH, "x86_64");
    assert_eq!(std::env::consts::OS, "linux");
    run_linux_c_abi_fixture("x86_64_sysv", "x86_64-unknown-linux-gnu", "gcc", None);
    run_linux_c_abi_fixture("c_abi_edges", "x86_64-unknown-linux-gnu", "gcc", None);
    for optimization in ["-O0", "-O2"] {
        run_linux_c_abi_fixture_at_optimization(
            "x86_64_sysv_pressure",
            "x86_64-unknown-linux-gnu",
            "gcc",
            None,
            optimization,
        );
    }
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-aarch64"))]
fn aarch64_c_abi_interoperates_with_c() {
    if std::env::var_os("WAVE_RUN_AARCH64_INTEROP_TESTS").is_none() {
        eprintln!("skipped: set WAVE_RUN_AARCH64_INTEROP_TESTS=1 to run ABI test");
        return;
    }

    assert_eq!(std::env::consts::OS, "linux");
    let native = std::env::consts::ARCH == "aarch64";
    run_linux_c_abi_fixture(
        "aarch64_aapcs64",
        "aarch64-unknown-linux-gnu",
        if native { "gcc" } else { "aarch64-linux-gnu-gcc" },
        if native { None } else { Some("qemu-aarch64") },
    );
    run_linux_c_abi_fixture(
        "c_abi_edges",
        "aarch64-unknown-linux-gnu",
        if native { "gcc" } else { "aarch64-linux-gnu-gcc" },
        if native { None } else { Some("qemu-aarch64") },
    );
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
fn riscv64_c_abi_interoperates_with_c_under_qemu() {
    if std::env::var_os("WAVE_RUN_RISCV64_INTEROP_TESTS").is_none() {
        eprintln!("skipped: set WAVE_RUN_RISCV64_INTEROP_TESTS=1 to run cross-toolchain test");
        return;
    }

    let dir = temp_case_dir("riscv64-c-abi-interop");
    let fixture_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("riscv64_psabi");
    let fixture_wave = fixture_dir.join("interop.wave");
    let c_object = dir.join("interop-c.o");
    let wave_out = dir.join("wave");
    let binary = dir.join("interop");

    let c_compile = Command::new("riscv64-linux-gnu-gcc")
        .args([
            "-march=rv64gc",
            "-mabi=lp64d",
            "-msmall-data-limit=0",
            "-O2",
            "-ffreestanding",
            "-fno-builtin",
            "-fno-stack-protector",
            "-c",
        ])
        .arg(fixture_dir.join("interop.c"))
        .arg("-o")
        .arg(&c_object)
        .output()
        .expect("failed to start riscv64-linux-gnu-gcc");
    assert!(
        c_compile.status.success(),
        "C fixture compile failed:\n{}",
        String::from_utf8_lossy(&c_compile.stderr)
    );

    run_wavec([
        OsStr::new("build"),
        fixture_wave.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("riscv64-unknown-linux-gnu"),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        wave_out.as_os_str(),
    ]);

    let link = Command::new("riscv64-linux-gnu-gcc")
        .args(["-march=rv64gc", "-mabi=lp64d", "-nostdlib", "-static", "-Wl,-e,_start"])
        .arg(wave_out.join("interop.o"))
        .arg(&c_object)
        .arg("-o")
        .arg(&binary)
        .output()
        .expect("failed to start riscv64-linux-gnu-gcc linker");
    assert!(
        link.status.success(),
        "RISC-V fixture link failed:\n{}",
        String::from_utf8_lossy(&link.stderr)
    );

    let run =
        Command::new("qemu-riscv64").arg(&binary).output().expect("failed to start qemu-riscv64");
    assert!(
        run.status.success(),
        "C/Wave psABI fixture failed with status {}\nstdout:\n{}\nstderr:\n{}",
        run.status,
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );

    run_linux_c_abi_fixture(
        "c_abi_edges",
        "riscv64-unknown-linux-gnu",
        "riscv64-linux-gnu-gcc",
        Some("qemu-riscv64"),
    );
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
#[test]
fn loongarch64_lp64d_c_abi_interoperates_with_clang_under_qemu() {
    run_loongarch64_c_abi_fixture("lp64d", "loongarch64_psabi");
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
#[test]
fn loongarch64_lp64s_c_abi_interoperates_with_clang_under_qemu() {
    run_loongarch64_c_abi_fixture("lp64s", "loongarch64_lp64s");
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
fn riscv64_lp64_abi_modes_interoperate_with_c_under_qemu() {
    if std::env::var_os("WAVE_RUN_RISCV64_INTEROP_TESTS").is_none() {
        eprintln!("skipped: set WAVE_RUN_RISCV64_INTEROP_TESTS=1 to run cross-toolchain test");
        return;
    }

    let fixture_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("riscv64_psabi");

    for (abi, march, features) in [
        ("lp64", "rv64imac", "+m,+a,-f,-d,+c"),
        ("lp64f", "rv64imafc", "+m,+a,+f,-d,+c"),
        ("lp64d", "rv64gc", "+m,+a,+f,+d,+c"),
    ] {
        let dir = temp_case_dir(&format!("riscv64-{abi}-c-abi-interop"));
        let c_object = dir.join("abi-modes-c.o");
        let wave_out = dir.join("wave");
        let binary = dir.join("abi-modes");

        let c_compile = Command::new("riscv64-linux-gnu-gcc")
            .arg(format!("-march={march}"))
            .arg(format!("-mabi={abi}"))
            .args([
                "-msmall-data-limit=0",
                "-O2",
                "-ffreestanding",
                "-fno-builtin",
                "-fno-stack-protector",
                "-c",
            ])
            .arg(fixture_dir.join("abi_modes.c"))
            .arg("-o")
            .arg(&c_object)
            .output()
            .expect("failed to start riscv64-linux-gnu-gcc");
        assert!(
            c_compile.status.success(),
            "{abi} C fixture compile failed:\n{}",
            String::from_utf8_lossy(&c_compile.stderr)
        );

        run_wavec([
            OsStr::new("build"),
            fixture_dir.join("abi_modes.wave").as_os_str(),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-linux-gnu"),
            OsStr::new("--features"),
            OsStr::new(features),
            OsStr::new("--abi"),
            OsStr::new(abi),
            OsStr::new("--emit=obj"),
            OsStr::new("--out-dir"),
            wave_out.as_os_str(),
        ]);

        let link = Command::new("riscv64-linux-gnu-gcc")
            .arg(format!("-march={march}"))
            .arg(format!("-mabi={abi}"))
            .args(["-nostdlib", "-static", "-Wl,-e,_start"])
            .arg(wave_out.join("abi_modes.o"))
            .arg(&c_object)
            .arg("-o")
            .arg(&binary)
            .output()
            .expect("failed to start riscv64-linux-gnu-gcc linker");
        assert!(
            link.status.success(),
            "{abi} RISC-V fixture link failed:\n{}",
            String::from_utf8_lossy(&link.stderr)
        );

        let run = Command::new("qemu-riscv64")
            .arg(&binary)
            .output()
            .expect("failed to start qemu-riscv64");
        assert!(
            run.status.success(),
            "{abi} C/Wave psABI fixture failed with status {}",
            run.status
        );
    }
}
