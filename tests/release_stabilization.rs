// This file is part of the Wave language project.
// SPDX-License-Identifier: MPL-2.0
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicU64, Ordering};

static NEXT: AtomicU64 = AtomicU64::new(0);
struct Case(PathBuf);
impl Case {
    fn new() -> Self {
        let path = std::env::temp_dir().join(format!(
            "wave-release-fixes-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir_all(&path).unwrap();
        Self(path)
    }
    fn command(&self) -> Command {
        let mut command = Command::new(env!("CARGO_BIN_EXE_wavec"));
        command.current_dir(&self.0).env("NO_COLOR", "1");
        command
    }
    fn write(&self, path: &str, text: &str) -> PathBuf {
        let path = self.0.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(&path, text).unwrap();
        path
    }
}
impl Drop for Case {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}
fn success(out: Output) {
    assert!(
        out.status.success(),
        "{}\n{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
}
fn rejected(out: Output, message: &str) {
    assert!(!out.status.success());
    assert!(
        String::from_utf8_lossy(&out.stderr).contains(message),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
}
fn source() -> &'static str {
    "fun main() -> i32 { return 0; }\n"
}

#[test]
fn output_aliases_preserve_all_input_bytes_before_any_emission() {
    let case = Case::new();
    let input = case.write("source.wave", source());
    for output in [input.clone(), case.0.join("./source.wave")] {
        rejected(
            case.command()
                .args(["build", "source.wave", "--emit=obj", "-o"])
                .arg(output)
                .output()
                .unwrap(),
            "aliases compiler input",
        );
        assert_eq!(fs::read_to_string(&input).unwrap(), source());
    }
    let hard = case.0.join("hard.o");
    fs::hard_link(&input, &hard).unwrap();
    rejected(
        case.command()
            .args(["build", "source.wave", "--emit=obj", "-o", "hard.o"])
            .output()
            .unwrap(),
        "aliases compiler input",
    );
    assert_eq!(fs::read_to_string(&input).unwrap(), source());
    // An auxiliary IR emission must not clobber a different command-line input.
    let ir = case.write("source_1.ll", "define i32 @other() { ret i32 0 }\n");
    rejected(
        case.command()
            .args(["build", "source.wave", "source_1.ll", "--emit=ir,obj"])
            .output()
            .unwrap(),
        "aliases compiler input",
    );
    assert_eq!(
        fs::read_to_string(ir).unwrap(),
        "define i32 @other() { ret i32 0 }\n"
    );
    assert!(!case.0.join("source_1.o").exists());
}

#[test]
fn identity_ir_emits_are_safe_for_relative_absolute_and_hardlink_aliases() {
    let case = Case::new();
    let text = "define i32 @main() { ret i32 0 }\n";
    let input = case.write("identity.ll", text);
    for path in [PathBuf::from("identity.ll"), input.clone()] {
        success(
            case.command()
                .arg("build")
                .arg(path)
                .args(["--emit=ir", "--out-dir", "."])
                .output()
                .unwrap(),
        );
        assert_eq!(fs::read_to_string(&input).unwrap(), text);
    }
    fs::create_dir(case.0.join("out")).unwrap();
    fs::hard_link(&input, case.0.join("out/identity.ll")).unwrap();
    success(
        case.command()
            .args(["build", "identity.ll", "--emit=ir", "--out-dir", "out"])
            .output()
            .unwrap(),
    );
    assert_eq!(fs::read_to_string(&input).unwrap(), text);
}

#[cfg(unix)]
#[test]
fn symlink_and_imported_source_outputs_are_rejected() {
    use std::os::unix::fs::symlink;
    let case = Case::new();
    let input = case.write("source.wave", source());
    symlink(&input, case.0.join("alias.o")).unwrap();
    rejected(
        case.command()
            .args(["build", "source.wave", "--emit=obj", "-o", "alias.o"])
            .output()
            .unwrap(),
        "aliases compiler input",
    );
    let imported = "pub const VALUE: i32 = 0;\n";
    case.write("dep.wave", imported);
    case.write(
        "main.wave",
        "import(\"./dep.wave\")::{VALUE}; fun main() -> i32 { return VALUE; }",
    );
    rejected(
        case.command()
            .args(["build", "main.wave", "--emit=obj", "-o", "dep.wave"])
            .output()
            .unwrap(),
        "aliases compiler input",
    );
    assert_eq!(
        fs::read_to_string(case.0.join("dep.wave")).unwrap(),
        imported
    );
}

fn stub_std(case: &Case, name: &str, value: i32) -> PathBuf {
    let root = case.0.join(name);
    fs::create_dir_all(&root).unwrap();
    fs::copy(
        Path::new(env!("CARGO_MANIFEST_DIR")).join("std/manifest.json"),
        root.join("manifest.json"),
    )
    .unwrap();
    fs::write(
        root.join("value.wave"),
        format!("pub const VALUE: i32 = {value};"),
    )
    .unwrap();
    root
}

#[test]
fn explicit_std_roots_are_isolated_and_never_fall_back() {
    let case = Case::new();
    let first = stub_std(&case, "std 한글 one", 7);
    let second = stub_std(&case, "second", 9);
    let input = case.write(
        "main.wave",
        "import(\"std::value\")::{VALUE}; fun main() -> i32 { return VALUE; }",
    );
    for (root, value) in [(&first, "7"), (&second, "9")] {
        success(
            case.command()
                .arg("--std-root")
                .arg(root)
                .arg("build")
                .arg(&input)
                .args(["--emit=ir", "--out-dir", "out"])
                .output()
                .unwrap(),
        );
        let ir = fs::read_to_string(case.0.join("out/main.ll")).unwrap();
        assert!(ir.contains(&format!("ret i32 {value}")), "{ir}");
    }
    rejected(
        case.command()
            .args(["--std-root", "missing", "check"])
            .arg(&input)
            .output()
            .unwrap(),
        "cannot resolve std root",
    );
    fs::write(
        first.join("manifest.json"),
        "{\"name\":\"std\",\"compatibility_revision\":999999}",
    )
    .unwrap();
    rejected(
        case.command()
            .arg("--std-root")
            .arg(&first)
            .args(["check", "main.wave"])
            .output()
            .unwrap(),
        "compatibility revision",
    );
    fs::remove_file(first.join("manifest.json")).unwrap();
    rejected(
        case.command()
            .arg("--std-root")
            .arg(&first)
            .args(["check", "main.wave"])
            .output()
            .unwrap(),
        "compatibility revision",
    );
    fs::write(first.join("manifest.json"), "malformed").unwrap();
    rejected(
        case.command()
            .arg("--std-root")
            .arg(&first)
            .args(["check", "main.wave"])
            .output()
            .unwrap(),
        "invalid",
    );
    // Explicit roots are checked even when the source has no std imports.
    case.write("plain.wave", source());
    rejected(
        case.command()
            .args(["--std-root", "missing", "check", "plain.wave"])
            .output()
            .unwrap(),
        "cannot resolve std root",
    );
}

#[test]
fn constant_cycles_fail_in_the_frontend_but_forward_dags_remain_valid() {
    let case = Case::new();
    for source in [
        "const A: i32 = A; fun main() -> i32 { return A; }",
        "const A: i32 = B; const B: i32 = C; const C: i32 = A; fun main() -> i32 { return A; }",
    ] {
        case.write("cycle.wave", source);
        for (mode, format) in [
            ("check", "human"),
            ("build", "human"),
            ("check", "json"),
            ("build", "json"),
        ] {
            let mut command = case.command();
            command.arg(format!("--error-format={format}"));
            command.args([mode, "cycle.wave"]);
            if mode == "build" {
                command.arg("--emit=ir");
            }
            let output = command.output().unwrap();
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains("E3001"), "{stderr}");
            if format == "json" {
                let diagnostic = utils::json::parse(stderr.trim()).unwrap();
                assert_eq!(diagnostic.get("error").unwrap().get_num("line"), Some(1.0));
            }
            rejected(output, "constant dependency cycle");
        }
    }
    case.write("dep.wave", "pub const A: i32 = B; const B: i32 = A;");
    case.write(
        "imported.wave",
        "import(\"./dep.wave\")::{A}; fun main() -> i32 { return A; }",
    );
    let out = case
        .command()
        .args(["check", "imported.wave"])
        .output()
        .unwrap();
    let error = String::from_utf8_lossy(&out.stderr);
    assert!(error.contains("dep.wave"), "{error}");
    rejected(out, "constant dependency cycle");
    case.write("dag.wave", "const A: i32 = B + C; const B: i32 = D; const C: i32 = D; const D: i32 = 3; fun main() -> i32 { return A; }");
    success(case.command().args(["check", "dag.wave"]).output().unwrap());
    case.write(
        "dag.wave",
        "const B: i32 = D; const C: i32 = D; const D: i32 = 3; fun main() -> i32 { return B + C; }",
    );
    success(
        case.command()
            .args(["build", "dag.wave", "--emit=ir", "-O2"])
            .output()
            .unwrap(),
    );
    assert!(fs::read_to_string(case.0.join("dag.ll"))
        .unwrap()
        .contains("ret i32 6"));
}

#[cfg(feature = "llvm-target-wasm")]
#[test]
fn webassembly_runtime_is_freestanding_and_wasi_retries_preserve_errors() {
    if std::env::var_os("WAVE_RUN_WASM_RUNTIME_TESTS").is_none() {
        eprintln!("set WAVE_RUN_WASM_RUNTIME_TESTS=1 for Node/wasm-ld execution");
        return;
    }
    let case = Case::new();
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    for (fixture, stem, target) in [
        ("wasi_retry", "retry", "wasm32-wasip1"),
        ("wasm_wide", "wide", "wasm64-unknown-unknown"),
    ] {
        for opt in ["-O0", "-O2"] {
            let output = case.0.join(format!("{fixture}{opt}"));
            success(
                case.command()
                    .arg("--std-root")
                    .arg(root.join("std"))
                    .arg("build")
                    .arg(root.join(format!("tests/fixtures/{fixture}/{stem}.wave")))
                    .args(["--target", target, "--emit=obj,ir", opt, "--out-dir"])
                    .arg(&output)
                    .output()
                    .unwrap(),
            );
            let wasm = output.join("module.wasm");
            let mut linker = Command::new("wasm-ld");
            if target.starts_with("wasm64") {
                linker.arg("-mwasm64");
            }
            linker.args(["--no-entry", "--export-memory"]);
            if fixture == "wasm_wide" {
                linker.arg("--export-all");
            } else {
                for name in [
                    "read_bytes",
                    "write_bytes",
                    "close_fd",
                    "sleep_ns",
                    "raw_sleep_ns",
                    "clock_read",
                ] {
                    linker.arg(format!("--export={name}"));
                }
            }
            success(
                linker
                    .arg(output.join(format!("{stem}.o")))
                    .arg("-o")
                    .arg(&wasm)
                    .output()
                    .unwrap(),
            );
            let mut node = Command::new("node");
            if target.starts_with("wasm64") {
                node.arg("--experimental-wasm-memory64");
            }
            success(
                node.arg(root.join(format!("tests/fixtures/{fixture}/host.cjs")))
                    .arg(&wasm)
                    .output()
                    .unwrap(),
            );
            if fixture == "wasi_retry" {
                success(
                    Command::new("node")
                        .arg(root.join("tests/fixtures/wasi_retry/real_host.cjs"))
                        .arg(&wasm)
                        .output()
                        .unwrap(),
                );
            }
        }
    }
}

#[cfg(any(feature = "llvm-target-riscv", feature = "llvm-target-aarch64"))]
fn aggregate_interop(arch: &str, abi: Option<&str>) {
    let case = Case::new();
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let target = format!("{arch}-unknown-linux-gnu");
    let mut clang_args = vec![format!("--target={target}")];
    if let Some(abi) = abi {
        clang_args.push(format!("-mabi={abi}"));
    }
    let start = case.0.join("start.o");
    success(
        Command::new("clang")
            .args(&clang_args)
            .args([
                "-O0",
                "-ffreestanding",
                "-fno-builtin",
                "-fno-stack-protector",
                "-c",
            ])
            .arg(root.join("tests/fixtures/linux_case_runtime/start.c"))
            .arg("-o")
            .arg(&start)
            .output()
            .unwrap(),
    );
    for name in [
        "pointer_float",
        "float_pressure",
        "mixed_gp_pressure",
        "aligned_i128",
        "triple_float",
        "hfa_stack",
        "aligned_pressure",
    ] {
        let fixture = root.join("tests/fixtures/aggregate_registers").join(name);
        let object = case.0.join(format!("{name}.c.o"));
        success(
            Command::new("clang")
                .args(&clang_args)
                .args([
                    "-O0",
                    "-ffreestanding",
                    "-fno-builtin",
                    "-fno-stack-protector",
                    "-c",
                ])
                .arg(fixture.with_extension("c"))
                .arg("-o")
                .arg(&object)
                .output()
                .unwrap(),
        );
        for opt in ["-O0", "-O2"] {
            let mut compiler = case.command();
            compiler
                .arg("build")
                .arg(fixture.with_extension("wave"))
                .args(["--target", &target, "--emit=obj", opt]);
            if let Some(abi) = abi {
                compiler.arg(format!("--abi={abi}"));
            }
            success(compiler.output().unwrap());
            let binary = case.0.join("run");
            success(
                Command::new("clang")
                    .args(&clang_args)
                    .args(["-fuse-ld=lld", "-nostdlib", "-static", "-Wl,-e,_start"])
                    .arg(&start)
                    .arg(&object)
                    .arg(case.0.join(format!("{name}.o")))
                    .arg("-o")
                    .arg(&binary)
                    .output()
                    .unwrap(),
            );
            if std::env::consts::ARCH == arch && std::env::consts::OS == "linux" {
                success(Command::new(&binary).output().unwrap());
            } else {
                success(
                    Command::new(format!("qemu-{arch}"))
                        .arg(&binary)
                        .output()
                        .unwrap(),
                );
            }
        }
    }
}

#[cfg(feature = "llvm-target-riscv")]
#[test]
fn riscv64_aggregate_registers_match_clang() {
    if std::env::var_os("WAVE_RUN_RISCV64_INTEROP_TESTS").is_none() {
        return;
    }
    for abi in ["lp64", "lp64f", "lp64d"] {
        aggregate_interop("riscv64", Some(abi));
    }
}

#[cfg(feature = "llvm-target-aarch64")]
#[test]
fn aarch64_aggregate_registers_match_clang() {
    if std::env::var_os("WAVE_RUN_AARCH64_INTEROP_TESTS").is_none() {
        return;
    }
    aggregate_interop("aarch64", None);
}

#[cfg(feature = "llvm-target-loongarch")]
#[test]
fn loongarch_fp_features_match_emitted_instructions() {
    let case = Case::new();
    case.write(
        "float.wave",
        "export(c) fun add_double(a: f64, b: f64) -> f64 { return a + b; }",
    );
    for abi in ["lp64s", "lp64f", "lp64d"] {
        success(
            case.command()
                .args([
                    "build",
                    "float.wave",
                    "--target=loongarch64-unknown-linux-gnu",
                    "--emit=asm,ir",
                    "--abi",
                    abi,
                ])
                .output()
                .unwrap(),
        );
        let asm = fs::read_to_string(case.0.join("float.s")).unwrap();
        let ir = fs::read_to_string(case.0.join("float.ll")).unwrap();
        if abi == "lp64d" {
            assert!(asm.contains("fadd.d"), "{asm}");
            assert!(ir.contains("+lsx"));
        } else {
            assert!(!asm.contains("fadd.d"), "{abi}: {asm}");
            assert!(asm.contains("__adddf3"), "{abi}: {asm}");
            assert!(ir.contains("-d,-lsx"), "{ir}");
            rejected(
                case.command()
                    .args([
                        "build",
                        "float.wave",
                        "--target=loongarch64-unknown-linux-gnu",
                        "--emit=asm",
                        "--abi",
                        abi,
                        "--features=+lsx",
                    ])
                    .output()
                    .unwrap(),
                "requires feature 'd'",
            );
        }
    }
}

#[cfg(feature = "llvm-target-wasm")]
#[test]
fn webassembly_runtime_helpers_are_private_unique_and_only_generated_when_used() {
    let case = Case::new();
    case.write(
        "plain.wave",
        "export(c) fun add(a: u64, b: u64) -> u64 { return a + b; }",
    );
    success(
        case.command()
            .args([
                "build",
                "plain.wave",
                "--target=wasm64-unknown-unknown",
                "--emit=ir",
            ])
            .output()
            .unwrap(),
    );
    assert!(!fs::read_to_string(case.0.join("plain.ll"))
        .unwrap()
        .contains("__wave.runtime."));
    // An application symbol with the same spelling remains an application import.
    case.write("wide.wave", "extern(c, \"__wave.runtime.udiv.i128.i128\") fun host_value() -> u64; export(c) fun divide(a: u128, b: u128) -> u128 { return a / b + a / b; } export(c) fun host() -> u64 { return host_value(); }");
    success(
        case.command()
            .args([
                "build",
                "wide.wave",
                "--target=wasm64-unknown-unknown",
                "--emit=ir",
                "-O0",
            ])
            .output()
            .unwrap(),
    );
    let ir = fs::read_to_string(case.0.join("wide.ll")).unwrap();
    assert_eq!(
        ir.matches("define private i128 @__wave.runtime.udiv.i128.i128.")
            .count(),
        1,
        "{ir}"
    );
    assert!(
        ir.contains("declare i64 @__wave.runtime.udiv.i128.i128()"),
        "{ir}"
    );
    assert!(ir.contains("\"wasm-import-module\"=\"env\""));
    assert!(!ir.contains("__wave.runtime.sdiv"));
}
