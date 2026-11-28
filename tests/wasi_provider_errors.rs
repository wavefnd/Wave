// SPDX-License-Identifier: MPL-2.0
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};

static NEXT_CASE: AtomicU64 = AtomicU64::new(0);

struct Case(PathBuf);

impl Case {
    fn new() -> Self {
        let directory = std::env::temp_dir().join(format!(
            "wave-wasi-provider-errors-{}-{}",
            std::process::id(),
            NEXT_CASE.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir_all(&directory).unwrap();
        Self(directory)
    }

    fn compiler(&self) -> Command {
        let mut command = Command::new(env!("CARGO_BIN_EXE_wavec"));
        command.current_dir(&self.0).arg("--std-root").arg(root().join("std"));
        command
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

fn checked(command: &mut Command) {
    let output = command.output().unwrap();
    assert!(
        output.status.success(),
        "{command:?}: {}\n{}\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn available(tool: &str) -> bool {
    Command::new(tool).arg("--version").output().is_ok_and(|output| output.status.success())
}

#[test]
fn wasi_case_runner_delivers_stdin_and_compares_the_program_exit() {
    if llvm::codegen::target::target_spec_for_triple("wasm32-wasip1").is_none()
        || !available("node")
        || !available("wasm-ld")
    {
        return;
    }
    let case = Case::new();
    let source = case.0.join("input.wave");
    let report_path = case.0.join("report.json");
    for (stdin, actual) in [("A", 7), ("B", 2)] {
        fs::write(
            &source,
            format!(
                r#"// wave-test: expected-exit=7, stdin={stdin}
import("std::sys::fs")::{{read}};
fun main() -> i32 {{
    var b: u8 = 0;
    if (read(0, &b, 1) != 1 || b != 65) {{ return 2; }}
    return 7;
}}
"#
            ),
        )
        .unwrap();
        let result = wave_tools::build::test_command()
            .unwrap()
            .args([
                "test",
                "--wavec",
                env!("CARGO_BIN_EXE_wavec"),
                "--target-id",
                "wasm-wasi",
                "--source",
            ])
            .arg(&source)
            .arg("--report-json")
            .arg(&report_path)
            .output()
            .unwrap();
        assert_eq!(
            result.status.success(),
            actual == 7,
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
        let report = wave_tools::config::read_json(&report_path).unwrap();
        let row = &report.get_arr("tests").unwrap()[0];
        assert_eq!(row.get_str("phase"), Some("run"));
        assert_eq!(row.get_u64("actual_exit"), Some(actual));
        assert_eq!(row.get_u64("expected_exit"), Some(7));
    }
}

#[test]
fn wasi_provider_failures_keep_the_wasi_errno_domain() {
    let target = "wasm32-wasip1";
    if llvm::codegen::target::target_spec_for_triple(target).is_none() {
        eprintln!("WASI errno execution skipped: WebAssembly target is disabled");
        return;
    }
    if !available("node") || !available("wasm-ld") {
        eprintln!("WASI errno execution skipped: Node and wasm-ld are required");
        return;
    }
    let case = Case::new();
    let fixtures = root().join("tests/fixtures/wasi_provider_errors");
    for optimization in ["-O0", "-O2"] {
        let directory = case.0.join(optimization);
        checked(
            case.compiler()
                .arg("build")
                .arg(fixtures.join("provider.wave"))
                .args(["--target", target, optimization, "--emit=obj", "--out-dir"])
                .arg(&directory),
        );
        let module = directory.join("errors.wasm");
        let mut link = Command::new("wasm-ld");
        link.args(["--no-entry", "--export-memory"]);
        for function in [
            "invalid_arguments",
            "unsupported_calls",
            "file_stat",
            "file_seek",
            "clock_value",
            "environment_value",
            "read_value",
            "write_value",
        ] {
            link.arg(format!("--export={function}"));
        }
        checked(link.arg(directory.join("provider.o")).arg("-o").arg(&module));
        checked(Command::new("node").arg(fixtures.join("host.cjs")).arg(module));
    }
}

#[test]
fn wasm_allocator_errors_follow_the_target_without_losing_allocations() {
    if !available("node") {
        eprintln!("WebAssembly allocator execution skipped: Node is required");
        return;
    }
    let case = Case::new();
    let fixture = root().join("tests/fixtures/wasi_provider_errors/allocator.wave");
    for target in ["wasm32-wasip1", "wasm32-unknown-unknown", "wasm64-unknown-unknown"] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        for optimization in ["-O0", "-O2"] {
            checked(
                case.compiler()
                    .arg("build")
                    .arg(&fixture)
                    .args(["--target", target, optimization, "--run", "--out-dir"])
                    .arg(case.0.join(format!("{target}-{optimization}"))),
            );
        }
    }
}

#[test]
fn wasi_environment_growth_preserves_capacity_and_entry_limits() {
    if llvm::codegen::target::target_spec_for_triple("wasm32-wasip1").is_none()
        || !available("node")
    {
        return;
    }
    let case = Case::new();
    let module = case.0.join("env.wasm");
    for opt in ["-O0", "-O2"] {
        checked(
            case.compiler()
                .arg("build")
                .arg(root().join("tests/fixtures/wasi_provider_errors/environment.wave"))
                .args(["--target", "wasm32-wasip1", opt, "-o"])
                .arg(&module),
        );
        for key in ["AAA", "ZZZ"] {
            for length in [32753, 32754, 32755, 40000] {
                checked(
                    Command::new("node")
                        .arg(root().join("tests/fixtures/wasi_provider_errors/environment.mjs"))
                        .arg(&module)
                        .args(["growth", key])
                        .arg(length.to_string()),
                );
            }
        }
        // The provider's independent entry-count bound must terminate promptly,
        // rather than repeatedly allocating larger byte buffers.
        fs::write(case.0.join("entry-limit.wave"), "import(\"std::env::environ\")::{env_get}; fun main() -> i32 { var value: array<u8, 8>; if (env_get(\"PROBE\", &value[0], 8) != -5) { return 1; } return 0; }").unwrap();
        checked(
            case.compiler()
                .arg("build")
                .arg(case.0.join("entry-limit.wave"))
                .args(["--target", "wasm32-wasip1", opt, "-o"])
                .arg(&module),
        );
        checked(
            Command::new("node")
                .arg(root().join("tests/fixtures/wasi_provider_errors/environment.mjs"))
                .arg(&module)
                .arg("entry-limit"),
        );
    }
}

#[cfg(unix)]
#[test]
fn wasi_metadata_follows_links_without_escaping_the_preopen() {
    if llvm::codegen::target::target_spec_for_triple("wasm32-wasip1").is_none()
        || !available("node")
    {
        return;
    }
    use std::os::unix::fs::symlink;
    let case = Case::new();
    let directory = case.0.join("preopen");
    fs::create_dir_all(directory.join("directory")).unwrap();
    fs::write(directory.join("target.txt"), "hello").unwrap();
    fs::write(case.0.join("outside.txt"), "outside").unwrap();
    for (target, name) in [
        ("target.txt", "link.txt"),
        ("link.txt", "chain.txt"),
        ("absent.txt", "dangling.txt"),
        ("../outside.txt", "escape.txt"),
    ] {
        symlink(target, directory.join(name)).unwrap();
    }
    let module = case.0.join("links.wasm");
    for opt in ["-O0", "-O2"] {
        checked(
            case.compiler()
                .arg("build")
                .arg(root().join("tests/fixtures/wasi_provider_errors/symlinks.wave"))
                .args(["--target", "wasm32-wasip1", opt, "-o"])
                .arg(&module),
        );
        checked(
            Command::new("node")
                .arg(root().join("tools/run_wasi_smoke.mjs"))
                .arg(&module)
                .arg(&directory),
        );
    }
}
