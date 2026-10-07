// SPDX-License-Identifier: MPL-2.0
use std::{
    fs,
    path::PathBuf,
    process::{Command, Output},
};
use utils::wson;

struct Case {
    root: PathBuf,
}
impl Case {
    fn new(name: &str, source: &str) -> Self {
        let root = std::env::temp_dir().join(format!("wave-whale-{name}-{}", std::process::id()));
        fs::create_dir_all(&root).unwrap();
        fs::write(root.join("main.wave"), source).unwrap();
        Self { root }
    }

    fn run(&self, args: &[&str]) -> Output {
        Command::new(env!("CARGO_BIN_EXE_wavec"))
            .current_dir(&self.root)
            .args(args)
            .output()
            .unwrap()
    }

    fn ir(&self) -> String {
        let output = self.run(&["--whale", "build", "main.wave", "--emit=ir"]);
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        fs::read_to_string(self.root.join("main.wir")).unwrap()
    }
}
impl Drop for Case {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.root);
    }
}

#[test]
fn scalar_control_flow_and_calls_produce_verified_whale_ir() {
    let case = Case::new(
        "control",
        r#"
fun inc(n: i32) -> i32 { return n + 1; }
fun main() -> i32 {
    var count: i32 = 0;
    while (count < 10) {
        count = inc(count);
        if (count == 3) { continue; }
        if (count > 6) { break; }
        if (count == 5) {
            var count: i32 = count + 1;
            if (count != 6) { return 1; }
        }
    }
    if (count == 7 && (inc(count) == 8 || count == 0)) { return 0; }
    else if (count == 8) { return 2; }
    else { return 3; }
}
"#,
    );
    let ir = case.ir();
    assert!(ir.contains("format_version 3"), "{ir}");
    assert!(ir.contains("x86_64-whale-linux"), "{ir}");
    assert!(ir.contains("phi"), "{ir}");
    assert!(ir.contains("call"), "{ir}");
    assert!(ir.contains("while.test"), "{ir}");
    assert!(!case.root.join("main.ll").exists());
    let check = case.run(&["--whale", "check", "main.wave"]);
    assert!(check.status.success(), "{}", String::from_utf8_lossy(&check.stderr));
}

#[test]
fn ordered_casts_and_contextual_arithmetic_survive_lowering() {
    let case = Case::new(
        "casts",
        r#"
fun narrow(n: i32) -> i32 { return (n as u8) as i32; }
fun signedness(n: u32) -> i32 { return n as i32; }
fun bound(value: u64) -> bool { return value <= 4294967296 - 32; }
fun wide() -> u128 { return 340282366920938463463374607431768211455; }
fun main() -> i32 { return narrow(257); }
"#,
    );
    let ir = case.ir();
    let trunc = ir.find("trunc").expect(&ir);
    let extend = ir.find("zext").expect(&ir);
    assert!(trunc < extend, "{ir}");
    assert!(ir.contains("bitcast"), "{ir}");
    assert!(ir.contains("4294967296"), "{ir}");
    assert!(ir.contains("340282366920938463463374607431768211455"), "{ir}");
    assert!(ir.contains("ule"), "{ir}");
}

#[test]
fn floating_bool_uses_unordered_nonzero_and_preserves_negative_zero() {
    let case = Case::new(
        "float",
        r#"
fun truth(n: f64) -> bool { return n as bool; }
fun not_int(n: i32) -> bool { return !n; }
fun neg(n: f64) -> f64 { return -n; }
fun zero() -> f64 { return -0.0; }
fun mixed(n: i32, f: f64) -> bool { return n != f; }
fun main() -> i32 { if (truth(0.0)) { return 1; } return 0; }
"#,
    );
    let ir = case.ir();
    assert!(ir.contains("une"), "{ir}");
    assert!(ir.contains("0x8000000000000000"), "{ir}");
}

#[test]
fn unsupported_source_has_json_location_and_preserves_previous_output() {
    let case = Case::new("unsupported", "fun main() -> i32 {\n    return 8 / 2;\n}\n");
    let output = case.root.join("main.wir");
    fs::write(&output, "previous verified IR").unwrap();
    let result = case.run(&["--whale", "build", "main.wave", "--emit=ir", "--error-format=json"]);
    assert!(!result.status.success());
    let stderr = String::from_utf8_lossy(&result.stderr);
    let json = wson::parse_json(stderr.trim()).unwrap();
    assert!(stderr.contains("Whale backend does not yet support operation Divide"), "{json:?}");
    assert!(stderr.contains("main.wave"), "{json:?}");
    assert!(stderr.contains("E4001"), "{json:?}");
    assert_eq!(fs::read_to_string(output).unwrap(), "previous verified IR");
    let targets = case.run(&["print", "target-list"]);
    assert!(targets.status.success());
    let targets = String::from_utf8(targets.stdout).unwrap();
    let llvm = case.run(&["check", "main.wave", "--target", targets.lines().next().unwrap()]);
    assert!(llvm.status.success(), "{}", String::from_utf8_lossy(&llvm.stderr));
    let whale = case.run(&["--whale", "check", "main.wave"]);
    assert!(!whale.status.success());
}

#[test]
fn unsupported_requests_fail_without_creating_outputs() {
    let case = Case::new("options", "fun main() -> i32 { return 0; }");
    for args in [
        vec!["--whale", "build", "main.wave"],
        vec!["--whale", "-O2", "build", "main.wave", "--emit=ir"],
        vec!["--whale", "--target=aarch64-unknown-linux-gnu", "check", "main.wave"],
        vec!["--whale", "build", "main.wave", "--emit=ir", "--run"],
        vec!["--whale", "build", "main.wave", "--emit=obj"],
        vec!["--whale", "print", "supported-targets"],
    ] {
        let result = case.run(&args);
        assert!(!result.status.success(), "{args:?}");
        assert!(
            String::from_utf8_lossy(&result.stderr).contains("Whale"),
            "{args:?}: {}",
            String::from_utf8_lossy(&result.stderr)
        );
    }
    assert!(!case.root.join("main.wir").exists());
    assert!(!case.root.join("main.ll").exists());
    let dry = case.run(&["--whale", "build", "main.wave", "--emit=ir", "--dry-run"]);
    assert!(dry.status.success(), "{}", String::from_utf8_lossy(&dry.stderr));
    assert!(!case.root.join("main.wir").exists());
}

#[test]
fn whale_output_cannot_overwrite_source_or_imported_input() {
    let source = "fun main() -> i32 { return 0; }";
    let case = Case::new("output", source);
    let result = case.run(&["--whale", "build", "main.wave", "--emit=ir", "-o", "main.wave"]);
    assert!(!result.status.success());
    assert_eq!(fs::read_to_string(case.root.join("main.wave")).unwrap(), source);
    fs::write(case.root.join("helper.wave"), "pub fun helper() -> i32 { return 1; }").unwrap();
    fs::write(
        case.root.join("main.wave"),
        "import(\"./helper.wave\")::{helper}; fun main() -> i32 { return helper(); }",
    )
    .unwrap();
    let original = fs::read_to_string(case.root.join("helper.wave")).unwrap();
    let result = case.run(&["--whale", "build", "main.wave", "--emit=ir", "-o", "helper.wave"]);
    assert!(!result.status.success());
    assert!(
        String::from_utf8_lossy(&result.stderr).contains("aliases compiler input"),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    assert_eq!(fs::read_to_string(case.root.join("helper.wave")).unwrap(), original);
}

#[test]
fn llvm_stays_default_and_whale_help_is_available() {
    let case = Case::new("default", "fun main() -> i32 { return 0; }");
    if cfg!(feature = "llvm-target-x86") {
        let output =
            case.run(&["--target=x86_64-unknown-linux-gnu", "build", "main.wave", "--emit=ir"]);
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        assert!(fs::read_to_string(case.root.join("main.ll")).unwrap().contains("target triple"));
        assert!(!case.root.join("main.wir").exists());
    }
    let help = case.run(&["--whale", "--help"]);
    assert!(help.status.success());
    assert!(String::from_utf8_lossy(&help.stdout).contains("LLVM is the default"));
    let version = case.run(&["--version"]);
    assert!(version.status.success());
    assert!(String::from_utf8_lossy(&version.stdout).contains(env!("CARGO_PKG_VERSION")));
    let version = case.run(&["--whale", "--version"]);
    assert!(version.status.success());
    assert!(String::from_utf8_lossy(&version.stdout).contains("backend: Whale IR"));
}

#[test]
fn whale_expression_depth_keeps_the_frontend_boundary() {
    let case = Case::new("depth", "");
    for kind in ["binary", "cast", "call"] {
        for depth in [128, 129] {
            let expression = match kind {
                "binary" => format!("1{}", " + 1".repeat(depth)),
                "cast" => format!("1{}", " as i32".repeat(depth)),
                _ => format!("{}1{}", "id(".repeat(depth), ")".repeat(depth)),
            };
            fs::write(case.root.join("main.wave"), format!(
                "fun id(n: i32) -> i32 {{ return n; }}\nfun main() -> i32 {{ return {expression}; }}"
            )).unwrap();
            #[cfg(target_os = "linux")]
            let mut command = {
                let mut command = Command::new("sh");
                command
                    .args(["-c", "ulimit -s 1024 && exec \"$@\"", "whale-depth"])
                    .arg(env!("CARGO_BIN_EXE_wavec"));
                command
            };
            #[cfg(not(target_os = "linux"))]
            let mut command = Command::new(env!("CARGO_BIN_EXE_wavec"));
            let result = command
                .current_dir(&case.root)
                .args(["--whale", "check", "main.wave", "--error-format=json"])
                .output()
                .unwrap();
            if depth == 128 {
                assert!(
                    result.status.success(),
                    "{kind}/{depth}: {}",
                    String::from_utf8_lossy(&result.stderr)
                );
            } else {
                assert_eq!(result.status.code(), Some(1), "{kind}/{depth}");
                assert!(String::from_utf8_lossy(&result.stderr)
                    .contains("expression nesting exceeds the maximum of 128 levels"));
            }
        }
    }
}

#[test]
fn checked_shifts_and_float_casts_emit_ir_for_literal_and_runtime_operands() {
    let case = Case::new(
        "checked-numeric",
        r#"
fun left(x: u8, n: u128) -> u8 { return x << n; }
fun right(x: i64, n: i8) -> i64 { return x >> n; }
fun literal(x: u128) -> u128 { return x << (127); }
fun to_signed(f: f64) -> i8 { return f as i8; }
fun to_unsigned(f: f32) -> u128 { return f as u128; }
fun main() -> i32 {
    var a: i8 = -128.9 as i8;
    var b: u8 = -0.9 as u8;
    if (a == -128 && b == 0) { return 0; }
    return 1;
}
"#,
    );
    let ir = case.ir();
    assert!(ir.contains("shift count out of range"), "{ir}");
    assert!(ir.contains("float-to-integer conversion out of range"), "{ir}");
    let check = case.run(&["--whale", "check", "main.wave"]);
    assert!(check.status.success(), "{}", String::from_utf8_lossy(&check.stderr));
}

#[test]
fn invalid_numeric_constants_keep_frontend_diagnostics_and_previous_output() {
    let case = Case::new("invalid-numeric", "");
    let output = case.root.join("main.wir");
    for source in [
        "fun main() -> i32 { return 1 << -1; }",
        "fun main() -> i32 { return 1 << 32; }",
        "fun main() -> i32 { return 1 << 4294967296; }",
        "fun main() -> i32 { return 1 << 340282366920938463463374607431768211456; }",
        "fun main() -> i32 { var x: u8 = 256.0 as u8; return 0; }",
        "fun main() -> i32 { var x: i8 = -129.0 as i8; return 0; }",
        "fun main() -> i32 { var x: u8 = -1.0 as u8; return 0; }",
        "fun main() -> i32 { return 1 << true; }",
        "fun main() -> i32 { return 1 << 1.0; }",
    ] {
        fs::write(case.root.join("main.wave"), source).unwrap();
        fs::write(&output, "previous verified IR").unwrap();
        for command in ["check", "build"] {
            let mut args = vec!["--whale", command, "main.wave", "--error-format=json"];
            if command == "build" {
                args.push("--emit=ir");
            }
            let result = case.run(&args);
            assert_eq!(result.status.code(), Some(1), "{source}");
            let stderr = String::from_utf8_lossy(&result.stderr);
            wson::parse_json(stderr.trim()).unwrap();
            assert!(stderr.contains("main.wave"), "{source}: {stderr}");
            assert!(!stderr.contains("internal compiler error"), "{source}: {stderr}");
            assert!(!stderr.contains("does not yet support"), "{source}: {stderr}");
            assert_eq!(fs::read_to_string(&output).unwrap(), "previous verified IR");
        }
    }
}

#[test]
fn short_circuited_invalid_numeric_operations_stay_in_the_rhs_block() {
    let case = Case::new(
        "numeric-short-circuit",
        r#"
fun main() -> i32 {
    if (true || (1 << 340282366920938463463374607431768211456) == 0) {
        if (false && (256.0 as u8) == 0) { return 1; }
        if (true || (1 << -1) == 0) { return 0; }
    }
    return 2;
}
"#,
    );
    let ir = case.ir();
    let branch = ir.find("cbr bool").expect(&ir);
    let trap = ir.find("trap_if").expect(&ir);
    assert!(branch < trap, "a skipped RHS must not trap in the entry block: {ir}");
}
