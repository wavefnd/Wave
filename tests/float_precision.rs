// SPDX-License-Identifier: MPL-2.0
use std::fs;
use std::path::PathBuf;
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};
use utils::wson;

static NEXT: AtomicUsize = AtomicUsize::new(0);

struct Case(PathBuf);

impl Case {
    fn new(source: &str) -> Self {
        let root = std::env::temp_dir().join(format!(
            "wave-float-precision-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir_all(&root).unwrap();
        fs::write(root.join("main.wave"), source).unwrap();
        Self(root)
    }

    fn run(&self, args: &[&str]) -> Output {
        Command::new(env!("CARGO_BIN_EXE_wavec")).current_dir(&self.0).args(args).output().unwrap()
    }

    fn frontend_target(&self) -> String {
        let output = self.run(&["print", "target-list"]);
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        String::from_utf8(output.stdout)
            .unwrap()
            .lines()
            .next()
            .expect("at least one target must be enabled")
            .to_owned()
    }
}

impl Drop for Case {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

#[cfg(any(
    feature = "llvm-target-wasm",
    all(target_os = "linux", target_arch = "x86_64", feature = "llvm-target-x86")
))]
const OUTPUT_SOURCE: &str = r#"
static calls: i32 = 0;
fun next() -> f64 { calls += 1; return (calls as f64) + 0.125; }
fun main() -> i32 {
    var single: f32 = 1.125;
    var double: f64 = 1.375;
    print("100% %n {:.2} {:.2} {:.0} {:.0}\n", single, double, 2.5, 3.5);
    println("{:.0} {:.2} {:.9} {}", -0.0 as f64, -0.0 as f32, 1.5 as f64, 0.0078125 as f64);
    println("{:.2} {:.2} {:.2} {:.2}", -1.125 as f64, -1.375 as f64, 1.005 as f64, 2.675 as f64);
    println("{:.2} {:.2}", 1e21 as f64, -5e-324 as f64);
    println("{:.2} {:.0002}", next(), next());
    println("{d} {x} {c} {s}", 255, 255, 'A', "kept");
    if (calls != 2) { return 1; }
    return 0;
}
"#;

#[cfg(any(
    feature = "llvm-target-wasm",
    all(target_os = "linux", target_arch = "x86_64", feature = "llvm-target-x86")
))]
const EXPECTED_OUTPUT: &str = "100% %n 1.12 1.38 2 4\n\
-0 -0.00 1.500000000 0.007812\n\
-1.12 -1.38 1.00 2.67\n\
1000000000000000000000.00 -0.00\n\
1.12 2.12\n\
255 ff A kept\n";

#[test]
#[cfg(all(target_os = "linux", target_arch = "x86_64", feature = "llvm-target-x86"))]
fn fixed_float_precision_matches_native_printf_at_o0_and_o2() {
    let case = Case::new(OUTPUT_SOURCE);
    for optimization in ["-O0", "-O2"] {
        let output = case.run(&["build", "main.wave", optimization, "--run"]);
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        assert_eq!(output.stdout, EXPECTED_OUTPUT.as_bytes(), "{optimization}");
    }
}

#[test]
#[cfg(feature = "llvm-target-wasm")]
fn fixed_float_precision_matches_wasm_hosts_at_o0_and_o2() {
    for tool in ["node", "wasm-ld"] {
        if !Command::new(tool).arg("--version").output().is_ok_and(|output| output.status.success())
        {
            eprintln!("skipping WebAssembly precision execution: {tool} unavailable");
            return;
        }
    }
    let case = Case::new(OUTPUT_SOURCE);
    for target in ["wasm32-unknown-unknown", "wasm32-wasip1", "wasm64-unknown-unknown"] {
        for optimization in ["-O0", "-O2"] {
            let output = case.run(&[
                "build",
                "main.wave",
                optimization,
                "--target",
                target,
                "--run",
                "--out-dir",
                target,
            ]);
            assert!(
                output.status.success(),
                "{target} {optimization}: {}",
                String::from_utf8_lossy(&output.stderr)
            );
            assert_eq!(output.stdout, EXPECTED_OUTPUT.as_bytes(), "{target} {optimization}");
        }
    }
}

#[test]
fn invalid_precision_formats_report_source_errors_and_preserve_existing_output() {
    let case = Case::new("");
    let target = case.frontend_target();
    for statement in [
        r#"println("{:.}", 1.0);"#,
        r#"println("{:.-1}", 1.0);"#,
        r#"println("{:.+1}", 1.0);"#,
        r#"println("{:.1e2}", 1.0);"#,
        r#"println("{:.1 2}", 1.0);"#,
        r#"println("{:.2f}", 1.0);"#,
        r#"println("{:.٢}", 1.0);"#,
        r#"println("{:.2147483648}", 1.0);"#,
        r#"println("{:.999999999999999999999999999999999999999999}", 1.0);"#,
        r#"println("{:8.2}", 1.0);"#,
        r#"println("{:.2}", 1);"#,
        r#"println("{:.2}", true);"#,
        r#"println("{:.2}", "text");"#,
        r#"var value: f64 = 0.0; input("{:.2}", value);"#,
        r#"println("{:.2}");"#,
        r#"println("{:.2}", 1.0, 2.0);"#,
    ] {
        fs::write(case.0.join("main.wave"), format!("fun main() {{\n    {statement}\n}}\n"))
            .unwrap();
        for diagnostic in ["human", "json"] {
            fs::write(case.0.join("main.ll"), "previous verified IR").unwrap();
            let output = case.run(&[
                "build",
                "main.wave",
                "--emit=ir",
                "--target",
                &target,
                "--error-format",
                diagnostic,
            ]);
            assert_eq!(output.status.code(), Some(1), "{statement}");
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains("main.wave"), "{statement}: {stderr}");
            assert!(!stderr.contains("internal compiler error"), "{statement}: {stderr}");
            assert!(!stderr.contains("panicked"), "{statement}: {stderr}");
            if diagnostic == "json" {
                let json = wson::parse_json(stderr.trim()).unwrap();
                let span = json.get("error").unwrap().get("span").unwrap();
                assert_eq!(span.get_num("line"), Some(2.0), "{statement}: {stderr}");
            }
            assert_eq!(fs::read_to_string(case.0.join("main.ll")).unwrap(), "previous verified IR");
        }
    }
}

#[test]
fn precision_limit_is_checked_without_generating_or_allocating_output() {
    let case = Case::new(r#"fun main() { println("{:.2147483647} {:.0002}", 1.0, 1.0); }"#);
    let output = case.run(&["check", "main.wave", "--target", &case.frontend_target()]);
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
}
