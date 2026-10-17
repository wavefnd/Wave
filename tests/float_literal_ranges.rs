// SPDX-License-Identifier: MPL-2.0
use std::{
    fs,
    path::PathBuf,
    process::{Command, Output},
};
use utils::wson;

struct Case(PathBuf);

impl Case {
    fn new(name: &str) -> Self {
        let root = std::env::temp_dir()
            .join(format!("wave-float-literal-range-{name}-{}", std::process::id()));
        fs::create_dir_all(&root).unwrap();
        Self(root)
    }

    fn run(&self, args: &[&str]) -> Output {
        Command::new(env!("CARGO_BIN_EXE_wavec")).current_dir(&self.0).args(args).output().unwrap()
    }
}

impl Drop for Case {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

#[test]
fn out_of_range_default_float_literals_report_the_token_and_preserve_output() {
    let case = Case::new("invalid");
    for statement in [
        "println(\"{}\", 1e39);",
        "println(\"{}\", -1e39);",
        "1e39 + 0.0;",
        "true || (1e39 as bool);",
    ] {
        let source = format!("fun main() {{\n    {statement}\n}}\n");
        fs::write(case.0.join("main.wave"), &source).unwrap();
        for optimization in ["-O0", "-O2"] {
            for command in ["check", "build"] {
                fs::write(case.0.join("main.ll"), "previous verified IR").unwrap();
                let mut args = vec![optimization, command, "main.wave", "--error-format=json"];
                if command == "build" {
                    args.push("--emit=ir");
                }
                let result = case.run(&args);
                assert_eq!(result.status.code(), Some(1), "{source}");
                let stderr = String::from_utf8_lossy(&result.stderr);
                let diagnostic = wson::parse_json(stderr.trim()).unwrap();
                let error = diagnostic.get("error").unwrap();
                let span = error.get("span").unwrap();
                let token = if statement.contains("-1e39") { "-1e39" } else { "1e39" };
                let start = source.find(token).unwrap();
                assert_eq!(span.get_num("start"), Some(start as f64), "{stderr}");
                assert_eq!(span.get_num("end"), Some((start + token.len()) as f64), "{stderr}");
                assert_eq!(span.get_num("line"), Some(2.0), "{stderr}");
                assert!(stderr.contains("floating-point literal is out of range"), "{stderr}");
                assert!(stderr.contains("main.wave"), "{stderr}");
                assert!(!stderr.contains("internal compiler error"), "{stderr}");
                assert_eq!(
                    fs::read_to_string(case.0.join("main.ll")).unwrap(),
                    "previous verified IR"
                );
                assert!(!case.0.join("main.wir").exists());
            }
        }
    }
}

#[test]
fn f64_context_and_finite_f32_literals_still_build_with_llvm_by_default() {
    if !cfg!(feature = "llvm-target-x86") {
        return;
    }
    let case = Case::new("valid");
    fs::write(
        case.0.join("main.wave"),
        r#"
fun wide(value: f64 = 1e39) -> f64 { return value; }
fun arithmetic() -> f64 { return 1e39 + 0.0; }
fun main() -> i32 {
    var maximum: f32 = 3.4028234663852886e38;
    var rounded: f32 = 0.1;
    var tiny: f32 = 1e-50;
    var zero: f32 = -0.0;
    var contextual: f64 = 1e39;
    var explicit: f64 = 1e39 as f64;
    var narrowed: f32 = (1e39 as f64) as f32;
    wide(1e39);
    wide();
    return 0;
}
"#,
    )
    .unwrap();
    for optimization in ["-O0", "-O2"] {
        let result = case.run(&[
            optimization,
            "--target=x86_64-unknown-linux-gnu",
            "build",
            "main.wave",
            "--emit=ir",
        ]);
        assert!(result.status.success(), "{}", String::from_utf8_lossy(&result.stderr));
        let ir = fs::read_to_string(case.0.join("main.ll")).unwrap();
        assert!(ir.contains("target triple"), "{ir}");
        assert!(!case.0.join("main.wir").exists());
    }
}
