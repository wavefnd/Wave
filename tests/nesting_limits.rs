// SPDX-License-Identifier: MPL-2.0
//! Subprocess coverage includes recursive drop, which cannot be caught as a panic.
use std::{fs, path::PathBuf, process::Command};
use utils::wson;

struct Case(PathBuf);
impl Case {
    fn new(name: &str) -> Self {
        let root = std::env::temp_dir().join(format!("wave-nesting-{name}-{}", std::process::id()));
        fs::create_dir_all(&root).unwrap();
        Self(root)
    }

    fn verify(&self, accepted: bool, message: &str) {
        let targets = Command::new(env!("CARGO_BIN_EXE_wavec"))
            .args(["print", "target-list"])
            .output()
            .unwrap();
        let target = String::from_utf8(targets.stdout).unwrap().lines().next().unwrap().to_string();
        for mode in ["check", "ir", "ast"] {
            let mut command = if cfg!(target_os = "linux") {
                let mut command = Command::new("python3");
                command.args(["-c", "import os,resource,sys; resource.setrlimit(resource.RLIMIT_STACK,(1048576,1048576)); os.execv(sys.argv[1],sys.argv[1:])", env!("CARGO_BIN_EXE_wavec")]);
                command
            } else {
                Command::new(env!("CARGO_BIN_EXE_wavec"))
            };
            command.current_dir(&self.0).args([
                "--error-format=json",
                if mode == "check" { "check" } else { "build" },
                "main.wave",
                "--target",
                &target,
            ]);
            if mode != "check" {
                command.arg(format!("--emit={mode}")).arg("--out-dir").arg(self.0.join(mode));
            }
            let result = command.output().unwrap();
            assert_eq!(
                result.status.code(),
                Some(if accepted { 0 } else { 1 }),
                "{mode}: {result:?}"
            );
            if !accepted {
                let text = String::from_utf8(result.stderr).unwrap();
                let record = wson::parse_json(text.trim()).unwrap();
                let error = record.get("error").unwrap();
                assert!(error.get_str("message").unwrap().contains(message), "{text}");
                assert!(error.get("span").is_some());
            }
        }
    }
}
impl Drop for Case {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

#[test]
fn block_and_type_maximum_is_accepted_and_one_more_is_located() {
    for kind in ["block", "type"] {
        let case = Case::new(kind);
        for depth in [128, 129] {
            let source = if kind == "block" {
                format!(
                    "fun main() {{ {}{} }}",
                    "if (true) {".repeat(depth - 1),
                    "}".repeat(depth - 1)
                )
            } else {
                format!(
                    "fun main() {{ var p: {}i32{}; }}",
                    "ptr<".repeat(depth - 1),
                    ">".repeat(depth - 1)
                )
            };
            fs::write(case.0.join("main.wave"), source).unwrap();
            case.verify(depth == 128, &format!("{kind} nesting exceeds"));
        }
        // A rejected parse must not poison subsequent budgets.
        fs::write(case.0.join("main.wave"), "fun main() {}").unwrap();
        case.verify(true, "");
    }
}

#[test]
fn import_depth_cycles_and_diamonds_have_distinct_outcomes() {
    let case = Case::new("imports");
    for index in 0..512 {
        let name = if index == 0 { "main".to_string() } else { format!("m{index}") };
        let import =
            if index < 511 { format!("import(\"./m{}\");", index + 1) } else { String::new() };
        fs::write(case.0.join(format!("{name}.wave")), format!("{import}fun {name}() {{}}"))
            .unwrap();
    }
    case.verify(true, "");
    fs::write(case.0.join("m511.wave"), "import(\"./m512\"); fun m511() {}").unwrap();
    fs::write(case.0.join("m512.wave"), "fun m512() {}").unwrap();
    case.verify(false, "import nesting exceeds");
    fs::write(case.0.join("main.wave"), "import(\"./m1\"); fun main() {}").unwrap();
    fs::write(case.0.join("m1.wave"), "import(\"./main\"); fun m1() {}").unwrap();
    let result = Command::new(env!("CARGO_BIN_EXE_wavec"))
        .current_dir(&case.0)
        .args(["--error-format=json", "check", "main.wave"])
        .output()
        .unwrap();
    assert_eq!(result.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&result.stderr).contains("import cycle"));
    fs::write(case.0.join("main.wave"), "import(\"./left\"); import(\"./right\"); fun main() {}")
        .unwrap();
    for name in ["left", "right"] {
        fs::write(
            case.0.join(format!("{name}.wave")),
            format!("import(\"./shared\"); fun {name}() {{}} "),
        )
        .unwrap();
    }
    fs::write(case.0.join("shared.wave"), "fun shared() {}").unwrap();
    case.verify(true, "");
}
