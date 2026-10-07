// SPDX-License-Identifier: MPL-2.0
use std::{fs, process::Command};
use utils::wson::{self, Value};

#[test]
fn source_ast_formats_preserve_literals_without_resolving_imports() {
    let root = std::env::temp_dir().join(format!("wave ast outputs {}", std::process::id()));
    fs::create_dir_all(&root).unwrap();
    let source = root.join("sample.wave");
    fs::write(
        &source,
        r#"
import("unavailable::module")::{missing};
fun main() -> i32 {
    var big: u1024 = 340282366920938463463374607431768211456;
    var fractional: f64 = 1.2500;
    var text: str = "한글\xFF\n\"";
    return missing(big);
}
"#,
    )
    .unwrap();
    let mut representations = Vec::new();
    for (format, extension) in [("wson", "ast.wson"), ("json", "ast.json"), ("sexpr", "ast")] {
        let output = Command::new(env!("CARGO_BIN_EXE_wavec"))
            .args(["build", "--emit=ast", "--ast-format", format, "--out-dir"])
            .arg(&root)
            .arg(&source)
            .output()
            .unwrap();
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        let text = fs::read_to_string(root.join(format!("sample.{extension}"))).unwrap();
        if format == "sexpr" {
            assert!(text.starts_with("(ast\n  (schema_version 1)"));
            assert!(text.contains("(function"));
        } else {
            let value = if format == "json" { wson::parse_json(&text) } else { wson::loads(&text) }
                .unwrap();
            assert_eq!(value.get_u64("schema_version"), Some(1));
            assert_eq!(value.get_str("stage"), Some("parsed"));
            assert!(text.contains("340282366920938463463374607431768211456"));
            assert!(text.contains("1.2500"));
            assert!(text.contains("255"));
            assert!(text.contains("unavailable::module"));
            assert!(matches!(value.get("nodes"), Some(Value::Array(_))));
            representations.push(value);
        }
    }
    assert_eq!(representations[0], representations[1]);
    let bad = Command::new(env!("CARGO_BIN_EXE_wavec"))
        .args(["build", "--emit=ast", "--ast-format=bogus"])
        .arg(&source)
        .output()
        .unwrap();
    assert!(!bad.status.success());
    fs::remove_dir_all(root).unwrap();
}

#[test]
fn parsed_ast_schema_snapshots_are_deterministic() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let output = std::env::temp_dir().join(format!("wave ast snapshots {}", std::process::id()));
    fs::create_dir_all(&output).unwrap();
    for (format, extension) in
        [(None, "ast.wson"), (Some("json"), "ast.json"), (Some("sexpr"), "ast")]
    {
        let mut command = Command::new(env!("CARGO_BIN_EXE_wavec"));
        command
            .current_dir(root)
            .args(["build", "tests/fixtures/ast/parsed.wave", "--emit=ast", "--out-dir"])
            .arg(&output);
        if let Some(format) = format {
            command.args(["--ast-format", format]);
        }
        let result = command.output().unwrap();
        assert!(result.status.success(), "{}", String::from_utf8_lossy(&result.stderr));
        let actual = fs::read_to_string(output.join(format!("parsed.{extension}"))).unwrap();
        let expected =
            fs::read_to_string(root.join(format!("tests/fixtures/ast/parsed.{extension}")))
                .unwrap();
        assert_eq!(actual, expected, "AST schema changed for {extension}");
        // Repeating the same command must not acquire IDs, expanded imports or timestamps.
        assert!(command.status().unwrap().success());
        assert_eq!(
            fs::read_to_string(output.join(format!("parsed.{extension}"))).unwrap(),
            expected
        );
    }
    fs::remove_dir_all(output).unwrap();
}
