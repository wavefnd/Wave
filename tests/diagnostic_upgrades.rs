// SPDX-License-Identifier: MPL-2.0
use std::{
    fs,
    path::PathBuf,
    process::{Command, Output},
    sync::atomic::{AtomicU64, Ordering},
};
use utils::wson::{self, Value};

static NEXT_CASE: AtomicU64 = AtomicU64::new(0);

struct Case {
    root: PathBuf,
    target: String,
}

impl Case {
    fn new() -> Self {
        let root = std::env::temp_dir().join(format!(
            "wave-diagnostic-upgrades-{}-{}",
            std::process::id(),
            NEXT_CASE.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir_all(&root).unwrap();
        let mut case = Self { root, target: String::new() };
        let targets = case.run(&["print", "target-list"]);
        assert!(targets.status.success());
        case.target = String::from_utf8(targets.stdout).unwrap().lines().next().unwrap().into();
        case
    }

    fn run(&self, args: &[&str]) -> Output {
        Command::new(env!("CARGO_BIN_EXE_wavec"))
            .current_dir(&self.root)
            .env("NO_COLOR", "1")
            .args(args)
            .output()
            .unwrap()
    }

    fn error(&self, source: &str, offending: &str, message: &str) -> Value {
        self.error_at(source, source.find(offending).unwrap(), message)
    }

    fn error_at(&self, source: &str, start: usize, message: &str) -> Value {
        fs::write(self.root.join("main.wave"), source).unwrap();
        fs::write(self.root.join("main.ll"), "previous IR").unwrap();
        let prefix = &source[..start];
        let line = prefix.chars().filter(|&c| c == '\n').count() + 1;
        let column = prefix.rsplit('\n').next().unwrap().chars().count() + 1;
        let mut record = Value::Null;
        for command in ["check", "build"] {
            for format in ["json", "human"] {
                let mut args =
                    vec![command, "main.wave", "--target", &self.target, "--error-format", format];
                if command == "build" {
                    args.push("--emit=ir");
                }
                let output = self.run(&args);
                assert_eq!(output.status.code(), Some(1), "{args:?}: {output:?}");
                assert!(output.stdout.is_empty(), "{output:?}");
                let text = String::from_utf8(output.stderr).unwrap();
                assert!(text.contains(message), "{source}\n{text}");
                if format == "json" {
                    let parsed = wson::parse_json(text.trim()).unwrap();
                    let error = parsed.get("error").unwrap();
                    assert_eq!(error.get_u64("line"), Some(line as u64), "{text}");
                    assert_eq!(error.get_u64("column"), Some(column as u64), "{text}");
                    assert_eq!(
                        error.get("span").unwrap().get_u64("start"),
                        Some(start as u64),
                        "{text}"
                    );
                    record = parsed;
                } else {
                    assert!(text.contains(&format!("main.wave:{line}:{column}")), "{text}");
                }
                assert_eq!(fs::read_to_string(self.root.join("main.ll")).unwrap(), "previous IR");
            }
        }
        record
    }

    fn valid(&self, source: &str) {
        fs::write(self.root.join("main.wave"), source).unwrap();
        for command in ["check", "build"] {
            let mut args = vec![command, "main.wave", "--target", &self.target];
            if command == "build" {
                args.push("--emit=ir");
            }
            let output = self.run(&args);
            assert!(output.status.success(), "{source}\n{output:?}");
        }
    }
}

impl Drop for Case {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.root);
    }
}

#[test]
fn keyword_migrations_keep_the_original_token_and_help() {
    let case = Case::new();
    for declaration in ["let value: i32 = 1;", "let mut value: i32 = 1;"] {
        let source = format!("fun main() {{\n    {declaration}\n}}");
        case.error(&source, "let", "use `var`");
    }
    case.error("fn main() {}", "fn", "did you mean `fun`?");
    let result = case.error("other main() {}", "other", "unexpected token at top level");
    assert!(!result.get("error").unwrap().get_str("help").unwrap().contains("did you mean"));
    case.valid("fun main() { var value: i32 = 1; }");
}

#[test]
fn missing_statement_terminators_identify_the_boundary() {
    let case = Case::new();
    // Inferred declarations are not valid Wave syntax; retain the explicit
    // type diagnostic instead of inventing an inferred-declaration grammar.
    case.error("fun main() { var x = 1 }", "=", "requires an explicit type");
    for statement in ["var x: i32 = 1", "return 1", "1 + 2"] {
        let source = format!("fun main() -> i32 {{\n    {statement}\n}}");
        let result = case.error(&source, "}", "expected ';'");
        let Value::Array(expected) = result.get("error").unwrap().get("expected").unwrap() else {
            panic!("expected-token list missing");
        };
        assert!(expected.iter().any(|value| matches!(value, Value::String(text) if text == "';'")));
    }
    for statement in ["break", "continue"] {
        case.error(
            &format!("fun main() {{ while (true) {{ {statement} }} }}"),
            "}",
            "expected ';'",
        );
    }
    let source = "fun main() { var x: i32 = 1";
    case.error_at(source, source.len(), "expected ';'");
    case.valid("fun main() -> i32 { var x: i32 = 1; return x; }");
}

#[test]
fn storage_errors_keep_names_types_and_array_initializers() {
    let case = Case::new();
    for (declaration, offending, message) in [
        ("var 123: i32 = 1;", "123", "identifier"),
        ("var value: = 1;", "=", "type"),
        ("var value: ptr<>;", "ptr", "type"),
    ] {
        case.error(&format!("fun main() {{ {declaration} }}"), offending, message);
    }
    for storage in ["const", "static"] {
        case.error(&format!("{storage} 123: i32 = 1; fun main() {{}}"), "123", "identifier");
        case.error(&format!("{storage} value: = 1; fun main() {{}}"), "=", "type");
        case.error(
            &format!("{storage} value = 1; fun main() {{}}"),
            "=",
            if storage == "const" { "expected ':'" } else { "explicit type" },
        );
    }
    for (initializer, count) in [("[]", 0), ("[1]", 1), ("[1, 2, 3]", 3)] {
        let message = format!("expected 2 elements, found {count}");
        case.error(
            &format!("fun main() {{ var a: array<i32, 2> = {initializer}; }}"),
            initializer,
            &message,
        );
        for storage in ["const", "static"] {
            case.error(
                &format!("{storage} a: array<i32, 2> = {initializer}; fun main() {{}}"),
                initializer,
                &message,
            );
        }
    }
    case.error(
        "fun main() { var a: array<array<i32, 2>, 1> = [[1]]; }",
        "[1]",
        "expected 2 elements, found 1",
    );
    case.valid("const a: array<i32, 2> = [1, 2]; static b: i32 = 3; fun main() { var c: array<array<i32, 2>, 1> = [[1, 2]]; }");
}

#[test]
fn ffi_and_proto_diagnostics_explain_local_failures() {
    let case = Case::new();
    for keyword in ["extern", "export"] {
        for (header, offending, message) in [
            ("123)", "123", "expected '('"),
            ("(123)", "123", "identifier"),
            ("(c, 123)", "123", "symbol string literal"),
            ("(c, \"\\xff\")", "\"\\xff\"", "valid UTF-8"),
            ("(c fun", "fun", "expected ')'"),
        ] {
            case.error(
                &format!("{keyword} {header} fun value(); fun main() {{}}"),
                offending,
                message,
            );
        }
    }
    case.error("export(c) fun value<T>() {} fun main() {}", "fun value", "cannot be generic");
    case.error("export(c) { fun value<T>() {} } fun main() {}", "fun value", "cannot be generic");
    case.error(
        "export(c, \"alias\") { fun value() {} } fun main() {}",
        "\"alias\"",
        "cannot use a single symbol alias",
    );
    case.error("proto 123 {} fun main() {}", "123", "identifier");
    case.error("proto Thing fun main() {}", "fun", "expected '{'");
    case.error("proto Thing { var x: i32; } fun main() {}", "var", "method declaration");
    case.error("proto Thing { fun () {} } fun main() {}", "(", "function name");
    let source = "proto Thing {";
    case.error_at(source, source.len(), "method declaration or '}'");
    case.valid("extern(c) { fun external(i32) -> i32; } export(c) { fun one() -> i32 { return 1; } } struct Thing { value: i32; } proto Thing { fun value(self: ptr<Thing>) -> i32 { return self.value; } } fun main() {}");
}

#[test]
fn io_syntax_and_counts_are_actionable() {
    let case = Case::new();
    for builtin in ["print", "println", "input"] {
        case.error(&format!("fun main() {{ {builtin} 1; }}"), "1", "expected '('");
        case.error(&format!("fun main() {{ {builtin}(1); }}"), "1", "format string literal");
        case.error(
            &format!("fun main() {{ {builtin}(\"{{}} {{}}\"); }}"),
            "\"",
            "expected 2 format arguments, found 0",
        );
        case.error(
            &format!("fun main() {{ {builtin}(\"plain\", 1); }}"),
            "\"",
            "expected 0 format arguments, found 1",
        );
        case.error(&format!("fun main() {{ {builtin}(\"plain\"; }}"), ";", "expected ')'");
        case.error(&format!("fun main() {{ {builtin}(\"plain\") }}"), "}", "expected ';'");
    }
    case.valid("fun main() { var x: i32 = 0; input(\"{}\", x); print(\"plain\"); println(\"{} {x} {:.2}\", x, x, 1.25); }");
}

#[test]
fn placeholder_spans_follow_raw_escapes_and_unicode() {
    let case = Case::new();
    for (literal, offending, message) in [
        ("\"prefix {wat} suffix\"", "{wat}", "unsupported format placeholder"),
        ("\"한글 \\t \\x41 {} {wat}\"", "{wat}", "unsupported format placeholder"),
        ("\"prefix \\x7bwat\\x7d\"", "\\x7b", "unsupported format placeholder"),
        ("\"{:.}\"", "{:.}", "precision"),
        ("\"{:.2147483648}\"", "{:.2147483648}", "precision exceeds"),
        ("\"{\\xff}\"", "{\\xff}", "UTF-8"),
    ] {
        let source = format!("fun main() {{ println({literal}); }}");
        let result = case.error(&source, offending, message);
        let error = result.get("error").unwrap();
        assert!(error.get_str("help").unwrap().contains("{:.N}"));
        let span = error.get("span").unwrap();
        let start = span.get_u64("start").unwrap() as usize;
        let end = span.get_u64("end").unwrap() as usize;
        assert!(!source[start..end].contains('"'));
        assert!(source[start..end].ends_with('}') || source[start..end].ends_with("\\x7d"));
    }
    case.error("fun main() { input(\"text {d}\"); }", "{d}", "only `{}`");
    // Unmatched opening braces have always been literal text, not syntax errors.
    case.valid("fun main() { println(\"literal {unclosed\"); }");
}

#[test]
fn import_failures_preserve_binding_and_loading_sites() {
    let case = Case::new();
    fs::write(case.root.join("helper.wave"), "pub fun exposed() {} fun secret() {}").unwrap();
    fs::write(case.root.join("other.wave"), "pub fun exposed() {}").unwrap();
    case.error("\n\nimport(\"./helper\")::{absent}; fun main() {}", "absent", "no symbol 'absent'");
    case.error("\n\nimport(\"./helper\")::{secret}; fun main() {}", "secret", "is private");
    let source = "import(\"./helper\" as h);\nimport(\"./other\" as h); fun main() {}";
    let record = case.error_at(
        source,
        source.find("import(\"./other").unwrap(),
        "bound to multiple modules",
    );
    assert_eq!(record.get("error").unwrap().get_arr("related").unwrap().len(), 1);
    case.error("\n\nimport(\"./absent\"); fun main() {}", "import", "Could not find import target");
    case.error(
        "\n\nimport(\"missing_package\"); fun main() {}",
        "import",
        "could not resolve external package",
    );
    fs::write(case.root.join("invalid.wave"), [255]).unwrap();
    let record = case.error("\n\nimport(\"./invalid\"); fun main() {}", "import", "valid UTF-8");
    assert!(record.get("error").unwrap().get_str("note").unwrap().contains("invalid.wave"));
    fs::write(case.root.join("broken.wave"), "pub fun broken() { var 123: i32; }").unwrap();
    fs::write(case.root.join("main.wave"), "\nimport(\"./broken\"); fun main() {}").unwrap();
    let output = case.run(&["check", "main.wave", "--target", &case.target, "--error-format=json"]);
    let record = wson::parse_json(std::str::from_utf8(&output.stderr).unwrap().trim()).unwrap();
    let error = record.get("error").unwrap();
    assert!(error.get_str("file").unwrap().ends_with("broken.wave"));
    assert_eq!(error.get_arr("related").unwrap().len(), 1);
    assert!(error.get_arr("related").unwrap()[0]
        .get("span")
        .unwrap()
        .get_str("file")
        .unwrap()
        .ends_with("main.wave"));
    case.valid("import(\"./helper\")::{exposed}; fun main() { exposed(); }");
}

#[test]
#[cfg(any(feature = "llvm-target-wasm", feature = "llvm-target-all"))]
fn missing_webassembly_socket_capabilities_are_not_spelling_errors() {
    let case = Case::new();
    let std_root = format!("{}/std", env!("CARGO_MANIFEST_DIR"));
    for target in ["wasm32-wasip1", "wasm32-unknown-unknown", "wasm64-unknown-unknown"] {
        for (symbol, message) in
            [("listen", "native sockets are unavailable"), ("misspelled", "no symbol 'misspelled'")]
        {
            fs::write(
                case.root.join("main.wave"),
                format!("import(\"std::sys::socket\")::{{{symbol}}}; fun main() {{}}"),
            )
            .unwrap();
            let output = case.run(&[
                "check",
                "main.wave",
                "--target",
                target,
                "--std-root",
                &std_root,
                "--error-format=json",
            ]);
            assert_eq!(output.status.code(), Some(1));
            let text = String::from_utf8(output.stderr).unwrap();
            assert!(text.contains(message), "{target}: {text}");
        }
        fs::write(case.root.join("main.wave"), "import(\"std::net::tcp\"); fun main() {}").unwrap();
        let output = case.run(&[
            "check",
            "main.wave",
            "--target",
            target,
            "--std-root",
            &std_root,
            "--error-format=json",
        ]);
        let text = String::from_utf8(output.stderr).unwrap();
        assert!(text.contains("native sockets are unavailable"), "{text}");
        assert!(text.contains("tcp.wave"), "{text}");
        assert!(text.contains(target.split('-').next().unwrap()), "{text}");
    }
}

#[test]
fn semantic_errors_focus_type_annotations_and_input_destinations() {
    let case = Case::new();
    for ty in ["Missing", "ptr<Missing>", "array<Missing, 2>"] {
        case.error(&format!("fun main() {{\n var 문: {ty};\n}}"), ty, "unknown type");
    }
    case.error(
        "fun main() { var x: i32 = 1; var x: i32 = 2; }",
        "x: i32 = 2",
        "duplicate variable",
    );
    for arguments in ["1, n, n", "n, 1, n", "n, n, 1"] {
        let source =
            format!("fun main() {{ var n: i32 = 0; input(\"{{}} {{}} {{}}\", {arguments}); }}");
        case.error(&source, "1", "mutable lvalue");
    }
    case.error(
        "const fixed: i32 = 1; fun main() { input(\"{}\", fixed); }",
        "fixed);",
        "immutable",
    );
    fs::write(case.root.join("helper.wave"), "pub fun exposed() { var 값: Unknown; }").unwrap();
    fs::write(
        case.root.join("main.wave"),
        "import(\"./helper\")::{exposed}; fun main() { exposed(); }",
    )
    .unwrap();
    let output = case.run(&["check", "main.wave", "--target", &case.target, "--error-format=json"]);
    let record = wson::parse_json(std::str::from_utf8(&output.stderr).unwrap().trim()).unwrap();
    let error = record.get("error").unwrap();
    assert!(error.get_str("file").unwrap().ends_with("helper.wave"));
    assert_eq!(
        error.get("span").unwrap().get_u64("start"),
        Some("pub fun exposed() { var 값: ".len() as u64)
    );
    case.valid("type Number = i32; fun main() { var x: Number = 0; input(\"{}\", x); }");
}

#[test]
fn recursive_inline_storage_is_rejected_before_llvm() {
    let case = Case::new();
    for (declarations, offending) in [
        ("struct Node { next: Node; }", "next: Node"),
        ("struct A { b: B; } struct B { a: A; }", "a: A"),
        ("struct Node { next: array<Node, 1>; }", "next: array"),
        ("type Alias = Node; struct Node { next: Alias; }", "next: Alias"),
        ("struct Node<T> { next: Node<T>; }", "next: Node"),
        ("struct Wrap<T> { value: T; } struct Node { next: Wrap<Node>; }", "value: T"),
    ] {
        case.error(&format!("{declarations} fun main() {{}}"), offending, "infinite size");
    }
    case.valid("struct Node { next: ptr<Node>; } fun main() { var n: Node; }");
    case.valid("struct Wrap<T> { value: T; } fun main() { var n: Wrap<Wrap<i32>>; }");
    case.valid("struct Node<T> { next: ptr<Node<T>>; value: T; } fun main() { var n: Node<i32>; }");
    fs::write(
        case.root.join("main.wave"),
        "struct Grow<T> { next: Grow<array<T, 1>>; } fun main() { var n: Grow<i32>; }",
    )
    .unwrap();
    let output = case.run(&["check", "main.wave", "--target", &case.target, "--error-format=json"]);
    assert_eq!(output.status.code(), Some(1));
    let text = String::from_utf8(output.stderr).unwrap();
    assert!(text.contains("generic instantiation depth exceeded"), "{text}");
}

#[test]
fn unclosed_comments_keep_eof_and_the_innermost_opening_span() {
    let case = Case::new();
    for newline in ["\n", "\r\n", "\r"] {
        let source = format!("// 한글{newline}/* outer{newline}/* inner{newline}");
        fs::write(case.root.join("main.wave"), &source).unwrap();
        let output =
            case.run(&["check", "main.wave", "--target", &case.target, "--error-format=json"]);
        assert_eq!(output.status.code(), Some(1));
        let record = wson::parse_json(std::str::from_utf8(&output.stderr).unwrap().trim()).unwrap();
        let error = record.get("error").unwrap();
        assert_eq!(error.get("span").unwrap().get_u64("start"), Some(source.len() as u64));
        let related = error.get_arr("related").unwrap();
        assert_eq!(
            related[0].get("span").unwrap().get_u64("start"),
            Some(source.find("/* inner").unwrap() as u64)
        );
        assert_eq!(related[0].get("span").unwrap().get_u64("line"), Some(3));
        let human = case.run(&["check", "main.wave", "--target", &case.target]);
        assert!(String::from_utf8(human.stderr).unwrap().contains("opened here"));
    }
    case.valid("/* outer /* inner */ */ fun main() {}");
}

#[test]
fn human_source_control_sequences_do_not_reach_the_terminal() {
    let case = Case::new();
    let source = "fun main() { \"\u{1b}[2J\"; missing; }";
    case.error(source, "missing", "undeclared identifier");
    let output = case.run(&["check", "main.wave", "--target", &case.target]);
    let human = String::from_utf8(output.stderr).unwrap();
    assert!(!human.contains('\u{1b}'), "{human:?}");
    assert!(human.contains("\\u{1b}[2J"));
    let output = case.run(&["bad\u{1b}[2Jcommand"]);
    assert!(!output.stderr.contains(&27));
}

#[test]
fn final_error_format_applies_to_usage_and_source_failures() {
    let case = Case::new();
    for (options, json) in [
        (vec!["--error-format=json", "--error-format=human"], false),
        (vec!["--error-format", "human", "--error-format", "json"], true),
    ] {
        for command in [vec!["oops"], vec!["check", "missing.wave"]] {
            let args: Vec<_> = options.iter().chain(&command).copied().collect();
            let output = case.run(&args);
            assert!(!output.status.success());
            let text = std::str::from_utf8(&output.stderr).unwrap().trim();
            assert_eq!(wson::parse_json(text).is_ok(), json, "{args:?}: {text}");
        }
    }
    for (first, last, json) in [("json", "human", false), ("human", "json", true)] {
        for suffix in [vec![], vec!["--unknown-option"]] {
            let mut args =
                vec!["--error-format", first, "check", "missing.wave", "--error-format", last];
            args.extend(suffix);
            let output = case.run(&args);
            let text = std::str::from_utf8(&output.stderr).unwrap().trim();
            assert_eq!(wson::parse_json(text).is_ok(), json, "{args:?}: {text}");
        }
    }
    assert!(!wavec::cli::args_request_json_errors([
        "--error-format=human",
        "--",
        "--error-format=json"
    ]));
    assert!(wavec::cli::args_request_json_errors([
        "--error-format=json",
        "--error-format=invalid"
    ]));
    assert!(wavec::cli::args_request_json_errors(["--error-format=json", "--error-format"]));
}

#[test]
fn source_read_errors_remain_structured_for_ast_and_other_outputs() {
    let case = Case::new();
    fs::write(case.root.join("invalid.wave"), [255]).unwrap();
    fs::create_dir(case.root.join("directory.wave")).unwrap();
    for (input, explanation) in
        [("missing.wave", "exists"), ("invalid.wave", "UTF-8"), ("directory.wave", "directory")]
    {
        for output in ["invalid.ll", "invalid.ast.wson"] {
            fs::write(case.root.join(output), "previous output").unwrap();
        }
        for emit in [None, Some("--emit=ast"), Some("--emit=ir"), Some("--emit=obj")] {
            for format in ["json", "human"] {
                let mut args = vec![
                    if emit.is_some() { "build" } else { "check" },
                    input,
                    "--target",
                    &case.target,
                    "--error-format",
                    format,
                ];
                if let Some(emit) = emit {
                    args.push(emit);
                }
                let output = case.run(&args);
                assert_eq!(output.status.code(), Some(1), "{args:?}: {output:?}");
                assert!(output.stdout.is_empty());
                let text = String::from_utf8(output.stderr).unwrap();
                if format == "json" {
                    let record = wson::parse_json(text.trim()).unwrap();
                    let error = record.get("error").unwrap();
                    assert_eq!(error.get_str("kind"), Some("file-read-error"));
                    assert!(error.get_str("help").unwrap().contains(explanation), "{text}");
                    assert!(matches!(error.get("span"), Some(Value::Null)));
                    assert_eq!(error.get_u64("line"), Some(0));
                    assert_eq!(error.get_u64("column"), Some(0));
                } else {
                    assert!(text.contains(explanation), "{text}");
                }
            }
        }
        for output in ["invalid.ll", "invalid.ast.wson"] {
            assert_eq!(fs::read_to_string(case.root.join(output)).unwrap(), "previous output");
        }
    }
}

#[test]
fn duplicate_declarations_show_both_name_locations_and_shadowing_stays_legal() {
    let case = Case::new();
    for (source, name, message) in [
        ("struct S { 항목: i32;\n 항목: i64; }\nfun main() {}", "항목", "duplicate field"),
        ("fun duplicate() {}\nfun duplicate() {}\nfun main() {}", "duplicate", "duplicate value"),
        (
            "fun main() {\n var value: i32 = 1;\n var value: i32 = 2;\n}",
            "value",
            "duplicate variable",
        ),
    ] {
        let second = source.rfind(name).unwrap();
        let record = case.error_at(source, second, message);
        let related = record.get("error").unwrap().get_arr("related").unwrap();
        assert_eq!(related.len(), 1);
        assert_eq!(
            related[0].get("span").unwrap().get_u64("start"),
            Some(source.find(name).unwrap() as u64)
        );
    }
    case.valid("fun main() { var value: i32 = 1; if (true) { var value: i32 = 2; } }");
}

#[test]
fn spelling_candidates_are_visible_kind_specific_and_deterministic() {
    let case = Case::new();
    for (source, wrong, candidate) in [
        ("fun main() { var count: i32 = 1; println(\"{}\", coutn); }", "coutn", "count"),
        ("fun count() {} fun main() { coutn(); }", "coutn", "count"),
        ("struct Counter { value: i32; } fun main() { var n: Countr; }", "Countr", "Counter"),
    ] {
        fs::write(case.root.join("main.wave"), source).unwrap();
        for _ in 0..3 {
            let output =
                case.run(&["check", "main.wave", "--target", &case.target, "--error-format=json"]);
            assert_eq!(output.status.code(), Some(1));
            let json = wson::parse_json(String::from_utf8_lossy(&output.stderr).trim()).unwrap();
            let suggestions = json.get("error").unwrap().get_arr("suggestions").unwrap();
            assert!(
                suggestions.iter().any(|s| matches!(s, Value::String(name) if name == candidate)),
                "{wrong}: {json:?}"
            );
        }
    }
    fs::write(
        case.root.join("main.wave"),
        "fun main() { if (true) { var secret: i32 = 1; } println(\"{}\", secrt); }",
    )
    .unwrap();
    let output = case.run(&["check", "main.wave", "--target", &case.target, "--error-format=json"]);
    let record = wson::parse_json(String::from_utf8_lossy(&output.stderr).trim()).unwrap();
    assert!(record.get("error").unwrap().get_arr("suggestions").unwrap().is_empty());
}

#[test]
fn imported_errors_render_loaded_related_source_without_changing_json_locations() {
    let case = Case::new();
    fs::write(case.root.join("main.wave"), "import(\"./bad.wave\");\nfun main() {}\n").unwrap();
    fs::write(case.root.join("bad.wave"), "fun broken( {\n").unwrap();
    let human = case.run(&["check", "main.wave", "--target", &case.target]);
    assert!(!human.status.success());
    let text = String::from_utf8(human.stderr).unwrap();
    assert!(text.contains("import(\"./bad.wave\");"), "{text}");
    assert!(text.contains("bad.wave") && text.contains("main.wave"));
}
