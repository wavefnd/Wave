//! Exercise the public renderers in isolated processes without changing global test state.
use error::{ErrorSeverity, WaveError, WaveErrorKind};
use std::process::Command;
use utils::wson::{self, Value};

fn error() -> WaveError {
    WaveError::new(WaveErrorKind::UnexpectedEndOfFile, "missing closer", "input.wave", 2, 1)
        .with_label("close the block")
}

#[test]
fn renderer_fixture() {
    let Ok(case) = std::env::var("WAVE_RENDER_TEST_CASE") else {
        return;
    };
    match case.as_str() {
        "batch" => {
            WaveError::display_batch(&[error(), error().with_severity(ErrorSeverity::Warning)])
        },
        "empty-batch" => WaveError::display_batch(&[]),
        "single-batch" => WaveError::display_batch(&[error()]),
        "lf" => error().with_source_code("fun main() {\n").display(),
        "cr" => error().with_source_code("fun main() {\r").display(),
        "crlf" => error().with_source_code("fun main() {\r\n").display(),
        "empty" => {
            WaveError::new(WaveErrorKind::UnexpectedEndOfFile, "expected item", "empty.wave", 1, 1)
                .with_source_code("")
                .display()
        },
        "controls" => {
            let escape = "\u{1b}[2J";
            let mut diagnostic = error()
                .with_source_code("safe\n\t한\u{1b}[2Jmissing")
                .with_label(escape)
                .with_context(escape)
                .with_found(escape)
                .with_help(escape)
                .with_note(escape)
                .with_suggestion(escape)
                .with_expected(escape)
                .with_code(escape);
            diagnostic.file = format!("file{escape}.wave");
            diagnostic.message = format!("message{escape}");
            diagnostic.display_auto();
        },
        "invalid-line" => error().with_source_code("only one line").display(),
        _ => panic!("unknown fixture"),
    }
}

fn rendered(case: &str, format: &str) -> String {
    let output = Command::new(std::env::current_exe().unwrap())
        .args(["--exact", "renderer_fixture", "--nocapture"])
        .env("WAVE_RENDER_TEST_CASE", case)
        .env("WAVE_ERROR_FORMAT", format)
        .env("NO_COLOR", "1")
        .output()
        .unwrap();
    assert!(output.status.success(), "{output:?}");
    String::from_utf8(output.stderr).unwrap()
}

#[test]
fn json_preserves_labels_and_escapes_them() {
    let label = "expected \"value\"\nnext\tcolumn\\";
    let value = wson::parse_json(&error().with_label(label).to_json()).unwrap();
    assert_eq!(value.get("error").unwrap().get_str("label"), Some(label));
    let value = wson::parse_json(
        &WaveError::new(WaveErrorKind::UnexpectedEndOfFile, "end", "x", 1, 1).to_json(),
    )
    .unwrap();
    assert!(matches!(value.get("error").unwrap().get("label"), Some(Value::Null)));
}

#[test]
fn type_mismatch_serializes_supplied_facts_without_changing_diagnostics() {
    let span = error::SourceSpan {
        file: "module.wave".into(),
        start: 9,
        end: 12,
        line: 3,
        column: 5,
        end_line: 3,
        end_column: 8,
        expansion: Vec::new(),
        focus: None,
    };
    let diagnostic = WaveError::type_mismatch("array<i32>", "str", "input.wave", 2, 1)
        .with_code("E3001")
        .with_span(Some(&span));
    assert_eq!(diagnostic.expected, vec!["array<i32>".to_string()]);
    assert_eq!(diagnostic.found.as_deref(), Some("str"));
    assert_eq!(diagnostic.message, "mismatched types: expected `array<i32>`, found `str`");
    assert_eq!(diagnostic.label.as_deref(), Some("expected `array<i32>`, found `str`"));
    let record = wson::parse_json(&diagnostic.to_json()).unwrap();
    let err = record.get("error").unwrap();
    assert_eq!(err.get_str("kind"), Some("type-mismatch"));
    assert_eq!(err.get_str("message"), Some(diagnostic.message.as_str()));
    assert_eq!(err.get_str("code"), Some("E3001"));
    assert_eq!(err.get_str("file"), Some("module.wave"));
    assert_eq!(err.get_u64("line"), Some(3));
    assert_eq!(err.get_u64("column"), Some(5));
    assert_eq!(err.get("span").unwrap().get_u64("start"), Some(9));
    assert_eq!(err.get("span").unwrap().get_u64("end"), Some(12));
    let Some(Value::Array(expected)) = err.get("expected") else {
        panic!("type mismatch expected facts must be a JSON array");
    };
    assert_eq!(expected.as_slice(), &[Value::String("array<i32>".to_string())]);
    assert_eq!(err.get_str("found"), Some("str"));
}

#[test]
fn text_only_semantic_error_is_not_inferred_from_its_message() {
    // Some semantic producers still supply only human-readable messages.
    // Fixing type_mismatch must not pretend to fill their structured fields.
    let diagnostic = WaveError::new(
        WaveErrorKind::InvalidStatement("incompatible type".to_string()),
        "semantic validation failed: expected i32, found str",
        "input.wave",
        4,
        2,
    )
    .with_code("E3001");
    let record = wson::parse_json(&diagnostic.to_json()).unwrap();
    let err = record.get("error").unwrap();
    assert_eq!(err.get_str("message"), Some(diagnostic.message.as_str()));
    assert_eq!(err.get_str("code"), Some("E3001"));
    assert!(matches!(err.get("expected"), Some(Value::Array(items)) if items.is_empty()));
    assert!(matches!(err.get("found"), Some(Value::Null)));
}

#[test]
fn json_batches_contain_only_one_json_record_per_diagnostic() {
    for (case, count) in [("empty-batch", 0), ("single-batch", 1), ("batch", 2)] {
        let output = rendered(case, "json");
        let records: Vec<_> = output
            .lines()
            .map(|line| wson::parse_json(line).expect("each line must be JSON"))
            .collect();
        assert_eq!(records.len(), count, "{output}");
        if count == 2 {
            assert_eq!(records[0].get("error").unwrap().get_str("severity"), Some("error"));
            assert_eq!(records[1].get("error").unwrap().get_str("severity"), Some("warning"));
        }
    }
}

#[test]
fn human_batches_keep_summary_counts() {
    let output = rendered("batch", "human");
    assert!(output.contains("error: aborting due to 1 previous error"), "{output}");
    assert!(output.contains("warning: 1 warning emitted"), "{output}");
}

#[test]
fn eof_carets_render_on_empty_final_source_lines() {
    for case in ["lf", "crlf", "cr", "empty"] {
        let output = rendered(case, "human");
        let lines: Vec<_> = output.lines().collect();
        let marker = lines.iter().position(|line| line.contains('^')).expect("EOF needs a caret");
        let source_line = if case == "empty" { "  1 | " } else { "  2 | " };
        assert_eq!(lines[marker - 1], source_line, "{output}");
        assert!(!output.contains('\r'), "{output}");
    }
}

#[test]
fn out_of_range_locations_do_not_show_an_unrelated_source_line() {
    let output = rendered("invalid-line", "human");
    assert!(output.contains("input.wave:2:1"));
    assert!(!output.contains("only one line"), "{output}");
}

#[test]
fn human_fields_and_source_controls_are_escaped_without_changing_json() {
    let human = rendered("controls", "human");
    assert!(!human.contains('\u{1b}'), "{human:?}");
    assert!(human.contains("\\u{1b}[2J"));
    let json = rendered("controls", "json");
    let record = wson::parse_json(json.trim()).unwrap();
    let error = record.get("error").unwrap();
    assert_eq!(error.get_str("label"), Some("\u{1b}[2J"));
    assert_eq!(error.get_str("file"), Some("file\u{1b}[2J.wave"));
}
