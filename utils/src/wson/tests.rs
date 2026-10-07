use super::*;

#[test]
fn wson_comments_dates_versions_and_order_share_the_value_model() {
    let value = loads(
        r##"{
            // markers inside quoted text remain bytes
            z = "https://wave/#anchor /* text */", a = 1.5,
            version = 1.2.3, when = 2024-02-29 23:59:59,
            "escaped\u0020key" = [true, null, "a,b",],
        }"##,
    )
    .unwrap();
    assert_eq!(value.get_str("z"), Some("https://wave/#anchor /* text */"));
    assert_eq!(value.get_num("a"), Some(1.5));
    assert_eq!(value.get("version"), Some(&Value::Version(vec![1, 2, 3])));
    assert_eq!(loads(&dumps(&value, Format::Wson, true).unwrap()).unwrap(), value);
    assert!(dumps(&value, Format::Json, false).is_err());
    let Value::Object(fields) = value else { panic!("object") };
    assert_eq!(fields[0].0, "z");
    for bad in [
        "{a=2023-02-29}",
        "{a=2024-13-01}",
        "{a=2024-01-01 24:00:00}",
        "{a=1.2.4294967296}",
        "{/* missing close",
        "{a=}",
    ] {
        assert!(loads(bad).is_err(), "{bad}");
    }
}

#[test]
fn exact_numbers_never_round_through_f64() {
    let integer = "17976931348623159077293051907890247336179769789423065727343008115";
    for text in [integer, "18446744073709551615", "-0", "-0.000e+10", "1.230000e-12"] {
        let value = parse_json(text).unwrap();
        assert_eq!(dumps(&value, Format::Json, false).unwrap(), text);
    }
    let value = parse_json(&format!("{{\"n\":{}}}", u64::MAX)).unwrap();
    assert_eq!(value.get_u64("n"), Some(u64::MAX));
    assert!(parse_json(&"9".repeat(700)).is_err());
    assert!(parse_json("1e-9999").is_err());
    assert!(parse_json("1e9999").is_err());
    assert!(parse_json("5e-324").is_ok());
}

#[test]
fn duplicate_keys_and_depth_are_rejected_on_both_read_and_write() {
    for (text, format) in [(r#"{"x":1,"\u0078":2}"#, Format::Json), ("{x=1,x=2}", Format::Wson)] {
        assert!(parse(text, format).unwrap_err().message.contains("duplicate"));
    }
    assert!(dumps(
        &Value::Object(vec![("x".into(), Value::Null), ("x".into(), Value::Null)]),
        Format::Json,
        false
    )
    .is_err());
    for format in [Format::Json, Format::Wson] {
        let at_limit = format!("{}0{}", "[".repeat(MAX_DEPTH), "]".repeat(MAX_DEPTH));
        let value = parse(&at_limit, format).unwrap();
        assert!(dumps(&value, format, false).is_ok());
        assert!(parse(&format!("[{at_limit}]"), format).is_err());
        let mut output = Vec::new();
        assert!(Value::Array(vec![value]).write_to(&mut output, format, false).is_err());
        assert!(output.is_empty());
    }
}

#[test]
fn json_mode_is_strict_and_errors_keep_physical_positions() {
    for text in ["{x:1}", "{\"x\"=1}", "[1,]", "{/*comment*/}", "True", "01", "+1", "NaN", "1.2.3"]
    {
        assert!(parse_json(text).is_err(), "{text}");
    }
    let text = "{\r\n\"한\": 1,\r\"한\":2}";
    let error = parse_json(text).unwrap_err();
    assert_eq!(error.line, 3);
    assert_eq!(error.column, 4);
    assert_eq!(error.offset, text.find(":2").unwrap());
}

#[test]
fn unicode_strings_and_keys_round_trip() {
    for (text, expected) in [
        (r#""한글😀""#, "한글😀"),
        (r#""\uD55C\uAE00\uD83D\uDE00""#, "한글😀"),
        (r#""a\u0000\n\t\b\f\r\/\\\"z""#, "a\0\n\t\x08\x0c\r/\\\"z"),
    ] {
        let value = parse_json(text).unwrap();
        assert!(matches!(&value, Value::String(s) if s == expected));
        let mut output = Vec::new();
        value.write_to(&mut output, Format::Json, false).unwrap();
        assert!(
            matches!(parse_json(std::str::from_utf8(&output).unwrap()).unwrap(), Value::String(s) if s == expected)
        );
        let object = parse_json(&format!("{{{text}:1}}")).unwrap();
        assert_eq!(object.get_num(expected), Some(1.0));
    }
}

#[test]
fn invalid_unicode_escapes_are_errors() {
    for text in [
        r#""\u12""#,
        r#""\uGGGG""#,
        r#""\uD800""#,
        r#""\uDC00""#,
        r#""\uD800\u0041""#,
        r#""\uD800\uD800""#,
        r#""\uD800x""#,
        r#""\uD800\u""#,
    ] {
        assert!(parse_json(text).is_err(), "{text}");
    }
}

#[test]
fn all_raw_control_bytes_are_rejected_in_values_and_keys() {
    for byte in 0..=31u8 {
        let text = format!("\"{}\"", char::from(byte));
        assert!(parse_json(&text).is_err(), "byte {byte}");
        assert!(parse_json(&format!("{{{text}:0}}")).is_err(), "key byte {byte}");
        assert!(parse_json(&format!(r#""\u{byte:04x}""#)).is_ok());
    }
}

#[test]
fn numeric_range_errors_do_not_change_json_types() {
    for text in ["1e9999", "-1e9999", "1.7976931348623159e308"] {
        assert!(parse_json(text).unwrap_err().message.contains("finite f64 range"));
    }
    for text in ["1.7976931348623157e308", "-1.7976931348623157e308", "1.5", "-0", "5e-324"] {
        let value = parse_json(text).unwrap();
        let output = dumps(&value, Format::Json, false).unwrap();
        assert_eq!(output, text);
        assert_eq!(parse_json(&output).unwrap(), value);
    }
}

#[test]
fn explicit_writer_budget_does_not_relax_default_or_parser_limits() {
    let mut value = Value::Null;
    for _ in 0..512 {
        value = Value::Array(vec![value]);
    }
    for format in [Format::Json, Format::Wson] {
        assert!(dumps(&value, format, false).is_err());
        let text = dumps_with_depth_limit(&value, format, false, 512).unwrap();
        assert!(parse(&text, format).is_err());
        assert!(dumps_with_depth_limit(&value, format, false, 513).is_err());
    }
    assert!(dumps_with_depth_limit(&Value::Array(vec![value]), Format::Json, false, 512).is_err());
}
