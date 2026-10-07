//! Driver-level Alpha frontend contracts, including imports and target selection.
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

static NEXT: AtomicUsize = AtomicUsize::new(0);
fn directory() -> PathBuf {
    let path = std::env::temp_dir().join(format!(
        "wave-alpha-{}-{}",
        std::process::id(),
        NEXT.fetch_add(1, Ordering::Relaxed)
    ));
    std::fs::create_dir_all(&path).unwrap();
    path
}

fn wave(args: &[&OsStr]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_wavec")).args(args).output().unwrap()
}
// Exercise the public host default, including MSVC on Windows.
#[cfg(any(feature = "llvm-target-core64", feature = "llvm-target-all"))]
fn native_wave(args: &[&OsStr]) -> Output {
    wave(args)
}

fn frontend_target() -> String {
    let output = wave(&[OsStr::new("print"), OsStr::new("target-list")]);
    successful(&output);
    String::from_utf8(output.stdout)
        .unwrap()
        .lines()
        .next()
        .expect("at least one LLVM target must be enabled")
        .to_owned()
}

fn check(path: &Path, target: &str) -> Output {
    wave(&[OsStr::new("check"), path.as_os_str(), OsStr::new("--target"), OsStr::new(target)])
}

fn successful(output: &Output) {
    assert!(
        output.status.success(),
        "status {:?}\n{}\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn imported_variants_respect_active_and_inactive_target_attributes() {
    let dir = directory();
    let target = frontend_target();
    let active_arch = target.split('-').next().unwrap();
    let inactive_arch = if active_arch == "wasm64" { "arm64" } else { "wasm64" };
    let declarations = |value_arch: &str, missing_arch: &str| {
        format!(
            r#"
#[target(arch="{missing_arch}")]
pub variant Choice {{
    Missing(Unavailable),
}}
#[target(arch="{value_arch}")]
pub variant Choice {{
    Value(i32),
}}
"#
        )
    };
    let library = dir.join("choices.wave");
    std::fs::write(&library, declarations(active_arch, inactive_arch)).unwrap();
    let source = dir.join("main.wave");
    std::fs::write(
        &source,
        "import(\"./choices\")::{Choice};\nfun main() { var x: Choice = Choice::Value(16); }\n",
    )
    .unwrap();
    successful(&check(&source, &target));
    std::fs::write(&library, declarations(inactive_arch, active_arch)).unwrap();
    let output = check(&source, &target);
    assert!(!output.status.success());
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("Choice::Value"),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn json_diagnostics_preserve_imported_byte_ranges() {
    let dir = directory();
    let library = dir.join("broken.wave");
    let text = "fun broken() {\n    var 이름: i32 = 1; 이름; missing; missing;\n    var after: i32 = 2;\n}\n";
    std::fs::write(&library, text).unwrap();
    let source = dir.join("main.wave");
    std::fs::write(&source, "import(\"./broken\"); fun main() {}\n").unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_wavec"))
        .args(["--error-format=json", "check"])
        .arg(&source)
        .args(["--target", &frontend_target()])
        .output()
        .unwrap();
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("broken.wave"), "{stderr}");
    let human = check(&source, &frontend_target());
    let human = String::from_utf8_lossy(&human.stderr);
    assert!(
        human.find('^').unwrap() < human.find("var after").unwrap(),
        "the marker must immediately follow the failing source line: {human}"
    );
    assert!(stderr.contains(&format!("\"start\":{}", text.find("missing").unwrap())), "{stderr}");
    assert!(stderr.contains(&format!("\"end\":{}", text.find("missing").unwrap() + 7)), "{stderr}");
}

#[cfg(any(feature = "llvm-target-core64", feature = "llvm-target-all"))]
#[test]
fn literal_defaults_compile_and_run_with_their_shared_numeric_values() {
    let dir = directory();
    let source = dir.join("defaults.wave");
    std::fs::write(&source,r#"
const RADIX: i128 = 0x10;
static OCTAL: u128 = 0o20;
const LARGE: f64 = 18446744073709551616 as f64;
const EXPONENT: f64 = 1e2;
enum E -> i32 { Min = -1, Hex = 0x10 }
fun hex(x: i32 = 0x10) -> i32 { return x; }
fun binary(x: i32 = 0b1_0000) -> i32 { return x; }
fun octal(x: i32 = 0o20) -> i32 { return x; }
fun decimal(x: i32 = 1_6) -> i32 { return x; }
fun exponent(x: f64 = 1.6e1) -> f64 { return x; }
fun main() -> i32 {
    if (hex() != 16 || binary() != 16 || octal() != 16 || decimal() != 16 || exponent() != 16.0) { return 1; }
    if (RADIX != 16 || OCTAL != 16 || LARGE != 18446744073709551616.0 || EXPONENT != 100.0 || E::Min != -1 || E::Hex != 16) { return 3; }
    var pointer_word: usz = 16;
    if (pointer_word != 16) { return 2; }
    return 0;
}
"#).unwrap();
    successful(&native_wave(&[
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--run"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]));
}

#[cfg(any(feature = "llvm-target-wasm", feature = "llvm-target-all"))]
#[test]
fn pointer_sized_integer_ranges_follow_wasm_target_width() {
    let dir = directory();
    let source = dir.join("word.wave");
    std::fs::write(&source, "fun main() { var x: usz = 4294967296; }\n").unwrap();
    successful(&check(&source, "wasm64-unknown-unknown"));
    let output = check(&source, "wasm32-unknown-unknown");
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("u32") && !stderr.contains("panicked"), "{stderr}");
    std::fs::write(&source, "fun word(x: isz) -> isz { return x; }\nfun main() {}\n").unwrap();
    for (target, bits) in [("wasm32-unknown-unknown", 32), ("wasm64-unknown-unknown", 64)] {
        let output_dir = dir.join(target);
        successful(&wave(&[
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--emit=ir"),
            OsStr::new("--out-dir"),
            output_dir.as_os_str(),
        ]));
        let ir = std::fs::read_to_string(output_dir.join("word.ll")).unwrap();
        assert!(ir.contains(&format!("define i{bits} @word(i{bits}")), "{ir}");
    }
}

#[cfg(any(feature = "llvm-target-core64", feature = "llvm-target-all"))]
#[test]
fn never_returning_calls_lower_to_noreturn_and_unreachable() {
    let dir = directory();
    let source = dir.join("never.wave");
    std::fs::write(
        &source,
        "fun stop() -> ! { while (true) {} } fun value() -> i32 { stop(); } fun main() {}\n",
    )
    .unwrap();
    successful(&wave(&[
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]));
    let ir = std::fs::read_to_string(dir.join("never.ll")).unwrap();
    assert!(ir.contains("define void @stop()"), "{ir}");
    assert!(ir.contains("noreturn"), "{ir}");
    let value = ir.split("define i32 @value()").nth(1).unwrap().split("\n}").next().unwrap();
    assert!(value.contains("call void @stop()") && value.contains("unreachable"), "{value}");
}

#[test]
fn file_errors_do_not_invent_a_source_position() {
    let source = directory().join("missing.wave");
    let target = frontend_target();
    let human = check(&source, &target);
    assert!(!human.status.success());
    let human = String::from_utf8_lossy(&human.stderr);
    assert!(human.contains("failed to read file"), "{human}");
    assert!(!human.contains("missing.wave:1:1"), "{human}");
    let json = wave(&[
        OsStr::new("--error-format=json"),
        OsStr::new("check"),
        source.as_os_str(),
        OsStr::new("--target"),
        OsStr::new(&target),
    ]);
    assert!(!json.status.success());
    let json = String::from_utf8_lossy(&json.stderr);
    assert!(json.contains("\"span\":null"), "{json}");
    assert!(json.contains("\"line\":0"), "{json}");
    assert!(json.contains("\"column\":0"), "{json}");
}

#[cfg(windows)]
#[test]
fn windows_std_import_and_print_use_userprofile_without_home() {
    let profile = directory().join("profile with spaces");
    let root = profile.join(".wave/lib/wave/std");
    std::fs::create_dir_all(&root).unwrap();
    std::fs::write(
        root.join("manifest.json"),
        format!(
            "{{\"name\":\"std\",\"compatibility_revision\":{}}}",
            parser::import::STD_COMPATIBILITY_REVISION,
        ),
    )
    .unwrap();
    std::fs::write(root.join("location_probe.wave"), "pub fun located() -> i32 { return 17; }\n")
        .unwrap();
    let source = profile.join("probe.wave");
    std::fs::write(
        &source,
        "import(\"std::location_probe\")::{located}; fun main() -> i32 { return located(); }\n",
    )
    .unwrap();
    let command = || {
        let mut cmd = Command::new(env!("CARGO_BIN_EXE_wavec"));
        cmd.env_remove("HOME").env("USERPROFILE", &profile);
        cmd
    };
    let printed = command().args(["print", "std-path"]).output().unwrap();
    successful(&printed);
    assert_eq!(String::from_utf8(printed.stdout).unwrap().trim(), root.to_string_lossy());
    let imported =
        command().arg("check").arg(source).args(["--target", &frontend_target()]).output().unwrap();
    successful(&imported);
    std::fs::remove_dir_all(profile).unwrap();
}

#[test]
fn control_header_diagnostics_agree_in_human_and_json_output() {
    let dir = directory();
    let target = frontend_target();
    let library = dir.join("broken.wave");
    let source = dir.join("main.wave");
    std::fs::write(&source, "import(\"./broken\"); fun main() {}\n").unwrap();
    for (body, context, expected, unexpected) in [
        ("if (1 {}", "if header", "')'", "{"),
        ("if (1) {} else if (0 {}", "else if header", "')'", "{"),
        ("if (1) {} else return;", "else header", "'{'", "return"),
        ("while (1 {}", "while header", "')'", "{"),
        ("for (i = 0 i < 2; i += 1) {}", "for initializer", "';'", "i <"),
    ] {
        let text = format!("fun broken() {{\n    {body}\n}}\n");
        std::fs::write(&library, &text).unwrap();
        let human = check(&source, &target);
        let json = wave(&[
            OsStr::new("--error-format=json"),
            OsStr::new("check"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(&target),
        ]);
        for output in [&human, &json] {
            assert!(!output.status.success());
            assert!(output.stdout.is_empty(), "legacy parser output: {:?}", output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains("E2001"), "{stderr}");
            assert!(stderr.contains("broken.wave"), "{stderr}");
            assert!(stderr.contains(&format!("expected {expected} in {context}")), "{stderr}");
            assert!(!stderr.contains("failed to parse function declaration"), "{stderr}");
        }
        let json = String::from_utf8_lossy(&json.stderr);
        let start = text.find(body).unwrap() + body.rfind(unexpected).unwrap();
        assert_eq!(json.lines().count(), 1, "{json}");
        assert!(json.contains(&format!("\"start\":{start},")), "{json}");
        assert!(json.contains(&format!("\"expected\":[\"{expected}\"]")), "{json}");
        assert!(json.contains(&format!("\"context\":\"{context}\"")), "{json}");
    }
}

#[cfg(any(feature = "llvm-target-core64", feature = "llvm-target-all"))]
#[test]
fn nested_control_flow_retains_truthy_conditions_and_for_initializers() {
    let dir = directory();
    let source = dir.join("control.wave");
    std::fs::write(
        &source,
        r#"
fun main() -> i32 {
    var total: i32 = 0;
    for (var i: i32 = 0; i < 4; i += 1) {
        if (i == 0) { continue; }
        else if (i == 1) { total += 2; }
        else { var n: i32 = i; while (n) { total += n; n -= 1; } }
    }
    for (j: i32 = 0; j < 3; j += 1) { total += j; }
    var k: i32 = 0;
    for (k = 0; k < 9; k += 1) { if (k == 2) { break; } total += 1; }
    if (total != 16) { return 1; }
    return 0;
}
"#,
    )
    .unwrap();
    successful(&native_wave(&[
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--run"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]));
}

#[test]
fn lexer_escape_diagnostics_keep_labels_and_point_at_the_escape() {
    let dir = directory();
    let source = dir.join("escape.wave");
    let target = frontend_target();
    for (escape, label) in [
        ("\\q", "unsupported escape sequence"),
        ("\\xGG", "hex escapes must be exactly two hexadecimal digits"),
    ] {
        let text = format!("// 한글\r\nfun main() {{ \"앞{escape}\"; }}\r\n");
        std::fs::write(&source, &text).unwrap();
        let human = check(&source, &target);
        assert!(!human.status.success());
        let human = String::from_utf8_lossy(&human.stderr);
        assert!(human.contains(label), "{human}");
        let output = wave(&[
            OsStr::new("--error-format=json"),
            OsStr::new("check"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(&target),
        ]);
        assert!(!output.status.success());
        assert!(output.stdout.is_empty());
        let json = String::from_utf8_lossy(&output.stderr);
        assert!(json.contains(&format!("\"label\":\"{label}\"")), "{json}");
        let start = text.find(escape).unwrap();
        assert!(
            json.contains(&format!("\"start\":{start},\"end\":{}", start + escape.len())),
            "{json}"
        );
        assert!(json.contains("\"code\":\"E1004\""), "{json}");
    }
}

#[test]
fn truncated_blocks_show_the_eof_source_line_and_caret() {
    let dir = directory();
    let source = dir.join("eof.wave");
    let target = frontend_target();
    for newline in ["\n", "\r\n"] {
        std::fs::write(&source, format!("fun main() {{{newline}")).unwrap();
        let output = Command::new(env!("CARGO_BIN_EXE_wavec"))
            .args(["check"])
            .arg(&source)
            .args(["--target", &target])
            .env("NO_COLOR", "1")
            .output()
            .unwrap();
        assert!(!output.status.success());
        let human = String::from_utf8(output.stderr).unwrap();
        assert!(human.contains("eof.wave:2:1"), "{human}");
        assert!(human.contains("  2 | \n    | ^"), "{human}");
        assert!(!human.contains('\r'), "{human}");
    }
}

#[test]
fn imported_expression_errors_keep_their_location_in_both_output_formats() {
    let dir = directory();
    let source = dir.join("main.wave");
    let library = dir.join("broken.wave");
    let target = frontend_target();
    std::fs::write(&source, "import(\"./broken\"); fun main() {}\n").unwrap();
    for (marked, expected, context) in [
        ("item.@;", "identifier", "member access"),
        ("pkg::@;", "identifier", "qualified name"),
        ("call(1 @;", "')'", "function call"),
        ("item.method(1 @;", "')'", "method call"),
        ("items[0 @;", "']'", "index expression"),
        ("(1 @;", "')'", "grouped expression"),
        ("[1 @;", "']'", "array literal"),
        ("Point { x: 1 @y: 2 };", "',' or '}'", "struct literal"),
    ] {
        let marked = format!("// 한글\r\nfun broken() {{\r\n    {marked}\r\n}}");
        let start = marked.find('@').unwrap();
        std::fs::write(&library, marked.replace('@', "")).unwrap();
        let human = check(&source, &target);
        let json = wave(&[
            OsStr::new("--error-format=json"),
            OsStr::new("check"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(&target),
        ]);
        for output in [&human, &json] {
            assert!(!output.status.success());
            assert!(output.stdout.is_empty());
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains(&format!("expected {expected} in {context}")), "{stderr}");
            assert!(stderr.contains("broken.wave"), "{stderr}");
            assert!(stderr.contains("E2001"), "{stderr}");
        }
        let stderr = String::from_utf8_lossy(&json.stderr);
        assert!(stderr.contains(&format!("\"start\":{start},")), "{stderr}");
        assert!(stderr.contains(&format!("\"context\":\"{context}\"")), "{stderr}");
    }
}

#[cfg(any(feature = "llvm-target-core64", feature = "llvm-target-all"))]
#[test]
fn generic_aggregates_and_postfix_chains_preserve_runtime_values() {
    let dir = directory();
    let source = dir.join("expressions.wave");
    std::fs::write(
        &source,
        r#"
struct Box<T> { value: T; fun get(self: ptr<Box<T>>) -> T { return self.value; } }
struct Values {
    items: array<i32, 3>;
    fun at(self: ptr<Values>, index: i32) -> i32 { return self.items[index]; }
    fun first(self: ptr<Values>) -> Box<i32> { return Box<i32> { value: self.items[0] }; }
}
fun boxed<T>(value: T) -> Box<T> { return Box<T> { value: value }; }
fun values() -> Values { return Values { items: [2, 5, 9] }; }
fun counted(count: ptr<i32>) -> Box<i32> { deref count += 1; return boxed<i32>(7); }
fun main() -> i32 {
    var holder: Box<Values> = boxed<Values>(values());
    var left: i32 = 0;
    var right: i32 = 0;
    left = right = (&holder.value).at(1) + boxed<i32>(3).value * 2;
    if (left != 11 || right != 11) { return 1; }
    var old: i32 = holder.value.items[1]++;
    var next: i32 = ++holder.value.items[2];
    if (old != 5 || next != 10 || (&holder.value).at(1) != 6) { return 2; }
    var count: i32 = 0;
    var result: i32 = counted(&count).value;
    var number: Box<i32> = boxed<i32>(3);
    if (result != 7 || count != 1 || (&number).get() != 3 || (&holder.value).first().value != 2) { return 3; }
    return 0;
}
"#,
    )
    .unwrap();
    successful(&native_wave(&[
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--run"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]));
}

#[test]
fn eof_diagnostics_link_the_unmatched_opener_in_main_and_imported_sources() {
    let dir = directory();
    let source = dir.join("main.wave");
    let library = dir.join("unclosed.wave");
    let target = frontend_target();
    for (marked, closer) in
        [("fun f@(", ")"), ("fun f() @{\r\n", "}"), ("fun f() { call(@[1,", "]")]
    {
        let marked = format!("// 한글\r\n{marked}");
        let opener = marked.find('@').unwrap();
        let text = marked.replace('@', "");
        std::fs::write(&library, &text).unwrap();
        std::fs::write(&source, "import(\"./unclosed\"); fun main() {}").unwrap();
        for entry in [&source, &library] {
            let output = wave(&[
                OsStr::new("--error-format=json"),
                OsStr::new("check"),
                entry.as_os_str(),
                OsStr::new("--target"),
                OsStr::new(&target),
            ]);
            assert!(!output.status.success());
            assert!(output.stdout.is_empty());
            let json = utils::wson::parse_json(&String::from_utf8(output.stderr).unwrap()).unwrap();
            let error = json.get("error").unwrap();
            assert_eq!(error.get_str("code"), Some("E2001"));
            assert_eq!(error.get("span").unwrap().get_num("start"), Some(text.len() as f64));
            let related = error.get_arr("related").unwrap();
            assert_eq!(related.len(), 1);
            assert!(related[0]
                .get_str("message")
                .unwrap()
                .contains(&format!("expected '{closer}'")));
            assert_eq!(related[0].get("span").unwrap().get_num("start"), Some(opener as f64));
            assert!(related[0]
                .get("span")
                .unwrap()
                .get_str("file")
                .unwrap()
                .ends_with("unclosed.wave"));
            let human = check(entry, &target);
            assert!(!human.status.success());
            let human = String::from_utf8_lossy(&human.stderr);
            assert!(human.contains(&format!("expected '{closer}'")), "{human}");
            assert!(human.contains("opened here"), "{human}");
            assert!(human.matches('^').count() >= 2, "{human}");
        }
    }
}

#[cfg(any(feature = "llvm-target-core64", feature = "llvm-target-all"))]
#[test]
fn generic_methods_and_nested_method_receivers_execute_once() {
    let dir = directory();
    let source = dir.join("methods.wave");
    std::fs::write(&source, r#"
struct Box<T> {
    value: T;
    fun pick<U>(self: ptr<Box<T>>, other: U) -> U { return other; }
    fun get(self: ptr<Box<T>>) -> T { return self.value; }
}
struct Counter { value: i32; }
proto Counter {
    fun pick<T>(self: ptr<Counter>, other: T) -> T { return other; }
    fun recur<T>(self: ptr<Counter>, value: T, n: i32) -> T {
        if (n == 0) { return value; }
        return self.recur<T>(value, n - 1);
    }
}
fun counted(calls: ptr<i32>) -> i32 { deref calls += 1; return 4; }
fun add(value: i32, other: i32) -> i32 { return value + other; }
fun receiver(calls: ptr<i32>, value: ptr<Counter>) -> ptr<Counter> { deref calls += 1; return value; }
fun main() -> i32 {
    var calls: i32 = 0;
    if (counted(&calls).add(3).add(2) != 9 || calls != 1) { return 1; }
    var box: Box<i32> = Box<i32> { value: 11 };
    var counter: Counter = Counter { value: 7 };
    var picked: Box<i32> = receiver(&calls, &counter).pick<Box<i32>>(box);
    if (calls != 2 || (&picked).get() != 11) { return 2; }
    if ((&box).pick<ptr<Counter>>(&counter).recur<i32>(17, 5) != 17) { return 3; }
    var wide: i64 = 99;
    if ((&box).pick(wide) != 99 || (&counter).pick(5) != 5) { return 4; }
    return 0;
}
"#).unwrap();
    successful(&native_wave(&[
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--run"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]));
}

#[test]
fn malformed_declarations_and_asm_are_clean_json_in_imported_sources() {
    let dir = directory();
    let library = dir.join("broken.wave");
    let source = dir.join("main.wave");
    std::fs::write(&source, "import(\"./broken\"); fun main() {}\n").unwrap();
    for marked in [
        "pub type Item @i32;",
        "enum E -> @{}",
        "pub variant V { X(i32 @i64) }",
        "fun f() { asm { in(@123) value } }",
        "fun f() { var x: i32 = asm { out(rax) @123 }; }",
    ] {
        let start = marked.find('@').unwrap();
        std::fs::write(&library, marked.replace('@', "")).unwrap();
        let out = wave(&[
            OsStr::new("--error-format=json"),
            OsStr::new("check"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(&frontend_target()),
        ]);
        assert!(!out.status.success());
        assert!(out.stdout.is_empty(), "{:?}", out.stdout);
        let stderr = String::from_utf8(out.stderr).unwrap();
        assert_eq!(stderr.lines().count(), 1, "{stderr}");
        assert!(
            stderr.contains("E2001")
                && stderr.contains("broken.wave")
                && stderr.contains(&format!("\"start\":{start},")),
            "{stderr}"
        );
    }
}

#[cfg(any(feature = "llvm-target-x86", feature = "llvm-target-all"))]
#[test]
fn backend_errors_keep_imported_spans_without_panic_message_guessing() {
    let dir = directory();
    let library = dir.join("assembly.wave");
    let source = dir.join("main.wave");
    let text = "pub fun helper() { asm { in(\"invalid_register\") 1 } }";
    std::fs::write(&library, text).unwrap();
    std::fs::write(&source, "import(\"./assembly\")::{helper}; fun main() { helper(); }").unwrap();
    for format in ["human", "json"] {
        let output = wave(&[
            OsStr::new(&format!("--error-format={format}")),
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target=x86_64-unknown-linux-gnu"),
            OsStr::new("--emit=obj"),
            OsStr::new("--out-dir"),
            dir.as_os_str(),
        ]);
        assert!(!output.status.success());
        assert!(output.stdout.is_empty());
        let diagnostic = String::from_utf8(output.stderr).unwrap();
        assert!(
            diagnostic.contains("E3401")
                && diagnostic.contains("assembly.wave")
                && diagnostic.contains("lowering-validation"),
            "{diagnostic}"
        );
        assert!(!diagnostic.contains("panic") && !diagnostic.contains("inferred"), "{diagnostic}");
        if format == "json" {
            assert_eq!(diagnostic.lines().count(), 1);
        }
        assert!(!dir.join("main.o").exists());
    }
}

#[cfg(any(feature = "llvm-target-core64", feature = "llvm-target-all"))]
#[test]
fn missing_linker_keeps_phase_and_previous_artifact() {
    let dir = directory();
    let source = dir.join("entry.wave");
    std::fs::write(&source, "fun main() -> i32 { return 0; }").unwrap();
    let executable = dir.join("preserved.exe");
    std::fs::write(&executable, b"previous artifact").unwrap();
    let linker = format!("-Clinker={}", dir.join("missing-linker").display());
    let output = wave(&[
        OsStr::new("--error-format=json"),
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new(&linker),
        OsStr::new("-o"),
        executable.as_os_str(),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]);
    assert_eq!(output.status.code(), Some(3));
    assert!(output.stdout.is_empty());
    let diagnostic = String::from_utf8(output.stderr).unwrap();
    assert!(
        diagnostic.contains("\"kind\":\"external-tool-missing\"")
            && diagnostic.contains("\"phase\":\"linking\""),
        "{diagnostic}"
    );
    assert_eq!(std::fs::read(&executable).unwrap(), b"previous artifact");
    assert!(!std::fs::read_dir(&dir).unwrap().any(|entry| entry
        .unwrap()
        .file_name()
        .to_string_lossy()
        .starts_with(".wave-output-")));
}

#[test]
fn terminal_carets_use_display_cells_without_changing_json_locations() {
    let dir = directory();
    let path = dir.join("display.wave");
    let source = "fun main() {\n\tprintln(\"한e\u{301}\"); @\n}\n";
    std::fs::write(&path, source).unwrap();
    let target = frontend_target();
    let human = Command::new(env!("CARGO_BIN_EXE_wavec"))
        .env("NO_COLOR", "1")
        .arg("check")
        .arg(&path)
        .args(["--target", &target])
        .output()
        .unwrap();
    assert!(!human.status.success());
    let text = String::from_utf8(human.stderr).unwrap();
    assert!(text.contains("  2 |     println(\"한e\u{301}\"); @"), "{text}");
    assert!(text.contains(&format!("    | {}^", " ".repeat(20))), "{text}");
    let json = wave(&[
        OsStr::new("check"),
        path.as_os_str(),
        OsStr::new("--target"),
        OsStr::new(&target),
        OsStr::new("--error-format=json"),
    ]);
    let text = String::from_utf8(json.stderr).unwrap();
    let parsed = utils::wson::parse_json(text.trim()).unwrap();
    let error = parsed.get("error").unwrap();
    assert_eq!(error.get_num("column"), Some(18.0));
    let span = error.get("span").unwrap();
    assert_eq!(span.get_num("start"), Some(source.find('@').unwrap() as f64));
    assert_eq!(span.get_num("end"), Some((source.find('@').unwrap() + 1) as f64));
    assert_eq!(span.get_num("column"), Some(18.0));
    assert_eq!(span.get_num("end_column"), Some(19.0));
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn expression_depth_is_bounded_across_check_build_and_ast() {
    let dir = directory();
    let target = frontend_target();
    let source = dir.join("depth.wave");
    for kind in ["group", "unary", "bitwise", "binary", "cast", "call", "array", "field", "struct"]
    {
        for depth in [128, 129, 5000] {
            let expr = match kind {
                "group" => format!("{}1{}", "(".repeat(depth), ")".repeat(depth)),
                "unary" => format!("{}1", "- ".repeat(depth)),
                "bitwise" => format!("{}1", "~ ".repeat(depth)),
                "binary" => format!("1{}", " + 1".repeat(depth)),
                "cast" => format!("1{}", " as i32".repeat(depth)),
                "call" => format!("{}1{}", "identity(".repeat(depth), ")".repeat(depth)),
                "array" => format!("{}1{}", "[".repeat(depth), "]".repeat(depth)),
                "field" => format!("v{}", ".next".repeat(depth)),
                "struct" => format!("{}1{}", "Item { value: ".repeat(depth), " }".repeat(depth)),
                _ => unreachable!(),
            };
            std::fs::write(&source, format!("fun identity(v: i32) -> i32 {{ return v; }}\nfun main() -> i32 {{ return {expr}; }}\n")).unwrap();
            for entry in ["check", "build", "ast"] {
                // These shapes are syntactically valid but deliberately untyped.
                if depth == 128 && ["array", "field", "struct"].contains(&kind) && entry != "ast" {
                    continue;
                }
                // MSVC's main thread has a smaller default stack than Linux.
                // Reduce the Linux child stack too, including debug builds, so
                // release-only checks cannot hide recursive-frame regressions.
                #[cfg(target_os = "linux")]
                let mut command = {
                    let mut command = Command::new("sh");
                    command
                        .args(["-c", "ulimit -s 1024 && exec \"$@\"", "depth-regression"])
                        .arg(env!("CARGO_BIN_EXE_wavec"));
                    command
                };
                #[cfg(not(target_os = "linux"))]
                let mut command = Command::new(env!("CARGO_BIN_EXE_wavec"));
                command.args([if entry == "ast" { "build" } else { entry }]).arg(&source).args([
                    "--target",
                    &target,
                    "--error-format=json",
                ]);
                if entry != "check" {
                    command.arg("--out-dir").arg(&dir);
                }
                if entry == "ast" {
                    command.args(["--emit=ast", "--ast-format=json"]);
                }
                if entry == "build" {
                    command.arg("--emit=obj");
                }
                let result = command.output().unwrap();
                if depth == 128 {
                    assert!(
                        result.status.success(),
                        "{kind}/{depth}/{entry}: status {:?}\n{}\n{}",
                        result.status,
                        String::from_utf8_lossy(&result.stdout),
                        String::from_utf8_lossy(&result.stderr)
                    );
                } else {
                    assert_eq!(
                        result.status.code(),
                        Some(1),
                        "{kind}/{depth}/{entry}: {:?}",
                        result
                    );
                    let diagnostic = String::from_utf8(result.stderr).unwrap();
                    assert!(
                        diagnostic.contains("expression nesting exceeds the maximum of 128 levels"),
                        "{kind}/{depth}/{entry}: {diagnostic}"
                    );
                    let json = utils::wson::parse_json(diagnostic.trim()).unwrap();
                    assert!(json.get("error").is_some(), "{diagnostic}");
                    assert!(diagnostic.contains("depth.wave"), "missing source: {diagnostic}");
                    assert!(diagnostic.contains("\"line\""), "missing location: {diagnostic}");
                }
            }
        }
    }
    // RAII depth counters must reset after a rejected parse in library users;
    // the command-line boundary should also remain usable after every failure.
    std::fs::write(&source, "fun main() -> i32 { return 0; }").unwrap();
    successful(&check(&source, &target));
    std::fs::remove_dir_all(dir).unwrap();
}
