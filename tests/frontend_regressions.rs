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
    Command::new(env!("CARGO_BIN_EXE_wavec"))
        .args(args)
        .output()
        .unwrap()
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
    wave(&[
        OsStr::new("check"),
        path.as_os_str(),
        OsStr::new("--target"),
        OsStr::new(target),
    ])
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
    let inactive_arch = if active_arch == "wasm64" {
        "arm64"
    } else {
        "wasm64"
    };
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
    assert!(
        stderr.contains(&format!("\"start\":{}", text.find("missing").unwrap())),
        "{stderr}"
    );
    assert!(
        stderr.contains(&format!("\"end\":{}", text.find("missing").unwrap() + 7)),
        "{stderr}"
    );
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
    successful(&wave(&[
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
    assert!(
        stderr.contains("u32") && !stderr.contains("panicked"),
        "{stderr}"
    );
    std::fs::write(
        &source,
        "fun word(x: isz) -> isz { return x; }\nfun main() {}\n",
    )
    .unwrap();
    for (target, bits) in [
        ("wasm32-unknown-unknown", 32),
        ("wasm64-unknown-unknown", 64),
    ] {
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
        assert!(
            ir.contains(&format!("define i{bits} @word(i{bits}")),
            "{ir}"
        );
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
    let value = ir
        .split("define i32 @value()")
        .nth(1)
        .unwrap()
        .split("\n}")
        .next()
        .unwrap();
    assert!(
        value.contains("call void @stop()") && value.contains("unreachable"),
        "{value}"
    );
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
