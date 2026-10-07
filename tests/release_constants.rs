// SPDX-License-Identifier: MPL-2.0
use std::{
    fs,
    path::{Path, PathBuf},
    process::{Command, Output},
    sync::atomic::{AtomicU64, Ordering},
};
static NEXT: AtomicU64 = AtomicU64::new(0);
struct Case(PathBuf);
impl Case {
    fn new() -> Self {
        let p = std::env::temp_dir().join(format!(
            "wave-release-constants-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir_all(&p).unwrap();
        Self(p)
    }

    fn command(&self) -> Command {
        let mut c = Command::new(env!("CARGO_BIN_EXE_wavec"));
        c.current_dir(&self.0)
            .args(["--std-root"])
            .arg(Path::new(env!("CARGO_MANIFEST_DIR")).join("std"));
        c
    }

    fn source(&self, source: &str) {
        fs::write(self.0.join("case.wave"), source).unwrap();
    }

    fn build(&self, opt: &str) -> Output {
        self.command()
            .args(["build", "case.wave", opt, "--emit=ir,bin", "-o", "case.exe"])
            .output()
            .unwrap()
    }

    fn run(&self, source: &str) {
        self.source(source);
        for opt in ["-O0", "-O2"] {
            ok(self.build(opt));
            ok(Command::new(self.0.join("case.exe")).output().unwrap());
        }
    }
}
impl Drop for Case {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}
fn ok(o: Output) {
    assert!(
        o.status.success(),
        "{:?}\n{}\n{}",
        o.status,
        String::from_utf8_lossy(&o.stdout),
        String::from_utf8_lossy(&o.stderr)
    );
}

fn native() -> bool {
    let o = Command::new(env!("CARGO_BIN_EXE_wavec"))
        .args(["print", "default-target"])
        .output()
        .unwrap();
    llvm::codegen::target::target_spec_for_triple(String::from_utf8_lossy(&o.stdout).trim())
        .is_some()
}
#[test]
fn frontend_constant_expressions_match_runtime() {
    if !native() {
        return;
    }
    Case::new().run(include_str!("fixtures/release_constants/numeric.wave"));
}
#[test]
fn constant_errors_agree_between_check_and_build() {
    let case = Case::new();
    for (source, message) in [
        ("const N: i32 = 1 / 0;", "division or remainder by zero"),
        ("const N: i32 = 1 % 0;", "division or remainder by zero"),
        ("const N: i8 = (-128 as i8) / (-1 as i8);", "signed division overflows"),
        ("const N: i32 = 1 << 32;", "shift count"),
        ("const N: i32 = (0.0 / 0.0) as i32;", "NaN or infinity"),
        ("fun f() -> i32 { return 3; } const N: i32 = f();", "unsupported constant expression"),
        ("static N: i32 = 1; const M: i32 = N;", "unsupported constant expression"),
        ("const A: array<i32, 1> = [1]; const N: i32 = A[0];", "unsupported constant expression"),
        ("const P: ptr<i32> = null; const Q: ptr<i32> = P + 1;", "unsupported constant expression"),
        (
            "fun f() -> bool { return true; } const N: bool = false && f();",
            "unsupported constant expression",
        ),
    ] {
        case.source(&format!("{source} fun main() -> i32 {{ return 0; }}"));
        for command in ["check", "build"] {
            for format in ["human", "json"] {
                let out = case
                    .command()
                    .args([command, "case.wave", &format!("--error-format={format}")])
                    .output()
                    .unwrap();
                let err = String::from_utf8_lossy(&out.stderr);
                assert!(!out.status.success() && err.contains(message), "{source}: {out:?}");
                if format == "json" {
                    let value = utils::wson::parse_json(err.trim()).unwrap();
                    let error = value.get("error").unwrap();
                    assert_eq!(error.get_str("code"), Some("E3001"));
                    assert!(error.get_num("column").unwrap() > 0.0);
                }
            }
        }
    }
}
#[cfg(unix)]
#[test]
fn cli_preserves_signal_and_regular_child_status() {
    if !native() {
        return;
    }
    let case = Case::new();
    // SIGTERM has the same number on the supported Unix targets and does not dump core.
    case.source(
        "extern(c) fun raise(signal: i32) -> i32; fun main() -> i32 { raise(15); return 0; }",
    );
    for args in [vec!["run", "case.wave"], vec!["build", "case.wave", "--run"]] {
        let out = case.command().args(args).output().unwrap();
        assert_eq!(out.status.code(), Some(143), "{out:?}");
    }
    case.source("fun main() -> i32 { return 37; }");
    let out = case.command().args(["run", "case.wave"]).output().unwrap();
    assert_eq!(out.status.code(), Some(37), "{out:?}");
}

#[cfg(feature = "llvm-target-wasm")]
#[test]
fn webassembly_mmap_rejects_without_allocator_effects() {
    let case = Case::new();
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    for opt in ["-O0", "-O2"] {
        ok(case
            .command()
            .arg("build")
            .arg(root.join("tests/fixtures/release_constants/mmap.wave"))
            .args(["--target=wasm64-unknown-unknown", "--emit=obj", opt, "--out-dir"])
            .arg(&case.0)
            .output()
            .unwrap());
        if std::env::var_os("WAVE_RUN_WASM_RUNTIME_TESTS").is_some() {
            ok(Command::new("wasm-ld")
                .args(["-mwasm64", "--no-entry", "--export=verify"])
                .arg(case.0.join("mmap.o"))
                .arg("-o")
                .arg(case.0.join("mmap.wasm"))
                .output()
                .unwrap());
            ok(Command::new("node")
                .arg("--experimental-wasm-memory64")
                .arg(root.join("tests/fixtures/release_constants/mmap.cjs"))
                .arg(case.0.join("mmap.wasm"))
                .output()
                .unwrap());
        }
    }
}

#[test]
fn constants_preserve_every_integer_width_and_intermediate_conversions() {
    if !native() {
        return;
    }
    let case = Case::new();
    let mut source = String::new();
    let mut body = String::from("fun main() -> i32 {\n");
    for width in [8, 16, 32, 64, 128, 256, 512, 1024] {
        source.push_str(&format!(r#"
const U{width}: u{width} = (((1 as u{width}) << {top}) - 1) * (3 as u{width});
const S{width}: i{width} = ((-17 as i{width}) / (3 as i{width})) + ((-17 as i{width}) % (3 as i{width}));
const B{width}: u{width} = (~(13 as u{width}) & (255 as u{width})) ^ (7 as u{width});
const C{width}: i{width} = ((257 as i{width}) as u8) as i{width};
"#, top = width - 1));
        body.push_str(&format!(r#"
var a{width}: u{width} = (1 as u{width}) << {top};
var b{width}: i{width} = -17;
var c{width}: i{width} = 3;
var d{width}: u{width} = 13;
var e{width}: i{width} = 257 as i{width};
if (U{width} != (a{width} - 1) * 3 || S{width} != {division}
    || B{width} != ((~d{width} & (255 as u{width})) ^ 7) || C{width} != ((e{width} as u8) as i{width})) {{ return 1; }}
"#, top = width - 1,
            // Native i128 division may require an external compiler runtime.
            // Wider constant quotients are checked against their exact known value.
            division = if width <= 64 { format!("b{width} / c{width} + b{width} % c{width}") }
                else { format!("(-7 as i{width})") }));
    }
    source.push_str(&body);
    source.push_str("return 0; }");
    case.run(&source);
}
