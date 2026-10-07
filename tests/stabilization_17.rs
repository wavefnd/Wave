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
            "wave-stabilization-{}-{}",
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
fn declaration_diagnostics_preserve_the_offending_token() {
    let case = Case::new();
    for (source, token, message) in [
        ("fun f(x: i24) {}", "i24", "type"),
        ("fun f() -> i24 {}", "i24", "type"),
        ("fun f(x i32) {}", "i32", "':'"),
        ("fun f(x: i32 y: i32) {}", "y:", "','"),
        ("fun f(x: i32, x: i32) {}", "x: i32)", "duplicate parameter"),
        ("import(\"./missing\" as );", ")", "identifier"),
        ("pub import(\"./missing\")::{ };", "}", "identifier"),
        ("import(\"./missing\")::{x y};", "y", "','"),
        ("import(\"./missing\" as m)::{x};", "::", "aliases"),
        ("import(123);", "123", "string literal"),
        ("import(\"\\xFF\");", "\"\\xFF\"", "UTF-8"),
        ("struct S { value: i24; }", "i24", "type"),
        ("struct S { x i32; }", "i32", "':'"),
        ("struct S { value: i32 other: i32; }", "other", "';'"),
        ("struct S { 123: i32; }", "123", "identifier"),
    ] {
        case.source(source);
        let column = source.find(token).unwrap() + 1;
        for format in ["human", "json"] {
            let o = case
                .command()
                .args(["check", "case.wave", &format!("--error-format={format}")])
                .output()
                .unwrap();
            let err = String::from_utf8_lossy(&o.stderr);
            assert!(!o.status.success() && o.stdout.is_empty(), "{source}: {o:?}");
            assert!(err.contains(message), "{source}: {err}");
            if format == "json" {
                let json = utils::wson::parse_json(err.trim()).unwrap();
                assert_eq!(
                    json.get("error").unwrap().get_num("column"),
                    Some(column as f64),
                    "{source}: {err}"
                );
            } else {
                assert!(err.contains(&format!(":1:{column}")), "{source}: {err}");
            }
        }
    }
}
#[test]
fn cli_rejects_invalid_debug_modes_and_empty_dependency_roots() {
    let case = Case::new();
    case.source("fun main() {}");
    for args in [
        vec!["--debug-wave=toknes"],
        vec!["--debug-wave", "ir,toknes"],
        vec!["--debug-wave=tokens,toknes"],
        vec!["--debug-wave", "toknes"],
        vec!["--dep-root="],
        vec!["--dep-root=  "],
        vec!["--dep-root", ""],
        vec!["--dep-root", "  "],
    ] {
        let o = case.command().args(args).args(["check", "case.wave"]).output().unwrap();
        assert_eq!(o.status.code(), Some(2), "{o:?}");
    }
    for args in [
        vec!["--debug-wave=ir"],
        vec!["--debug-wave", "ir,mc"],
        vec!["--debug-wave=all"],
        vec!["--dep-root", "relative"],
        vec!["--dep-root=/tmp"],
    ] {
        ok(case.command().args(args).args(["check", "case.wave"]).output().unwrap());
    }
}
#[test]
fn numeric_constants_reject_invalid_shifts_and_casts() {
    let case = Case::new();
    for source in [
        "struct S { value: f64; } const s: S = S { value: 1e300 }; fun main() -> i32 { return s.value as i32; }",
        "const values: array<f64, 2> = [1.0, 1e300]; fun main() -> i32 { return values[1] as i32; }",
        "fun main() -> i32 { return 1 << 32; }",
        "fun main() -> i32 { var x: u8 = 1; return (x << 4294967296) as i32; }",
        "fun main() -> i32 { var x: u8 = 1; return (x >> -4294967296) as i32; }",
        "fun main() -> i32 { return 1 >> -1; }",
        "const N: u64 = 4294967296; fun main() -> i32 { var x: u8 = 1; return (x << N) as i32; }",
        "fun main() -> i32 { var x: u8 = 1; return (x << (256 as u64)) as i32; }",
        "fun main() -> i32 { return (1 << (16 + 16)); }",
        "fun main() -> i32 { return (1.0 / 0.0) as i32; }",
        "fun main() -> i32 { return (0.0 / 0.0) as i32; }",
        "fun main() -> i32 { return 2147483648.0 as i32; }",
        "fun main() -> i32 { var p: ptr<i32> = null; return (p as bool) as i32; }",
        "fun main() -> i32 { return 1 << true; }",
        "fun main() -> i32 { return 1 << 1.0; }",
        "fun main() -> i32 { var x: i32 = 1; match(x) { - => {} _ => {} } return 0; }",
        "const NEG: i32 = -1; fun main() -> i32 { var x: i32 = -1; match(x) { -1 => {} NEG => {} _ => {} } return 0; }",
    ] {
        case.source(source);
        let o = case.command().args(["check", "case.wave", "--error-format=json"]).output().unwrap();
        assert!(!o.status.success(), "accepted {source}");
        utils::wson::parse_json(String::from_utf8_lossy(&o.stderr).trim()).unwrap();
    }
}

fn environment_fixture(provider: &str, fixture: &str) -> String {
    // Git's Windows checkout uses CRLF; never silently leave the real import
    // beside the mock declaration when replacing the provider boundary.
    let provider = provider.replace("\r\n", "\n").replace('\r', "\n");
    let import = "import(\"std::sys::env\")::{\n    env_read,\n};";
    assert_eq!(provider.matches(import).count(), 1, "env_read provider import changed");
    format!("{}\n{fixture}", provider.replacen(import, "", 1))
}

#[test]
fn environment_fixture_replaces_the_provider_for_all_line_endings() {
    let case = Case::new();
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let provider = fs::read_to_string(root.join("std/env/environ.wave"))
        .unwrap()
        .replace("\r\n", "\n")
        .replace('\r', "\n");
    let fixture =
        fs::read_to_string(root.join("tests/fixtures/stabilization_17/environment.wave")).unwrap();
    for newline in ["\n", "\r\n", "\r"] {
        case.source(&environment_fixture(&provider.replace('\n', newline), &fixture));
        ok(case.command().args(["check", "case.wave"]).output().unwrap());
    }
}

#[test]
fn checked_numeric_runtime_and_byte_apis() {
    if !native() {
        return;
    }
    let case = Case::new();
    for name in ["numbers", "bytes", "environment"] {
        let mut source = fs::read_to_string(
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .join(format!("tests/fixtures/stabilization_17/{name}.wave")),
        )
        .unwrap();
        if name == "environment" {
            let provider = fs::read_to_string(
                Path::new(env!("CARGO_MANIFEST_DIR")).join("std/env/environ.wave"),
            )
            .unwrap();
            source = environment_fixture(&provider, &source);
        }
        case.run(&source);
    }
}
#[test]
fn runtime_invalid_numeric_operations_trap_before_conversion() {
    if !native() {
        return;
    }
    let case = Case::new();
    for source in [
        "fun main() -> i32 { var x: u8 = 1; var n: u64 = 256; return (x << n) as i32; }",
        "fun main() -> i32 { var x: u1024 = 1; var n: i8 = -1; return (x >> n) as i32; }",
        "fun main() -> i32 { var x: f64 = 1e300; return x as i32; }",
        "fun main() -> i32 { var x: f64 = -1.0; return (x as u64) as i32; }",
        "fun main() -> i32 { var z: f64 = 0.0; var n: f64 = z / z; return (n as i64) as i32; }",
    ] {
        case.source(source);
        for opt in ["-O0", "-O2"] {
            ok(case.build(opt));
            let ir = fs::read_to_string(case.0.join("case.ll")).unwrap();
            assert!(ir.contains("llvm.trap"), "{ir}");
            let status = Command::new(case.0.join("case.exe")).status().unwrap();
            assert!(!status.success(), "{source} {opt}");
            #[cfg(unix)]
            {
                use std::os::unix::process::ExitStatusExt;
                assert!(status.signal().is_some(), "{status}");
            }
        }
    }
}

#[test]
fn shifts_and_float_conversions_cover_every_integer_width() {
    if !native() {
        return;
    }
    let case = Case::new();
    for bits in [8, 16, 32, 64, 128, 256, 512, 1024] {
        for signed in [false, true] {
            let ty = format!("{}{bits}", if signed { "i" } else { "u" });
            let top = 2f64.powi(bits - i32::from(signed));
            let below = if top.is_infinite() {
                f64::MAX
            } else {
                f64::from_bits(top.to_bits() - 1).trunc()
            };
            let negative = if signed {
                "var neg: f64 = -1.9; if ((neg as TYPE) != -1) { return 4; }".replace("TYPE", &ty)
            } else {
                String::new()
            };
            case.run(&format!("fun main() -> i32 {{ var x: {ty} = 1; var n: u64 = {}; var shifted: {ty} = x << n; var zero: u8 = 0; if ((shifted >> zero) != shifted) {{ return 1; }} var count: u8 = 1; var two: {ty} = x << count; if (two != 2) {{ return 2; }} var edge: f64 = {below:e}; var narrow: f32 = 3.9; if ((narrow as {ty}) != 3) {{ return 5; }} var converted: {ty} = edge as {ty}; if (converted != {below:.0}) {{ return 3; }} {negative} return 0; }}", bits - 1));
        }
    }
}

#[test]
fn webassembly_checked_numeric_runtime() {
    if std::env::var_os("WAVE_RUN_WASM_RUNTIME_TESTS").is_none() {
        return;
    }
    let case = Case::new();
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/stabilization_17");
    for target in ["wasm32-unknown-unknown", "wasm64-unknown-unknown"] {
        if llvm::codegen::target::target_spec_for_triple(target).is_none() {
            continue;
        }
        for opt in ["-O0", "-O2"] {
            let object = case.0.join("numeric.o");
            let module = case.0.join("numeric.wasm");
            ok(case
                .command()
                .arg("build")
                .arg(root.join("wasm.wave"))
                .args(["--target", target, opt, "--emit=obj", "-o"])
                .arg(&object)
                .output()
                .unwrap());
            let mut link = Command::new("wasm-ld");
            if target.starts_with("wasm64") {
                link.arg("-mwasm64");
            }
            link.arg("--no-entry");
            for name in ["shift", "signed_shift", "signed_cast", "unsigned_cast", "truth", "wide"] {
                link.arg(format!("--export={name}"));
            }
            ok(link.arg(object).arg("-o").arg(&module).output().unwrap());
            // An external deadline also bounds a broken target runtime.
            ok(Command::new("python3").current_dir(env!("CARGO_MANIFEST_DIR")).args(["-c", "import subprocess,sys; subprocess.run(['node','--experimental-wasm-memory64',sys.argv[1],sys.argv[2]],check=True,timeout=20)"]).arg(root.join("wasm.cjs")).arg(module).output().unwrap());
        }
    }
}

#[cfg(target_arch = "x86_64")]
#[test]
fn darwin_syscall_secondary_return_does_not_preserve_the_third_argument() {
    if !native() {
        return;
    }
    let provider = fs::read_to_string(
        Path::new(env!("CARGO_MANIFEST_DIR")).join("std/sys/macos/amd64/syscall.wave"),
    )
    .unwrap();
    assert_eq!(provider.matches("\"syscall\\n").count(), 7);
    // Model the XNU register boundary without executing Darwin syscalls on
    // another OS: echo the third input in rax, then overwrite rdx with zero.
    let provider = provider.replace("\"syscall\\n", "\"mov rax, rdx\\nxor edx, edx\\n");
    let mut body = String::from("fun main() -> i32 {\n");
    for count in 3..=6 {
        let mut args = vec![54, 1, 2, 7];
        args.extend(4..=count);
        let args = args.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ");
        body.push_str(&format!("if (syscall{count}({args}) != 7 || syscall{count}({args}) != 7) {{ return {count}; }}\n"));
    }
    body.push_str("return 0; }");
    Case::new().run(&format!("{provider}\n{body}"));
}
