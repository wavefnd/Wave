// SPDX-License-Identifier: MPL-2.0
//! Expression asm must preserve HIR conversions in both IR and native execution.
use super::*;

#[test]
#[cfg(any(feature = "llvm-target-x86", feature = "llvm-target-all"))]
fn typed_expression_asm_conversions_emit_ir_and_objects() {
    let dir = temp_case_dir("typed-asm-result");
    let source = write_wave(
        &dir,
        "asm-results.wave",
        r#"
fun main() -> i32 {
    var narrow: i32 = 0;
    var wide: i64 = asm { "mov eax, 7" out("eax") narrow clobber("nostack") };
    var sink: i64 = 0;
    var floating: f64 = asm { "mov rax, 7" out("rax") sink clobber("nostack") };
    var flag: bool = (asm { "mov rax, 7" out("rax") sink clobber("nostack") }) as bool;
    var sum: i64 = asm { "mov rax, 7" out("rax") sink clobber("nostack") } + 5;
    if (wide != 7 || floating != 7.0 || !flag || sum != 12) { return 1; }
    return 0;
}
"#,
    );
    for opt in ["-O0", "-O2"] {
        run_wavec([
            OsStr::new("check"),
            source.as_os_str(),
            OsStr::new("--target=x86_64-unknown-linux-gnu"),
        ]);
        let out = dir.join(opt);
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target=x86_64-unknown-linux-gnu"),
            OsStr::new("--emit=ir,obj"),
            OsStr::new(opt),
            OsStr::new("--out-dir"),
            out.as_os_str(),
        ]);
        let ir = fs::read_to_string(out.join("asm-results.ll")).unwrap();
        assert!(ir.contains("sext i32") && ir.contains("sitofp i64"), "{opt}: {ir}");
        assert!(ir.contains("inline_asm_expr"), "{opt}: {ir}");
        assert!(out.join("asm-results.o").is_file());
    }
}

#[test]
#[cfg(all(
    target_arch = "x86_64",
    target_os = "linux",
    any(feature = "llvm-target-x86", feature = "llvm-target-all")
))]
fn typed_expression_asm_conversions_execute_at_o0_and_o2() {
    let dir = temp_case_dir("typed-asm-run");
    let source = write_wave(
        &dir,
        "main.wave",
        r#"
fun main() -> i32 {
    var narrow: i32 = 0;
    var wide: i64 = asm { "mov eax, 7" out("eax") narrow clobber("nostack") };
    var sink: i64 = 0;
    var floating: f64 = asm { "mov rax, 7" out("rax") sink clobber("nostack") };
    var flag: bool = (asm { "mov rax, 7" out("rax") sink clobber("nostack") }) as bool;
    var sum: i64 = asm { "mov rax, 7" out("rax") sink clobber("nostack") } + 5;
    if (wide != 7 || floating != 7.0 || !flag || sum != 12) { return 1; }
    return 0;
}
"#,
    );
    for opt in ["-O0", "-O2"] {
        let output = wavec_command()
            .arg("build")
            .arg(&source)
            .arg(opt)
            .arg("--run")
            .arg("--out-dir")
            .arg(dir.join(opt))
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{opt}: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

#[test]
#[cfg(any(feature = "llvm-target-x86", feature = "llvm-target-all"))]
fn invalid_expression_asm_initializer_is_a_source_diagnostic() {
    let dir = temp_case_dir("typed-asm-errors");
    let source = write_wave(
        &dir,
        "wrong.wave",
        "fun main() { var sink: i64 = 0; var invalid: bool = asm { \"mov rax, 7\" out(\"rax\") sink }; }\n",
    );
    for emit in ["--emit=check", "--emit=ir"] {
        let output = run_wavec_raw([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target=x86_64-unknown-linux-gnu"),
            OsStr::new("--error-format=json"),
            OsStr::new(emit),
        ]);
        assert!(!output.status.success());
        let diagnostic = String::from_utf8_lossy(&output.stderr);
        assert!(diagnostic.contains("type mismatch"), "{diagnostic}");
        assert!(!diagnostic.contains("E9001"), "{diagnostic}");
    }
}
