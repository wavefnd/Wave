#![cfg(any(feature = "llvm-target-x86", feature = "llvm-target-all"))]
use hir::TypedProgram;
use llvm::backend::{link_objects, BackendOptions};
use llvm::codegen::ir::{emit_codegen_file, generate_ir, CodegenFileKind};
use llvm::diagnostic::CodegenPhase;
use std::sync::atomic::{AtomicU64, Ordering};

fn program(source: &str) -> TypedProgram {
    let tokens = lexer::Lexer::new_with_file(source, "backend-errors.wave").tokenize().unwrap();
    TypedProgram::lower(parser::parse_syntax_with_spans(&tokens).unwrap()).unwrap()
}

fn options() -> BackendOptions {
    BackendOptions { target: Some("x86_64-unknown-linux-gnu".into()), ..Default::default() }
}

fn directory() -> std::path::PathBuf {
    static NEXT: AtomicU64 = AtomicU64::new(0);
    let dir = std::env::temp_dir().join(format!(
        "wave-backend-errors-{}-{}",
        std::process::id(),
        NEXT.fetch_add(1, Ordering::Relaxed)
    ));
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

#[test]
fn target_and_asm_errors_return_without_unwinding() {
    let valid = program("fun main() -> i32 { return 0; }");
    let mut invalid = options();
    invalid.target = Some("invalid-target".into());
    assert_eq!(
        unsafe { generate_ir(&valid, "", &invalid) }.unwrap_err().phase,
        CodegenPhase::Target
    );
    invalid = options();
    invalid.code_model = Some("invalid-model".into());
    assert_eq!(
        unsafe { generate_ir(&valid, "", &invalid) }.unwrap_err().phase,
        CodegenPhase::Target
    );
    let invalid_asm = program("fun main() { asm { in(\"not_a_register\") 1 } }");
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| unsafe {
        generate_ir(&invalid_asm, "", &options())
    }));
    let error = result.expect("user-triggered failure must not unwind").unwrap_err();
    assert_eq!(error.phase, CodegenPhase::Validation);
    assert_eq!(error.operation, "inline assembly");
    assert_eq!(error.span.unwrap().file, "backend-errors.wave");
}

#[test]
fn failed_emission_preserves_old_output_and_cleans_staging_files() {
    let dir = directory();
    let output = dir.join("old.o");
    std::fs::write(&output, "previous artifact").unwrap();
    let invalid = program("fun main() { asm { clobber(\"not_a_register\") } }");
    assert!(unsafe {
        emit_codegen_file(&invalid, "", &options(), &output, CodegenFileKind::Object)
    }
    .is_err());
    assert_eq!(std::fs::read_to_string(&output).unwrap(), "previous artifact");
    let valid = program("fun main() -> i32 { return 0; }");
    let error = unsafe {
        emit_codegen_file(
            &valid,
            "",
            &options(),
            &dir.join("missing/out.o"),
            CodegenFileKind::Object,
        )
    }
    .unwrap_err();
    assert_eq!(error.phase, CodegenPhase::Emission);
    // Failed publication after LLVM has written the staging file also cleans it.
    let blocked = dir.join("directory.o");
    std::fs::create_dir(&blocked).unwrap();
    assert!(unsafe {
        emit_codegen_file(&valid, "", &options(), &blocked, CodegenFileKind::Object)
    }
    .is_err());
    assert!(blocked.is_dir());
    assert_eq!(std::fs::read_dir(&dir).unwrap().count(), 2);
}

#[test]
fn missing_linker_returns_a_typed_error_and_preserves_destination() {
    let dir = directory();
    let output = dir.join("old.exe");
    std::fs::write(&output, "previous artifact").unwrap();
    let mut backend = options();
    backend.linker = Some(dir.join("missing-linker").to_string_lossy().into_owned());
    let error = link_objects(&[], output.to_str().unwrap(), &[], &[], &backend).unwrap_err();
    assert_eq!(error.phase, CodegenPhase::Link);
    assert_eq!(std::fs::read_to_string(&output).unwrap(), "previous artifact");
    assert_eq!(std::fs::read_dir(&dir).unwrap().count(), 1);
}
