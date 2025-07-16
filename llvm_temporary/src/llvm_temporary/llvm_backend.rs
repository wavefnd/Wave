use std::process::Command;
use std::fs;
use std::path::Path;

pub fn compile_ir_to_machine_code(ir: &str, file_stem: &str) -> String {
    let target_dir = Path::new("target");
    if !target_dir.exists() {
        fs::create_dir_all(target_dir).expect("Unable to create target directory");
    }

    let ir_path = "target/temp.ll";
    fs::write(ir_path, ir).expect("Unable to write IR to file");

    let machine_code_path = format!("target/{}", file_stem);

    let output = Command::new("clang")
        .arg("-O2")
        .arg("-o")
        .arg(&machine_code_path)
        .arg(ir_path)
        .arg("-lc")
        .arg("-lm")
        .output()
        .expect("Failed to execute clang");

    if !output.status.success() {
        eprintln!("clang failed: {}", String::from_utf8_lossy(&output.stderr));
        return String::new();
    }

    machine_code_path
}