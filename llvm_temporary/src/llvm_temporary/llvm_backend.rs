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

pub fn compile_ir_to_img_code(ir: &str, file_stem: &str) -> String {
    let target_dir = Path::new("target");
    if !target_dir.exists() {
        fs::create_dir_all(target_dir).expect("Unable to create target directory");
    }

    let ll_ir = "target/temp.ll";
    let o_file = "target/boot.o";
    let bin_file = "target/boot.bin";
    let img_file = format!("target/{}.img", file_stem);

    fs::write(ll_ir, ir).expect("Unable to write IR to file");

    let o_output = Command::new("llc")
        .arg("-march=x86")
        .arg("-mattr=+16bit-mode")
        .arg("-filetype=obj")
        .arg(ll_ir)
        .arg("-o")
        .arg(o_file)
        .status()
        .expect("Failed to execute llc");

    if !o_output.success() {
        eprintln!("llc failed");
        return String::new();
    }

    let elf_output = Command::new("ld")
        .arg("-m")
        .arg("elf_i386")
        .arg("-Ttext")
        .arg("0x7c00")
        .arg("--oformat")
        .arg("binary")
        .arg(o_file)
        .arg("-o")
        .arg(bin_file)
        .status()
        .expect("Failed to execute ld");

    if !elf_output.success() {
        eprintln!("ld failed");
        return String::new();
    }

    let mut bin_data = fs::read(bin_file).expect("Unable to read binary");
    if bin_data.len() < 512 {
        bin_data.resize(512, 0);
    }
    bin_data[510] = 0x55;
    bin_data[511] = 0xAA;
    fs::write(&bin_file, &bin_data).expect("Unable to write binary");

    let dd_status = Command::new("dd")
        .arg("if=target/boot.bin")
        .arg(&format!("of={}", img_file))
        .arg("bs=512")
        .arg("count=1")
        .arg("conv=notrunc")
        .status()
        .expect("Failed to execute dd");
    if !dd_status.success() {
        eprintln!("dd failed");
        return String::new();
    }

    println!("[+] Image created: {}", img_file);

    img_file
}