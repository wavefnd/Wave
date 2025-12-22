use std::fs;
use std::path::Path;
use std::process::Command;

pub fn compile_ir_to_object(ir: &str, file_stem: &str, opt_flag: &str) -> String {
    let object_path = format!("target/{}.o", file_stem);

    let mut child = Command::new("clang")
        .arg(opt_flag)
        .arg("-c")
        .arg("-x")
        .arg("ir")
        .arg("-")
        .arg("-o")
        .arg(&object_path)
        .arg("-Wno-override-module")
        .stdin(std::process::Stdio::piped())
        .spawn()
        .expect("Failed to execute clang");

    use std::io::Write;
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(ir.as_bytes())
        .unwrap();

    let output = child.wait_with_output().unwrap();
    if !output.status.success() {
        eprintln!("clang failed: {}", String::from_utf8_lossy(&output.stderr));
        return String::new();
    }

    object_path
}

pub fn link_objects(objects: &[String], output: &str, libs: &[String], lib_paths: &[String]) {
    let mut cmd = Command::new("clang");

    for obj in objects {
        cmd.arg(obj);
    }

    for path in lib_paths {
        cmd.arg(format!("-L{}", path));
    }

    for lib in libs {
        cmd.arg(format!("-l{}", lib));
    }

    cmd.arg("-o").arg(output).arg("-lc").arg("-lm");

    let output = cmd.output().expect("Failed to link");
    if !output.status.success() {
        eprintln!("link failed: {}", String::from_utf8_lossy(&output.stderr));
    }
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
