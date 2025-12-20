use crate::commands::DebugFlags;
use ::error::*;
use ::parser::ast::{ASTNode, StatementNode};
use ::parser::import::local_import;
use ::parser::*;
use lexer::Lexer;
use llvm_temporary::llvm_temporary::llvm_backend::*;
use llvm_temporary::llvm_temporary::llvm_codegen::*;
use std::collections::HashSet;
use std::path::Path;
use std::{fs, process, process::Command};

pub(crate) unsafe fn run_wave_file(file_path: &Path, opt_flag: &str, debug: &DebugFlags) {
    let code = match fs::read_to_string(file_path) {
        Ok(c) => c,
        Err(_) => {
            WaveError::new(
                WaveErrorKind::FileReadError(file_path.display().to_string()),
                format!("failed to read file `{}`", file_path.display()),
                file_path.display().to_string(),
                0,
                0,
            )
            .with_help("check if the file exists and you have permission to read it")
            .display();
            process::exit(1);
        }
    };

    let mut lexer = Lexer::new(&code);
    let tokens = lexer.tokenize();

    let ast = match parse(&tokens) {
        Some(ast) => ast,
        None => {
            WaveError::new(
                WaveErrorKind::SyntaxError("failed to parse Wave code".to_string()),
                "failed to parse Wave code",
                file_path.display().to_string(),
                0,
                0,
            )
            .display();
            process::exit(1);
        }
    };

    if debug.tokens {
        println!("\n===== Tokens =====");
        for token in &tokens {
            println!("{:?}", token);
        }
    }

    if debug.ast {
        println!("\n===== AST =====\n{:#?}", ast);
    }

    let ir = generate_ir(&ast);

    if debug.ir {
        println!("\n===== LLVM IR =====\n{}", ir);
    }

    let file_stem = file_path.file_stem().unwrap().to_str().unwrap();
    let object_patch = compile_ir_to_object(&ir, file_stem, opt_flag);

    if debug.mc {
        println!("\n===== MACHINE CODE PATH =====");
        println!("{}", object_patch);
    }

    if debug.hex {
        println!("\n===== HEX DUMP =====");
        let data = fs::read(&object_patch).unwrap();
        for (i, b) in data.iter().enumerate() {
            if i % 16 == 0 {
                print!("\n{:04x}: ", i);
            }
            print!("{:02x} ", b);
        }
        println!();
    }

    let exe_patch = format!("target/{}", file_stem);

    let link_libs: Vec<String> = Vec::new();
    let link_lib_paths: Vec<String> = Vec::new();

    link_objects(
        &[object_patch.clone()],
        &exe_patch,
        &link_libs,
        &link_lib_paths,
    );

    let output = Command::new(&exe_patch).output();
    println!("{}", String::from_utf8_lossy(&output.unwrap().stdout));
}

pub(crate) unsafe fn img_wave_file(file_path: &Path) {
    let code = fs::read_to_string(file_path).expect("Failed to read file");

    let mut lexer = Lexer::new(&code);
    let tokens = lexer.tokenize();

    let mut ast = parse(&tokens).expect("Failed to parse Wave code");

    let file_path = Path::new(file_path);
    let base_dir = file_path
        .canonicalize()
        .ok()
        .and_then(|p| p.parent().map(|p| p.to_path_buf()))
        .unwrap_or_else(|| Path::new(".").to_path_buf());

    let mut already_imported = HashSet::new();
    let mut extended_ast = vec![];

    for node in &ast {
        if let ASTNode::Statement(StatementNode::Import(path)) = node {
            match local_import(&path, &mut already_imported, &base_dir, None) {
                Ok(mut imported_nodes) => {
                    extended_ast.append(&mut imported_nodes);
                }
                Err(err) => {
                    err.display();
                    process::exit(1);
                }
            }
        } else {
            extended_ast.push(node.clone());
        }
    }

    ast = extended_ast;

    // println!("{}\n", code);
    // for token in tokens {
    //     println!("{:?}", token);
    // }
    // println!("AST:\n{:#?}", ast);

    let ir = generate_ir(&ast);
    let path = Path::new(file_path);
    let file_stem = path.file_stem().unwrap().to_str().unwrap();
    let machine_code_path = compile_ir_to_img_code(&ir, file_stem);

    if machine_code_path.is_empty() {
        eprintln!("Failed to generate machine code");
        return;
    }

    Command::new("qemu-system-x86_64")
        .args(&["-drive", &format!("file={},format=raw", machine_code_path)])
        .status()
        .expect("Failed to run QEMU");

    // println!("Generated LLVM IR:\n{}", ir);
}
