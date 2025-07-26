use std::{fs, process, process::Command};
use std::collections::HashSet;
use std::path::Path;
use lexer::Lexer;
use llvm_temporary::llvm_temporary::llvm_backend::*;
use llvm_temporary::llvm_temporary::llvm_codegen::*;
use ::parser::*;
use ::parser::ast::{ASTNode, StatementNode};
use ::parser::import::local_import;

pub(crate) unsafe fn run_wave_file(file_path: &Path) {
    let code = fs::read_to_string(file_path).expect("Failed to read file");

    let mut lexer = Lexer::new(&code);
    let tokens = lexer.tokenize();

    let mut ast = parse(&tokens).expect("Failed to parse Wave code");

    let file_path = Path::new(file_path);
    let base_dir = file_path.canonicalize()
        .ok()
        .and_then(|p| p.parent().map(|p| p.to_path_buf()))
        .unwrap_or_else(|| Path::new(".").to_path_buf());

    let mut already_imported = HashSet::new();
    let mut extended_ast = vec![];

    for node in &ast {
        if let ASTNode::Statement(StatementNode::Import(path)) = node {
            if !path.starts_with("std::") {
                match local_import(&path, &mut already_imported, &base_dir) {
                    Ok(mut imported_nodes) => {
                        extended_ast.append(&mut imported_nodes);
                    }
                    Err(err) => {
                        err.display();
                        process::exit(1);
                    }
                }
            }
        } else {
            extended_ast.push(node.clone());
        }
    }

    ast = extended_ast;

    // println!("{}\n", code);
    // println!("AST:\n{:#?}", ast);

    let ir = generate_ir(&ast);
    let path = Path::new(file_path);
    let file_stem = path.file_stem().unwrap().to_str().unwrap();
    let machine_code_path = compile_ir_to_machine_code(&ir, file_stem);

    if machine_code_path.is_empty() {
        eprintln!("Failed to generate machine code");
        return;
    }

    let output = Command::new(machine_code_path)
        .output()
        .expect("Failed to execute machine code");

    // println!("Generated LLVM IR:\n{}", ir);
    println!("{}", String::from_utf8_lossy(&output.stdout));
}

pub(crate) unsafe fn img_wave_file(file_path: &Path) {
    let code = fs::read_to_string(file_path).expect("Failed to read file");

    let mut lexer = Lexer::new(&code);
    let tokens = lexer.tokenize();

    let mut ast = parse(&tokens).expect("Failed to parse Wave code");

    let file_path = Path::new(file_path);
    let base_dir = file_path.canonicalize()
        .ok()
        .and_then(|p| p.parent().map(|p| p.to_path_buf()))
        .unwrap_or_else(|| Path::new(".").to_path_buf());

    let mut already_imported = HashSet::new();
    let mut extended_ast = vec![];

    for node in &ast {
        if let ASTNode::Statement(StatementNode::Import(path)) = node {
            if !path.starts_with("std::") {
                match local_import(&path, &mut already_imported, &base_dir) {
                    Ok(mut imported_nodes) => {
                        extended_ast.append(&mut imported_nodes);
                    }
                    Err(err) => {
                        err.display();
                        process::exit(1);
                    }
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
