mod lexer;
mod parser;
mod error;

mod llvm_temporary;

use std::{env, fs, process, process::Command};
use std::path::Path;
use colorex::Colorize;
use lexer::{Lexer};
use crate::lexer::TokenType;
use crate::llvm_temporary::llvm_backend::compile_ir_to_machine_code;
use crate::llvm_temporary::llvm_codegen::generate_ir;
use crate::parser::{extract_body, extract_parameters, function};

const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("{} {}",
                  "Usage:".color("255,71,71"),
                  "wave <command> [arguments]");

        eprintln!("{}",
                  "Commands:".color("145,161,2"));

        eprintln!("  {}    {}",
                  "run <file>".color("38,139,235"),
                  "Execute the specified Wave file");

        eprintln!("  {}     {}",
                  "--version".color("38,139,235"),
                  "Show the CLI version");
        process::exit(1);
    }

    match args[1].as_str() {
        "--version" | "-V" => {
            println!("{}",
                     VERSION.color("2,161,47"));
            return;
        }
        "run" => unsafe {
            if args.len() < 3 {
                eprintln!("{} {}",
                          "Usage:".color("255,71,71"),
                          "wave run <file>");
                process::exit(1);
            }

            let file_path = &args[2];
            run_wave_file(file_path);
        }
        "help" => {
            println!("{}", "Options:".color("145,161,2"));
            println!("      {}       {}\n",
                     "run <file>".color("38,139,235"),
                     "Run the Wave code.");

            println!("{}", "Commands:".color("145,161,2"));
            println!("      {}    {}\n",
                     "-V, --version".color("38,139,235"),
                     "Verified the version of the Wave compiler.");
            return;
        }
        _ => {
            eprintln!("{} {}",
                      "Unknown command:".color("255,71,71"),
                      args[1]);
            eprintln!("{}",
                      "Use 'wave --version' or 'wave run <file>'".color("145,161,2"));
            process::exit(1);
        }
    }
}

/// Executes a Wave file by reading, parsing, compiling, and running its contents.
/// 
/// This function performs the following steps:
/// 
/// 1. Reads the source code from the specified file path. If the file cannot be read,
///    it prints an error message and exits the program.
/// 2. Tokenizes the retrieved code and builds an abstract syntax tree (AST).
/// 3. Generates an intermediate representation (IR) from the AST and compiles it to machine code.
/// 4. Executes the compiled machine code and prints its output.
/// 
/// # Safety
/// 
/// This function is unsafe and must be called within an `unsafe` block since it executes
/// arbitrary generated machine code.
/// 
/// # Examples
///
/// ```rust
/// // Ensure that "example.wave" exists and contains valid code.
/// unsafe {
///     run_wave_file("example.wave");
/// }
/// ```
unsafe fn run_wave_file(file_path: &str) {
    let code = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file {}: {}", file_path, err);
            process::exit(1);
        }
    };

    let mut lexer = Lexer::new(code.as_str());
    let tokens = lexer.tokenize();
    // eprintln!("Tokens: \n{:#?}", &tokens);

    // AST 생성
    let function_name = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::Identifier(_)))
        .map(|token| token.lexeme.clone())
        .unwrap_or_default();
    let params = extract_parameters(&tokens[..], 0, tokens.len());
    let mut peekable_tokens = tokens.iter().peekable();
    let body = extract_body(&mut peekable_tokens);
    let ast = function(function_name, params.clone(), body.clone());

    // println!("{}\n", code);

    // println!("AST:\n{:#?}", &ast);
    // dbg!("{},", &params);
    // dbg!("{},", &body);

    let ir = generate_ir(&ast);
    // println!("Generated LLVM IR:\n{}", ir);

    let path = Path::new(file_path);
    let file_stem = path.file_stem().unwrap().to_str().unwrap();

    let machine_code_path = compile_ir_to_machine_code(&ir, file_stem);
    // eprintln!("Generated Machine Code at:\n{}", machine_code_path);

    if machine_code_path.is_empty() {
        eprintln!("Failed to generate machine code");
        return;
    }

    let output = Command::new(machine_code_path)
        .output()
        .expect("Failed to execute machine code");

    println!("{}", String::from_utf8_lossy(&output.stdout));
}


/*
fn run_wave_file(file_path: &str) {
    let code = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file {}: {}", file_path, err);
            process::exit(1);
        }
    };

    let mut lexer = Lexer::new(code.as_str());

    let tokens = lexer.tokenize();
    eprintln!("Tokens: \n{:#?}", &tokens);

    let function_name = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::Identifier(_)))
        .map(|token| token.lexeme.clone())
        .unwrap_or_default();

    let params = extract_parameters(&tokens[..], 0, tokens.len());

    let mut peekable_tokens = tokens.iter().peekable();

    let body = extract_body(&mut peekable_tokens);

    let ast = function(function_name, params, body);

    eprintln!("AST:\n{:#?}", &ast);
}
 */