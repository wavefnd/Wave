use std::{env, fmt, process};
use std::path::{Path, PathBuf};
use colorex::Colorize;
use wavec::{compile_and_img, compile_and_run};
use wavec::version_wave;

mod compiler_config;
use compiler_config::{CompilerConfig, OptimizationLevel};

#[derive(Debug)]
enum CliError {
    NotEnoughArgs,
    UnknownCommand(String),
    MissingArgument {
        command: &'static str,
        expected: &'static str,
    },
}

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CliError::NotEnoughArgs => {
                write!(f, "{} Please provide a command.", "Error:".color("255,71,71"))
            }
            CliError::UnknownCommand(cmd) => {
                write!(f, "{} Unknown command: '{}'", "Error:".color("255,71,71"), cmd)
            }
            CliError::MissingArgument { command, expected } => {
                write!(
                    f,
                    "{} Missing argument for command '{}'. Expected: {}",
                    "Error:".color("255,71,71"),
                    command,
                    expected
                )
            }
        }
    }
}

fn main() {
    if let Err(e) = run() {
        eprintln!("{}", e);
        print_usage();
        process::exit(1);
    }
}

fn run() -> Result<(), CliError> {
    let mut args = env::args();

    args.next();

    let command = args.next().ok_or(CliError::NotEnoughArgs)?;

    match command.as_str() {
        "--version" | "-V" => {
            version_wave();
        }
        "run" => {
            let first_arg = args.next().ok_or(CliError::MissingArgument {
                command: "run",
                expected: "<file> or --img <file>",
            })?;

            if first_arg == "--img" {
                let file_path_str = args.next().ok_or(CliError::MissingArgument {
                    command: "run",
                    expected: "<file>",
                })?;

                img_run(Path::new(&file_path_str))?;
            } else {
                handle_run(Path::new(&first_arg))?;
            }
        }
        "build" => {
            let file_path_str = args.next().ok_or(CliError::MissingArgument {
                command: "build",
                expected: "<file>",
            })?;
            
            handle_build(Path::new(&file_path_str), &mut args)?;
        }
        "help" => {
            print_help();
        }
        _ => return Err(CliError::UnknownCommand(command)),
    }

    Ok(())
}

fn handle_run(file_path: &Path) -> Result<(), CliError> {
    // Create compiler config for standalone mode
    let config = CompilerConfig::new()
        .standalone_mode()
        .add_source_file(file_path.to_path_buf());
    
    config.print_info();
    
    unsafe {
        compile_and_run(file_path);
    }
    Ok(())
}

fn handle_build(file_path: &Path, args: &mut env::Args) -> Result<(), CliError> {
    let mut config = CompilerConfig::new()
        .standalone_mode()
        .add_source_file(file_path.to_path_buf());
    
    // Parse additional build arguments
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-o" | "--output" => {
                let output_path = args.next().ok_or(CliError::MissingArgument {
                    command: "build",
                    expected: "output path after -o/--output",
                })?;
                config = config.with_output_path(PathBuf::from(output_path));
            }
            "--debug" => {
                config = config.with_debug(true);
            }
            "-O1" => config = config.with_optimization(OptimizationLevel::O1),
            "-O2" => config = config.with_optimization(OptimizationLevel::O2),
            "-O3" => config = config.with_optimization(OptimizationLevel::O3),
            "--with-vex" => {
                // TODO: Implement actual Vex binary detection and path resolution
                let vex_path = args.next().unwrap_or_else(|| "vex".to_string());
                config = config.with_vex_integration(vex_path);
            }
            _ => {
                eprintln!("Warning: Unknown build argument: {}", arg);
            }
        }
    }
    
    if let Err(e) = config.validate() {
        eprintln!("Configuration error: {}", e);
        return Ok(());
    }
    
    config.print_info();
    
    if config.is_low_level_mode() {
        println!("\n{}", "Building in low-level mode (no standard library)".color("255,204,0"));
        println!("Only system calls and inline assembly are available.");
    }
    
    // TODO: Integrate new compiler configuration with actual compilation process
    // TODO: Pass stdlib_manager and Vex integration settings to compilation
    // For now, use existing compile function
    unsafe {
        compile_and_img(file_path);
    }
    
    Ok(())
}

fn img_run(file_path: &Path) -> Result<(), CliError> {
    unsafe {
        compile_and_img(file_path);
    }
    Ok(())
}

fn print_usage() {
    eprintln!("\n{} {}",
              "Usage:".color("255,71,71"),
              "wave <command> [arguments]");
}

fn print_help() {
    println!("{}", "A simple, fast, and modern compiler for the Wave language.".color("145,161,2"));
    print_usage();
    println!("\n{}", "Commands:".color("145,161,2"));
    println!("  {}      {}",
             "run <file>".color("38,139,235"),
             "Execute the specified Wave file (standalone mode)");
    println!("  {}    {}",
             "build <file>".color("38,139,235"),
             "Compile the specified Wave file");
    println!("  {}           {}",
             "help".color("38,139,235"),
             "Show this help message");
    println!("  {}       {}",
             "--version".color("38,139,235"),
             "Show the CLI version");
    
    println!("\n{}", "Build Options:".color("145,161,2"));
    println!("  {}             {}",
             "-o <path>".color("38,139,235"),
             "Specify output file path");
    println!("  {}            {}",
             "--debug".color("38,139,235"),
             "Enable debug mode");
    println!("  {}      {}",
             "-O1/-O2/-O3".color("38,139,235"),
             "Set optimization level");
    println!("  {}     {}",
             "--with-vex".color("38,139,235"),
             "Enable Vex integration (standard library access)");
    
    println!("\n{}", "Mode Information:".color("145,161,2"));
    println!("  {} Wave compiler runs in two modes:", "•".color("38,139,235"));
    println!("    {} Low-level system programming (default)", "•".color("145,161,2"));
    println!("      - No standard library");
    println!("      - Direct system calls and inline assembly only");
    println!("      - Optimal for bare metal and kernel development");
    println!("    {} High-level development (with --with-vex)", "•".color("145,161,2"));
    println!("      - Full standard library access via Vex package manager");
    println!("      - I/O, math, memory management functions available");
    println!("      - Requires Vex to be installed");
}