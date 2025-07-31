use std::{env, fmt, process};
use std::path::Path;
use colorex::Colorize;
use wavec::{compile_and_img, compile_and_run};
use wavec::version_wave;

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
        "help" => {
            print_help();
        }
        _ => return Err(CliError::UnknownCommand(command)),
    }

    Ok(())
}

fn handle_run(file_path: &Path) -> Result<(), CliError> {
    unsafe {
        compile_and_run(file_path);
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
    println!("  {}    {}",
             "run <file>".color("38,139,235"),
             "Execute the specified Wave file");
    println!("  {}         {}",
             "help".color("38,139,235"),
             "Show this help message");
    println!("  {}     {}",
             "--version".color("38,139,235"),
             "Show the CLI version");
}