use std::path::Path;
use std::{env, process};

use utils::colorex::*;
use wavec::commands::{handle_build, handle_install_std, handle_run, handle_update_std, DebugFlags};
use wavec::errors::CliError;
use wavec::{compile_and_build, compile_and_build_obj, version_wave};

fn main() {
    if let Err(e) = run() {
        eprintln!("{}", e);
        print_usage();
        process::exit(1);
    }
}

fn run() -> Result<(), CliError> {
    let mut args: Vec<String> = env::args().collect();
    args.remove(0);

    if args.is_empty() {
        return Err(CliError::NotEnoughArgs);
    }

    let mut debug_flags = DebugFlags::default();
    let mut opt_flag = "-O0".to_string();

    let base_args: Vec<String> = args
        .iter()
        .filter(|a| {
            if a.starts_with("--debug-wave=") {
                let mode = a.split('=').nth(1).unwrap_or("");
                debug_flags.apply(mode);
                false
            } else if a.starts_with("-O") {
                opt_flag = a.to_string();
                false
            } else {
                true
            }
        })
        .cloned()
        .collect();

    let command = base_args[0].clone();
    let mut iter = base_args.into_iter().skip(1);

    match command.as_str() {
        "--version" | "-V" => {
            version_wave();
        }

        "run" => {
            let next = iter.next().ok_or(CliError::MissingArgument {
                command: "run",
                expected: "<file | --img>",
            })?;

            if next == "--img" {
                let file = iter.next().ok_or(CliError::MissingArgument {
                    command: "run --img",
                    expected: "<file>",
                })?;

                handle_build(Path::new(&file), &opt_flag, &debug_flags)?;
            } else {
                handle_run(Path::new(&next), &opt_flag, &debug_flags)?;
            }
        }

        "build" => {
            let next = iter.next().ok_or(CliError::MissingArgument {
                command: "build",
                expected: "<file | -o>",
            })?;

            unsafe {
                if next == "-o" {
                    let file = iter.next().ok_or(CliError::MissingArgument {
                        command: "build -o",
                        expected: "<file>",
                    })?;

                    compile_and_build_obj(Path::new(&file), &opt_flag, &debug_flags);
                } else {
                    compile_and_build(Path::new(&next), &opt_flag, &debug_flags);
                }
            }
        }


        "install" => {
            let target = iter.next().ok_or(CliError::MissingArgument {
                command: "install",
                expected: "<target>",
            })?;

            match target.as_str() {
                "std" => {
                    handle_install_std()?;
                }
                _ => return Err(CliError::UnknownInstallTarget(target)),
            }
        }

        "update" => {
            let target = iter.next().ok_or(CliError::MissingArgument {
                command: "update",
                expected: "<target>",
            })?;

            match target.as_str() {
                "std" => handle_update_std()?,
                _ => return Err(CliError::UnknownUpdateTarget(target)),
            }
        }

        "--help" => print_help(),

        _ => return Err(CliError::UnknownCommand(command)),
    }

    Ok(())
}

fn print_usage() {
    eprintln!(
        "\n{} {}",
        "Usage:".color("255,71,71"),
        "wavec <command> [arguments]"
    );
}

fn print_help() {
    println!("{}", "Wave Compiler".color("145,161,2"));
    println!("{}", "Commands & Options");
    print_usage();

    println!("\nCommands:");
    println!(
        "  {:<18} {}",
        "run <file>".color("38,139,235"),
        "Execute Wave file"
    );
    println!(
        "  {:<18} {}",
        "build <file>".color("38,139,235"),
        "Compile Wave file"
    );
    println!(
        "  {:<18} {}",
        "install std".color("38,139,235"),
        "Install Wave standard library (std)"
    );
    println!(
        "  {:<18} {}",
        "update std".color("38,139,235"),
        "Update Wave standard library (std)"
    );
    println!(
        "  {:<18} {}",
        "--help".color("38,139,235"),
        "Show this help message"
    );
    println!(
        "  {:<18} {}",
        "--version".color("38,139,235"),
        "Show compiler version"
    );

    println!("\nOptimization:");
    println!(
        "  {:<18} {}",
        "-O0 .. -O3".color("38,139,235"),
        "Set optimization level"
    );
    println!(
        "  {:<18} {}",
        "-Oz".color("38,139,235"),
        "Optimize for binary size"
    );
    println!(
        "  {:<18} {}",
        "-Ofast".color("38,139,235"),
        "Enable aggressive optimizations"
    );

    println!("\nDebug options:");
    println!(
        "  {:<22} {}",
        "--debug-wave=tokens".color("38,139,235"),
        "Print lexer tokens"
    );
    println!(
        "  {:<22} {}",
        "--debug-wave=ast".color("38,139,235"),
        "Print AST"
    );
    println!(
        "  {:<22} {}",
        "--debug-wave=ir".color("38,139,235"),
        "Print LLVM IR"
    );
    println!(
        "  {:<22} {}",
        "--debug-wave=mc".color("38,139,235"),
        "Print machine code"
    );
    println!(
        "  {:<22} {}",
        "--debug-wave=hex".color("38,139,235"),
        "Print raw hex output"
    );
    println!(
        "  {:<22} {}",
        "--debug-wave=all".color("38,139,235"),
        "Enable all debug outputs"
    );

    println!("\nExamples:");
    println!("  wavec run test.wave");
    println!("  wavec run -O3 test.wave");
    println!("  wavec run --debug-wave=ir test.wave");
    println!("  wavec run -Ofast --debug-wave=all test.wave");
}
