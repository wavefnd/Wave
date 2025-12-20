use crate::errors::CliError;
use crate::{compile_and_img, compile_and_run};
use std::path::Path;

#[derive(Default)]
pub struct DebugFlags {
    pub ast: bool,
    pub tokens: bool,
    pub ir: bool,
    pub mc: bool,
    pub hex: bool,
}

impl DebugFlags {
    pub fn apply(&mut self, mode: &str) {
        match mode {
            "tokens" => self.tokens = true,
            "ast" => self.ast = true,
            "ir" => self.ir = true,
            "mc" => self.mc = true,
            "hex" => self.hex = true,
            "all" => {
                self.ast = true;
                self.ir = true;
                self.mc = true;
                self.hex = true;
            }
            _ => {}
        }
    }
}

pub fn handle_run(file_path: &Path, opt_flag: &str, debug: &DebugFlags) -> Result<(), CliError> {
    unsafe {
        compile_and_run(file_path, opt_flag, debug);
    }
    Ok(())
}

pub fn handle_build(file_path: &Path, opt_flag: &str, debug: &DebugFlags) -> Result<(), CliError> {
    println!("Building with {}...", opt_flag);

    unsafe {
        compile_and_img(file_path);
    }

    if debug.mc {
        println!("Machine code built successfully.");
    }

    Ok(())
}

pub fn img_run(file_path: &Path) -> Result<(), CliError> {
    unsafe {
        compile_and_img(file_path);
    }
    Ok(())
}
