pub mod runner;
pub mod version;
pub mod commands;
pub mod errors;

use std::path::Path;
use colorex::Colorize;
use crate::version::get_os_pretty_name;

use commands::DebugFlags;

pub unsafe fn compile_and_run(path: &Path, opt_flag: &str, debug: &DebugFlags) {
    runner::run_wave_file(path, opt_flag, debug);
}

pub unsafe fn compile_and_img(path: &Path) {
    runner::img_wave_file(path);
}

pub fn version_wave() {
    let os = format!("({})", get_os_pretty_name()).color("117,117,117");

    println!(
        "{} {} {}",
        "wavec".color("2,161,47"),
        version::version().color("2,161,47"),
        os
    );
}
