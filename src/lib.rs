pub mod runner;
pub mod version;

use std::path::Path;
use colorex::Colorize;
use crate::version::get_os_pretty_name;

pub unsafe fn compile_and_run(path: &Path) {
    runner::run_wave_file(path);
}

pub unsafe fn compile_and_img(path: &Path) {
    runner::img_wave_file(path);
}

pub fn version_wave() {
    let os = format!("({})", get_os_pretty_name()).color("117,117,117");

    println!("{} {} {}",
             "wavec".color("2,161,47"),
             version::version().color("2,161,47"),
             os);
}
