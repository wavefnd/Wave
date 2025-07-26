pub mod runner;
pub mod version;

use std::path::Path;
use colorex::Colorize;

pub unsafe fn compile_and_run(path: &Path) {
    runner::run_wave_file(path);
}

pub unsafe fn compile_and_img(path: &Path) {
    runner::img_wave_file(path);
}

pub fn version_wave() {
    println!("{} {}", "wavec".color("2,161,47"), version::version().color("2,161,47"));
}
