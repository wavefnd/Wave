// SPDX-License-Identifier: MPL-2.0
fn main() {
    let arguments: Vec<std::ffi::OsString> = std::env::args_os().skip(1).collect();
    match wave_tools::cli::main(&arguments) {
        Ok(status) => std::process::exit(status),
        Err(error) => {
            eprintln!("wave-tools: {error}");
            std::process::exit(1);
        },
    }
}
