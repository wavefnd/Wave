// SPDX-License-Identifier: MPL-2.0
use crate::{error, Result};
use std::ffi::OsString;
use std::path::PathBuf;

/// Preserve native paths/argv. Only names and manifest text require UTF-8.
pub struct Args {
    pub values: Vec<OsString>,
}

impl Args {
    pub fn new(values: &[OsString]) -> Self {
        Self { values: values.to_vec() }
    }

    pub fn flag(&mut self, name: &str) -> bool {
        if let Some(index) = self.values.iter().take_while(|v| *v != "--").position(|v| v == name) {
            self.values.remove(index);
            true
        } else {
            false
        }
    }

    pub fn option(&mut self, name: &str) -> Result<Option<OsString>> {
        if let Some(index) = self.values.iter().take_while(|v| *v != "--").position(|v| v == name) {
            self.values.remove(index);
            if index == self.values.len() || self.values[index] == "--" {
                return Err(error(format!("{name} requires a value")));
            }
            Ok(Some(self.values.remove(index)))
        } else {
            let prefix = format!("{name}=");
            if let Some(index) = self
                .values
                .iter()
                .take_while(|v| *v != "--")
                .position(|v| v.to_str().is_some_and(|v| v.starts_with(&prefix)))
            {
                let value = self.values.remove(index);
                Ok(Some(value.to_str().unwrap()[prefix.len()..].into()))
            } else {
                Ok(None)
            }
        }
    }

    pub fn text(&mut self, name: &str) -> Result<Option<String>> {
        self.option(name)?
            .map(|v| v.into_string().map_err(|_| error(format!("{name} must be UTF-8"))))
            .transpose()
    }

    pub fn path(&mut self, name: &str) -> Result<Option<PathBuf>> {
        Ok(self.option(name)?.map(PathBuf::from))
    }

    pub fn required(&mut self, name: &str) -> Result<String> {
        self.text(name)?.ok_or_else(|| error(format!("missing {name}")))
    }

    pub fn finish(self) -> Result<()> {
        if self.values.is_empty() {
            Ok(())
        } else {
            Err(error(format!("unexpected arguments: {:?}", self.values)))
        }
    }
}

pub fn main(arguments: &[OsString]) -> Result<i32> {
    let Some((command, rest)) = arguments.split_first() else {
        return Err(error("expected a wave-tools command"));
    };
    if command == "diagnose-windows-arm64" {
        return crate::windows::diagnose(rest);
    }
    if command == "msvc-acceptance" {
        return crate::windows::main(rest);
    }
    if command == "native-std" {
        return crate::native::main(rest);
    }
    if command == "__fixture-process" {
        return crate::native::process_child(rest);
    }
    if command == "__fixture-child" {
        return crate::native::child(rest);
    }
    if command == "__supervise" {
        return crate::process::supervise(rest);
    }
    if command == "freebsd" {
        return crate::freebsd::main(rest);
    }
    if command == "ci" {
        return crate::ci::main(rest);
    }
    if command == "config" {
        return crate::config::main(rest);
    }
    if command == "prepare" {
        return crate::provision::main(rest);
    }
    if command == "sdk" {
        return crate::sdk::main(rest);
    }
    if command == "release" {
        return crate::publication::main(rest, false);
    }
    if command == "nightly" {
        return crate::publication::main(rest, true);
    }
    if command == "check" {
        return crate::checks::main(rest);
    }
    if command == "maintain" {
        return crate::maintenance::main(rest);
    }
    if command == "build-cargo" {
        return crate::build::compiler(rest);
    }
    if command == "clean" {
        return crate::build::clean(rest);
    }
    if command == "package" {
        return crate::package::main(rest, false);
    }
    if command == "install" {
        return crate::package::main(rest, true);
    }
    if command == "test" {
        return crate::runner::main(rest, false);
    }
    if command == "corpus" {
        return crate::runner::main(rest, true);
    }
    if command == "matrix" {
        return crate::cases::main(rest);
    }
    if command == "exec" {
        let mut args = Args::new(rest);
        let timeout = args.text("--timeout")?.unwrap_or_else(|| "1800".into()).parse::<u64>()?;
        let limit =
            args.text("--output-limit")?.unwrap_or_else(|| "67108864".into()).parse::<usize>()?;
        let cwd = args.path("--cwd")?.map(Ok).unwrap_or_else(std::env::current_dir)?;
        let capture = args.flag("--capture");
        let stack = args.text("--stack")?.map(|v| v.parse::<u64>()).transpose()?;
        if args.values.first().is_some_and(|v| v == "--") {
            args.values.remove(0);
        }
        let mut spec = crate::process::Spec::new(args.values);
        spec.stack = stack;
        spec.timeout = std::time::Duration::from_secs(timeout);
        spec.output_limit = limit;
        spec.cwd = cwd;
        spec.stream = !capture;
        let output = spec.run()?;
        if capture {
            print!("{}", output.text_stdout());
            eprint!("{}", output.text_stderr());
        }
        if let Some(reason) = output.failure {
            return Err(error(reason));
        }
        return Ok(if output.status < 0 { 128 - output.status } else { output.status });
    }
    if command == "--help" || command == "help" {
        println!("wave-tools: native Wave build, test and distribution contracts");
        println!("  config version|rust-version|build-inputs");
        println!("  matrix [--json] [--github-output PATH]");
        println!("  test|corpus --wavec PATH [--target-id ID] [--report-json PATH]");
        println!("  check std-policy|std-inputs|case-sources|binary|crt|extracted-package|ci-jobs|release-assets");
        println!("  maintain format|std-api|maintainers|unicode|verify-patch");
        println!("  build-cargo|package|install|clean");
        println!("  prepare host|cross|loongarch|std|pins|tooling");
        println!("  sdk|freebsd|native-std|msvc-acceptance|diagnose-windows-arm64");
        println!("  ci artifact|report|runtime-evidence");
        println!("  release|nightly --stage STAGE [--publish]");
        println!("  exec [--timeout SECONDS] [--output-limit BYTES] -- COMMAND ...");
        return Ok(0);
    }
    Err(error(format!("unknown command: {}", command.to_string_lossy())))
}
