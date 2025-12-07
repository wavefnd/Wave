use std::fmt;

#[derive(Debug)]
pub enum CliError {
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
            CliError::NotEnoughArgs => write!(f, "Error: Not enough arguments"),
            CliError::UnknownCommand(cmd) => write!(f, "Error: Unknown command '{}'", cmd),
            CliError::MissingArgument { command, expected } => write!(
                f,
                "Error: Missing argument for '{}'. Expected: {}",
                command, expected
            ),
        }
    }
}
