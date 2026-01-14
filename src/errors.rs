use std::fmt;
use std::path::PathBuf;

#[derive(Debug)]
pub enum CliError {
    NotEnoughArgs,
    UnknownCommand(String),
    MissingArgument {
        command: &'static str,
        expected: &'static str,
    },

    // install/update
    UnknownInstallTarget(String),
    UnknownUpdateTarget(String),
    StdAlreadyInstalled { path: PathBuf },
    InvalidExecutablePath,
    ExternalToolMissing(&'static str),
    CommandFailed(String),
    HomeNotSet,

    // io
    Io(std::io::Error),
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

            CliError::UnknownInstallTarget(t) => write!(f, "Error: Unknown install target '{}'", t),
            CliError::UnknownUpdateTarget(t) => write!(f, "Error: Unknown update target '{}'", t),
            CliError::StdAlreadyInstalled { path } => write!(f, "Error: std already installed at '{}'", path.display()),
            CliError::InvalidExecutablePath => write!(f, "Error: Invalid executable path"),
            CliError::ExternalToolMissing(t) => write!(f, "Error: required tool not found: {}", t),
            CliError::CommandFailed(cmd) => write!(f, "Error: command failed: {}", cmd),
            CliError::HomeNotSet => write!(f, "Error: HOME environment variable not set"),

            CliError::Io(e) => write!(f, "IO Error: {}", e),
        }
    }
}

impl From<std::io::Error> for CliError {
    fn from(e: std::io::Error) -> Self {
        CliError::Io(e)
    }
}
