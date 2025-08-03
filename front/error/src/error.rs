#[derive(Debug, Clone, PartialEq)]
pub enum WaveErrorKind {
    // Lexer errors
    UnexpectedToken(String),
    ExpectedToken(String),
    UnexpectedChar(char),
    InvalidNumber(String),
    InvalidString(String),
    UnterminatedString,
    UnterminatedComment,
    
    // Parser errors
    SyntaxError(String),
    UnexpectedEndOfFile,
    InvalidExpression(String),
    InvalidStatement(String),
    InvalidType(String),
    
    // Import/Module errors
    ModuleNotFound(String),
    ImportError(String),
    CircularImport(String),
    
    // Type checking errors
    TypeMismatch { expected: String, found: String },
    UndefinedVariable(String),
    UndefinedFunction(String),
    InvalidFunctionCall(String),
    InvalidAssignment(String),
    
    // Standard library errors
    StandardLibraryNotAvailable,
    UnknownStandardLibraryModule(String),
    VexIntegrationRequired(String),
    
    // Compilation errors
    CompilationFailed(String),
    LinkingFailed(String),
    
    // I/O errors
    FileNotFound(String),
    FileReadError(String),
    FileWriteError(String),
}

#[derive(Debug, Clone)]
pub struct WaveError {
    pub kind: WaveErrorKind,
    pub message: String,
    pub file: String,
    pub line: usize,
    pub column: usize,
    pub source: Option<String>,
    pub label: Option<String>,
    pub help: Option<String>,
    pub note: Option<String>,
    pub suggestions: Vec<String>,
    pub severity: ErrorSeverity,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorSeverity {
    Error,
    Warning,
    Note,
    Help,
}

impl WaveError {
    pub fn new(kind: WaveErrorKind, message: impl Into<String>, file: impl Into<String>, line: usize, column: usize) -> Self {
        Self {
            kind,
            message: message.into(),
            file: file.into(),
            line,
            column,
            source: None,
            label: None,
            help: None,
            note: None,
            suggestions: Vec::new(),
            severity: ErrorSeverity::Error,
        }
    }

    pub fn with_source(mut self, source: impl Into<String>) -> Self {
        self.source = Some(source.into());
        self
    }

    pub fn with_label(mut self, label: impl Into<String>) -> Self {
        self.label = Some(label.into());
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.note = Some(note.into());
        self
    }

    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestions.push(suggestion.into());
        self
    }

    pub fn with_severity(mut self, severity: ErrorSeverity) -> Self {
        self.severity = severity;
        self
    }

    /// Create a new error for standard library access without Vex
    pub fn stdlib_requires_vex(module: &str, file: &str, line: usize, column: usize) -> Self {
        Self::new(
            WaveErrorKind::VexIntegrationRequired(module.to_string()),
            format!("standard library module 'std::{}' requires Vex package manager", module),
            file,
            line,
            column,
        )
        .with_help("Wave compiler in standalone mode only supports low-level system programming")
        .with_suggestion("Use 'vex build' or 'vex run' to access standard library modules")
        .with_suggestion(format!("Remove the import for 'std::{}' to use Wave in low-level mode", module))
    }

    /// Create a type mismatch error with detailed information
    pub fn type_mismatch(expected: &str, found: &str, file: &str, line: usize, column: usize) -> Self {
        Self::new(
            WaveErrorKind::TypeMismatch { 
                expected: expected.to_string(), 
                found: found.to_string() 
            },
            format!("mismatched types: expected `{}`, found `{}`", expected, found),
            file,
            line,
            column,
        )
        .with_label(format!("expected `{}`, found `{}`", expected, found))
    }

    /// Create an undefined variable error
    pub fn undefined_variable(var_name: &str, file: &str, line: usize, column: usize) -> Self {
        Self::new(
            WaveErrorKind::UndefinedVariable(var_name.to_string()),
            format!("cannot find value `{}` in this scope", var_name),
            file,
            line,
            column,
        )
        .with_label("not found in this scope")
        .with_help("make sure the variable is declared before use")
    }

    /// Display error in Rust-style format
    pub fn display(&self) {
        use colored::*;

        let severity_str = match self.severity {
            ErrorSeverity::Error => "error".red().bold(),
            ErrorSeverity::Warning => "warning".yellow().bold(),
            ErrorSeverity::Note => "note".cyan().bold(),
            ErrorSeverity::Help => "help".green().bold(),
        };

        // Main error message
        eprintln!("{}: {}", severity_str, self.message.bold());
        
        // Location
        eprintln!("  {} {}:{}:{}", "-->".blue().bold(), self.file, self.line, self.column);
        eprintln!("   {}", "|".blue().bold());

        // Source code with highlighting
        if let Some(source_line) = &self.source {
            eprintln!("{:>3} {} {}", self.line.to_string().blue().bold(), "|".blue().bold(), source_line);
            
            // Arrow pointing to the error
            let spaces = " ".repeat(self.column.saturating_sub(1));
            let arrow = match self.severity {
                ErrorSeverity::Error => "^".red().bold(),
                ErrorSeverity::Warning => "^".yellow().bold(),
                ErrorSeverity::Note => "^".cyan().bold(),
                ErrorSeverity::Help => "^".green().bold(),
            };
            
            if let Some(label) = &self.label {
                eprintln!("   {} {}{} {}", "|".blue().bold(), spaces, arrow, label.dimmed());
            } else {
                eprintln!("   {} {}{}", "|".blue().bold(), spaces, arrow);
            }
        }

        eprintln!("   {}", "|".blue().bold());

        // Additional information
        if let Some(note) = &self.note {
            eprintln!("   {} {}: {}", "=".blue().bold(), "note".cyan().bold(), note);
        }

        if let Some(help) = &self.help {
            eprintln!("   {} {}: {}", "=".blue().bold(), "help".green().bold(), help);
        }

        // Suggestions
        for suggestion in &self.suggestions {
            eprintln!("   {} {}: {}", "=".blue().bold(), "suggestion".green().bold(), suggestion);
        }
    }

    /// Display multiple errors in a batch
    pub fn display_batch(errors: &[WaveError]) {
        for (i, error) in errors.iter().enumerate() {
            if i > 0 {
                eprintln!();
            }
            error.display();
        }
        
        if errors.len() > 1 {
            let error_count = errors.iter().filter(|e| matches!(e.severity, ErrorSeverity::Error)).count();
            let warning_count = errors.iter().filter(|e| matches!(e.severity, ErrorSeverity::Warning)).count();
            
            eprintln!();
            if error_count > 0 {
                eprintln!("error: aborting due to {} previous error{}", 
                    error_count, 
                    if error_count == 1 { "" } else { "s" }
                );
            }
            if warning_count > 0 {
                eprintln!("warning: {} warning{} emitted", 
                    warning_count,
                    if warning_count == 1 { "" } else { "s" }
                );
            }
        }
    }

    /// Check if this error should abort compilation
    pub fn is_fatal(&self) -> bool {
        matches!(self.severity, ErrorSeverity::Error)
    }
}