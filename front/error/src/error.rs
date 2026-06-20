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
    pub code: Option<String>,
    pub kind: WaveErrorKind,
    pub message: String,
    pub file: String,
    pub line: usize,
    pub column: usize,
    pub source: Option<String>,
    pub source_code: Option<String>,
    pub span_len: usize,
    pub label: Option<String>,
    pub context: Option<String>,
    pub expected: Vec<String>,
    pub found: Option<String>,
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
    pub fn new(
        kind: WaveErrorKind,
        message: impl Into<String>,
        file: impl Into<String>,
        line: usize,
        column: usize,
    ) -> Self {
        Self {
            code: None,
            kind,
            message: message.into(),
            file: file.into(),
            line,
            column,
            source: None,
            source_code: None,
            span_len: 1,
            label: None,
            context: None,
            expected: Vec::new(),
            found: None,
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

    pub fn with_source_code(mut self, source: impl Into<String>) -> Self {
        self.source_code = Some(source.into());
        self
    }

    pub fn with_span_len(mut self, span_len: usize) -> Self {
        self.span_len = span_len.max(1);
        self
    }

    pub fn with_label(mut self, label: impl Into<String>) -> Self {
        self.label = Some(label.into());
        self
    }

    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    pub fn with_context(mut self, context: impl Into<String>) -> Self {
        self.context = Some(context.into());
        self
    }

    pub fn with_expected(mut self, expected: impl Into<String>) -> Self {
        self.expected.push(expected.into());
        self
    }

    pub fn with_expected_many<I, S>(mut self, expected: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.expected = expected.into_iter().map(|s| s.into()).collect();
        self
    }

    pub fn with_found(mut self, found: impl Into<String>) -> Self {
        self.found = Some(found.into());
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
            format!(
                "standard library module 'std::{}' requires Vex package manager",
                module
            ),
            file,
            line,
            column,
        )
        .with_help("Wave compiler in standalone mode only supports low-level system programming")
        .with_suggestion("Use 'vex build' or 'vex run' to access standard library modules")
        .with_suggestion(format!(
            "Remove the import for 'std::{}' to use Wave in low-level mode",
            module
        ))
    }

    /// Create a type mismatch error with detailed information
    pub fn type_mismatch(
        expected: &str,
        found: &str,
        file: &str,
        line: usize,
        column: usize,
    ) -> Self {
        Self::new(
            WaveErrorKind::TypeMismatch {
                expected: expected.to_string(),
                found: found.to_string(),
            },
            format!(
                "mismatched types: expected `{}`, found `{}`",
                expected, found
            ),
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
        .with_code("E0401")
        .with_label("not found in this scope")
        .with_help("make sure the variable is declared before use")
    }

    fn severity_color(&self) -> &'static str {
        match self.severity {
            ErrorSeverity::Error => "255,71,71",
            ErrorSeverity::Warning => "145,161,2",
            ErrorSeverity::Note => "0,255,255",
            ErrorSeverity::Help => "38,139,235",
        }
    }

    fn display_source_block(&self) {
        use utils::colorex::*;

        let pipe = "|".color("38,139,235").bold();
        let line = self.line.max(1);
        let col = self.column.max(1);

        if let Some(source_code) = &self.source_code {
            let lines: Vec<&str> = source_code.lines().collect();
            if !lines.is_empty() {
                let idx = line.saturating_sub(1).min(lines.len().saturating_sub(1));
                let start = idx.saturating_sub(1);
                let end = (idx + 1).min(lines.len().saturating_sub(1));
                let width = (end + 1).to_string().len().max(2);

                for (i, source_line) in lines.iter().enumerate().take(end + 1).skip(start) {
                    let ln = i + 1;
                    let ln_str = format!("{:>width$}", ln, width = width);
                    eprintln!(
                        " {} {} {}",
                        ln_str.color("38,139,235").bold(),
                        pipe,
                        source_line
                    );
                }

                let pad = " ".repeat(width);
                let spaces = " ".repeat(col.saturating_sub(1));
                let marks = "^"
                    .repeat(self.span_len.max(1))
                    .color(self.severity_color())
                    .bold();
                match &self.label {
                    Some(label) => {
                        eprintln!(" {} {} {}{} {}", pad, pipe, spaces, marks, label.dim())
                    }
                    None => eprintln!(" {} {} {}{}", pad, pipe, spaces, marks),
                }

                return;
            }
        }

        if let Some(source_line) = &self.source {
            let width = line.to_string().len().max(2);
            let ln_str = format!("{:>width$}", line, width = width);
            eprintln!(
                " {} {} {}",
                ln_str.color("38,139,235").bold(),
                pipe,
                source_line
            );

            let pad = " ".repeat(width);
            let spaces = " ".repeat(col.saturating_sub(1));
            let marks = "^"
                .repeat(self.span_len.max(1))
                .color(self.severity_color())
                .bold();
            match &self.label {
                Some(label) => eprintln!(" {} {} {}{} {}", pad, pipe, spaces, marks, label.dim()),
                None => eprintln!(" {} {} {}{}", pad, pipe, spaces, marks),
            }
        }
    }

    /// Display error in Rust-style format
    pub fn display(&self) {
        use utils::colorex::*;

        let severity_str = match self.severity {
            ErrorSeverity::Error => "error".color("255,71,71").bold(),
            ErrorSeverity::Warning => "warning".color("145,161,2").bold(),
            ErrorSeverity::Note => "note".color("0,255,255").bold(),
            ErrorSeverity::Help => "help".color("38,139,235").bold(),
        };

        let code = self
            .code
            .as_ref()
            .map(|c| {
                format!("[{}]", c)
                    .color(self.severity_color())
                    .bold()
                    .to_string()
            })
            .unwrap_or_default();

        if code.is_empty() {
            eprintln!("{}: {}", severity_str, self.message.bold());
        } else {
            eprintln!("{}{}: {}", severity_str, code, self.message.bold());
        }

        eprintln!(
            "  {} {}:{}:{}",
            "-->".color("38,139,235").bold(),
            self.file,
            self.line.max(1),
            self.column.max(1)
        );
        self.display_source_block();

        if let Some(context) = &self.context {
            eprintln!(
                "   {} {}: {}",
                "=".color("38,139,235").bold(),
                "context".color("38,139,235").bold(),
                context
            );
        }

        if !self.expected.is_empty() {
            eprintln!(
                "   {} {}: {}",
                "=".color("38,139,235").bold(),
                "expected".color("38,139,235").bold(),
                self.expected.join(", ")
            );
        }

        if let Some(found) = &self.found {
            eprintln!(
                "   {} {}: {}",
                "=".color("38,139,235").bold(),
                "found".color("38,139,235").bold(),
                found
            );
        }

        if let Some(note) = &self.note {
            eprintln!(
                "   {} {}: {}",
                "=".color("38,139,235").bold(),
                "note".color("0,255,255").bold(),
                note
            );
        }

        if let Some(help) = &self.help {
            eprintln!(
                "   {} {}: {}",
                "=".color("38,139,235").bold(),
                "help".color("38,139,235").bold(),
                help
            );
        }

        for suggestion in &self.suggestions {
            eprintln!(
                "   {} {}: {}",
                "=".color("38,139,235").bold(),
                "suggestion".color("38,139,235").bold(),
                suggestion
            );
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
            let error_count = errors
                .iter()
                .filter(|e| matches!(e.severity, ErrorSeverity::Error))
                .count();
            let warning_count = errors
                .iter()
                .filter(|e| matches!(e.severity, ErrorSeverity::Warning))
                .count();

            eprintln!();
            if error_count > 0 {
                eprintln!(
                    "error: aborting due to {} previous error{}",
                    error_count,
                    if error_count == 1 { "" } else { "s" }
                );
            }
            if warning_count > 0 {
                eprintln!(
                    "warning: {} warning{} emitted",
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
