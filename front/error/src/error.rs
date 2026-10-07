//! Structured Wave diagnostics and human/JSON rendering.
//!
//! [`WaveError`] is the canonical diagnostic record. Renderers consume the same
//! code, location, context, label, note, and help fields so machine-readable
//! output cannot diverge from terminal diagnostics.

use utils::wson::{self, Value};

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

impl WaveErrorKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            WaveErrorKind::UnexpectedToken(_) => "unexpected-token",
            WaveErrorKind::ExpectedToken(_) => "expected-token",
            WaveErrorKind::UnexpectedChar(_) => "unexpected-char",
            WaveErrorKind::InvalidNumber(_) => "invalid-number",
            WaveErrorKind::InvalidString(_) => "invalid-string",
            WaveErrorKind::UnterminatedString => "unterminated-string",
            WaveErrorKind::UnterminatedComment => "unterminated-comment",
            WaveErrorKind::SyntaxError(_) => "syntax-error",
            WaveErrorKind::UnexpectedEndOfFile => "unexpected-end-of-file",
            WaveErrorKind::InvalidExpression(_) => "invalid-expression",
            WaveErrorKind::InvalidStatement(_) => "invalid-statement",
            WaveErrorKind::InvalidType(_) => "invalid-type",
            WaveErrorKind::ModuleNotFound(_) => "module-not-found",
            WaveErrorKind::ImportError(_) => "import-error",
            WaveErrorKind::CircularImport(_) => "circular-import",
            WaveErrorKind::TypeMismatch { .. } => "type-mismatch",
            WaveErrorKind::UndefinedVariable(_) => "undefined-variable",
            WaveErrorKind::UndefinedFunction(_) => "undefined-function",
            WaveErrorKind::InvalidFunctionCall(_) => "invalid-function-call",
            WaveErrorKind::InvalidAssignment(_) => "invalid-assignment",
            WaveErrorKind::StandardLibraryNotAvailable => "standard-library-not-available",
            WaveErrorKind::UnknownStandardLibraryModule(_) => "unknown-standard-library-module",
            WaveErrorKind::VexIntegrationRequired(_) => "vex-integration-required",
            WaveErrorKind::CompilationFailed(_) => "compilation-failed",
            WaveErrorKind::LinkingFailed(_) => "linking-failed",
            WaveErrorKind::FileNotFound(_) => "file-not-found",
            WaveErrorKind::FileReadError(_) => "file-read-error",
            WaveErrorKind::FileWriteError(_) => "file-write-error",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RelatedDiagnostic {
    pub message: String,
    pub span: crate::SourceSpan,
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
    pub span: Option<crate::SourceSpan>,
    pub related: Vec<RelatedDiagnostic>,
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
            span: None,
            related: Vec::new(),
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

    pub fn with_span(mut self, span: Option<&crate::SourceSpan>) -> Self {
        if let Some(span) = span {
            let span = span.focus.as_deref().unwrap_or(span);
            self.file = span.file.clone();
            self.line = span.line;
            self.column = span.column;
            self.span_len = if span.line == span.end_line {
                span.end_column.saturating_sub(span.column).max(1)
            } else {
                1
            };
            self.span = Some(span.clone());
        }
        self
    }

    pub fn with_related(mut self, related: impl IntoIterator<Item = RelatedDiagnostic>) -> Self {
        self.related.extend(related);
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

    pub fn to_json(&self) -> String {
        let error = Value::object([
            ("kind", Value::string(self.kind.as_str())),
            ("message", Value::string(&self.message)),
            ("file", Value::string(&self.file)),
            ("line", Value::integer(self.line as u64)),
            ("column", Value::integer(self.column as u64)),
            ("span_len", Value::integer(self.span_len.max(1) as u64)),
            ("span", span_value(self.span.as_ref())),
            (
                "related",
                Value::Array(
                    self.related
                        .iter()
                        .map(|r| {
                            Value::object([
                                ("message", Value::string(&r.message)),
                                ("span", span_value(Some(&r.span))),
                            ])
                        })
                        .collect(),
                ),
            ),
            (
                "severity",
                Value::string(match self.severity {
                    ErrorSeverity::Error => "error",
                    ErrorSeverity::Warning => "warning",
                    ErrorSeverity::Note => "note",
                    ErrorSeverity::Help => "help",
                }),
            ),
            ("code", Value::optional_string(self.code.as_deref())),
            ("label", Value::optional_string(self.label.as_deref())),
            ("context", Value::optional_string(self.context.as_deref())),
            ("expected", Value::strings(&self.expected)),
            ("found", Value::optional_string(self.found.as_deref())),
            ("note", Value::optional_string(self.note.as_deref())),
            ("help", Value::optional_string(self.help.as_deref())),
            ("suggestions", Value::strings(&self.suggestions)),
        ]);
        wson::dumps(&Value::object([("error", error)]), wson::Format::Json, false)
            .expect("diagnostic schema contains only shallow JSON values")
    }

    pub fn display_auto(&self) {
        if std::env::var("WAVE_ERROR_FORMAT").as_deref() == Ok("json") {
            eprintln!("{}", self.to_json());
        } else {
            self.display();
        }
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
        if self.line == 0 || self.column == 0 {
            return;
        }
        let line = self.line;
        let col = self.column;

        if let Some(source_code) = &self.source_code {
            // `lines()` drops the empty line containing EOF after a final newline.
            let lines: Vec<&str> =
                crate::span::source_lines(source_code).into_iter().map(|(_, line)| line).collect();
            if line <= lines.len() {
                let idx = line - 1;
                let start = idx.saturating_sub(1);
                let end = (idx + 1).min(lines.len().saturating_sub(1));
                let width = (end + 1).to_string().len().max(2);

                for (i, source_line) in lines.iter().enumerate().take(end + 1).skip(start) {
                    let rendered =
                        utils::display_width::diagnostic_line(source_line, col, self.span_len);
                    let ln = i + 1;
                    let ln_str = format!("{:>width$}", ln, width = width);
                    eprintln!(" {} {} {}", ln_str.color("38,139,235").bold(), pipe, rendered.text);
                    if ln == line {
                        let pad = " ".repeat(width);
                        let spaces = " ".repeat(rendered.caret_offset);
                        let marks =
                            "^".repeat(rendered.caret_width).color(self.severity_color()).bold();
                        match &self.label {
                            Some(label) => {
                                eprintln!(" {} {} {}{} {}", pad, pipe, spaces, marks, label.dim())
                            },
                            None => eprintln!(" {} {} {}{}", pad, pipe, spaces, marks),
                        }
                    }
                }
            }
            return;
        }

        if let Some(source_line) = &self.source {
            let rendered = utils::display_width::diagnostic_line(source_line, col, self.span_len);
            let width = line.to_string().len().max(2);
            let ln_str = format!("{:>width$}", line, width = width);
            eprintln!(" {} {} {}", ln_str.color("38,139,235").bold(), pipe, rendered.text);

            let pad = " ".repeat(width);
            let spaces = " ".repeat(rendered.caret_offset);
            let marks = "^".repeat(rendered.caret_width).color(self.severity_color()).bold();
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
            .map(|c| format!("[{}]", c).color(self.severity_color()).bold().to_string())
            .unwrap_or_default();

        if code.is_empty() {
            eprintln!("{}: {}", severity_str, self.message.bold());
        } else {
            eprintln!("{}{}: {}", severity_str, code, self.message.bold());
        }

        if self.line > 0 && self.column > 0 {
            eprintln!(
                "  {} {}:{}:{}",
                "-->".color("38,139,235").bold(),
                self.file,
                self.line,
                self.column
            );
        } else {
            eprintln!("  {} {}", "-->".color("38,139,235").bold(), self.file);
        }
        self.display_source_block();
        for related in &self.related {
            eprintln!(
                "   {} {}: {}",
                "=".color("38,139,235").bold(),
                "note".color("0,255,255").bold(),
                related.message
            );
            eprintln!(
                "  {} {}:{}:{}",
                "-->".color("38,139,235").bold(),
                related.span.file,
                related.span.line,
                related.span.column
            );
            if related.span.file == self.file {
                let mut location = Self::new(
                    self.kind.clone(),
                    &related.message,
                    &related.span.file,
                    related.span.line,
                    related.span.column,
                )
                .with_span(Some(&related.span))
                .with_severity(ErrorSeverity::Note);
                location.source_code = self.source_code.clone();
                location.display_source_block();
            }
        }

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
        if std::env::var("WAVE_ERROR_FORMAT").as_deref() == Ok("json") {
            for error in errors {
                eprintln!("{}", error.to_json());
            }
            return;
        }

        for (i, error) in errors.iter().enumerate() {
            if i > 0 {
                eprintln!();
            }
            error.display_auto();
        }

        if errors.len() > 1 {
            let error_count =
                errors.iter().filter(|e| matches!(e.severity, ErrorSeverity::Error)).count();
            let warning_count =
                errors.iter().filter(|e| matches!(e.severity, ErrorSeverity::Warning)).count();

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

fn span_value(span: Option<&crate::SourceSpan>) -> Value {
    let Some(span) = span else {
        return Value::Null;
    };
    Value::object([
        ("file", Value::string(&span.file)),
        ("start", Value::integer(span.start as u64)),
        ("end", Value::integer(span.end as u64)),
        ("line", Value::integer(span.line as u64)),
        ("column", Value::integer(span.column as u64)),
        ("end_line", Value::integer(span.end_line as u64)),
        ("end_column", Value::integer(span.end_column as u64)),
        ("expansion", Value::strings(&span.expansion)),
    ])
}
