//! Recoverable backend failures. Panics remain reserved for compiler invariants.
use std::fmt;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CodegenPhase {
    Target,
    Validation,
    Lowering,
    Optimization,
    Emission,
    Tool,
    Link,
}
impl fmt::Display for CodegenPhase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Target => "target-configuration",
            Self::Validation => "lowering-validation",
            Self::Lowering => "ir-generation",
            Self::Optimization => "optimization",
            Self::Emission => "file-emission",
            Self::Tool => "external-tool",
            Self::Link => "linking",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CodegenErrorKind {
    BackendFailure,
    InvalidAssembly,
    MissingTool,
}

#[derive(Debug, Clone)]
pub struct CodegenError {
    pub phase: CodegenPhase,
    pub kind: CodegenErrorKind,
    pub operation: String,
    pub message: String,
    pub span: Option<Box<error::SourceSpan>>,
}
impl CodegenError {
    pub fn new(phase: CodegenPhase, operation: impl Into<String>, message: impl ToString) -> Self {
        Self {
            phase,
            kind: CodegenErrorKind::BackendFailure,
            operation: operation.into(),
            message: message.to_string(),
            span: None,
        }
    }

    pub fn tool_launch(phase: CodegenPhase, tool: &str, error: std::io::Error) -> Self {
        let missing = error.kind() == std::io::ErrorKind::NotFound;
        let mut diagnostic = Self::new(phase, format!("launch {tool}"), error);
        if missing {
            diagnostic.kind = CodegenErrorKind::MissingTool;
        }
        diagnostic
    }

    pub fn invalid_assembly(mut self) -> Self {
        self.kind = CodegenErrorKind::InvalidAssembly;
        self
    }

    pub fn with_span(mut self, span: Option<error::SourceSpan>) -> Self {
        self.span = span.map(Box::new);
        self
    }
}
impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}: {}", self.phase, self.operation, self.message)
    }
}
impl std::error::Error for CodegenError {}
impl From<inkwell::builder::BuilderError> for CodegenError {
    fn from(error: inkwell::builder::BuilderError) -> Self {
        Self::new(CodegenPhase::Lowering, "LLVM builder", error)
    }
}

/// Stage outputs alongside the destination. A failed producer cannot truncate
/// a previously successful artifact or leave a partial artifact at its path.
pub struct PendingOutput {
    path: PathBuf,
    destination: PathBuf,
}
impl PendingOutput {
    pub fn new(destination: &Path) -> Result<Self, CodegenError> {
        static NEXT: AtomicU64 = AtomicU64::new(0);
        let parent =
            destination.parent().filter(|p| !p.as_os_str().is_empty()).unwrap_or(Path::new("."));
        for _ in 0..128 {
            let path = parent.join(format!(
                ".wave-output-{}-{}.tmp",
                std::process::id(),
                NEXT.fetch_add(1, Ordering::Relaxed)
            ));
            match std::fs::OpenOptions::new().write(true).create_new(true).open(&path) {
                Ok(_) => return Ok(Self { path, destination: destination.to_owned() }),
                Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => continue,
                Err(e) => {
                    return Err(CodegenError::new(CodegenPhase::Emission, "create output", e))
                },
            }
        }
        Err(CodegenError::new(
            CodegenPhase::Emission,
            "create output",
            "temporary output name collision",
        ))
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn commit(self) -> Result<(), CodegenError> {
        let length = std::fs::metadata(&self.path)
            .map_err(|e| CodegenError::new(CodegenPhase::Emission, "inspect output", e))?
            .len();
        if length == 0 {
            return Err(CodegenError::new(
                CodegenPhase::Emission,
                "inspect output",
                "producer emitted an empty output",
            ));
        }
        std::fs::rename(&self.path, &self.destination)
            .map_err(|e| CodegenError::new(CodegenPhase::Emission, "publish output", e))
    }
}
impl Drop for PendingOutput {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.path);
    }
}
