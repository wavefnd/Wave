//! UTF-8 byte ranges with one-based Unicode scalar line/column coordinates.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceSpan {
    pub file: String,
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
    pub end_line: usize,
    pub end_column: usize,
    /// Empty for physical syntax; generated syntax records its transformation.
    pub expansion: Vec<String>,
    /// Optional parser-selected name token for declaration diagnostics.
    pub focus: Option<Box<SourceSpan>>,
}

impl SourceSpan {
    pub fn through(&self, last: &Self) -> Self {
        if self.file != last.file {
            return self.clone();
        }
        Self {
            end: last.end,
            end_line: last.end_line,
            end_column: last.end_column,
            ..self.clone()
        }
    }

    pub fn generated(mut self, reason: impl Into<String>) -> Self {
        let reason = reason.into();
        if let Some(focus) = &mut self.focus {
            focus.expansion.push(reason.clone());
        }
        self.expansion.push(reason);
        self
    }
}
