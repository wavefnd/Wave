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
    /// Optional parser-selected name token for declaration or member diagnostics.
    pub focus: Option<Box<SourceSpan>>,
}

impl SourceSpan {
    pub fn through(&self, last: &Self) -> Self {
        if self.file != last.file {
            return self.clone();
        }
        Self { end: last.end, end_line: last.end_line, end_column: last.end_column, ..self.clone() }
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

/// Logical source lines with original byte offsets; CRLF is one newline.
/// Retains the empty final line so EOF diagnostics have a source location.
pub fn source_lines(source: &str) -> Vec<(usize, &str)> {
    let bytes = source.as_bytes();
    let mut lines = Vec::new();
    let (mut start, mut i) = (0, 0);
    while i < bytes.len() {
        if matches!(bytes[i], b'\r' | b'\n') {
            lines.push((start, &source[start..i]));
            if bytes[i] == b'\r' && bytes.get(i + 1) == Some(&b'\n') {
                i += 1;
            }
            i += 1;
            start = i;
        } else {
            i += 1;
        }
    }
    lines.push((start, &source[start..]));
    lines
}
