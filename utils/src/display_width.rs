// SPDX-License-Identifier: MPL-2.0
//! Terminal diagnostic layout; source locations stay in Unicode scalar columns.
//! Wide/fullwidth scalars occupy two cells, combining marks and format
//! characters zero, and ambiguous characters one. This is scalar cell width,
//! not terminal-dependent grapheme/emoji ligature shaping.
#[path = "display_width_tables.rs"]
mod tables;
pub use tables::UNICODE_VERSION;

pub const TAB_STOP: usize = 4;

fn in_ranges(c: char, ranges: &[(u32, u32)]) -> bool {
    let value = c as u32;
    let i = ranges.partition_point(|&(_, end)| end < value);
    ranges.get(i).is_some_and(|&(start, _)| start <= value)
}

pub fn cell_width(c: char) -> usize {
    if in_ranges(c, tables::ZERO_WIDTH) {
        0
    } else if in_ranges(c, tables::WIDE) {
        2
    } else {
        1
    }
}

pub struct DiagnosticLine {
    pub text: String,
    pub caret_offset: usize,
    pub caret_width: usize,
}

/// Expand tabs relative to the source (excluding the diagnostic gutter), and
/// project a scalar-column range into terminal cells. EOF/zero-width ranges
/// still get one caret. A multiline/oversized range stops at the displayed line.
pub fn diagnostic_line(source: &str, column: usize, length: usize) -> DiagnosticLine {
    let start = column.saturating_sub(1);
    let end = start.saturating_add(length);
    let mut text = String::new();
    let mut cells = 0;
    let mut caret_offset = 0;
    let mut caret_width = 0;
    for (index, c) in source.chars().enumerate() {
        let width = if c == '\t' { TAB_STOP - cells % TAB_STOP } else { cell_width(c) };
        if c == '\t' {
            text.push_str(&" ".repeat(width));
        } else {
            text.push(c);
        }
        if index < start {
            caret_offset += width;
        }
        if index >= start && index < end {
            caret_width += width;
        }
        cells += width;
    }
    DiagnosticLine { text, caret_offset, caret_width: caret_width.max(1) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn aligns_tabs_wide_and_combining_text() {
        let line = diagnostic_line("\t한e\u{301}\t文x", 6, 1);
        assert_eq!(line.text, "    한e\u{301} 文x");
        assert_eq!((line.caret_offset, line.caret_width), (8, 2));
        let line = diagnostic_line("a\t文", 2, 2);
        assert_eq!((line.caret_offset, line.caret_width), (1, 5));
    }

    #[test]
    fn eof_and_zero_cell_ranges_remain_visible() {
        let line = diagnostic_line("文e\u{301}", 3, 1);
        assert_eq!((line.caret_offset, line.caret_width), (3, 1));
        let line = diagnostic_line("\t文", 3, 100);
        assert_eq!((line.caret_offset, line.caret_width), (6, 1));
        assert_eq!(diagnostic_line("", 1, 0).caret_width, 1);
        assert_eq!(diagnostic_line("abc", 2, usize::MAX).caret_width, 2);
    }

    #[test]
    fn pinned_tables_are_sorted_and_cover_supplementary_characters() {
        assert_eq!(UNICODE_VERSION, "17.0.0");
        for ranges in [tables::WIDE, tables::ZERO_WIDTH] {
            assert!(ranges.windows(2).all(|r| r[0].1 < r[1].0));
        }
        assert_eq!(cell_width('한'), 2);
        assert_eq!(cell_width('\u{20000}'), 2);
        assert_eq!(cell_width('\u{e0100}'), 0);
        assert_eq!(cell_width('é'), 1);
        assert_eq!(cell_width('\u{903}'), 0);
    }
}
