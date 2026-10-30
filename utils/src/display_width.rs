// SPDX-License-Identifier: MPL-2.0
//! Terminal diagnostic layout; source locations stay in Unicode scalar columns.
//! Wide/fullwidth scalars occupy two cells, combining marks and format
//! characters zero, and ambiguous characters one. This is scalar cell width,
//! grapheme clusters are kept intact when clipping previews.
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

fn escaped_control(c: char) -> Option<String> {
    if c.is_control() || matches!(c, '\u{202a}'..='\u{202e}' | '\u{2066}'..='\u{2069}') {
        Some(match c {
            '\n' => "\\n".into(),
            '\r' => "\\r".into(),
            '\t' => "\\t".into(),
            _ => format!("\\u{{{:x}}}", c as u32),
        })
    } else {
        None
    }
}

/// Escape untrusted diagnostic fields without changing stored source locations
/// or JSON values. Compiler-owned color/style sequences are added afterwards.
pub fn terminal_text(text: &str) -> String {
    let mut rendered = String::new();
    for c in text.chars() {
        if let Some(escaped) = escaped_control(c) {
            rendered.push_str(&escaped);
        } else {
            rendered.push(c);
        }
    }
    rendered
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
    use unicode_segmentation::UnicodeSegmentation;
    use unicode_width::UnicodeWidthStr;
    const LIMIT: usize = 160;
    let start = column.saturating_sub(1);
    let end = start.saturating_add(length.max(1));
    let mut glyphs = Vec::new();
    let (mut scalar, mut cells) = (0usize, 0usize);
    let (mut caret_start, mut caret_end) = (None, None);
    for cluster in source.graphemes(true) {
        let count = cluster.chars().count();
        let text = if cluster == "\t" {
            " ".repeat(TAB_STOP - cells % TAB_STOP)
        } else {
            cluster
                .chars()
                .map(|c| escaped_control(c).unwrap_or_else(|| c.to_string()))
                .collect::<String>()
        };
        let width = UnicodeWidthStr::width(text.as_str());
        if scalar <= start && start < scalar + count {
            caret_start = Some(cells);
        }
        if scalar < end && scalar + count > start {
            caret_end = Some(cells + width);
        }
        glyphs.push((cells, cells + width, text));
        cells += width;
        scalar += count;
    }
    let focus = caret_start.unwrap_or(cells);
    let focus_end = caret_end.unwrap_or(focus).max(focus + 1);
    let desired = if cells <= LIMIT {
        0
    } else {
        focus
            .saturating_sub((LIMIT - 2).saturating_sub(focus_end - focus) / 2)
            .min(cells.saturating_sub(LIMIT - 1))
    };
    let first = glyphs
        .iter()
        .position(|&(left, right, _)| left >= desired || right > desired)
        .unwrap_or(glyphs.len());
    let left = glyphs.get(first).map_or(cells, |g| g.0);
    let leading = usize::from(first > 0);
    let mut used = leading;
    let mut text = if leading > 0 { "…".to_string() } else { String::new() };
    let mut visible_end = left;
    for (index, &(_, right, ref glyph)) in glyphs.iter().enumerate().skip(first) {
        let width = right - visible_end;
        let trailing = usize::from(index + 1 < glyphs.len());
        if used + width + trailing > LIMIT {
            text.push('…');
            break;
        }
        text.push_str(glyph);
        used += width;
        visible_end = right;
    }
    let caret_offset = (leading + focus.saturating_sub(left)).min(LIMIT - 1);
    let caret_width =
        focus_end.min(visible_end).saturating_sub(focus.max(left)).max(1).min(LIMIT - caret_offset);
    DiagnosticLine { text, caret_offset, caret_width }
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
        assert_eq!((line.caret_offset, line.caret_width), (2, 1));
        let line = diagnostic_line("\t文", 3, 100);
        assert_eq!((line.caret_offset, line.caret_width), (6, 1));
        assert_eq!(diagnostic_line("", 1, 0).caret_width, 1);
        assert_eq!(diagnostic_line("abc", 2, usize::MAX).caret_width, 2);
    }

    #[test]
    fn controls_are_visible_and_carets_follow_the_escaped_cells() {
        let line = diagnostic_line("a\u{1b}[2Jx", 6, 1);
        assert_eq!(line.text, "a\\u{1b}[2Jx");
        assert_eq!((line.caret_offset, line.caret_width), (10, 1));
        let line = diagnostic_line("a\u{1b}b", 2, 1);
        assert_eq!((line.caret_offset, line.caret_width), (1, 6));
        assert_eq!(terminal_text("a\nb\tc\r\u{7f}\u{202e}"), "a\\nb\\tc\\r\\u{7f}\\u{202e}");
        assert_eq!(terminal_text("한e\u{301}"), "한e\u{301}");
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

#[cfg(test)]
mod clipping_tests {
    use super::*;
    use unicode_width::UnicodeWidthStr;
    #[test]
    fn clipping_keeps_graphemes_tabs_and_highlight_inside_the_window() {
        for marker in ["e\u{301}", "👩‍💻", "🇰🇷", "한"] {
            let source = format!("{}\t{marker}{}", "x".repeat(500), "y".repeat(500));
            let preview = diagnostic_line(&source, 502, marker.chars().count());
            assert!(UnicodeWidthStr::width(preview.text.as_str()) <= 160);
            assert!(preview.text.starts_with('…') && preview.text.ends_with('…'));
            assert!(preview.text.contains(marker));
            assert!(preview.caret_offset + preview.caret_width <= 160);
        }
    }
}
