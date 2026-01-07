// utils/formatx.rs
//
// Wave internal format utilities.
// This module replaces regex usage for placeholder detection.
// Supported pattern: `{ ... }` (non-nested, no escape)

/// Count `{...}` placeholders in the given string.
///
/// Equivalent to the regex pattern: `\{[^}]*\}`
///
/// Examples:
/// - "hello {}" -> 1
/// - "{a}{b}{c}" -> 3
/// - "{ not closed" -> 0
pub fn count_placeholders(input: &str) -> usize {
    let bytes = input.as_bytes();
    let mut i = 0;
    let mut count = 0;

    while i < bytes.len() {
        if bytes[i] == b'{' {
            let start = i;
            i += 1;

            while i < bytes.len() {
                if bytes[i] == b'}' {
                    count += 1;
                    i += 1;
                    break;
                }
                i += 1;
            }

            if i >= bytes.len() && bytes[start] == b'{' {
                break;
            }
        } else {
            i += 1;
        }
    }

    count
}
