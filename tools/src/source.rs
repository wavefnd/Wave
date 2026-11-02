// SPDX-License-Identifier: MPL-2.0
//! A tolerant source view for inventory/policy checks, not a replacement Wave parser.
use crate::{error, Result};
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Word,
    String,
    Char,
    Punct,
}
#[derive(Debug, Clone)]
pub struct Token {
    pub lexeme: String,
    pub line: usize,
    pub start: usize,
    pub end: usize,
    pub kind: Kind,
}

pub fn tokens(source: &str) -> Result<Vec<Token>> {
    let mut result = Vec::new();
    let mut index = 0;
    let mut line = 1;
    while index < source.len() {
        let start = index;
        let ch = source[index..].chars().next().unwrap();
        if ch.is_whitespace() {
            line += usize::from(ch == '\n');
            index += ch.len_utf8();
            continue;
        }
        if source[index..].starts_with("//") {
            index = source[index..].find('\n').map_or(source.len(), |end| index + end);
            continue;
        }
        if source[index..].starts_with("/*") {
            let mut depth = 1;
            index += 2;
            while index < source.len() && depth > 0 {
                if source[index..].starts_with("/*") {
                    depth += 1;
                    index += 2;
                } else if source[index..].starts_with("*/") {
                    depth -= 1;
                    index += 2;
                } else {
                    let ch = source[index..].chars().next().unwrap();
                    line += usize::from(ch == '\n');
                    index += ch.len_utf8();
                }
            }
            if depth > 0 {
                return Err(error("unterminated source block comment"));
            }
            continue;
        }
        let start_line = line;
        let (kind, lexeme) = if ch == '"' || ch == '\'' {
            index += 1;
            let mut value = String::new();
            let mut terminated = false;
            while index < source.len() {
                let next = source[index..].chars().next().unwrap();
                index += next.len_utf8();
                if next == ch {
                    terminated = true;
                    break;
                }
                if next == '\\' {
                    if index >= source.len() {
                        break;
                    }
                    let escape = source[index..].chars().next().unwrap();
                    index += escape.len_utf8();
                    if escape == 'x'
                        && index + 2 <= source.len()
                        && source.as_bytes()[index..index + 2].iter().all(u8::is_ascii_hexdigit)
                    {
                        value.push(u8::from_str_radix(&source[index..index + 2], 16)? as char);
                        index += 2;
                    } else {
                        value.push(match escape {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            c => c,
                        });
                    }
                } else {
                    line += usize::from(next == '\n');
                    value.push(next);
                }
            }
            if !terminated {
                return Err(error("unterminated source literal"));
            }
            (if ch == '"' { Kind::String } else { Kind::Char }, value)
        } else if ch.is_alphabetic() || ch == '_' {
            index += ch.len_utf8();
            while index < source.len() {
                let next = source[index..].chars().next().unwrap();
                if !next.is_alphanumeric() && next != '_' {
                    break;
                }
                index += next.len_utf8();
            }
            (Kind::Word, source[start..index].into())
        } else {
            index += ch.len_utf8();
            (Kind::Punct, ch.to_string())
        };
        result.push(Token { lexeme, line: start_line, start, end: index, kind });
    }
    Ok(result)
}

pub fn comments_masked(source: &str, tokens: &[Token]) -> String {
    let mut bytes = source.as_bytes().to_vec();
    let mut cursor = 0;
    for token in tokens {
        for b in &mut bytes[cursor..token.start] {
            if !b.is_ascii_whitespace() {
                *b = b' ';
            }
        }
        cursor = token.end;
    }
    for b in &mut bytes[cursor..] {
        if !b.is_ascii_whitespace() {
            *b = b' ';
        }
    }
    String::from_utf8(bytes).unwrap()
}
