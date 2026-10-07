// SPDX-License-Identifier: MPL-2.0
//! Integrated WSON/JSON parser. Byte cursor handling retains input locations.
use super::*;
pub(super) fn parse(input: &str, format: Format) -> Result<Value, Error> {
    let mut parser = Parser::new(input.as_bytes(), format);
    let result = parser.parse_value().and_then(|value| {
        parser.skip_ws();
        if !parser.eof() {
            Err("trailing characters".into())
        } else {
            Ok(value)
        }
    });
    result.map_err(|message| Error::at(input, parser.i, message))
}
struct Parser<'a> {
    s: &'a [u8],
    i: usize,
    depth: usize,
    format: Format,
}

impl<'a> Parser<'a> {
    fn new(s: &'a [u8], format: Format) -> Self {
        Self { s, i: 0, depth: 0, format }
    }

    fn eof(&self) -> bool {
        self.i >= self.s.len()
    }

    fn peek(&self) -> Option<u8> {
        self.s.get(self.i).copied()
    }

    fn next(&mut self) -> Option<u8> {
        let c = self.peek()?;
        self.i += 1;
        Some(c)
    }

    fn skip_ws(&mut self) {
        loop {
            while matches!(self.peek(), Some(b' ' | b'\n' | b'\r' | b'\t')) {
                self.i += 1;
            }
            if self.format != Format::Wson {
                break;
            }
            if self.peek() == Some(b'#') || self.s.get(self.i..self.i + 2) == Some(b"//") {
                while !matches!(self.peek(), None | Some(b'\n' | b'\r')) {
                    self.i += 1;
                }
            } else if self.s.get(self.i..self.i + 2) == Some(b"/*") {
                let start = self.i;
                self.i += 2;
                while !self.eof() && self.s.get(self.i..self.i + 2) != Some(b"*/") {
                    self.i += 1;
                }
                if self.eof() {
                    self.i = start;
                    break;
                }
                self.i += 2;
            } else {
                break;
            }
        }
    }

    fn expect(&mut self, ch: u8) -> Result<(), String> {
        self.skip_ws();
        match self.next() {
            Some(c) if c == ch => Ok(()),
            _ => Err(format!("expected '{}'", ch as char)),
        }
    }

    fn parse_value(&mut self) -> Result<Value, String> {
        self.skip_ws();
        let c = self.peek().ok_or("unexpected eof")?;
        if self.format == Format::Wson && !matches!(c, b'\"' | b'[' | b'{') {
            return self.parse_wson_literal();
        }
        match c {
            b'n' => {
                self.consume_bytes(b"null")?;
                Ok(Value::Null)
            },
            b't' => {
                self.consume_bytes(b"true")?;
                Ok(Value::Bool(true))
            },
            b'f' => {
                self.consume_bytes(b"false")?;
                Ok(Value::Bool(false))
            },
            b'"' => Ok(Value::String(self.parse_string()?)),
            b'[' => Ok(Value::Array(self.parse_array()?)),
            b'{' => Ok(Value::Object(self.parse_object()?)),
            b'-' | b'0'..=b'9' => number(&self.parse_number()?),
            _ => Err("invalid value".into()),
        }
    }

    fn consume_bytes(&mut self, lit: &[u8]) -> Result<(), String> {
        self.skip_ws();
        if self.s.get(self.i..self.i + lit.len()) == Some(lit) {
            self.i += lit.len();
            Ok(())
        } else {
            Err(format!("expected '{}'", String::from_utf8_lossy(lit)))
        }
    }

    fn parse_string(&mut self) -> Result<String, String> {
        self.expect(b'"')?;
        let mut out = Vec::new();
        while let Some(c) = self.next() {
            match c {
                b'"' => return String::from_utf8(out).map_err(|_| "invalid UTF-8 string".into()),
                b'\\' => {
                    let esc = self.next().ok_or("unfinished escape")?;
                    let ch = match esc {
                        b'"' => '"',
                        b'\\' => '\\',
                        b'/' => '/',
                        b'b' => '\x08',
                        b'f' => '\x0c',
                        b'n' => '\n',
                        b'r' => '\r',
                        b't' => '\t',

                        b'u' => {
                            let first = self.unicode_unit()?;
                            let scalar = match first {
                                0xd800..=0xdbff => {
                                    if self.next() != Some(b'\\') || self.next() != Some(b'u') {
                                        return Err(
                                            "high surrogate requires a low surrogate".into()
                                        );
                                    }
                                    let low = self.unicode_unit()?;
                                    if !(0xdc00..=0xdfff).contains(&low) {
                                        return Err("invalid low surrogate".into());
                                    }
                                    0x10000 + ((first - 0xd800) << 10) + (low - 0xdc00)
                                },
                                0xdc00..=0xdfff => return Err("lone low surrogate".into()),
                                other => other,
                            };
                            char::from_u32(scalar).ok_or("invalid Unicode scalar")?
                        },
                        _ => return Err("invalid escape".into()),
                    };
                    out.extend_from_slice(ch.encode_utf8(&mut [0; 4]).as_bytes());
                },
                0..=0x1f => return Err("unescaped control byte in string".into()),
                _ => out.push(c),
            }
        }
        Err("unterminated string".into())
    }

    fn unicode_unit(&mut self) -> Result<u32, String> {
        let mut value = 0;
        for _ in 0..4 {
            let digit = self
                .next()
                .and_then(|byte| char::from(byte).to_digit(16))
                .ok_or("expected four hexadecimal digits in Unicode escape")?;
            value = value * 16 + digit;
        }
        Ok(value)
    }

    fn parse_number(&mut self) -> Result<String, String> {
        self.skip_ws();
        let start = self.i;

        if self.peek() == Some(b'-') {
            self.i += 1;
        }

        // int
        match self.peek() {
            Some(b'0') => self.i += 1,
            Some(b'1'..=b'9') => {
                self.i += 1;
                while matches!(self.peek(), Some(b'0'..=b'9')) {
                    self.i += 1;
                }
            },
            _ => return Err("invalid number".into()),
        }

        // frac
        if self.peek() == Some(b'.') {
            self.i += 1;
            if !matches!(self.peek(), Some(b'0'..=b'9')) {
                return Err("invalid fraction".into());
            }
            while matches!(self.peek(), Some(b'0'..=b'9')) {
                self.i += 1;
            }
        }

        // exp
        if matches!(self.peek(), Some(b'e') | Some(b'E')) {
            self.i += 1;
            if matches!(self.peek(), Some(b'+') | Some(b'-')) {
                self.i += 1;
            }
            if !matches!(self.peek(), Some(b'0'..=b'9')) {
                return Err("invalid exponent".into());
            }
            while matches!(self.peek(), Some(b'0'..=b'9')) {
                self.i += 1;
            }
        }

        let s = std::str::from_utf8(&self.s[start..self.i]).map_err(|_| "utf8 error")?;
        number(s)?;
        Ok(s.to_owned())
    }

    fn parse_wson_literal(&mut self) -> Result<Value, String> {
        let start = self.i;
        while let Some(c) = self.peek() {
            if matches!(c, b',' | b'}' | b']' | b'\r' | b'\n' | b'#')
                || self.s.get(self.i..self.i + 2) == Some(b"//")
                || self.s.get(self.i..self.i + 2) == Some(b"/*")
            {
                break;
            }
            self.i += 1;
        }
        let raw = std::str::from_utf8(&self.s[start..self.i]).map_err(|_| "invalid UTF-8")?.trim();
        if raw.eq_ignore_ascii_case("null") {
            return Ok(Value::Null);
        }
        if raw.eq_ignore_ascii_case("true") {
            return Ok(Value::Bool(true));
        }
        if raw.eq_ignore_ascii_case("false") {
            return Ok(Value::Bool(false));
        }
        if valid_date(raw) {
            return Ok(Value::Date(raw.into()));
        }
        if valid_datetime(raw) {
            return Ok(Value::DateTime(raw.into()));
        }
        let parts: Vec<_> = raw.split('.').collect();
        if parts.len() >= 3
            && parts.iter().all(|p| !p.is_empty() && p.bytes().all(|b| b.is_ascii_digit()))
        {
            return parts
                .iter()
                .map(|s| s.parse::<u32>().map_err(|_| "version component exceeds u32".into()))
                .collect::<Result<Vec<_>, String>>()
                .map(Value::Version);
        }
        let mut strict = Parser::new(raw.as_bytes(), Format::Json);
        let text = strict.parse_number()?;
        if !strict.eof() {
            return Err("invalid WSON literal".into());
        }
        number(&text)
    }

    fn parse_array(&mut self) -> Result<Vec<Value>, String> {
        if self.depth >= MAX_DEPTH {
            return Err("maximum nesting depth exceeded".into());
        }
        self.depth += 1;
        let result = self.parse_array_inner();
        self.depth -= 1;
        result
    }

    fn parse_array_inner(&mut self) -> Result<Vec<Value>, String> {
        self.expect(b'[')?;
        self.skip_ws();
        let mut out = Vec::new();
        if self.peek() == Some(b']') {
            self.i += 1;
            return Ok(out);
        }

        loop {
            let v = self.parse_value()?;
            out.push(v);
            self.skip_ws();
            match self.next().ok_or("unexpected eof in array")? {
                b',' => {
                    self.skip_ws();
                    if self.format == Format::Wson && self.peek() == Some(b']') {
                        self.i += 1;
                        break;
                    }
                    continue;
                },
                b']' => break,
                _ => return Err("expected ',' or ']'".into()),
            }
        }
        Ok(out)
    }

    fn parse_object(&mut self) -> Result<Vec<(String, Value)>, String> {
        if self.depth >= MAX_DEPTH {
            return Err("maximum nesting depth exceeded".into());
        }
        self.depth += 1;
        let result = self.parse_object_inner();
        self.depth -= 1;
        result
    }

    fn parse_object_inner(&mut self) -> Result<Vec<(String, Value)>, String> {
        self.expect(b'{')?;
        self.skip_ws();
        let mut out = Vec::new();
        let mut keys = std::collections::HashSet::new();
        if self.peek() == Some(b'}') {
            self.i += 1;
            return Ok(out);
        }

        loop {
            self.skip_ws();
            let key = if self.peek() == Some(b'"') {
                self.parse_string()?
            } else if self.format == Format::Wson {
                let start = self.i;
                while matches!(self.peek(), Some(c) if c.is_ascii_alphanumeric() || matches!(c, b'_' | b'-'))
                {
                    self.i += 1;
                }
                if self.i == start {
                    return Err("object key must be a string or identifier".into());
                }
                std::str::from_utf8(&self.s[start..self.i]).unwrap().to_owned()
            } else {
                return Err("object key must be string".into());
            };
            if !keys.insert(key.clone()) {
                return Err(format!("duplicate object key: {key}"));
            }
            self.skip_ws();
            if self.format == Format::Wson && self.peek() == Some(b'=') {
                self.i += 1;
            } else {
                self.expect(b':')?;
            }
            let val = self.parse_value()?;
            out.push((key, val));
            self.skip_ws();
            match self.next().ok_or("unexpected eof in object")? {
                b',' => {
                    self.skip_ws();
                    if self.format == Format::Wson && self.peek() == Some(b'}') {
                        self.i += 1;
                        break;
                    }
                    continue;
                },
                b'}' => break,
                _ => return Err("expected ',' or '}'".into()),
            }
        }
        Ok(out)
    }
}
