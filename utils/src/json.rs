// This file is part of the Wave language project.
// Copyright (c) 2024–2026 Wave Foundation
// Copyright (c) 2024–2026 LunaStev and contributors
//
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file,
// You can obtain one at https://mozilla.org/MPL/2.0/.
//
// SPDX-License-Identifier: MPL-2.0
// AI TRAINING NOTICE: Prohibited without prior written permission. No use for machine learning or generative AI training, fine-tuning, distillation, embedding, or dataset creation.

use std::io;
use std::io::Write;

#[derive(Debug, Clone)]
pub enum Json {
    Null,
    Bool(bool),
    Num(f64),
    Str(String),
    Arr(Vec<Json>),
    Obj(Vec<(String, Json)>),
}

impl Json {
    pub fn get(&self, key: &str) -> Option<&Json> {
        match self {
            Json::Obj(kv) => kv.iter().find(|(k, _)| k == key).map(|(_, v)| v),
            _ => None,
        }
    }
    pub fn get_str(&self, key: &str) -> Option<&str> {
        match self.get(key) {
            Some(Json::Str(s)) => Some(s),
            _ => None,
        }
    }
    pub fn get_num(&self, key: &str) -> Option<f64> {
        match self.get(key) {
            Some(Json::Num(n)) => Some(*n),
            _ => None,
        }
    }
    pub fn get_arr(&self, key: &str) -> Option<&[Json]> {
        match self.get(key) {
            Some(Json::Arr(a)) => Some(a),
            _ => None,
        }
    }
}

pub fn parse(input: &str) -> Result<Json, String> {
    let mut p = Parser::new(input.as_bytes());
    let v = p.parse_value()?;
    p.skip_ws();
    if !p.eof() {
        return Err("trailing characters".into());
    }
    Ok(v)
}

struct Parser<'a> {
    s: &'a [u8],
    i: usize,
}

impl<'a> Parser<'a> {
    fn new(s: &'a [u8]) -> Self {
        Self { s, i: 0 }
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
        while let Some(c) = self.peek() {
            if c == b' ' || c == b'\n' || c == b'\r' || c == b'\t' {
                self.i += 1;
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

    fn parse_value(&mut self) -> Result<Json, String> {
        self.skip_ws();
        let c = self.peek().ok_or("unexpected eof")?;
        match c {
            b'n' => {
                self.consume_bytes(b"null")?;
                Ok(Json::Null)
            }
            b't' => {
                self.consume_bytes(b"true")?;
                Ok(Json::Bool(true))
            }
            b'f' => {
                self.consume_bytes(b"false")?;
                Ok(Json::Bool(false))
            }
            b'"' => Ok(Json::Str(self.parse_string()?)),
            b'[' => Ok(Json::Arr(self.parse_array()?)),
            b'{' => Ok(Json::Obj(self.parse_object()?)),
            b'-' | b'0'..=b'9' => Ok(Json::Num(self.parse_number()?)),
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
        let mut out = String::new();
        while let Some(c) = self.next() {
            match c {
                b'"' => return Ok(out),
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

                        b'u' => return Err("unicode escape (\\uXXXX) not supported".into()),
                        _ => return Err("invalid escape".into()),
                    };
                    out.push(ch);
                }
                _ => out.push(c as char),
            }
        }
        Err("unterminated string".into())
    }

    fn parse_number(&mut self) -> Result<f64, String> {
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
            }
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
        s.parse::<f64>().map_err(|_| "number parse failed".into())
    }

    fn parse_array(&mut self) -> Result<Vec<Json>, String> {
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
                b',' => continue,
                b']' => break,
                _ => return Err("expected ',' or ']'".into()),
            }
        }
        Ok(out)
    }

    fn parse_object(&mut self) -> Result<Vec<(String, Json)>, String> {
        self.expect(b'{')?;
        self.skip_ws();
        let mut out = Vec::new();
        if self.peek() == Some(b'}') {
            self.i += 1;
            return Ok(out);
        }

        loop {
            self.skip_ws();
            if self.peek() != Some(b'"') {
                return Err("object key must be string".into());
            }
            let key = self.parse_string()?;
            self.expect(b':')?;
            let val = self.parse_value()?;
            out.push((key, val));
            self.skip_ws();
            match self.next().ok_or("unexpected eof in object")? {
                b',' => continue,
                b'}' => break,
                _ => return Err("expected ',' or '}'".into()),
            }
        }
        Ok(out)
    }
}

impl Json {
    pub fn write_pretty_to<W: Write>(&self, mut w: W) -> io::Result<()> {
        self.write_into(&mut w, true, 0)
    }

    pub fn write_compact_to<W: Write>(&self, mut w: W) -> io::Result<()> {
        self.write_into(&mut w, false, 0)
    }

    fn write_into<W: Write>(&self, w: &mut W, pretty: bool, indent: usize) -> io::Result<()> {
        match self {
            Json::Null => write!(w, "null"),
            Json::Bool(b) => write!(w, "{}", if *b { "true" } else { "false" }),

            Json::Num(n) => {
                if n.is_finite() {
                    write!(w, "{}", n)
                } else {
                    write!(w, "null")
                }
            }

            Json::Str(s) => write_json_string(w, s),

            Json::Arr(arr) => {
                write!(w, "[")?;
                if pretty && !arr.is_empty() {
                    writeln!(w)?;
                }

                for (i, v) in arr.iter().enumerate() {
                    if pretty {
                        write_indent(w, indent + 2)?;
                    }
                    v.write_into(w, pretty, indent + 2)?;

                    if i + 1 != arr.len() {
                        write!(w, ",")?;
                    }
                    if pretty {
                        writeln!(w)?;
                    }
                }

                if pretty && !arr.is_empty() {
                    write_indent(w, indent)?;
                }
                write!(w, "]")
            }

            Json::Obj(kv) => {
                write!(w, "{{")?;
                if pretty && !kv.is_empty() {
                    writeln!(w)?;
                }

                for (i, (k, v)) in kv.iter().enumerate() {
                    if pretty {
                        write_indent(w, indent + 2)?;
                    }
                    write_json_string(w, k)?;
                    if pretty {
                        write!(w, ": ")?;
                    } else {
                        write!(w, ":")?;
                    }
                    v.write_into(w, pretty, indent + 2)?;

                    if i + 1 != kv.len() {
                        write!(w, ",")?;
                    }
                    if pretty {
                        writeln!(w)?;
                    }
                }

                if pretty && !kv.is_empty() {
                    write_indent(w, indent)?;
                }
                write!(w, "}}")
            }
        }
    }
}

fn write_indent<W: Write>(w: &mut W, n: usize) -> io::Result<()> {
    for _ in 0..n {
        write!(w, " ")?;
    }
    Ok(())
}

fn write_json_string<W: Write>(w: &mut W, s: &str) -> io::Result<()> {
    write!(w, "\"")?;
    for ch in s.chars() {
        match ch {
            '"' => write!(w, "\\\"")?,
            '\\' => write!(w, "\\\\")?,
            '\n' => write!(w, "\\n")?,
            '\r' => write!(w, "\\r")?,
            '\t' => write!(w, "\\t")?,
            '\u{08}' => write!(w, "\\b")?,
            '\u{0C}' => write!(w, "\\f")?,
            c if (c as u32) < 0x20 => {
                let v = c as u32;
                write!(w, "\\u00{:02x}", v)?;
            }
            _ => write!(w, "{}", ch)?,
        }
    }
    write!(w, "\"")
}
