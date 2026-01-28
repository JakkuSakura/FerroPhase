use crate::ast::{Value, ValueList, ValueMap};
use crate::error::{Error, Result};
use std::fmt::Write;

pub fn parse_value(source: &str) -> Result<Value> {
    let mut parser = JsonParser::new(source);
    let value = parser.parse_value()?;
    parser.skip_ws();
    if !parser.is_eof() {
        return Err(Error::Message("trailing characters after JSON value".to_string()));
    }
    Ok(value)
}

pub fn to_string(value: &Value) -> Result<String> {
    let mut out = String::new();
    write_value(value, &mut out, 0, false)?;
    Ok(out)
}

pub fn to_string_pretty(value: &Value) -> Result<String> {
    let mut out = String::new();
    write_value(value, &mut out, 0, true)?;
    Ok(out)
}

fn write_value(value: &Value, out: &mut String, indent: usize, pretty: bool) -> Result<()> {
    match value {
        Value::Null(_) => out.push_str("null"),
        Value::Bool(b) => out.push_str(if b.value { "true" } else { "false" }),
        Value::Int(i) => out.push_str(&i.value.to_string()),
        Value::Decimal(d) => out.push_str(&d.value.to_string()),
        Value::String(s) => write_string(&s.value, out),
        Value::List(list) => write_array(list, out, indent, pretty)?,
        Value::Map(map) => write_object(map, out, indent, pretty)?,
        _ => {
            return Err(Error::Message(
                "json serializer only supports null, bool, number, string, list, map".to_string(),
            ))
        }
    }
    Ok(())
}

fn write_array(list: &ValueList, out: &mut String, indent: usize, pretty: bool) -> Result<()> {
    out.push('[');
    if list.values.is_empty() {
        out.push(']');
        return Ok(());
    }
    if pretty {
        out.push('\n');
    }
    for (idx, value) in list.values.iter().enumerate() {
        if idx > 0 {
            out.push(',');
            if pretty {
                out.push('\n');
            } else {
                out.push(' ');
            }
        }
        if pretty {
            write_indent(out, indent + 1);
        }
        write_value(value, out, indent + 1, pretty)?;
    }
    if pretty {
        out.push('\n');
        write_indent(out, indent);
    }
    out.push(']');
    Ok(())
}

fn write_object(map: &ValueMap, out: &mut String, indent: usize, pretty: bool) -> Result<()> {
    out.push('{');
    if map.entries.is_empty() {
        out.push('}');
        return Ok(());
    }
    if pretty {
        out.push('\n');
    }
    for (idx, entry) in map.entries.iter().enumerate() {
        if idx > 0 {
            out.push(',');
            if pretty {
                out.push('\n');
            } else {
                out.push(' ');
            }
        }
        if pretty {
            write_indent(out, indent + 1);
        }
        let key = match &entry.key {
            Value::String(s) => &s.value,
            _ => {
                return Err(Error::Message(
                    "json object keys must be strings".to_string(),
                ))
            }
        };
        write_string(key, out);
        out.push(':');
        if pretty {
            out.push(' ');
        }
        write_value(&entry.value, out, indent + 1, pretty)?;
    }
    if pretty {
        out.push('\n');
        write_indent(out, indent);
    }
    out.push('}');
    Ok(())
}

fn write_string(value: &str, out: &mut String) {
    out.push('"');
    for ch in value.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{08}' => out.push_str("\\b"),
            '\u{0C}' => out.push_str("\\f"),
            c if c.is_control() => {
                let _ = write!(out, "\\u{:04X}", c as u32);
            }
            c => out.push(c),
        }
    }
    out.push('"');
}

fn write_indent(out: &mut String, indent: usize) {
    for _ in 0..indent {
        out.push_str("  ");
    }
}

struct JsonParser<'a> {
    src: &'a [u8],
    pos: usize,
}

impl<'a> JsonParser<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            src: source.as_bytes(),
            pos: 0,
        }
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.src.len()
    }

    fn peek(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }

    fn bump(&mut self) -> Option<u8> {
        let ch = self.peek()?;
        self.pos += 1;
        Some(ch)
    }

    fn skip_ws(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == b' ' || ch == b'\n' || ch == b'\r' || ch == b'\t' {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    fn parse_value(&mut self) -> Result<Value> {
        self.skip_ws();
        match self.peek() {
            Some(b'{') => self.parse_object(),
            Some(b'[') => self.parse_array(),
            Some(b'"') => Ok(Value::string(self.parse_string()?)),
            Some(b't') => {
                self.expect_bytes(b"true")?;
                Ok(Value::bool(true))
            }
            Some(b'f') => {
                self.expect_bytes(b"false")?;
                Ok(Value::bool(false))
            }
            Some(b'n') => {
                self.expect_bytes(b"null")?;
                Ok(Value::null())
            }
            Some(b'-') | Some(b'0'..=b'9') => self.parse_number(),
            _ => Err(Error::Message("unexpected JSON token".to_string())),
        }
    }

    fn parse_object(&mut self) -> Result<Value> {
        self.expect_byte(b'{')?;
        self.skip_ws();
        if self.try_byte(b'}') {
            return Ok(Value::Map(ValueMap::new()));
        }
        let mut entries = Vec::new();
        loop {
            self.skip_ws();
            let key = self.parse_string()?;
            self.skip_ws();
            self.expect_byte(b':')?;
            let value = self.parse_value()?;
            entries.push((Value::string(key), value));
            self.skip_ws();
            if self.try_byte(b'}') {
                break;
            }
            self.expect_byte(b',')?;
        }
        Ok(Value::Map(ValueMap::from_pairs(entries)))
    }

    fn parse_array(&mut self) -> Result<Value> {
        self.expect_byte(b'[')?;
        self.skip_ws();
        if self.try_byte(b']') {
            return Ok(Value::List(ValueList::new(Vec::new())));
        }
        let mut items = Vec::new();
        loop {
            let value = self.parse_value()?;
            items.push(value);
            self.skip_ws();
            if self.try_byte(b']') {
                break;
            }
            self.expect_byte(b',')?;
        }
        Ok(Value::List(ValueList::new(items)))
    }

    fn parse_string(&mut self) -> Result<String> {
        self.expect_byte(b'"')?;
        let mut out = String::new();
        while let Some(ch) = self.bump() {
            match ch {
                b'"' => return Ok(out),
                b'\\' => {
                    let escaped = self
                        .bump()
                        .ok_or_else(|| Error::Message("unterminated escape".to_string()))?;
                    match escaped {
                        b'"' => out.push('"'),
                        b'\\' => out.push('\\'),
                        b'/' => out.push('/'),
                        b'b' => out.push('\u{08}'),
                        b'f' => out.push('\u{0C}'),
                        b'n' => out.push('\n'),
                        b'r' => out.push('\r'),
                        b't' => out.push('\t'),
                        b'u' => {
                            let code = self.parse_hex_u16()?;
                            if (0xD800..=0xDBFF).contains(&code) {
                                if self.try_sequence(b"\\u") {
                                    let low = self.parse_hex_u16()?;
                                    if (0xDC00..=0xDFFF).contains(&low) {
                                        let combined = 0x10000
                                            + (((code - 0xD800) as u32) << 10)
                                            + ((low - 0xDC00) as u32);
                                        if let Some(c) = char::from_u32(combined) {
                                            out.push(c);
                                        }
                                        continue;
                                    }
                                }
                            }
                            if let Some(c) = char::from_u32(code as u32) {
                                out.push(c);
                            }
                        }
                        _ => {
                            return Err(Error::Message("invalid escape sequence".to_string()))
                        }
                    }
                }
                _ => out.push(ch as char),
            }
        }
        Err(Error::Message("unterminated string".to_string()))
    }

    fn parse_number(&mut self) -> Result<Value> {
        let start = self.pos;
        if self.peek() == Some(b'-') {
            self.pos += 1;
        }
        self.consume_digits();
        if self.peek() == Some(b'.') {
            self.pos += 1;
            self.consume_digits();
        }
        if matches!(self.peek(), Some(b'e') | Some(b'E')) {
            self.pos += 1;
            if matches!(self.peek(), Some(b'+') | Some(b'-')) {
                self.pos += 1;
            }
            self.consume_digits();
        }
        let slice = std::str::from_utf8(&self.src[start..self.pos])
            .map_err(|_| Error::Message("invalid number".to_string()))?;
        if !slice.contains('.') && !slice.contains('e') && !slice.contains('E') {
            if let Ok(value) = slice.parse::<i64>() {
                return Ok(Value::int(value));
            }
        }
        let value = slice
            .parse::<f64>()
            .map_err(|_| Error::Message("invalid number".to_string()))?;
        Ok(Value::decimal(value))
    }

    fn consume_digits(&mut self) {
        while matches!(self.peek(), Some(b'0'..=b'9')) {
            self.pos += 1;
        }
    }

    fn expect_byte(&mut self, expected: u8) -> Result<()> {
        match self.bump() {
            Some(actual) if actual == expected => Ok(()),
            _ => Err(Error::Message("unexpected character".to_string())),
        }
    }

    fn try_byte(&mut self, expected: u8) -> bool {
        if self.peek() == Some(expected) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn expect_bytes(&mut self, expected: &[u8]) -> Result<()> {
        for &byte in expected {
            self.expect_byte(byte)?;
        }
        Ok(())
    }

    fn parse_hex_u16(&mut self) -> Result<u16> {
        let mut value: u16 = 0;
        for _ in 0..4 {
            let ch = self
                .bump()
                .ok_or_else(|| Error::Message("invalid unicode escape".to_string()))?;
            value = value
                .checked_mul(16)
                .ok_or_else(|| Error::Message("invalid unicode escape".to_string()))?;
            value += match ch {
                b'0'..=b'9' => (ch - b'0') as u16,
                b'a'..=b'f' => (ch - b'a' + 10) as u16,
                b'A'..=b'F' => (ch - b'A' + 10) as u16,
                _ => return Err(Error::Message("invalid unicode escape".to_string())),
            };
        }
        Ok(value)
    }

    fn try_sequence(&mut self, sequence: &[u8]) -> bool {
        if self.src.len() < self.pos + sequence.len() {
            return false;
        }
        if &self.src[self.pos..self.pos + sequence.len()] == sequence {
            self.pos += sequence.len();
            true
        } else {
            false
        }
    }
}
