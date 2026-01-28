use crate::ast::{Value, ValueList, ValueMap, ValueMapEntry, ValueString};
use crate::error::{Error, Result};
use std::fmt::Write;

pub fn parse_value(source: &str) -> Result<Value> {
    let mut parser = TomlParser::new(source);
    let value = parser.parse_document()?;
    Ok(value)
}

pub fn to_string(value: &Value) -> Result<String> {
    let mut out = String::new();
    write_toml(value, &mut out)?;
    Ok(out)
}

pub fn to_string_pretty(value: &Value) -> Result<String> {
    let mut out = String::new();
    write_toml(value, &mut out)?;
    Ok(out)
}

fn write_toml(value: &Value, out: &mut String) -> Result<()> {
    let map = match value {
        Value::Map(map) => map,
        _ => {
            return Err(Error::Message(
                "toml serializer expects map at root".to_string(),
            ))
        }
    };
    write_table(out, None, map)?;
    Ok(())
}

fn write_table(out: &mut String, prefix: Option<&str>, map: &ValueMap) -> Result<()> {
    let mut nested_tables = Vec::new();
    let mut array_tables = Vec::new();

    for entry in &map.entries {
        let key = key_to_string(&entry.key)?;
        match &entry.value {
            Value::Map(child) => nested_tables.push((key, child)),
            Value::List(list) if list.values.iter().all(|v| matches!(v, Value::Map(_))) => {
                array_tables.push((key, list))
            }
            _ => {
                let full_key = join_key(prefix, &key);
                out.push_str(&full_key);
                out.push_str(" = ");
                write_inline_value(&entry.value, out)?;
                out.push('\n');
            }
        }
    }

    for (key, child) in nested_tables {
        let full = join_key(prefix, &key);
        out.push('\n');
        out.push('[');
        out.push_str(&full);
        out.push_str("]\n");
        write_table(out, Some(&full), child)?;
    }

    for (key, list) in array_tables {
        let full = join_key(prefix, &key);
        for item in &list.values {
            let child = match item {
                Value::Map(child) => child,
                _ => {
                    return Err(Error::Message(
                        "array table entries must be maps".to_string(),
                    ))
                }
            };
            out.push('\n');
            out.push_str("[[");
            out.push_str(&full);
            out.push_str("]]\n");
            write_table(out, Some(&full), child)?;
        }
    }

    Ok(())
}

fn write_inline_value(value: &Value, out: &mut String) -> Result<()> {
    match value {
        Value::String(s) => write_basic_string(&s.value, out),
        Value::Int(i) => out.push_str(&i.value.to_string()),
        Value::Decimal(d) => out.push_str(&d.value.to_string()),
        Value::Bool(b) => out.push_str(if b.value { "true" } else { "false" }),
        Value::List(list) => write_inline_array(list, out)?,
        Value::Map(map) => write_inline_table(map, out)?,
        Value::Null(_) => {
            return Err(Error::Message(
                "toml has no null literal".to_string(),
            ))
        }
        _ => {
            return Err(Error::Message(
                "toml serializer encountered unsupported value".to_string(),
            ))
        }
    }
    Ok(())
}

fn write_inline_array(list: &ValueList, out: &mut String) -> Result<()> {
    out.push('[');
    for (idx, value) in list.values.iter().enumerate() {
        if idx > 0 {
            out.push_str(", ");
        }
        write_inline_value(value, out)?;
    }
    out.push(']');
    Ok(())
}

fn write_inline_table(map: &ValueMap, out: &mut String) -> Result<()> {
    out.push('{');
    for (idx, entry) in map.entries.iter().enumerate() {
        if idx > 0 {
            out.push_str(", ");
        }
        let key = key_to_string(&entry.key)?;
        out.push_str(&key);
        out.push_str(" = ");
        write_inline_value(&entry.value, out)?;
    }
    out.push('}');
    Ok(())
}

fn write_basic_string(value: &str, out: &mut String) {
    out.push('"');
    for ch in value.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if c.is_control() => {
                let _ = write!(out, "\\u{:04X}", c as u32);
            }
            c => out.push(c),
        }
    }
    out.push('"');
}

fn join_key(prefix: Option<&str>, key: &str) -> String {
    match prefix {
        Some(p) if !p.is_empty() => format!("{p}.{key}"),
        _ => key.to_string(),
    }
}

fn key_to_string(value: &Value) -> Result<String> {
    match value {
        Value::String(s) => Ok(s.value.clone()),
        _ => Err(Error::Message("toml keys must be strings".to_string())),
    }
}

struct TableContext {
    path: Vec<String>,
    array_index: Option<usize>,
}

struct TomlParser<'a> {
    src: &'a str,
    pos: usize,
    current: TableContext,
}

impl<'a> TomlParser<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            src: source,
            pos: 0,
            current: TableContext {
                path: Vec::new(),
                array_index: None,
            },
        }
    }

    fn parse_document(&mut self) -> Result<Value> {
        let mut root = Value::Map(ValueMap::new());
        while !self.is_eof() {
            self.skip_ws_and_comments();
            if self.is_eof() {
                break;
            }
            if self.peek() == Some('[') {
                let (path, array) = self.parse_table_header()?;
                if array {
                    let index = ensure_array_table(&mut root, &path)?;
                    self.current = TableContext {
                        path,
                        array_index: Some(index),
                    };
                } else {
                    ensure_table(&mut root, &path)?;
                    self.current = TableContext {
                        path,
                        array_index: None,
                    };
                }
            } else {
                let key_path = self.parse_key_path()?;
                self.skip_ws();
                self.expect_char('=')?;
                self.skip_ws();
                let value = self.parse_value()?;
                insert_value(&mut root, &self.current, &key_path, value)?;
            }
            self.consume_line_end();
        }
        Ok(root)
    }

    fn parse_table_header(&mut self) -> Result<(Vec<String>, bool)> {
        self.expect_char('[')?;
        let array = if self.try_char('[') { true } else { false };
        self.skip_ws();
        let path = self.parse_key_path()?;
        self.skip_ws();
        if array {
            self.expect_char(']')?;
            self.expect_char(']')?;
        } else {
            self.expect_char(']')?;
        }
        Ok((path, array))
    }

    fn parse_key_path(&mut self) -> Result<Vec<String>> {
        let mut segments = Vec::new();
        loop {
            let key = self.parse_key()?;
            segments.push(key);
            self.skip_ws();
            if !self.try_char('.') {
                break;
            }
            self.skip_ws();
        }
        Ok(segments)
    }

    fn parse_key(&mut self) -> Result<String> {
        match self.peek() {
            Some('"') => {
                self.bump();
                self.parse_basic_string()
            }
            Some('\'') => {
                self.bump();
                self.parse_literal_string()
            }
            Some(_) => self.parse_bare_key(),
            None => Err(Error::Message("unexpected end of input".to_string())),
        }
    }

    fn parse_bare_key(&mut self) -> Result<String> {
        let start = self.pos;
        while let Some(ch) = self.peek() {
            if ch.is_alphanumeric() || ch == '_' || ch == '-' {
                self.bump();
            } else {
                break;
            }
        }
        if self.pos == start {
            return Err(Error::Message("expected key".to_string()));
        }
        Ok(self.src[start..self.pos].to_string())
    }

    fn parse_value(&mut self) -> Result<Value> {
        match self.peek() {
            Some('"') => {
                self.bump();
                Ok(Value::String(ValueString::new_owned(
                    self.parse_basic_string()?,
                )))
            }
            Some('\'') => {
                self.bump();
                Ok(Value::String(ValueString::new_owned(
                    self.parse_literal_string()?,
                )))
            }
            Some('[') => self.parse_array(),
            Some('{') => self.parse_inline_table(),
            Some(_) => self.parse_bare_value(),
            None => Err(Error::Message("unexpected end of input".to_string())),
        }
    }

    fn parse_array(&mut self) -> Result<Value> {
        self.expect_char('[')?;
        let mut values = Vec::new();
        loop {
            self.skip_ws_and_comments();
            if self.try_char(']') {
                break;
            }
            let value = self.parse_value()?;
            values.push(value);
            self.skip_ws_and_comments();
            if self.try_char(']') {
                break;
            }
            self.expect_char(',')?;
        }
        Ok(Value::List(ValueList::new(values)))
    }

    fn parse_inline_table(&mut self) -> Result<Value> {
        self.expect_char('{')?;
        let mut entries = Vec::new();
        loop {
            self.skip_ws();
            if self.try_char('}') {
                break;
            }
            let key = self.parse_key()?;
            self.skip_ws();
            self.expect_char('=')?;
            self.skip_ws();
            let value = self.parse_value()?;
            entries.push((Value::string(key), value));
            self.skip_ws();
            if self.try_char('}') {
                break;
            }
            self.expect_char(',')?;
        }
        Ok(Value::Map(ValueMap::from_pairs(entries)))
    }

    fn parse_bare_value(&mut self) -> Result<Value> {
        let token = self.parse_bare_token()?;
        if token == "true" {
            return Ok(Value::bool(true));
        }
        if token == "false" {
            return Ok(Value::bool(false));
        }
        if let Some(value) = parse_number(&token) {
            return Ok(value);
        }
        Ok(Value::string(token))
    }

    fn parse_bare_token(&mut self) -> Result<String> {
        let start = self.pos;
        while let Some(ch) = self.peek() {
            if ch == '\n' || ch == '\r' || ch == ',' || ch == ']' || ch == '}' || ch == '#' {
                break;
            }
            if ch.is_whitespace() {
                break;
            }
            self.bump();
        }
        if self.pos == start {
            return Err(Error::Message("expected value".to_string()));
        }
        Ok(self.src[start..self.pos].trim().to_string())
    }

    fn parse_basic_string(&mut self) -> Result<String> {
        let mut out = String::new();
        while let Some(ch) = self.bump() {
            match ch {
                '"' => return Ok(out),
                '\\' => {
                    let escaped = self
                        .bump()
                        .ok_or_else(|| Error::Message("unterminated escape".to_string()))?;
                    match escaped {
                        '"' => out.push('"'),
                        '\\' => out.push('\\'),
                        'n' => out.push('\n'),
                        'r' => out.push('\r'),
                        't' => out.push('\t'),
                        'u' => {
                            let code = self.parse_hex_u16()?;
                            if let Some(c) = char::from_u32(code as u32) {
                                out.push(c);
                            }
                        }
                        _ => {
                            return Err(Error::Message(
                                "invalid escape sequence".to_string(),
                            ))
                        }
                    }
                }
                other => out.push(other),
            }
        }
        Err(Error::Message("unterminated string".to_string()))
    }

    fn parse_literal_string(&mut self) -> Result<String> {
        let start = self.pos;
        while let Some(ch) = self.peek() {
            if ch == '\'' {
                let end = self.pos;
                self.bump();
                return Ok(self.src[start..end].to_string());
            }
            self.bump();
        }
        Err(Error::Message("unterminated string".to_string()))
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
                '0'..='9' => (ch as u16) - ('0' as u16),
                'a'..='f' => (ch as u16) - ('a' as u16) + 10,
                'A'..='F' => (ch as u16) - ('A' as u16) + 10,
                _ => return Err(Error::Message("invalid unicode escape".to_string())),
            };
        }
        Ok(value)
    }

    fn skip_ws(&mut self) {
        while matches!(self.peek(), Some(ch) if ch.is_whitespace()) {
            self.bump();
        }
    }

    fn skip_ws_and_comments(&mut self) {
        loop {
            self.skip_ws();
            if self.try_char('#') {
                while let Some(ch) = self.peek() {
                    self.bump();
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }
            break;
        }
    }

    fn consume_line_end(&mut self) {
        while matches!(self.peek(), Some('\n') | Some('\r')) {
            self.bump();
        }
    }

    fn peek(&self) -> Option<char> {
        self.src[self.pos..].chars().next()
    }

    fn bump(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.src.len()
    }

    fn try_char(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn expect_char(&mut self, expected: char) -> Result<()> {
        if self.try_char(expected) {
            Ok(())
        } else {
            Err(Error::Message("unexpected character".to_string()))
        }
    }
}

fn parse_number(token: &str) -> Option<Value> {
    let cleaned = token.replace('_', "");
    if cleaned.contains('.') || cleaned.contains('e') || cleaned.contains('E') {
        cleaned.parse::<f64>().ok().map(Value::decimal)
    } else {
        cleaned.parse::<i64>().ok().map(Value::int)
    }
}

fn ensure_table(root: &mut Value, path: &[String]) -> Result<()> {
    let mut current = root;
    for segment in path {
        let map = match current {
            Value::Map(map) => map,
            _ => {
                return Err(Error::Message(
                    "expected table while resolving path".to_string(),
                ))
            }
        };
        current = map_entry_or_insert(map, segment, Value::Map(ValueMap::new()));
    }
    Ok(())
}

fn ensure_array_table(root: &mut Value, path: &[String]) -> Result<usize> {
    let mut current = root;
    if path.is_empty() {
        return Err(Error::Message(
            "array table requires a non-empty path".to_string(),
        ));
    }
    for segment in &path[..path.len() - 1] {
        let map = match current {
            Value::Map(map) => map,
            _ => {
                return Err(Error::Message(
                    "expected table while resolving array path".to_string(),
                ))
            }
        };
        current = map_entry_or_insert(map, segment, Value::Map(ValueMap::new()));
    }
    let last = path.last().unwrap();
    let map = match current {
        Value::Map(map) => map,
        _ => {
            return Err(Error::Message(
                "expected table while resolving array path".to_string(),
            ))
        }
    };
    let entry = map_entry_or_insert(map, last, Value::List(ValueList::new(Vec::new())));
    let list = match entry {
        Value::List(list) => list,
        _ => {
            return Err(Error::Message(
                "array table conflicts with existing value".to_string(),
            ))
        }
    };
    list.values.push(Value::Map(ValueMap::new()));
    Ok(list.values.len() - 1)
}

fn insert_value(
    root: &mut Value,
    context: &TableContext,
    key_path: &[String],
    value: Value,
) -> Result<()> {
    let mut current = get_context_table(root, context)?;
    for segment in &key_path[..key_path.len().saturating_sub(1)] {
        let next = map_entry_or_insert(current, segment, Value::Map(ValueMap::new()));
        current = match next {
            Value::Map(map) => map,
            _ => {
                return Err(Error::Message(
                    "expected table while inserting value".to_string(),
                ))
            }
        };
    }
    if let Some(last) = key_path.last() {
        set_map_entry(current, last, value);
    }
    Ok(())
}

fn get_context_table<'a>(root: &'a mut Value, context: &TableContext) -> Result<&'a mut ValueMap> {
    let mut current = root;
    for segment in &context.path {
        let map = match current {
            Value::Map(map) => map,
            _ => {
                return Err(Error::Message(
                    "expected table while resolving context".to_string(),
                ))
            }
        };
        current = map_entry_or_insert(map, segment, Value::Map(ValueMap::new()));
    }
    if let Some(index) = context.array_index {
        let list = match current {
            Value::List(list) => list,
            _ => {
                return Err(Error::Message(
                    "expected array table while resolving context".to_string(),
                ))
            }
        };
        let value = list
            .values
            .get_mut(index)
            .ok_or_else(|| Error::Message("array table index out of range".to_string()))?;
        match value {
            Value::Map(map) => Ok(map),
            _ => Err(Error::Message("array table entry is not a table".to_string())),
        }
    } else {
        match current {
            Value::Map(map) => Ok(map),
            _ => Err(Error::Message("expected table for context".to_string())),
        }
    }
}

fn map_entry_or_insert<'a>(
    map: &'a mut ValueMap,
    key: &str,
    default: Value,
) -> &'a mut Value {
    if let Some(index) = map
        .entries
        .iter()
        .position(|entry| match &entry.key {
            Value::String(s) => s.value == key,
            _ => false,
        })
    {
        return &mut map.entries[index].value;
    }
    map.entries.push(ValueMapEntry::new(
        Value::String(ValueString::new_owned(key.to_string())),
        default,
    ));
    let index = map.entries.len() - 1;
    &mut map.entries[index].value
}

fn set_map_entry(map: &mut ValueMap, key: &str, value: Value) {
    if let Some(index) = map
        .entries
        .iter()
        .position(|entry| match &entry.key {
            Value::String(s) => s.value == key,
            _ => false,
        })
    {
        map.entries[index].value = value;
    } else {
        map.entries.push(ValueMapEntry::new(
            Value::String(ValueString::new_owned(key.to_string())),
            value,
        ));
    }
}
