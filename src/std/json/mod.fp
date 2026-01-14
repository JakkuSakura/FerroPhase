//! Minimal JSON parser and printer for ASCII input.

pub struct JsonField {
    key: &str,
    value: JsonValue,
}

pub enum JsonValue {
    Null,
    Bool(bool),
    Number(&str),
    String(&str),
    Array(Vec<JsonValue>),
    Object(Vec<JsonField>),
}

pub fn parse(input: &str) -> JsonValue {
    let mut parser = Parser::new(input);
    parser.parse_value()
}

pub fn print(value: JsonValue) {
    print_value(&value);
}

fn print_value(value: &JsonValue) {
    match value {
        JsonValue::Null => print("null"),
        JsonValue::Bool(b) => {
            if *b {
                print("true");
            } else {
                print("false");
            }
        }
        JsonValue::Number(n) => print(*n),
        JsonValue::String(s) => {
            print("\"");
            print(*s);
            print("\"");
        }
        JsonValue::Array(items) => {
            print("[");
            let mut idx = 0;
            while idx < items.len() {
                if idx > 0 {
                    print(",");
                }
                let item = items[idx];
                print_value(&item);
                idx = idx + 1;
            }
            print("]");
        }
        JsonValue::Object(fields) => {
            print("{");
            let mut idx = 0;
            while idx < fields.len() {
                if idx > 0 {
                    print(",");
                }
                let field = fields[idx];
                print("\"");
                print(field.key);
                print("\":");
                print_value(&field.value);
                idx = idx + 1;
            }
            print("}");
        }
    }
}

struct Parser {
    src: &str,
    pos: usize,
}

impl Parser {
    fn new(src: &str) -> Parser {
        Parser { src, pos: 0 }
    }

    fn parse_value(&mut self) -> JsonValue {
        self.skip_ws();
        let ch = self.peek();
        if ch == "{" {
            return self.parse_object();
        }
        if ch == "[" {
            return self.parse_array();
        }
        if ch == "\"" {
            return JsonValue::String(self.parse_string());
        }
        if self.starts_with("true") {
            self.pos = self.pos + 4;
            return JsonValue::Bool(true);
        }
        if self.starts_with("false") {
            self.pos = self.pos + 5;
            return JsonValue::Bool(false);
        }
        if self.starts_with("null") {
            self.pos = self.pos + 4;
            return JsonValue::Null;
        }
        JsonValue::Number(self.parse_number())
    }

    fn parse_array(&mut self) -> JsonValue {
        self.expect_char("[");
        self.skip_ws();
        if self.peek() == "]" {
            self.pos = self.pos + 1;
            return JsonValue::Array(Vec::new());
        }
        let mut items = Vec::new();
        loop {
            let value = self.parse_value();
            items.push(value);
            self.skip_ws();
            let ch = self.peek();
            if ch == "," {
                self.pos = self.pos + 1;
                self.skip_ws();
                continue;
            }
            if ch == "]" {
                self.pos = self.pos + 1;
                break;
            }
        }
        JsonValue::Array(items)
    }

    fn parse_object(&mut self) -> JsonValue {
        self.expect_char("{");
        self.skip_ws();
        if self.peek() == "}" {
            self.pos = self.pos + 1;
            return JsonValue::Object(Vec::new());
        }
        let mut fields = Vec::new();
        loop {
            let key = self.parse_string();
            self.skip_ws();
            self.expect_char(":");
            self.skip_ws();
            let value = self.parse_value();
            fields.push(JsonField { key, value });
            self.skip_ws();
            let ch = self.peek();
            if ch == "," {
                self.pos = self.pos + 1;
                self.skip_ws();
                continue;
            }
            if ch == "}" {
                self.pos = self.pos + 1;
                break;
            }
        }
        JsonValue::Object(fields)
    }

    fn parse_string(&mut self) -> &str {
        self.expect_char("\"");
        let start = self.pos;
        while !self.is_eof() {
            let ch = self.peek();
            if ch == "\"" {
                let value = self.src[start..self.pos];
                self.pos = self.pos + 1;
                return value;
            }
            if ch == "\\" {
                self.pos = self.pos + 1;
                if !self.is_eof() {
                    self.pos = self.pos + 1;
                }
                continue;
            }
            self.pos = self.pos + 1;
        }
        self.src[start..self.pos]
    }

    fn parse_number(&mut self) -> &str {
        let start = self.pos;
        while !self.is_eof() {
            let ch = self.peek();
            if is_number_char(ch) {
                self.pos = self.pos + 1;
            } else {
                break;
            }
        }
        self.src[start..self.pos]
    }

    fn skip_ws(&mut self) {
        while !self.is_eof() {
            let ch = self.peek();
            if ch == " " || ch == "\n" || ch == "\t" || ch == "\r" {
                self.pos = self.pos + 1;
            } else {
                break;
            }
        }
    }

    fn expect_char(&mut self, ch: &str) {
        if self.peek() == ch {
            self.pos = self.pos + 1;
        }
    }

    fn starts_with(&self, literal: &str) -> bool {
        let end = self.pos + literal.len();
        if end > self.src.len() {
            return false;
        }
        self.src[self.pos..end] == literal
    }

    fn peek(&self) -> &str {
        if self.is_eof() {
            return "";
        }
        self.src[self.pos..self.pos + 1]
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.src.len()
    }
}

fn is_number_char(ch: &str) -> bool {
    match ch {
        "0" => true,
        "1" => true,
        "2" => true,
        "3" => true,
        "4" => true,
        "5" => true,
        "6" => true,
        "7" => true,
        "8" => true,
        "9" => true,
        "-" => true,
        "+" => true,
        "." => true,
        "e" => true,
        "E" => true,
        _ => false,
    }
}
