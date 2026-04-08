//! Minimal JSON parser and printer for ASCII input.

use std::option::Option;

pub struct Field {
    key: &str,
    value: Value,
}

pub enum Value {
    Null,
    Bool(bool),
    Number(Number),
    String(&str),
    Array(Vec<Value>),
    Object(Vec<Field>),
}

pub enum NumberKind {
    Int,
    UInt,
    Float,
}

pub struct Number {
    raw: &str,
    kind: NumberKind,
    int: i64,
    uint: u64,
    float: f64,
    has_int: bool,
    has_uint: bool,
    has_float: bool,
}

impl Number {
    pub fn as_i64(&self) -> Option<i64> {
        if self.has_int {
            Option::Some(self.int)
        } else {
            Option::None
        }
    }

    pub fn as_u64(&self) -> Option<u64> {
        if self.has_uint {
            Option::Some(self.uint)
        } else {
            Option::None
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        if self.has_float {
            Option::Some(self.float)
        } else {
            Option::None
        }
    }

    pub fn is_i64(&self) -> bool {
        self.as_i64().is_some()
    }

    pub fn is_u64(&self) -> bool {
        self.as_u64().is_some()
    }

    pub fn is_f64(&self) -> bool {
        self.as_f64().is_some()
    }

    pub fn to_string(&self) -> &str {
        self.raw
    }
}

impl Value {
    pub fn is_null(&self) -> bool {
        match self {
            Value::Null => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Value::Bool(_) => true,
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Value::Number(_) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Value::String(_) => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            Value::Array(_) => true,
            _ => false,
        }
    }

    pub fn is_object(&self) -> bool {
        match self {
            Value::Object(_) => true,
            _ => false,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(flag) => Option::Some(flag),
            _ => Option::None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::String(text) => Option::Some(text),
            _ => Option::None,
        }
    }

    pub fn as_number(&self) -> Option<Number> {
        match self {
            Value::Number(number) => Option::Some(number),
            _ => Option::None,
        }
    }

    pub fn as_array(&self) -> Option<Vec<Value>> {
        match self {
            Value::Array(values) => Option::Some(values),
            _ => Option::None,
        }
    }

    pub fn as_object(&self) -> Option<Vec<Field>> {
        match self {
            Value::Object(fields) => Option::Some(fields),
            _ => Option::None,
        }
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        match self {
            Value::Object(fields) => {
                let mut idx = 0;
                while idx < fields.len() {
                    let field = fields[idx];
                    if field.key == key {
                        return Option::Some(field.value);
                    }
                    idx = idx + 1;
                }
                Option::None
            }
            _ => Option::None,
        }
    }

    pub fn get_index(&self, index: i64) -> Option<Value> {
        match self {
            Value::Array(values) => {
                if index < 0 {
                    return Option::None;
                }
                if index >= values.len() {
                    return Option::None;
                }
                let idx = index as usize;
                Option::Some(values[idx])
            }
            _ => Option::None,
        }
    }
}

#[lang = "json_parse"]
pub fn parse(input: &str) -> Value { compile_error!("compiler intrinsic") }

pub fn is_null(value: Value) -> bool {
    value.is_null()
}

pub fn get_string(value: Value) -> &str {
    match value.as_str() {
        Option::Some(text) => text,
        Option::None => panic("expected json string"),
    }
}

pub fn get_array(value: Value) -> Vec<Value> {
    match value.as_array() {
        Option::Some(items) => items,
        Option::None => panic("expected json array"),
    }
}

pub fn get_object_field(value: Value, key: &str) -> Value {
    match value.get(key) {
        Option::Some(found) => found,
        Option::None => panic(f"missing json object field: {key}"),
    }
}

pub fn find_object_field(value: Value, key: &str) -> Value {
    match value.get(key) {
        Option::Some(found) => found,
        Option::None => Value::Null,
    }
}

pub fn print(value: Value) {
    print_value(&value);
}

fn print_value(value: &Value) {
    match value {
        Value::Null => print("null"),
        Value::Bool(b) => {
            if b {
                print("true");
            } else {
                print("false");
            }
        }
        Value::Number(n) => print(n.to_string()),
        Value::String(s) => {
            print("\"");
            print(s);
            print("\"");
        }
        Value::Array(items) => {
            print("[");
            let mut idx: i64 = 0;
            let items_len: i64 = items.len();
            while idx < items_len {
                if idx > 0 {
                    print(",");
                }
                let item = items[idx as usize];
                print_value(&item);
                idx = idx + 1;
            }

            print("]");
        }
        Value::Object(fields) => {
            print("{");
            let mut idx: i64 = 0;
            let fields_len: i64 = fields.len();
            while idx < fields_len {
                if idx > 0 {
                    print(",");
                }
                let field = fields[idx as usize];
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
