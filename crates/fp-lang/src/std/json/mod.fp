//! Minimal JSON parser and printer for ASCII input.


pub struct Field {
    key: &str,
    value: Value,
}

pub enum Value {
    Null,
    Bool(bool),
    Number(Number),
    String(&str),
    Array(::std::alloc::Vec<Value>),
    Object(::std::alloc::Vec<Field>),
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
    pub fn as_i64(&self) -> ::std::option::Option<i64> {
        if self.has_int {
            ::std::option::Option::Some(self.int)
        } else {
            ::std::option::Option::None
        }
    }

    pub fn as_u64(&self) -> ::std::option::Option<u64> {
        if self.has_uint {
            ::std::option::Option::Some(self.uint)
        } else {
            ::std::option::Option::None
        }
    }

    pub fn as_f64(&self) -> ::std::option::Option<f64> {
        if self.has_float {
            ::std::option::Option::Some(self.float)
        } else {
            ::std::option::Option::None
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

    pub fn as_bool(&self) -> ::std::option::Option<bool> {
        match self {
            Value::Bool(flag) => ::std::option::Option::Some(flag),
            _ => ::std::option::Option::None,
        }
    }

    pub fn as_str(&self) -> ::std::option::Option<&str> {
        match self {
            Value::String(text) => ::std::option::Option::Some(text),
            _ => ::std::option::Option::None,
        }
    }

    pub fn as_number(&self) -> ::std::option::Option<Number> {
        match self {
            Value::Number(number) => ::std::option::Option::Some(number),
            _ => ::std::option::Option::None,
        }
    }

    pub fn as_array(&self) -> ::std::option::Option<::std::alloc::Vec<Value>> {
        match self {
            Value::Array(values) => ::std::option::Option::Some(values),
            _ => ::std::option::Option::None,
        }
    }

    pub fn as_object(&self) -> ::std::option::Option<::std::alloc::Vec<Field>> {
        match self {
            Value::Object(fields) => ::std::option::Option::Some(fields),
            _ => ::std::option::Option::None,
        }
    }

    pub fn get(&self, key: &str) -> ::std::option::Option<Value> {
        match self {
            Value::Object(fields) => {
                let mut idx = 0;
                while idx < fields.len() as i64 {
                    let field = fields[idx];
                    if field.key == key {
                        return ::std::option::Option::Some(field.value);
                    }
                    idx = idx + 1;
                }
                ::std::option::Option::None
            }
            _ => ::std::option::Option::None,
        }
    }

    pub fn get_index(&self, index: i64) -> ::std::option::Option<Value> {
        match self {
            Value::Array(values) => {
                if index < 0 {
                    return ::std::option::Option::None;
                }
                if index >= values.len() as i64 {
                    return ::std::option::Option::None;
                }
                ::std::option::Option::Some(values[index])
            }
            _ => ::std::option::Option::None,
        }
    }
}

#[op(func = "json_parse")]
pub fn parse(input: &str) -> Value { ::std::intrinsics::json::parse(input) }

pub fn is_null(value: Value) -> bool {
    value.is_null()
}

pub fn get_string(value: Value) -> &str {
    match value.as_str() {
        ::std::option::Option::Some(text) => text,
        ::std::option::Option::None => panic!("expected json string"),
    }
}

pub fn get_array(value: Value) -> ::std::alloc::Vec<Value> {
    match value.as_array() {
        ::std::option::Option::Some(items) => items,
        ::std::option::Option::None => panic!("expected json array"),
    }
}

pub fn get_object_field(value: Value, key: &str) -> Value {
    match value.get(key) {
        ::std::option::Option::Some(found) => found,
        ::std::option::Option::None => panic!(f"missing json object field: {key}"),
    }
}

pub fn find_object_field(value: Value, key: &str) -> Value {
    match value.get(key) {
        ::std::option::Option::Some(found) => found,
        ::std::option::Option::None => Value::Null,
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
            let items_len: i64 = items.len() as i64;
            while idx < items_len {
                if idx > 0 {
                    print(",");
                }
                let item = items[idx];
                print_value(&item);
                idx = idx + 1;
            }

            print("]");
        }
        Value::Object(fields) => {
            print("{");
            let mut idx: i64 = 0;
            let fields_len: i64 = fields.len() as i64;
            while idx < fields_len {
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
