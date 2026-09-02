#![allow(dead_code)]
use fp_core::ast::{Expr, File, Value};
use std::path::PathBuf;

pub fn literal_expr(value: i64) -> Expr {
    Expr::value(Value::int(value))
}

pub fn empty_file() -> File {
    File {
        path: PathBuf::from("<memory>"),
        attrs: Vec::new(),
        items: Vec::new(),
    }
}
