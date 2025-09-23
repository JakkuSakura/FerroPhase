use fp_core::ast::{AstExpr, AstFile, AstValue};
use std::path::PathBuf;

pub fn literal_expr(value: i64) -> AstExpr {
    AstExpr::value(AstValue::int(value))
}

pub fn empty_file() -> AstFile {
    AstFile {
        path: PathBuf::from("<memory>"),
        items: Vec::new(),
    }
}
