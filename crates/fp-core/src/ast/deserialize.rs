use crate::ast::Ty;
use crate::ast::{Expr, File, Item};

pub trait AstDeserializer {
    fn deserialize_file(&self, code: &str) -> Result<File, crate::Error>;
    fn deserialize_expr(&self, code: &str) -> Result<Expr, crate::Error>;
    fn deserialize_item(&self, code: &str) -> Result<Item, crate::Error>;
    fn deserialize_file_load(&self, path: &std::path::Path) -> Result<File, crate::Error>;
    fn deserialize_type(&self, code: &str) -> Result<Ty, crate::Error>;
}
