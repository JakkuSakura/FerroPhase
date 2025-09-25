use crate::ast::Ty;
use crate::ast::{Expr, File, Item, Node};

pub trait AstDeserializer {
    fn deserialize_node(&self, code: &str) -> Result<Node, crate::Error>;
    fn deserialize_expr(&self, code: &str) -> Result<Expr, crate::Error>;
    fn deserialize_item(&self, code: &str) -> Result<Item, crate::Error>;
    fn deserialize_file_load(&self, path: &std::path::Path) -> Result<File, crate::Error>;
    fn deserialize_type(&self, code: &str) -> Result<Ty, crate::Error>;
}
