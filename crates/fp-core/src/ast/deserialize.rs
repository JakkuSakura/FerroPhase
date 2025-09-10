use crate::ast::AstType;
use crate::ast::{AstExpr, AstFile, AstItem, AstNode};

pub trait AstDeserializer {
    fn deserialize_node(&self, code: &str) -> Result<AstNode, crate::Error>;
    fn deserialize_expr(&self, code: &str) -> Result<AstExpr, crate::Error>;
    fn deserialize_item(&self, code: &str) -> Result<AstItem, crate::Error>;
    fn deserialize_file_load(&self, path: &std::path::Path) -> Result<AstFile, crate::Error>;
    fn deserialize_type(&self, code: &str) -> Result<AstType, crate::Error>;
}
