use crate::ast::AstType;
use crate::ast::{AstExpr, AstFile, AstItem, AstNode};
use common::*;

pub trait AstDeserializer {
    fn deserialize_node(&self, code: &str) -> Result<AstNode>;
    fn deserialize_expr(&self, code: &str) -> Result<AstExpr>;
    fn deserialize_item(&self, code: &str) -> Result<AstItem>;
    fn deserialize_file_load(&self, path: &std::path::Path) -> Result<AstFile>;
    fn deserialize_type(&self, code: &str) -> Result<AstType>;
}
