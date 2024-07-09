use crate::ast::AstType;
use crate::ast::{AstExpr, AstFile, AstItem, AstTree};
use common::*;

pub trait AstDeserializer {
    fn deserialize_tree(&self, code: &str) -> Result<AstTree>;
    fn deserialize_expr(&self, code: &str) -> Result<AstExpr>;
    fn deserialize_item(&self, code: &str) -> Result<AstItem>;
    fn deserialize_file(&self, path: &std::path::Path) -> Result<AstFile>;
    fn deserialize_type(&self, code: &str) -> Result<AstType>;
}
