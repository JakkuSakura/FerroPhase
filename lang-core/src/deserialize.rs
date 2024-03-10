use crate::ast::Type;
use crate::ast::{Expr, File, Item, Tree};
use common::*;

pub trait Deserializer {
    fn deserialize_tree(&self, code: &str) -> Result<Tree>;
    fn deserialize_expr(&self, code: &str) -> Result<Expr>;
    fn deserialize_item(&self, code: &str) -> Result<Item>;
    fn deserialize_file(&self, path: &std::path::Path) -> Result<File>;
    fn deserialize_type(&self, code: &str) -> Result<Type>;
}
