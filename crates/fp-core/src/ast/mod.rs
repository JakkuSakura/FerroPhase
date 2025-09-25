//! AST are trees, so Box<T> is fine

use crate::error::Result;
use crate::{common_enum, common_struct};
use std::path::{Path, PathBuf};

pub use deserialize::*;
pub use serialize::*;

mod attr;
mod deserialize;
mod expr;
mod item;
mod serialize;
mod value;

pub use attr::*;
pub use expr::*;
pub use item::*;
pub use value::*;
common_struct! {
    pub struct File {
        pub path: PathBuf,
        pub items: ItemChunk,
    }
}
impl std::fmt::Display for File {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "File: {}", self.path.display())?;
        for item in &self.items {
            writeln!(f, "{}", item)?;
        }
        Ok(())
    }
}
common_enum! {
    /// Tree is any syntax tree element
    pub enum Node {
        Item(Item),
        Expr(Expr),
        File(File),
    }
}

pub trait AstProvider {
    fn get_ast_from_code(&self, cst: &str) -> Result<Node>;
    fn get_ast_from_file_path(&self, path: &Path) -> Result<Node>;
}
impl<D: AstDeserializer> AstProvider for D {
    fn get_ast_from_code(&self, cst: &str) -> Result<Node> {
        Ok(self.deserialize_node(cst)?)
    }
    fn get_ast_from_file_path(&self, path: &Path) -> Result<Node> {
        Ok(self.deserialize_file_load(path).map(Node::File)?)
    }
}
