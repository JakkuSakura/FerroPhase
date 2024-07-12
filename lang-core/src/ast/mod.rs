//! AST are trees, so Box<T> is fine

use crate::{common_enum, common_struct};
use eyre::Result;
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
    pub struct AstFile {
        pub path: PathBuf,
        pub module: Module,
    }
}

common_enum! {
    /// Tree is any syntax tree element
    pub enum AstTree {
        Item(AstItem),
        Expr(AstExpr),
        File(AstFile),
    }
}

pub trait AstProvider {
    fn get_ast_from_code(&self, cst: &str) -> Result<AstTree>;
    fn get_ast_from_file_path(&self, path: &Path) -> Result<AstTree>;
}
impl<D: AstDeserializer> AstProvider for D {
    fn get_ast_from_code(&self, cst: &str) -> Result<AstTree> {
        self.deserialize_tree(cst)
    }
    fn get_ast_from_file_path(&self, path: &Path) -> Result<AstTree> {
        self.deserialize_file(path).map(AstTree::File)
    }
}
