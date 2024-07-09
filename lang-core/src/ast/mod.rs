//! AST are trees, so Box<T> is fine

use crate::{common_enum, common_struct};
use std::path::PathBuf;

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
    fn get_ast_from_cst(&self, cst: &str) -> AstTree;
    fn get_ast_from_file(&self, path: &PathBuf) -> AstTree;
}
