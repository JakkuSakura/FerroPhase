use common::*;
use std::fmt::Debug;
use std::path::PathBuf;

mod anybox;
mod expr;
mod item;
mod pat;
mod stmt;
mod typing;

pub use anybox::*;
pub use expr::*;
pub use item::*;
pub use pat::*;
pub use stmt::*;
pub use typing::*;

/// Tree is any syntax tree element
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Tree {
    Item(Item),
    Expr(Expr),
    File(File),
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct File {
    pub path: PathBuf,
    pub module: Module,
}
