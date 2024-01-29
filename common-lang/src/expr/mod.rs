use crate::common_enum;
use crate::common_struct;

use common::*;
use std::fmt::Debug;
use std::path::PathBuf;

mod item;
mod stmt;
mod typing;
mod value;

pub use item::*;
pub use stmt::*;
pub use typing::*;
pub use value::*;

pub type ExprId = u64;

common_enum! {
    /// Tree is any syntax tree element
    pub enum Tree {
        Item(Item),
        Expr(Expr),
        File(File),
    }
}

common_struct! {
    pub struct File {
        pub path: PathBuf,
        pub module: Module,
    }
}
