use crate::expr::Expr;
use crate::{common_enum, common_struct};
use std::path::PathBuf;

mod item;
pub use item::*;

common_struct! {
    pub struct File {
        pub path: PathBuf,
        pub module: Module,
    }
}

common_enum! {
    /// Tree is any syntax tree element
    pub enum Tree {
        Item(Item),
        Expr(Expr),
        File(File),
    }
}
