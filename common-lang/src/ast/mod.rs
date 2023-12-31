use crate::common_enum;
use crate::common_struct;
use common::*;
use std::fmt::Debug;
use std::path::PathBuf;

mod anybox;
mod expr;
mod item;
mod locator;
mod pattern;
mod stmt;

pub use anybox::*;
pub use expr::*;
pub use item::*;
pub use locator::*;
pub use pattern::*;
pub use stmt::*;

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
