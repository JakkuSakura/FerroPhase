use crate::common_derives;
use common::*;
use std::fmt::Debug;
use std::path::PathBuf;

mod anybox;
mod expr;
mod item;
mod locator;
mod pattern;
mod stmt;
mod typing;

pub use anybox::*;
pub use expr::*;
pub use item::*;
pub use locator::*;
pub use pattern::*;
pub use stmt::*;
pub use typing::*;

common_derives! {
    /// Tree is any syntax tree element
    pub enum Tree {
        Item(Item),
        Expr(Expr),
        File(File),
    }
}

common_derives! {
    pub struct File {
        pub path: PathBuf,
        pub module: Module,
    }
}
