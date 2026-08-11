//! AST are trees, so Box<T> is fine

use crate::query::QueryDocument;
use crate::span::Span;
use crate::workspace::WorkspaceDocument;
use crate::{common_enum, common_struct};
use std::path::PathBuf;

pub use deserialize::*;
pub use serialize::*;

mod attr;
mod deserialize;
mod expr;
mod ident;
mod item;
mod item_collection;
pub mod json;
mod macros;
mod pat;
mod pretty;
mod schema;
mod serialize;
pub mod snapshot;
pub mod sql;

mod value;

pub use attr::*;
pub use expr::*;
pub use ident::*;
pub use item::*;
pub use item_collection::*;
pub use json::*;
pub use macros::*;
pub use pat::*;
pub use schema::*;
pub use snapshot::*;
#[allow(dead_code)]
pub use value::*;

/// Shared slot for storing optional type annotations on AST nodes.
pub type TySlot = Option<Ty>;

common_struct! {
    pub struct File {
        pub path: PathBuf,
        #[serde(default)]
        pub attrs: Vec<Attribute>,
        #[serde(default)]
        pub collected_items: ItemChunk,
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

/// A block that can contain both items and statements, like Python's
/// module/class bodies where `def`, `class`, assignments, and expressions
/// are interleaved. One ordered list — same model as `ExprBlock`'s `stmts`
/// (function/block bodies already support this exact mix via `BlockStmt`)
/// — rather than separate item/statement lists, which would lose source
/// order.
common_struct! {
    pub struct ScriptBlock {
        #[serde(default)]
        pub span: Span,
        pub stmts: Vec<BlockStmt>,
    }
}
impl ScriptBlock {
    /// The expression the whole script consists of, if it's exactly one
    /// bare expression and nothing else (no items, no lets, no defers).
    pub fn as_single_expr(&self) -> Option<&Expr> {
        match self.stmts.as_slice() {
            [BlockStmt::Expr(stmt)] => Some(&stmt.expr),
            _ => None,
        }
    }
}
