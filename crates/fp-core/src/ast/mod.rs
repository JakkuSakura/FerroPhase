//! AST are trees, so Box<T> is fine

use crate::common_struct;
use crate::span::Span;
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
pub mod module;
pub mod package;
mod pat;
pub mod path;
mod pretty;
pub mod program;
pub mod resolve;
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
pub use resolve::{
    AstResolver, Binding, DeclarationOutcome, DeclarationRules, LocalScope,
    ModuleTree as ResolveModuleTree, Namespace, ResolutionResult, ResolutionRules, Symbol,
};
pub use schema::*;
pub use snapshot::*;
#[allow(dead_code)]
pub use value::*;

/// Shared slot for storing optional type annotations on AST nodes.
pub type TySlot = Option<Ty>;

mod resolved_types {
    use super::{ItemId, PatternId, Ty};
    use std::cell::RefCell;
    use std::collections::HashMap;

    thread_local! {
        static RESOLVED_ITEM_TYPES: RefCell<HashMap<ItemId, Ty>> = RefCell::new(HashMap::new());
        static RESOLVED_PATTERN_TYPES: RefCell<HashMap<PatternId, Ty>> = RefCell::new(HashMap::new());
        static RESOLVED_PATTERN_OPS: RefCell<HashMap<PatternId, crate::intrinsics::PortableOp>> = RefCell::new(HashMap::new());
    }

    pub fn resolved_item_type(id: ItemId) -> Option<Ty> {
        RESOLVED_ITEM_TYPES.with(|cell| cell.borrow().get(&id).cloned())
    }

    pub fn set_resolved_item_type(id: ItemId, ty: Ty) {
        RESOLVED_ITEM_TYPES.with(|cell| {
            cell.borrow_mut().insert(id, ty);
        });
    }

    pub fn resolved_pattern_type(id: PatternId) -> Option<Ty> {
        RESOLVED_PATTERN_TYPES.with(|cell| cell.borrow().get(&id).cloned())
    }

    pub fn set_resolved_pattern_type(id: PatternId, ty: Ty) {
        RESOLVED_PATTERN_TYPES.with(|cell| {
            cell.borrow_mut().insert(id, ty);
        });
    }

    pub fn set_resolved_pattern_op(id: PatternId, op: crate::intrinsics::PortableOp) {
        RESOLVED_PATTERN_OPS.with(|cell| {
            cell.borrow_mut().insert(id, op);
        });
    }

    pub fn resolved_pattern_op(id: PatternId) -> Option<crate::intrinsics::PortableOp> {
        RESOLVED_PATTERN_OPS.with(|cell| cell.borrow().get(&id).cloned())
    }

}
pub use resolved_types::*;

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

common_struct! {
    /// A block that can contain both items and statements, like Python's
    /// module/class bodies where `def`, `class`, assignments, and expressions
    /// are interleaved. One ordered list — same model as `ExprBlock`'s `stmts`
    /// (function/block bodies already support this exact mix via `BlockStmt`)
    /// — rather than separate item/statement lists, which would lose source
    /// order.
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
