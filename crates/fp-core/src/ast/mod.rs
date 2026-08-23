//! AST are trees, so Box<T> is fine

use crate::span::Span;
use crate::common_struct;
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
mod pat;
pub mod package;
pub mod path;
pub mod program;
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

/// A small side-table of resolved (typeck-derived) types keyed by an
/// `Expr`'s stable `ExprId`, for the narrow set of cases where a backend
/// serializer or AST-level materializer genuinely needs "what did this
/// arbitrary subexpression resolve to" and there is no annotation-shaped
/// AST position to promote the type into (unlike, say, a `let` binding or a
/// closure param, which get a real `PatternKind::Type` slot instead — see
/// `HirToAstLifter`'s `Local`/`Closure` lifting).
///
/// This intentionally replaces the old ad hoc `Expr::ty`/`Pattern::ty`/
/// `Item::ty` cache field (removed entirely): rather than threading a
/// side-table as an explicit parameter through every affected serializer's
/// entry point, it's exposed the same way this crate already exposes the
/// thread-local `AstSerializer` (see `get_threadlocal_serializer` in
/// `serialize.rs`) — a single process/thread-wide registry populated once
/// by `HirToAstLifter` right after lifting, then read by whichever backend
/// serializer or AST materializer pass runs next on the same thread.
mod resolved_types {
    use super::{ExprId, ItemId, PatternId, Ty};
    use std::cell::RefCell;
    use std::collections::HashMap;

    thread_local! {
        static RESOLVED_EXPR_TYPES: RefCell<HashMap<ExprId, Ty>> = RefCell::new(HashMap::new());
        // Separate tables (rather than reusing `RESOLVED_EXPR_TYPES` keyed
        // by a `u64` that happens to also back `ItemId`/`PatternId`) since
        // `ExprId`/`ItemId`/`PatternId` are independently-counted `u64`s —
        // reusing one map would risk cross-kind id collisions.
        static RESOLVED_ITEM_TYPES: RefCell<HashMap<ItemId, Ty>> = RefCell::new(HashMap::new());
        static RESOLVED_PATTERN_TYPES: RefCell<HashMap<PatternId, Ty>> = RefCell::new(HashMap::new());
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

    /// Replace the whole table — called once by `HirToAstLifter` after
    /// lifting a file/package, with every `ExprId` -> resolved `Ty` pair it
    /// collected during lifting.
    pub fn set_resolved_expr_types(map: HashMap<ExprId, Ty>) {
        RESOLVED_EXPR_TYPES.with(|cell| *cell.borrow_mut() = map);
    }

    /// Merge additional entries into the table (used when lifting proceeds
    /// package-by-package rather than in one pass).
    pub fn extend_resolved_expr_types(map: HashMap<ExprId, Ty>) {
        RESOLVED_EXPR_TYPES.with(|cell| cell.borrow_mut().extend(map));
    }

    pub fn resolved_expr_type(id: ExprId) -> Option<Ty> {
        RESOLVED_EXPR_TYPES.with(|cell| cell.borrow().get(&id).cloned())
    }

    /// Record (or overwrite) a single expr id's resolved type — used by
    /// passes other than `HirToAstLifter` that synthesize/rewrite AST nodes
    /// and need to stash a type for a later same-pass read where no
    /// annotation-shaped AST position exists (e.g. `ast_to_hir`'s
    /// closure-environment-struct synthesis, which used to stash these on
    /// the since-removed `Expr.ty`/`Item.ty` cache fields directly).
    pub fn set_resolved_expr_type(id: ExprId, ty: Ty) {
        RESOLVED_EXPR_TYPES.with(|cell| {
            cell.borrow_mut().insert(id, ty);
        });
    }

    pub fn clear_resolved_expr_types() {
        RESOLVED_EXPR_TYPES.with(|cell| cell.borrow_mut().clear());
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
