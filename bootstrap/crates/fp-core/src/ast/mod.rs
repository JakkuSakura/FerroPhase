//! AST are trees, so Box<T> is fine

use crate::query::QueryDocument;
use crate::span::Span;
use crate::workspace::WorkspaceDocument;
use crate::common_struct;
use std::path::PathBuf;

pub use deserialize::*;
pub use serialize::*;

mod attr;
mod deserialize;
mod expr;
mod ident;
mod item;
// json module removed in bootstrap (no serde)
mod macros;
mod pat;
mod pretty;
mod schema;
mod serialize;
// snapshot module removed in bootstrap (no serde)

mod value;

pub use attr::*;
pub use expr::*;
pub use ident::*;
pub use item::*;
pub use macros::*;
pub use pat::*;
pub use schema::*;
#[allow(dead_code)]
pub use value::*;

/// Shared slot for storing optional type annotations on AST nodes.
pub type TySlot = Option<Ty>;

common_struct! {
    pub struct File {
        pub path: PathBuf,
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
/// Tree is any syntax tree element.
///
/// The enum deliberately distinguishes between:
/// - `Item`/`Expr`/`File`: the canonical FerroPhase AST hierarchy.
/// - `Query`: textual query documents (SQL, PRQL, ...).
/// - `Schema`: validation schemas such as JSON Schema.
/// - `Workspace`: metadata describing a Rust workspace.
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum NodeKind {
    Item(Item),
    Expr(Expr),
    File(File),
    Query(QueryDocument),
    Schema(schema::SchemaDocument),
    Workspace(WorkspaceDocument),
}

impl From<Item> for NodeKind {
    fn from(value: Item) -> Self {
        NodeKind::Item(value)
    }
}

impl From<Expr> for NodeKind {
    fn from(value: Expr) -> Self {
        NodeKind::Expr(value)
    }
}

impl From<File> for NodeKind {
    fn from(value: File) -> Self {
        NodeKind::File(value)
    }
}

impl From<QueryDocument> for NodeKind {
    fn from(value: QueryDocument) -> Self {
        NodeKind::Query(value)
    }
}

impl From<schema::SchemaDocument> for NodeKind {
    fn from(value: schema::SchemaDocument) -> Self {
        NodeKind::Schema(value)
    }
}

impl From<WorkspaceDocument> for NodeKind {
    fn from(value: WorkspaceDocument) -> Self {
        NodeKind::Workspace(value)
    }
}

common_struct! {
    pub struct Node {
        pub ty: TySlot,
        pub kind: NodeKind,
    }
}

impl Node {
    pub fn new(kind: NodeKind) -> Self {
        Self { ty: None, kind }
    }

    pub fn with_ty(kind: NodeKind, ty: TySlot) -> Self {
        Self { ty, kind }
    }

    pub fn ty(&self) -> Option<&Ty> {
        self.ty.as_ref()
    }

    pub fn ty_mut(&mut self) -> &mut TySlot {
        &mut self.ty
    }

    pub fn set_ty(&mut self, ty: Ty) {
        self.ty = Some(ty);
    }

    pub fn kind(&self) -> &NodeKind {
        &self.kind
    }

    pub fn kind_mut(&mut self) -> &mut NodeKind {
        &mut self.kind
    }

    pub fn span(&self) -> Span {
        match &self.kind {
            NodeKind::Item(item) => item.span(),
            NodeKind::Expr(expr) => expr.span(),
            _ => Span::null(),
        }
    }

    pub fn with_ty_slot(mut self, ty: TySlot) -> Self {
        self.ty = ty;
        self
    }

    pub fn file(file: File) -> Self {
        Node::from(NodeKind::File(file))
    }

    pub fn item(item: Item) -> Self {
        Node::from(NodeKind::Item(item))
    }

    pub fn expr(expr: Expr) -> Self {
        Node::from(NodeKind::Expr(expr))
    }

    pub fn query(query: QueryDocument) -> Self {
        Node::from(NodeKind::Query(query))
    }

    pub fn schema(schema: schema::SchemaDocument) -> Self {
        Node::from(NodeKind::Schema(schema))
    }

    pub fn workspace(workspace: WorkspaceDocument) -> Self {
        Node::from(NodeKind::Workspace(workspace))
    }
}

impl From<NodeKind> for Node {
    fn from(kind: NodeKind) -> Self {
        Node::new(kind)
    }
}
