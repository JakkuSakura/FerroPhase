//! AST are trees, so Box<T> is fine

use crate::error::Result;
use crate::{common_enum, common_struct};
use std::path::PathBuf;

pub use deserialize::*;
pub use serialize::*;

mod attr;
mod deserialize;
mod expr;
mod ident;
mod item;
mod pretty;
mod serialize;
mod value;

pub use attr::*;
pub use expr::*;
pub use ident::*;
pub use item::*;
pub use pretty::*;
pub use value::*;

/// Shared slot for storing optional type annotations on AST nodes.
pub type TySlot = Option<Ty>;

/// Placeholder alias so LAST callers can begin depending on a dedicated type.
/// The alias intentionally points at the canonical AST until the lowered
/// representation is split out.
pub type Last = Node;
pub type BLast = BExpr;
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
common_enum! {
    /// Tree is any syntax tree element
    pub enum NodeKind {
        Item(Item),
        Expr(Expr),
        File(File),
    }
}

common_struct! {
    pub struct Node {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub ty: TySlot,
        #[serde(flatten)]
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
}

impl From<NodeKind> for Node {
    fn from(kind: NodeKind) -> Self {
        Node::new(kind)
    }
}

pub trait AstProvider {
    fn get_ast_from_code(&self, cst: &str) -> Result<Node>;
    fn get_ast_from_file_path(&self, path: &std::path::Path) -> Result<Node>;
}
impl<D: AstDeserializer> AstProvider for D {
    fn get_ast_from_code(&self, cst: &str) -> Result<Node> {
        Ok(self.deserialize_node(cst)?)
    }
    fn get_ast_from_file_path(&self, path: &std::path::Path) -> Result<Node> {
        Ok(self.deserialize_file_load(path).map(Node::file)?)
    }
}
