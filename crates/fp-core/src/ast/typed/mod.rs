use crate::ast::{self, Node};
use crate::pretty::{PrettyCtx, PrettyPrintable};
use std::fmt::{self, Formatter};

/// Typed AST programme used by transpile targets.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    root: Node,
}

impl Program {
    pub fn new(root: Node) -> Self {
        Self { root }
    }

    pub fn root(&self) -> &Node {
        &self.root
    }

    pub fn into_inner(self) -> Node {
        self.root
    }
}

impl From<Node> for Program {
    fn from(root: Node) -> Self {
        Program::new(root)
    }
}

pub type Expr = ast::Expr;
pub type Item = ast::Item;
pub type File = ast::File;

impl PrettyPrintable for Program {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
        self.root.fmt_pretty(f, ctx)
    }
}
