use std::fmt::{Debug, Display, Formatter};
mod builtins;

use crate::common_enum;
use crate::id::Ident;
pub use builtins::*;

common_enum! {
    #[derive(Copy)]
    pub enum BinOpKind {
        Add,
        AddTrait,
        Sub,
        Mul,
        Div,
        Mod,
        Gt,
        Lt,
        Ge,
        Le,
        Eq,
        Ne,
        Or,
        And,
        BitOr,
        BitAnd,
        BitXor,
    }

}
impl BinOpKind {
    pub fn is_ret_bool(&self) -> bool {
        match self {
            BinOpKind::Gt
            | BinOpKind::Lt
            | BinOpKind::Ge
            | BinOpKind::Le
            | BinOpKind::Eq
            | BinOpKind::Ne
            | BinOpKind::Or
            | BinOpKind::And => true,
            _ => false,
        }
    }
}
impl Display for BinOpKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOpKind::Add => write!(f, "+"),
            BinOpKind::AddTrait => write!(f, "+"),
            BinOpKind::Sub => write!(f, "-"),
            BinOpKind::Mul => write!(f, "*"),
            BinOpKind::Div => write!(f, "/"),
            BinOpKind::Mod => write!(f, "%"),
            BinOpKind::Gt => write!(f, ">"),
            BinOpKind::Lt => write!(f, "<"),
            BinOpKind::Ge => write!(f, ">="),
            BinOpKind::Le => write!(f, "<="),
            BinOpKind::Eq => write!(f, "=="),
            BinOpKind::Ne => write!(f, "!="),
            BinOpKind::Or => write!(f, "||"),
            BinOpKind::And => write!(f, "&&"),
            BinOpKind::BitOr => write!(f, "|"),
            BinOpKind::BitAnd => write!(f, "&"),
            BinOpKind::BitXor => write!(f, "^"),
        }
    }
}
common_enum! {
    pub enum UnOpKind {
        Not,
        Neg,
        Deref,
        Any(Ident),
    }
}
impl Display for UnOpKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOpKind::Not => write!(f, "!"),
            UnOpKind::Neg => write!(f, "-"),
            UnOpKind::Deref => write!(f, "*"),
            UnOpKind::Any(i) => write!(f, "{}", i),
        }
    }
}
