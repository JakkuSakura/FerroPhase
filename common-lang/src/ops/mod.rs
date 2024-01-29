use std::fmt::{Debug, Display, Formatter};
pub mod builtins;

use crate::common_enum;
use crate::id::Ident;
pub use builtins::*;

common_enum! {
    pub enum BinOpKind {
        Add,
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
        Any(Ident),
    }

}
impl BinOpKind {
    pub fn is_bool(&self) -> bool {
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
            BinOpKind::Any(i) => write!(f, "{}", i),
        }
    }
}
common_enum! {
    pub enum UnOpKind {
        Not,
        Neg,
        Any(Ident),
    }
}
impl Display for UnOpKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOpKind::Not => write!(f, "!"),
            UnOpKind::Neg => write!(f, "-"),
            UnOpKind::Any(i) => write!(f, "{}", i),
        }
    }
}

// common_enum! {
//     pub enum BinOp<T> {
//         Add(AddOp<T>),
//         Sub(SubOp<T>),
//         Mul(MulOp<T>),
//         Div(DivOp<T>),
//         Mod(ModOp<T>),
//         Gt(GtOp<T>),
//         Lt(LtOp<T>),
//         Gte(GteOp<T>),
//         Lte(LteOp<T>),
//         Eq(EqOp<T>),
//         Ne(NeOp<T>),
//         Any(AnyBinOp<T>),
//     }
//
// // }
//
// common_derives! {
//     pub struct AddOp<T> {
//         pub lhs: Box<T>,
//         pub rhs: Box<T>,
//     }
// }
//
// common_derives! {
//
//     pub struct SubOp<T> {
//         pub lhs: Box<T>,
//         pub rhs: Box<T>,
//     }
// }
// common_derives! {
//
//     pub struct MulOp<T> {
//         pub lhs: Box<T>,
//         pub rhs: Box<T>,
//     }
// }
// common_derives! {
//
//     pub struct DivOp<T> {
//         pub lhs: Box<T>,
//         pub rhs: Box<T>,
//     }
// }
// common_derives! {
//
//     pub struct ModOp<T> {
//         pub lhs: Box<T>,
//         pub rhs: Box<T>,
//     }
// }
//
// common_derives! {
//
//     pub struct GtOp<T> {
//         pub lhs: Box<T>,
//         pub rhs: Box<T>,
//     }
// }
// common_derives! {
//
//     pub struct LtOp<T> {
//         pub lhs: Box<T>,
//         pub rhs: Box<T>,
//     }
// }
// common_derives! {
//
//     pub struct GteOp<T> {
//         pub lhs: Box<T>,
//         pub rhs: Box<T>,
//     }
// }
// common_derives! {
//
//     pub struct LteOp<T> {
//         pub lhs: Box<T>,
//         pub rhs: Box<T>,
//     }
// }
// common_derives! {
//
//     pub struct EqOp<T> {
//         pub lhs: Box<T>,
//         pub rhs: Box<T>,
//     }
// }
// common_derives! {
//
//     pub struct NeOp<T> {
//         pub lhs: Box<T>,
//         pub rhs: Box<T>,
//     }
// }
// common_derives! {
//
//     pub struct AnyBinOp<T> {
//         pub kind: BinOpKind,
//         pub lhs: Box<T>,
//         pub rhs: Box<T>,
// }
// }
//
// common_derives! {
//
//     pub struct UnOp<T> {
//         pub kind: UnOpKind,
//         pub expr: Box<T>,
//     }
// }
// common_derives! {
//     pub struct NotOp<T> {
//         pub expr: Box<T>,
//     }
// }
// common_derives! {
//     pub struct NegOp<T> {
//         pub expr: Box<T>,
//     }
// }
// common_derives! {
//     pub struct AnyUnOp<T> {
//         pub kind: UnOpKind,
//         pub expr: Box<T>,
//     }
// }
