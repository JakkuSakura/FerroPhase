use crate::ast::{BExpr, BValue, Expr, Type, Value};
use crate::ast::{FieldValue, ValueFunction};
use crate::id::{Ident, Locator};
use crate::ops::{BinOpKind, UnOpKind};
use crate::{common_enum, common_struct, get_threadlocal_serializer};
use std::fmt::{Display, Formatter};
use std::hash::Hash;
common_enum! {
    pub enum ExprInvokeTarget {
        Function(Locator),
        Type(Type),
        Method(ExprSelect),
        Closure(ValueFunction),
        BinOp(BinOpKind),
        Expr(BExpr),
    }
}
impl ExprInvokeTarget {
    pub fn expr(expr: BExpr) -> Self {
        match &*expr {
            Expr::Locator(locator) => Self::Function(locator.clone()),
            Expr::Select(select) => Self::Method(select.clone()),
            Expr::Value(value) => Self::value(value.clone()),
            _ => Self::Expr(expr),
        }
    }
    pub fn value(value: BValue) -> Self {
        match &*value {
            Value::Function(func) => Self::Closure(func.clone()),
            Value::BinOpKind(kind) => Self::BinOp(kind.clone()),
            Value::Type(ty) => Self::Type(ty.clone()),
            Value::Expr(expr) => Self::expr(expr.clone()),
            _ => panic!("Invalid value for ExprInvokeTarget::value: {}", value),
        }
    }
}

common_struct! {
    pub struct ExprInvoke {
        pub target: ExprInvokeTarget,
        pub args: Vec<BExpr>,
    }
}
impl Display for ExprInvoke {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = get_threadlocal_serializer().serialize_invoke(self).unwrap();

        f.write_str(&s)
    }
}
common_enum! {
    pub enum SelectType {
        Unknown,
        Field,
        Method,
        Function,
        Const,
    }

}

common_struct! {
    pub struct ExprSelect {
        pub obj: BExpr,
        pub field: Ident,
        pub select: SelectType,
    }
}

common_struct! {
    pub struct ExprReference {
        pub referee: BExpr,
        pub mutable: Option<bool>,
    }
}

common_struct! {
    pub struct ExprMatch {
        pub cases: Vec<ExprMatchCase>,
    }
}

common_struct! {
    pub struct ExprIf {
        pub cond: BExpr,
        pub then: BExpr,
        pub elze: Option<BExpr>,
    }
}
common_struct! {
    pub struct ExprLoop {
        pub label: Option<Ident>,
        pub body: BExpr,
    }
}

common_struct! {
    pub struct ExprMatchCase {
        pub cond: BExpr,
        pub body: BExpr,
    }
}

common_enum! {
    pub enum ControlFlow {
        Continue,
        #[from(ignore)]
        Break(Option<Expr>),
        #[from(ignore)]
        Return(Option<Expr>),
        Into,
        #[from(ignore)]
        IntoAndBreak(Option<Expr>),
    }
}
common_struct! {
    pub struct ExprInitStruct {
        pub name: BExpr, // TypeExpr
        pub fields: Vec<FieldValue>,
    }
}
common_struct! {
    pub struct ExprInitStructural {
        pub fields: Vec<FieldValue>,
    }
}

common_struct! {
    pub struct ExprBinOp {
        pub kind: BinOpKind,
        pub lhs: BExpr,
        pub rhs: BExpr,
    }
}
common_struct! {
    pub struct ExprUnOp {
        pub op: UnOpKind,
        pub val: BExpr,

    }
}

common_struct! {
    pub struct ExprAssign {
        pub target: BExpr,
        pub value: BExpr,
    }

}
