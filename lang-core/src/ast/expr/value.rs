use crate::ast::{get_threadlocal_serializer, AstExpr, AstType, BExpr, BValue, Value};
use crate::ast::{FieldValue, ValueFunction};
use crate::id::{Ident, Locator};
use crate::ops::{BinOpKind, UnOpKind};
use crate::{common_enum, common_struct};
use std::fmt::{Display, Formatter};
use std::hash::Hash;
common_enum! {
    pub enum ExprInvokeTarget {
        Function(Locator),
        Type(AstType),
        Method(ExprSelect),
        Closure(ValueFunction),
        BinOp(BinOpKind),
        Expr(BExpr),
    }
}
impl ExprInvokeTarget {
    pub fn expr(expr: BExpr) -> Self {
        match &*expr {
            AstExpr::Locator(locator) => Self::Function(locator.clone()),
            AstExpr::Select(select) => Self::Method(select.clone()),
            AstExpr::Value(value) => Self::value(value.clone()),
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
    pub enum ExprSelectType {
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
        pub select: ExprSelectType,
    }
}

common_struct! {
    pub struct ExprIndex {
        pub expr: BExpr,
        pub index: BExpr,
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
        Break(Option<AstExpr>),
        #[from(ignore)]
        Return(Option<AstExpr>),
        Into,
        #[from(ignore)]
        IntoAndBreak(Option<AstExpr>),
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
common_struct! {
    pub struct ExprParen {
        pub expr: BExpr,
    }
}
common_enum! {
    pub enum ExprRangeLimit {
        Inclusive,
        Exclusive,
    }
}
common_struct! {
    pub struct ExprRange {
        pub start: Option<BExpr>,
        pub limit: ExprRangeLimit,
        pub end: Option<BExpr>,
        pub step: Option<BExpr>,
    }
}
