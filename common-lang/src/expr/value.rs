use crate::expr::*;
use crate::id::Ident;
use crate::value::FieldValue;
use crate::{common_enum, common_struct};
use std::hash::Hash;

common_struct! {
    pub struct Invoke {
        pub func: Expr,
        pub args: Vec<Expr>,
    }
}
common_struct! {
    pub struct Invoke2 {
        pub func: ExprId,
        pub args: Vec<ExprId>,
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
    pub struct Select {
        pub obj: Expr,
        pub field: Ident,
        pub select: SelectType,
    }
}

common_struct! {
    pub struct Reference {
        pub referee: Expr,
        pub mutable: Option<bool>,
    }
}

common_struct! {
    pub struct Match {
        pub cases: Vec<MatchCase>,
    }
}

common_struct! {
    pub struct If {
        pub cases: Vec<MatchCase>,
    }
}

common_struct! {
    pub struct MatchCase {
        pub cond: Expr,
        pub body: Expr,
    }
}
common_enum! {
    pub enum ControlFlow {
        Continue,
        Break(Option<Expr>),
        Return(Option<Expr>),
        Into,
        IntoAndBreak(Option<Expr>),
    }
}
common_struct! {
    pub struct StructExpr {
        pub name: TypeExpr,
        pub fields: Vec<FieldValue>,
    }
}
