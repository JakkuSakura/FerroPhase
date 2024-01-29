use crate::expr::*;
use crate::expr::{Block, Statement};
use crate::id::{Ident, Locator};
use crate::utils::anybox::{AnyBox, AnyBoxable};
use crate::value::{FieldValue, Value, ValueUnit};
use crate::{common_enum, common_struct};
use std::hash::Hash;

common_enum! {
    /// Expr is an expression that returns a value
    /// aka ValueExpr
    pub enum Expr {
        Locator(Locator),
        Value(Box<Value>),
        Block(Block),
        Match(Match),
        If(If),
        Invoke(Box<Invoke>),
        Select(Box<Select>),
        Struct(Box<StructExpr>),
        Reference(Box<Reference>),
        Any(AnyBox),
    }

}
impl Expr {
    pub fn unit() -> Expr {
        Expr::Value(Value::Unit(ValueUnit).into())
    }
    pub fn is_unit(&self) -> bool {
        match self {
            Expr::Value(value) => value.is_unit(),
            _ => false,
        }
    }
    pub fn value(v: Value) -> Expr {
        match v {
            Value::Expr(expr) => expr,
            Value::Any(any) => Expr::Any(any),
            _ => Expr::Value(v.into()),
        }
    }
    pub fn ident(name: crate::id::Ident) -> Expr {
        Expr::Locator(crate::id::Locator::ident(name))
    }
    pub fn path(path: crate::id::Path) -> Expr {
        Expr::Locator(crate::id::Locator::path(path))
    }
    pub fn block(block: Block) -> Expr {
        if block.stmts.len() == 1 {
            let last = block.stmts.last().unwrap();
            if let Statement::Expr(expr) = last {
                return expr.clone();
            }
            if let Statement::SideEffect(expr) = last {
                if let Expr::Block(block) = &expr.expr {
                    let mut block = block.clone();
                    block.make_last_side_effect();
                    return Expr::block(block);
                }
            }
        }
        if block.stmts.is_empty() {
            return Expr::unit();
        }
        Expr::Block(block)
    }
    pub fn any<T: AnyBoxable>(any: T) -> Self {
        Self::Any(crate::utils::anybox::AnyBox::new(any))
    }
}

common_struct! {
    pub struct Invoke {
        pub func: Expr,
        pub args: Vec<Expr>,
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
