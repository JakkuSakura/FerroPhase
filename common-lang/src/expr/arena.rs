use crate::expr::{Expr, ExprId};
use crate::utils::arena::{Arena, ArenaMeta};
use serde::{Deserialize, Serialize, Serializer};
use std::fmt::Debug;
use std::rc::Rc;

impl ArenaMeta for Expr {
    type Item = Self;
    type Id = ExprId;
    fn id_to_usize(id: Self::Id) -> usize {
        id as usize
    }
    fn usize_to_id(id: usize) -> Self::Id {
        id as ExprId
    }
}
pub struct ExprArena {
    arena: Arena<Expr>,
}

impl ExprArena {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
        }
    }
    pub fn alloc(&self, expr: Expr) -> ExprId {
        self.arena.alloc(expr)
    }
    pub fn get(&self, id: ExprId) -> Option<Expr> {
        self.arena.get(id)
    }
    pub fn get_thread_local() -> Rc<ExprArena> {
        thread_local! {
            static EXPR_ARENA: Rc<ExprArena> = Rc::new(ExprArena::new());
        }
        EXPR_ARENA.with(|arena| arena.clone())
    }
}
#[derive(Clone, Copy, Hash)]
pub struct AExpr {
    id: ExprId,
    _marker: std::marker::PhantomData<(Expr, Rc<ExprArena>)>,
}
impl AExpr {
    pub fn new(id: Expr) -> Self {
        Self {
            id: ExprArena::get_thread_local().alloc(id),
            _marker: std::marker::PhantomData,
        }
    }
    pub fn id(&self) -> ExprId {
        self.id
    }
    pub fn get(&self) -> Expr {
        ExprArena::get_thread_local().get(self.id).unwrap()
    }
}
impl From<Expr> for AExpr {
    fn from(expr: Expr) -> Self {
        Self::new(expr)
    }
}
impl Into<Expr> for AExpr {
    fn into(self) -> Expr {
        self.get()
    }
}
impl Serialize for AExpr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.get().serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for AExpr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let expr: Expr = Deserialize::deserialize(deserializer)?;
        Ok(Self::new(expr))
    }
}
impl PartialEq for AExpr {
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
}
impl Eq for AExpr {}
impl Debug for AExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.get().fmt(f)
    }
}
