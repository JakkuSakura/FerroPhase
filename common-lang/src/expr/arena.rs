use crate::expr::{Expr, ExprId};
use crate::utils::arena::Arena;
use std::rc::Rc;

pub struct ExprArena {
    arena: Arena<Expr>,
}

impl ExprArena {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
        }
    }
    pub fn alloc(&mut self, expr: Expr) -> ExprId {
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
