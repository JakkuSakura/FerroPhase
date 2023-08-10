use crate::ast::*;
use common::*;
pub trait Visitor {
    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Expr(expr) => self.visit_expr(expr),
            Stmt::Let(let_stmt) => self.visit_let(let_stmt),
            Stmt::Return(return_stmt) => self.visit_return(return_stmt),
            Stmt::Break(break_stmt) => self.visit_break(break_stmt),
            Stmt::Continue(continue_stmt) => self.visit_continue(continue_stmt),
            Stmt::If(if_stmt) => self.visit_if(if_stmt),
            Stmt::While(while_stmt) => self.visit_while(while_stmt),
            Stmt::For(for_stmt) => self.visit_for(for_stmt),
            Stmt::Block(block) => self.visit_block(block),
            Stmt::Fn(func) => self.visit_fn(func),
            Stmt::Module(module) => self.visit_module(module),
            Stmt::Program(program) => self.visit_program(program),
        }
    }
    fn visit_block(&mut self, block: &Block) -> Result<()>;
    fn visit_fn(&mut self, func: &Fn) -> Result<()>;
    fn visit_module(&mut self, module: &Module) -> Result<()>;
    fn visit_program(&mut self, program: &Program) -> Result<()>;
}
