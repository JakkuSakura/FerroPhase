use fp_core::ast::Value;
use fp_core::diagnostics::report_error;
use fp_core::error::Result;
use fp_core::hir::typed as thir;
use fp_core::span::Span;
use std::collections::{HashMap, HashSet};

/// Represents a typed const block extracted from THIR.
#[derive(Debug, Clone)]
pub struct ConstBlock {
    pub id: u64,
    pub def_id: thir::ty::DefId,
    pub body_id: thir::BodyId,
    pub span: Span,
    pub dependencies: HashSet<u64>,
    pub state: ConstEvalState,
    pub result: Option<Value>,
}

impl ConstBlock {
    pub fn new(id: u64, def_id: thir::ty::DefId, body_id: thir::BodyId, span: Span) -> Self {
        Self {
            id,
            def_id,
            body_id,
            span,
            dependencies: HashSet::new(),
            state: ConstEvalState::NotEvaluated,
            result: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstEvalState {
    NotEvaluated,
    Evaluating,
    Evaluated,
    Error(String),
}

pub struct EvaluationContext {
    blocks: HashMap<u64, ConstBlock>,
    index_by_def: HashMap<thir::ty::DefId, u64>,
    dependencies: HashMap<u64, HashSet<u64>>,
    next_block_id: u64,
}

impl EvaluationContext {
    pub fn new() -> Self {
        Self {
            blocks: HashMap::new(),
            index_by_def: HashMap::new(),
            dependencies: HashMap::new(),
            next_block_id: 0,
        }
    }

    pub fn discover_const_blocks(&mut self, program: &thir::Program) -> Result<()> {
        self.blocks.clear();
        self.index_by_def.clear();
        self.dependencies.clear();
        self.next_block_id = 0;

        for item in &program.items {
            if let thir::ItemKind::Const(const_item) = &item.kind {
                if let Some(def_id) = const_item.def_id {
                    let block_id = self.next_id();
                    let mut block =
                        ConstBlock::new(block_id, def_id, const_item.body_id, item.span);
                    if let Some(body) = program.bodies.get(&const_item.body_id) {
                        let mut deps = HashSet::new();
                        collect_expr_dependencies(&body.value, &self.index_by_def, &mut deps);
                        block.dependencies = deps.clone();
                        self.dependencies.insert(block_id, deps);
                    } else {
                        return Err(report_error(format!(
                            "Missing THIR body {:?} for const block",
                            const_item.body_id
                        )));
                    }
                    self.index_by_def.insert(def_id, block_id);
                    self.blocks.insert(block_id, block);
                }
            }
        }

        // Recompute dependencies now that all blocks are known.
        for block in self.blocks.values_mut() {
            let mut deps = HashSet::new();
            if let Some(body) = program.bodies.get(&block.body_id) {
                collect_expr_dependencies(&body.value, &self.index_by_def, &mut deps);
            }
            block.dependencies = deps.clone();
            self.dependencies.insert(block.id, deps);
        }

        Ok(())
    }

    pub fn blocks(&self) -> &HashMap<u64, ConstBlock> {
        &self.blocks
    }

    pub fn get_dependencies(&self) -> &HashMap<u64, HashSet<u64>> {
        &self.dependencies
    }

    pub fn lookup_block_id(&self, def_id: thir::ty::DefId) -> Option<u64> {
        self.index_by_def.get(&def_id).copied()
    }

    pub fn begin_evaluation(&mut self, block_id: u64) -> Result<()> {
        let block = self
            .blocks
            .get_mut(&block_id)
            .ok_or_else(|| report_error(format!("Const block {} not found", block_id)))?;
        match block.state {
            ConstEvalState::NotEvaluated => {
                block.state = ConstEvalState::Evaluating;
                Ok(())
            }
            ConstEvalState::Evaluating => Err(report_error(format!(
                "Const block {} is already being evaluated",
                block_id
            ))),
            ConstEvalState::Evaluated => Err(report_error(format!(
                "Const block {} has already been evaluated",
                block_id
            ))),
            ConstEvalState::Error(ref msg) => Err(report_error(format!(
                "Const block {} is in error state ({}); cannot re-evaluate",
                block_id, msg
            ))),
        }
    }

    pub fn set_block_result(&mut self, block_id: u64, value: Value) -> Result<()> {
        let block = self
            .blocks
            .get_mut(&block_id)
            .ok_or_else(|| report_error(format!("Const block {} not found", block_id)))?;
        match block.state {
            ConstEvalState::Evaluating | ConstEvalState::NotEvaluated => {
                block.result = Some(value);
                block.state = ConstEvalState::Evaluated;
                Ok(())
            }
            ConstEvalState::Evaluated => Err(report_error(format!(
                "Const block {} already has a value",
                block_id
            ))),
            ConstEvalState::Error(ref msg) => Err(report_error(format!(
                "Const block {} is in error state ({}); cannot set result",
                block_id, msg
            ))),
        }
    }

    pub fn set_block_error(&mut self, block_id: u64, message: impl Into<String>) -> Result<()> {
        let block = self
            .blocks
            .get_mut(&block_id)
            .ok_or_else(|| report_error(format!("Const block {} not found", block_id)))?;
        block.state = ConstEvalState::Error(message.into());
        block.result = None;
        Ok(())
    }

    pub fn clone_results(&self) -> HashMap<thir::ty::DefId, Value> {
        self.blocks
            .values()
            .filter_map(|block| block.result.clone().map(|value| (block.def_id, value)))
            .collect()
    }

    fn next_id(&mut self) -> u64 {
        let id = self.next_block_id;
        self.next_block_id += 1;
        id
    }
}

fn collect_expr_dependencies(
    expr: &thir::Expr,
    index: &HashMap<thir::ty::DefId, u64>,
    deps: &mut HashSet<u64>,
) {
    match &expr.kind {
        thir::ExprKind::Literal(_) | thir::ExprKind::Local(_) | thir::ExprKind::VarRef { .. } => {}
        thir::ExprKind::Path(item_ref) => {
            if let Some(def_id) = item_ref.def_id {
                if let Some(block_id) = index.get(&def_id) {
                    deps.insert(*block_id);
                }
            }
        }
        thir::ExprKind::Binary(_, lhs, rhs)
        | thir::ExprKind::Assign { lhs, rhs }
        | thir::ExprKind::AssignOp { lhs, rhs, .. }
        | thir::ExprKind::LogicalOp { lhs, rhs, .. } => {
            collect_expr_dependencies(lhs, index, deps);
            collect_expr_dependencies(rhs, index, deps);
        }
        thir::ExprKind::Unary(_, val)
        | thir::ExprKind::Cast(val, _)
        | thir::ExprKind::Deref(val)
        | thir::ExprKind::Borrow { arg: val, .. }
        | thir::ExprKind::AddressOf { arg: val, .. }
        | thir::ExprKind::Use(val)
        | thir::ExprKind::Loop { body: val }
        | thir::ExprKind::Scope { value: val, .. } => collect_expr_dependencies(val, index, deps),
        thir::ExprKind::Call { fun, args, .. } => {
            collect_expr_dependencies(fun, index, deps);
            for arg in args {
                collect_expr_dependencies(arg, index, deps);
            }
        }
        thir::ExprKind::IntrinsicCall(call) => match &call.payload {
            fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                for arg in &template.args {
                    collect_expr_dependencies(arg, index, deps);
                }
                for kw in &template.kwargs {
                    collect_expr_dependencies(&kw.value, index, deps);
                }
            }
            fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                for arg in args {
                    collect_expr_dependencies(arg, index, deps);
                }
            }
        },
        thir::ExprKind::Index(base, idx) => {
            collect_expr_dependencies(base, index, deps);
            collect_expr_dependencies(idx, index, deps);
        }
        thir::ExprKind::Field { base, .. } => collect_expr_dependencies(base, index, deps),
        thir::ExprKind::If {
            cond,
            then,
            else_opt,
        } => {
            collect_expr_dependencies(cond, index, deps);
            collect_expr_dependencies(then, index, deps);
            if let Some(e) = else_opt {
                collect_expr_dependencies(e, index, deps);
            }
        }
        thir::ExprKind::Match { scrutinee, arms } => {
            collect_expr_dependencies(scrutinee, index, deps);
            for arm in arms {
                collect_expr_dependencies(&arm.body, index, deps);
                if let Some(guard) = &arm.guard {
                    collect_expr_dependencies(&guard.cond, index, deps);
                }
            }
        }
        thir::ExprKind::Block(block) => {
            for stmt in &block.stmts {
                collect_stmt_dependencies(stmt, index, deps);
            }
            if let Some(expr) = &block.expr {
                collect_expr_dependencies(expr, index, deps);
            }
        }
        thir::ExprKind::Let { expr, .. } => collect_expr_dependencies(expr, index, deps),
        thir::ExprKind::Break { value } | thir::ExprKind::Return { value } => {
            if let Some(val) = value {
                collect_expr_dependencies(val, index, deps);
            }
        }
        thir::ExprKind::UpvarRef { .. } | thir::ExprKind::Continue => {}
    }
}

fn collect_stmt_dependencies(
    stmt: &thir::Stmt,
    index: &HashMap<thir::ty::DefId, u64>,
    deps: &mut HashSet<u64>,
) {
    match &stmt.kind {
        thir::StmtKind::Expr(expr) => collect_expr_dependencies(expr, index, deps),
        thir::StmtKind::Let { initializer, .. } => {
            if let Some(init) = initializer {
                collect_expr_dependencies(init, index, deps);
            }
        }
    }
}
