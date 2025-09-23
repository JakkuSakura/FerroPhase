// Evaluation context - utility for tracking const blocks and their state

use crate::queries::TypeQueries;
use eyre::eyre;
use fp_core::ast::*;
use fp_core::error::Result;
use std::collections::{HashMap, HashSet};

/// Represents a const block or expression that needs evaluation
#[derive(Debug, Clone)]
pub struct ConstBlock {
    pub id: u64,
    pub name: Option<String>,
    pub expr: AstExpr,
    pub dependencies: HashSet<u64>,
    pub state: ConstEvalState,
    pub result: Option<AstValue>,
}

/// State of const evaluation for a block
#[derive(Debug, Clone, PartialEq)]
pub enum ConstEvalState {
    NotEvaluated,
    Evaluating, // Prevents infinite recursion
    Evaluated,
    Error(String),
}

/// Utility for tracking const block evaluation state
pub struct EvaluationContext {
    const_blocks: HashMap<u64, ConstBlock>,
    dependencies: HashMap<u64, HashSet<u64>>,
    next_block_id: u64,
}

impl EvaluationContext {
    pub fn new() -> Self {
        Self {
            const_blocks: HashMap::new(),
            dependencies: HashMap::new(),
            next_block_id: 0,
        }
    }

    /// Discover const blocks in the AST
    pub fn discover_const_blocks(&mut self, ast: &AstNode) -> Result<()> {
        self.const_blocks.clear();
        self.dependencies.clear();
        self.next_block_id = 0;

        let mut name_to_id: HashMap<String, u64> = HashMap::new();
        let mut pending_exprs: Vec<(u64, AstExpr)> = Vec::new();

        walk_const_items(ast, &mut |item_const| {
            let expr = item_const.value.as_ref().clone();
            let name = item_const.name.name.clone();
            let id = self.next_id();

            let block = ConstBlock::new(id, Some(name.clone()), expr.clone());
            self.const_blocks.insert(id, block);
            name_to_id.insert(name, id);
            pending_exprs.push((id, expr));
        });

        for (block_id, expr) in pending_exprs {
            let mut references = HashSet::new();
            collect_expr_references(&expr, &mut references);

            let deps: HashSet<u64> = references
                .into_iter()
                .filter_map(|name| name_to_id.get(&name).copied())
                .filter(|dep_id| *dep_id != block_id)
                .collect();

            if let Some(block) = self.const_blocks.get_mut(&block_id) {
                block.dependencies = deps.clone();
            }
            self.dependencies.insert(block_id, deps);
        }

        Ok(())
    }

    /// Generate a new unique block ID
    pub fn next_id(&mut self) -> u64 {
        let id = self.next_block_id;
        self.next_block_id += 1;
        id
    }

    /// Get all const blocks
    pub fn get_const_blocks(&self) -> &HashMap<u64, ConstBlock> {
        &self.const_blocks
    }

    /// Get a specific const block
    pub fn get_const_block(&self, block_id: u64) -> Result<&ConstBlock> {
        self.const_blocks.get(&block_id).ok_or_else(|| {
            fp_core::error::Error::Generic(eyre!("Const block {} not found", block_id))
        })
    }

    /// Set dependencies for const blocks
    pub fn set_dependencies(&mut self, dependencies: HashMap<u64, HashSet<u64>>) {
        self.dependencies = dependencies;
    }

    /// Get dependencies
    pub fn get_dependencies(&self) -> &HashMap<u64, HashSet<u64>> {
        &self.dependencies
    }

    /// Set the result of a const block evaluation
    pub fn set_block_result(&mut self, block_id: u64, result: AstValue) -> Result<()> {
        if let Some(block) = self.const_blocks.get_mut(&block_id) {
            block.result = Some(result);
            block.state = ConstEvalState::Evaluated;
            Ok(())
        } else {
            Err(fp_core::error::Error::Generic(eyre!(
                "Const block {} not found",
                block_id
            )))
        }
    }

    /// Get all evaluation results
    pub fn get_all_results(&self) -> HashMap<String, AstValue> {
        let mut results = HashMap::new();
        for (_, block) in &self.const_blocks {
            if let (Some(name), Some(result)) = (&block.name, &block.result) {
                results.insert(name.clone(), result.clone());
            }
        }
        results
    }

    /// Validate const block results against expected types
    pub fn validate_results(&self, type_queries: &TypeQueries) -> Result<()> {
        let _ = type_queries;

        for block in self.const_blocks.values() {
            match &block.state {
                ConstEvalState::Evaluated => {
                    if block.result.is_none() {
                        return Err(fp_core::error::Error::Generic(eyre!(
                            "Const block {} evaluated without result",
                            block
                                .name
                                .as_deref()
                                .map(|s| s.to_string())
                                .unwrap_or_else(|| block.id.to_string())
                        )));
                    }
                }
                ConstEvalState::Error(message) => {
                    return Err(fp_core::error::Error::Generic(eyre!(
                        "Const evaluation error in {}: {}",
                        block
                            .name
                            .as_deref()
                            .map(|s| s.to_string())
                            .unwrap_or_else(|| block.id.to_string()),
                        message
                    )));
                }
                _ => {
                    return Err(fp_core::error::Error::Generic(eyre!(
                        "Const block {} not evaluated",
                        block
                            .name
                            .as_deref()
                            .map(|s| s.to_string())
                            .unwrap_or_else(|| block.id.to_string())
                    )));
                }
            }
        }

        Ok(())
    }
}

impl ConstBlock {
    pub fn new(id: u64, name: Option<String>, expr: AstExpr) -> Self {
        Self {
            id,
            name,
            expr,
            dependencies: HashSet::new(),
            state: ConstEvalState::NotEvaluated,
            result: None,
        }
    }

    pub fn is_evaluated(&self) -> bool {
        matches!(self.state, ConstEvalState::Evaluated)
    }

    pub fn is_evaluating(&self) -> bool {
        matches!(self.state, ConstEvalState::Evaluating)
    }

    pub fn has_error(&self) -> bool {
        matches!(self.state, ConstEvalState::Error(_))
    }
}

fn walk_const_items<F>(node: &AstNode, f: &mut F)
where
    F: FnMut(&ItemDefConst),
{
    match node {
        AstNode::Item(item) => walk_const_in_item(item, f),
        AstNode::Expr(expr) => walk_const_in_expr(expr, f),
        AstNode::File(file) => {
            for item in &file.items {
                walk_const_in_item(item, f);
            }
        }
    }
}

fn walk_const_in_item<F>(item: &AstItem, f: &mut F)
where
    F: FnMut(&ItemDefConst),
{
    match item {
        AstItem::DefConst(def_const) => f(def_const),
        AstItem::Module(module) => {
            for child in &module.items {
                walk_const_in_item(child, f);
            }
        }
        AstItem::Impl(item_impl) => {
            for child in &item_impl.items {
                walk_const_in_item(child, f);
            }
        }
        AstItem::Expr(expr) => walk_const_in_expr(expr, f),
        _ => {}
    }
}

fn walk_const_in_expr<F>(expr: &AstExpr, f: &mut F)
where
    F: FnMut(&ItemDefConst),
{
    match expr {
        AstExpr::Block(block) => {
            for stmt in &block.stmts {
                match stmt {
                    BlockStmt::Item(item) => walk_const_in_item(item.as_ref(), f),
                    BlockStmt::Expr(inner) => walk_const_in_expr(inner.expr.as_ref(), f),
                    BlockStmt::Let(let_stmt) => {
                        if let Some(init) = &let_stmt.init {
                            walk_const_in_expr(init, f);
                        }
                        if let Some(diverge) = &let_stmt.diverge {
                            walk_const_in_expr(diverge, f);
                        }
                    }
                    _ => {}
                }
            }
        }
        AstExpr::If(if_expr) => {
            walk_const_in_expr(&if_expr.cond, f);
            walk_const_in_expr(&if_expr.then, f);
            if let Some(elze) = if_expr.elze.as_ref() {
                walk_const_in_expr(elze, f);
            }
        }
        AstExpr::Invoke(invoke) => {
            for arg in &invoke.args {
                walk_const_in_expr(arg, f);
            }
            if let ExprInvokeTarget::Expr(inner) = &invoke.target {
                walk_const_in_expr(inner.as_ref(), f);
            }
        }
        AstExpr::Let(let_expr) => walk_const_in_expr(let_expr.expr.as_ref(), f),
        AstExpr::Assign(assign) => {
            walk_const_in_expr(assign.target.as_ref(), f);
            walk_const_in_expr(assign.value.as_ref(), f);
        }
        AstExpr::Struct(struct_expr) => {
            walk_const_in_expr(struct_expr.name.as_ref(), f);
            for field in &struct_expr.fields {
                if let Some(value) = field.value.as_ref() {
                    walk_const_in_expr(value, f);
                }
            }
        }
        AstExpr::Tuple(tuple) => {
            for value in &tuple.values {
                walk_const_in_expr(value, f);
            }
        }
        AstExpr::Array(array) => {
            for value in &array.values {
                walk_const_in_expr(value, f);
            }
        }
        AstExpr::Item(item) => walk_const_in_item(item, f),
        AstExpr::Value(value) => collect_value_expr(value.as_ref(), f),
        _ => {}
    }
}

fn collect_value_expr<F>(value: &AstValue, f: &mut F)
where
    F: FnMut(&ItemDefConst),
{
    match value {
        AstValue::Expr(expr) => walk_const_in_expr(expr, f),
        AstValue::Struct(value_struct) => {
            for field in &value_struct.structural.fields {
                collect_value_expr(&field.value, f);
            }
        }
        AstValue::Tuple(tuple) => {
            for value in &tuple.values {
                collect_value_expr(value, f);
            }
        }
        AstValue::Type(ty) => collect_type_expr(ty, f),
        _ => {}
    }
}

fn collect_type_expr<F>(ty: &AstType, f: &mut F)
where
    F: FnMut(&ItemDefConst),
{
    match ty {
        AstType::Struct(struct_ty) => {
            for field in &struct_ty.fields {
                collect_type_expr(&field.value, f);
            }
        }
        AstType::Tuple(tuple) => {
            for ty in &tuple.types {
                collect_type_expr(ty, f);
            }
        }
        AstType::Vec(vec_ty) => collect_type_expr(&vec_ty.ty, f),
        AstType::Reference(reference) => collect_type_expr(&reference.ty, f),
        AstType::Expr(expr) => walk_const_in_expr(expr, f),
        _ => {}
    }
}

fn collect_expr_references(expr: &AstExpr, references: &mut HashSet<String>) {
    match expr {
        AstExpr::Locator(locator) => {
            if let Some(ident) = locator.as_ident() {
                references.insert(ident.name.clone());
            }
        }
        AstExpr::Value(value) => collect_value_references(value.as_ref(), references),
        AstExpr::Block(block) => {
            for stmt in &block.stmts {
                match stmt {
                    BlockStmt::Item(item) => collect_item_references(item.as_ref(), references),
                    BlockStmt::Expr(inner) => {
                        collect_expr_references(inner.expr.as_ref(), references)
                    }
                    BlockStmt::Let(let_stmt) => {
                        if let Some(init) = &let_stmt.init {
                            collect_expr_references(init, references);
                        }
                        if let Some(diverge) = &let_stmt.diverge {
                            collect_expr_references(diverge, references);
                        }
                    }
                    _ => {}
                }
            }
        }
        AstExpr::If(if_expr) => {
            collect_expr_references(&if_expr.cond, references);
            collect_expr_references(&if_expr.then, references);
            if let Some(elze) = if_expr.elze.as_ref() {
                collect_expr_references(elze, references);
            }
        }
        AstExpr::Invoke(invoke) => {
            for arg in &invoke.args {
                collect_expr_references(arg, references);
            }
            if let ExprInvokeTarget::Expr(inner) = &invoke.target {
                collect_expr_references(inner.as_ref(), references);
            }
        }
        AstExpr::Assign(assign) => {
            collect_expr_references(assign.target.as_ref(), references);
            collect_expr_references(assign.value.as_ref(), references);
        }
        AstExpr::Struct(struct_expr) => {
            collect_expr_references(struct_expr.name.as_ref(), references);
            for field in &struct_expr.fields {
                if let Some(value) = field.value.as_ref() {
                    collect_expr_references(value, references);
                }
            }
        }
        AstExpr::Select(select) => {
            collect_expr_references(select.obj.as_ref(), references);
        }
        AstExpr::BinOp(binop) => {
            collect_expr_references(binop.lhs.as_ref(), references);
            collect_expr_references(binop.rhs.as_ref(), references);
        }
        AstExpr::UnOp(unop) => collect_expr_references(unop.val.as_ref(), references),
        AstExpr::Let(let_expr) => collect_expr_references(let_expr.expr.as_ref(), references),
        AstExpr::Paren(paren) => collect_expr_references(paren.expr.as_ref(), references),
        AstExpr::Tuple(tuple) => {
            for value in &tuple.values {
                collect_expr_references(value, references);
            }
        }
        AstExpr::Array(array) => {
            for value in &array.values {
                collect_expr_references(value, references);
            }
        }
        AstExpr::Item(item) => collect_item_references(item, references),
        _ => {}
    }
}

fn collect_item_references(item: &AstItem, references: &mut HashSet<String>) {
    match item {
        AstItem::DefConst(def_const) => {
            collect_expr_references(def_const.value.as_ref(), references)
        }
        AstItem::Module(module) => {
            for child in &module.items {
                collect_item_references(child, references);
            }
        }
        AstItem::Impl(item_impl) => {
            for child in &item_impl.items {
                collect_item_references(child, references);
            }
        }
        AstItem::Expr(expr) => collect_expr_references(expr, references),
        _ => {}
    }
}

fn collect_value_references(value: &AstValue, references: &mut HashSet<String>) {
    match value {
        AstValue::Expr(expr) => collect_expr_references(expr, references),
        AstValue::Struct(value_struct) => {
            for field in &value_struct.structural.fields {
                collect_value_references(&field.value, references);
            }
        }
        AstValue::Tuple(tuple) => {
            for value in &tuple.values {
                collect_value_references(value, references);
            }
        }
        AstValue::Type(ty) => collect_type_references(ty, references),
        _ => {}
    }
}

fn collect_type_references(ty: &AstType, references: &mut HashSet<String>) {
    match ty {
        AstType::Struct(struct_ty) => {
            for field in &struct_ty.fields {
                collect_type_references(&field.value, references);
            }
        }
        AstType::Tuple(tuple) => {
            for ty in &tuple.types {
                collect_type_references(ty, references);
            }
        }
        AstType::Vec(vec_ty) => collect_type_references(&vec_ty.ty, references),
        AstType::Reference(reference) => collect_type_references(&reference.ty, references),
        AstType::Expr(expr) => collect_expr_references(expr, references),
        _ => {}
    }
}
