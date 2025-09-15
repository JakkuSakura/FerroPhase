// Pass 2: Const Discovery & Dependency Analysis

use super::context::*;
use super::ConstEvaluator;
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::error::Result;
use std::collections::{HashMap, HashSet};
use tracing::{debug, info};

/// Check if a function name is a const-time intrinsic
fn is_const_intrinsic(name: &str) -> bool {
    matches!(name, 
        "sizeof!" | "reflect_fields!" | "hasmethod!" | "type_name!" |
        "create_struct!" | "clone_struct!" | "addfield!" |
        "hasfield!" | "field_count!" | "field_type!" | "struct_size!" |
        "generate_method!" | "compile_error!" | "compile_warning!"
    )
}

impl ConstEvaluator {
    /// Pass 2: Const Discovery & Dependency Analysis
    pub fn discover_const_blocks(&self, ast: &AstNode, ctx: &SharedScopedContext) -> Result<()> {
        debug!("Pass 2: Discovering const blocks and analyzing dependencies");
        
        self.visit_node(ast, ctx)?;
        self.build_dependency_graph()?;
        self.compute_evaluation_order()?;
        
        Ok(())
    }

    /// Visit an AST node to discover const blocks
    pub(super) fn visit_node(&self, node: &AstNode, ctx: &SharedScopedContext) -> Result<()> {
        match node {
            AstNode::File(file) => {
                for item in &file.items {
                    self.visit_item(item, ctx)?;
                }
            },
            AstNode::Item(item) => {
                self.visit_item(item, ctx)?;
            },
            AstNode::Expr(expr) => {
                self.visit_expr(expr, ctx)?;
            },
        }
        Ok(())
    }

    /// Visit an item to discover const blocks
    pub(super) fn visit_item(&self, item: &AstItem, ctx: &SharedScopedContext) -> Result<()> {
        match item {
            AstItem::DefConst(const_def) => {
                let block_id = self.next_block_id.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                let const_block = ConstBlock::new(
                    block_id,
                    Some(const_def.name.name.clone()),
                    *const_def.value.clone(),
                );
                
                self.const_blocks.write().unwrap().insert(block_id, const_block);
                debug!("Discovered const block: {} (id: {})", const_def.name.name, block_id);
            },
            AstItem::DefFunction(func_def) => {
                self.visit_expr(&func_def.body.get(), ctx)?;
            },
            AstItem::Module(module) => {
                for item in &module.items {
                    self.visit_item(item, ctx)?;
                }
            },
            AstItem::DefStruct(struct_def) => {
                // Register struct type in type registry
                self.register_struct_type(struct_def)?;
            },
            _ => {}
        }
        Ok(())
    }

    /// Register a struct type in the type registry
    fn register_struct_type(&self, struct_def: &ItemDefStruct) -> Result<()> {
        use fp_core::ctx::ty::{TypeId, TypeInfo, FieldInfo};
        
        let type_id = TypeId::new();
        let fields: Vec<FieldInfo> = struct_def.value.fields.iter().map(|field| {
            FieldInfo {
                name: field.name.name.clone(),
                type_id: TypeId::new(), // Placeholder, will be resolved later
                ast_type: field.value.clone(),
                attributes: vec![],
            }
        }).collect();

        let type_info = TypeInfo {
            id: type_id,
            name: struct_def.name.name.clone(),
            ast_type: AstType::Struct(struct_def.value.clone()),
            size_bytes: None,
            fields,
            methods: vec![],
            traits_implemented: vec![],
        };

        self.type_registry.register_type(type_info);
        Ok(())
    }

    /// Visit an expression to discover inline const blocks
    pub(super) fn visit_expr(&self, expr: &AstExpr, ctx: &SharedScopedContext) -> Result<()> {
        match expr {
            AstExpr::Block(block) => {
                for stmt in block.first_stmts() {
                    if let BlockStmt::Expr(expr_stmt) = stmt {
                        self.visit_expr(&expr_stmt.expr, ctx)?;
                    }
                }
                if let Some(last_expr) = block.last_expr() {
                    self.visit_expr(&last_expr, ctx)?;
                }
            },
            AstExpr::Invoke(invoke) => {
                // Check if this is a metaprogramming intrinsic call
                if let ExprInvokeTarget::Function(locator) = &invoke.target {
                    if let Some(ident) = locator.as_ident() {
                        if is_const_intrinsic(&ident.name) {
                            // This is a const block that needs evaluation
                            let block_id = self.next_block_id.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                            let const_block = ConstBlock::new(
                                block_id,
                                Some(ident.name.clone()),
                                expr.clone(),
                            );
                            
                            self.const_blocks.write().unwrap().insert(block_id, const_block);
                            debug!("Discovered intrinsic const block: {} (id: {})", ident.name, block_id);
                        }
                    }
                }
                
                // Visit arguments
                for arg in &invoke.args {
                    self.visit_expr(&arg.get(), ctx)?;
                }
            },
            _ => {}
        }
        Ok(())
    }

    /// Build dependency graph between const blocks
    pub(super) fn build_dependency_graph(&self) -> Result<()> {
        let blocks = self.const_blocks.read().unwrap();
        let mut dependencies = self.dependencies.write().unwrap();
        
        for (id, block) in blocks.iter() {
            let deps = self.find_dependencies(&block.expr)?;
            dependencies.insert(*id, deps);
        }
        
        Ok(())
    }

    /// Find dependencies of a const expression
    pub(super) fn find_dependencies(&self, expr: &AstExpr) -> Result<HashSet<u64>> {
        let mut deps = HashSet::new();
        
        // Simple dependency analysis - look for variable references
        match expr {
            AstExpr::Locator(locator) => {
                // Check if this references another const
                let blocks = self.const_blocks.read().unwrap();
                for (id, block) in blocks.iter() {
                    if let Some(name) = &block.name {
                        if let Some(ident) = locator.as_ident() {
                            if ident.name == *name {
                                deps.insert(*id);
                            }
                        }
                    }
                }
            },
            _ => {}
        }
        
        Ok(deps)
    }

    /// Compute evaluation order using topological sort
    pub(super) fn compute_evaluation_order(&self) -> Result<()> {
        let dependencies = self.dependencies.read().unwrap();
        let mut order = Vec::new();
        let mut visited = HashSet::new();
        let mut visiting = HashSet::new();
        
        // Topological sort with cycle detection
        for block_id in dependencies.keys() {
            if !visited.contains(block_id) {
                self.dfs_topological_sort(*block_id, &dependencies, &mut visited, &mut visiting, &mut order)?;
            }
        }
        
        order.reverse(); // DFS gives reverse topological order
        *self.evaluation_order.write().unwrap() = order;
        
        info!("Computed evaluation order: {:?}", self.evaluation_order.read().unwrap());
        
        Ok(())
    }

    /// Depth-first search for topological sorting
    pub(super) fn dfs_topological_sort(
        &self,
        node: u64,
        dependencies: &HashMap<u64, HashSet<u64>>,
        visited: &mut HashSet<u64>,
        visiting: &mut HashSet<u64>,
        order: &mut Vec<u64>,
    ) -> Result<()> {
        use crate::error::optimization_error;
        
        if visiting.contains(&node) {
            return Err(optimization_error(format!("Circular dependency detected involving block {}", node)));
        }
        
        if visited.contains(&node) {
            return Ok(());
        }
        
        visiting.insert(node);
        
        if let Some(deps) = dependencies.get(&node) {
            for &dep in deps {
                self.dfs_topological_sort(dep, dependencies, visited, visiting, order)?;
            }
        }
        
        visiting.remove(&node);
        visited.insert(node);
        order.push(node);
        
        Ok(())
    }
}