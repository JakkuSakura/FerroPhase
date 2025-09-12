// Pass 3: Generic Context Preparation

use super::context::*;
use super::ConstEvaluator;
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::error::Result;
use std::collections::HashMap;
use tracing::{debug, info};

impl ConstEvaluator {
    /// Pass 3: Generic Context Preparation
    pub fn prepare_generic_contexts(&self, ast: &AstNode, ctx: &SharedScopedContext) -> Result<()> {
        debug!("Pass 3: Preparing generic contexts");
        
        // Discover all generic functions, types, and structs
        self.discover_generic_candidates(ast)?;
        
        // Analyze const expressions that depend on generic parameters
        self.analyze_generic_dependencies()?;
        
        // Set up evaluation contexts for different generic instantiations
        self.setup_generic_evaluation_contexts(ctx)?;
        
        Ok(())
    }

    /// Discover all generic functions, types, and structs in the AST
    pub(super) fn discover_generic_candidates(&self, node: &AstNode) -> Result<()> {
        match node {
            AstNode::File(file) => {
                for item in &file.items {
                    self.discover_generic_candidates_in_item(item)?;
                }
            },
            AstNode::Item(item) => {
                self.discover_generic_candidates_in_item(item)?;
            },
            _ => {}
        }
        Ok(())
    }

    /// Discover generic candidates in a specific item
    pub(super) fn discover_generic_candidates_in_item(&self, item: &AstItem) -> Result<()> {
        match item {
            AstItem::DefFunction(func_def) => {
                if !func_def.sig.generics_params.is_empty() {
                    let candidate_id = self.next_generic_id.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                    let candidate = GenericCandidate {
                        id: candidate_id,
                        name: func_def.name.name.clone(),
                        generic_params: func_def.sig.generics_params.clone(),
                        item_type: GenericItemType::Function(ValueFunction {
                            sig: func_def.sig.clone(),
                            body: func_def.body.clone(),
                        }),
                        ast_node: AstNode::Item(AstItem::DefFunction(func_def.clone())),
                    };
                    
                    self.generic_candidates.write().unwrap().insert(candidate_id, candidate);
                    debug!("Discovered generic function: {} (id: {})", func_def.name.name, candidate_id);
                }
            },
            AstItem::DefStruct(struct_def) => {
                // For now, we'll handle structs that might be targets of generic const evaluation
                let candidate_id = self.next_generic_id.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                let candidate = GenericCandidate {
                    id: candidate_id,
                    name: struct_def.name.name.clone(),
                    generic_params: vec![], // Structs don't have generics yet in current AST
                    item_type: GenericItemType::Struct(struct_def.value.clone()),
                    ast_node: AstNode::Item(AstItem::DefStruct(struct_def.clone())),
                };
                
                self.generic_candidates.write().unwrap().insert(candidate_id, candidate);
                debug!("Discovered struct candidate: {} (id: {})", struct_def.name.name, candidate_id);
            },
            AstItem::Module(module) => {
                for inner_item in &module.items {
                    self.discover_generic_candidates_in_item(inner_item)?;
                }
            },
            _ => {}
        }
        Ok(())
    }

    /// Analyze which const expressions depend on generic parameters
    pub(super) fn analyze_generic_dependencies(&self) -> Result<()> {
        let generic_candidates = self.generic_candidates.read().unwrap();
        let mut const_blocks = self.const_blocks.write().unwrap();
        
        // For each const block, check if it references generic parameters
        for (block_id, block) in const_blocks.iter_mut() {
            let generic_deps = self.find_generic_dependencies(&block.expr, &generic_candidates)?;
            
            if !generic_deps.is_empty() {
                // This const block depends on generic parameters
                debug!("Const block {} depends on generics: {:?}", block_id, generic_deps);
                
                // Create a generic evaluation context for this block
                let context_key = format!("const_block_{}", block_id);
                let generic_context = GenericEvaluationContext {
                    generic_params: generic_deps.clone(),
                    type_variable_mappings: HashMap::new(), // Will be filled during specialization
                    specialization_key: context_key.clone(),
                };
                
                block.generic_context = Some(generic_context.clone());
                self.generic_contexts.write().unwrap().insert(context_key, generic_context);
            }
        }
        
        Ok(())
    }

    /// Find generic parameters that an expression depends on
    pub(super) fn find_generic_dependencies(&self, expr: &AstExpr, generic_candidates: &HashMap<u64, GenericCandidate>) -> Result<Vec<GenericParam>> {
        let mut deps = Vec::new();
        
        match expr {
            AstExpr::Locator(locator) => {
                if let Some(ident) = locator.as_ident() {
                    // Check if this identifier refers to a generic parameter
                    for candidate in generic_candidates.values() {
                        for param in &candidate.generic_params {
                            if param.name.name == ident.name {
                                deps.push(param.clone());
                            }
                        }
                    }
                }
            },
            AstExpr::Invoke(invoke) => {
                // Check function calls - if calling a generic function, inherit its parameters
                if let ExprInvokeTarget::Function(locator) = &invoke.target {
                    if let Some(ident) = locator.as_ident() {
                        for candidate in generic_candidates.values() {
                            if candidate.name == ident.name {
                                deps.extend(candidate.generic_params.clone());
                            }
                        }
                    }
                }
                
                // Check arguments recursively
                for arg in &invoke.args {
                    deps.extend(self.find_generic_dependencies(&arg.get(), generic_candidates)?);
                }
            },
            AstExpr::Block(block) => {
                // Check all statements in the block
                for stmt in block.first_stmts() {
                    if let BlockStmt::Expr(expr_stmt) = stmt {
                        deps.extend(self.find_generic_dependencies(&expr_stmt.expr, generic_candidates)?);
                    }
                }
                if let Some(last_expr) = block.last_expr() {
                    deps.extend(self.find_generic_dependencies(&last_expr, generic_candidates)?);
                }
            },
            _ => {
                // For other expression types, no generic dependencies for now
            }
        }
        
        // Remove duplicates
        deps.dedup_by(|a, b| a.name.name == b.name.name);
        
        Ok(deps)
    }

    /// Set up evaluation contexts for different generic instantiations
    pub(super) fn setup_generic_evaluation_contexts(&self, _ctx: &SharedScopedContext) -> Result<()> {
        let generic_contexts = self.generic_contexts.read().unwrap();
        
        info!("Set up {} generic evaluation contexts", generic_contexts.len());
        
        // For now, just log the contexts. In a full implementation, this would:
        // 1. Analyze usage patterns to determine which specializations are needed
        // 2. Create type variable mappings for each specialization
        // 3. Set up evaluation environments with proper scoping
        
        for (key, context) in generic_contexts.iter() {
            debug!("Generic context {}: {} parameters", key, context.generic_params.len());
        }
        
        Ok(())
    }
}