// Orchestrator: 3-Phase Const Evaluation System
// Implements the comprehensive const evaluation system from ConstEval.md

use crate::orchestrators::InterpretationOrchestrator;
use crate::queries::{DependencyQueries, TypeQueries};
use crate::utils::{ConstEval, ConstEvalTracker, EvaluationContext, IntrinsicEvaluationContext};
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::error::Result;
use std::sync::Arc;
use tracing::{debug, info};

/// 3-Phase Const Evaluation Orchestrator
///
/// This is an orchestrator that coordinates the entire const evaluation process
/// following the 3-phase system described in ConstEval.md:
///
/// Phase 1: Basic Type Checking
/// Phase 2: Const Evaluation & Metaprogramming  
/// Phase 3: Final Type Checking
///
/// This orchestrator coordinates the 3-phase const evaluation process
/// but delegates actual expression evaluation to the InterpretationOrchestrator.
/// It does NOT implement OptimizePass because it's a coordinator that manages
/// the overall process and modifies the AST in-place.
pub struct ConstEvaluationOrchestrator {
    /// Type queries for introspection
    type_queries: TypeQueries,

    /// Dependency analysis
    dependency_queries: DependencyQueries,

    /// Const-eval mutation staging and application
    const_eval_tracker: ConstEvalTracker,

    /// Context for intrinsic evaluation
    intrinsic_context: IntrinsicEvaluationContext,

    /// Orchestrator for interpretation
    interpreter: InterpretationOrchestrator,

    /// Evaluation context for tracking const blocks
    evaluation_context: EvaluationContext,
}

impl ConstEvaluationOrchestrator {
    pub fn new(serializer: Arc<dyn AstSerializer>) -> Self {
        let type_registry = Arc::new(fp_core::ctx::ty::TypeRegistry::new());
        let intrinsic_context = IntrinsicEvaluationContext::new(type_registry.clone());

        Self {
            type_queries: TypeQueries::new(type_registry.clone()),
            dependency_queries: DependencyQueries::new(),
            const_eval_tracker: ConstEvalTracker::new(),
            intrinsic_context,
            interpreter: InterpretationOrchestrator::new(serializer.clone()),
            evaluation_context: EvaluationContext::new(),
        }
    }

    /// Execute the complete 3-phase const evaluation
    pub fn evaluate(&mut self, ast: &mut AstNode, ctx: &SharedScopedContext) -> Result<()> {
        info!("Starting 3-phase const evaluation");

        // Phase 1: Basic Type Checking
        self.phase1_basic_type_checking(ast, ctx)?;

        // Phase 2: Const Evaluation & Metaprogramming
        self.phase2_const_evaluation_metaprogramming(ast, ctx)?;

        // Phase 3: Final Type Checking
        self.phase3_final_type_checking(ast, ctx)?;

        info!("3-phase const evaluation completed successfully");
        Ok(())
    }

    /// Phase 1: Basic Type Checking
    /// Parse AST and build initial type registry
    /// Type check non-const code (functions, structs, regular expressions)
    /// Validate basic type references and struct definitions
    /// Establish baseline type system that const evaluation can query
    /// Skip const blocks - treat them as opaque for now
    fn phase1_basic_type_checking(
        &mut self,
        ast: &AstNode,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        debug!("Phase 1: Basic Type Checking");

        // Register basic types from the AST
        self.type_queries.register_basic_types(ast)?;

        // Validate non-const type references
        self.type_queries.validate_basic_references(ast, ctx)?;

        debug!("Phase 1 completed: Basic type system established");
        Ok(())
    }

    /// Phase 2: Const Evaluation & Metaprogramming
    /// Discover const blocks and build dependency graph
    /// Evaluate const expressions in topological order
    /// Execute intrinsics like sizeof!, create_struct!, addfield!
    /// Collect const-eval mutations (generated fields, methods, new types)
    /// Apply metaprogramming changes to AST
    /// Query established types from Phase 1 as needed
    fn phase2_const_evaluation_metaprogramming(
        &mut self,
        ast: &mut AstNode,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        debug!("Phase 2: Const Evaluation & Metaprogramming");

        // Discover const blocks
        self.evaluation_context.discover_const_blocks(ast)?;

        // Build dependency graph
        let dependencies = self
            .dependency_queries
            .analyze_dependencies(&self.evaluation_context.get_const_blocks())?;
        self.evaluation_context.set_dependencies(dependencies);

        // Evaluate const blocks in dependency order
        let evaluation_order = self
            .dependency_queries
            .compute_topological_order(&self.evaluation_context.get_dependencies())?;

        for block_id in evaluation_order {
            self.evaluate_const_block(block_id, ctx)?;
        }

        // Collect const-eval mutations from intrinsic context
        let intrinsic_ops = self.intrinsic_context.take_const_eval_ops();
        for op in intrinsic_ops {
            self.const_eval_tracker.record(op);
        }

        // Apply accumulated mutations to AST
        self.const_eval_tracker.apply(ast)?;

        debug!("Phase 2 completed: Const evaluation operations applied");
        Ok(())
    }

    /// Phase 3: Final Type Checking
    /// Type check generated code (new fields, methods, types)
    /// Validate all type references including generated ones
    /// Check const block results against expected types
    /// Ensure type system consistency after metaprogramming
    /// Generate final optimized AST
    fn phase3_final_type_checking(
        &mut self,
        ast: &AstNode,
        ctx: &SharedScopedContext,
    ) -> Result<()> {
        debug!("Phase 3: Final Type Checking");

        // Register generated types
        self.type_queries.register_generated_types(ast)?;

        // Validate all type references including generated ones
        self.type_queries.validate_all_references(ast, ctx)?;

        // Validate const block results
        self.evaluation_context
            .validate_results(&self.type_queries)?;

        debug!("Phase 3 completed: Final type checking passed");
        Ok(())
    }

    /// Evaluate a single const block
    /// Delegates to the InterpretationOrchestrator for actual evaluation
    fn evaluate_const_block(&mut self, block_id: u64, ctx: &SharedScopedContext) -> Result<()> {
        let const_block = self.evaluation_context.get_const_block(block_id)?;

        // Delegate actual expression evaluation to the interpretation orchestrator
        // The interpretation orchestrator knows how to handle const expressions and
        // intrinsics that may request AST mutations
        let result = self.interpreter.evaluate_const_expression(
            &const_block.expr,
            ctx,
            &self.intrinsic_context,
        )?;

        // Store the result
        self.evaluation_context.set_block_result(block_id, result)?;

        Ok(())
    }

    /// Get the results of const evaluation
    pub fn get_results(&self) -> std::collections::HashMap<String, AstValue> {
        self.evaluation_context.get_all_results()
    }

    /// Get accumulated const-eval operations without consuming them
    pub fn get_const_eval_ops(&self) -> Vec<ConstEval> {
        self.const_eval_tracker.pending()
    }

    /// Manually queue a const-eval operation (mainly used by tests harnesses)
    pub fn record_const_eval(&mut self, op: ConstEval) {
        self.const_eval_tracker.record(op);
    }

    /// Drop any queued const-eval operations without applying them
    pub fn clear_const_eval_ops(&mut self) {
        self.const_eval_tracker.clear();
    }

    /// Apply queued const-eval operations to a module in-place
    pub fn apply_const_eval_ops_to_module(&mut self, module: &mut AstModule) -> Result<bool> {
        let mut node = AstNode::Item(AstItem::Module(module.clone()));
        let changed = self.const_eval_tracker.apply(&mut node)?;
        if changed {
            match node {
                AstNode::Item(AstItem::Module(updated)) => {
                    *module = updated;
                }
                _ => unreachable!("const-eval tracker should yield a module node"),
            }
        }
        Ok(changed)
    }

    /// Evaluate only const items (const declarations, structs) in an AST expression,
    /// leaving function call expressions for runtime execution
    pub fn evaluate_const_items_only(
        &mut self,
        ast: &mut AstExpr,
        context: &SharedScopedContext,
    ) -> Result<()> {
        use fp_core::ast::{AstExpr, BlockStmt};

        // Process items and format string expressions
        match ast {
            AstExpr::Block(block) => {
                // Process Item statements for const evaluation
                for stmt in block.stmts.iter_mut() {
                    match stmt {
                        BlockStmt::Item(item_box) => {
                            // Evaluate const items - dereference the Box to get the AstItem
                            let mut item_node = AstNode::Item((**item_box).clone());
                            self.evaluate(&mut item_node, context)?;

                            // Update the item in the AST with the evaluated result
                            if let AstNode::Item(evaluated_item) = item_node {
                                **item_box = evaluated_item;
                            }
                        }
                        BlockStmt::Expr(expr_stmt) => {
                            // Process expression statements that contain format strings
                            self.evaluate_expr_const_parts(&mut expr_stmt.expr, context)?;
                        }
                        _ => {
                            // For other statement types, recursively process if they contain expressions
                        }
                    }
                }
            }
            _ => {
                // For non-block expressions, skip const evaluation completely
                // This preserves function calls and other runtime expressions
            }
        }

        Ok(())
    }

    /// Recursively evaluate const parts within expressions (like format strings)
    fn evaluate_expr_const_parts(
        &mut self,
        expr: &mut BExpr,
        context: &SharedScopedContext,
    ) -> Result<()> {
        use fp_core::ast::AstExpr;

        match &mut **expr {
            AstExpr::FormatString(_) => {
                // Format strings are now resolved during AST→HIR transformation
                // No const evaluation needed here
                debug!("Format string found - will be resolved during HIR transformation");
            }
            AstExpr::Invoke(invoke) => {
                // Recursively process arguments that might contain format strings
                for arg in invoke.args.iter_mut() {
                    let mut boxed_arg = Box::new(arg.clone());
                    self.evaluate_expr_const_parts(&mut boxed_arg, context)?;
                    *arg = *boxed_arg;
                }
            }
            AstExpr::Block(block) => {
                // Recursively process nested blocks
                for stmt in block.stmts.iter_mut() {
                    if let BlockStmt::Expr(expr_stmt) = stmt {
                        self.evaluate_expr_const_parts(&mut expr_stmt.expr, context)?;
                    }
                }
            }
            _ => {
                // For other expression types, we don't need to evaluate them as const
                // since we're focusing on format strings
            }
        }

        Ok(())
    }
}
