// Orchestrator: 3-Phase Const Evaluation System
// Implements the comprehensive const evaluation system from ConstEval.md

use crate::utils::{IntrinsicEvaluationContext, SideEffectTracker, EvaluationContext};
use crate::queries::{TypeQueries, DependencyQueries};
use crate::orchestrators::{InterpretationOrchestrator};
use crate::passes::{SpecializePass};
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
    
    /// Side effect tracking and application
    side_effect_tracker: SideEffectTracker,
    
    /// Context for intrinsic evaluation
    intrinsic_context: IntrinsicEvaluationContext,
    
    /// Orchestrator for interpretation
    interpreter: InterpretationOrchestrator,
    
    /// Pass for specialization  
    specializer: SpecializePass,
    
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
            side_effect_tracker: SideEffectTracker::new(),
            intrinsic_context,
            interpreter: InterpretationOrchestrator::new(serializer.clone()),
            specializer: SpecializePass::new(serializer),
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
    fn phase1_basic_type_checking(&mut self, ast: &AstNode, ctx: &SharedScopedContext) -> Result<()> {
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
    /// Collect side effects (generated fields, methods, new types)
    /// Apply metaprogramming changes to AST
    /// Query established types from Phase 1 as needed
    fn phase2_const_evaluation_metaprogramming(&mut self, ast: &mut AstNode, ctx: &SharedScopedContext) -> Result<()> {
        debug!("Phase 2: Const Evaluation & Metaprogramming");
        
        // Discover const blocks
        self.evaluation_context.discover_const_blocks(ast)?;
        
        // Build dependency graph
        let dependencies = self.dependency_queries.analyze_dependencies(&self.evaluation_context.get_const_blocks())?;
        self.evaluation_context.set_dependencies(dependencies);
        
        // Evaluate const blocks in dependency order
        let evaluation_order = self.dependency_queries.compute_topological_order(&self.evaluation_context.get_dependencies())?;
        
        for block_id in evaluation_order {
            self.evaluate_const_block(block_id, ctx)?;
        }
        
        // Collect side effects from intrinsic context
        let intrinsic_side_effects = self.intrinsic_context.get_side_effects();
        for effect in intrinsic_side_effects {
            self.side_effect_tracker.add_side_effect(effect);
        }
        self.intrinsic_context.clear_side_effects();
        
        // Apply accumulated side effects to AST
        self.side_effect_tracker.apply_side_effects(ast)?;
        
        debug!("Phase 2 completed: Const evaluation and side effects applied");
        Ok(())
    }
    
    /// Phase 3: Final Type Checking
    /// Type check generated code (new fields, methods, types)
    /// Validate all type references including generated ones
    /// Check const block results against expected types
    /// Ensure type system consistency after metaprogramming
    /// Generate final optimized AST
    fn phase3_final_type_checking(&mut self, ast: &AstNode, ctx: &SharedScopedContext) -> Result<()> {
        debug!("Phase 3: Final Type Checking");
        
        // Register generated types
        self.type_queries.register_generated_types(ast)?;
        
        // Validate all type references including generated ones
        self.type_queries.validate_all_references(ast, ctx)?;
        
        // Validate const block results
        self.evaluation_context.validate_results(&self.type_queries)?;
        
        debug!("Phase 3 completed: Final type checking passed");
        Ok(())
    }
    
    /// Evaluate a single const block
    /// Delegates to the InterpretationOrchestrator for actual evaluation
    fn evaluate_const_block(&mut self, block_id: u64, ctx: &SharedScopedContext) -> Result<()> {
        let const_block = self.evaluation_context.get_const_block(block_id)?;
        
        // Delegate actual expression evaluation to the interpretation orchestrator
        // The interpretation orchestrator knows how to handle const expressions,
        // intrinsics, and side effect generation
        let result = self.interpreter.evaluate_const_expression(
            &const_block.expr,
            ctx,
            &self.intrinsic_context
        )?;
        
        // Store the result
        self.evaluation_context.set_block_result(block_id, result)?;
        
        Ok(())
    }
    
    /// Get the results of const evaluation
    pub fn get_results(&self) -> std::collections::HashMap<String, AstValue> {
        self.evaluation_context.get_all_results()
    }
    
    /// Get accumulated side effects
    pub fn get_side_effects(&self) -> Vec<crate::utils::SideEffect> {
        self.side_effect_tracker.get_side_effects()
    }
}