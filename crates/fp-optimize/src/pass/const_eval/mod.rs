// Comprehensive const evaluation pass implementing the 10-pass architecture from docs/ConstEval.md

pub mod context;
pub mod discovery;
pub mod evaluation;
pub mod types;
pub mod generics;
pub mod side_effects;
pub mod codegen;
pub mod final_validation;

pub use context::*;
pub use discovery::*;
pub use evaluation::*;
pub use types::*;
pub use generics::*;
pub use side_effects::*;
pub use codegen::*;
pub use final_validation::*;

use crate::pass::{InterpreterPass, OptimizePass, SpecializePass};
use crate::error::optimization_error;
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::ctx::ty::{TypeRegistry, TypeId, TypeInfo, FieldInfo};
use fp_core::error::Result;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};
use tracing::{debug, info, warn};

/// Comprehensive const evaluator implementing the design from docs/ConstEval.md
pub struct ConstEvaluator {
    /// Interpreter for expression evaluation
    interpreter: InterpreterPass,
    
    /// Specializer for generic instantiation
    specializer: SpecializePass,
    
    /// Type registry for introspection
    type_registry: Arc<TypeRegistry>,
    
    /// Const blocks discovered and their evaluation order
    const_blocks: RwLock<HashMap<u64, ConstBlock>>,
    
    /// Dependency graph between const blocks
    dependencies: RwLock<HashMap<u64, HashSet<u64>>>,
    
    /// Side effects accumulated during evaluation
    side_effects: RwLock<Vec<SideEffect>>,
    
    /// Evaluation order computed from dependency analysis
    evaluation_order: RwLock<Vec<u64>>,
    
    /// Generic candidates that may need specialization
    generic_candidates: RwLock<HashMap<u64, GenericCandidate>>,
    
    /// Active generic evaluation contexts for different specializations
    generic_contexts: RwLock<HashMap<String, GenericEvaluationContext>>,
    
    /// Counter for generating unique const block IDs
    next_block_id: std::sync::atomic::AtomicU64,
    
    /// Counter for generating unique generic candidate IDs
    next_generic_id: std::sync::atomic::AtomicU64,
    
    /// Maximum iterations for the feedback loop
    max_iterations: usize,
    
    /// Current iteration count
    current_iteration: std::sync::atomic::AtomicUsize,
}

impl ConstEvaluator {
    pub fn new(serializer: Arc<dyn AstSerializer>) -> Self {
        Self {
            interpreter: InterpreterPass::new(serializer.clone()),
            specializer: SpecializePass::new(serializer),
            type_registry: Arc::new(TypeRegistry::new()),
            const_blocks: RwLock::new(HashMap::new()),
            dependencies: RwLock::new(HashMap::new()),
            side_effects: RwLock::new(Vec::new()),
            evaluation_order: RwLock::new(Vec::new()),
            generic_candidates: RwLock::new(HashMap::new()),
            generic_contexts: RwLock::new(HashMap::new()),
            next_block_id: std::sync::atomic::AtomicU64::new(0),
            next_generic_id: std::sync::atomic::AtomicU64::new(0),
            max_iterations: 10, // Prevent infinite loops
            current_iteration: std::sync::atomic::AtomicUsize::new(0),
        }
    }

    /// Main evaluation loop implementing the iterative feedback system
    pub fn evaluate_iterative(&self, ast: &AstNode, ctx: &SharedScopedContext) -> Result<()> {
        info!("Starting iterative const evaluation");
        
        // Phase 1: Setup & Discovery
        self.setup_type_system_integration(ctx)?;
        self.discover_const_blocks(ast, ctx)?;
        self.prepare_generic_contexts(ast, ctx)?;
        
        // Phase 2: Iterative Evaluation & Feedback Loop
        let mut iteration = 0;
        while iteration < self.max_iterations {
            self.current_iteration.store(iteration, std::sync::atomic::Ordering::SeqCst);
            
            info!("Const evaluation iteration {}", iteration);
            
            // Pass 4: Const Evaluation with Type Queries
            let changes_made = self.evaluate_const_blocks(ctx)?;
            
            // Pass 5: Type System Update & Validation
            let type_changes = self.update_and_validate_types(ctx)?;
            
            // Pass 6: Dependency Re-analysis (if needed)
            let new_dependencies = self.reanalyze_dependencies(ctx)?;
            
            // If no changes were made, we've converged
            if !changes_made && !type_changes && !new_dependencies {
                info!("Const evaluation converged after {} iterations", iteration + 1);
                break;
            }
            
            iteration += 1;
        }
        
        if iteration >= self.max_iterations {
            warn!("Const evaluation reached maximum iterations ({})", self.max_iterations);
        }
        
        Ok(())
    }

    /// Get the results of const evaluation
    pub fn get_evaluation_results(&self) -> HashMap<String, AstValue> {
        let blocks = self.const_blocks.read().unwrap();
        let mut results = HashMap::new();
        
        for (_, block) in blocks.iter() {
            if let (Some(name), Some(result)) = (&block.name, &block.result) {
                results.insert(name.clone(), result.clone());
            }
        }
        
        results
    }

    /// Get accumulated side effects
    pub fn get_side_effects(&self) -> Vec<SideEffect> {
        self.side_effects.read().unwrap().clone()
    }

    /// Get generic candidates (for testing)
    pub fn get_generic_candidates(&self) -> HashMap<u64, GenericCandidate> {
        self.generic_candidates.read().unwrap().clone()
    }

    /// Get generic contexts (for testing)
    pub fn get_generic_contexts(&self) -> HashMap<String, GenericEvaluationContext> {
        self.generic_contexts.read().unwrap().clone()
    }

    /// Get const blocks (for testing)
    pub fn get_const_blocks(&self) -> HashMap<u64, ConstBlock> {
        self.const_blocks.read().unwrap().clone()
    }
    
    /// Get access to the type registry (for testing)
    pub fn get_type_registry(&self) -> &Arc<TypeRegistry> {
        &self.type_registry
    }
    
    /// Register a const block for evaluation (for testing)
    pub fn register_const_block(&self, expr: &AstExpr, name: Option<String>, _ctx: &SharedScopedContext) -> Result<u64> {
        let mut blocks = self.const_blocks.write().unwrap();
        let block_id = blocks.len() as u64; // Simple ID generation for testing
        
        let const_block = ConstBlock {
            id: block_id,
            name,
            expr: expr.clone(),
            dependencies: HashSet::new(),
            state: ConstEvalState::NotEvaluated,
            result: None,
            generic_context: None,
        };
        
        blocks.insert(block_id, const_block);
        Ok(block_id)
    }
    
    /// Update const block state (for testing)
    pub fn update_const_block_state(&self, block_id: u64, state: ConstEvalState, result: Option<AstValue>) {
        let mut blocks = self.const_blocks.write().unwrap();
        if let Some(block) = blocks.get_mut(&block_id) {
            block.state = state;
            block.result = result;
        }
    }
}

/// Optimize pass that wraps the comprehensive const evaluator
pub struct ConstEvaluationPass {
    evaluator: ConstEvaluator,
}

impl ConstEvaluationPass {
    pub fn new(serializer: Arc<dyn AstSerializer>) -> Self {
        Self {
            evaluator: ConstEvaluator::new(serializer),
        }
    }
}

impl OptimizePass for ConstEvaluationPass {
    fn name(&self) -> &str {
        "const_evaluation"
    }

    fn optimize_expr(&self, expr: AstExpr, ctx: &SharedScopedContext) -> Result<AstExpr> {
        // Run the full const evaluation on this expression
        let ast_node = AstNode::Expr(expr);
        self.evaluator.evaluate_iterative(&ast_node, ctx)?;
        
        // For now, return the original expression
        // In a full implementation, this would apply const folding
        match ast_node {
            AstNode::Expr(e) => Ok(e),
            _ => unreachable!(),
        }
    }

    fn optimize_item(&self, item: AstItem, ctx: &SharedScopedContext) -> Result<AstItem> {
        // Run const evaluation on the item
        let ast_node = AstNode::Item(item);
        self.evaluator.evaluate_iterative(&ast_node, ctx)?;
        
        // Return the item (potentially modified by side effects)
        match ast_node {
            AstNode::Item(i) => Ok(i),
            _ => unreachable!(),
        }
    }
}