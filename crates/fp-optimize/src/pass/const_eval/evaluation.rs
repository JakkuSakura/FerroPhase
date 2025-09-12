// Pass 4: Const Evaluation with Type Queries

use super::context::*;
use super::ConstEvaluator;
use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::error::Result;
use tracing::{debug, info, warn};

impl ConstEvaluator {
    /// Pass 1: Type System Integration Setup
    pub fn setup_type_system_integration(&self, _ctx: &SharedScopedContext) -> Result<()> {
        debug!("Pass 1: Setting up type system integration");
        
        // Initialize bidirectional communication between const evaluator and type system
        // The TypeRegistry is already set up in the constructor
        
        Ok(())
    }

    /// Pass 4: Const Evaluation with Type Queries (Iterative)
    pub fn evaluate_const_blocks(&self, ctx: &SharedScopedContext) -> Result<bool> {
        debug!("Pass 4: Evaluating const blocks with type queries");
        
        let order = self.evaluation_order.read().unwrap().clone();
        let mut changes_made = false;
        
        for block_id in order {
            let mut blocks = self.const_blocks.write().unwrap();
            if let Some(block) = blocks.get_mut(&block_id) {
                if block.state == ConstEvalState::NotEvaluated {
                    debug!("Evaluating const block {} (id: {})", block.name.as_ref().unwrap_or(&"unnamed".to_string()), block_id);
                    
                    block.state = ConstEvalState::Evaluating;
                    
                    match self.interpreter.interpret_expr(&block.expr, ctx) {
                        Ok(result) => {
                            block.result = Some(result.clone());
                            block.state = ConstEvalState::Evaluated;
                            changes_made = true;
                            
                            // If this is a metaprogramming intrinsic, it might generate side effects
                            if let Some(name) = &block.name {
                                if name.starts_with("intrinsic_@") {
                                    self.handle_intrinsic_side_effects(name, &result, ctx)?;
                                }
                            }
                        },
                        Err(err) => {
                            block.state = ConstEvalState::Error(format!("{}", err));
                            warn!("Error evaluating const block {}: {}", block_id, err);
                        }
                    }
                }
            }
        }
        
        Ok(changes_made)
    }

    /// Handle side effects from metaprogramming intrinsics
    pub(super) fn handle_intrinsic_side_effects(&self, intrinsic_name: &str, result: &AstValue, _ctx: &SharedScopedContext) -> Result<()> {
        debug!("Intrinsic {} produced result: {:?}", intrinsic_name, result);
        
        // Parse the intrinsic name to determine what kind of side effect to generate
        match intrinsic_name {
            name if name.contains("@addfield") => {
                // This would be called from the @addfield intrinsic implementation
                // For now, just log that we would add a field
                info!("Would generate field addition side effect");
            },
            name if name.contains("@generate_method") => {
                // This would be called from the @generate_method intrinsic implementation
                info!("Would generate method side effect");
            },
            _ => {
                // Other intrinsics might not produce side effects
            }
        }
        
        Ok(())
    }

    /// Pass 6: Dependency Re-analysis (Conditional)
    pub fn reanalyze_dependencies(&self, _ctx: &SharedScopedContext) -> Result<bool> {
        debug!("Pass 6: Re-analyzing dependencies");
        
        // For now, return false indicating no new dependencies found
        // In a full implementation, this would check if new const blocks were generated
        // and rebuild the dependency graph if necessary
        
        Ok(false)
    }
}