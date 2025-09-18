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
    pub fn discover_const_blocks(&mut self, _ast: &AstNode) -> Result<()> {
        // TODO: Implement const block discovery
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
    pub fn validate_results(&self, _type_queries: &TypeQueries) -> Result<()> {
        // TODO: Implement result validation
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
