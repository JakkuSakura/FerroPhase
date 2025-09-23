// Intrinsic context - utility for const-eval aware intrinsic evaluation

use crate::utils::ConstEval;
use fp_core::ctx::ty::TypeRegistry;
use std::sync::{Arc, RwLock};

/// Context for intrinsic evaluation that can track const-eval operations
#[derive(Debug, Clone)]
pub struct IntrinsicEvaluationContext {
    /// Operations accumulated during intrinsic evaluation
    pub const_eval_ops: Arc<RwLock<Vec<ConstEval>>>,
    /// Type registry for type queries
    pub type_registry: Arc<TypeRegistry>,
}

impl IntrinsicEvaluationContext {
    pub fn new(type_registry: Arc<TypeRegistry>) -> Self {
        Self {
            const_eval_ops: Arc::new(RwLock::new(Vec::new())),
            type_registry,
        }
    }

    /// Record a const-eval operation to be processed later
    pub fn add_const_eval_op(&self, op: ConstEval) {
        self.const_eval_ops.write().unwrap().push(op);
    }

    /// Drain all queued operations for application
    pub fn take_const_eval_ops(&self) -> Vec<ConstEval> {
        let mut lock = self.const_eval_ops.write().unwrap();
        let ops = lock.clone();
        lock.clear();
        ops
    }
}
