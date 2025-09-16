// Intrinsic context - utility for side-effect-aware intrinsic evaluation

use crate::utils::SideEffect;
use fp_core::ctx::ty::TypeRegistry;
use std::sync::{Arc, RwLock};

/// Context for intrinsic evaluation that can track side effects
#[derive(Debug, Clone)]
pub struct IntrinsicEvaluationContext {
    /// Side effects accumulated during intrinsic evaluation
    pub side_effects: Arc<RwLock<Vec<SideEffect>>>,
    /// Type registry for type queries
    pub type_registry: Arc<TypeRegistry>,
}

impl IntrinsicEvaluationContext {
    pub fn new(type_registry: Arc<TypeRegistry>) -> Self {
        Self {
            side_effects: Arc::new(RwLock::new(Vec::new())),
            type_registry,
        }
    }

    /// Add a side effect to be processed later
    pub fn add_side_effect(&self, effect: SideEffect) {
        self.side_effects.write().unwrap().push(effect);
    }

    /// Get all accumulated side effects
    pub fn get_side_effects(&self) -> Vec<SideEffect> {
        self.side_effects.read().unwrap().clone()
    }

    /// Clear all side effects
    pub fn clear_side_effects(&self) {
        self.side_effects.write().unwrap().clear();
    }
}