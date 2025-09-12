// Side effects management for const evaluation

use super::context::*;
use super::ConstEvaluator;
use fp_core::error::Result;

impl ConstEvaluator {
    /// Add a side effect to be processed later
    pub fn add_side_effect(&self, effect: SideEffect) {
        self.side_effects.write().unwrap().push(effect);
    }

    /// Clear all accumulated side effects
    pub fn clear_side_effects(&self) {
        self.side_effects.write().unwrap().clear();
    }

    /// Check if there are any pending side effects
    pub fn has_side_effects(&self) -> bool {
        !self.side_effects.read().unwrap().is_empty()
    }
}

impl SideEffect {
    /// Get a description of this side effect for debugging
    pub fn description(&self) -> String {
        match self {
            SideEffect::GenerateField { target_type, field_name, .. } => {
                format!("Add field {} to {}", field_name, target_type)
            },
            SideEffect::GenerateMethod { target_type, method_name, .. } => {
                format!("Add method {} to {}", method_name, target_type)
            },
            SideEffect::GenerateImpl { target_type, trait_name, methods } => {
                format!("Implement {} for {} with {} methods", trait_name, target_type, methods.len())
            },
            SideEffect::GenerateType { type_name, .. } => {
                format!("Generate type {}", type_name)
            },
        }
    }
}