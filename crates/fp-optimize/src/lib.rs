// fp-optimize: Optimization and transformation passes for FerroPhase
//
// Architecture:
// - orchestrators: Complex passes that coordinate multiple systems
// - passes: Focused passes that implement OptimizePass  
// - queries: Stateless operations for extracting information
// - utils: Shared utilities and helper components

pub mod error;
pub mod orchestrators;
pub mod passes;
pub mod queries;
pub mod utils;

// Legacy module for backward compatibility
pub mod pass {
    pub use crate::passes::*;
    pub use crate::orchestrators::*;
    pub use crate::utils::{OptimizePass, FoldOptimizer, NoopPass, load_optimizers};
}

// Legacy module alias for interpreter
pub mod interpreter {
    pub use crate::orchestrators::interpretation::*;
    // Legacy alias
    pub type Interpreter = InterpretationOrchestrator;
}

// Re-export key types for convenience
pub use orchestrators::*;
pub use passes::*;
pub use queries::*;
pub use utils::*;
