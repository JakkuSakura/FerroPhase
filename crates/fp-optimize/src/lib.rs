// fp-optimize: Optimization and transformation passes for FerroPhase
//
// Architecture:
// - orchestrators: Complex passes that coordinate multiple systems
// - passes: Focused passes that implement OptimizePass
// - queries: Stateless operations for extracting information
// - utils: Shared utilities and helper components

pub mod error;
pub mod ir;
pub mod orchestrators;
pub mod passes;
pub mod pipeline; // New unified pipeline
pub mod queries;
pub mod transformations;
pub mod utils;

// Re-export key types for convenience
pub use orchestrators::*;
pub use passes::*;
pub use queries::*;
pub use utils::*;
