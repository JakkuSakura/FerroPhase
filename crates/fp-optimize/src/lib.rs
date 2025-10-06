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
pub mod transformations;

// Re-export typing from fp-typing crate
pub use fp_typing as typing;

// Re-export key types for convenience
pub use orchestrators::*;
pub use passes::*;
pub use queries::*;
