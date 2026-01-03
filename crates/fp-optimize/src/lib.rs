// fp-optimize: Optimization and transformation passes for FerroPhase
//
// Architecture:
// - orchestrators: Complex passes that coordinate multiple systems
// - transformations: Focused AST/IR rewrites and lowering helpers
// - queries: Stateless operations for extracting information
// - utils: Shared utilities and helper components

pub mod error;
pub mod orchestrators;
pub mod transformations;
