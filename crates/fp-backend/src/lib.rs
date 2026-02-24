// fp-backend: Optimization and transformation passes for FerroPhase
//
// Architecture:
// - transforms: Focused AST/IR rewrites and lowering helpers
// - queries: Stateless operations for extracting information
// - utils: Shared utilities and helper components

pub mod abi;
pub mod error;
pub mod optimizer;
pub mod transforms;

pub use transforms as transformations;
