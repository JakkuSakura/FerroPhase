//! Helpers for THIR→THIR rewrites.
//!
//! Keeps the symbolic → materialised split explicit so the pipeline can run
//! normalisation and backend specialisation without duplicating logic.

pub mod backends;
pub mod format;
pub mod materializer;
pub mod normalizer;
pub mod transformer;
pub use materializer::BackendThirMaterializer;
pub use normalizer::SymbolicThirNormalizer;
pub use transformer::{IdentityThirTransformer, ThirTransform};
