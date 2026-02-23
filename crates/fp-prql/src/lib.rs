//! FerroPhase frontend for PRQL pipelines.

pub mod compile;
pub mod dialect;
pub mod frontend;

pub use fp_core::query::SqlDialect;
pub use compile::{PrqlCompileResult, compile_prql};
pub use frontend::PrqlFrontend;

/// Canonical language identifier for PRQL sources.
pub const PRQL: &str = "prql";

impl PrqlFrontend {
    pub fn compile(
        &self,
        source: &str,
        target_override: Option<SqlDialect>,
    ) -> fp_core::error::Result<PrqlCompileResult> {
        compile_prql(source, target_override)
    }
}

#[cfg(test)]
mod tests;
