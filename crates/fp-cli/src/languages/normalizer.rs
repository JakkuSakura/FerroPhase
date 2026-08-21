use std::sync::Arc;

use fp_core::intrinsics::{IntrinsicNormalizationMode, IntrinsicNormalizer};

/// Resolve a source-language normalizer (source patterns → portable ops).
///
/// Every caller now goes through `PipelineMode::TypecheckedTranspile` (typing
/// is required), so plain-call/method-call portable-op detection belongs
/// entirely to the post-typecheck `HirToAstLifter` (real resolved types
/// available there) — this package-load-time pass must not also reclassify
/// the same calls by name alone first, which would mutate the AST out from
/// under the safer, type-gated pass.
pub fn normalizer_for_language(lang: &str) -> Option<Arc<dyn IntrinsicNormalizer>> {
    match lang {
        "ferrophase" | "rust" | "rs" | "fp" => Some(Arc::new(fp_lang::FerroIntrinsicNormalizer::new(
            IntrinsicNormalizationMode::TypedTranspile,
        ))),
        _ => None,
    }
}
