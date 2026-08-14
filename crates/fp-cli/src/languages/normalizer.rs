use std::sync::Arc;

use fp_core::intrinsics::{IntrinsicNormalizationMode, IntrinsicNormalizer};

/// Resolve a source-language normalizer (source patterns → portable ops).
///
/// `typed_transpile` should be `true` exactly when the caller knows the
/// downstream pipeline is `PipelineMode::TypecheckedTranspile` — plain-call/
/// method-call portable-op detection then belongs entirely to the
/// post-typecheck `HirToAstLifter` (real resolved
/// types available there), so this package-load-time pass must not also
/// reclassify the same calls by name alone first (that would just mutate
/// the AST out from under the safer, type-gated pass). `false` preserves
/// the original untyped-transpile behavior for pipelines that never reach
/// `HirToAstLifter` at all (`PipelineMode::Native`/`Compile`).
pub fn normalizer_for_language(lang: &str, typed_transpile: bool) -> Option<Arc<dyn IntrinsicNormalizer>> {
    let mode = if typed_transpile {
        IntrinsicNormalizationMode::TypedTranspile
    } else {
        IntrinsicNormalizationMode::Transpile
    };
    match lang {
        "ferrophase" | "rust" | "rs" | "fp" => {
            Some(Arc::new(fp_lang::FerroIntrinsicNormalizer::new(mode)))
        }
        _ => None,
    }
}
