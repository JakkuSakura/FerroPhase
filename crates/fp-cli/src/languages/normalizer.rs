use std::sync::Arc;

use fp_core::intrinsics::IntrinsicNormalizer;

/// Resolve a source-language normalizer (source patterns → portable ops).
pub fn normalizer_for_language(lang: &str) -> Option<Arc<dyn IntrinsicNormalizer>> {
    match lang {
        "ferrophase" | "rust" | "rs" | "fp" => Some(Arc::new(
            fp_lang::FerroIntrinsicNormalizer::new(
                fp_core::intrinsics::IntrinsicNormalizationMode::Transpile,
            )
        )),
        _ => None,
    }
}
