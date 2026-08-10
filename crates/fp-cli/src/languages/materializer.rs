use std::sync::Arc;

use fp_core::intrinsics::IntrinsicMaterializer;

/// Resolve a target-language materializer (portable ops → language idioms).
pub fn materializer_for_language(lang: &str) -> Option<Arc<dyn IntrinsicMaterializer>> {
    match lang {
        #[cfg(feature = "lang-kotlin")]
        "kotlin" | "kt" => Some(Arc::new(fp_kotlin::KotlinMaterializer)),
        _ => None,
    }
}
