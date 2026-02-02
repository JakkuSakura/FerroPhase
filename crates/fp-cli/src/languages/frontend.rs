pub use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
pub use fp_flatbuffers::FlatbuffersFrontend;
pub use fp_hcl::HclFrontend;
pub use fp_json::JsonFrontend;
pub use fp_jsonschema::JsonSchemaFrontend;
pub use fp_lang::FerroFrontend;
pub use fp_prql::PrqlFrontend;
pub use fp_python::PythonFrontend;
pub use fp_sql::SqlFrontend;
pub use fp_toml::TomlFrontend;
pub use fp_typescript::TypeScriptFrontend;
pub use fp_wit::WitFrontend;

use std::path::Path;

/// Known frontend source languages inferred from input file extensions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LanguageSource {
    TypeScript,
    JavaScript,
    Rust,
    Zig,
    Wit,
}

/// Detect the input language from file extension.
pub fn detect_language_source_by_path(path: &Path) -> Option<LanguageSource> {
    let ext = path.extension()?.to_str()?.to_ascii_lowercase();
    match ext.as_str() {
        // TypeScript / JavaScript family
        "ts" | "tsx" | "mts" | "cts" => Some(LanguageSource::TypeScript),
        "js" | "jsx" | "mjs" => Some(LanguageSource::JavaScript),
        // Other supported frontends
        "rs" => Some(LanguageSource::Rust),
        "zig" => Some(LanguageSource::Zig),
        "wit" => Some(LanguageSource::Wit),
        _ => None,
    }
}
