pub use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
#[cfg(feature = "lang-flatbuffers")]
pub use fp_flatbuffers::FlatbuffersFrontend;
#[cfg(feature = "lang-golang")]
pub use fp_golang::GoFrontend;
#[cfg(feature = "lang-hcl")]
pub use fp_hcl::HclFrontend;
#[cfg(feature = "lang-json")]
pub use fp_json::JsonFrontend;
#[cfg(feature = "lang-jsonschema")]
pub use fp_jsonschema::JsonSchemaFrontend;
pub use fp_lang::FerroFrontend;
#[cfg(feature = "lang-prql")]
pub use fp_prql::PrqlFrontend;
#[cfg(feature = "lang-python")]
pub use fp_python::PythonFrontend;
#[cfg(feature = "lang-sql")]
pub use fp_sql::SqlFrontend;
#[cfg(feature = "lang-toml")]
pub use fp_toml::TomlFrontend;
#[cfg(feature = "lang-typescript")]
pub use fp_typescript::TypeScriptFrontend;
#[cfg(feature = "lang-wit")]
pub use fp_wit::WitFrontend;

use std::path::Path;

/// Known frontend source languages inferred from input file extensions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LanguageSource {
    TypeScript,
    JavaScript,
    Rust,
    Go,
    Zig,
    Wit,
}

/// Detect the input language from file extension.
pub fn detect_language_source_by_path(path: &Path) -> Option<LanguageSource> {
    let ext = path.extension()?.to_str()?.to_ascii_lowercase();
    match ext.as_str() {
        // TypeScript / JavaScript family
        #[cfg(feature = "lang-typescript")]
        "ts" | "tsx" | "mts" | "cts" => Some(LanguageSource::TypeScript),
        #[cfg(feature = "lang-typescript")]
        "js" | "jsx" | "mjs" => Some(LanguageSource::JavaScript),
        // Other supported frontends
        "rs" => Some(LanguageSource::Rust),
        #[cfg(feature = "lang-golang")]
        "go" => Some(LanguageSource::Go),
        #[cfg(feature = "lang-zig")]
        "zig" => Some(LanguageSource::Zig),
        #[cfg(feature = "lang-wit")]
        "wit" => Some(LanguageSource::Wit),
        _ => None,
    }
}
