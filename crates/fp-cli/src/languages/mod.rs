pub mod backend;
pub mod frontend;

use std::path::Path;

// Language identifier constants
pub const TYPESCRIPT: &str = "typescript";
pub const JAVASCRIPT: &str = "javascript";
pub const CSHARP: &str = "csharp";
pub const PYTHON: &str = "python";
pub const GO: &str = "go";
pub const RUST: &str = "rust";
pub const ZIG: &str = "zig";
pub const CPP: &str = "cpp";
pub const C: &str = "c";
pub const JAVA: &str = "java";
pub const KOTLIN: &str = "kotlin";
pub const SWIFT: &str = "swift";
pub const FERROPHASE: &str = fp_lang::FERROPHASE;
pub const WIT: &str = "wit";
pub const SQL: &str = fp_sql::SQL;
pub const PRQL: &str = fp_prql::PRQL;
pub const JSONSCHEMA: &str = fp_jsonschema::JSON_SCHEMA;
pub const FLATBUFFERS: &str = fp_flatbuffers::FLATBUFFERS;

/// Language information structure
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Language {
    pub name: &'static str,
    pub extensions: &'static [&'static str],
    pub transpile_supported: bool,
}

/// All supported languages
pub const SUPPORTED_LANGUAGES: &[Language] = &[
    Language {
        name: RUST,
        extensions: &["rs"],
        transpile_supported: true,
    },
    Language {
        name: TYPESCRIPT,
        extensions: &["ts", "tsx", "js", "jsx", "mjs", "mts", "cts"],
        transpile_supported: true,
    },
    Language {
        name: JAVASCRIPT,
        extensions: &["js"],
        transpile_supported: true,
    },
    Language {
        name: CSHARP,
        extensions: &["cs"],
        transpile_supported: true,
    },
    Language {
        name: PYTHON,
        extensions: &["py"],
        transpile_supported: true,
    },
    Language {
        name: GO,
        extensions: &["go"],
        transpile_supported: true,
    },
    Language {
        name: ZIG,
        extensions: &["zig"],
        transpile_supported: true,
    },
    Language {
        name: CPP,
        extensions: &["cpp", "cc", "cxx"],
        transpile_supported: false,
    },
    Language {
        name: C,
        extensions: &["c"],
        transpile_supported: false,
    },
    Language {
        name: JAVA,
        extensions: &["java"],
        transpile_supported: false,
    },
    Language {
        name: KOTLIN,
        extensions: &["kt"],
        transpile_supported: false,
    },
    Language {
        name: SWIFT,
        extensions: &["swift"],
        transpile_supported: false,
    },
    Language {
        name: FERROPHASE,
        extensions: &["fp"],
        transpile_supported: true,
    },
    Language {
        name: WIT,
        extensions: &["wit"],
        transpile_supported: true,
    },
    Language {
        name: SQL,
        extensions: &["sql"],
        transpile_supported: false,
    },
    Language {
        name: PRQL,
        extensions: &["prql"],
        transpile_supported: false,
    },
    Language {
        name: JSONSCHEMA,
        extensions: &["jsonschema"],
        transpile_supported: false,
    },
    Language {
        name: FLATBUFFERS,
        extensions: &["fbs"],
        transpile_supported: false,
    },
];

/// Detect source language from file extension
pub fn detect_source_language(path: &Path) -> Option<&'static Language> {
    let ext = path.extension()?.to_str()?;
    SUPPORTED_LANGUAGES
        .iter()
        .find(|lang| lang.extensions.contains(&ext))
}

/// Detect target language from string identifier
pub fn detect_target_language(target: &str) -> Option<&'static Language> {
    SUPPORTED_LANGUAGES.iter().find(|lang| {
        lang.name == target
            || lang.extensions.contains(&target)
            || matches!(
                (lang.name, target),
                (TYPESCRIPT, "ts")
                    | (JAVASCRIPT, "js")
                    | (CSHARP, "cs" | "c#")
                    | (PYTHON, "py")
                    | (ZIG, "zig")
            )
    })
}

/// Get file extension for a target language
pub fn get_target_extension(target: &str) -> Option<&'static str> {
    backend::parse_language_target(target)
        .ok()
        .map(backend::output_extension_for)
}

/// Check if a target language is supported for transpilation
pub fn is_transpile_supported(target: &str) -> bool {
    detect_target_language(target)
        .map(|lang| lang.transpile_supported)
        .unwrap_or(false)
}
