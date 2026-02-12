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
pub const SYCL: &str = "sycl";
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
pub const JSON: &str = fp_json::JSON;
pub const FLATBUFFERS: &str = fp_flatbuffers::FLATBUFFERS;
pub const TOML: &str = fp_toml::TOML;
pub const HCL: &str = fp_hcl::HCL;

/// Language information structure
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Language {
    pub name: &'static str,
    pub extensions: &'static [&'static str],
    pub ast_target_supported: bool,
}

/// All supported languages
pub const SUPPORTED_LANGUAGES: &[Language] = &[
    Language {
        name: RUST,
        extensions: &["rs"],
        ast_target_supported: true,
    },
    Language {
        name: TYPESCRIPT,
        extensions: &["ts", "tsx", "js", "jsx", "mjs", "mts", "cts"],
        ast_target_supported: true,
    },
    Language {
        name: JAVASCRIPT,
        extensions: &["js"],
        ast_target_supported: true,
    },
    Language {
        name: CSHARP,
        extensions: &["cs"],
        ast_target_supported: true,
    },
    Language {
        name: PYTHON,
        extensions: &["py"],
        ast_target_supported: true,
    },
    Language {
        name: GO,
        extensions: &["go"],
        ast_target_supported: true,
    },
    Language {
        name: ZIG,
        extensions: &["zig"],
        ast_target_supported: true,
    },
    Language {
        name: SYCL,
        extensions: &["sycl"],
        ast_target_supported: true,
    },
    Language {
        name: CPP,
        extensions: &["cpp", "cc", "cxx"],
        ast_target_supported: false,
    },
    Language {
        name: C,
        extensions: &["c"],
        ast_target_supported: false,
    },
    Language {
        name: JAVA,
        extensions: &["java"],
        ast_target_supported: false,
    },
    Language {
        name: KOTLIN,
        extensions: &["kt"],
        ast_target_supported: false,
    },
    Language {
        name: SWIFT,
        extensions: &["swift"],
        ast_target_supported: false,
    },
    Language {
        name: FERROPHASE,
        extensions: &["fp"],
        ast_target_supported: true,
    },
    Language {
        name: WIT,
        extensions: &["wit"],
        ast_target_supported: true,
    },
    Language {
        name: SQL,
        extensions: &["sql"],
        ast_target_supported: false,
    },
    Language {
        name: PRQL,
        extensions: &["prql"],
        ast_target_supported: false,
    },
    Language {
        name: JSONSCHEMA,
        extensions: &["jsonschema"],
        ast_target_supported: false,
    },
    Language {
        name: JSON,
        extensions: &["json"],
        ast_target_supported: false,
    },
    Language {
        name: FLATBUFFERS,
        extensions: &["fbs"],
        ast_target_supported: false,
    },
    Language {
        name: TOML,
        extensions: &["toml"],
        ast_target_supported: false,
    },
    Language {
        name: HCL,
        extensions: &["hcl"],
        ast_target_supported: false,
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
                    | (GO, "golang")
                    | (ZIG, "zig")
                    | (SYCL, "sycl")
            )
    })
}

/// Get file extension for a target language
pub fn get_target_extension(target: &str) -> Option<&'static str> {
    backend::parse_ast_target(target)
        .ok()
        .map(backend::ast_output_extension_for)
}

/// Check if a target language is supported as an AST output target.
pub fn is_ast_target_supported(target: &str) -> bool {
    detect_target_language(target)
        .map(|lang| lang.ast_target_supported)
        .unwrap_or(false)
}
