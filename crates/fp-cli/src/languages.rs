//! Language identifier definitions and utilities for transpilation

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
        extensions: &["ts"],
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
            || match (lang.name, target) {
                (TYPESCRIPT, "ts") => true,
                (JAVASCRIPT, "js") => true,
                (CSHARP, "cs" | "c#") => true,
                (PYTHON, "py") => true,
                (ZIG, "zig") => true,
                _ => false,
            }
    })
}

/// Get file extension for a target language
pub fn get_target_extension(target: &str) -> Option<&'static str> {
    match target {
        TYPESCRIPT | "ts" => Some("ts"),
        JAVASCRIPT | "js" => Some("js"),
        CSHARP | "cs" | "c#" => Some("cs"),
        PYTHON | "py" => Some("py"),
        GO => Some("go"),
        ZIG => Some("zig"),
        RUST => Some("rs"),
        CPP => Some("cpp"),
        C => Some("c"),
        JAVA => Some("java"),
        KOTLIN => Some("kt"),
        SWIFT => Some("swift"),
        FERROPHASE => Some("fp"),
        WIT => Some("wit"),
        _ => None,
    }
}

/// Check if a target language is supported for transpilation
pub fn is_transpile_supported(target: &str) -> bool {
    detect_target_language(target)
        .map(|lang| lang.transpile_supported)
        .unwrap_or(false)
}
