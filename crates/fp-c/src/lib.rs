//! C frontend for FerroPhase.
//!
//! The Clang frontend owns the AST lowering; this crate provides the C-only
//! entry point and a small libc parsing helper.

use std::path::{Path, PathBuf};

pub use fp_clang::ast;
pub use fp_clang::{ClangError, CompileOptions};

pub type Result<T> = std::result::Result<T, ClangError>;
pub type TranslationUnit = ast::TranslationUnit;

/// A generated C package containing the requested libc headers.
#[derive(Debug, Clone)]
pub struct GeneratedPackage {
    pub manifest: String,
    pub headers: Vec<String>,
    pub source: String,
    pub translation_unit: TranslationUnit,
}

impl GeneratedPackage {
    /// Write the package to `root`, creating the conventional `src/libc.c`.
    pub fn write_to(&self, root: &Path) -> std::io::Result<()> {
        std::fs::create_dir_all(root.join("src"))?;
        std::fs::write(root.join("Magnet.toml"), &self.manifest)?;
        std::fs::write(root.join("src/libc.c"), &self.source)
    }
}

pub struct CParser {
    inner: fp_clang::ClangParser,
}

impl CParser {
    pub fn new() -> Result<Self> {
        Ok(Self {
            inner: fp_clang::ClangParser::new()?,
        })
    }

    pub fn with_path(path: PathBuf) -> Result<Self> {
        Ok(Self {
            inner: fp_clang::ClangParser::with_path(path)?,
        })
    }

    pub fn parse_file(&self, source: &Path, options: &CompileOptions) -> Result<TranslationUnit> {
        self.inner.parse_translation_unit(source, options)
    }

    pub fn parse_source(&self, source: &str, options: &CompileOptions) -> Result<TranslationUnit> {
        self.parse_temp_source(source, "c", options)
    }

    /// Parse a libc-facing C declaration with the platform headers enabled.
    pub fn parse_libc_source(
        &self,
        source: &str,
        mut options: CompileOptions,
    ) -> Result<TranslationUnit> {
        options.flags.push("-D_POSIX_C_SOURCE=200809L".to_string());
        self.parse_temp_source(source, "c", &options)
    }

    fn parse_libc_bindings(
        &self,
        source: &str,
        mut options: CompileOptions,
    ) -> Result<TranslationUnit> {
        options.flags.push("-D_POSIX_C_SOURCE=200809L".to_string());
        let file = tempfile::Builder::new()
            .prefix("fp-c-libc-")
            .suffix(".c")
            .tempfile()
            .map_err(ClangError::IoError)?;
        std::fs::write(file.path(), source).map_err(ClangError::IoError)?;
        self.inner
            .parse_translation_unit_with_includes(file.path(), &options)
    }

    fn parse_temp_source(
        &self,
        source: &str,
        extension: &str,
        options: &CompileOptions,
    ) -> Result<TranslationUnit> {
        let suffix = format!(".{extension}");
        let file = tempfile::Builder::new()
            .prefix("fp-c-")
            .suffix(&suffix)
            .tempfile()
            .map_err(ClangError::IoError)?;
        std::fs::write(file.path(), source).map_err(ClangError::IoError)?;
        self.parse_file(file.path(), options)
    }
}

/// Generates a standalone C package from libc-facing C headers.
pub struct LibcCodegen {
    parser: CParser,
}

impl LibcCodegen {
    pub fn new() -> Result<Self> {
        Ok(Self {
            parser: CParser::new()?,
        })
    }

    /// Generate a package from headers such as `unistd.h` and `fcntl.h`.
    ///
    /// Header names are included with angle brackets, so the platform's libc
    /// include search path remains responsible for selecting the ABI.
    pub fn generate_package(
        &self,
        package_name: &str,
        headers: &[&str],
        options: CompileOptions,
    ) -> Result<GeneratedPackage> {
        if package_name.is_empty()
            || !package_name
                .bytes()
                .all(|b| b.is_ascii_alphanumeric() || b == b'_')
        {
            return Err(ClangError::Other("invalid package name".to_string()));
        }
        if headers.is_empty() {
            return Err(ClangError::Other(
                "at least one libc header is required".to_string(),
            ));
        }

        let includes = headers
            .iter()
            .map(|header| format!("#include <{header}>"))
            .collect::<Vec<_>>()
            .join("\n");
        let unit = self.parser.parse_libc_bindings(&includes, options)?;
        Ok(GeneratedPackage {
            manifest: format!(
                "[package]\nname = \"{package_name}\"\nversion = \"0.1.0\"\nedition = \"2024\"\n"
            ),
            headers: headers.iter().map(|header| (*header).to_string()).collect(),
            source: format!("{includes}\n"),
            translation_unit: unit,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{CParser, CompileOptions, LibcCodegen, ast::Declaration};

    #[test]
    fn parses_c_source() {
        let parser = CParser::new().expect("clang is required for the C parser test");
        let unit = parser
            .parse_source(
                "int add(int a, int b) { return a + b; }",
                &CompileOptions::default(),
            )
            .expect("C source should parse");
        assert!(unit.declarations.iter().any(|decl| matches!(
            decl,
            Declaration::Function(function) if function.name == "add"
        )));
    }

    #[test]
    fn parses_libc_declarations() {
        let parser = CParser::new().expect("clang is required for the libc parser test");
        let unit = parser
            .parse_libc_source(
                "#include <unistd.h>\npid_t current_pid(void) { return getpid(); }",
                CompileOptions::default(),
            )
            .expect("libc-backed C source should parse");
        assert!(unit.declarations.iter().any(|decl| matches!(
            decl,
            Declaration::Function(function) if function.name == "current_pid"
        )));
    }

    #[test]
    fn codegens_libc_package() {
        let codegen = LibcCodegen::new().expect("clang is required for libc codegen");
        let package = codegen
            .generate_package("libc", &["unistd.h"], CompileOptions::default())
            .expect("libc package should generate");
        assert!(package.manifest.contains("name = \"libc\""));
        assert_eq!(package.headers, vec!["unistd.h"]);
        assert_eq!(package.source, "#include <unistd.h>\n");
        assert!(
            package
                .translation_unit
                .declarations
                .iter()
                .any(|decl| matches!(
                    decl,
                    Declaration::Function(function) if function.name == "getpid"
                ))
        );
    }
}
