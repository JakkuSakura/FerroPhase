//! C++ frontend for FerroPhase.
//!
//! This crate owns the C++-facing API while reusing the Clang AST lowering
//! shared with the C frontend. It parses declarations into the public
//! `fp_clang::ast` representation and leaves code generation to `fp-clang`.

use std::path::{Path, PathBuf};

pub use fp_clang::ast;
pub use fp_clang::{ClangError, CompileOptions, Standard};

pub type Result<T> = std::result::Result<T, ClangError>;
pub type TranslationUnit = ast::TranslationUnit;

/// Parser for C++ translation units.
pub struct CppParser {
    inner: fp_clang::ClangParser,
}

impl CppParser {
    /// Find a Clang executable in `PATH` and create a parser.
    pub fn new() -> Result<Self> {
        Ok(Self {
            inner: fp_clang::ClangParser::new()?,
        })
    }

    /// Create a parser using an explicit Clang executable.
    pub fn with_path(path: PathBuf) -> Result<Self> {
        Ok(Self {
            inner: fp_clang::ClangParser::with_path(path)?,
        })
    }

    /// Parse a C++ source file into a declaration translation unit.
    pub fn parse_file(&self, source: &Path, options: &CompileOptions) -> Result<TranslationUnit> {
        self.inner.parse_translation_unit(source, options)
    }

    /// Parse C++ source held in memory.
    pub fn parse_source(&self, source: &str, options: &CompileOptions) -> Result<TranslationUnit> {
        let file = tempfile::Builder::new()
            .prefix("fp-cpp-")
            .suffix(".cpp")
            .tempfile()
            .map_err(ClangError::IoError)?;
        std::fs::write(file.path(), source).map_err(ClangError::IoError)?;
        self.parse_file(file.path(), options)
    }

    /// Compile a C++ source file to LLVM IR text.
    pub fn compile_to_ir_text(&self, source: &Path, options: &CompileOptions) -> Result<String> {
        self.inner.compile_to_ir_text(source, options)
    }
}

#[cfg(test)]
mod tests {
    use super::{CompileOptions, CppParser, Standard, ast::Declaration};

    #[test]
    fn parses_cpp_source_from_memory() {
        let parser = CppParser::new().expect("clang is required for the C++ parser test");
        let mut options = CompileOptions::default();
        options.standard = Some(Standard::Cxx17);

        let unit = parser
            .parse_source(
                "struct Point { int x; }; int add(int a, int b) { return a + b; }",
                &options,
            )
            .expect("C++ source should parse");

        assert!(unit.declarations.iter().any(|decl| matches!(
            decl,
            Declaration::Struct(struct_decl) if struct_decl.name.as_deref() == Some("Point")
        )));
        assert!(unit.declarations.iter().any(|decl| matches!(
            decl,
            Declaration::Function(function) if function.name == "add"
        )));
    }
}
