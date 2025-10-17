//! Clang parser for C/C++ source files

use crate::{ast::TranslationUnit, clang_ast_lowering, ClangError, CompileOptions, Result};
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::NamedTempFile;
use tracing::{debug, info, warn};

/// Main interface for parsing C/C++ files using clang
pub struct ClangParser {
    clang_path: PathBuf,
}

impl ClangParser {
    /// Create a new ClangParser, finding clang in PATH
    pub fn new() -> Result<Self> {
        let clang_path = which::which("clang")
            .or_else(|_| which::which("clang-19"))
            .or_else(|_| which::which("clang-18"))
            .or_else(|_| which::which("clang-17"))
            .or_else(|_| which::which("clang-16"))
            .map_err(|e| ClangError::ClangNotFound(e.to_string()))?;

        info!("Found clang at: {}", clang_path.display());
        Ok(Self { clang_path })
    }

    /// Create a ClangParser with a specific clang path
    pub fn with_path(clang_path: PathBuf) -> Result<Self> {
        if !clang_path.exists() {
            return Err(ClangError::FileNotFound(clang_path));
        }
        Ok(Self { clang_path })
    }

    /// Parse a C/C++ source file and emit LLVM IR
    pub fn parse_to_llvm_ir(
        &self,
        source_file: &Path,
        options: &CompileOptions,
    ) -> Result<llvm_ir::Module> {
        // First, compile to LLVM IR text
        let ir_text = self.compile_to_ir_text(source_file, options)?;

        // Parse the LLVM IR text
        self.parse_llvm_ir(&ir_text)
    }

    /// Compile a C/C++ source file to LLVM IR text format
    pub fn compile_to_ir_text(
        &self,
        source_file: &Path,
        options: &CompileOptions,
    ) -> Result<String> {
        Self::ensure_valid_source_file(source_file)?;

        // Build clang command
        let mut cmd = Command::new(&self.clang_path);

        // Emit LLVM IR
        cmd.arg("-S").arg("-emit-llvm");

        // Add standard if specified
        if let Some(std) = &options.standard {
            cmd.arg(std.as_flag());
        }

        // Add optimization level
        if let Some(opt) = &options.optimization {
            cmd.arg(format!("-O{}", opt));
        }

        // Add debug info
        if options.debug {
            cmd.arg("-g");
        }

        // Add include directories
        for inc in &options.include_dirs {
            cmd.arg("-I").arg(inc);
        }

        // Add additional flags
        for flag in &options.flags {
            cmd.arg(flag);
        }

        // Output to stdout
        cmd.arg("-o").arg("-");
        cmd.arg(source_file);

        debug!("Running clang command: {:?}", cmd);

        // Execute clang
        let output = cmd.output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            warn!("Clang compilation failed: {}", stderr);
            return Err(ClangError::CompilationFailed(stderr.to_string()));
        }

        let ir_text = String::from_utf8_lossy(&output.stdout).to_string();

        Ok(ir_text)
    }

    /// Compile a C/C++ source file to LLVM bitcode
    pub fn compile_to_bitcode(
        &self,
        source_file: &Path,
        output_file: &Path,
        options: &CompileOptions,
    ) -> Result<()> {
        Self::ensure_valid_source_file(source_file)?;

        // Build clang command
        let mut cmd = Command::new(&self.clang_path);

        // Emit LLVM bitcode
        cmd.arg("-c").arg("-emit-llvm");

        // Add standard if specified
        if let Some(std) = &options.standard {
            cmd.arg(std.as_flag());
        }

        // Add optimization level
        if let Some(opt) = &options.optimization {
            cmd.arg(format!("-O{}", opt));
        }

        // Add debug info
        if options.debug {
            cmd.arg("-g");
        }

        // Add include directories
        for inc in &options.include_dirs {
            cmd.arg("-I").arg(inc);
        }

        // Add additional flags
        for flag in &options.flags {
            cmd.arg(flag);
        }

        // Output file
        cmd.arg("-o").arg(output_file);
        cmd.arg(source_file);

        debug!("Running clang command: {:?}", cmd);

        // Execute clang
        let output = cmd.output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(ClangError::CompilationFailed(stderr.to_string()));
        }

        Ok(())
    }

    /// Parse a C/C++ source file into fp-clang's AST representation
    pub fn parse_translation_unit(
        &self,
        source_file: &Path,
        options: &CompileOptions,
    ) -> Result<TranslationUnit> {
        Self::ensure_valid_source_file(source_file)?;
        let json = self.dump_ast_json(source_file, options)?;
        clang_ast_lowering::lower_translation_unit_from_json(&json, source_file)
    }

    fn dump_ast_json(&self, source_file: &Path, options: &CompileOptions) -> Result<String> {
        Self::ensure_valid_source_file(source_file)?;

        let mut cmd = Command::new(&self.clang_path);

        if let Some(std) = &options.standard {
            cmd.arg(std.as_flag());
        }

        if let Some(opt) = &options.optimization {
            cmd.arg(format!("-O{}", opt));
        }

        if options.debug {
            cmd.arg("-g");
        }

        for inc in &options.include_dirs {
            cmd.arg("-I").arg(inc);
        }

        for flag in &options.flags {
            cmd.arg(flag);
        }

        cmd.arg("-Xclang")
            .arg("-ast-dump=json")
            .arg("-fsyntax-only")
            .arg(source_file);

        debug!("Running clang command: {:?}", cmd);

        let output = cmd.output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            warn!("Clang AST dump failed: {}", stderr);
            return Err(ClangError::CompilationFailed(stderr.to_string()));
        }

        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }

    fn ensure_valid_source_file(source_file: &Path) -> Result<()> {
        if !source_file.exists() {
            return Err(ClangError::FileNotFound(source_file.to_path_buf()));
        }

        let ext = source_file
            .extension()
            .and_then(|e| e.to_str())
            .ok_or_else(|| ClangError::InvalidExtension("No extension".to_string()))?;

        match ext {
            "c" | "cc" | "cpp" | "cxx" | "c++" | "C" | "h" | "hpp" | "hxx" => Ok(()),
            _ => Err(ClangError::InvalidExtension(ext.to_string())),
        }
    }

    /// Parse LLVM IR text into an llvm_ir::Module
    fn parse_llvm_ir(&self, ir_text: &str) -> Result<llvm_ir::Module> {
        // Create a temporary file that persists
        let temp_file = NamedTempFile::new()?;
        let temp_path = temp_file.path().to_path_buf();

        use std::io::Write;
        let mut file = std::fs::File::create(&temp_path)?;
        file.write_all(ir_text.as_bytes())?;
        file.flush()?;
        drop(file);

        // Parse the LLVM IR
        let module = llvm_ir::Module::from_ir_path(&temp_path)
            .map_err(|e| ClangError::LlvmIrError(e.to_string()))?;

        Ok(module)
    }

    /// Get clang version information
    pub fn version(&self) -> Result<String> {
        let output = Command::new(&self.clang_path).arg("--version").output()?;

        if !output.status.success() {
            return Err(ClangError::Other("Failed to get clang version".to_string()));
        }

        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_clang_available() {
        let parser = ClangParser::new();
        assert!(parser.is_ok(), "Clang should be available for tests");
    }

    #[test]
    fn test_clang_version() {
        let parser = ClangParser::new().unwrap();
        let version = parser.version().unwrap();
        assert!(version.contains("clang"), "Version should mention clang");
    }
}
