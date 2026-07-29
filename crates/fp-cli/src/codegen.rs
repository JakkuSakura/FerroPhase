use crate::CliError;
use fp_core::ast::{AstSerializer, File};
use fp_lang::PrettyAstSerializer;

/// Code generation utilities
pub struct CodeGenerator;

impl CodeGenerator {
    /// Generate Rust code from AST
    pub fn generate_rust_code(file: &File) -> Result<String, CliError> {
        let printer = PrettyAstSerializer::new();
        printer
            .serialize_file(file)
            .map_err(|e| CliError::Compilation(format!("Failed to generate Rust code: {}", e)))
    }
}
