use crate::CliError;
use fp_core::ast::BExpr;
use fp_rust_lang::printer::RustPrinter;
use quote::quote;

/// Code generation utilities
pub struct CodeGenerator;

impl CodeGenerator {
    /// Generate Rust code from AST
    pub fn generate_rust_code(ast: &BExpr) -> Result<String, CliError> {
        let printer = RustPrinter::new();

        // Create a proper main function structure
        let main_body = printer
            .print_expr_no_braces(ast)
            .map_err(|e| CliError::Compilation(format!("Failed to generate Rust code: {}", e)))?;

        let rust_code = quote! {
            fn main() {
                #main_body
            }
        };

        Ok(rust_code.to_string())
    }
}