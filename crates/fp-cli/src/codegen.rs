use crate::CliError;
use fp_core::ast::{AstNode, AstSerializer};
use fp_rust::printer::RustPrinter;
use quote::quote;

/// Code generation utilities
pub struct CodeGenerator;

impl CodeGenerator {
    /// Generate Rust code from AST
    pub fn generate_rust_code(ast: &AstNode) -> Result<String, CliError> {
        let printer = RustPrinter::new();

        match ast {
            AstNode::Expr(expr) => {
                // Create a proper main function structure
                let main_body = printer.print_expr_no_braces(expr).map_err(|e| {
                    CliError::Compilation(format!("Failed to generate Rust code: {}", e))
                })?;

                let rust_code = quote! {
                    fn main() {
                        #main_body
                    }
                };

                Ok(rust_code.to_string())
            }
            AstNode::File(file) => printer
                .serialize_file(file)
                .map_err(|e| CliError::Compilation(format!("Failed to generate Rust code: {}", e))),
            AstNode::Item(item) => printer
                .serialize_item(item)
                .map_err(|e| CliError::Compilation(format!("Failed to generate Rust code: {}", e))),
        }
    }
}
