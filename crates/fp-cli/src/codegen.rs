use crate::CliError;
use fp_core::ast::{AstSerializer, Node, NodeKind};
use fp_lang::PrettyAstSerializer;
use quote::quote;

/// Code generation utilities
pub struct CodeGenerator;

impl CodeGenerator {
    /// Generate Rust code from AST
    pub fn generate_rust_code(node: &Node) -> Result<String, CliError> {
        let printer = PrettyAstSerializer::new();

        let node = node.clone();

        match node.kind() {
            NodeKind::Expr(expr) => {
                // Create a proper main function structure
                let body = printer.serialize_expr(expr).map_err(|e| {
                    CliError::Compilation(format!("Failed to generate Rust code: {}", e))
                })?;
                Ok(quote! {
                    fn main() {
                        #body
                    }
                }
                .to_string())
            }
            NodeKind::File(file) => printer
                .serialize_file(file)
                .map_err(|e| CliError::Compilation(format!("Failed to generate Rust code: {}", e))),
            NodeKind::Item(item) => printer
                .serialize_item(item)
                .map_err(|e| CliError::Compilation(format!("Failed to generate Rust code: {}", e))),
            NodeKind::Query(_) => Err(CliError::Compilation(
                "Query documents cannot be converted to Rust code".to_string(),
            )),
            NodeKind::Schema(_) => Err(CliError::Compilation(
                "Schema documents cannot be converted to Rust code".to_string(),
            )),
            NodeKind::Workspace(_) => Err(CliError::Compilation(
                "Workspace documents cannot be converted to Rust code".to_string(),
            )),
        }
    }
}
