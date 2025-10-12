use fp_core::ast::{AstSerializer, Node};
use fp_core::error::Error as CoreError;

use crate::CliError;

use fp_csharp::CSharpSerializer;
use fp_javascript::{JavaScriptSerializer, TypeScriptSerializer};
use fp_python::PythonSerializer;
use fp_rust::printer::RustPrinter;
use fp_wit::WitSerializer;

/// Supported transpilation targets for the CLI.
#[derive(Debug, Clone, Copy)]
pub enum TranspileTarget {
    TypeScript,
    JavaScript,
    CSharp,
    Python,
    Rust,
    Wit,
}

/// Result produced by the transpiler.
#[derive(Debug, Default, Clone)]
pub struct TranspileResult {
    pub code: String,
    pub type_defs: Option<String>,
}

/// Simple dispatcher that materialises language-specific serializers.
pub struct Transpiler {
    target: TranspileTarget,
    emit_type_defs: bool,
}

impl Transpiler {
    pub fn new(target: TranspileTarget, emit_type_defs: bool) -> Self {
        Self {
            target,
            emit_type_defs,
        }
    }

    pub fn transpile(&self, node: &Node) -> Result<TranspileResult, CliError> {
        match self.target {
            TranspileTarget::TypeScript => self.transpile_typescript(node),
            TranspileTarget::JavaScript => self.transpile_javascript(node),
            TranspileTarget::CSharp => self.transpile_csharp(node),
            TranspileTarget::Python => self.transpile_python(node),
            TranspileTarget::Rust => self.transpile_rust(node),
            TranspileTarget::Wit => self.transpile_wit(node),
        }
    }

    fn transpile_typescript(&self, node: &Node) -> Result<TranspileResult, CliError> {
        let serializer = TypeScriptSerializer::new(self.emit_type_defs);
        let code = serializer.serialize_node(node).map_err(map_error)?;
        let type_defs = serializer.take_type_defs();
        Ok(TranspileResult { code, type_defs })
    }

    fn transpile_javascript(&self, node: &Node) -> Result<TranspileResult, CliError> {
        let serializer = JavaScriptSerializer;
        let code = serializer.serialize_node(node).map_err(map_error)?;
        Ok(TranspileResult {
            code,
            type_defs: None,
        })
    }

    fn transpile_csharp(&self, node: &Node) -> Result<TranspileResult, CliError> {
        let serializer = CSharpSerializer;
        let code = serializer.serialize_node(node).map_err(map_error)?;
        Ok(TranspileResult {
            code,
            type_defs: None,
        })
    }

    fn transpile_python(&self, node: &Node) -> Result<TranspileResult, CliError> {
        let serializer = PythonSerializer;
        let code = serializer.serialize_node(node).map_err(map_error)?;
        Ok(TranspileResult {
            code,
            type_defs: None,
        })
    }

    fn transpile_rust(&self, node: &Node) -> Result<TranspileResult, CliError> {
        let serializer = RustPrinter::new_with_rustfmt();
        let code = serializer.serialize_node(node).map_err(map_error)?;
        Ok(TranspileResult {
            code,
            type_defs: None,
        })
    }

    fn transpile_wit(&self, node: &Node) -> Result<TranspileResult, CliError> {
        let serializer = WitSerializer::new();
        let code = serializer.serialize_node(node).map_err(map_error)?;
        Ok(TranspileResult {
            code,
            type_defs: None,
        })
    }
}

fn map_error(err: CoreError) -> CliError {
    CliError::Transpile(err.to_string())
}
