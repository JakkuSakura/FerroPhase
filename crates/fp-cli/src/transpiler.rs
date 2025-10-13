use fp_core::ast::{AstSerializer, Node};
use fp_core::error::Error as CoreError;

use crate::CliError;

use fp_csharp::CSharpSerializer;
use fp_python::PythonSerializer;
use fp_rust::printer::RustPrinter;
use fp_typescript::{JavaScriptSerializer, TypeScriptSerializer};
use fp_wit::{WitOptions, WitSerializer};
use fp_zig::ZigSerializer;

/// Supported transpilation targets for the CLI.
#[derive(Debug, Clone, Copy)]
pub enum TranspileTarget {
    TypeScript,
    JavaScript,
    CSharp,
    Python,
    Zig,
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
    wit_options: Option<WitOptions>,
}

impl Transpiler {
    pub fn new(target: TranspileTarget, emit_type_defs: bool) -> Self {
        Self {
            target,
            emit_type_defs,
            wit_options: None,
        }
    }

    pub fn with_wit_options(mut self, options: WitOptions) -> Self {
        self.wit_options = Some(options);
        self
    }

    pub fn transpile(&self, node: &Node) -> Result<TranspileResult, CliError> {
        match self.target {
            TranspileTarget::TypeScript => self.transpile_typescript(node),
            TranspileTarget::JavaScript => self.transpile_javascript(node),
            TranspileTarget::CSharp => self.transpile_csharp(node),
            TranspileTarget::Python => self.transpile_python(node),
            TranspileTarget::Zig => self.transpile_zig(node),
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

    fn transpile_zig(&self, node: &Node) -> Result<TranspileResult, CliError> {
        let serializer = ZigSerializer;
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
        let serializer = match &self.wit_options {
            Some(options) => WitSerializer::with_options(options.clone()),
            None => WitSerializer::new(),
        };
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
