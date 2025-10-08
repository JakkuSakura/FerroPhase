use fp_core::ast::{AstSerializer, Node};
use fp_core::error::Result;
use fp_csharp::CSharpSerializer;
use fp_javascript::{JavaScriptSerializer, TypeScriptSerializer};
use fp_python::PythonSerializer;
use fp_rust::printer::RustPrinter;

/// Result of a transpilation run.
pub struct TranspileResult {
    pub code: String,
    pub type_defs: Option<String>,
}

/// Front-end selector that delegates to language-specific serializers.
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

    pub fn transpile(&self, node: &Node) -> Result<TranspileResult> {
        match self.target {
            TranspileTarget::TypeScript => {
                let serializer = TypeScriptSerializer::new(self.emit_type_defs);
                let code = serializer.serialize_node(node)?;
                let type_defs = serializer.take_type_defs();
                Ok(TranspileResult { code, type_defs })
            }
            TranspileTarget::JavaScript => {
                let serializer = JavaScriptSerializer;
                let code = serializer.serialize_node(node)?;
                Ok(TranspileResult {
                    code,
                    type_defs: None,
                })
            }
            TranspileTarget::CSharp => {
                let serializer = CSharpSerializer;
                let code = serializer.serialize_node(node)?;
                Ok(TranspileResult {
                    code,
                    type_defs: None,
                })
            }
            TranspileTarget::Python => {
                let serializer = PythonSerializer;
                let code = serializer.serialize_node(node)?;
                Ok(TranspileResult {
                    code,
                    type_defs: None,
                })
            }
            TranspileTarget::Rust => render_rust(node),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum TranspileTarget {
    TypeScript,
    JavaScript,
    CSharp,
    Python,
    Rust,
}

fn render_rust(node: &Node) -> Result<TranspileResult> {
    let printer = RustPrinter::new();
    let code = printer.serialize_node(node)?;
    Ok(TranspileResult {
        code,
        type_defs: None,
    })
}
