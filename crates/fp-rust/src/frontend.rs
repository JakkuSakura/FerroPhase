use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_core::ast::{File, Node, NodeKind};
use fp_core::diagnostics::DiagnosticLevel;
use fp_core::error::{Error as CoreError, Result as CoreResult};
use fp_core::frontend::{FrontendResult, LanguageFrontend};
use fp_core::intrinsics::IntrinsicNormalizer;

use crate::parser::RustParser;
use crate::printer::RustPrinter;

const LANGUAGE_KEY: &str = "rust";
const EXTENSIONS: &[&str] = &["rs"];

pub struct RustFrontend {
    serializer: Arc<RustPrinter>,
}

impl RustFrontend {
    pub fn new() -> Self {
        Self {
            serializer: Arc::new(RustPrinter::new()),
        }
    }

    fn file_path(path: Option<&Path>) -> PathBuf {
        path.map(Path::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("<rust>"))
    }
}

impl LanguageFrontend for RustFrontend {
    fn language(&self) -> &'static str {
        LANGUAGE_KEY
    }

    fn extensions(&self) -> &'static [&'static str] {
        EXTENSIONS
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let mut parser = RustParser::new();
        let file_path = Self::file_path(path);
        let file = parser.parse_file(source, file_path.as_path())?;
        let diagnostics = parser.diagnostics();

        if diagnostics.has_errors() {
            if let Some(diag) = diagnostics
                .get_diagnostics()
                .into_iter()
                .find(|d| matches!(d.level, DiagnosticLevel::Error))
            {
                return Err(CoreError::diagnostic(diag));
            }
            return Err(CoreError::from("Rust parse failed"));
        }

        let file = File {
            path: file_path,
            items: file.items,
        };
        let node = Node::from(NodeKind::File(file));

        Ok(FrontendResult {
            last: node.clone(),
            ast: node,
            serializer: self.serializer.clone(),
            intrinsic_normalizer: None as Option<Arc<dyn IntrinsicNormalizer>>,
            snapshot: None,
            diagnostics,
        })
    }
}
