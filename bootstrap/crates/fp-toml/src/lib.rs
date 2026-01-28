//! Bootstrap TOML frontend without external dependencies.

use std::path::Path;
use std::sync::Arc;

use fp_core::ast::{AstSerializer, Expr, ExprKind, Node, Value};
use fp_core::diagnostics::DiagnosticManager;
use fp_core::error::{Error as CoreError, Result as CoreResult};
use fp_core::formats::toml;
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};

/// Canonical identifier for the TOML frontend.
pub const TOML: &str = "toml";

#[derive(Debug, Default, Clone)]
pub struct TomlFrontend;

impl TomlFrontend {
    pub fn new() -> Self {
        Self
    }

    fn build_value(&self, source: &str) -> CoreResult<Value> {
        toml::parse_value(source)
    }
}

impl LanguageFrontend for TomlFrontend {
    fn language(&self) -> &'static str {
        TOML
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["toml"]
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let diagnostics = Arc::new(DiagnosticManager::new());
        let value = self.build_value(source)?;
        let expr = Expr::new(ExprKind::Value(Box::new(value.clone())));

        let serializer: Arc<dyn AstSerializer> = Arc::new(TomlSerializer);
        let serialized = serializer.serialize_value(&value).ok();
        let description = match path {
            Some(path) => format!("TOML document {}", path.display()),
            None => "TOML document <stdin>".to_string(),
        };
        let snapshot = FrontendSnapshot {
            language: self.language().to_string(),
            description,
            serialized,
        };

        let node = Node::expr(expr);

        Ok(FrontendResult {
            last: node.clone(),
            ast: node,
            serializer,
            intrinsic_normalizer: None,
            macro_parser: None,
            snapshot: Some(snapshot),
            diagnostics,
        })
    }
}

struct TomlSerializer;

impl AstSerializer for TomlSerializer {
    fn serialize_expr(&self, node: &Expr) -> CoreResult<String> {
        match node.kind() {
            ExprKind::Value(value) => self.serialize_value(value),
            _ => Err(CoreError::from("toml serializer expects value expression")),
        }
    }

    fn serialize_value(&self, node: &Value) -> CoreResult<String> {
        toml::to_string_pretty(node)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_simple_toml() {
        let input = r#"name = \"fp\"\ncount = 3\n"#;
        let frontend = TomlFrontend::new();
        let result = frontend.parse(input, None).expect("parse toml");
        let fp_core::ast::NodeKind::Expr(expr) = result.ast.kind() else {
            panic!("expected expr node");
        };
        let ExprKind::Value(value) = expr.kind() else {
            panic!("expected value expression");
        };
        let Value::Map(map) = value.as_ref() else {
            panic!("expected map value");
        };
        assert_eq!(map.len(), 2);
    }
}
