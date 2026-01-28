//! Bootstrap JSON frontend without external dependencies.

use std::path::Path;
use std::sync::Arc;

use fp_core::ast::{AstSerializer, Expr, ExprKind, Node, Value};
use fp_core::diagnostics::DiagnosticManager;
use fp_core::error::{Error as CoreError, Result as CoreResult};
use fp_core::formats::json;
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};

/// Canonical identifier for the JSON frontend.
pub const JSON: &str = "json";

#[derive(Debug, Default, Clone)]
pub struct JsonFrontend;

impl JsonFrontend {
    pub fn new() -> Self {
        Self
    }

    fn build_value(&self, source: &str) -> CoreResult<Value> {
        json::parse_value(source)
    }
}

impl LanguageFrontend for JsonFrontend {
    fn language(&self) -> &'static str {
        JSON
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["json"]
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let diagnostics = Arc::new(DiagnosticManager::new());
        let value = self.build_value(source)?;
        let expr = Expr::new(ExprKind::Value(Box::new(value.clone())));

        let serializer: Arc<dyn AstSerializer> = Arc::new(JsonSerializer);
        let serialized = serializer.serialize_value(&value).ok();
        let description = match path {
            Some(path) => format!("JSON document {}", path.display()),
            None => "JSON document <stdin>".to_string(),
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

struct JsonSerializer;

impl AstSerializer for JsonSerializer {
    fn serialize_expr(&self, node: &Expr) -> CoreResult<String> {
        match node.kind() {
            ExprKind::Value(value) => self.serialize_value(value),
            _ => Err(CoreError::from("json serializer expects value expression")),
        }
    }

    fn serialize_value(&self, node: &Value) -> CoreResult<String> {
        json::to_string_pretty(node)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_simple_json() {
        let input = r#"{\"name\": \"fp\", \"count\": 2}"#;
        let frontend = JsonFrontend::new();
        let result = frontend.parse(input, None).expect("parse json");
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
