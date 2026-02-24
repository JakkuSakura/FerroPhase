//! FerroPhase frontend for JSON documents.

use std::path::Path;
use std::sync::Arc;

use fp_core::ast::{AstSerializer, Expr, ExprKind, Node, Value, ValueList, ValueMap};
use fp_core::diagnostics::DiagnosticManager;
use fp_core::error::{Error as CoreError, Result as CoreResult};
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
use fp_core::utils::to_json::ToJson;
use serde_json::Value as JsonValue;

/// Canonical identifier for the JSON frontend.
pub const JSON: &str = "json";

/// Frontend that converts JSON documents into FerroPhase values.
#[derive(Debug, Default, Clone)]
pub struct JsonFrontend;

impl JsonFrontend {
    pub fn new() -> Self {
        Self
    }

    fn build_value(&self, source: &str) -> CoreResult<Value> {
        let value: JsonValue = serde_json::from_str(source)?;
        Ok(lower_json_value(&value))
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
        let json = node.to_json()?;
        serde_json::to_string_pretty(&json).map_err(CoreError::from)
    }
}

fn lower_json_value(value: &JsonValue) -> Value {
    match value {
        JsonValue::Null => Value::null(),
        JsonValue::Bool(b) => Value::bool(*b),
        JsonValue::Number(num) => {
            if let Some(i) = num.as_i64() {
                Value::int(i)
            } else if let Some(u) = num.as_u64() {
                if u <= i64::MAX as u64 {
                    Value::int(u as i64)
                } else {
                    Value::decimal(u as f64)
                }
            } else {
                Value::decimal(num.as_f64().unwrap_or(0.0))
            }
        }
        JsonValue::String(s) => Value::string(s.clone()),
        JsonValue::Array(items) => {
            let values = items.iter().map(lower_json_value).collect::<Vec<_>>();
            Value::List(ValueList::new(values))
        }
        JsonValue::Object(map) => {
            let mut entries = Vec::with_capacity(map.len());
            for (key, value) in map {
                entries.push((Value::string(key.clone()), lower_json_value(value)));
            }
            Value::Map(ValueMap::from_pairs(entries))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_simple_json() {
        let input = r#"{"name": "fp", "count": 2, "ok": true}"#;
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
        assert_eq!(map.len(), 3);
    }
}
