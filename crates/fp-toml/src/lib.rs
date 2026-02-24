//! FerroPhase frontend for TOML documents.

use std::path::Path;
use std::sync::Arc;

use fp_core::ast::{AstSerializer, Expr, ExprKind, Node, Value, ValueList, ValueMap};
use fp_core::diagnostics::DiagnosticManager;
use fp_core::error::{Error as CoreError, Result as CoreResult};
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
use toml::Value as TomlValue;

/// Canonical identifier for the TOML frontend.
pub const TOML: &str = "toml";

/// Frontend that converts TOML documents into FerroPhase values.
#[derive(Debug, Default, Clone)]
pub struct TomlFrontend;

impl TomlFrontend {
    pub fn new() -> Self {
        Self
    }

    fn build_value(&self, source: &str) -> CoreResult<Value> {
        let value: TomlValue =
            toml::from_str(source).map_err(|err| CoreError::from(err.to_string()))?;
        Ok(lower_toml_value(&value))
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
        let value = value_to_toml(node)?;
        toml::to_string_pretty(&value).map_err(|err| CoreError::from(err.to_string()))
    }
}

fn lower_toml_value(value: &TomlValue) -> Value {
    match value {
        TomlValue::String(s) => Value::string(s.clone()),
        TomlValue::Integer(i) => Value::int(*i),
        TomlValue::Float(f) => Value::decimal(*f),
        TomlValue::Boolean(b) => Value::bool(*b),
        TomlValue::Datetime(dt) => Value::string(dt.to_string()),
        TomlValue::Array(items) => {
            let values = items.iter().map(lower_toml_value).collect::<Vec<_>>();
            Value::List(ValueList::new(values))
        }
        TomlValue::Table(table) => {
            let mut entries = Vec::with_capacity(table.len());
            for (key, value) in table {
                entries.push((Value::string(key.clone()), lower_toml_value(value)));
            }
            Value::Map(ValueMap::from_pairs(entries))
        }
    }
}

fn value_to_toml(value: &Value) -> CoreResult<TomlValue> {
    match value {
        Value::String(s) => Ok(TomlValue::String(s.value.clone())),
        Value::Int(i) => Ok(TomlValue::Integer(i.value)),
        Value::Decimal(d) => Ok(TomlValue::Float(d.value)),
        Value::Bool(b) => Ok(TomlValue::Boolean(b.value)),
        Value::List(list) => {
            let mut values = Vec::with_capacity(list.values.len());
            for item in &list.values {
                values.push(value_to_toml(item)?);
            }
            Ok(TomlValue::Array(values))
        }
        Value::Map(map) => {
            let mut table = toml::value::Table::new();
            for entry in &map.entries {
                let key = match &entry.key {
                    Value::String(s) => s.value.clone(),
                    _ => return Err(CoreError::from("toml serializer expects string keys")),
                };
                let value = value_to_toml(&entry.value)?;
                table.insert(key, value);
            }
            Ok(TomlValue::Table(table))
        }
        Value::Null(_) => Err(CoreError::from("toml has no null literal")),
        other => Err(CoreError::from(format!(
            "toml serializer does not support value: {other}"
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_simple_toml() {
        let input = "name = \"fp\"\ncount = 3\n";
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
