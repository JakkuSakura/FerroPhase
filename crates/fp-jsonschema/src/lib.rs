//! FerroPhase frontend for JSON Schema validation documents.

use std::collections::BTreeMap;
use std::path::Path;
use std::sync::Arc;

use fp_core::ast::{
    AstSerializer, Node, SchemaArray, SchemaDocument, SchemaKind, SchemaNode, SchemaObject,
    SchemaReference,
};
use fp_core::diagnostics::DiagnosticManager;
use fp_core::error::{Error as CoreError, Result as CoreResult};
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
use serde_json::Value;

/// Canonical identifier for the JSON Schema frontend.
pub const JSON_SCHEMA: &str = "jsonschema";

/// Frontend that converts JSON Schema documents into FerroPhase schema nodes.
#[derive(Debug, Default, Clone)]
pub struct JsonSchemaFrontend;

impl JsonSchemaFrontend {
    pub fn new() -> Self {
        Self
    }

    fn build_document(&self, source: &str, path: Option<&Path>) -> CoreResult<SchemaDocument> {
        let value: Value = serde_json::from_str(source)?;
        let mut document = SchemaDocument::new(lower_schema_value(&value));
        document.title = value
            .get("title")
            .and_then(|title| title.as_str())
            .map(|title| title.to_string())
            .or_else(|| {
                path.and_then(|p| p.file_name())
                    .and_then(|name| name.to_str())
                    .map(|name| name.to_string())
            });
        Ok(document)
    }
}

impl LanguageFrontend for JsonSchemaFrontend {
    fn language(&self) -> &'static str {
        JSON_SCHEMA
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["jsonschema"]
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let diagnostics = Arc::new(DiagnosticManager::new());
        let document = self.build_document(source, path)?;

        let serializer: Arc<dyn AstSerializer> = Arc::new(JsonSchemaSerializer);
        let serialized = serializer.serialize_schema(&document).ok();
        let description = match path {
            Some(path) => format!("JSON Schema document {}", path.display()),
            None => "JSON Schema document <stdin>".to_string(),
        };
        let snapshot = FrontendSnapshot {
            language: self.language().to_string(),
            description,
            serialized,
        };

        Ok(FrontendResult {
            serializer,
            last: Node::schema(document.clone()),
            ast: Node::schema(document),
            intrinsic_normalizer: None,
            macro_parser: None,
            snapshot: Some(snapshot),
            diagnostics,
        })
    }
}

struct JsonSchemaSerializer;

impl AstSerializer for JsonSchemaSerializer {
    fn serialize_schema(&self, node: &SchemaDocument) -> CoreResult<String> {
        serde_json::to_string_pretty(node).map_err(CoreError::from)
    }
}

fn lower_schema_value(value: &Value) -> SchemaNode {
    let description = value
        .get("description")
        .and_then(|desc| desc.as_str())
        .map(|desc| desc.to_string());

    let kind = if let Some(reference) = value.get("$ref").and_then(Value::as_str) {
        SchemaKind::Reference(SchemaReference::new(reference))
    } else if let Some(type_value) = value.get("type") {
        match type_value {
            Value::String(ty) => map_type_string(ty.as_str(), value),
            Value::Array(types) => {
                let mut first = SchemaKind::Any;
                for ty in types {
                    if let Some(string) = ty.as_str() {
                        let mapped = map_type_string(string, value);
                        if !matches!(mapped, SchemaKind::Any) {
                            first = mapped;
                            break;
                        }
                    }
                }
                first
            }
            _ => SchemaKind::Any,
        }
    } else if value.get("properties").is_some() {
        map_object_type(value)
    } else if value.get("items").is_some() {
        map_array_type(value)
    } else {
        SchemaKind::Any
    };

    let mut node = SchemaNode::new(kind);
    node.description = description;
    node
}

fn map_type_string(ty: &str, value: &Value) -> SchemaKind {
    match ty {
        "string" => SchemaKind::String,
        "number" => SchemaKind::Number,
        "integer" => SchemaKind::Integer,
        "boolean" => SchemaKind::Boolean,
        "null" => SchemaKind::Null,
        "array" => map_array_type(value),
        "object" => map_object_type(value),
        _ => SchemaKind::Any,
    }
}

fn map_array_type(value: &Value) -> SchemaKind {
    let items = value
        .get("items")
        .map(lower_schema_value)
        .unwrap_or_else(SchemaNode::any);
    SchemaKind::Array(SchemaArray::new(items))
}

fn map_object_type(value: &Value) -> SchemaKind {
    let mut object = SchemaObject::new();
    object.additional_properties = value
        .get("additionalProperties")
        .and_then(|ap| ap.as_bool())
        .unwrap_or(true);

    if let Some(properties) = value.get("properties").and_then(|props| props.as_object()) {
        let mut map = BTreeMap::new();
        for (name, schema_value) in properties {
            map.insert(name.clone(), lower_schema_value(schema_value));
        }
        object.properties = map;
    }

    if let Some(required) = value.get("required").and_then(|req| req.as_array()) {
        object.required = required
            .iter()
            .filter_map(|entry| entry.as_str().map(|s| s.to_string()))
            .collect();
    }

    SchemaKind::Object(object)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_simple_object_schema() {
        let schema = r#"{
            "title": "Example",
            "type": "object",
            "properties": {
                "name": {"type": "string"},
                "age": {"type": "integer"}
            },
            "required": ["name"]
        }"#;

        let frontend = JsonSchemaFrontend::new();
        let result = frontend.parse(schema, None).expect("should parse");
        let fp_core::ast::NodeKind::Schema(document) = result.ast.kind() else {
            panic!("expected schema node");
        };
        assert_eq!(document.title.as_deref(), Some("Example"));
        let SchemaKind::Object(object) = &document.root.kind else {
            panic!("expected object root");
        };
        assert!(object.properties.contains_key("name"));
        assert!(object.required.contains(&"name".to_string()));
    }
}
