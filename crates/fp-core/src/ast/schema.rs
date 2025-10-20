use crate::{common_enum, common_struct};
use std::collections::BTreeMap;

// JSON-schema-like validation document represented as an AST node.
common_struct! {
    pub struct SchemaDocument {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub title: Option<String>,
        pub root: SchemaNode,
    }
}

impl SchemaDocument {
    pub fn new(root: SchemaNode) -> Self {
        Self { title: None, root }
    }

    pub fn with_title(mut self, title: impl Into<String>) -> Self {
        self.title = Some(title.into());
        self
    }
}

common_struct! {
    pub struct SchemaNode {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub description: Option<String>,
        #[serde(flatten)]
        pub kind: SchemaKind,
    }
}

impl SchemaNode {
    pub fn new(kind: SchemaKind) -> Self {
        Self {
            description: None,
            kind,
        }
    }

    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    pub fn string() -> Self {
        SchemaKind::String.into()
    }

    pub fn number() -> Self {
        SchemaKind::Number.into()
    }

    pub fn boolean() -> Self {
        SchemaKind::Boolean.into()
    }

    pub fn any() -> Self {
        SchemaKind::Any.into()
    }
}

impl From<SchemaKind> for SchemaNode {
    fn from(kind: SchemaKind) -> Self {
        SchemaNode::new(kind)
    }
}

common_enum! {
    pub enum SchemaKind {
        Any,
        Null,
        Boolean,
        Number,
        Integer,
        String,
        Array(SchemaArray),
        Object(SchemaObject),
        Reference(SchemaReference),
    }
}

common_struct! {
    pub struct SchemaArray {
        pub items: Box<SchemaNode>,
    }
}

impl SchemaArray {
    pub fn new(items: SchemaNode) -> Self {
        Self {
            items: Box::new(items),
        }
    }
}

common_struct! {
    pub struct SchemaObject {
        #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
        pub properties: BTreeMap<String, SchemaNode>,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub required: Vec<String>,
        #[serde(default)]
        pub additional_properties: bool,
    }
}

impl SchemaObject {
    pub fn new() -> Self {
        Self {
            properties: BTreeMap::new(),
            required: Vec::new(),
            additional_properties: true,
        }
    }

    pub fn with_property(mut self, name: impl Into<String>, schema: SchemaNode) -> Self {
        self.properties.insert(name.into(), schema);
        self
    }

    pub fn with_required(mut self, required: Vec<String>) -> Self {
        self.required = required;
        self
    }
}

common_struct! {
    pub struct SchemaReference {
        pub path: String,
    }
}

impl SchemaReference {
    pub fn new(path: impl Into<String>) -> Self {
        Self { path: path.into() }
    }
}
