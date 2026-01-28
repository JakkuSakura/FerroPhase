use crate::common_struct;
use std::collections::BTreeMap;

// JSON-schema-like validation document represented as an AST node.
common_struct! {
    pub struct SchemaDocument {
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
        pub description: Option<String>,
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

#[derive(Debug, Clone, PartialEq, Hash)]
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

impl From<SchemaArray> for SchemaKind {
    fn from(value: SchemaArray) -> Self {
        SchemaKind::Array(value)
    }
}

impl From<SchemaObject> for SchemaKind {
    fn from(value: SchemaObject) -> Self {
        SchemaKind::Object(value)
    }
}

impl From<SchemaReference> for SchemaKind {
    fn from(value: SchemaReference) -> Self {
        SchemaKind::Reference(value)
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
        pub properties: BTreeMap<String, SchemaNode>,
        pub required: Vec<String>,
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
