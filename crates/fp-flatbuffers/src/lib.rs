//! FerroPhase frontend for FlatBuffers interface definition files.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_core::ast::{
    AstSerializer, File, Ident, Item, ItemDefEnum, ItemDefStruct, Node, TypeEnum, Visibility,
};
use fp_core::diagnostics::DiagnosticManager;
use fp_core::error::Result as CoreResult;
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
use fp_core::pretty::{pretty, PrettyOptions};
use regex::Regex;

/// Canonical identifier for FlatBuffers IDL files.
pub const FLATBUFFERS: &str = "flatbuffers";

/// Frontend that converts FlatBuffers declarations into FerroPhase struct/enum items.
#[derive(Debug, Default, Clone)]
pub struct FlatbuffersFrontend;

impl FlatbuffersFrontend {
    pub fn new() -> Self {
        Self
    }
}

impl LanguageFrontend for FlatbuffersFrontend {
    fn language(&self) -> &'static str {
        FLATBUFFERS
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["fbs"]
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let diagnostics = Arc::new(DiagnosticManager::new());
        let items = collect_items(source);

        let file = File {
            path: path
                .map(Path::to_path_buf)
                .unwrap_or_else(|| PathBuf::from("<stdin.fbs>")),
            items,
        };

        let serializer: Arc<dyn AstSerializer> = Arc::new(FlatbuffersSerializer);
        let serialized = serializer.serialize_file(&file).ok();
        let description = match path {
            Some(path) => format!("FlatBuffers file {}", path.display()),
            None => "FlatBuffers file <stdin>".to_string(),
        };

        Ok(FrontendResult {
            last: Node::file(file.clone()),
            ast: Node::file(file),
            serializer,
            intrinsic_normalizer: None,
            snapshot: Some(FrontendSnapshot {
                language: self.language().to_string(),
                description,
                serialized,
            }),
            diagnostics,
        })
    }
}

struct FlatbuffersSerializer;

impl AstSerializer for FlatbuffersSerializer {
    fn serialize_file(&self, file: &File) -> CoreResult<String> {
        Ok(pretty(file, PrettyOptions::default()).to_string())
    }
}

fn collect_items(source: &str) -> Vec<Item> {
    let mut items = Vec::new();
    let re = Regex::new(r"^(table|struct|enum)\s+([A-Za-z0-9_]+)").unwrap();

    for line in source.lines() {
        if let Some(captures) = re.captures(line.trim()) {
            let name = Ident::new(&captures[2]);
            let item = match &captures[1].to_ascii_lowercase()[..] {
                "table" | "struct" => Item::from(ItemDefStruct::new(name, Vec::new())),
                "enum" => {
                    let ident = name.clone();
                    let enum_item = ItemDefEnum {
                        visibility: Visibility::Public,
                        name: ident.clone(),
                        value: TypeEnum {
                            name: ident,
                            variants: Vec::new(),
                        },
                    };
                    Item::from(enum_item)
                }
                _ => continue,
            };
            items.push(item);
        }
    }

    items
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::ItemKind;

    #[test]
    fn parses_basic_table_and_enum() {
        let source = r#"
namespace example;

table Monster { hp: int; }
struct Vec3 { x: float; y: float; z: float; }
enum Color: byte { Red, Green, Blue }
"#;

        let frontend = FlatbuffersFrontend::new();
        let result = frontend.parse(source, None).expect("parse flatbuffers");
        let fp_core::ast::NodeKind::File(file) = result.ast.kind() else {
            panic!("expected file node");
        };
        assert!(file.items.iter().any(
            |item| matches!(item.kind(), ItemKind::DefStruct(def) if def.name.name == "Monster")
        ));
        assert!(file.items.iter().any(
            |item| matches!(item.kind(), ItemKind::DefStruct(def) if def.name.name == "Vec3")
        ));
        assert!(file
            .items
            .iter()
            .any(|item| matches!(item.kind(), ItemKind::DefEnum(def) if def.name.name == "Color")));
    }
}
