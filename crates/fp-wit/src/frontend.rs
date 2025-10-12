use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_core::ast::{
    File, FunctionParam, FunctionSignature, Ident, Item, ItemDeclFunction, ItemDefEnum,
    ItemDefStruct, ItemDefType, Module, Node, Ty, TypeTuple, Visibility,
};
use fp_core::ast::AstSerializer;
use fp_core::diagnostics::DiagnosticManager;
use fp_core::error::{Error as CoreError, Result as CoreResult};
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};

use crate::model::{WitDocument, WitFunction, WitInterface, WitPackage, WitParameter, WitType};
use crate::parser::{parse_str, sanitize_identifier};
use crate::serializer::WitSerializer;

/// Canonical identifier for the WIT frontend.
pub const WIT: &str = "wit";

/// Language frontend that parses WIT sources into FerroPhase AST nodes.
pub struct WitFrontend;

impl WitFrontend {
    pub fn new() -> Self {
        Self
    }
}

impl LanguageFrontend for WitFrontend {
    fn language(&self) -> &'static str {
        WIT
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["wit"]
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let parse_path = path.unwrap_or_else(|| Path::new("<stdin.wit>"));
        let document = parse_str(parse_path, source).map_err(into_core_error)?;

        let file = lower_document(&document, parse_path);
        let last = Node::file(file.clone());
        let ast = Node::file(file);

        let serializer: Arc<dyn AstSerializer> = Arc::new(WitSerializer::new());
        let serialized = serializer.serialize_node(&ast).ok();

        let snapshot = FrontendSnapshot {
            language: self.language().to_string(),
            description: format!("WIT document {}", parse_path.display()),
            serialized,
        };

        Ok(FrontendResult {
            last,
            ast,
            serializer,
            intrinsic_normalizer: None,
            snapshot: Some(snapshot),
            diagnostics: Arc::new(DiagnosticManager::new()),
        })
    }
}

fn lower_document(document: &WitDocument, path: &Path) -> File {
    let mut items = Vec::new();
    for package in &document.packages {
        if let Some(module) = lower_package(package) {
            items.push(Item::from(module));
        }
    }

    File {
        path: path_to_buf(path),
        items,
    }
}

fn lower_package(package: &WitPackage) -> Option<Module> {
    let mut items = Vec::new();
    for interface in &package.interfaces {
        if let Some(module) = lower_interface(interface) {
            items.push(Item::from(module));
        }
    }

    if items.is_empty() {
        return None;
    }

    let name = Ident::new(sanitize_identifier(&package.name));
    Some(Module {
        name,
        items,
        visibility: Visibility::Public,
    })
}

fn lower_interface(interface: &WitInterface) -> Option<Module> {
    let mut items = Vec::new();

    for ty in &interface.types {
        items.push(lower_type(ty));
    }

    for function in &interface.functions {
        items.push(lower_function(function));
    }

    if items.is_empty() {
        return None;
    }

    Some(Module {
        name: interface.name.clone(),
        items,
        visibility: Visibility::Public,
    })
}

fn lower_type(ty: &WitType) -> Item {
    match &ty.ty {
        Ty::Struct(struct_ty) => {
            let mut value = struct_ty.clone();
            value.name = ty.name.clone();
            Item::from(ItemDefStruct {
                visibility: Visibility::Public,
                name: ty.name.clone(),
                value,
            })
        }
        Ty::Enum(enum_ty) => {
            let mut value = enum_ty.clone();
            value.name = ty.name.clone();
            Item::from(ItemDefEnum {
                visibility: Visibility::Public,
                name: ty.name.clone(),
                value,
            })
        }
        other => Item::from(ItemDefType {
            visibility: Visibility::Public,
            name: ty.name.clone(),
            value: other.clone(),
        }),
    }
}

fn lower_function(function: &WitFunction) -> Item {
    let mut signature = FunctionSignature::unit();
    signature.name = Some(function.name.clone());
    signature.params = function
        .params
        .iter()
        .enumerate()
        .map(|(idx, param)| FunctionParam::new(param_name(param, idx), param.ty.clone()))
        .collect();
    signature.ret_ty = map_results(&function.results);

    Item::from(ItemDeclFunction {
        ty_annotation: None,
        name: function.name.clone(),
        sig: signature,
    })
}

fn param_name(param: &WitParameter, index: usize) -> Ident {
    param
        .name
        .clone()
        .unwrap_or_else(|| Ident::new(format!("arg{}", index)))
}

fn map_results(results: &[WitParameter]) -> Option<Ty> {
    match results.len() {
        0 => None,
        1 => {
            let ty = results[0].ty.clone();
            if matches!(ty, Ty::Unit(_)) {
                None
            } else {
                Some(ty)
            }
        }
        _ => Some(Ty::Tuple(TypeTuple {
            types: results.iter().map(|result| result.ty.clone()).collect(),
        })),
    }
}

fn path_to_buf(path: &Path) -> PathBuf {
    if path == Path::new("<stdin.wit>") {
        PathBuf::from("<stdin.wit>")
    } else {
        path.to_path_buf()
    }
}

fn into_core_error(err: crate::error::WitError) -> CoreError {
    CoreError::from(err.to_string())
}
