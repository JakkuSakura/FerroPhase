use crate::ast::lower::expr::{lower_expr_from_cst, lower_type_from_cst};
use crate::syntax::{SyntaxElement, SyntaxKind, SyntaxNode};
use fp_core::ast::{
    EnumTypeVariant, Expr, ExprKind, FunctionParam, FunctionParamReceiver, FunctionSignature,
    GenericParam, Ident, Item, ItemDeclConst, ItemDeclFunction, ItemDeclType, ItemDefConst,
    ItemDefEnum,
    ItemDefFunction, ItemDefStatic, ItemDefStruct, ItemDefTrait, ItemDefType, ItemImpl, ItemImport,
    ItemImportGroup, ItemImportPath, ItemImportRename, ItemImportTree, ItemKind, ItemMacro,
    Locator, MacroDelimiter, MacroInvocation, Module, Path, StructuralField, Ty, TypeBounds,
    TypeEnum, TypeStruct, Value, Visibility,
};
use fp_core::cst::CstCategory;

#[derive(Debug, thiserror::Error)]
pub enum LowerItemsError {
    #[error("unexpected CST node kind: {0:?}")]
    UnexpectedNode(SyntaxKind),
    #[error("missing required token: {0}")]
    MissingToken(&'static str),
}

pub fn lower_items_from_cst(node: &SyntaxNode) -> Result<Vec<Item>, LowerItemsError> {
    if node.kind != SyntaxKind::ItemList {
        // Be forgiving: some callers may pass a single item node directly.
        if node.kind.category() == CstCategory::Item {
            return Ok(vec![lower_item_from_cst(node)?]);
        }
        return Err(LowerItemsError::UnexpectedNode(node.kind));
    }

    let mut out = Vec::new();
    for child in &node.children {
        let SyntaxElement::Node(n) = child else {
            continue;
        };
        if n.kind.category() != CstCategory::Item {
            continue;
        }
        out.push(lower_item_from_cst(n.as_ref())?);
    }
    Ok(out)
}

pub(crate) fn lower_item_from_cst(node: &SyntaxNode) -> Result<Item, LowerItemsError> {
    match node.kind {
        SyntaxKind::ItemUse => Ok(Item::from(ItemKind::Import(lower_use_item(node)?))),
        SyntaxKind::ItemExternCrate => Ok(Item::from(ItemKind::Import(lower_extern_crate(node)?))),
        SyntaxKind::ItemMod => Ok(Item::from(ItemKind::Module(lower_mod(node)?))),
        SyntaxKind::ItemStruct => Ok(Item::from(ItemKind::DefStruct(lower_struct(node)?))),
        SyntaxKind::ItemEnum => Ok(Item::from(ItemKind::DefEnum(lower_enum(node)?))),
        SyntaxKind::ItemTrait => Ok(Item::from(ItemKind::DefTrait(lower_trait(node)?))),
        SyntaxKind::ItemImpl => Ok(Item::from(ItemKind::Impl(lower_impl(node)?))),
        SyntaxKind::ItemTypeAlias => Ok(Item::from(ItemKind::DefType(lower_type_alias(node)?))),
        SyntaxKind::ItemConst => Ok(Item::from(ItemKind::DefConst(lower_const(node)?))),
        SyntaxKind::ItemStatic => Ok(Item::from(ItemKind::DefStatic(lower_static(node)?))),
        SyntaxKind::ItemFn => Ok(Item::from(ItemKind::DefFunction(lower_fn(node)?))),
        SyntaxKind::ItemMacro => Ok(Item::from(ItemKind::Macro(lower_item_macro(node)?))),
        SyntaxKind::ItemExpr => {
            let expr_node = first_child_by_category(node, CstCategory::Expr)
                .ok_or(LowerItemsError::MissingToken("expr"))?;
            let expr = lower_expr_from_cst(expr_node)
                .map_err(|_| LowerItemsError::UnexpectedNode(node.kind))?;
            Ok(Item::from(ItemKind::Expr(expr)))
        }
        other => Err(LowerItemsError::UnexpectedNode(other)),
    }
}

fn lower_visibility(node: Option<&SyntaxNode>) -> Result<Visibility, LowerItemsError> {
    let Some(node) = node else {
        return Ok(Visibility::Public);
    };
    match node.kind {
        SyntaxKind::VisibilityPublic => Ok(Visibility::Public),
        SyntaxKind::VisibilityCrate => Ok(Visibility::Crate),
        SyntaxKind::VisibilityPrivate => Ok(Visibility::Private),
        SyntaxKind::VisibilityInherited => Ok(Visibility::Inherited),
        SyntaxKind::VisibilityRestricted => {
            let child = node
                .children
                .iter()
                .find_map(|c| match c {
                    SyntaxElement::Node(n) => Some(n.as_ref()),
                    _ => None,
                })
                .ok_or(LowerItemsError::MissingToken("restricted path"))?;
            Ok(Visibility::Restricted(lower_use_path_as_import_path(
                child,
            )?))
        }
        other => Err(LowerItemsError::UnexpectedNode(other)),
    }
}

fn lower_use_item(node: &SyntaxNode) -> Result<ItemImport, LowerItemsError> {
    let visibility = lower_visibility(first_visibility(node)?)?;
    let tree = node
        .children
        .iter()
        .find_map(|c| match c {
            SyntaxElement::Node(n)
                if matches!(
                    n.kind,
                    SyntaxKind::UseTreePath
                        | SyntaxKind::UseTreeGroup
                        | SyntaxKind::UseTreeGlob
                        | SyntaxKind::UseTreeRename
                ) =>
            {
                Some(n.as_ref())
            }
            _ => None,
        })
        .ok_or(LowerItemsError::MissingToken("use tree"))?;
    Ok(ItemImport {
        visibility,
        tree: lower_use_tree(tree)?,
    })
}

fn lower_extern_crate(node: &SyntaxNode) -> Result<ItemImport, LowerItemsError> {
    let visibility = lower_visibility(first_visibility(node)?)?;
    let crate_name =
        first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("crate name"))?;
    let crate_ident = Ident::new(crate_name);
    let tree = if let Some(rename) = node.children.iter().find_map(|c| match c {
        SyntaxElement::Node(n) if n.kind == SyntaxKind::UseTreeRename => Some(n.as_ref()),
        _ => None,
    }) {
        let to = rename
            .children
            .iter()
            .filter_map(|c| match c {
                SyntaxElement::Token(t) if !t.is_trivia() => Some(t.text.clone()),
                _ => None,
            })
            .nth(1)
            .ok_or(LowerItemsError::MissingToken("rename"))?;
        ItemImportTree::Rename(ItemImportRename {
            from: crate_ident,
            to: Ident::new(to),
        })
    } else {
        let mut path = ItemImportPath::new();
        path.push(ItemImportTree::Ident(crate_ident));
        ItemImportTree::Path(path)
    };
    Ok(ItemImport { visibility, tree })
}

fn lower_mod(node: &SyntaxNode) -> Result<Module, LowerItemsError> {
    let visibility = lower_visibility(first_visibility(node)?)?;
    let name =
        Ident::new(first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("mod name"))?);
    let inner_list = node
        .children
        .iter()
        .find_map(|c| match c {
            SyntaxElement::Node(n) if n.kind == SyntaxKind::ItemList => Some(n.as_ref()),
            _ => None,
        })
        .ok_or(LowerItemsError::MissingToken("mod items"))?;
    let items = lower_items_from_cst(inner_list)?;
    Ok(Module {
        name,
        items,
        visibility,
    })
}

fn lower_struct(node: &SyntaxNode) -> Result<ItemDefStruct, LowerItemsError> {
    let visibility = lower_visibility(first_visibility(node)?)?;
    let name = Ident::new(
        first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("struct name"))?,
    );

    let generics = node
        .children
        .iter()
        .find_map(|c| match c {
            SyntaxElement::Node(n) if n.kind == SyntaxKind::GenericParams => Some(n.as_ref()),
            _ => None,
        })
        .map(lower_generic_params)
        .transpose()?
        .unwrap_or_default();

    let mut fields = Vec::new();
    for child in &node.children {
        let SyntaxElement::Node(field) = child else {
            continue;
        };
        if field.kind != SyntaxKind::StructFieldDecl {
            continue;
        }
        let fname = Ident::new(
            first_ident_token_text(field).ok_or(LowerItemsError::MissingToken("field"))?,
        );
        let ty_node = first_child_by_category(field, CstCategory::Type)
            .ok_or(LowerItemsError::MissingToken("field type"))?;
        let fty = lower_type_from_cst(ty_node)
            .map_err(|_| LowerItemsError::UnexpectedNode(field.kind))?;
        fields.push(StructuralField::new(fname, fty));
    }
    Ok(ItemDefStruct {
        visibility,
        name: name.clone(),
        value: TypeStruct {
            name,
            generics_params: generics,
            fields,
        },
    })
}

fn lower_enum(node: &SyntaxNode) -> Result<ItemDefEnum, LowerItemsError> {
    let visibility = lower_visibility(first_visibility(node)?)?;
    let name =
        Ident::new(first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("enum name"))?);

    let generics = node
        .children
        .iter()
        .find_map(|c| match c {
            SyntaxElement::Node(n) if n.kind == SyntaxKind::GenericParams => Some(n.as_ref()),
            _ => None,
        })
        .map(lower_generic_params)
        .transpose()?
        .unwrap_or_default();

    let mut variants = Vec::new();
    for child in &node.children {
        let SyntaxElement::Node(v) = child else {
            continue;
        };
        if v.kind != SyntaxKind::EnumVariantDecl {
            continue;
        }
        let vname =
            Ident::new(first_ident_token_text(v).ok_or(LowerItemsError::MissingToken("variant"))?);
        let value_ty = if let Some(tn) = first_child_by_category(v, CstCategory::Type) {
            lower_type_from_cst(tn).map_err(|_| LowerItemsError::UnexpectedNode(v.kind))?
        } else {
            Ty::any()
        };
        let discriminant = first_child_by_category(v, CstCategory::Expr)
            .map(|e| lower_expr_from_cst(e).map(Box::new))
            .transpose()
            .map_err(|_| LowerItemsError::UnexpectedNode(v.kind))?;
        variants.push(EnumTypeVariant {
            name: vname,
            value: value_ty,
            discriminant,
        });
    }
    Ok(ItemDefEnum {
        visibility,
        name: name.clone(),
        value: TypeEnum {
            name,
            generics_params: generics,
            variants,
        },
    })
}

fn lower_type_alias(node: &SyntaxNode) -> Result<ItemDefType, LowerItemsError> {
    let visibility = lower_visibility(first_visibility(node)?)?;
    let name =
        Ident::new(first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("type name"))?);
    let value_node = first_child_by_category(node, CstCategory::Type)
        .ok_or(LowerItemsError::MissingToken("type value"))?;
    let value =
        lower_type_from_cst(value_node).map_err(|_| LowerItemsError::UnexpectedNode(node.kind))?;
    Ok(ItemDefType {
        visibility,
        name,
        value,
    })
}

fn lower_const(node: &SyntaxNode) -> Result<ItemDefConst, LowerItemsError> {
    let visibility = lower_visibility(first_visibility(node)?)?;
    let name = Ident::new(
        first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("const name"))?,
    );
    let ty = first_child_by_category(node, CstCategory::Type)
        .map(|t| lower_type_from_cst(t))
        .transpose()
        .map_err(|_| LowerItemsError::UnexpectedNode(node.kind))?;
    let value_node = first_child_by_category(node, CstCategory::Expr)
        .ok_or(LowerItemsError::MissingToken("const value"))?;
    let value =
        lower_expr_from_cst(value_node).map_err(|_| LowerItemsError::UnexpectedNode(node.kind))?;
    Ok(ItemDefConst {
        ty_annotation: None,
        visibility,
        name,
        ty,
        value: Box::new(value),
    })
}

fn lower_static(node: &SyntaxNode) -> Result<ItemDefStatic, LowerItemsError> {
    let visibility = lower_visibility(first_visibility(node)?)?;
    let name = Ident::new(
        first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("static name"))?,
    );
    let ty_node = first_child_by_category(node, CstCategory::Type)
        .ok_or(LowerItemsError::MissingToken("static type"))?;
    let ty =
        lower_type_from_cst(ty_node).map_err(|_| LowerItemsError::UnexpectedNode(node.kind))?;
    let value_node = first_child_by_category(node, CstCategory::Expr)
        .ok_or(LowerItemsError::MissingToken("static value"))?;
    let value =
        lower_expr_from_cst(value_node).map_err(|_| LowerItemsError::UnexpectedNode(node.kind))?;
    Ok(ItemDefStatic {
        ty_annotation: None,
        visibility,
        name,
        ty,
        value: Box::new(value),
    })
}

fn lower_fn(node: &SyntaxNode) -> Result<ItemDefFunction, LowerItemsError> {
    let visibility = lower_visibility(first_visibility(node)?)?;
    let sig_node = node
        .children
        .iter()
        .find_map(|c| match c {
            SyntaxElement::Node(n) if n.kind == SyntaxKind::FnSig => Some(n.as_ref()),
            _ => None,
        })
        .ok_or(LowerItemsError::MissingToken("fn sig"))?;
    let mut sig = lower_fn_sig(sig_node)?;

    // const fn marker stored as a token "const" directly on the item node.
    if node
        .children
        .iter()
        .any(|c| matches!(c, SyntaxElement::Token(t) if !t.is_trivia() && t.text == "const"))
    {
        sig.is_const = true;
    }

    let body_node = first_child_by_category(node, CstCategory::Expr)
        .ok_or(LowerItemsError::MissingToken("fn body"))?;
    let body = lower_expr_from_cst(body_node).map_err(|err| match err {
        crate::ast::lower::expr::LowerError::UnexpectedNode(kind) => {
            LowerItemsError::UnexpectedNode(kind)
        }
        _ => LowerItemsError::UnexpectedNode(node.kind),
    })?;
    let mut def = ItemDefFunction::new_simple(
        sig.name
            .clone()
            .unwrap_or_else(|| Ident::new("<anon>".to_string())),
        body.into(),
    );
    def.visibility = visibility;
    def.sig = sig;
    Ok(def)
}

fn lower_trait(node: &SyntaxNode) -> Result<ItemDefTrait, LowerItemsError> {
    let visibility = lower_visibility(first_visibility(node)?)?;
    let name = Ident::new(
        first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("trait name"))?,
    );
    let bounds = node
        .children
        .iter()
        .filter_map(|c| match c {
            SyntaxElement::Node(n) if n.kind.category() == CstCategory::Type => Some(n.as_ref()),
            _ => None,
        })
        .map(|t| lower_type_from_cst(t).map(|ty| Expr::value(Value::Type(ty))))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_| LowerItemsError::UnexpectedNode(node.kind))?;
    let bounds = TypeBounds { bounds };

    let mut items = Vec::new();
    for child in &node.children {
        let SyntaxElement::Node(n) = child else {
            continue;
        };
        if n.kind != SyntaxKind::TraitMember {
            continue;
        }
        items.push(lower_trait_member(n.as_ref())?);
    }

    Ok(ItemDefTrait {
        visibility,
        name,
        bounds,
        items,
    })
}

fn lower_trait_member(node: &SyntaxNode) -> Result<Item, LowerItemsError> {
    let head = first_token_text(node).ok_or(LowerItemsError::MissingToken("trait member"))?;
    match head.as_str() {
        "fn" => {
            let sig_node = node
                .children
                .iter()
                .find_map(|c| match c {
                    SyntaxElement::Node(n) if n.kind == SyntaxKind::FnSig => Some(n.as_ref()),
                    _ => None,
                })
                .ok_or(LowerItemsError::MissingToken("fn sig"))?;
            let sig = lower_fn_sig(sig_node)?;
            let name = sig
                .name
                .clone()
                .ok_or(LowerItemsError::MissingToken("fn name"))?;

            if let Some(body_node) = first_child_by_category(node, CstCategory::Expr) {
                let body = lower_expr_from_cst(body_node)
                    .map_err(|_| LowerItemsError::UnexpectedNode(node.kind))?;
                return Ok(Item::from(ItemKind::DefFunction(ItemDefFunction {
                    ty_annotation: None,
                    attrs: Vec::new(),
                    name: name.clone(),
                    ty: None,
                    sig,
                    body: Box::new(body),
                    visibility: Visibility::Inherited,
                })));
            }
            Ok(Item::from(ItemKind::DeclFunction(ItemDeclFunction {
                ty_annotation: None,
                name,
                sig,
            })))
        }
        "const" => {
            let name = Ident::new(
                first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("const name"))?,
            );
            let ty_node = first_child_by_category(node, CstCategory::Type)
                .ok_or(LowerItemsError::MissingToken("const type"))?;
            let ty = lower_type_from_cst(ty_node)
                .map_err(|_| LowerItemsError::UnexpectedNode(node.kind))?;
            Ok(Item::from(ItemKind::DeclConst(ItemDeclConst {
                ty_annotation: None,
                name,
                ty,
            })))
        }
        "type" => {
            let name = Ident::new(
                first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("type name"))?,
            );
            Ok(Item::from(ItemKind::DeclType(ItemDeclType {
                ty_annotation: None,
                name,
                bounds: TypeBounds::any(),
            })))
        }
        _ => Err(LowerItemsError::UnexpectedNode(node.kind)),
    }
}

fn lower_impl(node: &SyntaxNode) -> Result<ItemImpl, LowerItemsError> {
    let generics = node
        .children
        .iter()
        .find_map(|c| match c {
            SyntaxElement::Node(n) if n.kind == SyntaxKind::GenericParams => Some(n.as_ref()),
            _ => None,
        })
        .map(lower_generic_params)
        .transpose()?
        .unwrap_or_default();

    let type_nodes: Vec<&SyntaxNode> = node
        .children
        .iter()
        .filter_map(|c| match c {
            SyntaxElement::Node(n) if n.kind.category() == CstCategory::Type => Some(n.as_ref()),
            _ => None,
        })
        .collect();

    let items_node = node
        .children
        .iter()
        .find_map(|c| match c {
            SyntaxElement::Node(n) if n.kind == SyntaxKind::ItemList => Some(n.as_ref()),
            _ => None,
        })
        .ok_or(LowerItemsError::MissingToken("impl items"))?;
    let items = lower_items_from_cst(items_node)?;

    if type_nodes.is_empty() {
        return Err(LowerItemsError::MissingToken("impl type"));
    }

    let (trait_ty, self_ty_node) = if type_nodes.len() >= 2 {
        let trait_path =
            path_from_ty_node(type_nodes[0]).ok_or(LowerItemsError::MissingToken("trait path"))?;
        (Some(Locator::path(trait_path)), type_nodes[1])
    } else {
        (None, type_nodes[0])
    };

    let self_ty = if let Some(path) = path_from_ty_node(self_ty_node) {
        Expr::path(path)
    } else {
        let ty = lower_type_from_cst(self_ty_node)
            .map_err(|_| LowerItemsError::UnexpectedNode(node.kind))?;
        match ty {
            Ty::Expr(expr) if matches!(expr.kind(), ExprKind::Locator(_)) => *expr,
            other => Expr::value(Value::Type(other)),
        }
    };

    Ok(ItemImpl {
        trait_ty,
        self_ty,
        generics_params: generics,
        items,
    })
}

fn lower_item_macro(node: &SyntaxNode) -> Result<ItemMacro, LowerItemsError> {
    let name = Ident::new(
        first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("macro name"))?,
    );
    let delimiter = node
        .children
        .iter()
        .find_map(|c| match c {
            SyntaxElement::Token(t) if !t.is_trivia() => match t.text.as_str() {
                "(" => Some(MacroDelimiter::Parenthesis),
                "{" => Some(MacroDelimiter::Brace),
                "[" => Some(MacroDelimiter::Bracket),
                _ => None,
            },
            _ => None,
        })
        .unwrap_or(MacroDelimiter::Brace);
    Ok(ItemMacro {
        invocation: MacroInvocation::new(Path::from_ident(name), delimiter, String::new()),
    })
}

fn lower_fn_sig(node: &SyntaxNode) -> Result<FunctionSignature, LowerItemsError> {
    let name =
        Ident::new(first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("fn name"))?);

    let mut receiver: Option<FunctionParamReceiver> = None;
    let mut params: Vec<FunctionParam> = Vec::new();
    for child in &node.children {
        let SyntaxElement::Node(n) = child else {
            continue;
        };
        match n.kind {
            SyntaxKind::FnReceiver => {
                let has_ref = n.children.iter().any(
                    |c| matches!(c, SyntaxElement::Token(t) if !t.is_trivia() && t.text == "&"),
                );
                let has_mut = n.children.iter().any(
                    |c| matches!(c, SyntaxElement::Token(t) if !t.is_trivia() && t.text == "mut"),
                );
                receiver = Some(match (has_ref, has_mut) {
                    (true, true) => FunctionParamReceiver::RefMut,
                    (true, false) => FunctionParamReceiver::Ref,
                    (false, true) => FunctionParamReceiver::MutValue,
                    (false, false) => FunctionParamReceiver::Value,
                });
            }
            SyntaxKind::FnParam => {
                let is_const = n.children.iter().any(
                    |c| matches!(c, SyntaxElement::Token(t) if !t.is_trivia() && t.text == "const"),
                );
                let pname_text = if is_const {
                    first_ident_token_text_skipping(n, &["const"])
                        .ok_or(LowerItemsError::MissingToken("param"))?
                } else {
                    first_ident_token_text(n).ok_or(LowerItemsError::MissingToken("param"))?
                };
                let pname = Ident::new(pname_text);
                let ty_node = first_child_by_category(n, CstCategory::Type)
                    .ok_or(LowerItemsError::MissingToken("param type"))?;
                let ty = lower_type_from_cst(ty_node)
                    .map_err(|_| LowerItemsError::UnexpectedNode(n.kind))?;
                let mut param = FunctionParam::new(pname, ty);
                param.is_const = is_const;
                params.push(param);
            }
            _ => {}
        }
    }

    let generics_params = node
        .children
        .iter()
        .find_map(|c| match c {
            SyntaxElement::Node(n) if n.kind == SyntaxKind::GenericParams => Some(n.as_ref()),
            _ => None,
        })
        .map(lower_generic_params)
        .transpose()?
        .unwrap_or_default();

    let ret_ty = node
        .children
        .iter()
        .find_map(|c| match c {
            SyntaxElement::Node(n) if n.kind == SyntaxKind::FnRet => Some(n.as_ref()),
            _ => None,
        })
        .and_then(|ret| first_child_by_category(ret, CstCategory::Type))
        .map(|t| lower_type_from_cst(t))
        .transpose()
        .map_err(|_| LowerItemsError::UnexpectedNode(node.kind))?;

    Ok(FunctionSignature {
        name: Some(name),
        receiver,
        params,
        generics_params,
        is_const: false,
        ret_ty,
    })
}

fn lower_generic_params(node: &SyntaxNode) -> Result<Vec<GenericParam>, LowerItemsError> {
    let mut out = Vec::new();
    for child in &node.children {
        let SyntaxElement::Node(n) = child else {
            continue;
        };
        if n.kind != SyntaxKind::GenericParam {
            continue;
        }
        let name =
            Ident::new(first_ident_token_text(n).ok_or(LowerItemsError::MissingToken("generic"))?);
        let bounds = n
            .children
            .iter()
            .filter_map(|c| match c {
                SyntaxElement::Node(t) if t.kind.category() == CstCategory::Type => {
                    Some(t.as_ref())
                }
                _ => None,
            })
            .map(|t| lower_type_from_cst(t).map(|ty| Expr::value(Value::Type(ty))))
            .collect::<Result<Vec<_>, _>>()
            .map_err(|_| LowerItemsError::UnexpectedNode(n.kind))?;
        out.push(GenericParam {
            name,
            bounds: TypeBounds { bounds },
        });
    }
    Ok(out)
}

fn lower_use_tree(node: &SyntaxNode) -> Result<ItemImportTree, LowerItemsError> {
    match node.kind {
        SyntaxKind::UseTreeGroup => {
            let mut group = ItemImportGroup::new();
            for child in &node.children {
                let SyntaxElement::Node(n) = child else {
                    continue;
                };
                group.push(lower_use_tree(n.as_ref())?);
            }
            Ok(ItemImportTree::Group(group))
        }
        SyntaxKind::UseTreeGlob => Ok(ItemImportTree::Glob),
        SyntaxKind::UseTreeRename => {
            let mut ids = node.children.iter().filter_map(|c| match c {
                SyntaxElement::Token(t) if !t.is_trivia() => Some(t.text.clone()),
                _ => None,
            });
            let from = ids
                .next()
                .ok_or(LowerItemsError::MissingToken("rename from"))?;
            let to = ids
                .next()
                .ok_or(LowerItemsError::MissingToken("rename to"))?;
            Ok(ItemImportTree::Rename(ItemImportRename {
                from: Ident::new(from),
                to: Ident::new(to),
            }))
        }
        SyntaxKind::UseTreePath => Ok(ItemImportTree::Path(lower_use_path_as_import_path(node)?)),
        other => Err(LowerItemsError::UnexpectedNode(other)),
    }
}

fn lower_use_path_as_import_path(node: &SyntaxNode) -> Result<ItemImportPath, LowerItemsError> {
    let mut path = ItemImportPath::new();
    for child in &node.children {
        match child {
            SyntaxElement::Node(n) => match n.kind {
                SyntaxKind::UseTreeRoot => path.push(ItemImportTree::Root),
                SyntaxKind::UseTreeSelf => path.push(ItemImportTree::SelfMod),
                SyntaxKind::UseTreeSuper => path.push(ItemImportTree::SuperMod),
                SyntaxKind::UseTreeGroup | SyntaxKind::UseTreeGlob | SyntaxKind::UseTreeRename => {
                    path.push(lower_use_tree(n.as_ref())?)
                }
                _ => {}
            },
            SyntaxElement::Token(t) if !t.is_trivia() => {
                if t.text == "::" {
                    continue;
                }
                if t.text == "crate" {
                    path.push(ItemImportTree::Crate);
                } else {
                    path.push(ItemImportTree::Ident(Ident::new(t.text.clone())));
                }
            }
            _ => {}
        }
    }
    Ok(path)
}

fn path_from_ty_node(node: &SyntaxNode) -> Option<Path> {
    if node.kind != SyntaxKind::TyPath {
        return None;
    }
    let mut segments = Vec::new();
    let mut saw_generic_start = false;
    for child in &node.children {
        let SyntaxElement::Token(t) = child else {
            continue;
        };
        if t.is_trivia() {
            continue;
        }
        match t.text.as_str() {
            "::" => {}
            "<" => {
                saw_generic_start = true;
                break;
            }
            ">" | "," | "=" => {}
            _ => {
                if t.text
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
                {
                    segments.push(Ident::new(t.text.clone()));
                }
            }
        }
    }
    if saw_generic_start {
        return None;
    }
    if segments.is_empty() {
        None
    } else {
        Some(Path::new(segments))
    }
}

fn first_visibility<'a>(node: &'a SyntaxNode) -> Result<Option<&'a SyntaxNode>, LowerItemsError> {
    Ok(node.children.iter().find_map(|c| match c {
        SyntaxElement::Node(n)
            if matches!(
                n.kind,
                SyntaxKind::VisibilityPublic
                    | SyntaxKind::VisibilityCrate
                    | SyntaxKind::VisibilityRestricted
                    | SyntaxKind::VisibilityPrivate
                    | SyntaxKind::VisibilityInherited
            ) =>
        {
            Some(n.as_ref())
        }
        _ => None,
    }))
}

fn first_child_by_category<'a>(node: &'a SyntaxNode, cat: CstCategory) -> Option<&'a SyntaxNode> {
    node.children.iter().find_map(|c| match c {
        SyntaxElement::Node(n) if n.kind.category() == cat => Some(n.as_ref()),
        _ => None,
    })
}

fn first_ident_token_text(node: &SyntaxNode) -> Option<String> {
    node.children.iter().find_map(|c| match c {
        SyntaxElement::Token(t)
            if !t.is_trivia()
                && t.text
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_alphabetic() || c == '_') =>
        {
            Some(t.text.clone())
        }
        _ => None,
    })
}

fn first_ident_token_text_skipping(node: &SyntaxNode, skip: &[&str]) -> Option<String> {
    node.children.iter().find_map(|c| match c {
        SyntaxElement::Token(t)
            if !t.is_trivia()
                && !skip.contains(&t.text.as_str())
                && t.text
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_alphabetic() || c == '_') =>
        {
            Some(t.text.clone())
        }
        _ => None,
    })
}

fn first_token_text(node: &SyntaxNode) -> Option<String> {
    node.children.iter().find_map(|c| match c {
        SyntaxElement::Token(t) if !t.is_trivia() => Some(t.text.clone()),
        _ => None,
    })
}
