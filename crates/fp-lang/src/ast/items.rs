use crate::ast::expr::{lower_expr_from_cst, lower_type_from_cst};
use crate::ast::lower_common::{decode_string_literal, split_path_prefix};
use crate::syntax::{collect_tokens, SyntaxElement, SyntaxKind, SyntaxNode};
use fp_core::ast::{
    Abi, AttrMeta, AttrMetaList, AttrMetaNameValue, AttrStyle, Attribute, BExpr, EnumTypeVariant,
    Expr, ExprAsync, ExprKind, ExprMacro, ExprUnOp, FunctionParam, FunctionParamReceiver,
    FunctionSignature, GenericParam, Ident, Item, ItemDeclConst, ItemDeclFunction, ItemDeclType,
    ItemDeclStatic, ItemDefConst, ItemDefEnum, ItemDefFunction, ItemDefStatic, ItemDefStruct,
    ItemDefTrait, ItemDefType, ItemImpl, ItemImport, ItemImportGroup, ItemImportPath,
    ItemImportRename,
    ItemImportStyle, ItemImportTree, ItemKind, ItemMacro, ItemOpaqueType, MacroDelimiter,
    MacroInvocation, Module, Name, Path, QuoteFragmentKind, ReprFlags, ReprOptions,
    StructuralField, Ty, TypeBinaryOp, TypeBinaryOpKind, TypeBounds, TypeEnum, TypeQuote,
    TypeStruct, Value, ValueNone, Visibility,
};
use fp_core::cst::CstCategory;
use fp_core::module::path::PathPrefix;
use fp_core::ops::UnOpKind;

#[derive(Debug, thiserror::Error)]
pub enum LowerItemsError {
    #[error("unexpected CST node kind: {0:?}")]
    UnexpectedNode(SyntaxKind),
    #[error("missing required token: {0}")]
    MissingToken(&'static str),
}

pub fn lower_items_from_cst(node: &SyntaxNode) -> Result<Vec<Item>, LowerItemsError> {
    if node.kind == SyntaxKind::Root {
        if let Some(list) = first_child_by_kind(node, SyntaxKind::ItemList) {
            return lower_items_from_cst(list);
        }
        return lower_items_from_cst_children(node);
    }

    if node.kind != SyntaxKind::ItemList {
        // Be forgiving: some callers may pass a single item node directly.
        if node.kind.category() == CstCategory::Item {
            if node.kind == SyntaxKind::ItemExternBlock {
                return lower_extern_block(node);
            }
            return Ok(vec![lower_item_from_cst(node)?]);
        }
        return Err(LowerItemsError::UnexpectedNode(node.kind));
    }

    lower_items_from_cst_children(node)
}

pub fn lower_file_from_cst(node: &SyntaxNode) -> Result<(Vec<Attribute>, Vec<Item>), LowerItemsError> {
    if node.kind == SyntaxKind::Root {
        if let Some(list) = first_child_by_kind(node, SyntaxKind::ItemList) {
            let mut attrs = lower_inner_attrs(node);
            attrs.extend(lower_inner_attrs(list));
            let items = lower_items_from_cst(list)?;
            return Ok((attrs, items));
        }
        let attrs = lower_inner_attrs(node);
        let items = lower_items_from_cst(node)?;
        return Ok((attrs, items));
    }

    if node.kind != SyntaxKind::ItemList {
        if node.kind.category() == CstCategory::Item {
            if node.kind == SyntaxKind::ItemExternBlock {
                let items = lower_extern_block(node)?;
                return Ok((Vec::new(), items));
            }
            let item = lower_item_from_cst(node)?;
            return Ok((Vec::new(), vec![item]));
        }
        return Err(LowerItemsError::UnexpectedNode(node.kind));
    }

    let attrs = lower_inner_attrs(node);
    let items = lower_items_from_cst(node)?;
    Ok((attrs, items))
}

pub(crate) fn lower_item_from_cst(node: &SyntaxNode) -> Result<Item, LowerItemsError> {
    let mut item = match node.kind {
        SyntaxKind::ItemUse => Ok(Item::from(ItemKind::Import(lower_use_item(node)?))),
        SyntaxKind::ItemExternCrate => Ok(Item::from(ItemKind::Import(lower_extern_crate(node)?))),
        SyntaxKind::ItemMod => Ok(Item::from(ItemKind::Module(lower_mod(node)?))),
        SyntaxKind::ItemStruct => Ok(Item::from(ItemKind::DefStruct(lower_struct(node)?))),
        SyntaxKind::ItemUnion => Ok(Item::from(ItemKind::DefStruct(lower_union(node)?))),
        SyntaxKind::ItemEnum => Ok(Item::from(ItemKind::DefEnum(lower_enum(node)?))),
        SyntaxKind::ItemTrait => Ok(Item::from(ItemKind::DefTrait(lower_trait(node)?))),
        SyntaxKind::ItemImpl => Ok(Item::from(ItemKind::Impl(lower_impl(node)?))),
        SyntaxKind::ItemTypeAlias => Ok(Item::from(ItemKind::DefType(lower_type_alias(node)?))),
        SyntaxKind::ItemOpaqueType => {
            Ok(Item::from(ItemKind::OpaqueType(lower_opaque_type(node)?)))
        }
        SyntaxKind::ItemConst => Ok(Item::from(ItemKind::DefConst(lower_const(node)?))),
        SyntaxKind::ItemStatic => Ok(Item::from(ItemKind::DefStatic(lower_static(node)?))),
        SyntaxKind::ItemFn => Ok(Item::from(ItemKind::DefFunction(lower_fn(node)?))),
        SyntaxKind::ItemExternFnDecl => Ok(Item::from(ItemKind::DeclFunction(
            lower_extern_fn_decl(node, None)?,
        ))),
        SyntaxKind::ItemExternStaticDecl => {
            Ok(Item::from(ItemKind::DeclStatic(lower_extern_static_decl(node)?)))
        }
        SyntaxKind::ItemMacro => Ok(Item::from(ItemKind::Macro(lower_item_macro(node)?))),
        SyntaxKind::ItemExpr => {
            let expr_node = first_child_by_category(node, CstCategory::Expr)
                .ok_or(LowerItemsError::MissingToken("expr"))?;
            let expr = lower_expr_from_cst(expr_node)
                .map_err(|_| LowerItemsError::UnexpectedNode(node.kind))?;
            Ok(Item::from(ItemKind::Expr(expr)))
        }
        other => Err(LowerItemsError::UnexpectedNode(other)),
    }?;

    item = item.with_span(node.span);
    Ok(item)
}

pub(crate) fn lower_extern_block(node: &SyntaxNode) -> Result<Vec<Item>, LowerItemsError> {
    let abi = lower_extern_abi(node)?;
    let mut items = Vec::new();
    for child in &node.children {
        let SyntaxElement::Node(n) = child else {
            continue;
        };
        match n.kind {
            SyntaxKind::ItemExternFnDecl => {
                let decl = lower_extern_fn_decl(n.as_ref(), Some(abi.clone()))?;
                items.push(Item::from(ItemKind::DeclFunction(decl)));
            }
            SyntaxKind::ItemExternStaticDecl => {
                let decl = lower_extern_static_decl(n.as_ref())?;
                items.push(Item::from(ItemKind::DeclStatic(decl)));
            }
            SyntaxKind::ItemFn => {
                let mut def = lower_fn(n.as_ref())?;
                def.sig.abi = abi.clone();
                items.push(Item::from(ItemKind::DefFunction(def)));
            }
            _ => {}
        }
    }
    Ok(items)
}

fn lower_extern_fn_decl(
    node: &SyntaxNode,
    inherited_abi: Option<Abi>,
) -> Result<ItemDeclFunction, LowerItemsError> {
    let sig_node = node
        .children
        .iter()
        .find_map(|c| match c {
            SyntaxElement::Node(n) if n.kind == SyntaxKind::FnSig => Some(n.as_ref()),
            _ => None,
        })
        .ok_or(LowerItemsError::MissingToken("fn sig"))?;
    let mut sig = lower_fn_sig(sig_node)?;
    sig.abi = inherited_abi.unwrap_or(lower_extern_abi(node)?);
    let name = sig
        .name
        .clone()
        .ok_or(LowerItemsError::MissingToken("fn name"))?;
    Ok(ItemDeclFunction {
        attrs: lower_outer_attrs(node),
        ty_annotation: None,
        name,
        sig,
    })
}

fn lower_extern_static_decl(node: &SyntaxNode) -> Result<ItemDeclStatic, LowerItemsError> {
    let name = Ident::new(
        first_ident_token_text_skipping(node, &["mut"])
            .ok_or(LowerItemsError::MissingToken("static name"))?,
    );
    let ty_node = first_child_by_category(node, CstCategory::Type)
        .ok_or(LowerItemsError::MissingToken("static type"))?;
    let ty =
        lower_type_from_cst(ty_node).map_err(|_| LowerItemsError::UnexpectedNode(node.kind))?;
    Ok(ItemDeclStatic {
        ty_annotation: None,
        name,
        ty,
    })
}

fn lower_extern_abi(node: &SyntaxNode) -> Result<Abi, LowerItemsError> {
    let abi_text = node.children.iter().find_map(|child| match child {
        SyntaxElement::Token(tok) if tok.text.starts_with('"') || tok.text.starts_with("r#") => {
            Some(tok.text.clone())
        }
        _ => None,
    });
    let Some(raw) = abi_text else {
        return Ok(Abi::Rust);
    };
    let cleaned = if let (Some(start), Some(end)) = (raw.find('"'), raw.rfind('"')) {
        if start < end {
            raw[start + 1..end].to_string()
        } else {
            raw.clone()
        }
    } else {
        raw.clone()
    };
    Ok(Abi::Named(cleaned))
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
    let attrs = lower_outer_attrs(node);
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
        attrs,
        visibility,
        style: ItemImportStyle::Plain,
        tree: lower_use_tree(tree)?,
    })
}

fn lower_extern_crate(node: &SyntaxNode) -> Result<ItemImport, LowerItemsError> {
    let attrs = lower_outer_attrs(node);
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
    Ok(ItemImport {
        attrs,
        visibility,
        style: ItemImportStyle::Plain,
        tree,
    })
}

fn lower_mod(node: &SyntaxNode) -> Result<Module, LowerItemsError> {
    let mut attrs = lower_outer_attrs(node);
    let visibility = lower_visibility(first_visibility(node)?)?;
    let name =
        Ident::new(first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("mod name"))?);
    let inner_list = node.children.iter().find_map(|c| match c {
        SyntaxElement::Node(n) if n.kind == SyntaxKind::ItemList => Some(n.as_ref()),
        _ => None,
    });
    let (items, is_external) = if let Some(inner_list) = inner_list {
        attrs.extend(lower_inner_attrs(inner_list));
        (lower_items_from_cst(inner_list)?, false)
    } else {
        (Vec::new(), true)
    };
    Ok(Module {
        attrs,
        name,
        items,
        visibility,
        is_external,
    })
}

fn lower_struct(node: &SyntaxNode) -> Result<ItemDefStruct, LowerItemsError> {
    let mut attrs = lower_outer_attrs(node);
    if is_const_struct(node) {
        attrs.push(const_struct_attr());
    }
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
    let mut tuple_index = 0usize;
    for child in &node.children {
        let SyntaxElement::Node(field) = child else {
            continue;
        };
        if field.kind != SyntaxKind::StructFieldDecl {
            continue;
        }
        let fname = if let Some(name) = first_ident_token_text(field) {
            Ident::new(name)
        } else if let Some(name) = first_token_text(field)
            .filter(|text| text.chars().next().is_some_and(|c| c.is_ascii_digit()))
        {
            Ident::new(name)
        } else {
            let name = tuple_index.to_string();
            tuple_index += 1;
            Ident::new(name)
        };
        let ty_node = first_child_by_category(field, CstCategory::Type)
            .ok_or(LowerItemsError::MissingToken("field type"))?;
        let mut fty = lower_type_from_cst(ty_node)
            .map_err(|_| LowerItemsError::UnexpectedNode(field.kind))?;
        let is_optional = field.children.iter().any(|c| {
            matches!(
                c,
                SyntaxElement::Token(t) if !t.is_trivia() && t.text == "?"
            )
        });
        if is_optional {
            fty = Ty::TypeBinaryOp(
                TypeBinaryOp {
                    kind: TypeBinaryOpKind::Union,
                    lhs: Box::new(fty),
                    rhs: Box::new(Ty::value(Value::None(ValueNone))),
                }
                .into(),
            );
        }
        fields.push(StructuralField::new(fname, fty));
    }
    Ok(ItemDefStruct {
        attrs,
        visibility,
        name: name.clone(),
        value: TypeStruct {
            name,
            generics_params: generics,
            repr: ReprOptions::default(),
            fields,
        },
    })
}

fn lower_union(node: &SyntaxNode) -> Result<ItemDefStruct, LowerItemsError> {
    let attrs = lower_outer_attrs(node);
    let visibility = lower_visibility(first_visibility(node)?)?;
    let name = Ident::new(
        first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("union name"))?,
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
    let mut tuple_index = 0usize;
    for child in &node.children {
        let SyntaxElement::Node(field) = child else {
            continue;
        };
        if field.kind != SyntaxKind::StructFieldDecl {
            continue;
        }
        let fname = if let Some(name) = first_ident_token_text(field) {
            Ident::new(name)
        } else if let Some(name) = first_token_text(field)
            .filter(|text| text.chars().next().is_some_and(|c| c.is_ascii_digit()))
        {
            Ident::new(name)
        } else {
            let name = tuple_index.to_string();
            tuple_index += 1;
            Ident::new(name)
        };
        let ty_node = first_child_by_category(field, CstCategory::Type)
            .ok_or(LowerItemsError::MissingToken("field type"))?;
        let mut fty = lower_type_from_cst(ty_node)
            .map_err(|_| LowerItemsError::UnexpectedNode(field.kind))?;
        let is_optional = field.children.iter().any(|c| {
            matches!(
                c,
                SyntaxElement::Token(t) if !t.is_trivia() && t.text == "?"
            )
        });
        if is_optional {
            fty = Ty::TypeBinaryOp(
                TypeBinaryOp {
                    kind: TypeBinaryOpKind::Union,
                    lhs: Box::new(fty),
                    rhs: Box::new(Ty::value(Value::None(ValueNone))),
                }
                .into(),
            );
        }
        fields.push(StructuralField::new(fname, fty));
    }

    let mut repr = ReprOptions::default();
    repr.flags.insert(ReprFlags::IS_C);
    Ok(ItemDefStruct {
        attrs,
        visibility,
        name: name.clone(),
        value: TypeStruct {
            name,
            generics_params: generics,
            repr,
            fields,
        },
    })
}

fn is_const_struct(node: &SyntaxNode) -> bool {
    let mut tokens = Vec::new();
    collect_tokens(node, &mut tokens);
    tokens
        .iter()
        .filter(|tok| !tok.is_trivia())
        .any(|tok| tok.text == "const")
}

fn const_struct_attr() -> Attribute {
    Attribute {
        style: AttrStyle::Outer,
        meta: AttrMeta::Path(Path::plain(vec![Ident::new("const".to_string())])),
    }
}

fn lower_enum(node: &SyntaxNode) -> Result<ItemDefEnum, LowerItemsError> {
    let attrs = lower_outer_attrs(node);
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
            Ty::unit()
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
        attrs,
        visibility,
        name: name.clone(),
        value: TypeEnum {
            name,
            generics_params: generics,
            repr: ReprOptions::default(),
            variants,
        },
    })
}

fn lower_type_alias(node: &SyntaxNode) -> Result<ItemDefType, LowerItemsError> {
    let attrs = lower_outer_attrs(node);
    let visibility = lower_visibility(first_visibility(node)?)?;
    let name =
        Ident::new(first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("type name"))?);
    let value_node = first_child_by_category(node, CstCategory::Type)
        .ok_or(LowerItemsError::MissingToken("type value"))?;
    let value =
        lower_type_from_cst(value_node).map_err(|_| LowerItemsError::UnexpectedNode(node.kind))?;
    Ok(ItemDefType {
        attrs,
        visibility,
        name,
        value,
    })
}

fn lower_opaque_type(node: &SyntaxNode) -> Result<ItemOpaqueType, LowerItemsError> {
    let attrs = lower_outer_attrs(node);
    let visibility = lower_visibility(first_visibility(node)?)?;
    let name = Ident::new(
        first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("opaque type name"))?,
    );
    Ok(ItemOpaqueType {
        attrs,
        visibility,
        name,
    })
}

fn lower_const(node: &SyntaxNode) -> Result<ItemDefConst, LowerItemsError> {
    let attrs = lower_outer_attrs(node);
    let visibility = lower_visibility(first_visibility(node)?)?;
    let is_mutable = node.children.iter().any(|child| match child {
        SyntaxElement::Token(token) => token.text == "mut",
        _ => false,
    });
    let name = Ident::new(
        first_ident_token_text_skipping(node, &["mut"])
            .ok_or(LowerItemsError::MissingToken("const name"))?,
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
        attrs,
        mutable: if is_mutable { Some(true) } else { None },
        ty_annotation: None,
        visibility,
        name,
        ty,
        value: Box::new(value),
    })
}

fn lower_static(node: &SyntaxNode) -> Result<ItemDefStatic, LowerItemsError> {
    let attrs = lower_outer_attrs(node);
    let visibility = lower_visibility(first_visibility(node)?)?;
    let name = Ident::new(
        first_ident_token_text_skipping(node, &["mut"])
            .ok_or(LowerItemsError::MissingToken("static name"))?,
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
        attrs,
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
    sig.abi = lower_extern_abi(node)?;

    let quote_kind = node
        .children
        .iter()
        .find_map(|c| match c {
            SyntaxElement::Token(t) if !t.is_trivia() && t.text == "quote" => Some(()),
            _ => None,
        })
        .and_then(|_| sig.ret_ty.as_ref())
        .and_then(|ty| match ty {
            Ty::Expr(expr) => match expr.kind() {
                ExprKind::Name(locator) => locator.as_ident().and_then(|id| match id.as_str() {
                    "expr" => Some(QuoteFragmentKind::Expr),
                    "stmt" => Some(QuoteFragmentKind::Stmt),
                    "item" => Some(QuoteFragmentKind::Item),
                    "type" => Some(QuoteFragmentKind::Type),
                    "items" | "fns" | "structs" | "enums" | "traits" | "impls" | "consts"
                    | "statics" | "mods" | "uses" | "macros" => {
                        tracing::warn!("deprecated plural quote fragment kind: {}", id.as_str());
                        Some(QuoteFragmentKind::Item)
                    }
                    "exprs" | "stmts" | "types" => None,
                    _ => Some(QuoteFragmentKind::Item),
                }),
                _ => None,
            },
            Ty::Quote(quote) => Some(quote.kind),
            _ => None,
        })
        .or_else(|| {
            if node.children.iter().any(
                |c| matches!(c, SyntaxElement::Token(t) if !t.is_trivia() && t.text == "quote"),
            ) {
                Some(QuoteFragmentKind::Item)
            } else {
                None
            }
        });

    let body_node = first_child_by_category(node, CstCategory::Expr)
        .ok_or(LowerItemsError::MissingToken("fn body"))?;
    let body = lower_expr_from_cst(body_node).map_err(|err| match err {
        crate::ast::expr::LowerError::UnexpectedNode(kind) => LowerItemsError::UnexpectedNode(kind),
        _ => LowerItemsError::UnexpectedNode(node.kind),
    })?;
    let is_async = node.children.iter().any(|c| {
        matches!(
            c,
            SyntaxElement::Token(t) if !t.is_trivia() && t.text == "async"
        )
    });
    let body = if is_async {
        Expr::new(ExprKind::Async(ExprAsync {
            span: body.span(),
            expr: Box::new(body),
        }))
    } else {
        body
    };
    if let Some(kind) = quote_kind {
        sig.is_const = true;
        sig.quote_kind = Some(kind);
        let inner = sig.ret_ty.as_ref().and_then(|ty| match ty {
            Ty::Quote(quote) if quote.kind == QuoteFragmentKind::Expr => {
                quote.inner.clone().map(|ty| (*ty).clone())
            }
            _ => None,
        });
        sig.ret_ty = Some(Ty::Quote(TypeQuote {
            span: node.span,
            kind,
            item: None,
            inner: inner.map(Box::new),
        }));
    }
    let mut def = ItemDefFunction::new_simple(
        sig.name
            .clone()
            .unwrap_or_else(|| Ident::new("<anon>".to_string())),
        body.into(),
    );
    def.attrs = lower_outer_attrs(node);
    def.visibility = visibility;
    def.sig = sig;
    Ok(def)
}

fn lower_outer_attrs(node: &SyntaxNode) -> Vec<Attribute> {
    node.children
        .iter()
        .filter_map(|child| match child {
            SyntaxElement::Node(attr) if attr.kind == SyntaxKind::AttrOuter => {
                lower_attr(attr.as_ref(), AttrStyle::Outer)
            }
            _ => None,
        })
        .collect()
}

fn lower_inner_attrs(node: &SyntaxNode) -> Vec<Attribute> {
    node.children
        .iter()
        .filter_map(|child| match child {
            SyntaxElement::Node(attr) if attr.kind == SyntaxKind::AttrInner => {
                lower_attr(attr.as_ref(), AttrStyle::Inner)
            }
            _ => None,
        })
        .collect()
}

fn lower_attr(node: &SyntaxNode, style: AttrStyle) -> Option<Attribute> {
    let mut tokens = Vec::new();
    crate::syntax::collect_tokens(node, &mut tokens);
    let mut inner_tokens = Vec::new();
    let mut in_brackets = false;
    for tok in tokens.iter().filter(|t| !t.is_trivia()) {
        match tok.text.as_str() {
            "[" => {
                in_brackets = true;
            }
            "]" => break,
            _ if !in_brackets => continue,
            text => inner_tokens.push(text.to_string()),
        }
    }

    let meta = parse_attr_meta(&inner_tokens)?;
    Some(Attribute { style, meta })
}

fn parse_attr_meta(tokens: &[String]) -> Option<AttrMeta> {
    let (meta, consumed) = parse_attr_meta_at(tokens, 0)?;
    if consumed != tokens.len() {
        return None;
    }
    Some(meta)
}

fn parse_attr_meta_at(tokens: &[String], mut idx: usize) -> Option<(AttrMeta, usize)> {
    let (path, next) = parse_attr_path(tokens, idx)?;
    idx = next;

    if tokens.get(idx).is_some_and(|tok| tok == "(") {
        idx += 1;
        let mut items = Vec::new();
        while !tokens.get(idx).is_some_and(|tok| tok == ")") {
            let (item, next_idx) = parse_attr_meta_at(tokens, idx)?;
            idx = next_idx;
            items.push(item);
            if tokens.get(idx).is_some_and(|tok| tok == ",") {
                idx += 1;
            }
        }
        if !tokens.get(idx).is_some_and(|tok| tok == ")") {
            return None;
        }
        idx += 1;
        return Some((AttrMeta::List(AttrMetaList { name: path, items }), idx));
    }

    if tokens.get(idx).is_some_and(|tok| tok == "=") {
        idx += 1;
        let (value_expr, next_idx) = parse_attr_value_expr(tokens, idx)?;
        return Some((
            AttrMeta::NameValue(AttrMetaNameValue {
                name: path,
                value: value_expr,
            }),
            next_idx,
        ));
    }

    Some((AttrMeta::Path(path), idx))
}

fn parse_attr_path(tokens: &[String], mut idx: usize) -> Option<(Path, usize)> {
    let mut segments = Vec::new();
    let mut saw_root = false;
    let mut saw_first_token = false;
    while let Some(tok) = tokens.get(idx) {
        if !saw_first_token && tok == "::" {
            saw_root = true;
            saw_first_token = true;
            idx += 1;
            continue;
        }
        if tok == "::" {
            idx += 1;
            continue;
        }
        if is_attr_ident(tok) {
            segments.push(Ident::new(tok.to_string()));
            idx += 1;
            saw_first_token = true;
            continue;
        }
        break;
    }
    if segments.is_empty() {
        None
    } else {
        let (prefix, segments) = split_path_prefix(segments, saw_root);
        if segments.is_empty() && matches!(prefix, PathPrefix::Plain | PathPrefix::Root) {
            return None;
        }
        Some((Path::new(prefix, segments), idx))
    }
}

fn parse_attr_value_expr(tokens: &[String], start: usize) -> Option<(BExpr, usize)> {
    let mut end = start;
    let mut paren_depth = 0usize;
    while let Some(token) = tokens.get(end) {
        match token.as_str() {
            "(" => paren_depth += 1,
            ")" => {
                if paren_depth == 0 {
                    break;
                }
                paren_depth -= 1;
            }
            "," if paren_depth == 0 => break,
            _ => {}
        }
        end += 1;
    }

    let cleaned: Vec<&str> = tokens[start..end]
        .iter()
        .map(|tok| tok.as_str())
        .filter(|t| !t.is_empty())
        .collect();
    if let Some(expr) = parse_include_str_macro_expr(&cleaned) {
        return Some((expr, end));
    }
    let value_token = cleaned.first()?;
    let expr = match *value_token {
        "true" => Box::new(Expr::value(Value::bool(true))),
        "false" => Box::new(Expr::value(Value::bool(false))),
        _ => {
            if let Ok(value) = value_token.parse::<i64>() {
                return Some((Box::new(Expr::value(Value::int(value))), end));
            }
            let decoded = decode_string_literal(value_token)?;
            Box::new(Expr::value(Value::string(decoded)))
        }
    };
    Some((expr, end))
}

fn is_attr_ident(token: &str) -> bool {
    token
        .chars()
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
}

fn parse_include_str_macro_expr(tokens: &[&str]) -> Option<BExpr> {
    if tokens.len() < 4 {
        return None;
    }
    if tokens[0] != "include_str" || tokens[1] != "!" {
        return None;
    }
    if tokens[2] != "(" || tokens[tokens.len() - 1] != ")" {
        return None;
    }
    let inner = &tokens[3..tokens.len() - 1];
    if inner.is_empty() {
        return None;
    }
    let tokens_text = inner.join(" ");
    let path = Path::plain(vec![Ident::new("include_str".to_string())]);
    let invocation = MacroInvocation::new(path, MacroDelimiter::Parenthesis, tokens_text);
    Some(Box::new(Expr::new(ExprKind::Macro(ExprMacro::new(
        invocation,
    )))))
}

fn lower_trait(node: &SyntaxNode) -> Result<ItemDefTrait, LowerItemsError> {
    fn is_type_bound_node(node: &SyntaxNode) -> bool {
        node.kind.category() == CstCategory::Type
            && node.kind != SyntaxKind::GenericParam
            && node.kind != SyntaxKind::GenericParams
    }
    let attrs = lower_outer_attrs(node);
    let visibility = lower_visibility(first_visibility(node)?)?;
    let name = Ident::new(
        first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("trait name"))?,
    );
    let bounds = node
        .children
        .iter()
        .filter_map(|c| match c {
            SyntaxElement::Node(n) if is_type_bound_node(n.as_ref()) => Some(n.as_ref()),
            _ => None,
        })
        .map(lower_bound_expr_from_cst)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|err| match err {
            LowerItemsError::UnexpectedNode(kind) => LowerItemsError::UnexpectedNode(kind),
            _ => LowerItemsError::UnexpectedNode(node.kind),
        })?;
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
        attrs,
        visibility,
        name,
        bounds,
        items,
    })
}

fn lower_trait_member(node: &SyntaxNode) -> Result<Item, LowerItemsError> {
    let head = first_token_text(node).ok_or(LowerItemsError::MissingToken("trait member"))?;
    let sig_node = node.children.iter().find_map(|c| match c {
        SyntaxElement::Node(n) if n.kind == SyntaxKind::FnSig => Some(n.as_ref()),
        _ => None,
    });
    if let Some(sig_node) = sig_node {
        let mut sig = lower_fn_sig(sig_node)?;
        if node.children.iter().any(|c| {
            matches!(
                c,
                SyntaxElement::Token(t) if !t.is_trivia() && t.text == "const"
            )
        }) {
            sig.is_const = true;
        }
        sig.abi = lower_extern_abi(node)?;
        let name = sig
            .name
            .clone()
            .ok_or(LowerItemsError::MissingToken("fn name"))?;

        if let Some(body_node) = first_child_by_category(node, CstCategory::Expr) {
            let body = lower_expr_from_cst(body_node).map_err(|err| match err {
                crate::ast::expr::LowerError::UnexpectedNode(kind) => {
                    LowerItemsError::UnexpectedNode(kind)
                }
                _ => LowerItemsError::UnexpectedNode(node.kind),
            })?;
            let is_async = node.children.iter().any(|c| {
                matches!(
                    c,
                    SyntaxElement::Token(t) if !t.is_trivia() && t.text == "async"
                )
            });
            let body = if is_async {
                Expr::new(ExprKind::Async(ExprAsync {
                    span: body.span(),
                    expr: Box::new(body),
                }))
            } else {
                body
            };
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
        return Ok(Item::from(ItemKind::DeclFunction(ItemDeclFunction {
            attrs: Vec::new(),
            ty_annotation: None,
            name,
            sig,
        })));
    }
    match head.as_str() {
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
    fn is_impl_type_node(node: &SyntaxNode) -> bool {
        node.kind.category() == CstCategory::Type
            && node.kind != SyntaxKind::GenericParam
            && node.kind != SyntaxKind::GenericParams
    }
    let attrs = lower_outer_attrs(node);
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
            SyntaxElement::Node(n) if is_impl_type_node(n.as_ref()) => Some(n.as_ref()),
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

    fn unwrap_not<'a>(
        node: &'a SyntaxNode,
        is_negative: &mut bool,
    ) -> Result<&'a SyntaxNode, LowerItemsError> {
        if node.kind != SyntaxKind::TyNot {
            return Ok(node);
        }
        let inner = node
            .children
            .iter()
            .find_map(|c| match c {
                SyntaxElement::Node(n) if n.kind.category() == CstCategory::Type => {
                    Some(n.as_ref())
                }
                _ => None,
            })
            .ok_or(LowerItemsError::UnexpectedNode(node.kind))?;
        *is_negative = true;
        Ok(inner)
    }

    let mut is_negative = false;

    let (trait_ty, self_ty_node) = if type_nodes.len() >= 2 {
        let trait_node = unwrap_not(type_nodes[0], &mut is_negative)?;
        let trait_name =
            name_from_ty_node(trait_node).ok_or(LowerItemsError::MissingToken("trait path"))?;
        (Some(trait_name), type_nodes[1])
    } else {
        let self_ty_node = unwrap_not(type_nodes[0], &mut is_negative)?;
        (None, self_ty_node)
    };

    let self_ty = if let Some(name) = name_from_ty_node(self_ty_node) {
        Expr::name(name)
    } else {
        let ty = lower_type_from_cst(self_ty_node)
            .map_err(|_| LowerItemsError::UnexpectedNode(node.kind))?;
        match ty {
            Ty::Expr(expr) if matches!(expr.kind(), ExprKind::Name(_)) => *expr,
            other => Expr::value(Value::Type(other)),
        }
    };

    Ok(ItemImpl {
        attrs,
        is_negative,
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
    let macro_tokens = crate::ast::macros::macro_group_tokens(node)
        .ok_or(LowerItemsError::UnexpectedNode(SyntaxKind::ItemMacro))?;

    let declared_name = if name.as_str() == "macro_rules" {
        find_macro_rules_name(node)
    } else {
        None
    };

    Ok(ItemMacro {
        invocation: MacroInvocation::new(
            Path::from_ident(name),
            macro_tokens.delimiter,
            macro_tokens.text,
        )
        .with_token_trees(macro_tokens.token_trees)
        .with_span(node.span),
        declared_name,
    })
}

fn find_macro_rules_name(node: &SyntaxNode) -> Option<Ident> {
    let mut tokens = Vec::new();
    collect_tokens(node, &mut tokens);
    let mut seen_bang = false;
    for tok in tokens.iter().filter(|tok| !tok.is_trivia()) {
        if !seen_bang {
            if tok.text == "!" {
                seen_bang = true;
            }
            continue;
        }
        if tok
            .text
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        {
            return Some(Ident::new(tok.text.clone()));
        }
    }
    None
}

fn lower_fn_sig(node: &SyntaxNode) -> Result<FunctionSignature, LowerItemsError> {
    let name =
        Ident::new(first_ident_token_text(node).ok_or(LowerItemsError::MissingToken("fn name"))?);

    let mut receiver: Option<FunctionParamReceiver> = None;
    let mut params: Vec<FunctionParam> = Vec::new();
    let mut saw_keyword_only_boundary = false;
    for child in &node.children {
        match child {
            SyntaxElement::Token(token) if !token.is_trivia() && token.text == "/" => {
                for param in &mut params {
                    if !param.as_tuple && !param.as_dict {
                        param.positional_only = true;
                    }
                }
            }
            SyntaxElement::Token(token) if !token.is_trivia() && token.text == "*" => {
                saw_keyword_only_boundary = true;
            }
            SyntaxElement::Node(n) if n.kind == SyntaxKind::FnReceiver => {
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
            SyntaxElement::Node(n) if n.kind == SyntaxKind::FnParam => {
                let tokens = n
                    .children
                    .iter()
                    .filter_map(|child| match child {
                        SyntaxElement::Token(token) if !token.is_trivia() => {
                            Some(token.text.as_str())
                        }
                        _ => None,
                    })
                    .collect::<Vec<_>>();
                let is_const = tokens.iter().any(|token| *token == "const");
                let is_context = tokens.first() == Some(&"context") && tokens.len() >= 2;
                let pname_text = first_ident_token_text_skipping(n, &["const", "mut", "context"])
                    .or_else(|| {
                        first_ident_token_text_deep_skipping(
                            n,
                            &["const", "mut", "context", "box", "ref"],
                        )
                    });
                if pname_text.is_none()
                    && tokens.iter().any(|token| *token == "...")
                {
                    continue;
                }
                let pname = if let Some(pname_text) = pname_text {
                    Ident::new(pname_text)
                } else if first_child_by_category(n, CstCategory::Type).is_some() {
                    Ident::new("_".to_string())
                } else {
                    return Err(LowerItemsError::MissingToken("param"));
                };
                let ty_node = first_child_by_category(n, CstCategory::Type)
                    .ok_or(LowerItemsError::MissingToken("param type"))?;
                let ty = lower_type_from_cst(ty_node)
                    .map_err(|_| LowerItemsError::UnexpectedNode(n.kind))?;
                let mut param = FunctionParam::new(pname, ty);
                param.is_const = is_const;
                param.is_context = is_context;
                param.as_tuple = tokens.first() == Some(&"*");
                param.as_dict = tokens.first() == Some(&"**");
                if saw_keyword_only_boundary {
                    param.keyword_only = true;
                }
                if param.as_tuple {
                    saw_keyword_only_boundary = true;
                }
                if let Some(default_node) = n.children.iter().find_map(|child| match child {
                    SyntaxElement::Node(node) if node.kind.category() == CstCategory::Expr => {
                        Some(node.as_ref())
                    }
                    _ => None,
                }) {
                    let expr = lower_expr_from_cst(default_node)
                        .map_err(|_| LowerItemsError::UnexpectedNode(default_node.kind))?;
                    let ExprKind::Value(value) = expr.kind() else {
                        return Err(LowerItemsError::MissingToken("literal default value"));
                    };
                    param.default = Some((**value).clone());
                }
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
        abi: Abi::Rust,
        quote_kind: None,
        ret_ty,
    })
}

fn lower_generic_params(node: &SyntaxNode) -> Result<Vec<GenericParam>, LowerItemsError> {
    fn is_bound_node(node: &SyntaxNode) -> bool {
        node.kind.category() == CstCategory::Type
            && node.kind != SyntaxKind::GenericParam
            && node.kind != SyntaxKind::GenericParams
    }
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
                SyntaxElement::Node(t) if is_bound_node(t.as_ref()) => Some(t.as_ref()),
                _ => None,
            })
            .map(lower_bound_expr_from_cst)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|_| LowerItemsError::UnexpectedNode(n.kind))?;
        out.push(GenericParam {
            name,
            bounds: TypeBounds { bounds },
        });
    }
    Ok(out)
}

fn lower_bound_expr_from_cst(node: &SyntaxNode) -> Result<Expr, LowerItemsError> {
    if node.kind == SyntaxKind::TyNot {
        let inner = node
            .children
            .iter()
            .find_map(|c| match c {
                SyntaxElement::Node(n) if n.kind.category() == CstCategory::Type => {
                    Some(n.as_ref())
                }
                _ => None,
            })
            .ok_or(LowerItemsError::UnexpectedNode(node.kind))?;
        let ty = lower_type_from_cst(inner).map_err(|err| match err {
            crate::ast::expr::LowerError::UnexpectedNode(kind) => {
                LowerItemsError::UnexpectedNode(kind)
            }
            _ => LowerItemsError::UnexpectedNode(node.kind),
        })?;
        let inner_expr = Expr::value(Value::Type(ty));
        let expr = ExprKind::UnOp(ExprUnOp {
            span: fp_core::span::Span::null(),
            op: UnOpKind::Not,
            val: Box::new(inner_expr),
        });
        return Ok(Expr::new(expr));
    }

    lower_type_from_cst(node)
        .map(|ty| Expr::value(Value::Type(ty)))
        .map_err(|err| match err {
            crate::ast::expr::LowerError::UnexpectedNode(kind) => {
                LowerItemsError::UnexpectedNode(kind)
            }
            _ => LowerItemsError::UnexpectedNode(node.kind),
        })
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

fn name_from_ty_node(node: &SyntaxNode) -> Option<Name> {
    let ty = lower_type_from_cst(node).ok()?;
    match ty {
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(name) => Some(name.clone()),
            _ => None,
        },
        _ => None,
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

fn first_child_by_kind<'a>(node: &'a SyntaxNode, kind: SyntaxKind) -> Option<&'a SyntaxNode> {
    node.children.iter().find_map(|c| match c {
        SyntaxElement::Node(n) if n.kind == kind => Some(n.as_ref()),
        _ => None,
    })
}

fn lower_items_from_cst_children(node: &SyntaxNode) -> Result<Vec<Item>, LowerItemsError> {
    let mut out = Vec::new();
    for child in &node.children {
        let SyntaxElement::Node(n) = child else {
            continue;
        };
        if n.kind.category() != CstCategory::Item {
            continue;
        }
        if n.kind == SyntaxKind::ItemExternBlock {
            out.extend(lower_extern_block(n.as_ref())?);
            continue;
        }
        out.push(lower_item_from_cst(n.as_ref())?);
    }
    Ok(out)
}

fn first_ident_token_text(node: &SyntaxNode) -> Option<String> {
    node.children.iter().find_map(|c| match c {
        SyntaxElement::Token(t)
            if !t.is_trivia()
                && t.text
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_alphabetic() || c == '_' || c == '\'') =>
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
                    .is_some_and(|c| c.is_alphabetic() || c == '_' || c == '\'') =>
        {
            Some(t.text.clone())
        }
        _ => None,
    })
}

fn first_ident_token_text_deep_skipping(node: &SyntaxNode, skip: &[&str]) -> Option<String> {
    for child in &node.children {
        match child {
            SyntaxElement::Token(t)
                if !t.is_trivia()
                    && !skip.contains(&t.text.as_str())
                    && t.text
                        .chars()
                        .next()
                        .is_some_and(|c| c.is_alphabetic() || c == '_' || c == '\'') =>
            {
                return Some(t.text.clone());
            }
            SyntaxElement::Node(n) => {
                if let Some(found) = first_ident_token_text_deep_skipping(n.as_ref(), skip) {
                    return Some(found);
                }
            }
            _ => {}
        }
    }
    None
}

fn first_token_text(node: &SyntaxNode) -> Option<String> {
    node.children.iter().find_map(|c| match c {
        SyntaxElement::Token(t) if !t.is_trivia() => Some(t.text.clone()),
        _ => None,
    })
}
