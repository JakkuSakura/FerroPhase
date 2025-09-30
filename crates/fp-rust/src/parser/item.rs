use eyre::ContextCompat;
use fp_core::bail;
use fp_core::error::Result;
use itertools::Itertools;
use syn::{Fields, FnArg, ReturnType};

use fp_core::ast::*;
use fp_core::id::Locator;

use crate::parser::attr::parse_attrs;
use crate::parser::expr::parse_expr;
use crate::parser::pat::parse_pat;
use crate::parser::ty::{parse_struct_field, parse_type, parse_type_param_bounds};
use crate::parser::{parse_ident, parse_path, parse_value_fn, parse_vis};
use crate::{parser, RawItemMacro};

fn parse_fn_arg_receiver(r: syn::Receiver) -> Result<FunctionParamReceiver> {
    // let ty = parse_type(*r.ty)?;
    // TODO: check if ty is correct
    match (&r.reference, &r.mutability) {
        (Some((_, None)), Some(_)) => Ok(FunctionParamReceiver::RefMut),
        (Some((_, Some(lf))), Some(_)) if lf.ident == "static" => {
            Ok(FunctionParamReceiver::RefMutStatic)
        }
        (Some((_, None)), None) => Ok(FunctionParamReceiver::Ref),
        (Some((_, Some(lf))), None) if lf.ident == "static" => Ok(FunctionParamReceiver::RefStatic),
        (None, Some(_)) => Ok(FunctionParamReceiver::MutValue),
        (None, None) => Ok(FunctionParamReceiver::Value),
        _ => bail!("Does not support receiver {:?}", r),
    }
}
pub fn parse_fn_arg(i: FnArg) -> Result<Option<FunctionParam>> {
    Ok(match i {
        FnArg::Receiver(_) => None,
        FnArg::Typed(t) => Some(FunctionParam::new(
            parse_pat(*t.pat)?.as_ident().context("No ident")?.clone(),
            parse_type(*t.ty)?,
        )),
    })
}

pub fn parse_fn_sig(sig: syn::Signature) -> Result<FunctionSignature> {
    let generics_params = sig
        .generics
        .params
        .into_iter()
        .map(|x| match x {
            syn::GenericParam::Type(t) => Ok(GenericParam {
                name: parse_ident(t.ident),
                bounds: parse_type_param_bounds(t.bounds.into_iter().collect())?,
            }),
            _ => bail!("Does not generic param {:?}", x),
        })
        .try_collect()?;
    let receiver = match sig.inputs.first() {
        Some(FnArg::Receiver(r)) => Some(parse_fn_arg_receiver(r.clone())?),
        _ => None,
    };
    let mut params = vec![];
    for arg in sig.inputs.into_iter().skip(receiver.is_some() as usize) {
        if let Some(p) = parse_fn_arg(arg.clone())? {
            params.push(p);
        }
    }
    Ok(FunctionSignature {
        name: Some(parse_ident(sig.ident)),
        receiver,
        params,
        generics_params,
        ret_ty: match sig.output {
            ReturnType::Default => None,
            ReturnType::Type(_, t) => Some(parse_type(*t)?),
        },
    })
}
fn parse_use_tree(tree: syn::UseTree) -> Result<ItemImportTree> {
    let tree = match tree {
        syn::UseTree::Path(p) => {
            let mut path = ItemImportPath::new();
            path.push(ItemImportTree::Ident(parse_ident(p.ident)));
            let parsed = parse_use_tree(*p.tree)?;
            path.extend(parsed.into_path());
            ItemImportTree::Path(path)
        }
        syn::UseTree::Name(name) => ItemImportTree::Ident(parse_ident(name.ident)),
        syn::UseTree::Rename(rename) => ItemImportTree::Rename(ItemImportRename {
            from: parse_ident(rename.ident),
            to: parse_ident(rename.rename),
        }),
        syn::UseTree::Glob(_) => ItemImportTree::Glob,
        syn::UseTree::Group(g) => {
            let mut group = ItemImportGroup::new();
            for tree in g.items {
                group.push(parse_use_tree(tree)?);
            }
            ItemImportTree::Group(group)
        }
    };
    Ok(tree)
}
pub fn parse_use(u: syn::ItemUse) -> Result<ItemImport> {
    let tree = parse_use_tree(u.tree)?;
    Ok(ItemImport {
        visibility: parse_vis(u.vis),
        tree,
    })
}

pub fn parse_type_struct(s: syn::ItemStruct) -> Result<TypeStruct> {
    Ok(TypeStruct {
        name: parse_ident(s.ident),
        fields: s
            .fields
            .into_iter()
            .enumerate()
            .map(|(i, f)| parse_struct_field(i, f))
            .try_collect()?,
    })
}

fn parse_item_trait(t: syn::ItemTrait) -> Result<ItemDefTrait> {
    // TODO: generis params
    let bounds = parse_type_param_bounds(t.supertraits.into_iter().collect())?;
    let vis = parse_vis(t.vis);
    Ok(ItemDefTrait {
        name: parse_ident(t.ident),
        bounds,
        items: t
            .items
            .into_iter()
            .map(|x| parse_trait_item(x))
            .try_collect()?,
        visibility: vis,
    })
}

fn parse_impl_item(item: syn::ImplItem) -> Result<Item> {
    match item {
        syn::ImplItem::Fn(m) => {
            let attrs = parse_attrs(m.attrs.clone())?;
            let func = parse_value_fn(syn::ItemFn {
                attrs: m.attrs,
                vis: m.vis.clone(),
                sig: m.sig,
                block: Box::new(m.block),
            })?;
            Ok(ItemKind::DefFunction(ItemDefFunction {
                ty_annotation: None,
                attrs,
                name: func.name.clone().unwrap(),
                ty: None,
                sig: func.sig,
                body: func.body,
                visibility: parse_vis(m.vis),
            })
            .into())
        }
        syn::ImplItem::Type(t) => Ok(ItemKind::DefType(ItemDefType {
            name: parse_ident(t.ident),
            value: parse_type(t.ty)?,
            visibility: parse_vis(t.vis),
            ty_annotation: None,
        })
        .into()),
        _ => bail!("Does not support impl item {:?}", item),
    }
}
fn parse_item_static(s: syn::ItemStatic) -> Result<ItemDefStatic> {
    let vis = parse_vis(s.vis);
    let ty = parse_type(*s.ty)?;
    let value = parse_expr(*s.expr)?.into();
    Ok(ItemDefStatic {
        ty_annotation: None,
        name: parse_ident(s.ident),
        ty,
        value,
        visibility: vis,
    })
}
fn parse_item_const(s: syn::ItemConst) -> Result<ItemDefConst> {
    let vis = parse_vis(s.vis);
    let ty = parse_type(*s.ty)?;
    let value = parse_expr(*s.expr)?.into();
    Ok(ItemDefConst {
        ty_annotation: None,
        name: parse_ident(s.ident),
        ty: ty.into(),
        value,
        visibility: vis,
    })
}
fn parse_item_impl(im: syn::ItemImpl) -> Result<ItemImpl> {
    Ok(ItemImpl {
        trait_ty: im
            .trait_
            .map(|x| parse_path(x.1))
            .transpose()?
            .map(Locator::path),
        self_ty: Expr::value(parse_type(*im.self_ty.clone())?.into()),
        items: im.items.into_iter().map(parse_impl_item).try_collect()?,
    })
}

fn parse_item_enum(e: syn::ItemEnum) -> Result<ItemDefEnum> {
    let visibility = parse_vis(e.vis.clone());
    let ident = parse_ident(e.ident.clone());
    let variants = e
        .variants
        .into_iter()
        .map(|x| {
            let name = parse_ident(x.ident);
            let ty = match x.fields {
                Fields::Named(_) => bail!("Does not support named fields"),
                Fields::Unnamed(_) => bail!("Does not support unnamed fields"),
                Fields::Unit => {
                    // be int or string
                    Ty::any()
                }
            };
            Ok(EnumTypeVariant { name, value: ty })
        })
        .try_collect()?;
    Ok(ItemDefEnum {
        name: ident.clone(),
        value: TypeEnum {
            name: ident.clone(),
            variants,
        },
        visibility,
    })
}
fn parse_item_fn(f: syn::ItemFn) -> Result<ItemDefFunction> {
    let visibility = parse_vis(f.vis.clone());
    let attrs = parse_attrs(f.attrs.clone())?;
    let f = parse_value_fn(f)?;
    let d = ItemDefFunction {
        ty_annotation: None,
        attrs,
        name: f.name.clone().unwrap(),
        ty: None,
        sig: f.sig,
        body: f.body,
        visibility,
    };
    Ok(d)
}
pub fn parse_item(item: syn::Item) -> Result<Item> {
    let item = match item {
        syn::Item::Fn(f0) => {
            let f = parse_item_fn(f0)?;
            ItemKind::DefFunction(f).into()
        }
        syn::Item::Impl(im) => ItemKind::Impl(parse_item_impl(im)?).into(),
        syn::Item::Use(u) => ItemKind::Import(parse_use(u)?).into(),
        syn::Item::Macro(m) => Item::any(RawItemMacro { raw: m }),
        syn::Item::Struct(s) => {
            let s = parse_type_struct(s)?;
            ItemKind::DefStruct(ItemDefStruct {
                name: s.name.clone(),
                value: s,
                visibility: Visibility::Private,
            })
            .into()
        }
        syn::Item::Enum(e) => {
            let e = parse_item_enum(e)?;
            ItemKind::DefEnum(e).into()
        }
        syn::Item::Type(t) => {
            let visibility = parse_vis(t.vis.clone());
            let ty = parse_type(*t.ty)?;
            ItemKind::DefType(ItemDefType {
                name: parse_ident(t.ident),
                value: ty,
                visibility,
            })
            .into()
        }
        syn::Item::Mod(m) => ItemKind::Module(parser::parse_module(m)?).into(),
        syn::Item::Trait(t) => {
            let trait_ = parse_item_trait(t)?;
            ItemKind::DefTrait(trait_).into()
        }

        syn::Item::Const(s) => {
            let s = parse_item_const(s)?;
            ItemKind::DefConst(s).into()
        }
        syn::Item::Static(s) => {
            let s = parse_item_static(s)?;
            ItemKind::DefStatic(s).into()
        }
        _ => bail!("Does not support item yet: {:?}", item),
    };
    Ok(item)
}

pub fn parse_impl_trait(im: syn::TypeImplTrait) -> Result<ImplTraits> {
    Ok(ImplTraits {
        bounds: parse_type_param_bounds(im.bounds.into_iter().collect())?,
    })
}
pub fn parse_trait_item(f: syn::TraitItem) -> Result<Item> {
    match f {
        syn::TraitItem::Fn(f) => {
            let name = parse_ident(f.sig.ident.clone());
            Ok(ItemDeclFunction {
                ty_annotation: None,
                name,
                sig: parse_fn_sig(f.sig)?,
            }
            .into())
        }
        syn::TraitItem::Type(t) => {
            let name = parse_ident(t.ident);
            let bounds = parse_type_param_bounds(t.bounds.into_iter().collect())?;
            Ok(ItemDeclType {
                ty_annotation: None,
                name,
                bounds,
            }
            .into())
        }
        syn::TraitItem::Const(c) => {
            let name = parse_ident(c.ident);
            let ty = parse_type(c.ty)?;
            Ok(ItemDeclConst {
                ty_annotation: None,
                name,
                ty,
            }
            .into())
        }
        _ => bail!("Does not support trait item {:?}", f),
    }
}
