use eyre::{bail, ContextCompat};
use itertools::Itertools;
use lang_core::ast::{
    AstExpr, AstItem, AstType, EnumTypeVariant, ExprSelfType, FunctionParam, ItemDefEnum,
    ItemDefFunction, ItemDefStatic, ItemDefStruct, ItemDefTrait, ItemDefType, ItemImpl, ItemImport,
    TypeEnum, TypeStruct, Visibility,
};
use lang_core::id::{Ident, Locator, Path};
use lang_core::utils::anybox::AnyBox;
use syn::{Fields, FnArg, ReturnType};

use crate::parser::attr::parse_attrs;
use crate::parser::expr::parse_expr;
use crate::parser::{parse_ident, parse_path, parse_type_value, parse_value_fn, parse_vis};
use crate::{parser, RawItemMacro, RawUse};

pub fn parse_fn_arg(i: FnArg) -> eyre::Result<FunctionParam> {
    Ok(match i {
        FnArg::Receiver(rev) => FunctionParam {
            name: Ident::new("self"),
            ty: {
                AstType::expr(AstExpr::SelfType(
                    ExprSelfType {
                        reference: rev.reference.is_some(),
                        mutability: rev.mutability.is_some(),
                    }
                    .into(),
                ))
            },
        },

        FnArg::Typed(t) => FunctionParam {
            name: parser::parse_pat(*t.pat)?
                .as_ident()
                .context("No ident")?
                .clone(),
            ty: parse_type_value(*t.ty)?,
        },
    })
}

pub fn parse_return_type(o: ReturnType) -> eyre::Result<AstType> {
    Ok(match o {
        ReturnType::Default => AstType::unit(),
        ReturnType::Type(_, t) => parse_type_value(*t)?,
    })
}

pub fn parse_use(u: syn::ItemUse) -> eyre::Result<ItemImport, RawUse> {
    let mut segments = vec![];
    let mut tree = u.tree.clone();
    loop {
        match tree {
            syn::UseTree::Path(p) => {
                segments.push(parse_ident(p.ident));
                tree = *p.tree;
            }
            syn::UseTree::Name(name) => {
                segments.push(parse_ident(name.ident));
                break;
            }
            syn::UseTree::Glob(_) => {
                segments.push(Ident::new("*"));
                break;
            }
            _ => return Err(RawUse { raw: u }.into()),
        }
    }
    Ok(ItemImport {
        visibility: parse_vis(u.vis),
        path: Path::new(segments),
    })
}

pub fn parse_type_struct(s: syn::ItemStruct) -> eyre::Result<TypeStruct> {
    Ok(TypeStruct {
        name: parse_ident(s.ident),
        fields: s
            .fields
            .into_iter()
            .enumerate()
            .map(|(i, f)| parser::parse_struct_field(i, f))
            .try_collect()?,
    })
}

fn parse_item_trait(t: syn::ItemTrait) -> eyre::Result<ItemDefTrait> {
    // TODO: generis params
    let bounds = parser::parse_type_param_bounds(t.supertraits.into_iter().collect())?;
    let vis = parse_vis(t.vis);
    Ok(ItemDefTrait {
        name: parse_ident(t.ident),
        bounds,
        items: t
            .items
            .into_iter()
            .map(|x| parser::parse_trait_item(x))
            .try_collect()?,
        visibility: vis,
    })
}

fn parse_impl_item(item: syn::ImplItem) -> eyre::Result<AstItem> {
    match item {
        syn::ImplItem::Fn(m) => {
            let attrs = parse_attrs(m.attrs.clone())?;
            let func = parse_value_fn(syn::ItemFn {
                attrs: m.attrs,
                vis: m.vis.clone(),
                sig: m.sig,
                block: Box::new(m.block),
            })?;
            Ok(AstItem::DefFunction(ItemDefFunction {
                attrs,
                name: func.name.clone().unwrap(),
                ty: None,
                sig: func.sig,
                body: func.body,
                visibility: parse_vis(m.vis),
            }))
        }
        syn::ImplItem::Type(t) => Ok(AstItem::DefType(ItemDefType {
            name: parse_ident(t.ident),
            value: parse_type_value(t.ty)?,
            visibility: parse_vis(t.vis),
        })),
        _ => bail!("Does not support impl item {:?}", item),
    }
}
fn parse_item_static(s: syn::ItemStatic) -> eyre::Result<ItemDefStatic> {
    let vis = parse_vis(s.vis);
    let ty = parse_type_value(*s.ty)?;
    let value = parse_expr(*s.expr)?;
    Ok(ItemDefStatic {
        name: parse_ident(s.ident),
        ty,
        value,
        visibility: vis,
    })
}
fn parse_item_const(s: syn::ItemConst) -> eyre::Result<ItemDefStatic> {
    let vis = parse_vis(s.vis);
    let ty = parse_type_value(*s.ty)?;
    let value = parse_expr(*s.expr)?;
    Ok(ItemDefStatic {
        name: parse_ident(s.ident),
        ty,
        value,
        visibility: vis,
    })
}
fn parse_item_impl(im: syn::ItemImpl) -> eyre::Result<ItemImpl> {
    Ok(ItemImpl {
        trait_ty: im
            .trait_
            .map(|x| parse_path(x.1))
            .transpose()?
            .map(Locator::path),
        self_ty: AstExpr::value(parse_type_value(*im.self_ty.clone())?.into()),
        items: im.items.into_iter().map(parse_impl_item).try_collect()?,
    })
}

fn parse_item_enum(e: syn::ItemEnum) -> eyre::Result<ItemDefEnum> {
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
                    AstType::any()
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
fn parse_item_fn(f: syn::ItemFn) -> eyre::Result<ItemDefFunction> {
    let visibility = parse_vis(f.vis.clone());
    let attrs = parse_attrs(f.attrs.clone())?;
    let f = parse_value_fn(f)?;
    let d = ItemDefFunction {
        attrs,
        name: f.name.clone().unwrap(),
        ty: None,
        sig: f.sig,
        body: f.body,
        visibility,
    };
    Ok(d)
}
pub fn parse_item(item: syn::Item) -> eyre::Result<AstItem> {
    let item = match item {
        syn::Item::Fn(f0) => {
            let f = parse_item_fn(f0)?;
            AstItem::DefFunction(f)
        }
        syn::Item::Impl(im) => AstItem::Impl(parse_item_impl(im)?),
        syn::Item::Use(u) => match parse_use(u) {
            Ok(i) => AstItem::Import(i),
            Err(r) => AstItem::Any(AnyBox::new(r)),
        },
        syn::Item::Macro(m) => AstItem::any(RawItemMacro { raw: m }),
        syn::Item::Struct(s) => {
            let s = parse_type_struct(s)?;
            AstItem::DefStruct(ItemDefStruct {
                name: s.name.clone(),
                value: s,
                visibility: Visibility::Private,
            })
        }
        syn::Item::Enum(e) => {
            let e = parse_item_enum(e)?;
            AstItem::DefEnum(e)
        }
        syn::Item::Type(t) => {
            let visibility = parse_vis(t.vis.clone());
            let ty = parse_type_value(*t.ty)?;
            AstItem::DefType(ItemDefType {
                name: parse_ident(t.ident),
                value: ty,
                visibility,
            })
        }
        syn::Item::Mod(m) => AstItem::Module(parser::parse_module(m)?),
        syn::Item::Trait(t) => {
            let trait_ = parse_item_trait(t)?;
            AstItem::DefTrait(trait_)
        }

        syn::Item::Const(s) => {
            let s = parse_item_const(s)?;
            AstItem::DefStatic(s)
        }
        syn::Item::Static(s) => {
            let s = parse_item_static(s)?;
            AstItem::DefStatic(s)
        }
        _ => bail!("Does not support item yet: {:?}", item),
    };
    Ok(item)
}
