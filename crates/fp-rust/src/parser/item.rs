use fp_core::diagnostics::DiagnosticLevel;
use fp_core::error::Result;
use itertools::Itertools;
use syn::spanned::Spanned;
use syn::{Fields, FnArg, ReturnType};

use fp_core::ast::Locator;
use fp_core::ast::*;

use super::RustParser;
use crate::parser::ty::parse_struct_field;
use crate::parser::{parse_ident, parse_path, parse_vis};
// use crate::RawItemMacro;

impl RustParser {
    fn parse_fn_arg_receiver_internal(&self, r: syn::Receiver) -> Result<FunctionParamReceiver> {
        match (&r.reference, &r.mutability) {
            (Some((_, None)), Some(_)) => Ok(FunctionParamReceiver::RefMut),
            (Some((_, Some(lf))), Some(_)) if lf.ident == "static" => {
                Ok(FunctionParamReceiver::RefMutStatic)
            }
            (Some((_, None)), None) => Ok(FunctionParamReceiver::Ref),
            (Some((_, Some(lf))), None) if lf.ident == "static" => {
                Ok(FunctionParamReceiver::RefStatic)
            }
            (None, Some(_)) => Ok(FunctionParamReceiver::MutValue),
            (None, None) => Ok(FunctionParamReceiver::Value),
            _ => self.error(
                format!("Does not support receiver {:?}", r),
                FunctionParamReceiver::Value,
            ),
        }
    }

    fn parse_fn_arg_internal(&self, arg: FnArg) -> Result<Option<FunctionParam>> {
        Ok(match arg {
            FnArg::Receiver(_) => None,
            FnArg::Typed(t) => {
                let pat = self.parse_pat(*t.pat)?;
                let ident = match pat.as_ident() {
                    Some(id) => id.clone(),
                    None => match pat.kind() {
                        PatternKind::Wildcard(_) => Ident::new("_"),
                        _ => return self.error("Function parameter must be an identifier", None),
                    },
                };
                let ty = self.parse_type(*t.ty)?;
                Some(FunctionParam::new(ident, ty))
            }
        })
    }

    pub(super) fn parse_fn_sig_internal(&self, sig: syn::Signature) -> Result<FunctionSignature> {
        let mut generics_params = Vec::new();
        for param in sig.generics.params {
            match param {
                syn::GenericParam::Type(t) => {
                    let bounds =
                        self.parse_type_param_bounds_internal(t.bounds.into_iter().collect())?;
                    generics_params.push(GenericParam {
                        name: parse_ident(t.ident),
                        bounds,
                    });
                }
                syn::GenericParam::Lifetime(lifetime) => {
                    if self.lossy_mode() {
                        self.record_diagnostic(
                            DiagnosticLevel::Warning,
                            format!(
                                "Lifetime generics are not supported yet; dropping {} in lossy mode",
                                lifetime.lifetime.ident
                            ),
                        );
                    } else {
                        self.error(
                            format!("Unsupported generic parameter: {:?}", lifetime),
                            (),
                        )?;
                    }
                }
                syn::GenericParam::Const(const_param) => {
                    if self.lossy_mode() {
                        self.record_diagnostic(
                            DiagnosticLevel::Warning,
                            format!(
                                "Const generics are not supported yet; dropping {} in lossy mode",
                                const_param.ident
                            ),
                        );
                    } else {
                        self.error(
                            format!("Unsupported generic parameter: {:?}", const_param),
                            (),
                        )?;
                    }
                }
            }
        }

        let receiver = match sig.inputs.first() {
            Some(FnArg::Receiver(r)) => Some(self.parse_fn_arg_receiver_internal(r.clone())?),
            _ => None,
        };

        let mut params = Vec::new();
        for arg in sig.inputs.into_iter().skip(receiver.is_some() as usize) {
            if let Some(param) = self.parse_fn_arg_internal(arg.clone())? {
                params.push(param);
            }
        }

        Ok(FunctionSignature {
            name: Some(parse_ident(sig.ident)),
            receiver,
            params,
            generics_params,
            is_const: sig.constness.is_some(),
            quote_kind: None,
            ret_ty: match sig.output {
                ReturnType::Default => None,
                ReturnType::Type(_, ty) => Some(self.parse_type(*ty)?),
            },
        })
    }

    fn parse_use_tree_internal(&self, tree: syn::UseTree) -> Result<ItemImportTree> {
        let tree = match tree {
            syn::UseTree::Path(p) => {
                let mut path = ItemImportPath::new();
                path.push(ItemImportTree::Ident(parse_ident(p.ident)));
                let nested = self.parse_use_tree_internal(*p.tree)?;
                path.extend(nested.into_path());
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
                    group.push(self.parse_use_tree_internal(tree)?);
                }
                ItemImportTree::Group(group)
            }
        };
        Ok(tree)
    }

    pub(super) fn parse_item_use_internal(&self, u: syn::ItemUse) -> Result<ItemImport> {
        let tree = self.parse_use_tree_internal(u.tree)?;
        Ok(ItemImport {
            visibility: parse_vis(u.vis),
            tree,
        })
    }

    fn parse_item_trait_internal(&self, t: syn::ItemTrait) -> Result<ItemDefTrait> {
        let bounds = self.parse_type_param_bounds_internal(t.supertraits.into_iter().collect())?;
        let vis = parse_vis(t.vis);
        Ok(ItemDefTrait {
            name: parse_ident(t.ident),
            bounds,
            items: t
                .items
                .into_iter()
                .map(|item| self.parse_trait_item_internal(item))
                .try_collect()?,
            visibility: vis,
        })
    }

    fn parse_impl_item_internal(&self, item: syn::ImplItem) -> Result<Item> {
        match item {
            syn::ImplItem::Fn(method) => {
                let attrs = self.parse_attrs(method.attrs.clone())?;
                let func = self.parse_value_fn(syn::ItemFn {
                    attrs: method.attrs,
                    vis: method.vis.clone(),
                    sig: method.sig,
                    block: Box::new(method.block),
                })?;
                Ok(ItemKind::DefFunction(ItemDefFunction {
                    ty_annotation: None,
                    attrs,
                    name: func.name.clone().unwrap(),
                    ty: None,
                    sig: func.sig,
                    body: func.body,
                    visibility: parse_vis(method.vis),
                })
                .into())
            }
            syn::ImplItem::Type(t) => Ok(ItemKind::DefType(ItemDefType {
                name: parse_ident(t.ident),
                value: self.parse_type(t.ty)?,
                visibility: parse_vis(t.vis),
            })
            .into()),
            syn::ImplItem::Const(item) => {
                if self.lossy_mode() {
                    self.record_diagnostic(
                        DiagnosticLevel::Warning,
                        "Impl const items are not fully supported yet; skipping in lossy mode",
                    );
                    return Ok(Item::unit());
                }
                self.error(
                    format!("Does not support impl item {:?}", item),
                    Item::unit(),
                )
            }
            other => self.error(
                format!("Does not support impl item {:?}", other),
                Item::unit(),
            ),
        }
    }

    fn parse_item_static_internal(&self, s: syn::ItemStatic) -> Result<ItemDefStatic> {
        let vis = parse_vis(s.vis);
        let ty = self.parse_type(*s.ty)?;
        let value = self.parse_expr(*s.expr)?.into();
        Ok(ItemDefStatic {
            ty_annotation: None,
            name: parse_ident(s.ident),
            ty,
            value,
            visibility: vis,
        })
    }

    fn parse_item_const_internal(&self, c: syn::ItemConst) -> Result<ItemDefConst> {
        let vis = parse_vis(c.vis);
        let ty = self.parse_type(*c.ty)?;
        let value = self.parse_expr(*c.expr)?.into();
        Ok(ItemDefConst {
            mutable: None,
            ty_annotation: None,
            name: parse_ident(c.ident),
            ty: ty.into(),
            value,
            visibility: vis,
        })
    }

    fn parse_item_impl_internal(&self, im: syn::ItemImpl) -> Result<ItemImpl> {
        Ok(ItemImpl {
            trait_ty: im
                .trait_
                .map(|x| parse_path(x.1))
                .transpose()?
                .map(Locator::path),
            self_ty: Expr::value(self.parse_type(*im.self_ty.clone())?.into()),
            generics_params: parse_generics_params_simple(&im.generics),
            items: im
                .items
                .into_iter()
                .map(|item| self.parse_impl_item_internal(item))
                .try_collect()?,
        })
    }

    fn parse_item_enum_internal(&self, e: syn::ItemEnum) -> Result<ItemDefEnum> {
        let visibility = parse_vis(e.vis.clone());
        let ident = parse_ident(e.ident.clone());
        let generics_params = parse_generics_params_simple(&e.generics);
        let variants: Vec<EnumTypeVariant> = e
            .variants
            .into_iter()
            .map(|variant| -> Result<EnumTypeVariant> {
                let name = parse_ident(variant.ident);
                let ty = match variant.fields {
                    Fields::Named(_) => {
                        if self.lossy_mode() {
                            self.record_diagnostic(
                                DiagnosticLevel::Warning,
                                "Named enum fields are not supported yet; using Any in lossy mode",
                            );
                            Ty::any()
                        } else {
                            self.error("Does not support named enum fields", Ty::any())?
                        }
                    }
                    Fields::Unnamed(fields) => {
                        // Parse tuple variant fields
                        let field_types: Vec<Ty> = fields
                            .unnamed
                            .into_iter()
                            .map(|field| self.parse_type_internal(field.ty))
                            .try_collect()?;

                        if field_types.is_empty() {
                            Ty::any()
                        } else if field_types.len() == 1 {
                            // Single field tuple variant - just use the type directly
                            field_types.into_iter().next().unwrap()
                        } else {
                            // Multiple fields - create a tuple type
                            Ty::Tuple(TypeTuple { types: field_types }.into())
                        }
                    }
                    Fields::Unit => Ty::any(),
                };
                let discriminant = variant
                    .discriminant
                    .map(|(_, expr)| self.parse_expr(expr))
                    .transpose()?;
                Ok(EnumTypeVariant {
                    name,
                    value: ty,
                    discriminant: discriminant.map(Box::new),
                })
            })
            .try_collect()?;
        Ok(ItemDefEnum {
            name: ident.clone(),
            value: TypeEnum {
                name: ident.clone(),
                generics_params,
                variants,
            },
            visibility,
        })
    }

    fn parse_item_fn_internal(&self, f: syn::ItemFn) -> Result<ItemDefFunction> {
        let visibility = parse_vis(f.vis.clone());
        let attrs = self.parse_attrs(f.attrs.clone())?;
        let f = self.parse_value_fn(f)?;
        Ok(ItemDefFunction {
            ty_annotation: None,
            attrs,
            name: f.name.clone().unwrap(),
            ty: None,
            sig: f.sig,
            body: f.body,
            visibility,
        })
    }

    pub(super) fn parse_item_internal(&self, item: syn::Item) -> Result<Item> {
        let parsed = match item {
            syn::Item::Fn(f) => {
                let f = self.parse_item_fn_internal(f)?;
                ItemKind::DefFunction(f).into()
            }
            syn::Item::Impl(im) => ItemKind::Impl(self.parse_item_impl_internal(im)?).into(),
            syn::Item::Use(u) => ItemKind::Import(self.parse_item_use_internal(u)?).into(),
            syn::Item::ExternCrate(c) => {
                let visibility = parse_vis(c.vis);
                let from = parse_ident(c.ident);
                let tree = match c.rename {
                    Some((_, rename)) => ItemImportTree::Rename(ItemImportRename {
                        from,
                        to: parse_ident(rename),
                    }),
                    None => ItemImportTree::Ident(from),
                };
                ItemKind::Import(ItemImport { visibility, tree }).into()
            }
            syn::Item::ForeignMod(m) => {
                let mut items = Vec::new();
                for item in m.items {
                    match item {
                        syn::ForeignItem::Fn(f) => {
                            let sig = self.parse_fn_sig_internal(f.sig)?;
                            let name = sig.name.clone().ok_or_else(|| {
                                fp_core::error::Error::from("Missing function name")
                            })?;
                            items.push(
                                ItemKind::DeclFunction(ItemDeclFunction {
                                    ty_annotation: None,
                                    name,
                                    sig,
                                })
                                .into(),
                            );
                        }
                        other => {
                            self.record_diagnostic(
                                DiagnosticLevel::Warning,
                                format!("Ignoring unsupported foreign item: {:?}", other),
                            );
                        }
                    }
                }

                let module = Module {
                    name: Ident::new("__extern"),
                    items,
                    visibility: Visibility::Private,
                    is_external: false,
                };
                ItemKind::Module(module).into()
            }
            syn::Item::Macro(m) => {
                // Convert item macro into a first-class AST ItemMacro instead of AnyBox
                let path = parse_path(m.mac.path.clone())?;
                let delimiter = match m.mac.delimiter {
                    syn::MacroDelimiter::Paren(_) => MacroDelimiter::Parenthesis,
                    syn::MacroDelimiter::Brace(_) => MacroDelimiter::Brace,
                    syn::MacroDelimiter::Bracket(_) => MacroDelimiter::Bracket,
                };
                let tokens = m.mac.tokens.to_string();
                let mut invocation = MacroInvocation::new(path, delimiter, tokens);
                if let Some(span) = self.span_for_proc_macro(m.mac.span()) {
                    invocation = invocation.with_span(span);
                }
                ItemKind::Macro(ItemMacro::new(invocation)).into()
            }
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
                let e = self.parse_item_enum_internal(e)?;
                ItemKind::DefEnum(e).into()
            }
            syn::Item::Type(t) => {
                let visibility = parse_vis(t.vis.clone());
                let ty = self.parse_type(*t.ty)?;
                ItemKind::DefType(ItemDefType {
                    name: parse_ident(t.ident),
                    value: ty,
                    visibility,
                })
                .into()
            }
            syn::Item::Mod(m) => ItemKind::Module(self.parse_module(m)?).into(),
            syn::Item::Trait(t) => {
                let trait_ = self.parse_item_trait_internal(t)?;
                ItemKind::DefTrait(trait_).into()
            }
            syn::Item::Const(c) => {
                let c = self.parse_item_const_internal(c)?;
                ItemKind::DefConst(c).into()
            }
            syn::Item::Static(s) => {
                let s = self.parse_item_static_internal(s)?;
                ItemKind::DefStatic(s).into()
            }
            other => {
                return self.error(
                    format!("Does not support item yet: {:?}", other),
                    Item::unit(),
                );
            }
        };
        Ok(parsed)
    }

    pub(super) fn parse_trait_item_internal(&self, item: syn::TraitItem) -> Result<Item> {
        match item {
            syn::TraitItem::Fn(f) => {
                let name = parse_ident(f.sig.ident.clone());
                Ok(ItemDeclFunction {
                    ty_annotation: None,
                    name,
                    sig: self.parse_fn_sig_internal(f.sig)?,
                }
                .into())
            }
            syn::TraitItem::Type(t) => {
                let name = parse_ident(t.ident);
                let bounds =
                    self.parse_type_param_bounds_internal(t.bounds.into_iter().collect())?;
                Ok(ItemDeclType {
                    ty_annotation: None,
                    name,
                    bounds,
                }
                .into())
            }
            syn::TraitItem::Const(c) => {
                let name = parse_ident(c.ident);
                let ty = self.parse_type(c.ty)?;
                Ok(ItemDeclConst {
                    ty_annotation: None,
                    name,
                    ty,
                }
                .into())
            }
            other => self.error(
                format!("Does not support trait item {:?}", other),
                Item::unit(),
            ),
        }
    }
}

#[cfg(test)]
pub fn parse_fn_sig(sig: syn::Signature) -> Result<FunctionSignature> {
    RustParser::new().parse_fn_sig_internal(sig)
}

#[cfg(test)]
pub fn parse_item(parser: &RustParser, item: syn::Item) -> Result<Item> {
    parser.parse_item_internal(item)
}

#[cfg(test)]
pub fn parse_trait_item(parser: &RustParser, item: syn::TraitItem) -> Result<Item> {
    parser.parse_trait_item_internal(item)
}

#[cfg(test)]
pub fn parse_impl_trait(im: syn::TypeImplTrait) -> Result<ImplTraits> {
    let parser = RustParser::new();
    parse_impl_trait_with(&parser, im)
}

#[cfg(test)]
pub fn parse_use(u: syn::ItemUse) -> Result<ItemImport> {
    RustParser::new().parse_item_use_internal(u)
}

pub fn parse_impl_trait_with(parser: &RustParser, im: syn::TypeImplTrait) -> Result<ImplTraits> {
    parser
        .parse_type_param_bounds_internal(im.bounds.into_iter().collect())
        .map(|bounds| ImplTraits { bounds })
}

pub fn parse_type_struct(s: syn::ItemStruct) -> Result<TypeStruct> {
    let generics_params = parse_generics_params_simple(&s.generics);
    Ok(TypeStruct {
        name: parse_ident(s.ident),
        generics_params,
        fields: s
            .fields
            .into_iter()
            .enumerate()
            .map(|(i, f)| parse_struct_field(i, f))
            .try_collect()?,
    })
}

fn parse_generics_params_simple(generics: &syn::Generics) -> Vec<GenericParam> {
    generics
        .params
        .iter()
        .filter_map(|p| {
            if let syn::GenericParam::Type(tp) = p {
                Some(GenericParam {
                    name: parse_ident(tp.ident.clone()),
                    bounds: TypeBounds::any(),
                })
            } else {
                None
            }
        })
        .collect()
}
