mod attr;
mod expr;
mod item;
pub mod macros;
mod ty;

use crate::parser::expr::parse_block;

use crate::parser::item::parse_fn_sig;
use common::*;
use itertools::Itertools;
use lang_core::ast::*;
use lang_core::id::{Ident, Locator, ParameterPath, ParameterPathSegment, Path};
use lang_core::pat::{
    Pattern, PatternIdent, PatternTuple, PatternTupleStruct, PatternType, PatternWildcard,
};
use quote::ToTokens;
use std::path::PathBuf;
use syn::parse_str;
use syn_inline_mod::InlinerBuilder;

pub fn parse_ident(i: syn::Ident) -> Ident {
    Ident::new(i.to_string())
}
pub fn parse_path(p: syn::Path) -> Result<Path> {
    Ok(Path {
        segments: p
            .segments
            .into_iter()
            .map(|x| {
                let ident = parse_ident(x.ident);
                ensure!(
                    x.arguments.is_none(),
                    "Does not support path arguments: {:?}",
                    x.arguments
                );
                Ok(ident)
            })
            .try_collect()?,
    })
}
pub fn parse_parameter_path(p: syn::Path) -> Result<ParameterPath> {
    Ok(ParameterPath {
        segments: p
            .segments
            .into_iter()
            .map(|x| {
                let args = match x.arguments {
                    syn::PathArguments::AngleBracketed(a) => {
                        a.args
                            .into_iter()
                            .map(|x| match x {
                                syn::GenericArgument::Type(t) => ty::parse_type(t),
                                syn::GenericArgument::Const(c) => expr::parse_expr(c)
                                    .map(|x| AstType::value(AstValue::expr(x.get()))),
                                _ => bail!("Does not support path arguments: {:?}", x),
                            })
                            .try_collect()?
                    }
                    _ => bail!("Does not support path arguments: {:?}", x),
                };
                let ident = parse_ident(x.ident);
                Ok(ParameterPathSegment { ident, args })
            })
            .try_collect()?,
    })
}
fn parse_locator(p: syn::Path) -> Result<Locator> {
    if let Ok(path) = parse_path(p.clone()) {
        return Ok(Locator::path(path));
    }
    let path = parse_parameter_path(p.clone())?;
    return Ok(Locator::parameter_path(path));
}

pub fn parse_impl_trait(im: syn::TypeImplTrait) -> Result<ImplTraits> {
    Ok(ImplTraits {
        bounds: ty::parse_type_param_bounds(im.bounds.into_iter().collect())?,
    })
}

pub fn parse_pat_ident(i: syn::PatIdent) -> Result<PatternIdent> {
    Ok(PatternIdent {
        ident: parse_ident(i.ident),
        mutability: Some(i.mutability.is_some()),
    })
}
pub fn parse_pat(p: syn::Pat) -> Result<Pattern> {
    Ok(match p {
        syn::Pat::Ident(ident) => parse_pat_ident(ident)?.into(),
        syn::Pat::Wild(_) => Pattern::Wildcard(PatternWildcard {}),
        syn::Pat::TupleStruct(t) => Pattern::TupleStruct(PatternTupleStruct {
            name: parse_locator(t.path)?,
            patterns: t.elems.into_iter().map(parse_pat).try_collect()?,
        }),
        syn::Pat::Tuple(t) => Pattern::Tuple(PatternTuple {
            patterns: t.elems.into_iter().map(parse_pat).try_collect()?,
        }),
        syn::Pat::Type(p) => Pattern::Type(PatternType {
            pat: parse_pat(*p.pat)?.into(),
            ty: ty::parse_type(*p.ty)?,
        }),
        _ => bail!("Pattern not supported {}: {:?}", p.to_token_stream(), p),
    })
}

pub fn parse_trait_item(f: syn::TraitItem) -> Result<AstItem> {
    match f {
        syn::TraitItem::Fn(f) => {
            let name = parse_ident(f.sig.ident.clone());
            Ok(ItemDeclFunction {
                name,
                sig: parse_fn_sig(f.sig)?,
            }
            .into())
        }
        syn::TraitItem::Type(t) => {
            let name = parse_ident(t.ident);
            let bounds = ty::parse_type_param_bounds(t.bounds.into_iter().collect())?;
            Ok(ItemDeclType { name, bounds }.into())
        }
        syn::TraitItem::Const(c) => {
            let name = parse_ident(c.ident);
            let ty = ty::parse_type(c.ty)?;
            Ok(ItemDeclConst { name, ty }.into())
        }
        _ => bail!("Does not support trait item {:?}", f),
    }
}

fn parse_vis(v: syn::Visibility) -> Visibility {
    match v {
        syn::Visibility::Public(_) => Visibility::Public,
        syn::Visibility::Restricted(_) => Visibility::Public,
        syn::Visibility::Inherited => Visibility::Private,
    }
}
pub fn parse_file(path: PathBuf, file: syn::File) -> Result<AstFile> {
    let items = file.items.into_iter().map(item::parse_item).try_collect()?;
    Ok(AstFile { path, items })
}
pub fn parse_module(m: syn::ItemMod) -> Result<AstModule> {
    Ok(AstModule {
        name: parse_ident(m.ident),
        items: m
            .content
            .unwrap()
            .1
            .into_iter()
            .map(item::parse_item)
            .try_collect()?,
        visibility: parse_vis(m.vis),
    })
}
pub fn parse_value_fn(f: syn::ItemFn) -> Result<ValueFunction> {
    let sig = parse_fn_sig(f.sig)?;
    let body = parse_block(*f.block)?;
    Ok(ValueFunction {
        sig,
        body: AstExpr::block(body).into(),
    })
}
#[derive(Debug, Clone, Eq, PartialEq, Copy)]
pub struct RustParser {}

impl RustParser {
    pub fn new() -> Self {
        RustParser {}
    }
    pub fn parse_file_recursively(&self, path: PathBuf) -> Result<AstFile> {
        let builder = InlinerBuilder::new();
        let path = path
            .canonicalize()
            .with_context(|| format!("Could not find file: {}", path.display()))?;
        info!("Parsing {}", path.display());
        let module = builder
            .parse_and_inline_modules(&path)
            .with_context(|| format!("path: {}", path.display()))?;
        let (outputs, errors) = module.into_output_and_errors();
        let mut errors_str = String::new();
        for err in errors {
            errors_str.push_str(&format!("{}\n", err));
        }
        ensure!(
            errors_str.is_empty(),
            "Errors when parsing {}: {}",
            path.display(),
            errors_str
        );
        let file = self.parse_file_content(path, outputs)?;
        Ok(file)
    }
    pub fn parse_value(&self, code: syn::Expr) -> Result<AstValue> {
        expr::parse_expr(code).map(|x| AstValue::expr(x.get()))
    }
    pub fn parse_expr(&self, code: syn::Expr) -> Result<AstExpr> {
        expr::parse_expr(code).map(|x| x.get())
    }
    pub fn parse_item(&self, code: syn::Item) -> Result<AstItem> {
        item::parse_item(code)
    }
    pub fn parse_items(&self, code: Vec<syn::Item>) -> Result<Vec<AstItem>> {
        code.into_iter().map(|x| self.parse_item(x)).try_collect()
    }
    pub fn parse_file_content(&self, path: PathBuf, code: syn::File) -> Result<AstFile> {
        parse_file(path, code)
    }
    pub fn parse_module(&self, code: syn::ItemMod) -> Result<AstModule> {
        parse_module(code)
    }
    pub fn parse_type(&self, code: syn::Type) -> Result<AstType> {
        ty::parse_type(code)
    }
}

impl AstDeserializer for RustParser {
    fn deserialize_node(&self, code: &str) -> Result<AstNode> {
        let code: syn::File = parse_str(code)?;
        let path = PathBuf::from("__file__");
        self.parse_file_content(path, code).map(AstNode::File)
    }

    fn deserialize_expr(&self, code: &str) -> Result<AstExpr> {
        let code: syn::Expr = parse_str(code)?;
        self.parse_expr(code)
    }

    fn deserialize_item(&self, code: &str) -> Result<AstItem> {
        let code: syn::Item = parse_str(code)?;
        self.parse_item(code)
    }

    fn deserialize_file_load(&self, path: &std::path::Path) -> Result<AstFile> {
        self.parse_file_recursively(path.to_owned())
    }
    fn deserialize_type(&self, code: &str) -> Result<AstType> {
        let code: syn::Type = parse_str(code)?;
        self.parse_type(code)
    }
}
