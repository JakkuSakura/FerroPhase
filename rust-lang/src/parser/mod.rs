mod attr;
mod expr;
mod item;

use crate::parser::attr::parse_attrs;
use crate::parser::expr::parse_block;
use common::*;
use itertools::Itertools;
use lang_core::ast::*;
use lang_core::id::{Ident, Locator, ParameterPath, ParameterPathSegment, Path};
use lang_core::ops::BinOpKind;
use lang_core::pat::{
    Pattern, PatternIdent, PatternTuple, PatternTupleStruct, PatternType, PatternWildcard,
};
use quote::ToTokens;
use std::path::PathBuf;
use syn::parse::ParseStream;
use syn::{parse_quote, FieldsNamed, Token};
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
                                syn::GenericArgument::Type(t) => parse_type_value(t),
                                syn::GenericArgument::Const(c) => expr::parse_expr(c)
                                    .map(|x| AstType::value(Value::expr(x.get()))),
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

fn parse_type_value(t: syn::Type) -> Result<AstType> {
    let t = match t {
        syn::Type::BareFn(f) => AstType::Function(
            TypeFunction {
                params: f
                    .inputs
                    .into_iter()
                    .map(|x| x.ty)
                    .map(parse_type_value)
                    .try_collect()?,
                generics_params: vec![],
                ret: item::parse_return_type(f.output)?.into(),
            }
            .into(),
        )
        .into(),
        syn::Type::Path(p) => {
            let s = p.path.to_token_stream().to_string();
            fn int(ty: TypeInt) -> AstType {
                AstType::Primitive(TypePrimitive::Int(ty))
            }
            fn float(ty: DecimalType) -> AstType {
                AstType::Primitive(TypePrimitive::Decimal(ty))
            }

            match s.as_str() {
                "i64" => int(TypeInt::I64),
                "i32" => int(TypeInt::I32),
                "i16" => int(TypeInt::I16),
                "i8" => int(TypeInt::I8),
                "u64" => int(TypeInt::U64),
                "u32" => int(TypeInt::U32),
                "u16" => int(TypeInt::U16),
                "u8" => int(TypeInt::U8),
                "f64" => float(DecimalType::F64),
                "f32" => float(DecimalType::F32),
                _ => AstType::locator(parse_locator(p.path)?),
            }
        }
        syn::Type::ImplTrait(im) => AstType::ImplTraits(parse_impl_trait(im)?),
        syn::Type::Tuple(t) if t.elems.is_empty() => AstType::unit().into(),
        // types like t!{ }
        syn::Type::Macro(m) if m.mac.path == parse_quote!(t) => {
            AstType::expr(parse_custom_type_expr(m)?)
        }
        syn::Type::Reference(r) => AstType::Reference(parse_type_reference(r)?.into()),
        t => bail!("Type not supported {:?}", t),
    };
    Ok(t)
}
fn parse_type_reference(r: syn::TypeReference) -> Result<TypeReference> {
    Ok(TypeReference {
        ty: Box::new(parse_type_value(*r.elem)?),
        mutability: r.mutability.map(|_| true),
        lifetime: r.lifetime.map(|x| parse_ident(x.ident)),
    })
}
pub fn parse_impl_trait(im: syn::TypeImplTrait) -> Result<ImplTraits> {
    Ok(ImplTraits {
        bounds: parse_type_param_bounds(im.bounds.into_iter().collect())?,
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
            ty: parse_type_value(*p.ty)?,
        }),
        _ => bail!("Pattern not supported {}: {:?}", p.to_token_stream(), p),
    })
}

pub fn parse_type_param_bound(b: syn::TypeParamBound) -> Result<AstExpr> {
    match b {
        syn::TypeParamBound::Trait(t) => {
            let path = parse_path(t.path)?;
            Ok(AstExpr::path(path))
        }
        _ => bail!("Does not support lifetimes {:?}", b),
    }
}
pub fn parse_type_param_bounds(bs: Vec<syn::TypeParamBound>) -> Result<TypeBounds> {
    Ok(TypeBounds {
        bounds: bs.into_iter().map(parse_type_param_bound).try_collect()?,
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
    Ok(FunctionSignature {
        name: Some(parse_ident(sig.ident)),
        params: sig
            .inputs
            .into_iter()
            .map(item::parse_fn_arg)
            .try_collect()?,
        generics_params,
        ret: item::parse_return_type(sig.output)?,
    })
}
pub fn parse_trait_item(f: syn::TraitItem) -> Result<AstItem> {
    match f {
        syn::TraitItem::Fn(f) => {
            let name = parse_ident(f.sig.ident.clone());
            Ok(DeclFunction {
                name,
                sig: parse_fn_sig(f.sig)?,
            }
            .into())
        }
        syn::TraitItem::Type(t) => {
            let name = parse_ident(t.ident);
            let bounds = parse_type_param_bounds(t.bounds.into_iter().collect())?;
            Ok(DeclType { name, bounds }.into())
        }
        syn::TraitItem::Const(c) => {
            let name = parse_ident(c.ident);
            let ty = parse_type_value(c.ty)?;
            Ok(DeclConst { name, ty }.into())
        }
        _ => bail!("Does not support trait item {:?}", f),
    }
}

fn parse_member(mem: syn::Member) -> Result<Ident> {
    Ok(match mem {
        syn::Member::Named(n) => parse_ident(n),
        syn::Member::Unnamed(_) => bail!("Does not support unnamed field yet {:?}", mem),
    })
}
fn parse_field_value(fv: syn::FieldValue) -> Result<FieldValue> {
    Ok(FieldValue {
        name: parse_member(fv.member)?,
        value: Value::expr(expr::parse_expr(fv.expr)?),
    })
}

fn parse_vis(v: syn::Visibility) -> Visibility {
    match v {
        syn::Visibility::Public(_) => Visibility::Public,
        syn::Visibility::Restricted(_) => Visibility::Public,
        syn::Visibility::Inherited => Visibility::Private,
    }
}
fn parse_impl_item(item: syn::ImplItem) -> Result<AstItem> {
    match item {
        syn::ImplItem::Fn(m) => {
            let attrs = parse_attrs(m.attrs.clone())?;
            let expr = parse_value_fn(syn::ItemFn {
                attrs: m.attrs,
                vis: m.vis.clone(),
                sig: m.sig,
                block: Box::new(m.block),
            })?;
            Ok(AstItem::DefFunction(DefFunction {
                attrs,
                name: expr.name.clone().unwrap(),
                ty: None,
                value: expr,
                visibility: parse_vis(m.vis),
            }))
        }
        syn::ImplItem::Type(t) => Ok(AstItem::DefType(DefType {
            name: parse_ident(t.ident),
            value: parse_type_value(t.ty)?,
            visibility: parse_vis(t.vis),
        })),
        _ => bail!("Does not support impl item {:?}", item),
    }
}
fn parse_impl(im: syn::ItemImpl) -> Result<Impl> {
    Ok(Impl {
        trait_ty: im
            .trait_
            .map(|x| parse_path(x.1))
            .transpose()?
            .map(Locator::path),
        self_ty: AstExpr::value(parse_type_value(*im.self_ty.clone())?.into()),
        items: im.items.into_iter().map(parse_impl_item).try_collect()?,
    })
}
fn parse_struct_field(i: usize, f: syn::Field) -> Result<FieldTypeValue> {
    Ok(FieldTypeValue {
        name: f
            .ident
            .map(parse_ident)
            .unwrap_or(Ident::new(format!("{}", i))),

        value: parse_type_value(f.ty)?.into(),
    })
}
struct UnnamedStructTypeParser(TypeStructural);
impl syn::parse::Parse for UnnamedStructTypeParser {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![struct]>()?;

        let fields: FieldsNamed = input.parse()?;

        Ok(UnnamedStructTypeParser(TypeStructural {
            fields: fields
                .named
                .into_iter()
                .enumerate()
                .map(|(i, f)| parse_struct_field(i, f))
                .try_collect()
                .map_err(|err| input.error(err))?,
        }))
    }
}
enum TypeValueParser {
    UnnamedStruct(TypeStructural),
    NamedStruct(TypeStruct),
    Path(Path),
    // Ident(Ident),
}
impl Into<AstType> for TypeValueParser {
    fn into(self) -> AstType {
        match self {
            // TypeExprParser::Add(o) => TypeExpr::Op(TypeOp::Add(AddOp {
            //     lhs: o.lhs.into(),
            //     rhs: o.rhs.into(),
            // })),
            // TypeExprParser::Sub(o) => TypeExpr::Op(TypeOp::Sub(SubOp {
            //     lhs: o.lhs.into(),
            //     rhs: o.rhs.into(),
            // })),
            TypeValueParser::UnnamedStruct(s) => AstType::Structural(s),
            TypeValueParser::NamedStruct(s) => AstType::Struct(s),
            TypeValueParser::Path(p) => AstType::path(p),
            // TypeValueParser::Ident(i) => TypeValue::ident(i),
        }
    }
}
impl syn::parse::Parse for TypeValueParser {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![struct]) {
            if input.peek2(syn::Ident) {
                let s: syn::ItemStruct = input.parse()?;
                Ok(TypeValueParser::NamedStruct(
                    item::parse_item_struct(s).map_err(|err| input.error(err))?,
                ))
            } else {
                Ok(TypeValueParser::UnnamedStruct(
                    input.parse::<UnnamedStructTypeParser>()?.0,
                ))
            }
        } else {
            let path = input.parse::<syn::Path>()?;
            Ok(TypeValueParser::Path(
                parse_path(path).map_err(|err| input.error(err))?,
            ))
        }
    }
}

enum TypeExprParser {
    Add { left: AstExpr, right: AstExpr },
    Value(AstType),
}
impl syn::parse::Parse for TypeExprParser {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut lhs = TypeExprParser::Value(input.parse::<TypeValueParser>()?.into());
        loop {
            if input.is_empty() {
                break;
            }
            if input.peek(Token![+]) {
                input.parse::<Token![+]>()?;
                let rhs: AstType = input.parse::<TypeValueParser>()?.into();
                lhs = TypeExprParser::Add {
                    left: lhs.into(),
                    right: AstExpr::value(rhs.into()),
                };
            // } else if input.peek(Token![-]) {
            //     input.parse::<Token![-]>()?;
            //     let rhs: TypeValue = input.parse::<TypeValueParser>()?.into();
            //     lhs = TypeExprParser::Sub {
            //         left: lhs.into(),
            //         right: Expr::value(rhs.into()),
            //     };
            } else {
                return Err(input.error("Expected + or -"));
            }
        }
        Ok(lhs)
    }
}
impl Into<AstExpr> for TypeExprParser {
    fn into(self) -> AstExpr {
        match self {
            TypeExprParser::Add { left, right } => AstExpr::BinOp(ExprBinOp {
                lhs: left.into(),
                rhs: right.into(),
                kind: BinOpKind::Add,
            }),
            // TypeExprParser::Sub { .. } => {
            //     unreachable!()
            // }
            TypeExprParser::Value(v) => AstExpr::value(v.into()),
        }
    }
}
fn parse_custom_type_expr(m: syn::TypeMacro) -> Result<AstExpr> {
    let t: TypeExprParser = m.mac.parse_body().with_context(|| format!("{:?}", m))?;
    Ok(t.into())
}

pub fn parse_file(path: PathBuf, file: syn::File) -> Result<AstFile> {
    let module = Module {
        name: Ident::new("__file__"),
        items: file.items.into_iter().map(item::parse_item).try_collect()?,
        visibility: Visibility::Public,
    };
    Ok(AstFile { path, module })
}
pub fn parse_module(m: syn::ItemMod) -> Result<Module> {
    Ok(Module {
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
    Ok(ValueFunction {
        sig,
        body: AstExpr::block(parse_block(*f.block)?).into(),
    })
}

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
        let file = self.parse_file(path, outputs)?;
        Ok(file)
    }
    pub fn parse_value(&self, code: syn::Expr) -> Result<Value> {
        expr::parse_expr(code).map(|x| Value::expr(x.get()))
    }
    pub fn parse_expr(&self, code: syn::Expr) -> Result<AstExpr> {
        expr::parse_expr(code).map(|x| x.get())
    }
    pub fn parse_item(&self, code: syn::Item) -> Result<AstItem> {
        item::parse_item(code)
    }
    pub fn parse_file(&self, path: PathBuf, code: syn::File) -> Result<AstFile> {
        parse_file(path, code)
    }
    pub fn parse_module(&self, code: syn::ItemMod) -> Result<Module> {
        parse_module(code)
    }
    pub fn parse_type_value(&self, code: syn::Type) -> Result<AstType> {
        parse_type_value(code)
    }
}
