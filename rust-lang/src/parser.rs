use crate::{RawExpr, RawExprMacro, RawItemMacro, RawStmtMacro, RawUse};
use common::*;
use itertools::Itertools;
use lang_core::ast::*;
use lang_core::id::{Ident, Locator, ParameterPath, ParameterPathSegment, Path};
use lang_core::ops::{BinOpKind, UnOpKind};
use lang_core::pat::{
    Pattern, PatternIdent, PatternTuple, PatternTupleStruct, PatternType, PatternWildcard,
};
use lang_core::utils::anybox::AnyBox;
use quote::ToTokens;
use std::path::PathBuf;
use syn::parse::ParseStream;
use syn::{parse_quote, Fields, FieldsNamed, FnArg, Lit, ReturnType, Token};
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
                    syn::PathArguments::AngleBracketed(a) => a
                        .args
                        .into_iter()
                        .map(|x| match x {
                            syn::GenericArgument::Type(t) => parse_type_value(t),
                            syn::GenericArgument::Const(c) => {
                                parse_expr(c).map(|x| Type::value(Value::expr(x.get())))
                            }
                            _ => bail!("Does not support path arguments: {:?}", x),
                        })
                        .try_collect()?,
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

fn parse_type_value(t: syn::Type) -> Result<Type> {
    let t = match t {
        syn::Type::BareFn(f) => Type::Function(
            TypeFunction {
                params: f
                    .inputs
                    .into_iter()
                    .map(|x| x.ty)
                    .map(parse_type_value)
                    .try_collect()?,
                generics_params: vec![],
                ret: parse_return_type(f.output)?.into(),
            }
            .into(),
        )
        .into(),
        syn::Type::Path(p) => {
            let s = p.path.to_token_stream().to_string();
            fn int(ty: TypeInt) -> Type {
                Type::Primitive(TypePrimitive::Int(ty))
            }
            fn float(ty: DecimalType) -> Type {
                Type::Primitive(TypePrimitive::Decimal(ty))
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
                _ => Type::locator(parse_locator(p.path)?),
            }
        }
        syn::Type::ImplTrait(im) => Type::ImplTraits(parse_impl_trait(im)?),
        syn::Type::Tuple(t) if t.elems.is_empty() => Type::unit().into(),
        // types like t!{ }
        syn::Type::Macro(m) if m.mac.path == parse_quote!(t) => {
            Type::expr(parse_custom_type_expr(m)?)
        }
        syn::Type::Reference(r) => Type::Reference(parse_type_reference(r)?.into()),
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
fn parse_impl_trait(im: syn::TypeImplTrait) -> Result<ImplTraits> {
    Ok(ImplTraits {
        bounds: parse_type_param_bounds(im.bounds.into_iter().collect())?,
    })
}
fn parse_input(i: FnArg) -> Result<FunctionParam> {
    Ok(match i {
        FnArg::Receiver(rev) => FunctionParam {
            name: Ident::new("self"),
            ty: {
                Type::expr(Expr::SelfType(
                    ExprSelfType {
                        reference: rev.reference.is_some(),
                        mutability: rev.mutability.is_some(),
                    }
                    .into(),
                ))
            },
        },

        FnArg::Typed(t) => FunctionParam {
            name: parse_pat(*t.pat)?.as_ident().context("No ident")?.clone(),
            ty: parse_type_value(*t.ty)?,
        },
    })
}

fn parse_pat_ident(i: syn::PatIdent) -> Result<PatternIdent> {
    Ok(PatternIdent {
        ident: parse_ident(i.ident),
        mutability: Some(i.mutability.is_some()),
    })
}
fn parse_pat(p: syn::Pat) -> Result<Pattern> {
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

fn parse_return_type(o: ReturnType) -> Result<Type> {
    Ok(match o {
        ReturnType::Default => Type::unit(),
        ReturnType::Type(_, t) => parse_type_value(*t)?,
    })
}
fn parse_type_param_bound(b: syn::TypeParamBound) -> Result<Expr> {
    match b {
        syn::TypeParamBound::Trait(t) => {
            let path = parse_path(t.path)?;
            Ok(Expr::path(path))
        }
        _ => bail!("Does not support lifetimes {:?}", b),
    }
}
fn parse_type_param_bounds(bs: Vec<syn::TypeParamBound>) -> Result<TypeBounds> {
    Ok(TypeBounds {
        bounds: bs.into_iter().map(parse_type_param_bound).try_collect()?,
    })
}
fn parse_fn_sig(sig: syn::Signature) -> Result<FunctionSignature> {
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
        params: sig.inputs.into_iter().map(parse_input).try_collect()?,
        generics_params,
        ret: parse_return_type(sig.output)?,
    })
}
fn parse_fn(f: syn::ItemFn) -> Result<ValueFunction> {
    let sig = parse_fn_sig(f.sig)?;
    Ok(ValueFunction {
        sig,
        body: Expr::block(parse_block(*f.block)?).into(),
    })
}
pub fn parse_trait_item(f: syn::TraitItem) -> Result<Item> {
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

fn parse_call(call: syn::ExprCall) -> Result<ExprInvoke> {
    let fun = parse_expr(*call.func)?;
    let args: Vec<_> = call.args.into_iter().map(parse_expr).try_collect()?;

    Ok(ExprInvoke {
        target: ExprInvokeTarget::expr(fun),
        args,
    })
}
fn parse_method_call(call: syn::ExprMethodCall) -> Result<ExprInvoke> {
    Ok(ExprInvoke {
        target: ExprInvokeTarget::Method(
            ExprSelect {
                obj: parse_expr(*call.receiver)?.into(),
                field: parse_ident(call.method),
                select: ExprSelectType::Method,
            }
            .into(),
        )
        .into(),
        args: call.args.into_iter().map(parse_expr).try_collect()?,
    })
}
fn parse_index(i: syn::ExprIndex) -> Result<ExprIndex> {
    Ok(ExprIndex {
        expr: parse_expr(*i.expr)?,
        index: parse_expr(*i.index)?,
    })
}

fn parse_if(i: syn::ExprIf) -> Result<ExprIf> {
    let cond = parse_expr(*i.cond)?;
    let then = parse_block(i.then_branch)?;
    let elze;
    if let Some((_, e)) = i.else_branch {
        elze = Some(parse_expr(*e)?);
    } else {
        elze = None;
    }
    Ok(ExprIf {
        cond,
        then: Expr::block(then).into(),
        elze,
    })
}
fn parse_loop(l: syn::ExprLoop) -> Result<ExprLoop> {
    Ok(ExprLoop {
        label: None, // TODO: label
        body: Expr::block(parse_block(l.body)?).into(),
    })
}
fn parse_binary(b: syn::ExprBinary) -> Result<Expr> {
    let lhs = parse_expr(*b.left)?;
    let rhs = parse_expr(*b.right)?;
    let (kind, _flatten) = match b.op {
        syn::BinOp::Add(_) => (BinOpKind::Add, true),
        syn::BinOp::Mul(_) => (BinOpKind::Mul, true),
        syn::BinOp::Sub(_) => (BinOpKind::Sub, false),
        syn::BinOp::Div(_) => (BinOpKind::Div, false),
        syn::BinOp::Gt(_) => (BinOpKind::Gt, false),
        syn::BinOp::Ge(_) => (BinOpKind::Ge, false),
        syn::BinOp::Le(_) => (BinOpKind::Le, false),
        syn::BinOp::Lt(_) => (BinOpKind::Lt, false),
        syn::BinOp::Eq(_) => (BinOpKind::Eq, false),
        syn::BinOp::Ne(_) => (BinOpKind::Ne, false),
        syn::BinOp::BitOr(_) => (BinOpKind::BitOr, true),
        syn::BinOp::BitAnd(_) => (BinOpKind::BitAnd, true),
        syn::BinOp::BitXor(_) => (BinOpKind::BitXor, true),
        syn::BinOp::Or(_) => (BinOpKind::Or, true),
        syn::BinOp::And(_) => (BinOpKind::And, true),
        _ => bail!("Op not supported {:?}", b.op),
    };

    Ok(ExprBinOp { kind, lhs, rhs }.into())
}
fn parse_tuple(t: syn::ExprTuple) -> Result<ValueTuple> {
    let mut values = vec![];
    for e in t.elems {
        let expr = parse_expr(e)?;
        let value = Value::expr(*expr);
        values.push(value);
    }

    Ok(ValueTuple { values })
}
fn parse_member(mem: syn::Member) -> Result<Ident> {
    Ok(match mem {
        syn::Member::Named(n) => parse_ident(n),
        syn::Member::Unnamed(_) => bail!("Does not support unnmaed field yet {:?}", mem),
    })
}
fn parse_field_value(fv: syn::FieldValue) -> Result<FieldValue> {
    Ok(FieldValue {
        name: parse_member(fv.member)?,
        value: Value::expr(parse_expr(fv.expr)?),
    })
}
pub fn parse_struct_expr(s: syn::ExprStruct) -> Result<ExprInitStruct> {
    Ok(ExprInitStruct {
        name: Expr::path(parse_path(s.path)?).into(),
        fields: s
            .fields
            .into_iter()
            .map(|x| parse_field_value(x))
            .try_collect()?,
    })
}
pub fn parse_literal(lit: syn::Lit) -> Result<Value> {
    Ok(match lit {
        Lit::Int(i) => Value::Int(ValueInt::new(i.base10_parse()?)),
        Lit::Float(i) => Value::Decimal(ValueDecimal::new(i.base10_parse()?)),
        Lit::Str(s) => Value::String(ValueString::new_ref(s.value())),
        Lit::Bool(b) => Value::Bool(ValueBool::new(b.value)),
        _ => bail!("Lit not supported: {:?}", lit.to_token_stream()),
    })
}
pub fn parse_unary(u: syn::ExprUnary) -> Result<ExprUnOp> {
    let expr = parse_expr(*u.expr)?;
    let op = match u.op {
        syn::UnOp::Neg(_) => UnOpKind::Neg,
        syn::UnOp::Not(_) => UnOpKind::Not,
        _ => bail!("Unary op not supported: {:?}", u.op),
    };
    Ok(ExprUnOp { op, val: expr })
}
pub fn parse_expr(expr: syn::Expr) -> Result<BExpr> {
    let expr = match expr {
        syn::Expr::Binary(b) => parse_binary(b)?,
        syn::Expr::Unary(u) => parse_unary(u)?.into(),
        syn::Expr::Block(b) if b.label.is_none() => Expr::block(parse_block(b.block)?),
        syn::Expr::Call(c) => Expr::Invoke(parse_call(c)?.into()),
        syn::Expr::If(i) => Expr::If(parse_if(i)?),
        syn::Expr::Loop(l) => Expr::Loop(parse_loop(l)?),
        syn::Expr::Lit(l) => Expr::value(parse_literal(l.lit)?),
        syn::Expr::Macro(m) => Expr::any(RawExprMacro { raw: m }),
        syn::Expr::MethodCall(c) => Expr::Invoke(parse_method_call(c)?.into()),
        syn::Expr::Index(i) => Expr::Index(parse_index(i)?),
        syn::Expr::Path(p) => Expr::path(parse_path(p.path)?),
        syn::Expr::Reference(r) => Expr::Reference(parse_expr_reference(r)?.into()),
        syn::Expr::Tuple(t) => Expr::value(Value::Tuple(parse_tuple(t)?)),
        syn::Expr::Struct(s) => Expr::InitStruct(parse_struct_expr(s)?.into()),

        raw => {
            warn!("RawExpr {:?}", raw);
            Expr::Any(AnyBox::new(RawExpr { raw }))
        } // x => bail!("Expr not supported: {:?}", x),
    };
    Ok(expr.into())
}

/// returns: statement, with_semicolon
fn parse_stmt(stmt: syn::Stmt) -> Result<(Statement, bool)> {
    Ok(match stmt {
        syn::Stmt::Local(l) => (
            Statement::Let(StatementLet {
                pat: parse_pat(l.pat)?,
                value: parse_expr(*l.init.context("No value")?.expr)?.into(),
            }),
            true,
        ),
        syn::Stmt::Item(tm) => (parse_item(tm).map(Statement::item)?, true),
        syn::Stmt::Expr(e, semicolon) => {
            (Statement::Expr(parse_expr(e)?.into()), semicolon.is_some())
        }
        syn::Stmt::Macro(raw) => (Statement::any(RawStmtMacro { raw }), true),
    })
}

fn parse_block(block: syn::Block) -> Result<ExprBlock> {
    // info!("Parsing block {:?}", block);
    let mut stmts = vec![];
    let mut last_with_semicolon = true;
    for stmt in block.stmts.into_iter() {
        let (stmt, with_semicolon) = parse_stmt(stmt)?;
        stmts.push(stmt);
        last_with_semicolon = with_semicolon;
    }
    let ret = if !last_with_semicolon {
        let expr = match stmts.pop().unwrap() {
            Statement::Expr(e) => e,
            x => bail!("Last statement should be expr, but got {:?}", x),
        };
        Some(expr.into())
    } else {
        None
    };
    Ok(ExprBlock { stmts, ret })
}
fn parse_vis(v: syn::Visibility) -> Visibility {
    match v {
        syn::Visibility::Public(_) => Visibility::Public,
        syn::Visibility::Restricted(_) => Visibility::Public,
        syn::Visibility::Inherited => Visibility::Private,
    }
}
fn parse_impl_item(item: syn::ImplItem) -> Result<Item> {
    match item {
        syn::ImplItem::Fn(m) => {
            // TODO: defaultness
            let expr = parse_fn(syn::ItemFn {
                attrs: m.attrs,
                vis: m.vis.clone(),
                sig: m.sig,
                block: Box::new(m.block),
            })?;
            Ok(Item::DefFunction(DefFunction {
                name: expr.name.clone().unwrap(),
                ty: None,
                value: expr,
                visibility: parse_vis(m.vis),
            }))
        }
        syn::ImplItem::Type(t) => Ok(Item::DefType(DefType {
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
        self_ty: Expr::value(parse_type_value(*im.self_ty.clone())?.into()),
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
fn parse_use(u: syn::ItemUse) -> Result<Import, RawUse> {
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
    Ok(Import {
        visibility: parse_vis(u.vis),
        path: Path::new(segments),
    })
}
pub fn parse_item_struct(s: syn::ItemStruct) -> Result<TypeStruct> {
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
impl Into<Type> for TypeValueParser {
    fn into(self) -> Type {
        match self {
            // TypeExprParser::Add(o) => TypeExpr::Op(TypeOp::Add(AddOp {
            //     lhs: o.lhs.into(),
            //     rhs: o.rhs.into(),
            // })),
            // TypeExprParser::Sub(o) => TypeExpr::Op(TypeOp::Sub(SubOp {
            //     lhs: o.lhs.into(),
            //     rhs: o.rhs.into(),
            // })),
            TypeValueParser::UnnamedStruct(s) => Type::Structural(s),
            TypeValueParser::NamedStruct(s) => Type::Struct(s),
            TypeValueParser::Path(p) => Type::path(p),
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
                    parse_item_struct(s).map_err(|err| input.error(err))?,
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
    Add { left: Expr, right: Expr },
    Value(Type),
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
                let rhs: Type = input.parse::<TypeValueParser>()?.into();
                lhs = TypeExprParser::Add {
                    left: lhs.into(),
                    right: Expr::value(rhs.into()),
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
impl Into<Expr> for TypeExprParser {
    fn into(self) -> Expr {
        match self {
            TypeExprParser::Add { left, right } => Expr::BinOp(ExprBinOp {
                lhs: left.into(),
                rhs: right.into(),
                kind: BinOpKind::Add,
            }),
            // TypeExprParser::Sub { .. } => {
            //     unreachable!()
            // }
            TypeExprParser::Value(v) => Expr::value(v.into()),
        }
    }
}
fn parse_custom_type_expr(m: syn::TypeMacro) -> Result<Expr> {
    let t: TypeExprParser = m.mac.parse_body().with_context(|| format!("{:?}", m))?;
    Ok(t.into())
}
fn parse_trait(t: syn::ItemTrait) -> Result<DefTrait> {
    // TODO: generis params
    let bounds = parse_type_param_bounds(t.supertraits.into_iter().collect())?;
    let vis = parse_vis(t.vis);
    Ok(DefTrait {
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
fn parse_item(item: syn::Item) -> Result<Item> {
    match item {
        syn::Item::Fn(f0) => {
            let visibility = parse_vis(f0.vis.clone());
            let f = parse_fn(f0)?;
            let d = DefFunction {
                name: f.name.clone().unwrap(),
                ty: None,
                value: f,
                visibility,
            };
            Ok(Item::DefFunction(d))
        }
        syn::Item::Impl(im) => Ok(Item::Impl(parse_impl(im)?)),
        syn::Item::Use(u) => Ok(match parse_use(u) {
            Ok(i) => Item::Import(i),
            Err(r) => Item::Any(AnyBox::new(r)),
        }),
        syn::Item::Macro(m) => Ok(Item::any(RawItemMacro { raw: m })),
        syn::Item::Struct(s) => {
            let visibility = parse_vis(s.vis.clone());

            let struct_type = parse_item_struct(s)?;
            Ok(Item::DefStruct(DefStruct {
                name: struct_type.name.clone(),
                value: struct_type,
                visibility,
            }))
        }
        syn::Item::Enum(e) => {
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
                            Type::any()
                        }
                    };
                    Ok(EnumTypeVariant { name, value: ty })
                })
                .try_collect()?;
            Ok(Item::DefEnum(DefEnum {
                name: ident.clone(),
                value: TypeEnum {
                    name: ident.clone(),
                    variants,
                },
                visibility,
            }))
        }
        syn::Item::Type(t) => {
            let visibility = parse_vis(t.vis.clone());
            let ty = parse_type_value(*t.ty)?;
            Ok(Item::DefType(DefType {
                name: parse_ident(t.ident),
                value: ty,
                visibility,
            }))
        }
        syn::Item::Mod(m) => Ok(Item::Module(parse_module(m)?)),
        syn::Item::Trait(t) => {
            let trait_ = parse_trait(t)?;
            Ok(Item::DefTrait(trait_))
        }
        _ => bail!("Does not support item yet: {:?}", item),
    }
}
fn parse_expr_reference(item: syn::ExprReference) -> Result<ExprReference> {
    Ok(ExprReference {
        referee: parse_expr(*item.expr)?.into(),
        mutable: Some(item.mutability.is_some()),
    })
}

pub fn parse_file(path: PathBuf, file: syn::File) -> Result<File> {
    let module = Module {
        name: Ident::new("__file__"),
        items: file.items.into_iter().map(parse_item).try_collect()?,
        visibility: Visibility::Public,
    };
    Ok(File { path, module })
}
pub fn parse_module(m: syn::ItemMod) -> Result<Module> {
    Ok(Module {
        name: parse_ident(m.ident),
        items: m
            .content
            .unwrap()
            .1
            .into_iter()
            .map(parse_item)
            .try_collect()?,
        visibility: parse_vis(m.vis),
    })
}
pub struct RustParser {}

impl RustParser {
    pub fn new() -> Self {
        RustParser {}
    }
    pub fn parse_file_recursively(&self, path: PathBuf) -> Result<File> {
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
        parse_expr(code).map(|x| Value::expr(x.get()))
    }
    pub fn parse_expr(&self, code: syn::Expr) -> Result<Expr> {
        parse_expr(code).map(|x| x.get())
    }
    pub fn parse_item(&self, code: syn::Item) -> Result<Item> {
        parse_item(code)
    }
    pub fn parse_file(&self, path: PathBuf, code: syn::File) -> Result<File> {
        parse_file(path, code)
    }
    pub fn parse_module(&self, code: syn::ItemMod) -> Result<Module> {
        parse_module(code)
    }
    pub fn parse_type_value(&self, code: syn::Type) -> Result<Type> {
        parse_type_value(code)
    }
}
