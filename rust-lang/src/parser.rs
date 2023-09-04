use crate::{RawExpr, RawExprMacro, RawItemMacro, RawStmtMacro, RawUse};
use common::*;
use common_lang::ast::*;
use common_lang::ops::{AddOp, BinOpKind, SubOp, UnOpKind};
use common_lang::value::*;
use quote::ToTokens;
use std::path::PathBuf;
use syn::parse::ParseStream;
use syn::{parse_quote, FieldsNamed, FnArg, Lit, ReturnType, Token};
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
                                parse_expr(c).map(|x| TypeValue::value(Value::expr(x)))
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

fn parse_type_value(t: syn::Type) -> Result<TypeValue> {
    let t = match t {
        syn::Type::BareFn(f) => TypeValue::Function(FunctionType {
            params: f
                .inputs
                .into_iter()
                .map(|x| x.ty)
                .map(parse_type_value)
                .try_collect()?,
            generics_params: vec![],
            ret: parse_return_type(f.output)?.into(),
        })
        .into(),
        syn::Type::Path(p) => {
            let s = p.path.to_token_stream().to_string();
            fn int(ty: IntType) -> TypeValue {
                TypeValue::Primitive(PrimitiveType::Int(ty))
            }
            fn float(ty: DecimalType) -> TypeValue {
                TypeValue::Primitive(PrimitiveType::Decimal(ty))
            }

            match s.as_str() {
                "i64" => int(IntType::I64),
                "i32" => int(IntType::I32),
                "i16" => int(IntType::I16),
                "i8" => int(IntType::I8),
                "u64" => int(IntType::U64),
                "u32" => int(IntType::U32),
                "u16" => int(IntType::U16),
                "u8" => int(IntType::U8),
                "f64" => float(DecimalType::F64),
                "f32" => float(DecimalType::F32),
                _ => TypeValue::locator(parse_locator(p.path)?),
            }
        }
        syn::Type::ImplTrait(im) => TypeValue::ImplTraits(parse_impl_trait(im)?),
        syn::Type::Tuple(t) if t.elems.is_empty() => TypeValue::unit().into(),
        // types like t!{ }
        syn::Type::Macro(m) if m.mac.path == parse_quote!(t) => {
            TypeValue::expr(parse_custom_type_expr(m)?)
        }
        syn::Type::Reference(r) => TypeValue::Reference(parse_type_reference(r)?),
        t => bail!("Type not supported {:?}", t),
    };
    Ok(t)
}
fn parse_type_reference(r: syn::TypeReference) -> Result<ReferenceType> {
    Ok(ReferenceType {
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
                TypeValue::expr(TypeExpr::SelfType(SelfType {
                    reference: rev.reference.is_some(),
                    mutability: rev.mutability.is_some(),
                }))
            },
        },

        FnArg::Typed(t) => FunctionParam {
            name: parse_pat(*t.pat)?,
            ty: parse_type_value(*t.ty)?,
        },
    })
}

fn parse_pat(p: syn::Pat) -> Result<Ident> {
    Ok(match p {
        syn::Pat::Ident(name) => parse_ident(name.ident),
        syn::Pat::Wild(_) => Ident::new("_"),
        _ => bail!("Pattern not supported {:?}", p),
    })
}

fn parse_return_type(o: ReturnType) -> Result<TypeValue> {
    Ok(match o {
        ReturnType::Default => TypeValue::unit(),
        ReturnType::Type(_, t) => parse_type_value(*t)?,
    })
}
fn parse_type_param_bound(b: syn::TypeParamBound) -> Result<TypeExpr> {
    match b {
        syn::TypeParamBound::Trait(t) => {
            let path = parse_path(t.path)?;
            Ok(TypeExpr::path(path))
        }
        _ => bail!("Does not support liftimes {:?}", b),
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
fn parse_fn(f: syn::ItemFn) -> Result<FunctionValue> {
    let sig = parse_fn_sig(f.sig)?;
    Ok(FunctionValue {
        sig,
        body: Expr::block(parse_block(*f.block)?).into(),
    })
}
pub fn parse_trait_item(f: syn::TraitItem) -> Result<Declare> {
    match f {
        syn::TraitItem::Fn(f) => {
            let name = parse_ident(f.sig.ident.clone());
            Ok(Declare {
                name,
                kind: DeclareKind::Function {
                    sig: parse_fn_sig(f.sig)?,
                },
            })
        }
        syn::TraitItem::Type(t) => {
            let name = parse_ident(t.ident);
            let bounds = parse_type_param_bounds(t.bounds.into_iter().collect())?;
            Ok(Declare {
                name,
                kind: DeclareKind::Type { bounds },
            })
        }
        syn::TraitItem::Const(c) => {
            let name = parse_ident(c.ident);
            let ty = parse_type_value(c.ty)?;
            Ok(Declare {
                name,
                kind: DeclareKind::Const { ty },
            })
        }
        _ => bail!("Does not support trait item {:?}", f),
    }
}

fn parse_call(call: syn::ExprCall) -> Result<Invoke> {
    let fun = parse_expr(*call.func)?;
    let args: Vec<_> = call.args.into_iter().map(parse_expr).try_collect()?;

    Ok(Invoke {
        func: fun.into(),
        args,
    })
}
fn parse_method_call(call: syn::ExprMethodCall) -> Result<Invoke> {
    Ok(Invoke {
        func: Expr::Select(Select {
            obj: parse_expr(*call.receiver)?.into(),
            field: parse_ident(call.method),
            select: SelectType::Method,
        })
        .into(),
        args: call.args.into_iter().map(parse_expr).try_collect()?,
    })
}

fn parse_if(i: syn::ExprIf) -> Result<Cond> {
    let mut cases = vec![CondCase {
        cond: parse_expr(*i.cond)?,
        body: Expr::block(parse_block(i.then_branch)?).into(),
    }];
    if let Some((_, else_body)) = i.else_branch {
        'else_check: {
            let body = parse_expr(*else_body)?;
            match &body {
                Expr::Cond(m) => {
                    if m.if_style {
                        cases.extend(m.cases.clone());
                        break 'else_check;
                    }
                }
                _ => {}
            }

            cases.push(CondCase {
                cond: Expr::value(Value::bool(true)),
                body,
            });
        };
    }

    Ok(Cond {
        cases,
        if_style: true,
    })
}
fn parse_binary(b: syn::ExprBinary) -> Result<Invoke> {
    let mut lhs = parse_expr(*b.left)?;
    let rhs = parse_expr(*b.right)?;
    let (op, flatten) = match b.op {
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
    if flatten {
        match &mut lhs {
            Expr::Invoke(first_arg) => match &*first_arg.func {
                Expr::Value(Value::BinOpKind(i)) if i == &op => {
                    first_arg.args.push(rhs);
                    return Ok(first_arg.clone());
                }
                _ => {}
            },
            _ => {}
        }
    }
    Ok(Invoke {
        func: Expr::value(Value::BinOpKind(op)).into(),
        args: vec![lhs, rhs],
    })
}
fn parse_tuple(t: syn::ExprTuple) -> Result<TupleValue> {
    let values = t
        .elems
        .into_iter()
        .map(parse_expr)
        .map(|x| x.map(Value::expr))
        .try_collect()?;
    Ok(TupleValue { values })
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
        value: Value::expr(parse_expr(fv.expr)?.into()),
    })
}
pub fn parse_struct_expr(s: syn::ExprStruct) -> Result<StructExpr> {
    Ok(StructExpr {
        name: TypeExpr::path(parse_path(s.path)?).into(),
        fields: s
            .fields
            .into_iter()
            .map(|x| parse_field_value(x))
            .try_collect()?,
    })
}
pub fn parse_literal(lit: syn::Lit) -> Result<Value> {
    Ok(match lit {
        Lit::Int(i) => Value::Int(IntValue::new(i.base10_parse()?)),
        Lit::Float(i) => Value::Decimal(DecimalValue::new(i.base10_parse()?)),
        Lit::Str(s) => Value::String(StringValue::new_ref(s.value())),
        Lit::Bool(b) => Value::Bool(BoolValue::new(b.value)),
        _ => bail!("Lit not supported: {:?}", lit.to_token_stream()),
    })
}
pub fn parse_unary(u: syn::ExprUnary) -> Result<Invoke> {
    let expr = parse_expr(*u.expr)?;
    let op = match u.op {
        syn::UnOp::Neg(_) => UnOpKind::Neg,
        syn::UnOp::Not(_) => UnOpKind::Not,
        _ => bail!("Unary op not supported: {:?}", u.op),
    };
    Ok(Invoke {
        func: Expr::value(Value::UnOpKind(op)).into(),
        args: vec![expr],
    })
}
pub fn parse_expr(expr: syn::Expr) -> Result<Expr> {
    Ok(match expr {
        syn::Expr::Binary(b) => Expr::Invoke(parse_binary(b)?),
        syn::Expr::Unary(u) => Expr::Invoke(parse_unary(u)?),
        syn::Expr::Block(b) if b.label.is_none() => Expr::Block(parse_block(b.block)?),
        syn::Expr::Call(c) => Expr::Invoke(parse_call(c)?),
        syn::Expr::If(i) => Expr::Cond(parse_if(i)?),
        syn::Expr::Lit(l) => Expr::value(parse_literal(l.lit)?),
        syn::Expr::Macro(m) => Expr::any(RawExprMacro { raw: m }),
        syn::Expr::MethodCall(c) => Expr::Invoke(parse_method_call(c)?),
        syn::Expr::Path(p) => Expr::path(parse_path(p.path)?),
        syn::Expr::Reference(r) => Expr::Reference(parse_expr_reference(r)?),
        syn::Expr::Tuple(t) => Expr::value(Value::Tuple(parse_tuple(t)?)),
        syn::Expr::Struct(s) => Expr::Struct(parse_struct_expr(s)?),

        raw => {
            warn!("RawExpr {:?}", raw);
            Expr::Any(AnyBox::new(RawExpr { raw }))
        } // x => bail!("Expr not supported: {:?}", x),
    })
}

fn parse_stmt(stmt: syn::Stmt) -> Result<Statement> {
    Ok(match stmt {
        syn::Stmt::Local(l) => Statement::Let(Let {
            mutability: match &l.pat {
                syn::Pat::Ident(i) => Some(i.mutability.is_some()),
                _ => None,
            },
            name: parse_pat(l.pat)?,
            ty: None,
            value: parse_expr(*l.init.context("No value")?.expr)?,
        }),
        syn::Stmt::Item(tm) => parse_item(tm).map(Statement::item)?,
        syn::Stmt::Expr(e, semicolon) => {
            Statement::maybe_stmt_expr(parse_expr(e)?, semicolon.is_some())
        }
        syn::Stmt::Macro(raw) => Statement::any(RawStmtMacro { raw }),
    })
}

fn parse_block(block: syn::Block) -> Result<Block> {
    // info!("Parsing block {:?}", block);
    let last_value = block
        .stmts
        .last()
        .map(|x| match x {
            syn::Stmt::Expr(_, s) => s.is_none(),
            _ => false,
        })
        .unwrap_or_default();
    let mut stmts: Vec<_> = block.stmts.into_iter().map(parse_stmt).try_collect()?;
    if last_value {
        if let Some(last) = stmts.last_mut() {
            last.try_make_expr()
        }
    }
    Ok(Block { stmts })
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
            Ok(Item::Define(Define {
                name: expr.name.clone().unwrap(),
                kind: DefineKind::Function,
                ty: None,
                value: DefineValue::Function(expr),
                visibility: parse_vis(m.vis),
            }))
        }
        syn::ImplItem::Type(t) => Ok(Item::Define(Define {
            name: parse_ident(t.ident),
            kind: DefineKind::Type,
            ty: None,
            value: DefineValue::Type(TypeExpr::value(parse_type_value(t.ty)?)),
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
        self_ty: TypeExpr::value(parse_type_value(*im.self_ty.clone())?),
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
pub fn parse_item_struct(s: syn::ItemStruct) -> Result<StructType> {
    Ok(StructType {
        name: parse_ident(s.ident),
        fields: s
            .fields
            .into_iter()
            .enumerate()
            .map(|(i, f)| parse_struct_field(i, f))
            .try_collect()?,
    })
}
struct UnnamedStructTypeParser(StructuralType);
impl syn::parse::Parse for UnnamedStructTypeParser {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![struct]>()?;

        let fields: FieldsNamed = input.parse()?;

        Ok(UnnamedStructTypeParser(StructuralType {
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
    UnnamedStruct(StructuralType),
    NamedStruct(StructType),
    Path(Path),
    // Ident(Ident),
}
impl Into<TypeValue> for TypeValueParser {
    fn into(self) -> TypeValue {
        match self {
            // TypeExprParser::Add(o) => TypeExpr::Op(TypeOp::Add(AddOp {
            //     lhs: o.lhs.into(),
            //     rhs: o.rhs.into(),
            // })),
            // TypeExprParser::Sub(o) => TypeExpr::Op(TypeOp::Sub(SubOp {
            //     lhs: o.lhs.into(),
            //     rhs: o.rhs.into(),
            // })),
            TypeValueParser::UnnamedStruct(s) => TypeValue::Structural(s),
            TypeValueParser::NamedStruct(s) => TypeValue::Struct(s),
            TypeValueParser::Path(p) => TypeValue::path(p),
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
    Add(AddOp<TypeExpr>),
    Sub(SubOp<TypeExpr>),
    Value(TypeValue),
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
                let rhs = input.parse::<TypeValueParser>()?.into();
                lhs = TypeExprParser::Add(AddOp {
                    lhs: Box::new(lhs.into()),
                    rhs: TypeExpr::value(rhs).into(),
                });
            } else if input.peek(Token![-]) {
                input.parse::<Token![-]>()?;
                let rhs = input.parse::<TypeValueParser>()?.into();
                lhs = TypeExprParser::Sub(SubOp {
                    lhs: Box::new(lhs.into()),
                    rhs: TypeExpr::value(rhs).into(),
                });
            } else {
                return Err(input.error("Expected + or -"));
            }
        }
        Ok(lhs)
    }
}
impl Into<TypeExpr> for TypeExprParser {
    fn into(self) -> TypeExpr {
        match self {
            TypeExprParser::Add(o) => TypeExpr::BinOp(TypeBinOp::Add(o)),
            TypeExprParser::Sub(o) => TypeExpr::BinOp(TypeBinOp::Sub(o)),
            TypeExprParser::Value(v) => TypeExpr::value(v),
        }
    }
}
fn parse_custom_type_expr(m: syn::TypeMacro) -> Result<TypeExpr> {
    let t: TypeExprParser = m.mac.parse_body().with_context(|| format!("{:?}", m))?;
    Ok(t.into())
}
fn parse_trait(t: syn::ItemTrait) -> Result<Trait> {
    // TODO: generis params
    let bounds = parse_type_param_bounds(t.supertraits.into_iter().collect())?;
    Ok(Trait {
        name: parse_ident(t.ident),
        bounds,
        items: t
            .items
            .into_iter()
            .map(|x| parse_trait_item(x).map(Item::Declare))
            .try_collect()?,
    })
}
fn parse_item(item: syn::Item) -> Result<Item> {
    match item {
        syn::Item::Fn(f0) => {
            let visibility = parse_vis(f0.vis.clone());
            let f = parse_fn(f0)?;
            let d = Define {
                name: f.name.clone().unwrap(),
                kind: DefineKind::Function,
                ty: None,
                value: DefineValue::Function(f),
                visibility,
            };
            Ok(Item::Define(d))
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
            Ok(Item::Define(Define {
                name: struct_type.name.clone(),
                kind: DefineKind::Type,
                ty: None,
                value: DefineValue::Type(TypeExpr::value(TypeValue::Struct(struct_type))),
                visibility,
            }))
        }
        syn::Item::Type(t) => {
            let visibility = parse_vis(t.vis.clone());
            let ty = parse_type_value(*t.ty)?;
            Ok(Item::Define(Define {
                name: parse_ident(t.ident),
                kind: DefineKind::Type,
                ty: None,
                value: DefineValue::Type(TypeExpr::value(ty)),
                visibility,
            }))
        }
        syn::Item::Mod(m) => Ok(Item::Module(parse_module(m)?)),
        syn::Item::Trait(t) => {
            let visibility = parse_vis(t.vis.clone());
            let trait_ = parse_trait(t)?;
            Ok(Item::Define(Define {
                name: trait_.name.clone(),
                kind: DefineKind::Trait,
                ty: None,
                value: DefineValue::Trait(trait_),
                visibility,
            }))
        }
        _ => bail!("Does not support item yet: {:?}", item),
    }
}
fn parse_expr_reference(item: syn::ExprReference) -> Result<Reference> {
    Ok(Reference {
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
        let module = builder.parse_and_inline_modules(&path)?;
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
    pub fn parse_expr(&self, code: syn::Expr) -> Result<Expr> {
        parse_expr(code)
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
    pub fn parse_type_value(&self, code: syn::Type) -> Result<TypeValue> {
        parse_type_value(code)
    }
}
