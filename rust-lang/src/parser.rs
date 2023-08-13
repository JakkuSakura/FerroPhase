use crate::{RawExpr, RawExprMacro, RawItemMacro, RawUse};
use common::*;
use common_lang::tree::*;
use common_lang::value::*;
use quote::ToTokens;
use syn::{BinOp, FnArg, GenericParam, Lit, Pat, ReturnType, TypeParamBound};

fn parse_ident(i: syn::Ident) -> Ident {
    Ident::new(i.to_string())
}
fn parse_type(t: syn::Type) -> Result<TypeExpr> {
    let t = match t {
        syn::Type::BareFn(f) => TypeExpr::value(TypeValue::Function(FunctionType {
            params: f
                .inputs
                .into_iter()
                .map(|x| x.ty)
                .map(parse_type)
                .map(|x| x.map(Box::new).map(TypeValue::Expr))
                .try_collect()?,
            ret: TypeValue::expr(parse_return_type(f.output)?.into()).into(),
        }))
        .into(),
        syn::Type::Path(p) => {
            let s = p.path.to_token_stream().to_string();

            match s.as_str() {
                "i64" | "i32" | "u64" | "u32" | "f64" | "f32" => TypeExpr::Ident(Ident::new(s)),
                _ if p.path.segments.len() == 1 => {
                    let first_segment = p.path.segments.first().unwrap();
                    match &first_segment.arguments {
                        syn::PathArguments::None => {
                            let ident = parse_ident(first_segment.ident.clone());
                            TypeExpr::Ident(ident)
                        }
                        // _ => TypeExpr::AnyTypeExpr(RawType { raw: p }.into()),
                        _ => bail!("Does not support path arguments {:?}", p),
                    }
                }
                _ => bail!("Type not supported: {}", s),
            }
        }
        // syn::Type::ImplTrait(im) => TypeExpr::AnyTypeExpr(RawImplTrait { raw: im }.into()),
        syn::Type::ImplTrait(im) => bail!("Does not support impl trait {:?}", im),
        syn::Type::Tuple(t) if t.elems.is_empty() => TypeExpr::unit().into(),
        t => bail!("Type not supported {:?}", t),
    };
    Ok(t)
}

fn parse_input(i: FnArg) -> Result<FuncDeclParam> {
    Ok(match i {
        FnArg::Receiver(rev) => FuncDeclParam {
            name: Ident::new("self"),
            ty: {
                let ident = match (rev.reference.is_some(), rev.mutability.is_some()) {
                    (true, true) => Ident::new("&mut Self").into(),
                    (true, false) => Ident::new("&Self").into(),
                    (false, true) => Ident::new("Self").into(),
                    (false, false) => Ident::new("mut Self").into(),
                };
                TypeExpr::Ident(ident)
            },
        },

        FnArg::Typed(t) => FuncDeclParam {
            name: parse_pat(*t.pat)?,
            ty: parse_type(*t.ty)?,
        },
    })
}

fn parse_pat(p: syn::Pat) -> Result<Ident> {
    Ok(match p {
        Pat::Ident(name) => parse_ident(name.ident),
        Pat::Wild(_) => Ident::new("_"),
        _ => bail!("Pattern not supported {:?}", p),
    })
}

fn parse_return_type(o: ReturnType) -> Result<TypeExpr> {
    Ok(match o {
        ReturnType::Default => TypeExpr::unit(),
        ReturnType::Type(_, t) => parse_type(*t)?,
    })
}
fn parse_type_param_bound(b: TypeParamBound) -> Result<Path> {
    match b {
        TypeParamBound::Trait(t) => parse_path(t.path),
        _ => bail!("Does not support liftimes {:?}", b),
    }
}

fn parse_path(p: syn::Path) -> Result<Path> {
    Ok(Path {
        segments: p
            .segments
            .into_iter()
            .map(|x| {
                let ident = parse_ident(x.ident);
                ensure!(x.arguments.is_none(), "Does not support path arguments");
                Ok(ident)
            })
            .try_collect()?,
    })
}
fn parse_fn(f: syn::ItemFn) -> Result<FuncDecl> {
    let name = parse_ident(f.sig.ident);
    let generics_params = f
        .sig
        .generics
        .params
        .into_iter()
        .map(|x| match x {
            GenericParam::Type(t) => Ok(FuncDeclParam {
                name: parse_ident(t.ident),
                // TODO: support multiple type bounds
                ty: TypeExpr::Path(parse_type_param_bound(t.bounds.first().cloned().unwrap())?),
            }),
            _ => bail!("Does not generic param {:?}", x),
        })
        .try_collect()?;
    Ok(FuncDecl {
        name: name.clone(),
        params: f
            .sig
            .inputs
            .into_iter()
            .map(|x| parse_input(x))
            .try_collect()?,
        generics_params,
        ret: parse_return_type(f.sig.output)?.into(),
        body: parse_block(*f.block)?,
    })
}

fn parse_call(call: syn::ExprCall) -> Result<InvokeExpr> {
    let fun = parse_expr(*call.func)?;
    let args: Vec<_> = call.args.into_iter().map(parse_expr).try_collect()?;

    Ok(InvokeExpr {
        fun: fun.into(),
        args,
    })
}
fn parse_method_call(call: syn::ExprMethodCall) -> Result<InvokeExpr> {
    Ok(InvokeExpr {
        fun: Expr::Select(Select {
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
        body: Expr::Block(parse_block(i.then_branch)?).into(),
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
fn parse_binary(b: syn::ExprBinary) -> Result<InvokeExpr> {
    let mut lhs = parse_expr(*b.left)?;
    let rhs = parse_expr(*b.right)?;
    let (op, flatten) = match b.op {
        BinOp::Add(_) => (Ident::new("+"), true),
        BinOp::Mul(_) => (Ident::new("*"), true),
        BinOp::Sub(_) => (Ident::new("-"), false),
        BinOp::Gt(_) => (Ident::new(">"), false),
        BinOp::Ge(_) => (Ident::new(">="), false),
        BinOp::Le(_) => (Ident::new("<="), false),
        BinOp::Lt(_) => (Ident::new("<"), false),
        BinOp::Eq(_) => (Ident::new("=="), false),
        BinOp::Ne(_) => (Ident::new("!="), false),
        BinOp::BitOr(_) => (Ident::new("|"), true),
        _ => bail!("Op not supported {:?}", b.op),
    };
    if flatten {
        match &mut lhs {
            Expr::Invoke(first_arg) => match &*first_arg.fun {
                Expr::Ident(i) if i == &op => {
                    first_arg.args.push(rhs);
                    return Ok(first_arg.clone());
                }
                _ => {}
            },
            _ => {}
        }
    }
    Ok(InvokeExpr {
        fun: Expr::Ident(op).into(),
        args: vec![lhs, rhs],
    })
}
fn parse_tuple(t: syn::ExprTuple) -> Result<TupleValue> {
    let values = t
        .elems
        .into_iter()
        .map(parse_expr)
        .map(|x| x.map(Box::new).map(Value::Expr))
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
        value: Value::Expr(parse_expr(fv.expr)?.into()),
    })
}
pub fn parse_struct_expr(s: syn::ExprStruct) -> Result<StructValue> {
    Ok(StructValue {
        name: TypeExpr::Path(parse_path(s.path)?),
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
pub fn parse_expr(expr: syn::Expr) -> Result<Expr> {
    Ok(match expr {
        syn::Expr::Binary(b) => Expr::Invoke(parse_binary(b)?),
        syn::Expr::Block(b) if b.label.is_none() => Expr::Block(parse_block(b.block)?),

        syn::Expr::Call(c) => Expr::Invoke(parse_call(c)?),
        syn::Expr::If(i) => Expr::Cond(parse_if(i)?),

        syn::Expr::Lit(l) => Expr::value(parse_literal(l.lit)?),
        syn::Expr::Macro(m) => Expr::any(RawExprMacro { raw: m }),
        syn::Expr::MethodCall(c) => Expr::Invoke(parse_method_call(c)?),
        syn::Expr::Path(p) => Expr::Path(parse_path(p.path)?),
        syn::Expr::Reference(r) => Expr::Reference(parse_ref(r)?),
        syn::Expr::Tuple(t) => Expr::value(Value::Tuple(parse_tuple(t)?)),
        syn::Expr::Struct(s) => Expr::value(Value::Struct(parse_struct_expr(s)?)),

        raw => {
            warn!("RawExpr {:?}", raw);
            Expr::Any(AnyBox::new(RawExpr { raw }))
        } // x => bail!("Expr not supported: {:?}", x),
    })
}

fn parse_stmt(stmt: syn::Stmt) -> Result<Item> {
    Ok(match stmt {
        syn::Stmt::Local(l) => Item::Def(Define {
            name: parse_pat(l.pat)?,
            kind: DefKind::Variable,
            ty: None,
            value: DefValue::Variable(parse_expr(*l.init.context("No value")?.expr)?),
            visibility: Visibility::Public,
        }),
        syn::Stmt::Item(tm) => parse_item(tm)?,
        syn::Stmt::Expr(e, _) => Item::Expr(parse_expr(e)?),
        syn::Stmt::Macro(m) => bail!("Macro not supported: {:?}", m),
    })
}

fn parse_block(block: syn::Block) -> Result<Block> {
    info!("Parsing block {:?}", block);
    let last_value = block
        .stmts
        .last()
        .map(|x| match x {
            syn::Stmt::Expr(_, s) => s.is_none(),
            _ => false,
        })
        .unwrap_or_default();
    Ok(Block {
        stmts: block.stmts.into_iter().map(parse_stmt).try_collect()?,
        last_value,
    })
}
fn parse_vis(v: syn::Visibility) -> Visibility {
    match v {
        syn::Visibility::Public(_) => Visibility::Public,
        syn::Visibility::Restricted(_) => Visibility::Public,
        syn::Visibility::Inherited => Visibility::Private,
    }
}
fn parse_impl_item(item: syn::ImplItem) -> Result<Define> {
    match item {
        syn::ImplItem::Fn(m) => {
            // TODO: defaultness
            let expr = parse_fn(syn::ItemFn {
                attrs: m.attrs,
                vis: m.vis.clone(),
                sig: m.sig,
                block: Box::new(m.block),
            })?;
            Ok(Define {
                name: expr.name.clone(),
                kind: DefKind::Function,
                ty: None,
                value: DefValue::Function(expr),
                visibility: parse_vis(m.vis),
            })
        }
        syn::ImplItem::Type(t) => Ok(Define {
            name: parse_ident(t.ident),
            kind: DefKind::Type,
            ty: None,
            value: DefValue::Type(parse_type(t.ty)?),
            visibility: parse_vis(t.vis),
        }),
        _ => bail!("Does not support impl item {:?}", item),
    }
}
fn parse_impl(im: syn::ItemImpl) -> Result<Impl> {
    Ok(Impl {
        name: match parse_type(*im.self_ty.clone())? {
            TypeExpr::Ident(i) => i,
            _ => bail!("Does not support impl for {:?}", im.self_ty),
        },
        defs: im.items.into_iter().map(parse_impl_item).try_collect()?,
    })
}
fn parse_struct_field(i: usize, f: syn::Field) -> Result<FieldTypeValue> {
    Ok(FieldTypeValue {
        name: f
            .ident
            .map(parse_ident)
            .unwrap_or(Ident::new(format!("{}", i))),

        value: TypeValue::Expr(parse_type(f.ty)?.into()),
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
        segments,
    })
}
pub fn parse_item_struct(s: syn::ItemStruct) -> Result<NamedStructType> {
    Ok(NamedStructType {
        name: parse_ident(s.ident),
        fields: s
            .fields
            .into_iter()
            .enumerate()
            .map(|(i, f)| parse_struct_field(i, f))
            .try_collect()?,
    })
}
fn parse_item(item: syn::Item) -> Result<Item> {
    match item {
        syn::Item::Fn(f0) => {
            let visibility = parse_vis(f0.vis.clone());
            let f = parse_fn(f0)?;
            let d = Define {
                name: f.name.clone(),
                kind: DefKind::Function,
                ty: None,
                value: DefValue::Function(f),
                visibility,
            };
            Ok(Item::Def(d))
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
            Ok(Item::Def(Define {
                name: struct_type.name.clone(),
                kind: DefKind::Type,
                ty: None,
                value: DefValue::Type(TypeExpr::value(TypeValue::NamedStruct(struct_type))),
                visibility,
            }))
        }
        _ => bail!("Does not support item {:?} yet", item),
    }
}
fn parse_ref(item: syn::ExprReference) -> Result<Reference> {
    Ok(Reference {
        referee: parse_expr(*item.expr)?.into(),
        mutable: Some(item.mutability.is_some()),
    })
}

pub fn parse_file(file: syn::File) -> Result<Module> {
    Ok(Module {
        name: Ident::new("__file__"),
        items: file.items.into_iter().map(parse_item).try_collect()?,
    })
}
pub fn parse_module(file: syn::ItemMod) -> Result<Module> {
    Ok(Module {
        name: parse_ident(file.ident),
        items: file
            .content
            .unwrap()
            .1
            .into_iter()
            .map(parse_item)
            .try_collect()?,
    })
}
pub struct RustParser;

impl RustParser {
    pub fn parse_expr(&self, code: syn::Expr) -> Result<Expr> {
        parse_expr(code)
    }
    pub fn parse_file(&self, code: syn::File) -> Result<Module> {
        parse_file(code)
    }
    pub fn parse_module(&self, code: syn::ItemMod) -> Result<Module> {
        parse_module(code)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::RustSerde;
    use common_lang::tree::FuncDecl;
    use common_lang::Serializer;
    use quote::format_ident;
    use syn::parse_quote;

    #[test]
    fn test_parse_fn() {
        let code: syn::ItemFn = parse_quote!(
            fn foo(a: i32) -> i32 {
                a + 1
            }
        );
        let (name, expr) = super::parse_fn(code).unwrap();
        assert_eq!(name, super::parse_ident(format_ident!("foo")));

        assert_eq!(
            RustSerde
                .serialize_tree(
                    &expr
                        .as_ast::<FuncDecl>()
                        .unwrap()
                        .body
                        .clone()
                        .unwrap()
                        .stmts[0]
                )
                .unwrap(),
            RustSerde
                .serialize_expr(&parse_expr(parse_quote!(a + 1)).unwrap())
                .unwrap()
        );
    }
}
