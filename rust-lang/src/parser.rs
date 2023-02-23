use crate::{RawImplTrait, RawMacro, RawUse, RustSerde};
use common::*;
use common_lang::*;
use quote::ToTokens;
use syn::{
    BinOp, FnArg, GenericParam, Item, ItemFn, Lit, Pat, ReturnType, Stmt, Type, TypeParamBound,
};
fn parse_ident(i: syn::Ident) -> Ident {
    Ident::new(i.to_string())
}
fn parse_type(t: syn::Type) -> Result<Expr> {
    let t = match t {
        Type::BareFn(f) => Types::func(
            f.inputs
                .into_iter()
                .map(|x| x.ty)
                .map(parse_type)
                .try_collect()?,
            parse_return_type(f.output)?,
        )
        .into(),
        Type::Path(p) => {
            let s = p.path.to_token_stream().to_string();
            match s.as_str() {
                "i64" | "i32" | "u64" | "u32" | "f64" | "f32" => Ident::new(s).into(),
                x => Ident::new(x).into(),
                // _ => bail!("Type not supported: {}", s),
            }
        }
        Type::ImplTrait(im) => RawImplTrait { raw: im }.into(),
        t => bail!("Type not supported {:?}", t),
    };
    Ok(t)
}

fn parse_input(i: FnArg) -> Result<Param> {
    Ok(match i {
        FnArg::Receiver(_) => {
            todo!()
        }
        FnArg::Typed(t) => Param {
            name: parse_pat(*t.pat)?,
            ty: parse_type(*t.ty)?,
        },
    })
}

fn parse_pat(p: syn::Pat) -> Result<Ident> {
    Ok(match p {
        Pat::Ident(name) => parse_ident(name.ident),
        _ => todo!(),
    })
}

fn parse_return_type(o: ReturnType) -> Result<Expr> {
    Ok(match o {
        ReturnType::Default => Unit.into(),
        ReturnType::Type(_, t) => parse_type(*t)?,
    })
}
fn parse_type_param_bound(b: TypeParamBound) -> Result<Expr> {
    match b {
        TypeParamBound::Trait(t) => parse_path(t.path).map(|x| x.into()),
        _ => bail!("Does not support liftimes {:?}", b),
    }
}
fn parse_path(p: syn::Path) -> Result<Ident> {
    Ok(parse_ident(p.segments.first().unwrap().ident.clone()))
}
fn parse_fn(f: ItemFn) -> Result<(Ident, Expr)> {
    let name = parse_ident(f.sig.ident);
    let ff = FuncDecl {
        name: Some(name.clone()),
        params: Params {
            params: f
                .sig
                .inputs
                .into_iter()
                .map(|x| parse_input(x))
                .try_collect()?,
        },
        ret: parse_return_type(f.sig.output)?,
        body: Some(parse_block(*f.block)?),
    };
    if f.sig.generics.params.is_empty() {
        Ok((name, ff.into()))
    } else {
        let params = f
            .sig
            .generics
            .params
            .into_iter()
            .map(|x| match x {
                GenericParam::Type(t) => Ok(Param {
                    name: parse_ident(t.ident),
                    // TODO: support multiple type bounds
                    ty: parse_type_param_bound(t.bounds.first().cloned().unwrap())?,
                }),
                _ => bail!("Does not generic param {:?}", x),
            })
            .try_collect()?;
        Ok((
            name,
            Generics {
                params: Params { params },
                value: ff.into(),
            }
            .into(),
        ))
    }
}

fn parse_call(call: syn::ExprCall) -> Result<Call> {
    let fun = parse_expr(*call.func)?;
    let args: Vec<_> = call.args.into_iter().map(parse_expr).try_collect()?;

    Ok(Call {
        fun,
        args: PosArgs { args },
    })
}
fn parse_method_call(call: syn::ExprMethodCall) -> Result<Call> {
    Ok(Call {
        fun: Select {
            obj: parse_expr(*call.receiver)?,
            field: parse_ident(call.method),
            select: SelectType::Method,
        }
        .into(),
        args: PosArgs {
            args: call.args.into_iter().map(parse_expr).try_collect()?,
        },
    })
}

fn parse_if(i: syn::ExprIf) -> Result<Cond> {
    let mut cases = vec![CondCase {
        cond: parse_expr(*i.cond)?,
        body: parse_block(i.then_branch)?.into(),
    }];
    if let Some((_, else_body)) = i.else_branch {
        'else_check: {
            let body = parse_expr(*else_body)?;
            if let Some(m) = body.as_ast::<Cond>() {
                if m.if_style {
                    cases.extend(m.cases.clone());
                    break 'else_check;
                }
            }
            cases.push(CondCase {
                cond: LiteralBool { value: true }.into(),
                body,
            });
        };
    }

    Ok(Cond {
        cases,
        if_style: true,
    })
}
fn parse_binary(b: syn::ExprBinary) -> Result<Call> {
    let l = parse_expr(*b.left)?;
    let r = parse_expr(*b.right)?;
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
        if let Some(first_arg) = l.as_ast::<Call>() {
            if Some(&op) == first_arg.fun.as_ast::<Ident>() {
                let mut first_arg = first_arg.clone();
                first_arg.args.args.push(r);
                return Ok(first_arg);
            }
        }
    }
    Ok(Call {
        fun: op.into(),
        args: PosArgs { args: vec![l, r] },
    })
}
fn parse_expr(expr: syn::Expr) -> Result<Expr> {
    Ok(match expr {
        // Expr::Array(_) => {}
        // Expr::Assign(_) => {}
        // Expr::AssignOp(_) => {}
        // Expr::Async(_) => {}
        // Expr::Await(_) => {}
        syn::Expr::Binary(b) => parse_binary(b)?.into(),
        syn::Expr::Block(b) if b.label.is_none() => parse_block(b.block)?.into(),
        // Expr::Box(_) => {}
        // Expr::Break(_) => {}
        syn::Expr::Call(c) => parse_call(c)?.into(),
        // Expr::Cast(_) => {}
        // Expr::Closure(_) => {}
        // Expr::Continue(_) => {}
        // Expr::Field(_) => {}
        // Expr::ForLoop(_) => {}
        // Expr::Group(_) => {}
        syn::Expr::If(i) => parse_if(i)?.into(),
        // Expr::Index(_) => {}
        // Expr::Let(_) => {}
        syn::Expr::Lit(l) => match l.lit {
            Lit::Int(i) => LiteralInt::new(i.base10_parse()?).into(),
            Lit::Float(i) => LiteralDecimal::new(i.base10_parse()?).into(),
            _ => bail!("Lit not supported: {:?}", l.lit.to_token_stream()),
        },
        // Expr::Loop(_) => {}
        syn::Expr::Macro(m) => RawMacro { raw: m }.into(),
        // Expr::Match(_) => {}
        syn::Expr::MethodCall(c) => parse_method_call(c)?.into(),
        // Expr::Paren(_) => {}
        syn::Expr::Path(p) => parse_path(p.path)?.into(),
        // Expr::Range(_) => {}
        // Expr::Reference(_) => {}
        // Expr::Repeat(_) => {}
        // Expr::Return(_) => {}
        // Expr::Struct(_) => {}
        // Expr::Try(_) => {}
        // Expr::TryBlock(_) => {}
        // Expr::Tuple(_) => {}
        // Expr::Type(_) => {}
        // Expr::Unary(_) => {}
        // Expr::Unsafe(_) => {}
        // Expr::Verbatim(_) => {}
        // Expr::While(_) => {}
        // Expr::Yield(_) => {}
        x => bail!("Expr not supported: {:?}", x),
    })
}

fn parse_stmt(stmt: syn::Stmt) -> Result<Expr> {
    Ok(match stmt {
        Stmt::Local(l) => Def {
            name: parse_pat(l.pat)?,
            ty: None,
            value: parse_expr(*l.init.context("No value")?.1)?,
            visibility: Visibility::Public,
        }
        .into(),
        Stmt::Item(_) => {
            todo!()
        }
        Stmt::Expr(e) => parse_expr(e)?,
        Stmt::Semi(e, _) => parse_expr(e)?,
    })
}

fn parse_block(block: syn::Block) -> Result<Block> {
    let last_value = block
        .stmts
        .last()
        .map(|x| match x {
            Stmt::Semi(..) => false,
            _ => true,
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
        syn::Visibility::Crate(_) => Visibility::Public,
        syn::Visibility::Restricted(_) => Visibility::Public,
        syn::Visibility::Inherited => Visibility::Private,
    }
}
fn parse_item(item: syn::Item) -> Result<Expr> {
    match item {
        Item::Fn(f0) => {
            let visibility = parse_vis(f0.vis.clone());
            let (name, f) = parse_fn(f0)?;
            let d = Def {
                name,
                ty: None,
                value: f.into(),
                visibility,
            };
            Ok(d.into())
        }
        Item::Use(u) => Ok(RawUse { raw: u }.into()),
        _ => bail!("Does not support item {:?} yet", item),
    }
}

pub fn parse_file(file: syn::File) -> Result<Expr> {
    Ok(Module {
        name: Ident::new("__file__"),
        stmts: file
            .items
            .into_iter()
            .map(parse_item)
            .filter(|x| {
                x.as_ref()
                    .map(|x| x.as_ast::<Unit>().is_none())
                    .unwrap_or(true)
            })
            .try_collect()?,
    }
    .into())
}
pub fn parse_module(file: syn::ItemMod) -> Result<Expr> {
    Ok(Module {
        name: parse_ident(file.ident),
        stmts: file
            .content
            .unwrap()
            .1
            .into_iter()
            .map(parse_item)
            .filter(|x| {
                x.as_ref()
                    .map(|x| x.as_ast::<Unit>().is_none())
                    .unwrap_or(true)
            })
            .try_collect()?,
    }
    .into())
}

impl RustSerde {
    pub fn deserialize_expr(&self, code: syn::Expr) -> Result<Expr> {
        parse_expr(code)
    }
    pub fn deserialize_file(&self, code: syn::File) -> Result<Expr> {
        parse_file(code)
    }
    pub fn deserialize_module(&self, code: syn::ItemMod) -> Result<Expr> {
        parse_module(code)
    }
}
