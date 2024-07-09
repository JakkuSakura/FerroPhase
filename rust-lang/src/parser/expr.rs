use crate::{parser, RawExpr, RawExprMacro, RawItemMacro, RawStmtMacro, RawUse};
use common::warn;
use eyre::{bail, ContextCompat};
use itertools::Itertools;
use lang_core::ast::{
    BExpr, DefEnum, DefFunction, DefStruct, DefTrait, DefType, EnumTypeVariant, Expr, ExprBinOp,
    ExprBlock, ExprIf, ExprIndex, ExprInitStruct, ExprInvoke, ExprInvokeTarget, ExprLoop,
    ExprReference, ExprSelect, ExprSelectType, ExprSelfType, ExprUnOp, FunctionParam, Import, Item,
    Statement, StatementLet, Type, TypeEnum, TypeStruct, Value, ValueBool, ValueDecimal,
    ValueFunction, ValueInt, ValueString, ValueTuple,
};
use lang_core::id::{Ident, Path};
use lang_core::ops::{BinOpKind, UnOpKind};
use lang_core::utils::anybox::AnyBox;
use quote::ToTokens;
use syn::{Fields, FnArg, Lit, ReturnType};

pub fn parse_literal(lit: syn::Lit) -> eyre::Result<Value> {
    Ok(match lit {
        Lit::Int(i) => Value::Int(ValueInt::new(i.base10_parse()?)),
        Lit::Float(i) => Value::Decimal(ValueDecimal::new(i.base10_parse()?)),
        Lit::Str(s) => Value::String(ValueString::new_ref(s.value())),
        Lit::Bool(b) => Value::Bool(ValueBool::new(b.value)),
        _ => bail!("Lit not supported: {:?}", lit.to_token_stream()),
    })
}

pub fn parse_unary(u: syn::ExprUnary) -> eyre::Result<ExprUnOp> {
    let expr = parse_expr(*u.expr)?;
    let op = match u.op {
        syn::UnOp::Neg(_) => UnOpKind::Neg,
        syn::UnOp::Not(_) => UnOpKind::Not,
        _ => bail!("Unary op not supported: {:?}", u.op),
    };
    Ok(ExprUnOp { op, val: expr })
}

pub fn parse_expr(expr: syn::Expr) -> eyre::Result<BExpr> {
    let expr = match expr {
        syn::Expr::Binary(b) => parse_expr_binary(b)?,
        syn::Expr::Unary(u) => parse_unary(u)?.into(),
        syn::Expr::Block(b) if b.label.is_none() => Expr::block(parse_block(b.block)?),
        syn::Expr::Call(c) => Expr::Invoke(parse_expr_call(c)?.into()),
        syn::Expr::If(i) => Expr::If(parse_expr_if(i)?),
        syn::Expr::Loop(l) => Expr::Loop(parse_expr_loop(l)?),
        syn::Expr::Lit(l) => Expr::value(parse_literal(l.lit)?),
        syn::Expr::Macro(m) => Expr::any(RawExprMacro { raw: m }),
        syn::Expr::MethodCall(c) => Expr::Invoke(parse_expr_method_call(c)?.into()),
        syn::Expr::Index(i) => Expr::Index(parse_expr_index(i)?),
        syn::Expr::Path(p) => Expr::path(parser::parse_path(p.path)?),
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
pub fn parse_stmt(stmt: syn::Stmt) -> eyre::Result<(Statement, bool)> {
    Ok(match stmt {
        syn::Stmt::Local(l) => (
            Statement::Let(StatementLet {
                pat: parser::parse_pat(l.pat)?,
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

pub fn parse_block(block: syn::Block) -> eyre::Result<ExprBlock> {
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

pub fn parse_expr_reference(item: syn::ExprReference) -> eyre::Result<ExprReference> {
    Ok(ExprReference {
        referee: parse_expr(*item.expr)?.into(),
        mutable: Some(item.mutability.is_some()),
    })
}

pub fn parse_expr_call(call: syn::ExprCall) -> eyre::Result<ExprInvoke> {
    let fun = parse_expr(*call.func)?;
    let args: Vec<_> = call.args.into_iter().map(parse_expr).try_collect()?;

    Ok(ExprInvoke {
        target: ExprInvokeTarget::expr(fun),
        args,
    })
}

pub fn parse_expr_method_call(call: syn::ExprMethodCall) -> eyre::Result<ExprInvoke> {
    Ok(ExprInvoke {
        target: ExprInvokeTarget::Method(
            ExprSelect {
                obj: parse_expr(*call.receiver)?.into(),
                field: parser::parse_ident(call.method),
                select: ExprSelectType::Method,
            }
            .into(),
        )
        .into(),
        args: call.args.into_iter().map(parse_expr).try_collect()?,
    })
}

pub fn parse_expr_index(i: syn::ExprIndex) -> eyre::Result<ExprIndex> {
    Ok(ExprIndex {
        expr: parse_expr(*i.expr)?,
        index: parse_expr(*i.index)?,
    })
}

pub fn parse_expr_if(i: syn::ExprIf) -> eyre::Result<ExprIf> {
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

pub fn parse_expr_loop(l: syn::ExprLoop) -> eyre::Result<ExprLoop> {
    Ok(ExprLoop {
        label: None, // TODO: label
        body: Expr::block(parse_block(l.body)?).into(),
    })
}

pub fn parse_expr_binary(b: syn::ExprBinary) -> eyre::Result<Expr> {
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

fn parse_tuple(t: syn::ExprTuple) -> eyre::Result<ValueTuple> {
    let mut values = vec![];
    for e in t.elems {
        let expr = parse_expr(e)?;
        let value = Value::expr(*expr);
        values.push(value);
    }

    Ok(ValueTuple { values })
}

pub fn parse_struct_expr(s: syn::ExprStruct) -> eyre::Result<ExprInitStruct> {
    Ok(ExprInitStruct {
        name: Expr::path(parser::parse_path(s.path)?).into(),
        fields: s
            .fields
            .into_iter()
            .map(|x| parser::parse_field_value(x))
            .try_collect()?,
    })
}

pub fn parse_fn_arg(i: FnArg) -> eyre::Result<FunctionParam> {
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
            name: parser::parse_pat(*t.pat)?
                .as_ident()
                .context("No ident")?
                .clone(),
            ty: parser::parse_type_value(*t.ty)?,
        },
    })
}

pub fn parse_return_type(o: ReturnType) -> eyre::Result<Type> {
    Ok(match o {
        ReturnType::Default => Type::unit(),
        ReturnType::Type(_, t) => parser::parse_type_value(*t)?,
    })
}

pub fn parse_fn(f: syn::ItemFn) -> eyre::Result<ValueFunction> {
    let sig = parser::parse_fn_sig(f.sig)?;
    Ok(ValueFunction {
        sig,
        body: Expr::block(parse_block(*f.block)?).into(),
    })
}

fn parse_use(u: syn::ItemUse) -> eyre::Result<Import, RawUse> {
    let mut segments = vec![];
    let mut tree = u.tree.clone();
    loop {
        match tree {
            syn::UseTree::Path(p) => {
                segments.push(parser::parse_ident(p.ident));
                tree = *p.tree;
            }
            syn::UseTree::Name(name) => {
                segments.push(parser::parse_ident(name.ident));
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
        visibility: parser::parse_vis(u.vis),
        path: Path::new(segments),
    })
}

pub fn parse_item_struct(s: syn::ItemStruct) -> eyre::Result<TypeStruct> {
    Ok(TypeStruct {
        name: parser::parse_ident(s.ident),
        fields: s
            .fields
            .into_iter()
            .enumerate()
            .map(|(i, f)| parser::parse_struct_field(i, f))
            .try_collect()?,
    })
}

fn parse_trait(t: syn::ItemTrait) -> eyre::Result<DefTrait> {
    // TODO: generis params
    let bounds = parser::parse_type_param_bounds(t.supertraits.into_iter().collect())?;
    let vis = parser::parse_vis(t.vis);
    Ok(DefTrait {
        name: parser::parse_ident(t.ident),
        bounds,
        items: t
            .items
            .into_iter()
            .map(|x| parser::parse_trait_item(x))
            .try_collect()?,
        visibility: vis,
    })
}

pub fn parse_item(item: syn::Item) -> eyre::Result<Item> {
    match item {
        syn::Item::Fn(f0) => {
            let visibility = parser::parse_vis(f0.vis.clone());
            let f = parse_fn(f0)?;
            let d = DefFunction {
                name: f.name.clone().unwrap(),
                ty: None,
                value: f,
                visibility,
            };
            Ok(Item::DefFunction(d))
        }
        syn::Item::Impl(im) => Ok(Item::Impl(parser::parse_impl(im)?)),
        syn::Item::Use(u) => Ok(match parse_use(u) {
            Ok(i) => Item::Import(i),
            Err(r) => Item::Any(AnyBox::new(r)),
        }),
        syn::Item::Macro(m) => Ok(Item::any(RawItemMacro { raw: m })),
        syn::Item::Struct(s) => {
            let visibility = parser::parse_vis(s.vis.clone());

            let struct_type = parse_item_struct(s)?;
            Ok(Item::DefStruct(DefStruct {
                name: struct_type.name.clone(),
                value: struct_type,
                visibility,
            }))
        }
        syn::Item::Enum(e) => {
            let visibility = parser::parse_vis(e.vis.clone());
            let ident = parser::parse_ident(e.ident.clone());
            let variants = e
                .variants
                .into_iter()
                .map(|x| {
                    let name = parser::parse_ident(x.ident);
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
            let visibility = parser::parse_vis(t.vis.clone());
            let ty = parser::parse_type_value(*t.ty)?;
            Ok(Item::DefType(DefType {
                name: parser::parse_ident(t.ident),
                value: ty,
                visibility,
            }))
        }
        syn::Item::Mod(m) => Ok(Item::Module(parser::parse_module(m)?)),
        syn::Item::Trait(t) => {
            let trait_ = parse_trait(t)?;
            Ok(Item::DefTrait(trait_))
        }
        _ => bail!("Does not support item yet: {:?}", item),
    }
}
