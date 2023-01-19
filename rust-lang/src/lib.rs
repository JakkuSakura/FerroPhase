pub mod rustfmt;

use barebone::{Block, Expr, Ident, *};
use common::{Result, *};
use proc_macro2::TokenStream;
use quote::*;
use std::fmt::{Debug, Formatter};
use syn::*;

struct Macro {
    raw: syn::ExprMacro,
}
impl Ast for Macro {}

impl Debug for Macro {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.raw.fmt(f)
    }
}

pub struct RustSerde;
impl RustSerde {
    fn serialize_ident(&self, i: &Ident) -> TokenStream {
        match i.name.as_str() {
            "+" => quote!(+),
            "*" => quote!(*),
            a => format_ident!("{}", a).into_token_stream(),
        }
    }
    fn serialize_block(&self, n: &Block) -> Result<TokenStream> {
        let stmts: Vec<_> = n
            .stmts
            .iter()
            .map(|x| self.serialize_expr(x))
            .try_collect()?;
        let q = if n.last_value {
            quote!({
                #(#stmts);*
            })
        } else {
            quote!({
                #(#stmts;)*
            })
        };
        return Ok(q);
    }
    fn serialize_fun(&self, n: &FuncDecl) -> Result<TokenStream> {
        let name = format_ident!("{}", n.name.as_ref().unwrap().name);
        let ret = self.serialize_expr(&n.ret)?;
        let param_names: Vec<_> = n
            .params
            .params
            .iter()
            .map(|x| self.serialize_ident(&x.name))
            .collect();
        let param_types: Vec<_> = n
            .params
            .params
            .iter()
            .map(|x| self.serialize_expr(&x.ty))
            .try_collect()?;
        let stmts = self.serialize_block(n.body.as_ref().unwrap())?;
        let q = quote!(
            fn #name(#(#param_names: #param_types), *) -> #ret
                #stmts

        );
        return Ok(q);
    }
    fn serialize_apply(&self, node: &Call) -> Result<TokenStream> {
        let fun = self.serialize_expr(&node.fun)?;
        let fun_str = fun.to_string();
        let args: Vec<_> = node
            .args
            .args
            .iter()
            .map(|x| self.serialize_expr(x))
            .try_collect()?;
        match fun_str.as_str() {
            "+" => Ok(quote!(#(#args) + *)),
            "-" => Ok(quote!(#(#args) - *)),
            "/" => Ok(quote!(#(#args) / *)),
            "*" => {
                let mut result = vec![];
                for (i, a) in args.into_iter().enumerate() {
                    if i != 0 {
                        result.push(quote!(*));
                    }
                    result.push(a);
                }
                Ok(quote!(#(#result)*))
            }
            x if x.contains(".") => Ok(quote!(
                (#fun)(#(#args), *)
            )),
            _ => Ok(quote!(
                #fun(#(#args), *)
            )),
        }
    }
    fn serialize_literal(&self, n: &Expr) -> Result<TokenStream> {
        if let Some(n) = n.as_ast::<LiteralInt>() {
            let n = n.value;
            return Ok(quote!(
                #n
            ));
        }
        if let Some(n) = n.as_ast::<LiteralDecimal>() {
            let n = n.value;
            return Ok(quote!(
                #n
            ));
        }
        bail!("Failed to serialize literal {:?}", n)
    }
    fn serialize_func_type(&self, fun: &FuncType) -> Result<TokenStream> {
        let args: Vec<_> = fun
            .params
            .iter()
            .map(|x| self.serialize_expr(x))
            .try_collect()?;
        let ret = self.serialize_expr(&fun.ret)?;
        Ok(quote!(
            fn(#(#args), *) -> #ret
        ))
    }
    fn serialize_expr(&self, node: &Expr) -> Result<TokenStream> {
        if let Some(n) = node.as_ast::<Block>() {
            return self.serialize_block(n);
        }
        if let Some(m) = node.as_ast::<Module>() {
            let stmts: Vec<_> = m
                .stmts
                .iter()
                .map(|x| self.serialize_expr(x))
                .try_collect()?;
            return Ok(quote!(
                #(#stmts)*
            ));
        }
        if let Some(n) = node.as_ast::<Def>() {
            if let Some(n) = n.value.as_ast::<FuncDecl>() {
                return self.serialize_fun(n);
            }
        }
        if let Some(n) = node.as_ast::<Ident>() {
            return Ok(self.serialize_ident(n).to_token_stream());
        }

        if let Some(_n) = node.as_ast::<Unit>() {
            return Ok(quote!(()));
        }

        if let Some(n) = node.as_ast::<Call>() {
            return self.serialize_apply(n);
        }
        if node.is_literal() {
            return self.serialize_literal(node);
        }
        if let Some(n) = node.as_ast::<Macro>() {
            return Ok(n.raw.to_token_stream());
        }
        if let Some(n) = node.as_ast() {
            return self.serialize_func_type(n);
        }
        bail!("Unable to serialize {:?}", node)
    }
}
impl Serializer for RustSerde {
    fn serialize(&self, node: &Expr) -> Result<String> {
        self.serialize_expr(node).map(|x| x.to_string())
    }
}
impl Deserializer for RustSerde {
    fn deserialize(&self, code: &str) -> Result<Expr> {
        let code: syn::File = parse_str(code)?;
        parse_file(code)
    }
}
fn parse_type(t: syn::Type) -> Result<Expr> {
    let t = match t {
        Type::BareFn(f) => FuncType {
            params: f
                .inputs
                .into_iter()
                .map(|x| x.ty)
                .map(parse_type)
                .try_collect()?,
            ret: parse_return_type(f.output)?,
        }
        .into(),
        Type::Path(p) => {
            let s = p.path.to_token_stream().to_string();
            match s.as_str() {
                "i64" | "i32" | "u64" | "u32" | "f64" | "f32" => Ident::new(s).into(),
                x => Ident::new(x).into(),
                // _ => bail!("Type not supported: {}", s),
            }
        }
        t => bail!("Type not supported {:?}", t.to_token_stream()),
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
        Pat::Ident(name) => Ident::new(name.ident.to_string()),
        _ => todo!(),
    })
}
fn parse_return_type(o: ReturnType) -> Result<Expr> {
    Ok(match o {
        ReturnType::Default => Unit.into(),
        ReturnType::Type(_, t) => parse_type(*t)?,
    })
}
fn parse_fn(f: ItemFn) -> Result<FuncDecl> {
    Ok(FuncDecl {
        name: Some(Ident::new(f.sig.ident.to_string())),
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
    })
}
fn parse_call(call: syn::ExprCall) -> Result<Call> {
    Ok(Call {
        fun: parse_expr(*call.func)?,
        args: PosArgs {
            args: call.args.into_iter().map(parse_expr).try_collect()?,
        },
    })
}
fn parse_expr(expr: syn::Expr) -> Result<Expr> {
    Ok(match expr {
        // Expr::Array(_) => {}
        // Expr::Assign(_) => {}
        // Expr::AssignOp(_) => {}
        // Expr::Async(_) => {}
        // Expr::Await(_) => {}
        syn::Expr::Binary(b) => {
            let l = parse_expr(*b.left)?;
            let r = parse_expr(*b.right)?;
            let op = match b.op {
                BinOp::Add(_) => Ident::new("+"),
                BinOp::Mul(_) => Ident::new("*"),
                _ => bail!("Op not supported {:?}", b.op.to_token_stream()),
            };
            Call {
                fun: op.into(),
                args: PosArgs { args: vec![l, r] },
            }
            .into()
        }
        // Expr::Block(_) => {}
        // Expr::Box(_) => {}
        // Expr::Break(_) => {}
        syn::Expr::Call(c) => parse_call(c)?.into(),
        // Expr::Cast(_) => {}
        // Expr::Closure(_) => {}
        // Expr::Continue(_) => {}
        // Expr::Field(_) => {}
        // Expr::ForLoop(_) => {}
        // Expr::Group(_) => {}
        // Expr::If(_) => {}
        // Expr::Index(_) => {}
        // Expr::Let(_) => {}
        syn::Expr::Lit(l) => match l.lit {
            Lit::Int(i) => LiteralInt::new(i.base10_parse()?).into(),
            Lit::Float(i) => LiteralDecimal::new(i.base10_parse()?).into(),
            _ => bail!("Lit not supported: {:?}", l.lit.to_token_stream()),
        },
        // Expr::Loop(_) => {}
        syn::Expr::Macro(m) => Macro { raw: m }.into(),
        // Expr::Match(_) => {}
        // Expr::MethodCall(_) => {}
        // Expr::Paren(_) => {}
        syn::Expr::Path(p) => Ident::new(p.path.segments.first().unwrap().ident.to_string()).into(),
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
        x => bail!("Expr not supported: {:?}", x.to_token_stream()),
    })
}
fn parse_stmt(stmt: syn::Stmt) -> Result<Expr> {
    Ok(match stmt {
        Stmt::Local(l) => Def {
            name: parse_pat(l.pat)?,
            ty: None,
            value: parse_expr(*l.init.context("No value")?.1)?,
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
fn parse_item(item: syn::Item) -> Result<Expr> {
    match item {
        Item::Fn(f) => {
            let f = parse_fn(f)?;
            let d = Def {
                name: f.name.clone().context("no fun name")?,
                ty: None,
                value: f.into(),
            };
            Ok(d.into())
        }
        Item::Use(_) => Ok(Unit.into()),
        _ => bail!("Does not support item {:?} yet", item),
    }
}
fn parse_file(file: syn::File) -> Result<Expr> {
    Ok(Module {
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
